//! Memory management interface and garbage collection.
//!
//! # Finalization
//!
//! Finalizers allow Lea code to be run when a table is unreachable and would be collected by the
//! GC. They can execute arbitrary code and potentially make the table reachable again, and have
//! access to the table and its referenced objects, which means that the GC must not collect them
//! when a finalizer needs to run.
//!
//! A tables metatable can specify a `__gc` field, which will be run as a finalizer for the table
//! (if it's a function). The table should register itself for finalization in the GC when its
//! metatable is changed and contains a `__gc` field. If the metatable doesn't contain the field,
//! and it is later added to it, the table will *not* be marked for finalization, matching Lua
//! semantics.
//!
//! The GC manages a list of objects with finalizers and scans through them after the main mark
//! phase. If any object is unmarked, it will be added to a finalization list and recursively
//! marked. This makes sure that the finalization function can access the object and all reachable
//! objects. The object is removed from the finalized-object list and its finalizer is invoked.

use vm::VM;

use std::cell::RefCell;
use std::thread;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::transmute;
use std::ops::Deref;


pub mod noop;
pub mod stw;


// TODO we might want to use #[no_move] like servo does
// TODO properly check for unsafety and annotate unsafe fns
// TODO finalizers and weak references (with callback)

pub type DefaultGc<'gc> = stw::Stw<'gc>;

/// A garbage-collectable object.
pub trait GcObj {}

impl GcObj for String {}

/// A GC-managed object that may reference other GC-managed objects and keep them alive. Provides a
/// `trace` method that will be invoked by the GC to trace these references.
pub trait Traceable : GcObj {
    fn trace<T: Tracer>(&self, t: &mut T);
}


/// Trait implemented by all references to GC objects.
pub trait GcReference<'gc, T: GcObj> {
    /// Get a pointer to the referenced object. The validity of the pointer depends on the specific
    /// reference type.
    fn get_ptr(&self) -> *const T;
}

impl <'gc, T: GcObj> PartialEq for GcReference<'gc, T> {
    fn eq(&self, other: &Self) -> bool {
        self.get_ptr() == other.get_ptr()
    }
}

impl <'gc, T: GcObj> Eq for GcReference<'gc, T> {}

impl <'gc, T: GcObj> Hash for GcReference<'gc, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.get_ptr() as usize);
    }
}

impl <'gc, T: GcObj> fmt::Debug for GcReference<'gc, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:p}", self.get_ptr())
    }
}

/// impl_gcref!(RefType, ptr_field) generates an `impl GcReference<...>` for the type `RefType`,
/// using the field `ptrfield` as the pointer to the referenced object (`ptrfield` must be a field
/// of the object, storing a `*const T`). The reference type must be generic over `GcObj`s.
///
/// (It's probably easier to understand by just reading the macro definition and uses).
///
/// This also generates all `impl`s required by `GcReference`.
macro_rules! impl_gcref(
    ( $t:ty, $ptrfield:ident ) => (
        /// (macro-generated impl)
        ///
        /// `PartialEq` is implemented in terms of pointer equality. Note that some GC objects can
        /// override this method with a metafunction, but that is handled purely by the VM.
        impl <'gc, T: GcObj> PartialEq for $t {
            fn eq(&self, other: &Self) -> bool {
                self.get_ptr() == other.get_ptr()
            }
        }

        /// (macro-generated impl)
        ///
        /// Equality is reflexive, since only the address is compared (`x == y` iff x refers to the
        /// same object as y, so `x == x` always holds).
        impl <'gc, T: GcObj> Eq for $t {}

        /// (macro-generated impl)
        ///
        /// Hash is also implemented purely on the pointer and will just feed the address of the
        /// object to the `Hasher`.
        impl <'gc, T: GcObj> Hash for $t {
            fn hash<H: Hasher>(&self, state: &mut H) {
                state.write_usize(self.get_ptr() as usize);
            }
        }

        /// (macro-generated impl)
        impl <'gc, T: GcObj> fmt::Debug for $t {
            fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                write!(f, "{:p}", self.get_ptr())
            }
        }

        /// (macro-generated impl)
        impl <'gc, T: GcObj + 'gc> GcReference<'gc, T> for $t {
            fn get_ptr(&self) -> *const T {
                self.$ptrfield
            }
        }
    );
);


/// Traced reference to an object owned by the GC. This is inherently unsafe. May be stored in
/// traced structs, which will keep the referenced object alive. Cannot outlive the GC that owns
/// the object.
pub struct TracedRef<'gc, T: GcObj + 'gc> {
    ptr: *const T,
    phantom: PhantomData<&'gc T>,
}

impl <'gc, T: GcObj + 'gc> TracedRef<'gc, T> {
    /// Obtain an unsafe reference to the referenced object. The caller must ensure that the
    /// `TracedRef` is reachable to make sure that the referenced object will not be collected
    /// while being used.
    pub unsafe fn get_ref<'a>(&'a self) -> &'a T {
        // Safety: If the `TracedRef` is stored in an object that's owned by the GC, this reference
        // will live as long as that object.
        transmute(self.ptr)
    }

    /// Obtains a mutable reference to the object. This is unsafe, because you can create 2
    /// aliasing mutable references this way (the same unsafeness of `get_ref` applies here as
    /// well).
    pub /*very*/ unsafe fn get_mut_ref<'a>(&'a self) -> &'a mut T {
        transmute(self.ptr)
    }
}

impl_gcref!(TracedRef<'gc, T>, ptr);

/// `GcRef`s can be safely copied if the user adheres to the usual invariants (no collections while
/// untraced refs to any objects exist).
impl <'gc, T: GcObj> Copy for TracedRef<'gc, T> {}

impl <'gc, T: GcObj> Clone for TracedRef<'gc, T> {
    fn clone(&self) -> TracedRef<'gc, T> {
        TracedRef {
            ptr: self.ptr,
            phantom: self.phantom,
        }
    }
}

/// Contains the roots of a garbage collector. The rooted objects will not be collected.
///
/// This is just a default implementation, improved GCs may provide their own, faster version.
pub struct Roots {
    // we store pointers to () and only allow `GcObj`s inside
    stack: RefCell<Vec<*const ()>>,
}

impl Roots {
    fn root<T: GcObj>(&self, obj: *const T) {
        self.stack.borrow_mut().push(obj as *const ());
    }

    /// Unroots the given `Rooted` reference. If `rooted` is not unrooted in LIFO order, this will
    /// panic. This should only be called from `Rooted<T>`s destructor.
    fn unroot<T: GcObj>(&self, rooted: *const T) {
        let last = self.stack.borrow_mut().pop().expect("empty root set when unrooting");

        // assert that we popped the correct object (keep LIFO ordering)
        if last as *const T != rooted {
            panic!("wrong unrooting order: attempted to unroot {:?}, should unroot {:?} first",
                rooted, last);
        }
    }

    /// Returns the number of rooted objects
    fn len(&self) -> usize {
        self.stack.borrow().len()
    }

    /// Calls a closure with each rooted pointer.
    fn each<F>(&self, mut f: F) where F: FnMut(*const ()) {
        let stack = self.stack.borrow();
        for ptr in stack.iter() {
            f(*ptr);
        }
    }
}

impl Default for Roots {
    fn default() -> Roots {
        Roots {
            stack: RefCell::new(Vec::new()),
        }
    }
}

/// Reference to a rooted object. TODO #[no_move]
pub struct Rooted<'gc, T: GcObj + 'gc, G: GcStrategy<'gc> + 'gc> {
    ptr: *const T,
    gc: &'gc G,
}

//impl_gcref!(Rooted<'gc, T>, ptr);

impl <'gc, T: GcObj + 'gc, G: GcStrategy<'gc>> Rooted<'gc, T, G> {
    /// Gets a reference to the rooted object that is bound to the lifetime of this `Rooted<T>`.
    pub fn get_ref<'a>(&'a self) -> &'a T {
        unsafe { transmute(self.ptr) }
    }
}

impl <'gc, T: GcObj + 'gc, G: GcStrategy<'gc>> GcReference<'gc, T> for Rooted<'gc, T, G> {
    fn get_ptr(&self) -> *const T {
        self.ptr
    }
}

impl <'gc, T: GcObj + 'gc, G: GcStrategy<'gc>> Drop for Rooted<'gc, T, G> {
    /// Unroots the object
    fn drop(&mut self) {
        // Don't unroot while panicking, since that might lead to another panic. If the GC drops
        // all objects when it's destroyed (it should), this won't leak memory.
        if !thread::panicking() {
            self.gc.unroot(self.ptr);
        }
    }
}

/// A `Tracer` provides methods to mark objects as reachable. A `GcStrategy` will pass an object
/// that implements `Tracer` to the `trace` method of all `Traceable` and reachable objects.
pub trait Tracer {
    /// Marks a `TracedRef` as reachable without following the referenced object's own references
    /// (ie. without tracing it).
    ///
    /// This is unsafe and may only be called if the object really doesn't carry any references
    /// (currently only strings) (FIXME: Needs either some hack to work safely or specialization).
    unsafe fn mark_untraceable<'gc, T: GcObj>(&mut self, TracedRef<'gc, T>);

    /// Marks an object as reachable. This will also (at some undefined point in the future) trace
    /// the object, finding all transitively reachable objects.
    fn mark_traceable<'gc, T: Traceable>(&mut self, TracedRef<'gc, T>);
}

/// Trait for Garbage Collector implementations.
pub trait GcStrategy<'gc> {
    /// Instruct the GC to perform a collection step. This can be called by user code. A VM
    /// instance is passed so the GC can run finalizers.
    fn collect_step(&self, &mut VM<'gc, Self>);

    /// Perform an atomic collection. The GC shall perform all necessary tracing and free all
    /// currently unreachable objects.
    fn collect_atomic(&self, &mut VM<'gc, Self>);

    /// Called when a garbage-collected object is created. The GC should take ownership of the
    /// object and can dispose of it when collecting. The GC may also use this to trigger a
    /// collection according to some internal heuristic.
    ///
    /// Callees must ensure that this isn't called when unrooted references to objects exist.
    fn register_obj<T: GcObj + 'gc>(&'gc self, T) -> TracedRef<'gc, T>;

    /// Roots the given object, creating a rooted reference from a traced reference
    unsafe fn root<T: GcObj + 'gc>(&'gc self, obj: TracedRef<'gc, T>) -> Rooted<'gc, T, Self>;

    /// Unroots the given object, making it eligible for garbage collection. This should only be
    /// called by `Rooted`s `Drop` implementation.
    fn unroot<T: GcObj + 'gc>(&self, rooted: *const T);

    // TODO: Statistics interface
}
