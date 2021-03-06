//! Memory management interface and garbage collection.

// TODO finalizers and weak tables

extern crate aligned_alloc;

use std::any::Any;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem::transmute;

use string::Str;

pub mod arena;
pub mod noop;
pub mod string;

/// The recommended allocator. May change without further notice.
pub type DefaultGc = noop::NoopGc;

/// A GC-managed object that may reference other GC-managed objects and keep them alive. Provides a
/// `trace` method that will be invoked by the GC to trace these references.
pub trait Traceable : Any {
    fn trace<T: Tracer>(&self, t: &mut T);
}

/// Traced reference to an object owned by the GC. This is inherently unsafe. May be stored in
/// traced structs, which will keep the referenced object alive. Cannot outlive the GC that owns
/// the object.
pub struct TracedRef<T: Any + ?Sized> {
    ptr: *const T,
}

impl<T: Any> Copy for TracedRef<T> {}

impl<T: Any> Clone for TracedRef<T> {
    fn clone(&self) -> TracedRef<T> {
        *self
    }
}

impl<T: Any> PartialEq for TracedRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T: Any> Eq for TracedRef<T> {}

impl<T: Any> Hash for TracedRef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.ptr as usize);
    }
}

impl<T: Any> fmt::Debug for TracedRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:p}", self.ptr)
    }
}

impl<T: Any> fmt::Pointer for TracedRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:p}", self.ptr)
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
    unsafe fn mark_untraceable<T: Any>(&mut self, TracedRef<T>);

    /// Marks an object as reachable. This will also (at some undefined point in the future) trace
    /// the object, finding all transitively reachable objects.
    fn mark_traceable<T: Traceable>(&mut self, TracedRef<T>);
}

/// Contains methods called on various GC events. Must be implemented by the GC user and can
/// specify application-specific behaviour.
pub trait GcCallback {
    /// Invoke `f` on each object that is part of the root set. This is used to find the initially
    /// reachable objects when starting a collection.
    fn for_each_root<F>(&self, f: F) where F: FnMut(TracedRef<Any>);
}

/// Trait for Garbage Collector implementations.
pub trait GcStrategy {
    /// Instruct the GC to perform a collection step. This can be called by user code. A callback
    /// is passed so the GC can run finalizers and find the roots.
    fn collect_step<C: GcCallback>(&mut self, &mut C);

    /// Perform an atomic collection. The GC should try to free all currently unreachable objects.
    fn collect_atomic<C: GcCallback>(&mut self, &mut C);

    /// Called when a garbage-collected object is created. The GC should take ownership of the
    /// object and can dispose of it when collecting.
    fn register_obj<T: Any>(&mut self, T) -> TracedRef<T>;

    /// Interns a string and returns a `TracedRef` to the shared instance.
    fn intern_str<T: Into<Str>>(&mut self, T) -> TracedRef<Str>;

    /// Given a `TracedRef` of an object owned by this GC, this method unsafely returns a reference
    /// to the object.
    unsafe fn get_ref<T: Any>(&self, t: TracedRef<T>) -> &T {
        &*t.ptr
    }

    /// The mutable version of `get_ref`, returns a mutable reference to the object.
    ///
    /// This will mutably borrow the GC and thus prevent collections and aliasing references (from
    /// `get_ref`).
    unsafe fn get_mut<T: Any>(&mut self, t: TracedRef<T>) -> &mut T {
        transmute(t.ptr)
    }

    // TODO: Statistics interface
}
