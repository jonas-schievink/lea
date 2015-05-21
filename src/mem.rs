//! Memory management interface and garbage collection.
//!
//! # Finalization
//!
//! Finalizers allow Lea code to be run when a table is unreachable and would be collected by the
//! GC. They can execute arbitrary code and potentially make the table reachable again, and have
//! access to the table and its referenced objects, which means that the GC must not collect them
//! when a finalizer needs to run.
//!
//! A tables metatables can specify a `__gc` field, which will be run as a finalizer for the table
//! (if it's a function). The table should register itself for finalization in the GC when its
//! metatable is changed and contains a `__gc` field. If the metatable doesn't contain the field,
//! and it is later added to it, the table will *not* be marked for finalization, matching Lua
//! semantics.
//!
//! The GC manages a list of objects with finalizers and scans through them after the main mark
//! phase. If any object is unmarked, it will be added to a finalization list and recursively
//! marked. This makes sure that the finalization function can access the object and all reachable
//! objects. The object is removed from the finalized-object list and its finalizer is invoked.

// TODO: Finalizers, weak tables, less horrific code

#![allow(mutable_transmutes)]

use program::{Function, FunctionProto};
use array::Array;
use table::Table;
use vm::VM;

use rustc_serialize::{Encoder, Encodable};

use std::mem::{transmute, size_of, replace};
use std::boxed::into_raw;
use std::hash::{Hash, Hasher};
use std::fmt;


/// A garbage-collectable object. Provides a method to get its `GcType` for proper destruction by
/// the GC.
///
/// Note: This is an internal type and may not be used from other modules.
pub trait GcObj : Encodable {
    fn get_type() -> GcType;
}

/// Enables garbage collection for a list of types. All identifiers passed to this macro must refer
/// to compatible types in scope.
///
/// When testing, the list after the ";" is also considered. This allows us to define custom
/// testing types that are not available when building normally.
macro_rules! gc_objects {
    ( $( $name:ident ),+ ; $( $dbg:ident ),* ) => {
        // For some reason, `#[derive]` is applied before `#[cfg]`, so we have to put `GcType` into
        // its own module or we get colliding impls.

        #[cfg(not(test))]
        mod gctype {
            #[derive(Copy, Clone, Debug)]
            pub enum GcType {
                $($name,)+
            }
        }

        #[cfg(test)]
        mod gctype {
            #[derive(Copy, Clone, Debug)]
            pub enum GcType {
                $($name,)+
                $($dbg,)*
            }
        }

        pub use self::gctype::GcType;

        impl GcType {
            /// Runs the drop glue for the object stored behind the given `GcHeader`.
            #[cfg(not(test))]
            fn drop_obj(self, h: &GcHeader) {
                match self {
                    $(
                        GcType::$name => {
                            let _:Box<(GcHeader, $name)> = unsafe { transmute(h) };
                        },
                    )+
                }
            }

            #[cfg(test)]
            fn drop_obj(self, h: &GcHeader) {
                match self {
                    $(
                        GcType::$name => {
                            let _:Box<(GcHeader, $name)> = unsafe { transmute(h) };
                        },
                    )+
                    $(
                        GcType::$dbg => {
                            let _:Box<(GcHeader, tests::$dbg)> = unsafe { transmute(h) };
                        },
                    )*
                }
            }

            #[cfg(not(test))]
            #[allow(unused_must_use)]
            fn traverse(self, gc: &mut Gc, h: &GcHeader) {
                match self {
                    $(
                        GcType::$name => {
                            unsafe { h.get_obj::<$name>().encode(gc); }
                        },
                    )+
                }
            }

            #[cfg(test)]
            #[allow(unused_must_use)]
            fn traverse(self, gc: &mut Gc, h: &GcHeader) {
                match self {
                    $(
                        GcType::$name => {
                            unsafe { h.get_obj::<$name>().encode(gc); }
                        },
                    )+
                    $(
                        GcType::$dbg => {
                            unsafe { h.get_obj::<tests::$dbg>().encode(gc); }
                        },
                    )*
                }
            }
        }

        $(
            impl GcObj for $name {
                fn get_type() -> GcType {
                    GcType::$name
                }
            }
        )+

        $(
            #[cfg(test)]
            impl GcObj for tests::$dbg {
                fn get_type() -> GcType {
                    GcType::$dbg
                }
            }
        )*
    };
}

gc_objects!(String, Table, Array, Function, FunctionProto; Dummy);


/// Header of all GC objects.
#[derive(Debug)]
struct GcHeader {
    /// Next object in GC object list
    next: Option<&'static GcHeader>,
    /// true if this object is considered reachable. Only valid after mark phase.
    marked: bool,
    /// true if in `fin` list (and the object needs finalization)
    needs_fin: bool,
    /// type of the object
    ty: GcType,

    // object data lies here
}

#[test]
fn header_size() {
    // important for proper alignment of the object behind the header: GcHeader must be a multiple
    // of the pointer size (this should cover all alignment restrictions)
    assert_eq!(size_of::<GcHeader>(), size_of::<usize>() * 2);
}

impl GcHeader {
    fn new(next: Option<&'static GcHeader>, ty: GcType) -> GcHeader {
        GcHeader {
            next: next,
            ty: ty,
            marked: false,
            needs_fin: false,
        }
    }

    /// Gets a mutable reference to the object behind this header.
    unsafe fn get_obj<T: GcObj>(&self) -> &T {
        let addr: usize = transmute(self);

        transmute(addr + size_of::<GcHeader>())
    }
}


/// Unsafe, traced reference to a `GcObj`. Implements `PartialEq` and `Eq` in terms of pointer
/// equality and `Hash` by using the address of the object. Can be trivially copied.
///
/// Note: Never access a `GcRef` from the `drop` method, because the referenced objects might
/// already be collected.
pub struct GcRef<T: GcObj>(*mut T);

impl <T: GcObj> GcRef<T> {
    /// Get a reference to the object pointed to.
    pub unsafe fn get(&self) -> &T {
        transmute(self.0)
    }

    /// Get a mutable reference to the object pointed to.
    pub unsafe fn get_mut(&self) -> &mut T {
        transmute(self.0)
    }

    fn get_header(&self) -> &'static GcHeader {
        let addr = self.0 as usize - size_of::<GcHeader>();

        unsafe { transmute(addr) }
    }

    /// Gets the corresponding GcHeader for the object, assuming it is wrapped inside `Wrap<T>`
    /// (which it should be, since only the GC creates `GcRef`s and ensures this invariant).
    fn get_header_mut(&self) -> &'static mut GcHeader {
        unsafe { transmute(self.get_header()) }
    }
}

/// `GcRef`s can be safely copied if the user adheres to the usual invariants (no collections while
/// untraced refs to any objects exist).
impl <T: GcObj> Copy for GcRef<T> {}

impl <T: GcObj> Clone for GcRef<T> {
    fn clone(&self) -> GcRef<T> {
        GcRef(self.0)
    }
}

/// `PartialEq` is implemented in terms of pointer equality. Note that some GC objects can override
/// this method with a metafunction, but that is handled purely by the VM.
impl <T: GcObj> PartialEq for GcRef<T> {
    fn eq(&self, other: &GcRef<T>) -> bool {
        self.0 == other.0
    }
}

impl <T: GcObj> Eq for GcRef<T> {}

/// Hash is also implemented purely on the pointer and will just feed the address of the object to
/// the `Hasher`.
impl <T: GcObj> Hash for GcRef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as usize);
    }
}

impl <T: GcObj> fmt::Debug for GcRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:p}", self.0)
    }
}

/// We implement tracing with the `Encodable` trait.
impl <T: GcObj> Encodable for GcRef<T> {
    fn encode<E: Encoder>(&self, enc: &mut E) -> Result<(), E::Error> {
        // This is unsafe because `encode` might get called with any type implementing Encoder.
        // It is only supported to call this method with an instance of `Gc` as the encoder.
        unsafe {
            let gc: &mut Gc = transmute(enc);
            gc.process(self);
        }

        Ok(())
    }
}


/// The garbage collector.
///
/// Each instance manages a list of known objects and a list of GC roots. When collecting, the
/// roots are recursively traced, marking all GC objects that can be reached. In the sweep phase,
/// the object list in traversed and all unmarked objects are removed from the list and dropped.
pub struct Gc {
    /// Regular GC object list
    first: Option<&'static GcHeader>,
    /// List of objects that need to be finalized before they are collected.
    fin: Option<&'static GcHeader>,
    /// List of rooted GC objects
    first_root: Option<&'static GcHeader>,
}

impl Gc {
    /// Creates a new garbage collector that knows about no objects and has no roots.
    pub fn new() -> Gc {
        Gc {
            first: None,
            fin: None,
            first_root: None,
        }
    }

    /// Roots the given object and returns a `GcRef`.
    pub fn root<T: GcObj>(&mut self, t: T) -> GcRef<T> {
        let first_root = replace(&mut self.first_root, None);
        let b = Box::new((GcHeader::new(first_root, T::get_type()), t));

        unsafe {
            let addr: *mut (GcHeader, T) = into_raw(b);
            let (ref header_addr, ref obj_addr) = *addr;
            self.first_root = Some(transmute(header_addr));

            //println!("reg root {:p}", addr);

            GcRef(transmute(obj_addr))
        }
    }

    // TODO unroot (maybe later, and if the need for Rooted<T> arises)

    /// Transfers ownership of a garbage-collected object to the GC and returns a garbage-collected
    /// reference to it. The GC is now in charge of managing the object and will dispose it if no
    /// `GcRef`s to it are found during a collection.
    pub fn register<T: GcObj>(&mut self, t: T) -> GcRef<T> {
        let first = replace(&mut self.first, None);
        let b = Box::new((GcHeader::new(first, T::get_type()), t));

        unsafe {
            let addr: *mut (GcHeader, T) = into_raw(b);
            let (ref header_addr, ref obj_addr) = *addr;
            self.first = Some(transmute(header_addr));

            //println!("reg {:p}", addr);

            GcRef(transmute(obj_addr))
        }
    }

    /// Registers an object for finalization. If it was already registered, does nothing.
    pub fn register_finalizer<T: GcObj + 'static>(&mut self, obj: GcRef<T>) {
        unsafe {
            let header = obj.get_header_mut();
            if header.needs_fin { return; }

            header.needs_fin = true;

            let header = obj.get_header();  // get as immutable

            // move to finalized object list (need to scan through the list to remove the object)
            // this is not as inefficient as it sounds, because tables that get metatables attached
            // (that define __gc) are usually young and thus at the beginning of the list
            let mut last: Option<&GcHeader> = None;
            let mut cur: &GcHeader = self.first.expect("unregistered GcRef");
            loop {
                if cur as *const GcHeader == header as *const GcHeader {
                    match last {
                        None => self.first = header.next,
                        Some(ref l) => {
                            // FIXME this is getting ridiculous. use linked list and cells!
                            let m: &mut GcHeader = transmute(l);
                            m.next = header.next;
                        },
                    }

                    break;
                }

                last = Some(cur);
                cur = cur.next.expect("unregistered GcRef");
            }

            // add to fin list
            let header = obj.get_header_mut();
            header.next = self.fin;
            self.fin = Some(header);
        }
    }

    /// Performs an atomic garbage collection (stop-the-world).
    ///
    /// Returns the number of objects that were collected.
    pub fn collect(&mut self, vm: &mut VM) -> usize {
        self.mark();
        self.finalize(vm);
        self.sweep()
    }

    /// The mark phase. Recursively traces all reachable objects, starting at the GC roots.
    fn mark(&mut self) {
        let mut cur: Option<&'static GcHeader> = self.first_root;

        while cur.is_some() {
            let h = cur.unwrap();
            h.ty.traverse(self, h);

            cur = h.next;
        }
    }

    /// Traces and marks objects that are to be finalized, puts them into the regular GC object
    /// list, and runs their finalization code.
    fn finalize(&mut self, _vm: &mut VM) {
        // TODO
    }

    /// Iterates through the object list and frees all unmarked objects. Resets the mark bit to
    /// unmarked for all other objects.
    fn sweep(&mut self) -> usize {
        let mut swept: usize = 0;
        let mut prev: Option<&'static GcHeader> = None;
        let mut cur: Option<&'static GcHeader> = self.first;

        while cur.is_some() {
            let h = cur.unwrap();

            if h.marked {
                // unmark so the object can be collected in the next collection cycle
                // TODO use cells for this (and figure out how slow they are)
                unsafe { transmute::<_, &mut GcHeader>(h).marked = false; }

                prev = cur;
                cur = h.next;
            } else {
                //println!("sweeping {:p}", h);

                // remove from object list
                match prev {
                    Some(prev_header) => {
                        // TODO we should *probably* use cells
                        let mut_header: &mut GcHeader = unsafe { transmute(prev_header) };
                        mut_header.next = h.next;
                    }
                    None => {
                        self.first = h.next;
                    },
                }

                prev = cur;
                cur = h.next;

                // run drop glue
                h.ty.drop_obj(h);

                swept += 1;
            }
        }

        swept
    }

    /// Marks an object as reachable and traces its references (if the object is traceable).
    #[allow(unused_must_use)]
    unsafe fn process<T: GcObj>(&mut self, ptr: &GcRef<T>) {
        let header = ptr.get_header_mut();

        if !header.marked {
            // mark and traverse the object itself
            header.marked = true;
            ptr.get_mut().encode(self);
        }
    }
}

/// The collector will drop all objects when it is dropped.
impl Drop for Gc {
    fn drop(&mut self) {
        // TODO: If a collection is in progress (requires non-atomic collections), ignore the mark
        // bit while sweeping!
        let mut cur = self.first_root;
        while cur.is_some() {
            let h = cur.unwrap();
            //println!("drop root {:p}", h);
            cur = h.next;

            h.ty.drop_obj(h);   // invalidates `h`
        }

        cur = self.first;
        while cur.is_some() {
            let h = cur.unwrap();
            //println!("drop {:p}", h);

            cur = h.next;
            h.ty.drop_obj(h);
        }
    }
}

impl Encoder for Gc {
    type Error = ();

    fn emit_nil(&mut self) -> Result<(), ()> { Ok(()) }
    fn emit_usize(&mut self, _v: usize) -> Result<(), ()> { Ok(()) }
    fn emit_u64(&mut self, _v: u64) -> Result<(), ()> { Ok(()) }
    fn emit_u32(&mut self, _v: u32) -> Result<(), ()> { Ok(()) }
    fn emit_u16(&mut self, _v: u16) -> Result<(), ()> { Ok(()) }
    fn emit_u8(&mut self, _v: u8) -> Result<(), ()> { Ok(()) }
    fn emit_isize(&mut self, _v: isize) -> Result<(), ()> { Ok(()) }
    fn emit_i64(&mut self, _v: i64) -> Result<(), ()> { Ok(()) }
    fn emit_i32(&mut self, _v: i32) -> Result<(), ()> { Ok(()) }
    fn emit_i16(&mut self, _v: i16) -> Result<(), ()> { Ok(()) }
    fn emit_i8(&mut self, _v: i8) -> Result<(), ()> { Ok(()) }
    fn emit_bool(&mut self, _v: bool) -> Result<(), ()> { Ok(()) }
    fn emit_f64(&mut self, _v: f64) -> Result<(), ()> { Ok(()) }
    fn emit_f32(&mut self, _v: f32) -> Result<(), ()> { Ok(()) }
    fn emit_char(&mut self, _v: char) -> Result<(), ()> { Ok(()) }
    fn emit_str(&mut self, _v: &str) -> Result<(), ()> { Ok(()) }
    fn emit_enum<F>(&mut self, _name: &str, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_enum_variant<F>(&mut self, _v_name: &str, _v_id: usize, _len: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_enum_variant_arg<F>(&mut self, _a_idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_enum_struct_variant<F>(&mut self, _v_name: &str, _v_id: usize, _len: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_enum_struct_variant_field<F>(&mut self, _f_name: &str, _f_idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_struct<F>(&mut self, _name: &str, _len: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_struct_field<F>(&mut self, _f_name: &str, _f_idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_tuple<F>(&mut self, _len: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_tuple_arg<F>(&mut self, _idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_tuple_struct<F>(&mut self, _name: &str, _len: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_tuple_struct_arg<F>(&mut self, _f_idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_option<F>(&mut self, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_option_none(&mut self) -> Result<(), ()> { Ok(()) }
    fn emit_option_some<F>(&mut self, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_seq<F>(&mut self, _len: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_seq_elt<F>(&mut self, _idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_map<F>(&mut self, _len: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_map_elt_key<F>(&mut self, _idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
    fn emit_map_elt_val<F>(&mut self, _idx: usize, f: F) -> Result<(), ()>
        where F: FnOnce(&mut Self) -> Result<(), ()> { f(self) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use value::*;
    use vm::VM;
    use array::Array;

    use rustc_serialize::{Encodable, Encoder};

    /// Dummy type managed by the GC. Calls closure with `val` when dropped.
    pub struct Dummy {
        val: u32,
        on_drop: Box<FnMut(u32)>,
    }

    impl Dummy {
        fn new(val: u32, on_drop: Box<FnMut(u32)>) -> Dummy {
            Dummy {
                val: val,
                on_drop: on_drop,
            }
        }
    }

    impl Encodable for Dummy {
        fn encode<E: Encoder>(&self, _enc: &mut E) -> Result<(), E::Error> {
            Ok(())
        }
    }

    impl Drop for Dummy {
        fn drop(&mut self) {
            (*self.on_drop)(self.val);
        }
    }

    #[test]
    fn preserve_roots() {
        let root = Array::new(vec![TNil, TBool(true), TBool(false)]);
        let root2 = "test".to_string();
        let mut gc = Gc::new();
        let rref = gc.root(root);
        let sref = gc.root(root2);

        unsafe {
            assert_eq!(rref.get().len(), 3);
            assert_eq!(**rref.get(), vec![TNil, TBool(true), TBool(false)]);
            let s: &str = sref.get().as_ref();
            assert_eq!(s, "test");
        }

        assert_eq!(gc.collect(&mut VM::new()), 0);

        unsafe {
            assert_eq!(rref.get().len(), 3);
            assert_eq!(**rref.get(), vec![TNil, TBool(true), TBool(false)]);
            let s: &str = sref.get().as_ref();
            assert_eq!(s, "test");
        }
    }

    #[test]
    fn empty() {
        let mut gc = Gc::new();
        assert_eq!(gc.collect(&mut VM::new()), 0);
    }

    /// Tests if the GC actually collects unreachable objects
    #[test]
    fn collect() {
        let a = Array::new(vec![TNil, TBool(true), TBool(false)]);
        let mut gc = Gc::new();
        let rref = gc.register(a);

        unsafe {
            assert_eq!(rref.get().len(), 3);
            assert_eq!(**rref.get(), vec![TNil, TBool(true), TBool(false)]);
        }

        assert_eq!(gc.collect(&mut VM::new()), 1);
        assert_eq!(gc.collect(&mut VM::new()), 0);
    }

    /// Test if the drop glue is run. I haven't found an easy way for the closure to return a value
    /// to the outside, so I just panic in the drop handler.
    #[test] #[should_panic(expected = "dropped 42")]
    fn drop() {
        let d = Dummy::new(42, Box::new(|val| {
            panic!("dropped {}", val);
        }));
        let mut gc = Gc::new();
        gc.register(d);
        gc.collect(&mut VM::new());
    }

    /// Test if the GC correctly drops all objects when it is dropped.
    #[test] #[should_panic(expected = "dropped 123")]
    fn drop_gc() {
        let d = Dummy::new(123, Box::new(|val| {
            panic!("dropped {}", val);
        }));
        let mut gc = Gc::new();
        gc.register(d);
    }

    /// Test if the GC correctly drops rooted objects when it is dropped.
    #[test] #[should_panic(expected = "dropped 123")]
    fn drop_rooted() {
        let d = Dummy::new(123, Box::new(|val| {
            panic!("dropped {}", val);
        }));
        let mut gc = Gc::new();
        gc.root(d);
    }

    /// Tests if the GC correctly traces all types of objects and marks the reachable objects.
    #[test]
    fn trace() {
        let mut gc = Gc::new();
        let s = gc.register("a test string".to_string());
        let arr = gc.register(Array::new(vec![TStr(s), TNil]));
        let tbl = gc.register(table!(
            TBool(true) => TInt(0),
            TStr(s) => TBool(false),
        ));
        let root = Array::new(vec![TArray(arr), TStr(s), TInt(9), TTable(tbl)]);
        gc.root(root);

        // TODO test functions. this requires functions to work...

        assert_eq!(gc.collect(&mut VM::new()), 0);
    }
}
