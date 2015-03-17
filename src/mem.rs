//! Memory management interface and garbage collection.

use program::{Function, FunctionProto};
use array::Array;
use table::Table;

use rustc_serialize::{Encoder, Encodable};

use std::mem::{transmute, size_of, replace};
use std::boxed::into_raw;
use std::hash::{Hash, Hasher};
use std::fmt;


/// A garbage-collectable object. Provides a method to get its `GcType` for proper destruction by
/// the GC.
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
        // its own module

        #[cfg(not(test))]
        mod gctype {
            #[derive(Debug, Copy)]
            pub enum GcType {
                $($name,)+
            }
        }

        #[cfg(test)]
        mod gctype {
            #[derive(Debug, Copy)]
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
    /// type of the object
    ty: GcType,

    // object data lies here
}

#[test]
fn header_size() {
    // important for proper alignment of the object behind the header: GcHeader must be a multiple
    // of the pointer size
    assert_eq!(size_of::<GcHeader>(), size_of::<usize>() * 2);
}

impl GcHeader {
    /// Gets a mutable reference to the object behind this header.
    unsafe fn get_obj<T: GcObj>(&self) -> &T {
        let addr: usize = transmute(self);

        transmute(addr + size_of::<GcHeader>())
    }
}


/// Unsafe, traced reference to a `GcObj`. Implements `PartialEq` and `Eq` in terms of pointer
/// equality and `Hash` by using the address of the object.
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

    /// Gets the corresponding GcHeader for the object, assuming it is wrapped inside `Wrap<T>`
    /// (which it should be, since the GC manages all `GcRef`s).
    fn get_header_mut(&self) -> &mut GcHeader {
        let addr = self.0 as usize - size_of::<GcHeader>();

        unsafe { transmute(addr) }
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
    fn hash<H>(&self, state: &mut H) where H: Hasher {
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


pub struct Gc {
    /// Start of GC object list
    first: Option<&'static GcHeader>,
    /// Start of rooted GC object list
    first_root: Option<&'static GcHeader>,
}

impl Gc {
    /// Creates a new garbage collector that knows about no objects and has no roots.
    pub fn new() -> Gc {
        Gc {
            first: None,
            first_root: None,
        }
    }

    /// Roots the given object.
    pub fn root<T: GcObj>(&mut self, t: T) -> GcRef<T> {
        let first_root = replace(&mut self.first_root, None);
        let b = Box::new((GcHeader {
            next: first_root,
            marked: false,
            ty: T::get_type(),
        }, t));

        unsafe {
            let addr: *mut (GcHeader, T) = into_raw(b);
            let (ref header_addr, ref obj_addr) = *addr;
            self.first_root = Some(transmute(header_addr));

            GcRef(transmute(obj_addr))
        }
    }

    // TODO unroot (maybe later, and if the need for Rooted<T> arises)

    /// Transfers ownership of a garbage-collected object to the GC and returns a garbage-collected
    /// reference to it. The GC is now in charge of managing the object and will dispose it if no
    /// `GcRef`s to it are found during a collection.
    pub fn register<T: GcObj>(&mut self, t: T) -> GcRef<T> {
        let first = replace(&mut self.first, None);
        let b = Box::new((GcHeader {
            next: first,
            marked: false,
            ty: T::get_type(),
        }, t));

        unsafe {
            let addr: *mut (GcHeader, T) = into_raw(b);
            let (ref header_addr, ref obj_addr) = *addr;
            self.first = Some(transmute(header_addr));

            //println!("reg {:p}", addr);

            GcRef(transmute(obj_addr))
        }
    }

    /// Performs an atomic garbage collection (stop-the-world).
    ///
    /// Returns the number of objects that were collected.
    pub fn collect(&mut self) -> usize {
        self.mark();
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

    /// Iterates through the object list and frees all unmarked objects. Resets the mark bit to
    /// unmarked for all other objects.
    fn sweep(&mut self) -> usize {
        let mut swept: usize = 0;
        let mut prev: Option<&'static GcHeader> = None;
        let mut cur: Option<&'static GcHeader> = self.first;

        while cur.is_some() {
            let h = cur.unwrap();

            if !h.marked {
                //println!("sweeping {:p}", h);

                // remove from object list
                match prev {
                    Some(prev_header) => {
                        // TODO we should *probably* use cells
                        let mut_header: &mut GcHeader = unsafe { transmute(prev_header) };
                        mut_header.next = h.next;
                    }
                    None => {
                        self.first = cur;
                    },
                }

                // run drop glue
                h.ty.drop_obj(h);

                swept += 1;
            }

            // unmark so the object can be collected in the next collection cycle
            // TODO use cells for this (and figure out how slow they are)
            unsafe { transmute::<_, &mut GcHeader>(h).marked = false; }

            prev = cur;
            cur = match prev {
                Some(h) => h.next,
                None => None,
            };
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
    use array::Array;
    use value::*;

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
            assert_eq!(sref.get().as_slice(), "test");
        }

        assert_eq!(gc.collect(), 0);

        unsafe {
            assert_eq!(rref.get().len(), 3);
            assert_eq!(**rref.get(), vec![TNil, TBool(true), TBool(false)]);
            assert_eq!(sref.get().as_slice(), "test");
        }
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

        assert_eq!(gc.collect(), 1);
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
        gc.collect();
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

        assert_eq!(gc.collect(), 0);
    }
}
