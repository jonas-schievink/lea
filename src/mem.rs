//! Memory management interface and garbage collection.

use program::Function;
use array::Array;
use table::Table;

use rustc_serialize::{Encoder, Encodable};

use std::mem::{transmute, size_of, replace};
use std::ops::{Deref, DerefMut};
use std::boxed::into_raw;

#[repr(usize)]
pub enum GcObj {
    String(Wrap<String>),
    Table(Wrap<Table>),
    Array(Wrap<Array>),
    Function(Wrap<Function>),
}

impl GcObj {
    fn get_header(&self) -> &GcHeader {
        unsafe {
            let addr: usize = transmute(self);
            transmute(addr + size_of::<usize>())
        }
    }

    fn get_header_mut(&mut self) -> &mut GcHeader {
        unsafe {
            let addr: usize = transmute(self);
            transmute(addr + size_of::<usize>())
        }
    }
}

pub struct GcHeader {
    /// Next object in GC object list
    next: Option<Box<GcObj>>,
    /// true if this object is considered reachable. Only valid after mark phase.
    marked: bool,
}

pub trait ToGcObj : Encodable {
    fn to_gc_obj(self, header: GcHeader) -> GcObj;
}

pub struct Wrap<T: ToGcObj> {
    _header: GcHeader,
    obj: T,
}

impl <T: ToGcObj> Wrap<T> {
    fn new(header: GcHeader, obj: T) -> Wrap<T> {
        Wrap {
            _header: header,
            obj: obj,
        }
    }
}

impl <T: ToGcObj> Deref for Wrap<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.obj
    }
}

impl <T: ToGcObj> DerefMut for Wrap<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.obj
    }
}

macro_rules! impl_gc_obj {
    ( $( $name:ident ),+ ) => { $(
        impl ToGcObj for $name {
            fn to_gc_obj(self, header: GcHeader) -> GcObj {
                GcObj::$name(Wrap::new(header, self))
            }
        }
    )+ };
}

impl_gc_obj!(String, Table, Array, Function);

/// Unsafe, traced reference to a `GcObj`. Implements `PartialEq` and `Eq` in terms of pointer
/// equality and `Hash` by using the address of the object (these traits are implemented via
/// `#[derive]`).
#[allow(raw_pointer_derive)]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct GcRef<T: ToGcObj>(*mut T);

impl <T: ToGcObj> GcRef<T> {
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

impl <T: ToGcObj> Encodable for GcRef<T> {
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
    first: Option<Box<GcObj>>,
    /// Start of rooted GC object list
    first_root: Box<GcObj>,
}

impl Gc {
    /// Creates a new garbage collector that knows about no objects.
    pub fn new<T: ToGcObj>(initial_root: T) -> Gc {
        Gc {
            first: None,
            first_root: Box::new(initial_root.to_gc_obj(GcHeader {
                next: None,
                marked: false,  // ignored
            })),
        }
    }

    /// Roots the given object.
    pub fn root<T: ToGcObj>(&mut self, t: T) -> GcRef<T> {
        unsafe {
            let first_root = replace(&mut self.first_root, transmute(0usize));   // null pointer box, careful!
            let b = Box::new(t.to_gc_obj(GcHeader {
                next: Some(first_root),
                marked: false,  // ignored
            }));

            let wrap_addr: *mut GcObj = into_raw(b);
            self.first_root = Box::from_raw(wrap_addr);
            let obj_addr = wrap_addr as usize + size_of::<GcHeader>();
            GcRef(transmute(obj_addr))
        }
    }

    // TODO unroot (maybe later, and if the need for Rooted<T> arises)

    /// Transfers ownership of a garbage-collected object to the GC and returns a garbage-collected
    /// reference to it. The GC is now in charge of managing the object and will dispose it if no
    /// `GcRef`s to it are found during a collection.
    pub fn register<T: ToGcObj>(&mut self, t: T) -> GcRef<T> {
        let first = replace(&mut self.first, None);
        let b = Box::new(t.to_gc_obj(GcHeader {
            next: first,
            marked: false,
        }));

        unsafe {
            let wrap_addr: *mut GcObj = into_raw(b);
            self.first = Some(Box::from_raw(wrap_addr));

            let obj_addr = wrap_addr as usize + size_of::<GcHeader>();
            GcRef(transmute(obj_addr))
        }
    }

    /// Performs an atomic garbage collection (stop-the-world).
    pub fn collect(&mut self) {
        self.mark();
        self.sweep();
    }

    /// The mark phase. Recursively traces all reachable objects, starting at the GC roots.
    fn mark(&mut self) {
        let mut cur: Option<&mut GcObj> = unsafe { Some(transmute(&self.first_root)) };

        while cur.is_some() {
            let obj = cur.unwrap();
            self.traverse_obj(obj);

            let next = match obj.get_header_mut().next {
                Some(ref mut b) => Some(b.deref_mut()),
                None => None,
            };
            cur = next;
        }
    }

    /// Iterates through the object list and frees all unmarked objects. Resets the mark bit to
    /// unmarked for all other objects.
    fn sweep(&mut self) {
        let mut cur: Option<&mut GcObj> = match self.first {
            Some(ref mut b) => Some(b.deref_mut()),
            None => None,
        };

        let mut next: Option<&mut GcObj> = match cur {
            Some(ref obj) => match obj.get_header().next {
                Some(ref b) => Some(unsafe { transmute(b.deref()) }),
                None => None,
            },
            None => None,
        };

        while cur.is_some() {
            let h = cur.unwrap().get_header();
            if !h.marked {
                // drop object from list
            }

            cur = next;
            next = match cur {
                Some(ref obj) => match obj.get_header().next {
                    Some(ref b) => Some(unsafe { transmute(b.deref()) }),
                    None => None,
                },
                None => None,
            };
        }
    }

    #[allow(unused_must_use)]
    fn traverse_obj(&mut self, obj: &mut GcObj) {
        match *obj {
            GcObj::String(_) => {},
            GcObj::Table(ref mut t) => {
                t.encode(self);
            },
            GcObj::Array(ref mut a) => {
                a.encode(self);
            },
            GcObj::Function(ref mut f) => {
                f.encode(self);
            },
        }
    }

    /// Marks an object as reachable and traces its references (if the object is traceable).
    #[allow(unused_must_use)]
    unsafe fn process<T: ToGcObj>(&mut self, ptr: &GcRef<T>) {
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
/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let root = Array::new(vec![]);
        let gc = Gc::new(root);
    }
}
*/
