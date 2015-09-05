//! A simple Stop-The-World atomic GC.

// XXX This is incomplete, doesn't really work and stores nasty pointer offsets in TracedRefs!

use super::*;

use string::Str;

use std::any::Any;
use std::mem::{replace, transmute};
use std::raw::TraitObject;


struct Wrapped {
    next: Option<Box<Wrapped>>,
    marked: bool,
    obj: Box<Any>,
}

impl Wrapped {
    fn new(obj: Box<Any>) -> Wrapped {
        Wrapped {
            next: None,
            marked: false,
            obj: obj,
        }
    }
}

#[derive(Default)]
pub struct Stw {
    obj_count: usize,
    first_obj: Option<Box<Wrapped>>,
}

impl Stw {
    /// Drop all objects that do not have their `marked` flag set
    fn sweep(&mut self) {
        /// Returns the next reachable object
        fn find_reachable(start: Box<Wrapped>) -> Option<Box<Wrapped>> {
            if start.marked {
                Some(start)
            } else {
                match start.next {
                    Some(next) => find_reachable(next),
                    None => None,
                }
            }
        }

        let mut cur = replace(&mut self.first_obj, None);
        while let Some(box_wrapped) = cur {
            cur = match find_reachable(box_wrapped) {
                None => None,
                Some(t) => t.next,
            }
        }
    }

    /// Reset the `marked` flag of all objects to `false`
    fn reset(&mut self) {
        let mut cur = self.first_obj.as_mut();
        while let Some(box_wrapped) = cur {
            box_wrapped.marked = false;
            cur = box_wrapped.next.as_mut();
        }
    }
}

impl GcStrategy for Stw {
    fn collect_step<G: GcCallback>(&mut self, cb: &mut G) {
        self.collect_atomic(cb);
    }

    fn collect_atomic<G: GcCallback>(&mut self, cb: &mut G) {
        // list of reachable objects that have not yet been traced
        let mut worklist: Vec<&mut Wrapped> = Vec::new();

        cb.for_each_root(|root| {
            let wrapped: *mut Wrapped = root.ptr as *mut Wrapped;
            let wrapped: &mut Wrapped = unsafe { &mut *wrapped };
            wrapped.marked = true;
            worklist.push(wrapped);
        });

        // while the worklist is non-empty, pop an object, trace it (marking reachable objects and
        // adding them to the worklist), repeat
        while let Some(_) = worklist.pop() {
            // This is basically impossible without specialization or massive code clutter
            // (or negative trait bounds?)
            unimplemented!();
        }

        self.sweep();
        self.reset();
    }

    fn register_obj<T: Any>(&mut self, t: T) -> TracedRef<T> {
        let boxed = Box::new(t) as Box<Any>;

        let mut new = Box::new(Wrapped::new(boxed));
        let ptr: *const Wrapped = &*new;
        new.next = replace(&mut self.first_obj, None);
        replace(&mut self.first_obj, Some(new));

        self.obj_count += 1;

        // this ref points to the wrapper, not the T itself
        TracedRef {
            ptr: ptr as *const T,
        }
    }

    fn intern_str<T: Into<Str>>(&mut self, _: T) -> TracedRef<Str> { unimplemented!() }

    unsafe fn get_ref<'a, T: Any>(&'a self, t: TracedRef<T>) -> &'a T {
        let ptr = t.ptr as *const Wrapped;
        let gcref: &Any = &*(*ptr).obj;
        let traitobj: TraitObject = transmute(gcref);

        transmute(traitobj.data)
    }

    unsafe fn get_mut<'a, T: Any>(&'a mut self, t: TracedRef<T>) -> &'a mut T {
        let ptr = t.ptr as *mut Wrapped;
        let gcref: &mut Any = &mut *(*ptr).obj;
        let traitobj: TraitObject = transmute(gcref);

        transmute(traitobj.data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mem::*;

    use std::any::Any;

    #[test]
    fn create() {
        // Checks GC creation, object registration and dereferencing
        let mut gc = Stw::default();
        let traced = gc.register_obj("test".to_string());

        assert_eq!(&*unsafe { gc.get_ref(traced) }, "test");
    }

    #[test]
    fn get_mut() {
        // Checks GC object mutation (via mutable reference obtained through `gc.get_mut`)
        let mut gc = Stw::default();
        let traced = gc.register_obj(0);

        assert_eq!(*unsafe { gc.get_ref(traced) }, 0);
        unsafe {
            *gc.get_mut(traced) = 1;
        }
        assert_eq!(*unsafe { gc.get_ref(traced) }, 1);
    }

    #[test]
    fn drop_objs() {
        // Check that objects are correctly dropped when the GC is dropped (no collection)
        static mut DROPPED: bool = false;
        struct D;

        impl Drop for D {
            fn drop(&mut self) {
                unsafe { DROPPED = true; }
            }
        }

        let mut gc = Stw::default();
        gc.register_obj(D);

        drop(gc);

        assert_eq!(unsafe { DROPPED }, true);
    }

    #[test]
    fn collect() {
        // Check that objects are collected (no roots, no reachable objects)
        static mut DROPPED: u32 = 0;

        struct D;
        impl Drop for D {
            fn drop(&mut self) {
                unsafe { DROPPED += 1; }
            }
        }

        struct CB;
        impl GcCallback for CB {
            fn for_each_root<F>(&self, _: F) where F: FnMut(TracedRef<Any>) {
                // No roots
            }
        }

        let mut gc = Stw::default();
        gc.register_obj(D);
        gc.collect_atomic(&mut CB);
        assert_eq!(unsafe { DROPPED }, 1);

        // No double-drop
        drop(gc);
        assert_eq!(unsafe { DROPPED }, 1);
    }
}
