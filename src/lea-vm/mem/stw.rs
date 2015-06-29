//! A simple Stop-The-World atomic GC.

use super::*;

use vm::VM;

use std::cell::{Cell, RefCell};
use std::mem::swap;


/// A block stores a fixed number of garbage-collected objects along with a small metadata header.
struct Wrapped<'a> {
    next: Option<Box<Wrapped<'a>>>,
    marked: bool,
    obj: Box<GcObj + 'a>,
}

impl <'a> Wrapped<'a> {
    fn new(obj: Box<GcObj + 'a>) -> Wrapped<'a> {
        Wrapped {
            next: None,
            marked: false,
            obj: obj,
        }
    }
}

#[derive(Default)]
pub struct Stw<'gc> {
    roots: Roots,
    obj_count: Cell<usize>,
    first_obj: RefCell<Option<Box<Wrapped<'gc>>>>,
}

impl <'gc> Stw<'gc> {
    fn obj_count(&self) -> usize {
        self.obj_count.get()
    }
}

impl <'gc> GcStrategy<'gc> for Stw<'gc> {
    fn collect_step(&mut self, vm: &mut VM<'gc, Self>) {
        self.collect_atomic(vm);
    }

    fn collect_atomic(&mut self, _: &mut VM<'gc, Self>) {
        let worklist: Vec<&'gc Wrapped<'gc>> = Vec::new();

        unimplemented!();
    }

    fn register_obj<T: GcObj + 'gc>(&self, t: T) -> TracedRef<'gc, T> {
        let boxed = Box::new(t);
        let ptr = &*boxed as *const T;
        let boxed = boxed as Box<GcObj + 'gc>;

        let mut cur = self.first_obj.borrow_mut();
        let mut new = Wrapped::new(boxed);
        swap(&mut new.next, &mut *cur);

        self.obj_count.set(self.obj_count.get() + 1);

        TracedRef {
            ptr: ptr,
            phantom: ::std::marker::PhantomData,
        }
    }

    #[inline]
    fn get_roots(&self) -> &Roots {
        &self.roots
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mem::GcStrategy;

    #[test]
    fn create() {
        let gc = Stw::default();
        gc.register_obj("test".to_string());
    }
}
