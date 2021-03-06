//! No-op Garbage Collector. This implementation doesn't collect garbage at all, so it is very
//! simple: Neither Finalizers nor `Drop` glue needs to run, Weak references can be ignored (they
//! will always be valid) and the collection methods do nothing.
//!
//! When the `NoopGc` is dropped, all objects will be dropped along with it.
//!
//! This is the default GC until we have one that works.

use std::any::Any;
use std::mem::replace;
use std::collections::HashMap;

use string::Str;

use super::*;

#[allow(dead_code)]     // possible bug in dead_code lint (the fields *are* used in the drop glue)
struct Boxed {
    obj: Box<Any>,
    next: Option<Box<Boxed>>,
}

#[derive(Default)]
pub struct NoopGc {
    first: Option<Box<Boxed>>,
    strings: HashMap<Box<Str>, TracedRef<Str>>,
}

impl GcStrategy for NoopGc {
    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_step<G: GcCallback>(&mut self, _: &mut G) {}

    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_atomic<G: GcCallback>(&mut self, _: &mut G) {}

    #[inline]
    fn register_obj<T: Any>(&mut self, t: T) -> TracedRef<T> {
        let b = Box::new(t);
        let ptr = &*b as *const T;

        let next = replace(&mut self.first, None);
        let boxed = Boxed {
            obj: b as Box<Any>,
            next: next,
        };

        self.first = Some(Box::new(boxed));

        TracedRef {
            ptr: ptr,
        }
    }

    fn intern_str<T: Into<Str>>(&mut self, s: T) -> TracedRef<Str> {
        let s = s.into();
        if let Some(&r) = self.strings.get(&s) {
            return r
        }

        let b = Box::new(s);
        let r = TracedRef { ptr: &*b };
        self.strings.insert(b, r);

        r
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use GcStrategy;

    #[test]
    fn test_drop() {
        static mut DROPPED: u8 = 0;

        struct DropGc;

        impl Drop for DropGc {
            fn drop(&mut self) {
                unsafe {
                    DROPPED += 1;
                }
            }
        }

        let mut gc = NoopGc::default();
        gc.register_obj(DropGc);
        gc.register_obj("test".to_owned()); // just for fun
        gc.register_obj(DropGc);
        gc.register_obj(());

        drop(gc);

        assert_eq!(unsafe {DROPPED}, 2);
    }
}
