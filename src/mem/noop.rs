//! No-op Garbage Collector. This implementation doesn't collect garbage at all, so it is very
//! simple: Neither Finalizers nor `Drop` glue needs to run, Weak references can be ignored (they
//! will always be valid) and we don't have to keep track of all live objects.
//!
//! As a simplification, this also won't run `Drop` on GC objects when a `NoopGc` is dropped.
//!
//! The `NoopGc` strategy is used to perform very basic tests on the GC infrastructure that don't
//! involve actual GC logic. It might even prove useful in practice, when a script is supposed to
//! only run very shortly and/or not create much garbage.

use std::marker::PhantomData;
use std::mem::transmute;

use vm::VM;

use super::{TracedRef, Roots, GcStrategy, GcObj};

pub struct NoopGc {
    /// This collector ignores the root set, but it is required to implement `GcStrategy`.
    roots: Roots,
}

impl Default for NoopGc {
    #[inline]
    fn default() -> NoopGc {
        NoopGc {
            roots: Roots::new(),
        }
    }
}

impl GcStrategy for NoopGc {
    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_step(&mut self, _: &mut VM<NoopGc>) {}

    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_atomic(&mut self, _: &mut VM<NoopGc>) {}

    /// Allocates `T` on the heap and leaks it.
    #[inline]
    fn register_obj<'gc, 'a: 'gc, T: GcObj>(&'gc mut self, t: T) -> TracedRef<'a, T> {
        // The transmute is safe, but leaks the object (this is intended).
        let ptr: *const T = unsafe { transmute::<_, *const T>(Box::new(t)) };

        TracedRef {
            ptr: ptr,
            phantom: PhantomData,
        }
    }

    #[inline]
    fn get_roots<'gc>(&'gc self) -> &'gc Roots {
        &self.roots
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mem::*;

    #[test]
    fn rooted() {
        let mut gc = NoopGc::default();
        let traced: TracedRef<String> = gc.register_obj("teststr".to_string());
        let traced2: TracedRef<String> = gc.register_obj("Second Test-String".to_string());
        let rooted: Rooted<String> = unsafe { traced.root(&gc) };
        let rooted2 = unsafe { traced.root(&gc) };
        let rooted_b = unsafe { traced2.root(&gc) };
        assert_eq!(gc.get_roots().len(), 3);

        drop(rooted_b);
        assert_eq!(gc.get_roots().len(), 2);

        // `rooted` and `rooted2` are unrooted in the wrong order, but since they are equal, this
        // doesn't matter
        drop(rooted);
        assert_eq!(gc.get_roots().len(), 1);
        drop(rooted2);
        assert_eq!(gc.get_roots().len(), 0);
    }

    #[test] #[should_panic(expected = "unrooting order")]
    fn unroot_order() {
        let mut gc = NoopGc::default();
        let traced = gc.register_obj("teststr".to_string());
        let traced2 = gc.register_obj("teststr2".to_string());
        let rooted = unsafe { traced.root(&gc) };
        let rooted2 = unsafe { traced2.root(&gc) };

        // this panics, since `rooted2` was rooted after `rooted` and thus needs to be unrooted
        // before `rooted`.
        drop(rooted);

        drop(rooted2);
    }

    #[test]
    fn mut_ref() {
        let mut gc = NoopGc::default();
        let traced = gc.register_obj("teststr".to_string());
        let r: &String = unsafe { traced.get_ref() };
        let m: &mut String = unsafe { traced.get_mut_ref() };

        // The references now alias. User code must avoid this.
        assert_eq!(r as *const String, m as *const String);
    }
}
