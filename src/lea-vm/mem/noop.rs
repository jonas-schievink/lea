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

use super::*;

#[derive(Default)]
pub struct NoopGc;

impl <'gc> GcStrategy<'gc> for NoopGc {
    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_step(&self, _: &mut VM<Self>) {}

    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_atomic(&self, _: &mut VM<Self>) {}

    /// Allocates `T` on the heap and leaks it.
    #[inline]
    fn register_obj<T: GcObj + 'gc>(&'gc self, t: T) -> TracedRef<'gc, T> {
        // The transmute is safe, but leaks the object (this is intended).
        let ptr: *const T = unsafe { transmute::<_, *const T>(Box::new(t)) };

        TracedRef {
            ptr: ptr,
            phantom: PhantomData,
        }
    }

    unsafe fn root<T: GcObj + 'gc>(&'gc self, obj: TracedRef<'gc, T>) -> Rooted<'gc, T, Self> {
        Rooted {
            ptr: obj.ptr,
            gc: self,
        }
    }

    fn unroot<T: GcObj + 'gc>(&self, rooted: *const T) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    use mem::*;

    #[test]
    fn rooted() {
        let gc = NoopGc::default();
        let traced: TracedRef<String> = gc.register_obj("teststr".to_string());
        let traced2: TracedRef<String> = gc.register_obj("Second Test-String".to_string());
        let rooted: Rooted<String, _> = unsafe { gc.root(traced) };
        let rooted2 = unsafe { gc.root(traced) };
        let rooted_b = unsafe { gc.root(traced2) };

        drop(rooted_b);

        // `rooted` and `rooted2` are unrooted in the wrong order, but since they are equal, this
        // doesn't matter
        drop(rooted);
        drop(rooted2);
    }

    #[test] #[ignore] #[should_panic(expected = "unrooting order")]
    fn unroot_order() {
        let gc = NoopGc::default();
        let traced = gc.register_obj("teststr".to_string());
        let traced2 = gc.register_obj("teststr2".to_string());
        let rooted = unsafe { gc.root(traced) };
        let rooted2 = unsafe { gc.root(traced2) };

        // this panics, since `rooted2` was rooted after `rooted` and thus needs to be unrooted
        // before `rooted`.
        drop(rooted);

        drop(rooted2);
    }

    #[test]
    fn mut_ref() {
        let gc = NoopGc::default();
        let traced = gc.register_obj("teststr".to_string());
        let r: &String = unsafe { traced.get_ref() };
        let m: &mut String = unsafe { traced.get_mut_ref() };

        // The references now alias. User code must avoid this.
        assert_eq!(r as *const String, m as *const String);
    }
}
