//! No-op Garbage Collector. This implementation doesn't collect garbage at all, so it is very
//! simple: Neither Finalizers nor `Drop` glue needs to run, Weak references can be ignored (they
//! will always be valid) and we don't have to keep track of all live objects.
//!
//! As a simplification, this also won't run `Drop` on GC objects when a `NoopGc` is dropped.
//!
//! The `NoopGc` strategy might even prove useful in practice, when a script is supposed to only
//! run very shortly and/or not create much garbage.

use std::any::Any;
use std::mem::transmute;

use super::*;

#[derive(Default)]
pub struct NoopGc;

impl GcStrategy for NoopGc {
    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_step<G: GcCallback>(&mut self, _: &mut G) {}

    /// Does nothing (hence `NoopGc`)
    #[inline]
    fn collect_atomic<G: GcCallback>(&mut self, _: &mut G) {}

    /// Allocates `T` on the heap and leaks it.
    #[inline]
    fn register_obj<T: Any>(&mut self, t: T) -> TracedRef<T> {
        // The transmute is safe, but leaks the object (this is intended).
        let ptr: *const T = unsafe { transmute(Box::new(t)) };

        TracedRef {
            ptr: ptr,
        }
    }
}
