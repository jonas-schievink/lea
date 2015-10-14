//! Version of `Box<T>` that is always 64 bits large.

#![allow(dead_code)]    // FIXME Actually use this

use std::ops::{Deref, DerefMut};

#[cfg(target_pointer_width = "32")]
pub struct Box64<T>(Box<T>, u32);
#[cfg(target_pointer_width = "64")]
pub struct Box64<T>(Box<T>);

impl<T> Box64<T> {
    #[cfg(target_pointer_width = "32")]
    pub fn new(obj: T) -> Self {
        Box64(Box::new(obj), 0)
    }

    #[cfg(target_pointer_width = "64")]
    pub fn new(obj: T) -> Self {
        Box64(Box::new(obj))
    }
}

impl<T> Deref for Box64<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.0 }
}

impl<T> DerefMut for Box64<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.0 }
}

#[test]
fn size() {
    assert_eq!(::std::mem::size_of::<Box64<()>>(), 64 / 8);
}
