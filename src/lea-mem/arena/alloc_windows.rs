//! Arena allocation routines for Windows. Not yet implemented.

use super::{ARENA_SIZE, Arena};

use std::ptr;
use std::ops::{Deref, DerefMut};

pub struct ArenaBox(*mut Arena);

impl Drop for ArenaBox {
    fn drop(&mut self) {

    }
}

impl Deref for ArenaBox {
    type Target = Arena;
    fn deref(&self) -> &Arena {
        unsafe { &*self.0 }
    }
}

impl DerefMut for ArenaBox {
    fn deref_mut(&mut self) -> &mut Arena {
        unsafe { &mut *self.0 }
    }
}

/// Allocates memory for an arena and returns it as an `ArenaBox`.
///
/// This is `unsafe` because the returned memory is uninitialized.
pub unsafe fn alloc_arena() -> ArenaBox {
    unimplemented!();
}
