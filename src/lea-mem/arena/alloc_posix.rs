//! Arena allocation routines for Unix/POSIX systems using `posix_memalign`.
//!
//! Platform-specific allocation routines need to provide an `ArenaBox` type to manage the arena (it
//! should deallocate the arena on `Drop` and implement `Deref` and `DerefMut`).

use super::{ARENA_SIZE, Arena};

use libc::{c_void, c_int, size_t, EINVAL, ENOMEM};
use libc::free;

use std::ptr;
use std::ops::{Deref, DerefMut};

extern "C" {
    fn posix_memalign(memptr: *mut *mut c_void, alignment: size_t, size: size_t) -> c_int;
}

pub struct ArenaBox(*mut Arena);

impl Drop for ArenaBox {
    fn drop(&mut self) {
        unsafe {
            free(self.0 as *mut c_void)
        }
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
    let mut memptr: *mut c_void = ptr::null_mut();
    let ret = posix_memalign(&mut memptr, ARENA_SIZE as size_t, ARENA_SIZE as size_t);

    match ret {
        0 => {
            // Success
            return ArenaBox(memptr as *mut Arena)
        }
        EINVAL => {
            // "The alignment argument was not a power of two, or was not a multiple of
            // sizeof(void*).
            // This can't possibly happen, since `ARENA_SIZE` is at least `1024*64`
            unreachable!();
        }
        ENOMEM => {
            // FIXME: What to do properly here?
            panic!("OOM");
        }
        _ => unreachable!()
    }
}
