//! Arena allocator inspired by LuaJIT's [new GC](http://wiki.luajit.org/New-Garbage-Collector).
//!
//! The allocator uses fixed-size arenas to allocate all objects. An arena can have any power-of-two
//! size from 64 KiB up to 1 MiB. Arenas are split up into 16 Byte cells. 1/64th of the arena is
//! reserved for metadata. Arenas must be aligned to their size, which requires platform-specific
//! allocation code (there seems to be no suitable "aligned allocation" crate).

// TODO: Evaluate the use of huge pages for arena allocation (on each OS separately). Might want to
// switch dynamically, to prevent too much wasted memory (does this really matter?).

mod box64;

use GcStrategy;
use GcCallback;
use string::Str;
use TracedRef;

use aligned_alloc::{aligned_alloc, aligned_free};

use std::any::Any;
use std::mem;
use std::ops::{Deref, DerefMut};

/// Selects the size of an arena:
///
/// * 0 = 64 KiB
/// * 1 = 128 KiB
/// * 2 = 256 KiB
/// * 3 = 512 KiB
/// * 4 = 1 MiB
const ARENA_SIZE_CLASS: usize = 4;


/// Size of each arena in bytes
const ARENA_SIZE: usize = 1 << (16 + ARENA_SIZE_CLASS);
/// Size of the metadata area (always 1/64th of the arena size)
const METADATA_SIZE: usize = ARENA_SIZE / 64;
/// Size of the data area in an arena (area available for allocation) (in Bytes)
const DATA_SIZE: usize = ARENA_SIZE - METADATA_SIZE;

/// Number of cells in the data area.
///
/// There are at most 64512 cells when using 1 MiB arenas, so a cell number always fits in 16 bits.
const CELL_COUNT: u16 = (DATA_SIZE / 16) as u16;

/// First valid cell index / size of metadata in cells.
///
/// This is also the number of unused bits at the start of the bitmaps.
const FIRST_CELL_INDEX: u16 = (METADATA_SIZE / 16) as u16;

/// Number of unused bytes at the start of the bitmaps
const UNUSED_METADATA_BYTES: usize = (FIRST_CELL_INDEX / 8) as usize;

/// Size of the bitmaps in bytes
const BITMAP_SIZE: usize = CELL_COUNT as usize / 8;


type Cell = [u8; 16];

#[repr(C)]
struct Bitmap([u8; BITMAP_SIZE]);

/// The metadata section of an arena contains 2 bitmaps, storing block and mark bits for each cell.
#[repr(C)]
struct Metadata {
    unused_0: [u8; UNUSED_METADATA_BYTES],
    block_bitmap: Bitmap,
    unused_1: [u8; UNUSED_METADATA_BYTES],
    mark_bitmap: Bitmap,
}

impl Default for Metadata {
    fn default() -> Self {
        Metadata {
            unused_0: [0; UNUSED_METADATA_BYTES],
            block_bitmap: Bitmap([0; BITMAP_SIZE]),
            unused_1: [0; UNUSED_METADATA_BYTES],
            mark_bitmap: Bitmap([0; BITMAP_SIZE]),
        }
    }
}

/// An arena. This structure is large, never handle it by-value, only via `ArenaBox`, provided by
/// the platform-specific allocation module.
#[repr(C)]
struct Arena {
    metadata: Metadata,
    cells: [Cell; CELL_COUNT as usize],
}

impl Arena {
    /// Allocates a new `Arena` and returns an `ArenaBox`.
    fn new() -> ArenaBox {
        // `alloc_arena` returns uninitialized memory. The arena doesn't care if the data area is
        // uninitialized, but the metadata area needs to be cleared first.
        unsafe {
            let mut b = ArenaBox::alloc();
            b.metadata = Metadata::default();

            b
        }
    }

    /// Tries to allocate `size` bytes inside this arena. The allocation will always be aligned to
    /// 16 bytes.
    fn alloc(&mut self, size: usize) -> Option<*mut ()> {
        let _cells = (size >> 4).next_power_of_two();
        unimplemented!();
    }
}

/// Manages an `Arena`, allocated with a suitable alignment.
struct ArenaBox(*mut Arena);

impl ArenaBox {
    /// Allocates an uninitialized `Arena`
    unsafe fn alloc() -> ArenaBox {
        ArenaBox(aligned_alloc(ARENA_SIZE, ARENA_SIZE) as *mut Arena)
    }
}

impl Deref for ArenaBox {
    type Target = Arena;
    fn deref(&self) -> &Arena { unsafe { &*self.0 } }
}

impl DerefMut for ArenaBox {
    fn deref_mut(&mut self) -> &mut Arena { unsafe { &mut *self.0 } }
}

impl Drop for ArenaBox {
    fn drop(&mut self) {
        unsafe { aligned_free(self.0 as *mut ()) }
    }
}

/// Arena-based GC
pub struct ArenaGc {
    arenas: Vec<ArenaBox>,
}

impl GcStrategy for ArenaGc {
    fn collect_step<C: GcCallback>(&mut self, cb: &mut C) {
        // TODO Make this incremental
        self.collect_atomic(cb)
    }

    fn collect_atomic<C: GcCallback>(&mut self, _cb: &mut C) { unimplemented!(); }

    fn register_obj<T: Any>(&mut self, _obj: T) -> TracedRef<T> {
        // FIXME: Don't traverse arenas for allocation
        // FIXME: Implement a less naive algorithm for allocation (see LuaJIT wiki)
        // FIXME: Allow higher alignment (or not, depends)
        // FIXME: Allocate large objects in segregated area (or not, we don't really use it)

        let size = mem::size_of::<T>();

        debug_assert!(mem::align_of::<T>() <= 16,
            "align {} too large for arena allocation", mem::align_of::<T>());
        debug_assert!(size <= 64, "size {} too large for arena allocation", size);

        for arena in &mut self.arenas {
            if let Some(ptr) = arena.alloc(size) {
                return TracedRef {
                    ptr: ptr as *const T,
                }
            }
        }

        // no luck, allocate a new arena
        let mut arena = Arena::new();
        let traced_ref = match arena.alloc(size) {
            Some(ptr) => TracedRef {
                ptr: ptr as *const T,
            },
            None => panic!("empty arena couldn't allocate {} bytes", size)
        };

        self.arenas.push(arena);
        traced_ref
    }

    fn intern_str<T: Into<Str>>(&mut self, _s: T) -> TracedRef<Str> { unimplemented!(); }
}

/// Makes sure that the selected arena size is valid
#[test]
fn arena_size() {
    assert!(ARENA_SIZE.is_power_of_two(), "arena size {} not a power of 2", ARENA_SIZE);
    assert!(ARENA_SIZE >= 64 * 1024, "arena size {} smaller than 64 KiB", ARENA_SIZE);
    assert!(ARENA_SIZE <= 1024 * 1024, "arena size {} larger than 1 MiB", ARENA_SIZE);
}

/// Makes sure that all constants have sane values
#[test]
fn consts() {
    assert!(FIRST_CELL_INDEX >= 16 * 8);    // unused bits in the bitmaps == free space
    assert!(METADATA_SIZE >= 1024);
    assert!(UNUSED_METADATA_BYTES >= 16);
    assert_eq!(METADATA_SIZE % 16, 0);
    assert_eq!(METADATA_SIZE * 64, ARENA_SIZE);
    assert_eq!(CELL_COUNT as usize * 16, DATA_SIZE);
}

#[test]
fn struct_size() {
    use std::mem::size_of;

    assert_eq!(size_of::<Arena>(), ARENA_SIZE);
    assert_eq!(size_of::<Metadata>(), METADATA_SIZE);
}
