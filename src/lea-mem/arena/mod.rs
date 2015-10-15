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
use std::cmp;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::u16;

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

/// Number of unused bytes at the start of the bitmaps. Two times this is the number of unused Bytes
/// in the arena metadata.
const UNUSED_BITMAP_BYTES: usize = (FIRST_CELL_INDEX / 8) as usize;

/// Size of the bitmaps in bytes
const BITMAP_SIZE: usize = CELL_COUNT as usize / 8;


type Cell = [u8; 16];

#[repr(C)]
struct Bitmap([u8; BITMAP_SIZE]);

impl Bitmap {
    fn get_bit(&self, bit: u16) -> bool {
        let byte_index = bit / 8;
        let bit_off = bit % 8;
        let byte = self.0[byte_index as usize];

        byte & (1 << bit_off) != 0
    }

    fn get(&self, cell: u16) -> bool {
        self.get_bit(cell - FIRST_CELL_INDEX)
    }

    fn set_bit(&mut self, bit: u16, value: bool) {
        let byte_index = bit / 8;
        let bit_off = bit % 8;
        let byte = &mut self.0[byte_index as usize];

        match value {
            true  => *byte |= 1 << bit_off,
            false => *byte &= !(1 << bit_off),
        }
    }

    fn set(&mut self, cell: u16, value: bool) {
        self.set_bit(cell - FIRST_CELL_INDEX, value)
    }
}

/// The metadata section of an arena contains 2 bitmaps, storing block and mark bits for each cell.
#[repr(C)]
struct Metadata {
    unused_0: [u8; UNUSED_BITMAP_BYTES],
    block_bitmap: Bitmap,
    unused_1: [u8; UNUSED_BITMAP_BYTES],
    mark_bitmap: Bitmap,
}

impl Default for Metadata {
    fn default() -> Self {
        let mut this = Metadata {
            unused_0: [0; UNUSED_BITMAP_BYTES],
            block_bitmap: Bitmap([0; BITMAP_SIZE]),
            unused_1: [0; UNUSED_BITMAP_BYTES],
            mark_bitmap: Bitmap([0; BITMAP_SIZE]),
        };

        // Mark the first block as free. This marks the entire arena as free.
        this.mark_bitmap.set(FIRST_CELL_INDEX, true);

        this
    }
}

/// An arena. This structure is large, never handle it by-value, only via `ArenaBox`.
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

    fn cell_index_to_ptr(&self, cell: u16) -> *mut () {
        let self_ptr = self as *const _ as usize;
        let cell_addr = self_ptr + ((cell as usize) << 4);

        cell_addr as *mut ()
    }

    /// Tries to allocate `cells` Cells inside this arena. Return the allocated cell index on
    /// success and `None` on failure.
    fn alloc_cells(&mut self, cells: u16) -> Option<u16> {
        debug_assert!(cells > 0);

        // Find a free block (first cell has: Block = 0, Mark = 1) that's large enough

        // First index of current free block
        let mut block_start = None;
        // Length of our current free block
        let mut block_len = 0;

        for cell in FIRST_CELL_INDEX..CELL_COUNT {
            let block = self.metadata.block_bitmap.get(cell);
            let mark = self.metadata.mark_bitmap.get(cell);

            if block_start.is_none() {
                // We don't have a free block we can follow, so we're only interested in new free
                // blocks.
                if !block && mark {
                    // Start of a free block. Save the index.
                    block_start = Some(cell);
                    block_len = 0;
                }
            } else {
                match (block, mark) {
                    (false, false) => {
                        // Block extent
                        block_len += 1;
                    }
                    (false, true) => {
                        // New free block. Coalesce with our current free block, since they're
                        // adjacent.
                        self.metadata.mark_bitmap.set(cell, false); // Make cell block extent
                        block_len += 1;
                    }
                    (true, _) => {
                        // Start of allocated block. Since our block is too small for the
                        // allocation, we have to start over.
                        block_start = None;
                    }
                }

                if block_len == cells {
                    // Done, cut the block into pieces and allocate. `cell` is the last cell of the
                    // allocated block.

                    let block_start = block_start.unwrap();

                    // Mark the block as allocated:
                    self.metadata.block_bitmap.set(block_start, true);

                    // Mark the following cell as a new free block, if applicable
                    if cell < CELL_COUNT {
                        let block = self.metadata.block_bitmap.get(cell + 1);
                        let mark = self.metadata.mark_bitmap.get(cell + 1);
                        if !block && !mark {
                            // Block extent. Make new free block.
                            self.metadata.mark_bitmap.set(cell + 1, true);
                        }
                    }

                    return Some(block_start)
                }
            }
        }

        None
    }

    /// Tries to allocate `size` bytes inside this arena. The allocation will always be aligned to
    /// 16 bytes.
    fn alloc(&mut self, size: usize) -> Option<*mut ()> {
        let cells = cmp::max(1, size.next_power_of_two());
        debug_assert!(cells <= u16::MAX as usize);

        self.alloc_cells(cells as u16).map(|cell| self.cell_index_to_ptr(cell))
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
    assert!(FIRST_CELL_INDEX >= 1024 / 16,
        "first valid cell index is {}", FIRST_CELL_INDEX);
    assert!(METADATA_SIZE >= 1024,
        "metadata size ({} Bytes) too small", METADATA_SIZE);
    assert!(UNUSED_BITMAP_BYTES >= 8,
        "unused metadata bytes: {}", UNUSED_BITMAP_BYTES);
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

#[test]
fn bitmaps() {
    let mut bm = Bitmap([0; BITMAP_SIZE]);
    assert_eq!(bm.get_bit(0), false);
    assert_eq!(bm.get_bit(1), false);
    assert_eq!(bm.get_bit(2), false);
    assert_eq!(bm.get_bit(BITMAP_SIZE as u16 - 1), false);
    bm.set_bit(0, true);
    assert_eq!(bm.get_bit(0), true);
    assert_eq!(bm.get_bit(1), false);
    assert_eq!(bm.get_bit(2), false);
    assert_eq!(bm.get_bit(BITMAP_SIZE as u16 - 1), false);
    bm.set_bit(1, true);
    assert_eq!(bm.get_bit(0), true);
    assert_eq!(bm.get_bit(1), true);
    assert_eq!(bm.get_bit(2), false);
    assert_eq!(bm.get_bit(BITMAP_SIZE as u16 - 1), false);
    bm.set_bit(0, false);
    assert_eq!(bm.get_bit(0), false);
    assert_eq!(bm.get_bit(1), true);
    assert_eq!(bm.get_bit(2), false);
    assert_eq!(bm.get_bit(BITMAP_SIZE as u16 - 1), false);
    bm.set_bit(1, false);
    assert_eq!(bm.get_bit(1), false);
    bm.set_bit(BITMAP_SIZE as u16 - 1, true);
    assert_eq!(bm.get_bit(BITMAP_SIZE as u16 - 1), true);
}

#[test]
fn alloc() {
    let mut arena = Arena::new();
    assert_eq!(arena.metadata.mark_bitmap.get(FIRST_CELL_INDEX), true);
    assert_eq!(arena.metadata.block_bitmap.get(FIRST_CELL_INDEX), false);
    assert_eq!(arena.alloc_cells(1), Some(FIRST_CELL_INDEX));
}
