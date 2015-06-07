//! The Lea Virtual Machine

use mem::GcStrategy;

/// A VM context. Holds a garbage collector that manages the program's memory, the stack used for
/// local and temporary variables, the callstack, etc.
pub struct VM<G: GcStrategy> {
    gc: G,
}

impl <G: GcStrategy + Default> Default for VM<G> {
    fn default() -> VM<G> {
        VM {
            gc: Default::default(),
        }
    }
}

impl <G: GcStrategy> VM<G> {
    pub fn new(gc: G) -> VM<G> {
        VM {
            gc: gc,
        }
    }

    pub fn gc(&self) -> &G {
        &self.gc
    }
}
