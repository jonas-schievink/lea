//! The Lea Virtual Machine

/// A VM context. Holds a garbage collector that manages the program's memory, the stack used for
/// local and temporary variables, the callstack, etc.
pub struct VM;

#[cfg(test)]
impl VM {
    /// Creates a default VM for unit tests
    pub fn new() -> VM {
        VM
    }
}
