//! # The Lea Virtual Machine
//!
//! The VM executes Lea byte code generated by the emitter.
//!
//! # Calls
//!
//! Calls are recorded as `CallInfo`s inside a vector used as a stack. When a call instruction is
//! executed, the current instruction pointer and stack top is saved in a `CallInfo` struct, along
//! with the called function, and pushed onto the call stack.
//!
//! Then, the stack is extended by the (constant) number of stack slots needed, as stored in the
//! function prototype, and the call arguments are copied to the first slots. Depending on whether
//! a variable number of arguments was passed (as encoded in the call instruction) and whether the
//! callee accepts variable parameters, some of the arguments may be discarded or the stack may be
//! grown dynamically.
//!
//! When executing a return instruction, the current `CallInfo` is popped off the call stack and
//! the instruction pointer is restored. The returned values are then copied into the caller's
//! stack frame (the number - possibly dynamic - is encoded in the call instruction). The VM stack
//! is adjusted by truncating it to a length of `lasttop`, stored in the `CallInfo`.
//!
//! If the call stack is now empty, the main function of the program has just returned and the VM
//! will exit.
//!
//! Otherwise, the topmost `CallInfo` on the call stack is the caller of the function that just
//! returned.

use mem::{TracedRef, GcStrategy};
use program::Function;
use value::Value;

/// Contains information about a called Lea function.
pub struct CallInfo<'gc> {
    /// The function active at this call level
    func: TracedRef<'gc, Function<'gc>>,
    /// `lasttop` == vm.stack.len() at the time this call was made
    lasttop: usize,
    /// Dynamic stack top. This is updated any time an instruction that returns a variable number
    /// of results is executed and stores the stack slot of the last value returned by it.
    dtop: usize,
}

/// A VM context. Holds a garbage collector that manages the program's memory, the stack used for
/// local and temporary variables, the callstack, etc.
pub struct VM<'gc, G: GcStrategy + 'gc> {
    gc: G,
    /// Call stack
    calls: Vec<CallInfo<'gc>>,
    /// "VM stack", "value stack" or just stack. Stores the activation of functions in the form of
    /// variables (registers) used by the function.
    stack: Vec<Value<'gc>>,
}

impl <'gc, G: GcStrategy + 'gc> VM<'gc, G> {
    pub fn new(gc: G, main: TracedRef<'gc, Function<'gc>>) -> VM<'gc, G> {
        VM {
            gc: gc,
            calls: Vec::new(),
            stack: Vec::new(),
            main: main,
        }
    }

    pub fn gc(&self) -> &G {
        &self.gc
    }
}
