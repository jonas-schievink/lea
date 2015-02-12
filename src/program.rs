//! This module provides the `Program` struct, which represents a compiled Lea program including
//! all constants and optionally debug info (such as local names and line numbers).

use opcode::Opcode;

use std::vec::Vec;

/// Describes how an Upvalue is used (= where to get it from)
pub enum UpvalDesc {
    /// Upvalue references the local variable of the parent in the given stack slot
    Stack(usize),
    /// Upvalue references the parent's Upvalue with the given ID
    Upval(usize),
}

/// A compiled function (prototype). Instantiated by the VM
pub struct FunctionProto {
    /// The max. number of stack slots used
    stacksize: u8,
    /// Number of named parameters accepted
    params: u8,
    /// true if declared as a varargs function
    varargs: bool,
    /// Local names, indexed by stack slot. Unnamed stack slots aren't mapped (temporaries,
    /// stripped debug info)
    local_names: Vec<String>,
    /// The opcodes emitted for the code in the function body
    opcodes: Vec<Opcode>,
    /// Constants used by this function. Maximum of 65535. These map to indices into the program's
    /// constant table.
    consts: Vec<usize>,
    /// List of Upvalue reference descriptions
    upvalues: Vec<UpvalDesc>,
    /// Contains the last opcode number emitted for a given line
    lines: Vec<usize>,
}

pub struct Program {
    /// Functions defined inside the program
    funcs: Vec<FunctionProto>,
    /// Constants used within the program (strings, integers, floats)
    consts: Vec<Value>,
}


//////// Runtime types below ////////

/// An active Upvalue
pub enum Upval {
    /// Upvalue owned by parent. usize is either the stack slot or the index in the parent's upval
    /// list (stored in Upvalue definition in function prototype).
    Open(usize),
    /// Closed upvalue owned by the function that references it
    Closed(Value),
}

/// Instantiated function
pub struct Function<'a> {
    /// The prototype from which this function was instantiated
    proto: &'a FunctionProto,
    /// Upvalue references, indexed by upvalue ID
    upvalues: Vec<Upval>,
}
