//! This module provides the `Program` struct, which represents a compiled Lea program including
//! all constants and optionally debug info (such as local names and line numbers).

use opcode::Opcode;
use value::Value;

use std::vec::Vec;

/// Describes how an open Upvalue is referenced
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum UpvalDesc {
    /// Upvalue is the parent's local with the given id
    ///
    /// This is emitted by the resolver and converted to `Stack` by the emitter
    Local(usize),
    /// Upvalue is local variable of the parent in the given stack slot
    Stack(usize),
    /// Upvalue is the parent's Upvalue with the given ID
    Upval(usize),
}

/// A compiled function (prototype). Instantiated by the VM
pub struct FunctionProto {
    /// The max. number of stack slots used
    pub stacksize: u8,
    /// Number of parameters accepted (ignoring varargs)
    pub params: u8,
    /// True if declared as a varargs function
    pub varargs: bool,
    /// The opcodes emitted for the code in the function body
    pub opcodes: Vec<Opcode>,
    /// Constants used by this function. Maximum of 65535. These map to indices into the program's
    /// constant table.
    pub consts: Vec<usize>,
    /// List of Upvalue reference descriptions
    pub upvalues: Vec<UpvalDesc>,
    /// Names of Upvalues (names may not be defined)
    pub upval_names: Vec<String>,
    /// Contains the last opcode number emitted for a given line
    pub lines: Vec<usize>,
}

pub struct Program {
    /// Functions defined inside the program
    pub funcs: Vec<FunctionProto>,
    /// Constants used within the program (strings, integers, floats)
    pub consts: Vec<Value>,
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
pub struct Function {
    /// The index of the prototype from which this function was instantiated
    pub proto: usize,
    /// Upvalue references, indexed by upvalue ID
    pub upvalues: Vec<Upval>,
}
