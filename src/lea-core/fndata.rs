//! Provides the `FnData` struct which is created by the compiler when emitting byte code and
//! passed to the VM for execution.

use opcode::Opcode;
use literal::Literal;

use std::vec::Vec;


/// Describes how an Upvalue is referenced
#[derive(PartialEq, Eq, Clone, Copy, Debug, RustcEncodable, RustcDecodable)]
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

/// Function representation used by the emitter. Later converted to a `FunctionProto`. This owns
/// all child functions (as `FnData`) and can be serialized.
#[derive(Debug, RustcEncodable, RustcDecodable)]
pub struct FnData {
    /// Size of the variable stack. This is also used as the next slot allocated for a value.
    pub stacksize: u8,
    pub params: usize,
    pub varargs: bool,
    pub opcodes: Vec<Opcode>,
    pub consts: Vec<Literal>,
    pub upvals: Vec<UpvalDesc>,
    pub lines: Vec<usize>,
    pub source_name: String,
    pub child_protos: Vec<Box<FnData>>,
}
