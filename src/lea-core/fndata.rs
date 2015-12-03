//! Provides the `FnData` struct which is created by the compiler when emitting byte code and
//! passed to the VM for execution.

use opcode::Opcodes;
use Const;


/// Describes how an Upvalue is referenced
#[derive(PartialEq, Eq, Clone, Copy, Debug, RustcEncodable, RustcDecodable)]
pub enum UpvalDesc {
    /// Upvalue is the parent's local with the given id
    ///
    /// This is emitted by the resolver and converted to `Stack` by the emitter
    Local(usize),
    /// Upvalue is local variable of the parent in the given stack slot
    Stack(u8),
    /// Upvalue is the parent's Upvalue with the given ID
    Upval(usize),
}

/// Function representation output by the compiler. Later converted to a `FunctionProto`. This owns
/// all child functions (as `FnData`) and can be serialized.
#[derive(Debug, RustcEncodable, RustcDecodable)]
pub struct FnData {
    /// Size of the variable stack. This is also used as the next slot allocated for a value.
    pub stacksize: u8,
    pub params: u8,
    pub varargs: bool,
    pub opcodes: Opcodes,
    pub consts: Vec<Const>,
    pub upvals: Vec<UpvalDesc>,
    pub lines: Vec<usize>,
    pub source_name: String,
    pub child_protos: Vec<Box<FnData>>,
}
