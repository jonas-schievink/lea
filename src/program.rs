//! This module provides the `Program` struct, which represents a compiled Lea program including
//! all constants and optionally debug info (such as local names and line numbers).

use opcode::Opcode;
use value::Value;
use compiler::ast::{self, Literal};
use mem::{Traceable, TracedRef, Tracer, GcStrategy, GcObj};

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

/// Function represenation used by the emitter. Later converted to a `FunctionProto`. This owns all
/// child functions (as `FnData`) and can be serialized.
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
    pub child_protos: Vec<Box<FnData>>,
}

impl FnData {
    pub fn new(f: &ast::Function) -> FnData {
        FnData {
            stacksize: 0,
            params: f.params.len(),
            varargs: f.varargs,
            opcodes: Vec::with_capacity(32),
            consts: Vec::with_capacity(8),
            upvals: Vec::with_capacity(f.upvalues.len()),
            lines: Vec::new(),  // TODO: emit line info
            child_protos: Vec::new(),
        }
    }

    /// Converts this `FnData` instance to a `FunctionProto`. Registers all child functions with
    /// the given garbage collector.
    pub fn to_proto<'gc, G: GcStrategy>(self, _gc: &'gc mut G) -> FunctionProto<'gc> {
        // TODO
        unimplemented!();
    }
}

/// A compiled function. When instantiated by the VM, becomes a `Function`.
#[derive(Clone, Debug)]
pub struct FunctionProto<'gc> {
    /// The name of the source from which this function was compiled
    pub source_name: String,
    /// The number of stack slots required by this function (might be dynamically increased)
    pub stacksize: u8,
    /// Number of parameters accepted (ignoring varargs)
    pub params: u8,
    /// True if declared as a varargs function
    pub varargs: bool,
    /// The opcodes emitted for the code in the function bodyt.mark_traceable(r),
    pub opcodes: Vec<Opcode>,
    /// Constants used by this function.
    pub consts: Vec<Literal>,
    /// List of Upvalue reference descriptions
    pub upvalues: Vec<UpvalDesc>,
    /// Names of Upvalues (names may not be defined)
    pub upval_names: Vec<String>,
    /// Contains the last opcode number emitted for a given line
    pub lines: Vec<usize>,
    /// References to the prototypes of all function declared within the body of this function.
    /// When dynamically compiling code, this allows the GC to collect prototypes that are unused.
    pub child_protos: Vec<TracedRef<'gc, FunctionProto<'gc>>>,
}

impl <'gc> GcObj for FunctionProto<'gc> {}
impl <'gc> Traceable for FunctionProto<'gc> {
    fn trace<T: Tracer>(&self, t: &mut T) {
        for ch in &self.child_protos {
            t.mark_traceable(*ch);
        }
    }
}

/// An active Upvalue
#[derive(Debug)]
pub enum Upval<'gc> {
    /// Upvalue owned by parent. usize is either the stack slot or the index in the parent's upval
    /// list (stored in Upvalue definition in function prototype).
    Open(usize),
    /// Closed upvalue owned by the function that references it
    Closed(Value<'gc>),
}

/// Instantiated function
#[derive(Debug)]
pub struct Function<'gc> {
    /// The prototype from which this function was instantiated
    pub proto: TracedRef<'gc, FunctionProto<'gc>>,
    /// Upvalue references, indexed by upvalue ID
    pub upvalues: Vec<Upval<'gc>>,
}

impl <'gc> GcObj for Function<'gc> {}
impl <'gc> Traceable for Function<'gc> {
    fn trace<T: Tracer>(&self, t: &mut T) {
        t.mark_traceable(self.proto);

        for uv in &self.upvalues {
            if let Upval::Closed(ref val) = *uv {
                val.trace(t);
            }
        }
    }
}
