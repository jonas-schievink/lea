//! Defines the `FunctionProto` type, which is a garbage-collected version of core's `FnData`, as
//! well as the `Function` type, which is an instantiated, runnable function inside the VM.

use lea_core::fndata::{UpvalDesc, FnData};
use lea_core::opcode::*;
use lea_core::literal::*;

use mem::*;
use value::Value;

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

impl <'gc> FunctionProto<'gc> {
    pub fn from_fndata<G: GcStrategy<'gc>>(_fndata: FnData, _gc: &mut G)
    -> TracedRef<'gc, FunctionProto> {
        unimplemented!();
    }
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
