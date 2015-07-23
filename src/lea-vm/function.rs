//! Defines the `FunctionProto` type, which is a garbage-collected version of core's `FnData`, as
//! well as the `Function` type, which is an instantiated, runnable function inside the VM.

use lea_core::fndata::{UpvalDesc, FnData};
use lea_core::opcode::*;

use mem::*;
use value::Value;

/// A compiled function. When instantiated by the VM, becomes a `Function`.
#[derive(Clone, Debug)]
pub struct FunctionProto {
    /// The name of the source from which this function was compiled
    pub source_name: String,
    /// The number of stack slots required by this function (might be dynamically increased)
    pub stacksize: u8,
    /// Number of parameters accepted (ignoring varargs)
    pub params: u8,
    /// True if declared as a varargs function
    pub varargs: bool,
    /// The opcodes emitted for the code in the function bodyt.mark_traceable(r),
    pub opcodes: Opcodes,
    /// Constants used by this function.
    pub consts: Vec<Value>,
    /// List of Upvalue reference descriptions
    pub upvalues: Vec<UpvalDesc>,
    /// Names of Upvalues (names may not be defined)
    pub upval_names: Vec<String>,
    /// Contains the last opcode number emitted for a given line
    pub lines: Vec<usize>,
    /// References to the prototypes of all function declared within the body of this function.
    /// When dynamically compiling code, this allows the GC to collect prototypes that are unused.
    pub child_protos: Vec<TracedRef<FunctionProto>>,
}

impl FunctionProto {
    pub fn from_fndata<G: GcStrategy>(fndata: FnData, gc: &mut G) -> TracedRef<FunctionProto> {
        let proto = FunctionProto {
            source_name: fndata.source_name,
            stacksize: fndata.stacksize,
            params: fndata.params,
            varargs: fndata.varargs,
            opcodes: fndata.opcodes,
            upvalues: fndata.upvals,
            upval_names: vec![],    // TODO
            lines: fndata.lines,
            consts: fndata.consts.into_iter().map(|lit| Value::from_literal(lit, gc)).collect(),
            child_protos: fndata.child_protos.into_iter()
                .map(|data| FunctionProto::from_fndata(*data, gc)).collect(),
        };

        gc.register_obj(proto)

        /*pub stacksize: u8,
        pub params: usize,
        pub varargs: bool,
        pub opcodes: Opcodes,
        pub consts: Vec<Literal>,
        pub upvals: Vec<UpvalDesc>,
        pub lines: Vec<usize>,
        pub source_name: String,
        pub child_protos: Vec<Box<FnData>>,*/
    }
}

impl Traceable for FunctionProto {
    fn trace<T: Tracer>(&self, t: &mut T) {
        for ch in &self.child_protos {
            t.mark_traceable(*ch);
        }
    }
}

/// An active Upvalue
#[derive(Debug)]
pub enum Upval {
    /// Upvalue owned by parent. usize is either the stack slot or the index in the parent's upval
    /// list (stored in Upvalue definition in function prototype).
    Open(usize),
    /// Closed upvalue owned by the function that references it
    Closed(Value),
}

/// Instantiated function
#[derive(Debug)]
pub struct Function {
    /// The prototype from which this function was instantiated
    pub proto: TracedRef<FunctionProto>,
    /// Upvalue references, indexed by upvalue ID
    pub upvalues: Vec<Upval>,
}

impl Function {
    /// Create a new `Function` from its prototype.
    ///
    /// Upvalues are filled in by calling `search_upval` with the upvalue description from the
    /// prototype. `search_upval` will be called in the right order: Upvalue 0 will be resolved
    /// first.
    pub fn new<F, G: GcStrategy>(gc: &mut G, proto: TracedRef<FunctionProto>, mut search_upval: F)
    -> Function
    where F: FnMut(&UpvalDesc) -> Upval {
        // TODO Code style of fn decl (is there something official?)

        let protoref = unsafe { gc.get_ref(proto) };
        Function {
            proto: proto,
            upvalues: protoref.upvalues.iter().map(|desc| search_upval(desc)).collect(),
        }
    }

    /// Sets an upvalue to the given value. The upvalue must be closed, otherwise, this function
    /// panics (the VM has to close upvalues, we cannot do that).
    pub fn set_closed_upvalue(&mut self, id: usize, val: Value) {
        let upval: &mut Upval = &mut self.upvalues[id];

        if let Upval::Closed(ref mut oldval) = *upval {
            *oldval = val;
        } else {
            panic!("set_closed_upvalue called on open upvalue #{}", id);
        }
    }
}

impl Traceable for Function {
    fn trace<T: Tracer>(&self, t: &mut T) {
        t.mark_traceable(self.proto);

        for uv in &self.upvalues {
            if let Upval::Closed(ref val) = *uv {
                val.trace(t);
            }
        }
    }
}
