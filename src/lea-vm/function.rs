//! Defines the `FunctionProto` type, which is a garbage-collected version of core's `FnData`, as
//! well as the `Function` type, which is an instantiated, runnable function inside the VM.

use lea_core::fndata::{UpvalDesc, FnData};
use lea_core::opcode::*;

use mem::*;
use Value;

use std::rc::Rc;
use std::cell::Cell;

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
#[derive(Debug, Clone, Copy)]
pub enum Upval {
    /// Upvalue is stored in a stack slot
    Open(usize),
    /// Closed upvalue, storing its value inline
    Closed(Value),
}

/// Instantiated function
#[derive(Debug)]
pub struct Function {
    /// The prototype from which this function was instantiated
    pub proto: TracedRef<FunctionProto>,
    /// Upvalue references, indexed by upvalue ID
    pub upvalues: Vec<Rc<Cell<Upval>>>,
}

impl Function {
    /// Create a new `Function` from its prototype.
    ///
    /// Upvalues are filled in by calling `search_upval` with the upvalue description from the
    /// prototype. `search_upval` will be called in the right order: Upvalue 0 will be resolved
    /// first.
    pub fn new<F, G: GcStrategy>(gc: &G, proto: TracedRef<FunctionProto>, mut search_upval: F)
    -> Function
    where F: FnMut(&UpvalDesc) -> Rc<Cell<Upval>> {
        // TODO Code style of fn decl (is there something official?)

        let protoref = unsafe { gc.get_ref(proto) };
        Function {
            proto: proto,
            upvalues: protoref.upvalues.iter().map(|desc| search_upval(desc)).collect(),
        }
    }

    pub fn with_env<G: GcStrategy>(gc: &G, proto: TracedRef<FunctionProto>, env: Value)
    -> Function {
        let mut first = true;
        Function::new(gc, proto, |_| if first {
            first = false;
            Rc::new(Cell::new(Upval::Closed(env)))
        } else {
            Rc::new(Cell::new(Upval::Closed(Value::Nil)))
        })
    }

    /// Sets an upvalue to the given value.
    pub fn set_upvalue(&mut self, id: usize, val: Upval) {
        self.upvalues[id].set(val);
    }
}

impl Traceable for Function {
    fn trace<T: Tracer>(&self, t: &mut T) {
        t.mark_traceable(self.proto);

        for uv in &self.upvalues {
            if let Upval::Closed(val) = uv.get() {
                val.trace(t);
            }
        }
    }
}
