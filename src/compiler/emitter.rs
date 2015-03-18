//! This module implements the byte code emitter.

#![allow(dead_code)]

use compiler::ast::*;
use compiler::visit::*;
use opcode::*;
use program::FunctionProto;
use limits;

use std::mem::replace;


#[derive(Clone, Debug)]
pub struct EmitError {
    pub msg: &'static str,
    pub detail: Option<String>,
}

/// On success, the ID of the main function is returned. On error, the `EmitError`s produced.
pub type EmitResult = Result<FunctionProto, Vec<EmitError>>;

struct Emitter {
    source_name: String,
    /// Next stack slot used for variables (local to current function)
    next_slot: usize,
    errs: Vec<EmitError>,
    /// Prototypes emitted by this emitter. Used as a stack: The innermost function is at the end
    /// (and is the function the emitter currently works on).
    protos: Vec<FunctionProto>,
}

impl Emitter {
    /// Get the prototype of the currently emitted function
    fn curproto(&mut self) -> &mut FunctionProto {
        let len = self.protos.len();
        debug_assert!(len > 0);
        &mut self.protos[len-1]
    }

    /// Adds a constant to the program's constant table and returns its index (does not add it if
    /// the same value already exists in the constant table).
    fn add_const(&mut self, lit: &Literal) -> usize {
        // ensure that only "useful" constant are added
        debug_assert!(match *lit {
            TInt(..) | TFloat(..) | TStr(..) => true,
            TBool(..) | TNil => false, // handled by special opcodes
        });

        let mut proto = self.curproto();

        for i in 0..proto.consts.len() {
            let c = &proto.consts[i];
            if lit == c { return i; }
        }

        proto.consts.push(lit.clone());
        proto.consts.len() - 1
    }

    fn err(&mut self, msg: &'static str, detail: Option<String>) {
        self.errs.push(EmitError {
            msg: msg,
            detail: detail,
        });
    }

    /// Emits an opcode into the current function and returns its "address" or index
    fn emit(&mut self, op: Opcode) -> usize {
        let ops = &mut self.curproto().opcodes;
        let idx = ops.len();
        ops.push(op);

        idx
    }
}

impl Visitor for Emitter {
    fn visit_func(&mut self, mut f: Function) -> Function {
        let old_slot = replace(&mut self.next_slot, 0);
        let proto = FunctionProto {
            source_name: self.source_name.clone(),
            stacksize: 0,
            params: if f.params.len() as u64 > limits::PARAM_LIMIT {
                self.err("parameter count exceeds limit",
                    Some(format!("count {} > {}", f.params.len(), limits::PARAM_LIMIT)));
                return f;
            } else {
                f.params.len() as u8
            },
            varargs: f.varargs,
            opcodes: Vec::with_capacity(32),
            consts: Vec::with_capacity(8),
            upvalues: Vec::with_capacity(f.upvalues.len()),
            upval_names: Vec::with_capacity(f.upvalues.len()),
            lines: Vec::new(),  // TODO emit line info
            child_protos: Vec::with_capacity(8),
        };
        self.protos.push(proto);

        self.add_const(&TInt(42));  // TEST!

        f = walk_func(f, self);

        let stacksz = self.next_slot;
        if stacksz as u64 > limits::STACK_LIMIT {
            self.err("stack size exceeds maximum value",
                Some(format!("size {} > {}", stacksz, limits::STACK_LIMIT)));
            return f;
        }
        let mut proto = self.protos.pop().unwrap();
        proto.stacksize = stacksz as u8;

        // TODO shrink vectors to save space
        
        if self.protos.is_empty() {
            // just emitted the main function, put it back
            self.protos.push(proto);
        } else {
            //let parent = &mut self.protos[self.protos.len()-1];
            // TODO store function prototype in its parent, shrink vectors
        }

        self.next_slot = old_slot;

        f
    }
}


/// Builds a `FunctionProto` for the given main function by emitting byte code
pub fn emit_func(f: Function, source_name: &str) -> (Function, EmitResult) {
    let mut emitter = Emitter {
        source_name: source_name.to_string(),
        next_slot: 0,
        errs: vec![],
        protos: Vec::with_capacity(16),
    };

    let f = emitter.visit_func(f);
    let res = if emitter.errs.is_empty() {
        debug_assert_eq!(emitter.protos.len(), 1);
        Ok(emitter.protos.pop().unwrap())
    } else {
        Err(emitter.errs)
    };

    (f, res)
}


#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parse_and_resolve;
    use program::FunctionProto;

    fn test(code: &str) -> FunctionProto {
        emit_func(parse_and_resolve(code).unwrap(), "<test>").1.unwrap()
    }

    #[test]
    fn tdd() {
        test("local i");
    }
}
