//! This module implements the byte code emitter.

#![allow(dead_code)]

use compiler::ast::*;
use compiler::visit::*;
use opcode::*;
use program::FnData;
use limits;

use std::u16;


#[derive(Clone, Debug)]
pub struct EmitError {
    pub msg: &'static str,
    pub detail: Option<String>,
}

/// On success, the main function is returned as an `FnData` object. On error, the `EmitError`s
/// produced are returned.
pub type EmitResult = Result<FnData, Vec<EmitError>>;

struct Emitter {
    source_name: String,
    /// List of errors that have occurred while emitting. They are ignored until the main function
    /// is traversed. If this list isn't empty when the emitter is done, it will not complete the
    /// process and return an error to the caller instead.
    errs: Vec<EmitError>,
    /// Function stack. The last entry is the currently emitted function. When done emitting, the
    /// last `FnData` is popped off and added to the child function list of the parent.
    funcs: Vec<FnData>,

    block: Option<Block>,
}

impl Emitter {
    fn new(source_name: &str) -> Emitter {
        Emitter {
            source_name: source_name.to_string(),
            errs: Vec::new(),
            funcs: Vec::with_capacity(4),
            block: None,
        }
    }

    fn cur_block(&self) -> &Block {
        match self.block {
            Some(ref b) => b,
            None => panic!("emitter has no current block"),
        }
    }

    /// Get the FnData of the currently emitted function
    fn cur_func(&mut self) -> &mut FnData {
        let len = self.funcs.len();
        debug_assert!(len > 0);
        &mut self.funcs[len-1]
    }

    /// Adds a constant to the program's constant table and returns its index (does not add it if
    /// the same value already exists in the constant table).
    fn add_const(&mut self, lit: &Literal) -> u16 {
        // ensure that only "useful" constant are added
        debug_assert!(match *lit {
            TInt(..) | TFloat(..) | TStr(..) => true,
            TBool(..) | TNil => false, // handled by special opcodes
        });

        {
            let func = self.cur_func();
            for i in 0..func.consts.len() {
                let c = &func.consts[i];
                if lit == c {
                    // this cast cannot fail, because we stop adding constants when the u16 limit is
                    // reached
                    return i as u16;
                }
            }
        }

        let id = self.cur_func().consts.len();
        if id > u16::MAX as usize {
            self.err("constant limit reached", Some(format!("limit: {}", u16::MAX + 1)));
            u16::MAX
        } else {
            self.cur_func().consts.push(lit.clone());
            id as u16
        }
    }

    /// Adds an error to the error list. The emitter ignores errors until it is done. If the error
    /// list isn't empty when the main function was traversed, the emitter will return an error to
    /// the caller.
    fn err(&mut self, msg: &'static str, detail: Option<String>) {
        self.errs.push(EmitError {
            msg: msg,
            detail: detail,
        });
    }

    fn get_result(mut self) -> EmitResult {
        if self.errs.len() > 0 {
            Err(self.errs)
        } else {
            assert_eq!(self.funcs.len(), 1);

            Ok(self.funcs.pop().unwrap())
        }
    }

    /// Emits an opcode into the current function and returns its "address" or index
    fn emit(&mut self, op: Opcode) -> usize {
        println!("{:?}", op);

        let idx = self.cur_func().opcodes.len();
        if idx as u64 >= limits::OP_LIMIT {
            self.err("opcode limit reached", Some(format!("limit: {}", limits::OP_LIMIT)));
        } else {
            self.cur_func().opcodes.push(op);
        }

        idx
    }
}

impl <'a> Visitor<'a> for Emitter {
    /*fn visit_stmt(&mut self, s: Stmt) -> Stmt {
        s.value = match s.value {
            SDecl(names, exprs) => {
                for name in &names {
                    println!("DECL {:?}", self.cur_block().get_local(name));
                }

                SDecl(names, exprs)
            }
            _ => s.value,
        };

        walk_stmt(s, self)
    }*/

    fn visit_expr(&mut self, _e: &Expr) {
        panic!("Emitter::visit_expr entered (this should never happen)");
    }

    /*fn visit_block(&mut self, b: Block) -> Block {
        self.block = Some(b);
        walk_block(b, self);

        replace(&mut self.block, None).unwrap()
    }*/

    fn visit_func(&mut self, f: &Function) {
        self.funcs.push(FnData::new(f));

        self.visit_block(&f.body);
        self.emit(RETURN(0, 1));

        let func = self.funcs.pop().unwrap();
        if func.stacksize as u64 > limits::STACK_LIMIT {
            self.err("stack size exceeds maximum value",
                Some(format!("got size {}, max is {}", func.stacksize, limits::STACK_LIMIT)));
            return;
        }

        // TODO shrink vectors to save space

        if self.funcs.is_empty() {
            // just emitted the main function, put it back, we are done
            self.funcs.push(func);
        } else {
            let parent_idx = self.funcs.len()-1;
            let parent = &mut self.funcs[parent_idx];
            parent.child_protos.push(Box::new(func));
        }
    }
}


/// Builds a `FunctionProto` for the given main function and emits byte code for execution by the
/// VM.
pub fn emit_func(f: &Function, source_name: &str) -> EmitResult {
    let mut emitter = Emitter::new(source_name);
    emitter.visit_func(f);

    emitter.get_result()
}


#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parse_and_resolve;
    use program::FnData;

    fn test(code: &str) -> FnData {
        emit_func(&parse_and_resolve(code).unwrap(), "<test>").unwrap()
    }

    #[test]
    fn tdd() {
        test("local i");
    }
}
