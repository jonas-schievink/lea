//! This module implements the byte code emitter.

use compiler::ast::*;
use compiler::visit::*;
use program::Program;
use opcode::*;

use std::mem::replace;


#[derive(Clone, Debug)]
pub struct EmitError {
    pub msg: &'static str,
    pub detail: Option<String>,
}

pub type EmitResult = Result<(), EmitError>;

struct Emitter<'a> {
    program: &'a mut Program,
    ops: Vec<Opcode>,
    next_slot: usize,
    result: EmitResult,
}

impl <'a> Emitter<'a> {
    /// Adds a constant to the program's constant table and returns its index (does not add it if
    /// the same value already exists in the constant table).
    fn add_const(&mut self, lit: &Literal) -> usize {
        // ensure that only "useful" constant are added
        debug_assert!(match *lit {
            TInt(..) | TFloat(..) | TStr(..) => true,
            TBool(..) | TNil => false, // handled by special opcodes
        });

        for i in 0..self.program.consts.len() {
            let c = &self.program.consts[i];
            if lit == c { return i; }
        }

        self.program.consts.push(lit.clone());
        self.program.consts.len() - 1
    }
}

impl <'a> Visitor for Emitter<'a> {
    fn visit_func(&mut self, mut f: Function) -> Function {
        self.add_const(&TInt(42));  // TEST!

        let old_slot = replace(&mut self.next_slot, 0);
        let old_ops = replace(&mut self.ops, vec![]);
        f = walk_func(f, self);
        self.next_slot = old_slot;
        self.ops = old_ops;

        f
    }
}


/// Emits code for the given main function into a program structure.
pub fn emit_func(p: &mut Program, f: Function) -> (Function, EmitResult) {
    let mut emitter = Emitter {
        program: p,
        result: Ok(()),
        next_slot: 0,
        ops: vec![],
    };

    (emitter.visit_func(f), emitter.result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parse_and_resolve;
    use program::Program;

    fn test(code: &str) -> Program {
        let mut prog = Program::new();
        emit_func(&mut prog, parse_and_resolve(code).unwrap()).1.unwrap();
        prog
    }

    #[test]
    fn tdd() {
        test("local i");
    }
}
