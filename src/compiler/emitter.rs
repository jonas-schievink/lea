//! This module implements the byte code emitter.

use compiler::ast::*;
use compiler::visit::*;
use program::Program;


struct Emitter<'a> {
    program: &'a mut Program,
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
    fn visit_func(&mut self, f: Function) -> Function {
        self.add_const(&TInt(42));  // TEST! TNil is not a valid constant anyways

        f
    }
}


/// Emits code for the given main function into a program structure.
pub fn emit_func(p: &mut Program, f: Function) -> Function {
    let mut emitter = Emitter {
        program: p,
    };

    emitter.visit_func(f)
}
