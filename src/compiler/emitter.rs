//! This module implements the byte code emitter.

use compiler::ast::*;
use compiler::visit::*;
use program::Program;

use std::u16;

/// The global limit on the stack size of a single function. This limit depends on the
/// implementation.
pub static LEA_STACK_LIMIT: u8 = 128;
/// Global limit of constants used by a single function.
pub static LEA_CONST_LIMIT: u16 = u16::MAX;

struct Emitter<'a> {
    _program: &'a mut Program,
}

/*impl Emitter {
    /// Adds a constant to the program's constant table and returns its index (does not add it if
    /// the same value already exists in the constant table).
    fn add_const(&mut self, lit: &Literal) -> usize {
        for c in self.program.consts {

        }
    }
}*/

impl <'a> Visitor for Emitter<'a> {

}


/// Emits code for the given main function into a program structure.
pub fn emit_func(p: &mut Program, f: Function) -> Function {
    let mut emitter = Emitter {
        _program: p,
    };

    emitter.visit_func(f)
}
