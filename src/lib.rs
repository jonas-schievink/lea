#![feature(plugin, core, old_io)]

#![plugin(peg_syntax_ext)]


pub mod parser;
pub mod expr_parser;
pub mod span;
pub mod ast;
pub mod opcode;
pub mod program;
pub mod value;
pub mod visit;
pub mod prettyprint;
pub mod check;
pub mod resolve;
