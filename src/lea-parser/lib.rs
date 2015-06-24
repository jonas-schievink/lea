#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate lea_ast as ast;
extern crate lea_core;
extern crate term;

mod expr_parser;
pub mod parser;

pub use parser::*;
