#![feature(plugin)]
#![plugin(peg_syntax_ext)]

extern crate lea_core;
extern crate term;
extern crate unicode_segmentation;

mod expr_parser;

pub mod op;
pub mod span;
pub mod parsetree;
pub mod parser;
pub mod prettyprint;

pub use parser::*;
