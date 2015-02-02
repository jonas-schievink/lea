#![feature(plugin)]
#![feature(core)]

// FIXME
#![allow(unused_features)]
#![feature(io)]
#![feature(collections)]

#[plugin] #[macro_use]
extern crate rustlex;

#[plugin] #[no_link]
extern crate phf_macros;
extern crate phf;

extern crate "parser-combinators" as parser_combinators;

#[plugin]
extern crate peg_syntax_ext;

peg_file! parser("../lea.rustpeg");

pub mod lexer;
pub mod ast;


#[cfg(test)]
mod parsetest;
