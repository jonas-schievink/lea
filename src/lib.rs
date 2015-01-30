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

pub mod lexer;
pub mod parser;
pub mod ast;
