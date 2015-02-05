#![feature(plugin)]
#![feature(core)]
#![feature(io)]

// FIXME
#![allow(unused_features)]
#![feature(collections)]

#[plugin]
extern crate peg_syntax_ext;

pub mod parser;
pub mod ast;
pub mod visit;
pub mod prettyprint;

#[cfg(test)]
mod parsetest;
