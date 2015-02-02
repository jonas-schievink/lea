#![feature(plugin)]
#![feature(core)]

// FIXME
#![allow(unused_features)]
#![feature(collections)]

#[plugin]
extern crate peg_syntax_ext;

peg_file! parser("../lea.rustpeg");

pub mod ast;


#[cfg(test)]
mod parsetest;
