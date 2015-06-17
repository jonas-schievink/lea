//! This crate contains the abstract syntax tree and source code span handling

#![feature(collections)]

extern crate term;
extern crate unicode_segmentation;

extern crate lea_core;

pub mod op;
pub mod ast;
pub mod span;
pub mod visit;

pub use ast::*;
