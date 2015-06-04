//! Lea is a scripting language derived from Lua.

#![feature(plugin, core, collections, hash)]
#![allow(trivial_casts)]

#![plugin(peg_syntax_ext)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate log;

extern crate rustc_serialize;
extern crate term;
extern crate unicode_segmentation;

pub mod compiler;
pub mod op;
pub mod opcode;
pub mod program;
pub mod value;
pub mod array;
pub mod table;
pub mod mem;
pub mod limits;
pub mod vm;
