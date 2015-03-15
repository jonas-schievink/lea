//! Lea is a scripting language derived from Lua.

#![feature(plugin, core, collections, log_syntax, hash, alloc)]

#![plugin(peg_syntax_ext)]

#![unstable]

#[macro_use]
extern crate lazy_static;

extern crate "rustc-serialize" as rustc_serialize;

pub mod compiler;
pub mod op;
pub mod opcode;
pub mod program;
pub mod value;
pub mod array;
pub mod table;
pub mod mem;
