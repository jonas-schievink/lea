//! Lea is a scripting language derived from Lua.

#![feature(plugin, core, collections, log_syntax, hash)]

#![plugin(peg_syntax_ext)]
#![plugin(phf_macros)]

#![unstable]

extern crate phf;
extern crate "rustc-serialize" as rustc_serialize;

pub mod compiler;
pub mod op;
pub mod opcode;
pub mod program;
pub mod value;
pub mod array;
pub mod table;
