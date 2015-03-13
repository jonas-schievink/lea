//! Lea is a scripting language derived from Lua.

#![feature(plugin, core, old_io, io, collections)]

#![plugin(peg_syntax_ext)]
#![plugin(phf_macros)]

#![unstable]

extern crate phf;

pub mod compiler;
pub mod op;
pub mod opcode;
pub mod program;
pub mod value;
