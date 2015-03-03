#![feature(plugin, core, old_io, io, collections)]

#![plugin(peg_syntax_ext)]

pub mod compiler;

pub mod opcode;
pub mod program;
pub mod value;
