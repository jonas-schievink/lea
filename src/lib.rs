#![feature(plugin, core, old_io)]

#![plugin(peg_syntax_ext)]

pub mod compiler;

pub mod opcode;
pub mod program;
pub mod value;
