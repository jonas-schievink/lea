//! The `core` crate provides types and methods used by both the compiler and the VM.

extern crate rustc_serialize;

pub mod opcode;
pub mod fndata;
pub mod limits;
pub mod literal;
