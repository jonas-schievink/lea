//! The `core` crate provides types and methods used by both the compiler and the VM.

extern crate rustc_serialize;
extern crate lea_num as num;

pub mod opcode;
pub mod fndata;
pub mod limits;
mod constant;

#[doc(inline)]
pub use constant::Const;
