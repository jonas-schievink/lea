//! This crate provides the VM used to execute compiled Lea programs.
//!
//! Since this is considered an internal crate, everything inside here is entirely unstable and
//! unsafe (because Rust really doesn't like it when I strap a GC on top of it).

#![feature(raw)]

extern crate lea_core;

#[macro_use]
extern crate log;

pub mod libfn;
pub mod function;
pub mod error;
pub mod mem;
pub mod number;
mod array;
mod string;
mod table;
mod value;
mod vm;

#[doc(inline)]
pub use array::Array;

#[doc(inline)]
pub use string::Str;

#[doc(inline)]
pub use table::Table;

#[doc(inline)]
pub use value::Value;

#[doc(inline)]
pub use vm::VM;
