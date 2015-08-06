//! This crate provides the byte code VM used to execute compiled Lea programs.
//!
//! Since this is considered an internal crate, everything inside here is entirely unstable and
//! unsafe (because Rust really doesn't like it when I strap a GC on top of it).

#![feature(hasher_write, raw, vec_resize)]

extern crate lea_core;

#[macro_use]
extern crate log;

pub mod value;
pub mod array;
pub mod table;
pub mod function;
pub mod vm;
pub mod error;
pub mod mem;
