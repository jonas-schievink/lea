//! This crate provides the byte code VM used to execute compiled Lea programs.

#![feature(hash)]

extern crate core;

pub mod value;
pub mod array;
pub mod table;
pub mod function;
pub mod vm;
pub mod mem;
