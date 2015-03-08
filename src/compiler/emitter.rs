//! This module implements the byte code emitter.

use compiler::ast::*;

use std::u16;

/// The global limit on the stack size of a single function. This limit depends on the
/// implementation.
pub static LEA_STACK_LIMIT: u8 = 128;
/// Global limit of constants used by a single function.
pub static LEA_CONST_LIMIT: u16 = u16::MAX;

struct Emitter;
