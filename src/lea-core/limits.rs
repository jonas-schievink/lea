//! Implementation-dependent limitations on Lea programs

use std::{u16, usize};

/// The global limit on the stack size of a single function. This limit depends on the
/// implementation.
pub const STACK_LIMIT: u64 = 128;
/// Global limit of constants used by a single function.
pub const CONST_LIMIT: u64 = u16::MAX as u64;
/// Maximal number of opcodes defined for a single function
pub const OP_LIMIT: u64 = usize::MAX as u64;
/// Max. number of function parameters (minus varargs)
pub const PARAM_LIMIT: u64 = 50;
