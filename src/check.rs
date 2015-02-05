//! Checks if an AST is valid

use ast::*;
use visit::*;

pub struct CheckError {
    msg: &'static str,
    detail: String,
}

fn mkerr(msg: &'static str, detail: String) -> CheckError {
    CheckError {
        msg: msg,
        detail: detail,
    }
}

/// Checks that the passed block contains only valid statements.
pub fn check_block(block: &Block) -> Result<(), CheckError> {
    Err(mkerr("test", "detail".to_string()))
}
