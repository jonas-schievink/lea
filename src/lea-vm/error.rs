//! Runtime errors

use value::Value;

pub struct VmError {
    pub msg: &'static str,
    // TODO: Extend this!
}

/// The result of an executed program. The program's main function can return any number of values.
pub type VmResult = Result<Vec<Value>, VmError>;
