//! Runtime errors

use value::Value;

#[derive(Debug)]
pub struct VmError {
    pub msg: String,
    // TODO: Extend this! More debug info!
}

impl From<String> for VmError {
    fn from(msg: String) -> VmError {
        VmError {
            msg: msg,
        }
    }
}

/// The result of an executed program. The program's main function can return any number of values.
pub type VmResult = Result<Vec<Value>, VmError>;
