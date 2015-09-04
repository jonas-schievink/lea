//! Runtime errors

use Value;

use std::fmt;

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

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.msg)
    }
}

/// The result of an executed program. The program's main function can return any number of values.
pub type VmResult = Result<Vec<Value>, VmError>;
