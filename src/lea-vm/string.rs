//! Immutable string type used by the VM

use std::ops::Deref;
use std::fmt;

/// `Str` is an immutable `String`.
#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Str(String);

impl Str {
    pub fn new(s: String) -> Str {
        Str(s)
    }
}

impl Deref for Str {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<String> for Str {
    fn from(s: String) -> Str {
        Str::new(s)
    }
}
