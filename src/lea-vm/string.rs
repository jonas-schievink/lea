//! Immutable string type used by the VM

use std::ops::Deref;
use std::fmt;

/// `Str` is an immutable `String`. All strings are 0-terminated, but can contain '0' bytes. This
/// allows them to be passed to C code without an extra conversion.
#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Str(String);

impl Str {
    pub fn new(mut s: String) -> Str {
        s.push('\0');
        Str(s)
    }

    pub fn len(&self) -> usize {
        self.0.len() - 1
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
