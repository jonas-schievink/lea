//! Literal types/values found in Lea source code (does not include compound data types such as
//! tables and arrays).

pub use self::Literal::*;

/// Literal constants
#[derive(Debug, PartialEq, Clone, RustcEncodable, RustcDecodable)]
pub enum Literal {
    TInt(i64),
    TFloat(f64),
    TStr(String),
    TBool(bool),
    TNil,
}

impl Literal {
    /// Returns a string representation of the literal's type
    pub fn get_type_str(&self) -> &'static str {
        match *self {
            TInt(_) => "integer",
            TFloat(_) => "float",
            TStr(_) => "string",
            TBool(_) => "boolean",
            TNil => "nil",
        }
    }
}
