//! Constant values found in Lea source code (does not include compound data types such as
//! tables and arrays).

use Number;

#[derive(Debug, PartialEq, Clone, RustcEncodable, RustcDecodable)]
pub enum Const {
    Number(Number),
    Str(String),
    Bool(bool),
    Nil,
}

impl Const {
    /// Returns a string representation of the constant's type
    pub fn get_type_str(&self) -> &'static str {
        match *self {
            Const::Number(_) => "number",
            Const::Str(_) => "string",
            Const::Bool(_) => "boolean",
            Const::Nil => "nil",
        }
    }
}
