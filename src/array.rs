//! Contains the `Array` type that's used for arrays in Lea code

use value::Value;

use std::ops::{Deref, DerefMut};

/// An array that can contain arbitrary Lea values
#[derive(RustcEncodable)]
pub struct Array(Vec<Value>);

impl Array {
    pub fn new(vec: Vec<Value>) -> Array {
        Array(vec)
    }
}

impl Deref for Array {
    type Target = Vec<Value>;

    fn deref(&self) -> &Vec<Value> {
        &self.0
    }
}

impl DerefMut for Array {
    fn deref_mut(&mut self) -> &mut Vec<Value> {
        &mut self.0
    }
}
