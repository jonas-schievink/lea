//! Contains the `Array` type that's used for arrays in Lea code

use mem::*;
use value::Value;

use std::ops::{Deref, DerefMut};

/// An array that can contain arbitrary Lea values
#[derive(Debug)]
pub struct Array(Vec<Value>);

impl Array {
    pub fn new(vec: Vec<Value>) -> Array {
        Array(vec)
    }
}

impl Traceable for Array {
    fn trace<T: Tracer>(&self, t: &mut T) {
        for v in &self.0 {
            v.trace(t);
        }
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
