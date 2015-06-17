//! Contains the `Array` type that's used for arrays in Lea code

use mem::*;
use value::Value;

use std::ops::{Deref, DerefMut};

/// An array that can contain arbitrary Lea values
#[derive(Debug)]
pub struct Array<'gc>(Vec<Value<'gc>>);

impl <'gc> Array<'gc> {
    pub fn new(vec: Vec<Value<'gc>>) -> Array<'gc> {
        Array(vec)
    }
}

impl <'gc> GcObj for Array<'gc> {}
impl <'gc> Traceable for Array<'gc> {
    fn trace<T: Tracer>(&self, t: &mut T) {
        for v in &self.0 {
            v.trace(t);
        }
    }
}

impl <'gc> Deref for Array<'gc> {
    type Target = Vec<Value<'gc>>;

    fn deref(&self) -> &Vec<Value<'gc>> {
        &self.0
    }
}

impl <'gc> DerefMut for Array<'gc> {
    fn deref_mut(&mut self) -> &mut Vec<Value<'gc>> {
        &mut self.0
    }
}
