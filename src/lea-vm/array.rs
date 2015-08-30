//! Contains the `Array` type that's used for arrays in Lea code

use mem::{Tracer, Traceable};
use value::Value;

use std::ops::{Deref, DerefMut};

/// An array that can contain Lea values. This represents the `Array` value.
///
/// Arrays employ an invariant: They may not contain `Nil`. This was done to prevent confusion
/// about array length and to prevent programming errors caused by stray `nil` elements, but is
/// mainly an experiment.
///
/// When getting a value stored in an array, `Nil` will be returned if and only if the index is
/// invalid (negative or >= the array length).
///
/// When setting values, the index must be between 0 and the array length (inclusive): If the index
/// is equal to the array length, the array length will increase by 1 (push operation). `Nil` can
/// only be stored when the index is equal to the array length minus 1, causing the array length to
/// decrease by 1 (pop operation).
#[derive(Debug, Default)]
pub struct Array(Vec<Value>);

impl Array {
    pub fn new(vec: Vec<Value>) -> Array {
        Array(vec)
    }

    /// Index operation, as seen by Lea code (directly called by VM). Array indexing can never
    /// fail and will return `Nil` when out of bounds.
    pub fn get(&self, idx: i64) -> Value {
        if idx < 0 {
            Value::Nil
        } else {
            *self.0.get(idx as usize).unwrap_or(&Value::Nil)
        }
    }

    /// Set the value at an index.
    ///
    /// Unlike `get`, this might fail:
    /// * if `idx` is negative
    /// * if `idx` is greater than the array length (so that we'd have to fill in a default value
    ///   to close the gap)
    /// * if `value` is `Nil` and `idx` isn't equal to the array length - 1 (which would cause a
    ///   `nil` hole inside the array)
    pub fn set(&mut self, idx: i64, value: Value) -> Result<(), ()> {
        if idx < 0 {
            Err(())
        } else if idx as usize > self.0.len() {
            Err(())
        } else {
            if idx as usize == self.0.len() {
                // push
                if value == Value::Nil {
                    Err(())
                } else {
                    self.0.push(value);
                    Ok(())
                }
            } else {
                if value == Value::Nil {
                    // must store to largest index (pop)
                    if idx as usize != self.0.len() - 1 {
                        Err(())
                    } else {
                        self.0.pop();
                        Ok(())
                    }
                } else {
                    // simple overwrite
                    self.0[idx as usize] = value;
                    Ok(())
                }
            }
        }
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
