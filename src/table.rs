//! Contains Lea's table type

use value::Value;

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/// A table that maps Lea values to Lea values
#[derive(RustcEncodable, RustcDecodable, Debug)]
pub struct Table {
    data: HashMap<Value, Value>,
    metatable: Option<Box<Table>>,  // TODO GcRef
}

impl Table {
    pub fn new(data: HashMap<Value, Value>) -> Table {
        Table {
            data: data,
            metatable: None,
        }
    }
}

impl Deref for Table {
    type Target = HashMap<Value, Value>;

    fn deref(&self) -> &<Self as Deref>::Target {
        &self.data
    }
}

impl DerefMut for Table {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.data
    }
}
