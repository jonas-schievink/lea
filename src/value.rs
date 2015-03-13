//! This module defines the dynamically typed value type used by Lea

pub use self::Value::*;

/*use table::Table;
use array::Array;
use program::Function;*/

use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

/// Wrapper around `f64` that implements `Hash` and `Eq` (needed when used as table keys).
#[derive(RustcEncodable, RustcDecodable, PartialEq, Debug)]
pub struct HashedFloat(f64);

/// Manual implementation needed since `f64` doesn't implement Hash, but this is required since
/// floats can be used as table keys (although I don't recommend it).
impl Hash for HashedFloat {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        state.write_i64(self.0 as i64);
    }

    fn hash_slice<H>(data: &[Self], state: &mut H) where H: Hasher {
        for s in data {
            state.write_i64(s.0 as i64);
        }
    }
}

impl Eq for HashedFloat {}

impl Deref for HashedFloat {
    type Target = f64;

    fn deref(&self) -> &f64 {
        &self.0
    }
}

impl DerefMut for HashedFloat {
    fn deref_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}


#[derive(RustcEncodable, RustcDecodable, PartialEq, Eq, Debug, Hash)]
pub enum Value {
    TNil,
    TBool(bool),
    TInt(i64),
    TFloat(HashedFloat),
    TString(Box<String>),
    /*TFunc(Box<Function>),
    TArray(Box<Array>),
    TTable(Box<Table>),*/
}

impl Value {
    pub fn get_type_name(&self) -> &'static str {
        match *self {
            TNil => "nil",
            TBool(..) => "boolean",
            TInt(..) | TFloat(..) => "number",
            TString(..) => "string",
        }
    }
}
