//! This module defines the dynamically typed value type used by Lea

pub use self::Value::*;

use table::Table;
use array::Array;
use program::Function;
use mem::GcRef;

use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

/// Wrapper around `f64` that implements `Hash` and `Eq` (needed when used as table keys).
///
/// Lua supports this, so we will, too. I would still never recommend doing this!
#[derive(RustcEncodable, PartialEq, Debug)]
pub struct HashedFloat(pub f64);

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


#[derive(RustcEncodable, PartialEq, Eq, Debug, Hash)]
pub enum Value {
    TNil,
    TBool(bool),
    TInt(i64),
    TFloat(HashedFloat),
    TStr(GcRef<String>),
    TFunc(GcRef<Function>),
    TArray(GcRef<Array>),
    TTable(GcRef<Table>),
}

impl Value {
    /// The equivalent of Lua's `type()` function, returns the type name of a value.
    pub fn get_type_name(&self) -> &'static str {
        match *self {
            TNil => "nil",
            TBool(..) => "boolean",
            TInt(..) | TFloat(..) => "number",
            TStr(..) => "string",
            TTable(..) => "table",
            TArray(..) => "array",
            TFunc(..) => "function",
        }
    }
}
