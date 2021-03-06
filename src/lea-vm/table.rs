//! Contains Lea's table type.
//!
//! # Weak tables
//!
//! A table can have weak keys and/or values. These will reference objects weakly and make no
//! difference for value types and strings. If an object is only reachable through weak references,
//! the GC will still collect the object and then modify all weak references that pointed to it to
//! make sure that they are still valid and don't point to freed memory.
//!
//! In case of weak tables, an entry will be removed if either the key or the value is collected
//! due to being only weakly reachable.
//!
//! A table with weak values will thus delete a value's mapping if the value isn't reachable from
//! outside the table and allows the table to be used like a cache.
//!
//! A table with weak keys will delete a key's mapping if the key is only weakly reachable. It is
//! acting as an "ephemeron" table and allows associating properties with objects that will be
//! automatically cleared when the object is collected.

#![macro_use]

use Value;
use mem::{TracedRef, Tracer, Traceable};

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/// A table that maps Lea values to Lea values.
#[derive(Debug, Default)]
pub struct Table {
    data: HashMap<Value, Value>,
    metatable: Option<TracedRef<Table>>,
    weak_keys: bool,
    weak_vals: bool,
}

impl Table {
    /// Creates a new table, using the given `HashMap<Value, Value>` as the internal storage.
    pub fn new(data: HashMap<Value, Value>) -> Table {
        Table {
            data: data,
            metatable: None,
            weak_keys: false,
            weak_vals: false,
        }
    }

    pub fn get_weak_keys(&self) -> bool {
        self.weak_keys
    }

    pub fn set_weak_keys(&mut self, weak: bool) {
        self.weak_keys = weak;
    }

    pub fn get_weak_values(&self) -> bool {
        self.weak_vals
    }

    pub fn set_weak_values(&mut self, weak: bool) {
        self.weak_vals = weak;
    }

    /// Equivalent to assignment by Lea code. If the value is nil, the mapping is deleted. Returns
    /// the old value stored with the given key or `Err(())` if the key is nil (this is not
    /// supported).
    pub fn set(&mut self, k: Value, v: Value) -> Result<Option<Value>, ()> {
        if k == Value::Nil { return Err(()); }

        if v == Value::Nil {
            Ok(self.data.remove(&k))
        } else {
            Ok(self.data.insert(k, v))
        }
    }

    pub fn get(&self, k: &Value) -> Value {
        match self.data.get(k) {
            None => Value::Nil,
            Some(&v) => v,
        }
    }
}

impl Traceable for Table {
    fn trace<T: Tracer>(&self, t: &mut T) {
        for (ref k, ref v) in &self.data {
            k.trace(t);
            v.trace(t);
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

macro_rules! hashmap {
    ( $($k:expr => $v:expr),* ) => {{
        let mut m = ::std::collections::HashMap::new();
        $( m.insert($k, $v); )*
        m
    }};
    ( $($k:expr => $v:expr,)* ) => {
        hashmap!($($k => $v),*)
    };
}

macro_rules! table {
    ( $($t:tt)* ) => {
        $crate::table::Table::new(hashmap!($($t)*))
    };
}

#[cfg(test)]
mod tests {
    use value::Value;

    #[test]
    fn test() {
        let mut t = table! {
            Value::Number(0.5.into()) => Value::Number(42.into()),
            Value::Bool(true) => Value::Number(24.into()),
        };

        assert_eq!(t.set(Value::Bool(false), Value::Number(123.0.into())), Ok(None));
        assert_eq!(t.data, hashmap! {
            Value::Number(0.5.into()) => Value::Number(42.into()),
            Value::Bool(true) => Value::Number(24.into()),
            Value::Bool(false) => Value::Number(123.0.into()),
        });

        assert_eq!(t.set(Value::Bool(true), Value::Nil), Ok(Some(Value::Number(24.into()))));
        assert_eq!(t.data, hashmap! {
            Value::Number(0.5.into()) => Value::Number(42.into()),
            Value::Bool(false) => Value::Number(123.0.into()),
        });

        assert_eq!(t.set(Value::Nil, Value::Nil), Err(()));
    }
}
