//! This module defines the dynamically typed value type used by Lea

pub use self::Value::*;

use table::Table;
use array::Array;
use function::Function;
use mem::{TracedRef, Tracer, GcStrategy};

use lea_core::constant::Const;

use std::io::{self, Write};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::mem::transmute;

/// Wrapper around `f64` that implements `Hash` and `Eq` (needed when used as table keys).
///
/// Lua supports this, so we will, too. I would still never recommend doing this!
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct HashedFloat(pub f64);

/// Manual implementation needed since `f64` doesn't implement Hash, but this is required since
/// floats can be used as table keys (although I don't recommend it).
impl Hash for HashedFloat {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Just use the float's raw bits for the hash. This is safe.
        state.write_i64(unsafe { transmute(self.0) } );
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


#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Value {
    TNil,
    TBool(bool),
    TInt(i64),
    TFloat(HashedFloat),
    TStr(TracedRef<String>),
    TFunc(TracedRef<Function>),
    TArray(TracedRef<Array>),
    TTable(TracedRef<Table>),
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

    /// Is this value considered true or false when used in a boolean context (if/loop condition,
    /// shortcircuit operators)?
    pub fn is_truthy(&self) -> bool {
        match *self {
            TNil | TBool(false) => false,
            _ => true,
        }
    }

    /// If this `Value` references a GC-object, marks it using the given `Tracer`.
    pub fn trace<T: Tracer>(&self, t: &mut T) {
        match *self {
            TNil | TBool(_) | TInt(_) | TFloat(_) => {},
            TStr(r) => unsafe { t.mark_untraceable(r) },    // Safe, since T is String
            TFunc(r) => t.mark_traceable(r),
            TArray(r) => t.mark_traceable(r),
            TTable(r) => t.mark_traceable(r),
        }
    }

    pub fn from_literal<G: GcStrategy>(c: Const, gc: &mut G) -> Value {
        match c {
            Const::Int(i) => TInt(i),
            Const::Float(f) => TFloat(HashedFloat(f)),
            Const::Str(s) => TStr(gc.register_obj(s)),
            Const::Bool(b) => TBool(b),
            Const::Nil => TNil,
        }
    }

    /// Formats this `Value`. This requires a reference to the GC that owns the referenced objects
    /// and is unsafe because the `TracedRef`s are dereferenced using the GC.
    pub unsafe fn fmt<G: GcStrategy, W: Write>(&self, mut f: W, gc: &G) -> io::Result<()> {
        match *self {
            TNil => write!(f, "nil"),
            TBool(b) => write!(f, "{}", b),
            TInt(i) => write!(f, "{}", i),
            TFloat(flt) => write!(f, "{}", *flt),
            TStr(s) => write!(f, "{}", gc.get_ref(s)),
            TFunc(func) => write!(f, "function:{:p}", func),
            TArray(a) => write!(f, "array:{:p}", a),
            TTable(t) => write!(f, "table:{:p}", t),
        }
    }
}
