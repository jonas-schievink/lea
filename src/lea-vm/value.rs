//! This module defines the dynamically typed value type used by Lea

pub use self::Value::*;

use table::Table;
use array::Array;
use function::Function;
use mem::{TracedRef, Tracer, GcStrategy};
use number::Number;

use lea_core::constant::Const;

use std::io::{self, Write};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Value {
    TNil,
    TBool(bool),
    TNumber(Number),
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
            TNumber(..) => "number",
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
            TNil | TBool(_) | TNumber(_) => {},
            TStr(r) => unsafe { t.mark_untraceable(r) },    // Safe, since T is String
            TFunc(r) => t.mark_traceable(r),
            TArray(r) => t.mark_traceable(r),
            TTable(r) => t.mark_traceable(r),
        }
    }

    pub fn from_literal<G: GcStrategy>(c: Const, gc: &mut G) -> Value {
        match c {
            Const::Int(i) => TNumber(i.into()),
            Const::Float(f) => TNumber(f.into()),
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
            TNumber(n) => write!(f, "{}", n),
            TStr(s) => write!(f, "{}", gc.get_ref(s)),
            TFunc(func) => write!(f, "function:{:p}", func),
            TArray(a) => write!(f, "array:{:p}", a),
            TTable(t) => write!(f, "table:{:p}", t),
        }
    }
}
