//! This module defines the dynamically typed value type used by Lea

use Table;
use Array;
use Str;
use function::Function;
use mem::{TracedRef, Tracer, GcStrategy};
use number::Number;
use libfn::LibFn;

use lea_core::constant::Const;

use std::io::{self, Write};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(Number),
    String(TracedRef<Str>),
    Closure(TracedRef<Function>),
    Array(TracedRef<Array>),
    Table(TracedRef<Table>),
    LibFn(LibFn),
}

impl Value {
    /// The equivalent of Lua's `type()` function, returns the type name of a value.
    pub fn get_type_name(&self) -> &'static str {
        match *self {
            Value::Nil => "nil",
            Value::Bool(..) => "boolean",
            Value::Number(..) => "number",
            Value::String(..) => "string",
            Value::Table(..) => "table",
            Value::Array(..) => "array",
            Value::Closure(..) => "function",
            Value::LibFn(..) => "function",
        }
    }

    /// Is this value considered true or false when used in a boolean context (if/loop condition,
    /// shortcircuit operators)?
    pub fn is_truthy(&self) -> bool {
        match *self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }

    /// If this `Value` references a GC-object, marks it using the given `Tracer`.
    pub fn trace<T: Tracer>(&self, t: &mut T) {
        match *self {
            Value::Nil | Value::Bool(_) | Value::Number(_) | Value::LibFn(_) => {},
            Value::String(r) => unsafe { t.mark_untraceable(r) },    // Safe, since T is Str
            Value::Table(r) => t.mark_traceable(r),
            Value::Array(r) => t.mark_traceable(r),
            Value::Closure(r) => t.mark_traceable(r),
        }
    }

    pub fn from_literal<G: GcStrategy>(c: Const, gc: &mut G) -> Value {
        match c {
            Const::Int(i) => Value::Number(i.into()),
            Const::Float(f) => Value::Number(f.into()),
            Const::Str(s) => Value::String(gc.intern_str(Str::new(s))),
            Const::Bool(b) => Value::Bool(b),
            Const::Nil => Value::Nil,
        }
    }

    /// Formats this `Value`. This requires a reference to the GC that owns the referenced objects
    /// and is unsafe because the `TracedRef`s are dereferenced using the GC.
    pub unsafe fn fmt<G: GcStrategy, W: Write>(&self, mut f: W, gc: &G) -> io::Result<()> {
        match *self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", gc.get_ref(s)),
            Value::Table(t) => write!(f, "table:{:p}", t),
            Value::Array(a) => write!(f, "array:{:p}", a),
            Value::Closure(func) => write!(f, "function:{:p}", func),
            Value::LibFn(libfn) => write!(f, "function:{:p}", libfn),
        }
    }
}
