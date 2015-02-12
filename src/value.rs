//! This module defines the dynamically typed value type used by Lea

pub use self::Value::*;

pub enum Value {
    TNil,
    TInt(i64),
    TFloat(f64),
    TString(String),
    TFunc(Function),
    //TArray(),
    //TTable(),
    //...
}
