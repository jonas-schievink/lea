//! This module defines the dynamically typed value type used by Lea

pub use self::Value::*;

use table::Table;
use program::Function;

pub enum Value {
    TNil,
    TInt(i64),
    TFloat(f64),
    TString(String),
    /*TFunc(Box<Function>),
    TArray(Box<Vec<Value>>),    // TODO use own array type
    TTable(Box<Table>),*/
    //...
}
