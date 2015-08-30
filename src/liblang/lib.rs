//! The `lang` library bundles core functions that integrate deeply into the language runtime and
//! form an essential part of the language.
//!
//! Examples:
//! * `assert`
//! * `error`
//! * `setmetatable`
//! * `getmetatable`

#![feature(slice_patterns)] // required by the abomination macros

#[macro_use]
extern crate lea_vm;

lea_libfn! {
    fn assert {
        (val: *, msg: string) -> (val: *) => {
            if !val.is_truthy() { return Err(msg.into()) }
            return [val]
        }
        (val: *) -> (val: *) => {
            if !val.is_truthy() { return Err("assertion failed".to_string().into()) }
            return [val]
        }
    }

    fn error {
        (val: *) -> () => {
            return Err(val.into())
        }
    }
}

lea_lib! {
    _VERSION = str option_env!("CARGO_PKG_VERSION").unwrap_or("<unknown version>"),
    assert = fn assert,
    error = fn error,
}
