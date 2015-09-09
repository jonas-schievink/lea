//! The `lang` library bundles core functions that integrate deeply into the language runtime and
//! form an essential part of the language.
//!
//! Examples:
//! * `assert`
//! * `error`
//! * `setmetatable`
//! * `tonumber`
//! * `tostring`
//! * `type`
//! * `pcall`

#![feature(slice_patterns)] // required by the abomination macros

#[macro_use]
extern crate lea_vm as vm;

lea_libfn! {
    fn assert(vm) {
        (val: *, msg: *, rest: ...) -> (vals: ...) => {
            if !val.is_truthy() { return Err(msg.into()) }
            return [val, msg, rest]
        }
        (val: *) -> (val: *) => {
            if !val.is_truthy() { return Err("assertion failed".to_owned().into()) }
            return [val]
        }
    }

    fn error(vm) {
        () -> () => {
            return Err("explicit error".to_owned().into())
        }
        (msg: *) -> () => {
            return Err(msg.into())
        }
        (msg: *, _level: number) -> () => {
            // TODO add error position info according to `level`
            return Err(msg.into())
        }
    }

    fn tostring(vm) {
        (val: string) -> (s: string) => {
            return [Value::String(val)]
        }
        (val: number) -> (s: string) => {
            use vm::Str;

            let s = Str::new(format!("{}", val));
            return [Value::String(vm.gc.register_obj(s))]
        }
    }
}

lea_lib! {
    _VERSION = str option_env!("CARGO_PKG_VERSION").unwrap_or("<unknown version>"),
    assert = fn assert,
    error = fn error,
    tostring = fn tostring,
}
