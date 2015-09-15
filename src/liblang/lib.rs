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
extern crate lea_compiler as compiler;

use vm::VM;
use vm::mem::{GcStrategy, TracedRef};
use vm::{Str, Value};
use vm::function::{Function, FunctionProto, Upval};

use compiler::{CompileConfig, compile_str};

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
            return [format!("{}", val)]
        }
    }

    fn tonumber(vm) {
        (num: number) -> (num: number) => {
            return [num]
        }
        (s: string) -> (num: number) => {
            let result = unsafe { vm.gc.get_ref(s) }.parse::<::vm::number::LeaFloat>();
            let num = match result {
                Ok(num) => num,
                Err(e) => return Err(format!("{}", e).into()),
            };

            return [num]
        }
    }

    fn type_name(vm) {
        (of: *) -> (typename: string) => {
            return [of.get_type_name()]
        }
    }

    fn print(vm) {
        (values: ...) -> () => {
            for (i, val) in values.iter().enumerate() {
                if i != 0 {
                    print!("\t");
                }

                unsafe { val.fmt(::std::io::stdout(), &vm.gc) }.unwrap();
            }
            println!("");
        }
    }

    fn load(vm) {
        (code: string, source_name: string, _mode: string, env: *) -> (compiled: fn, err: string) => {
            let res = match super::load_impl(vm, code, Some(source_name), Some(env)) {
                Ok(res) => res,
                Err(e) => return Err(e.into()),
            };

            return [res]
        }
        (code: string, source_name: string, _mode: string) -> (compiled: fn, err: string) => {
            let res = match super::load_impl(vm, code, Some(source_name), None) {
                Ok(res) => res,
                Err(e) => return Err(e.into()),
            };

            return [res]
        }
        (code: string, source_name: string) -> (compiled: fn, err: string) => {
            let res = match super::load_impl(vm, code, Some(source_name), None) {
                Ok(res) => res,
                Err(e) => return Err(e.into()),
            };

            return [res]
        }
        (code: string) -> (compiled: fn, err: string) => {
            let res = match super::load_impl(vm, code, None, None) {
                Ok(res) => res,
                Err(e) => return Err(e.into()),
            };

            return [res]
        }
    }

    /*fn dofile(vm) {
        () -> (ret: ...) => {
            use std::io::{Read, stdin};

            let mut buf = String::new();
            let _res: &[Value] = match stdin().read_to_string(&mut buf) {
                Ok(_) => {
                    let code = vm.gc.intern_str(buf);

                    match super::load_impl(vm, code, None, None) {
                        Ok(_func) => unimplemented!(),    // TODO call this
                        Err(e) => return Err(e.into()),
                    }
                }
                Err(e) => return Err(format!("couldn't read from stdin: {}", e).into())
            };

            return [_res]
        }
        (filename: string) -> (ret: ...) => {
            use std::io::Read;
            use std::fs::File;

            let mut file = match File::open(unsafe { &vm.gc.get_ref(filename) as &str }) {
                Ok(f) => f,
                Err(e) => return Err(format!(
                    "couldn't open file '{}': {}",
                    unsafe { &vm.gc.get_ref(filename) }, e).into()),
            };

            let mut buf = String::new();
            let _res: &[Value] = match file.read_to_string(&mut buf) {
                Ok(_) => {
                    let code = vm.gc.intern_str(buf);

                    match super::load_impl(vm, code, None, None) {
                        Ok(_func) => unimplemented!(),    // TODO call this
                        Err(e) => return Err(e.into()),
                    }
                }
                Err(e) => return Err(format!(
                    "couldn't read file '{}': {}",
                    unsafe { &vm.gc.get_ref(filename) }, e).into())
            };

            return [_res]
        }
    }*/
}

fn load_impl(vm: &mut VM,
             code: TracedRef<Str>,
             source_name: Option<TracedRef<Str>>,
             env: Option<Value>)
             -> Result<Value, String> {
    let compile_result = {
        let cfg = CompileConfig::default();
        let code: &str = unsafe { &vm.gc.get_ref(code) };
        let source_name: &str = match source_name {
            Some(s) => unsafe { &vm.gc.get_ref(s) },
            None => "<load>",
        };

        // compile and dispose of the unneeded data. since the AST is borrowed, this wouldn't work.
        compile_str(code, source_name, &cfg).map(|result| result.mainproto)
    };

    match compile_result {
        Ok(res) => {
            let proto = FunctionProto::from_fndata(res, &mut vm.gc);
            let main = vm.main_ref();
            let main_env: Value = match unsafe { vm.gc.get_ref(main).upvalues[0].get() } {
                Upval::Closed(env) => env,
                Upval::Open(_) => unreachable!(),
            };

            let f = Function::with_env(&vm.gc, proto, env.unwrap_or(main_env));

            Ok(Value::Closure(vm.gc.register_obj(f)))
        }
        Err(e) => {
            Err(format!("{:?}", e)) // FIXME impl Display for CompileError
        }
    }
}

lea_lib! {
    _VERSION = str option_env!("CARGO_PKG_VERSION").unwrap_or("<unknown version>"),
    assert = fn assert,
    error = fn error,
    tostring = fn tostring,
    tonumber = fn tonumber,
    type = fn type_name,
    print = fn print,
    load = fn load,
    //dofile = fn dofile,
}
