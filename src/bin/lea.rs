//! Minimal REPL

#![feature(plugin)]
#![plugin(docopt_macros)]

extern crate docopt;
extern crate rustc_serialize;
extern crate term;
extern crate lea_compiler as compiler;
extern crate lea_parser as parser;
extern crate lea_vm as vm;
extern crate lea;

use parser::span::DummyTerm;
use compiler::{CompileConfig, FnData};
use vm::mem::DefaultGc;
use vm::error::VmResult;
use vm::function::FunctionProto;
use vm::value::Value;
use vm::vm::VM;

use std::io::{self, stdin, stderr, Write, BufRead};


docopt!(Args derive Debug, "
The interactive Lea REPL

Usage:
    lea [ --help | --version ]
    lea [ -e <code> ]... [--] <file> [ <arg> ]...
    lea [ -e <code> ]... [-i | --interactive]

Options:
    -h, --help          Show this help
    -v, --version       Show the version of the Lea package
    -e, --exec <code>   Execute <code> as a piece of Lea script
    -i, --interactive   Enter interactive mode after processing all arguments
", arg_file: Option<String>);


/// Opens a terminal that writes to stderr. If stderr couldn't be opened as a terminal, creates a
/// `DummyTerm` that writes to stderr instead.
fn stderr_term() -> Box<term::StderrTerminal> {
    term::stderr().unwrap_or_else(|| Box::new(DummyTerm(io::stderr())))
}

/// Compiles a piece of code (expression or block of statements). Prints all errors / warnings to
/// stderr, using color if stderr is a terminal.
fn compile(code: &str, filename: &str) -> io::Result<Option<FnData>> {
    let mut fmt_target = stderr_term();
    let fmt_target = &mut *fmt_target;

    // try to compile an expression first
    let mut result = compiler::compile_expr(code, filename, &CompileConfig::default());
    if result.is_err() {
        result = compiler::compile_str(code, filename, &CompileConfig::default());
    }

    match result {
        Err(e) => {
            try!(e.format(code, filename, fmt_target));
            Ok(None)
        },
        Ok(output) => {
            let warns = output.warns;
            if warns.len() > 0 {
                for w in warns {
                    try!(w.format(code, filename, fmt_target));
                }
            }

            Ok(Some(output.mainproto))
        }
    }
}

/// Creates a VM and GC and executes the given `FnData` object. Prints the returned value or the
/// error thrown.
///
/// Returns `true` if the code executed successfully and `false` if the VM returned with an error.
fn run_fndata(main: FnData, vm: &mut VM<DefaultGc>) -> bool {
    // XXX This really needs to be easier
    use std::rc::Rc;
    use std::cell::Cell;
    use vm::function::{Function, Upval};
    use vm::mem::GcStrategy;

    let proto = FunctionProto::from_fndata(main, vm.gc_mut());
    let mut first = true;
    let f = Function::new(vm.gc(), proto, |_| if first {
        first = false;
        // XXX Set up the stdlib here
        Rc::new(Cell::new(Upval::Closed(Value::TNil)))
    } else {
        Rc::new(Cell::new(Upval::Closed(Value::TNil)))
    });
    let f = vm.gc_mut().register_obj(f);

    let ret: VmResult = vm.start(f);
    match ret {
        Ok(vals) => {
            if !vals.is_empty() {
                for (i, val) in vals.into_iter().enumerate() {
                    if i != 0 {
                        print!("\t");
                    }

                    unsafe { val.fmt(io::stdout(), vm.gc()) }.unwrap();
                }
                println!("");
            }

            true
        }
        Err(e) => {
            println!("runtime error: {}", e);

            false
        }
    }
}

fn run_code(code: &str, file: &str, vm: &mut VM<DefaultGc>) -> io::Result<bool> {
    if let Some(fndata) = try!(compile(code, file)) {
        Ok(run_fndata(fndata, vm))
    } else {
        Ok(false)   // compile error
    }
}

fn run_file(filename: &str, vm: &mut VM<DefaultGc>) -> io::Result<bool> {
    use std::fs::File;
    use std::io::Read;

    let mut file = try!(File::open(filename));
    let mut code = String::new();
    try!(file.read_to_string(&mut code));

    run_code(&code, filename, vm)
}

fn print_prompt() -> io::Result<()> {
    let mut stdout = io::stdout();
    try!(write!(stdout, "> "));
    try!(stdout.flush());

    Ok(())
}

fn repl(vm: &mut VM<DefaultGc>) -> io::Result<()> {
    let stdin = io::stdin();
    let stdin = io::BufReader::new(stdin);

    try!(print_prompt());
    for input in stdin.lines() {
        let input = try!(input);
        try!(run_code(&input, "<repl>", vm));

        try!(print_prompt());
    }

    Ok(())
}

fn build_vm() -> VM<DefaultGc> {
    VM::new(DefaultGc::default())
}

#[allow(dead_code)]     // TODO write tests
fn main() {
    match Args::docopt().decode::<Args>() {
        Err(e) => {
            println!("{}", e);
        }
        Ok(args) => match args {
            Args { flag_version: true, .. } => {
                println!("Lea {}", lea::version_str());
            }
            Args { flag_exec, flag_interactive, arg_file, /*arg_arg,*/ .. } => {
                let enter_repl = flag_interactive || (flag_exec.len() == 0 && arg_file.is_none());
                let mut vm = build_vm();

                for code in flag_exec {
                    match run_code(&code, "<cmdline>", &mut vm) {
                        Ok(true) => {}
                        Ok(false) => {return}
                        Err(e) => {
                            println!("{}", e);
                            return
                        }
                    }
                }

                // TODO Pass the arguments to the program
                if let Some(file) = arg_file {
                    match run_file(&file, &mut vm) {
                        Ok(true) => {}
                        Ok(false) => {return}
                        Err(e) => {
                            println!("{}", e);
                            return
                        }
                    }
                }

                if enter_repl {
                    match repl(&mut vm) {
                        Err(e) => println!("{}", e),
                        Ok(_) => {}
                    }
                }
            }
        }
    }
}
