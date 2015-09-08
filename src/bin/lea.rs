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
use vm::function::FunctionProto;
use vm::Value;
use vm::VM;

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
            if !warns.is_empty() {
                for w in warns {
                    try!(w.format(code, filename, fmt_target));
                }
            }

            Ok(Some(output.mainproto))
        }
    }
}

/// Executes the given `FnData` object using the given VM. Prints the returned value or the error
/// thrown.
///
/// Returns `true` if the code executed successfully and `false` if the VM returned with an error.
fn run_fndata(main: FnData, vm: &mut VM<DefaultGc>, env: Value) -> bool {
    // XXX This really needs to be easier
    use std::rc::Rc;
    use std::cell::Cell;
    use vm::function::{Function, Upval};
    use vm::mem::GcStrategy;

    let proto = FunctionProto::from_fndata(main, &mut vm.gc);
    let mut first = true;
    let f = Function::new(&vm.gc, proto, |_| if first {
        first = false;
        Rc::new(Cell::new(Upval::Closed(env)))
    } else {
        Rc::new(Cell::new(Upval::Closed(Value::Nil)))
    });
    let f = vm.gc.register_obj(f);

    match vm.start(f, |error| {
        println!("runtime error: {}", error);
    }) {
        None => false,
        Some(vals) => {
            if !vals.is_empty() {
                for (i, val) in vals.into_iter().enumerate() {
                    if i != 0 {
                        print!("\t");
                    }

                    unsafe { val.fmt(io::stdout(), &vm.gc) }.unwrap();
                }
                println!("");
            }

            true
        }
    }
}

fn run_code(code: &str, file: &str, vm: &mut VM<DefaultGc>, env: Value) -> io::Result<bool> {
    if let Some(fndata) = try!(compile(code, file)) {
        Ok(run_fndata(fndata, vm, env))
    } else {
        Ok(false)   // compile error
    }
}

fn run_file(filename: &str, vm: &mut VM<DefaultGc>, env: Value) -> io::Result<bool> {
    use std::fs::File;
    use std::io::Read;

    let mut file = try!(File::open(filename));
    let mut code = String::new();
    try!(file.read_to_string(&mut code));

    run_code(&code, filename, vm, env)
}

fn print_prompt() -> io::Result<()> {
    let mut stdout = io::stdout();
    try!(write!(stdout, "> "));
    try!(stdout.flush());

    Ok(())
}

fn repl(vm: &mut VM<DefaultGc>, env: Value) -> io::Result<()> {
    let stdin = io::stdin();
    let stdin = io::BufReader::new(stdin);

    try!(print_prompt());
    for input in stdin.lines() {
        let input = try!(input);
        try!(run_code(&input, "<repl>", vm, env));

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
                let enter_repl = flag_interactive || (flag_exec.is_empty() && arg_file.is_none());
                let mut vm = build_vm();
                let env = lea::build_stdlib(&mut vm.gc);

                for code in flag_exec {
                    match run_code(&code, "<cmdline>", &mut vm, env) {
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
                    match run_file(&file, &mut vm, env) {
                        Ok(true) => {}
                        Ok(false) => {return}
                        Err(e) => {
                            println!("{}", e);
                            return
                        }
                    }
                }

                if enter_repl {
                    match repl(&mut vm, env) {
                        Err(e) => println!("{}", e),
                        Ok(_) => {}
                    }
                }
            }
        }
    }
}
