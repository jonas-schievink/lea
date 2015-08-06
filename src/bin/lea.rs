//! Minimal REPL

#![feature(plugin)]
#![plugin(docopt_macros)]

extern crate docopt;
extern crate rustc_serialize;
extern crate term;
extern crate lea_compiler as compiler;
extern crate lea_parser as parser;
extern crate lea_vm as vm;

use parser::span::DummyTerm;
use compiler::{CompileConfig, FnData};
use vm::mem::DefaultGc;
use vm::error::VmResult;
use vm::function::FunctionProto;
use vm::value::Value;
use vm::vm::VM;

use std::io::{self, stdin, stderr, Write};


docopt!(Args derive Debug, "
The interactive Lea REPL

Usage:
    lea [ --help | --version ]

Options:
    -h, --help          Show this help.
    -v, --version       Show the version of the Lea package.
");


/// Opens a terminal that writes to stderr. If stderr couldn't be opened as a terminal, creates a
/// `DummyTerm` that writes to stderr instead.
fn stderr_term() -> Box<term::StderrTerminal> {
    term::stderr().unwrap_or_else(|| Box::new(DummyTerm(io::stderr())))
}

/// Compiles a piece of code. Prints all errors / warnings to stderr, using color if stderr is a
/// terminal.
fn compile(code: &str, filename: &str) -> io::Result<Option<FnData>> {
    let mut fmt_target = stderr_term();
    let fmt_target = &mut *fmt_target;

    match compiler::compile_str(code, filename, &CompileConfig::default()) {
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
fn run_fndata(main: FnData) {
    println!("{:#?}", main);

    let mut gc = DefaultGc::default();
    let proto = FunctionProto::from_fndata(main, &mut gc);
    let mut vm = VM::with_env(gc, proto, Value::TNil);

    let ret: VmResult = vm.start();
    match ret {
        Ok(vals) => {
            let vals: Vec<String> = vals.into_iter().map(|val| format!("{:?}", val)).collect();
            println!("{}", vals.join("\t"));
        }
        Err(e) => {
            println!("{:?}", e);
        }
    }
}

fn repl() -> io::Result<()> {
    let mut input = String::new();
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        try!(write!(stdout, "> "));
        try!(stdout.flush());
        try!(stdin.read_line(&mut input));

        if let Some(fndata) = try!(compile(&input, "<repl>")) {
            run_fndata(fndata);
        }

        input.clear();
    }
}

#[allow(dead_code)]     // TODO write tests
fn main() {
    match Args::docopt().decode::<Args>() {
        Err(e) => {
            println!("{}", e);
        }
        Ok(args) => {
            match args {
                Args { flag_version: true, .. } => {
                    println!("Lea {}", option_env!("CARGO_PKG_VERSION").unwrap_or("<unknown version>"));
                }
                Args { .. } => {
                    match repl() {
                        Err(e) => println!("{}", e),
                        Ok(_) => unreachable!(),
                    }
                }
            }
        }
    }
}
