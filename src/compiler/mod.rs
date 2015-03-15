//! This module contains the compiler, resolver and AST.
//!
//! The complation process can be divided into the following phases:
//! 1. Parsing
//! 2. Checking
//! 3. Resolving
//! 4. Linting / Optimizing
//! 5. Emitting
//!
//! This module provides several helper methods that will either wrap up the whole compilation
//! process or stop after a step, to enable more advanced uses of the compiler.
//!
//! Below is a brief description of what each phase does.
//!
//! ## Parsing
//!
//! This phase runs the `rust-peg` generated parser. It is generated at compile-time from the
//! Parsing Expression Grammar (PEG) in "lea.rustpeg" with help of the `rust-peg` crate.
//!
//! After the generated parser was run, an additional expression parser is run. The generated
//! parser will collect binary expressions in a list, ignoring operator precedences. It returns a
//! `ERawOp` expression for all expressions. The expression parser in "expr_parser.rs" runs over
//! all `ERawOp`s, applies the shunting-yard algorithm, builds a tree of `EBinOp`s, and replaces
//! the AST node with the result.
//!
//! This could be done in the PEG parser, but since `rust-peg` doesn't support memoization, it
//! leads to a significant slowdown. Additionally, this would require redefining all operator
//! precedences as PEG rules (which duplicates code that shouldn't ever be duplicate), and
//! supporting both left- and right-associative operators is easier in a shunting-yard parser.
//!
//! ## Checking
//!
//! In "check.rs", a `Checker` visitor is implemented, which runs over an AST and verifies that all
//! statements and expressions are used in a valid context. Since the PEG parser has no state, it
//! cannot do this (and it is better - though slower - to do it this way, since it separates
//! syntax error and context-dependant errors).
//!
//! For example, the checker will deny a `break` statement used outside of a loop and the varargs
//! expression `...` outside of a varargs function.
//!
//! ## Resolving
//!
//! The resolver implemented in "resolve.rs" transforms any named variable (`VNamed` in the AST) to
//! a local, upvalue, or global reference.
//!
//! ## Linting / Optimizing
//!
//! In this step, all configured Linters and Optimizers are run on the resolved AST. Internally, we
//! make to distinction between a Linter and an Optimizer: Both take a `Function` node, do
//! something to it, and return a transformed `Function` along with a list of warnings to be
//! issued.
//!
//! Most Linters will just return the unmodified function and generate some warnings, while most
//! optimizers will modify the AST, but return no warnings. Of course, there are exceptions.
//!
//! Lea, being derived from Lua, is a dynamically typed language. This means that the type of an
//! expression or variable is only known at runtime. Since metatables allow for operator
//! overloading, what an operator does can only be known at runtime in the general case. This
//! prevents many optimizations like strength reduction and reassociation. Some optimizations can
//! (and should) still be performed: Most prominently, constant folding.
//!
//! Linters and Optimizers are implemented in the files inside the "transform" directory:
//!
//! "fold.rs" implements a constant folding optimization that looks for operators that are applied
//! to literals, applies them at compile-time, and replaces the node with the result. This is a
//! basic optimization performed by most compilers (Lua does this too).
//!
//! "globalwrite.rs" implements a Linter that will issue a warning for all assignments to globals.
//! Global variables are generally meant to hold the standard library functions, and not much more.
//! User code should always prefer locals, and modules are meant to return the module when loaded,
//! which should be assigned to a local by the code that `require`d the module. If you still need
//! to access a global, you can do so explicitly by indexing `_ENV`.
//!
//! ## Emitting
//!
//! The byte code emitter for the compiler still needs to be written. It will take an AST and
//! convert it to machine code for the virtual machine.

mod expr_parser;
pub mod transform;
pub mod ast;
pub mod check;
pub mod emitter;
pub mod parser;
pub mod prettyprint;
pub mod resolve;
pub mod span;
pub mod visit;

pub use self::CompileError::*;

use program::Program;
use self::ast::Function;
use self::transform::{Transform, LintMode};
use self::span::Span;

use std::default::Default;


/// Defines options that can be used to tweak the compilation process (optimizations, linters, ...)
pub struct CompileConfig {
    /// AST transforms to apply. All transforms are applied in the order specified here.
    trans: Vec<(Transform, LintMode)>,
}

impl CompileConfig {
    /// Creates an empty `CompileConfig` that applies no optimizations or lints
    pub fn empty() -> CompileConfig {
        CompileConfig {
            trans: vec![],
        }
    }

    /// Creates a `CompileConfig` that will apply named lints. Returns `None` if a Lint doesn't
    /// exist (TODO).
    pub fn named_lints(names: &[&str]) -> Option<CompileConfig> {
        let mut conf = CompileConfig::empty();
        for name in names {
            match transform::TRANSFORMS.get(name) {
                None => { return None; },
                Some(tr) => {
                    conf.trans.push((*tr, LintMode::Warn));
                }
            }
        }

        Some(conf)
    }

    pub fn add_transform(&mut self, tr: Transform, mode: LintMode) {
        self.trans.push((tr, mode));
    }
}

impl Default for CompileConfig {
    /// Creates a default `CompileConfig` that applies the default set of optimizers and linters
    fn default() -> CompileConfig {
        CompileConfig {
            trans: transform::TRANSFORMS_DEFAULT.clone(),
        }
    }
}

/// Kinds of errors that can occur when compiling code
#[derive(Debug)]
pub enum CompileError {
    /// The input source code could not be parsed
    ErrParse(parser::ParseError),

    /// The checker encountered one or more problems
    ErrCheck(Vec<check::CheckError>),

    /// One or more Lints have issued warnings and were configured to cause a compilation error.
    /// Contains all warnings of all Lints that were configured to error.
    ErrLint(Vec<Warning>),
}

/// A warning emitted by an AST transform (Linter/Optimizer)
#[derive(Clone, Debug)]
pub struct Warning {
    span: Span,
    message: String,
    /// Additional help text attached to the message (one per line)
    info: Vec<String>,
}

impl Warning {
    pub fn new(span: Span, message: String) -> Warning {
        Warning::with_info(span, message, vec![])
    }

    pub fn with_info(span: Span, message: String, info: Vec<String>) -> Warning {
        Warning {
            span: span,
            message: message,
            info: info,
        }
    }

    /// Formats this warning, its span, and all attached info lines. Does not append a trailing
    /// newline.
    pub fn format(&self, code: &str, source_name: &str) -> String {
        let (startline, _end) = self.span.get_lines(code);
        let mut res = format!("{}:{}: warning: {} \n", source_name, startline, self.message);
        res.push_str(self.span.format(code, source_name).as_slice());

        for info in &self.info {
            res.push_str(format!("\n{}:{}: info: {}", source_name, startline, info).as_slice());
        }

        res
    }
}

/// Output of a successful compilation.
pub struct CompileOutput {
    /// Warnings generated by Linters
    warns: Vec<Warning>,
}

impl CompileOutput {
    pub fn get_warns(&self) -> &[Warning] {
        self.warns.as_slice()
    }
}

/// Parses and checks the given source code (stops after compilation step 2). Returns a valid
/// `Function` node on success.
///
/// If this succeeds, this means that the program described by the source code is valid and can be
/// compiled.
pub fn parse_and_check(code: &str) -> Result<Function, CompileError> {
    match parser::parse_main(code) {
        Err(e) => Err(ErrParse(e)),
        Ok(main) => {
            let (main, res) = check::check_func(main);
            match res {
                Err(errs) => Err(ErrCheck(errs)),
                Ok(()) => {
                    Ok(main)
                }
            }
        }
    }
}

/// Parses, checks and resolves the given source code (stops after compilation step 3). Returns the
/// resolved main function on success.
pub fn parse_and_resolve(code: &str) -> Result<Function, CompileError> {
    let mut main = try!(parse_and_check(code));
    main = resolve::resolve_func(main);

    Ok(main)
}

/// Applies all transforms in the given `CompileConfig` to a `main` function. Returns all warnings
/// generated by the transforms. `main` must be a resolved main function.
///
/// If any transform is configured to cause a compilation error, a corresponding error will be
/// returned.
pub fn apply_transforms(mut main: Function, conf: &CompileConfig) -> Result<(Function, Vec<Warning>), CompileError> {
    let mut warnings: Vec<Warning> = vec![];
    let mut errwarns: Vec<Warning> = vec![];  // warnings handled as errors
    for &(tr, mode) in &conf.trans {
        let (newmain, mut new_warns) = tr(main);
        main = newmain;

        if mode == LintMode::Error && new_warns.len() > 0 {
            errwarns.append(&mut new_warns.clone());
        }

        warnings.append(&mut new_warns);
    }

    if errwarns.len() > 0 {
        Err(ErrLint(errwarns))
    } else {
        Ok((main, warnings))
    }
}

/// Compiles source code into a program. This executes the whole compilation process and emits
/// byte code (TODO).
///
/// # Parameters
///
/// * `p` - The `Program` to which the compiled functions and constants are added to
/// * `code` - The source code to compile
/// * `source_name` - The name of this source code. Used when reporting errors. This should be
///   a file name or something similar.
/// * `conf` - The `CompileConfig` to use for this compilation
pub fn compile_str(_p: &mut Program, code: &str, _source_name: &str,
        conf: &CompileConfig) -> Result<CompileOutput, CompileError> {
    let main = try!(parse_and_resolve(code));

    let (_, warnings) = try!(apply_transforms(main, &conf));

    Ok(CompileOutput {
        warns: warnings,
    })
}
