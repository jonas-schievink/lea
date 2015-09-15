//! This module contains the compiler. The compiler parses source code and generates byte code
//! suitable for execution by the VM.
//!
//! The complation process can be divided into the following phases:
//!
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
//! This phase runs the generated Lea parser. It is generated at compile-time from the Parsing
//! Expression Grammar (PEG) in `lea.rustpeg` with help of the `rust-peg` crate.
//!
//! ## Checking
//!
//! In `check.rs`, a `Checker` visitor is implemented, which runs over an AST and verifies that all
//! statements and expressions are used in a valid context. Since the PEG parser has no state, it
//! cannot do this (and it is better - though slower - to do it this way, since it separates
//! syntax error and context-dependant errors).
//!
//! For example, the checker will deny a `break` statement used outside of a loop and the varargs
//! expression `...` outside of a varargs function.
//!
//! ## Resolving
//!
//! The resolver implemented in `resolve.rs` transforms any named variable (`VNamed` in the AST) to
//! a local, upvalue, or global reference. This makes it easy to see which identifier refers to
//! which variable (might be useful for code highlighting in IDEs) and makes the emitter a bit
//! simpler, since it doesn't have to figure out what an identifier means if it encounters it: It
//! already knows, since the resolver stored it in the AST.
//!
//! ## Linting / Optimizing
//!
//! In this step, all configured Lints and Optimizers are run on the resolved AST. Internally, we
//! make no distinction between a Lint and an Optimizer: Both take a `Function` node, do
//! something to it, and return a transformed `Function` along with a list of warnings to be
//! issued.
//!
//! Most Lints will just return the unmodified function and generate some warnings, while most
//! optimizers will modify the AST, but return no warnings. Of course, there might be exceptions.
//!
//! Lints and Optimizers are implemented in the files inside the "transform" directory:
//!
//! * `fold.rs` implements a constant folding optimization that looks for operators that are
//!   applied to literals, applies them at compile-time, and replaces the node with the result.
//!   This is a basic optimization performed by most compilers (Lua does this too).
//!
//! * `globalwrite.rs` implements a Lint that will warn for all assignments to globals. Global
//!   variables are generally meant to hold the standard library functions, and not much more. User
//!   code should always prefer locals, and modules are meant to return the module when loaded,
//!   which should be assigned to a local by the code that `require`d the module. If you still need
//!   to access a global (for example, when setting up a sandbox), you can do so explicitly by
//!   indexing `_ENV`.
//!
//! * `deprecated_ops.rs` is a Lint that emits a warning whenever a Lua operator is used that has
//!   an equivalent replacement in Lea.
//!
//! ## Emitting
//!
//! The byte code emitter takes a resolved AST and emits byte code for each function that can be
//! executed by the VM.

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate log;

extern crate rustc_serialize;
extern crate term;

extern crate lea_core;
extern crate lea_parser as parser;

use transform::{Transform, LintMode};
use emitter::emit_func;

use ast::Function;


mod errors;
mod check;
mod ast;
mod emitter;
mod resolve;
pub mod transform;

pub use errors::*;
pub use lea_core::fndata::FnData;


/// Defines options that can be used to tweak the compilation process (optimizations, lints, ...)
pub struct CompileConfig<'a> {
    /// AST transforms to apply. All transforms are applied in the order specified here.
    trans: Vec<(&'a Transform, LintMode)>,
}

impl<'a> CompileConfig<'a> {
    /// Creates an empty `CompileConfig` that runs no lints
    pub fn empty() -> CompileConfig<'a> {
        CompileConfig {
            trans: vec![],
        }
    }

    /// Creates a `CompileConfig` that will apply named lints. Returns `None` if a Lint doesn't
    /// exist (TODO).
    pub fn named_lints(names: &[&str]) -> Option<CompileConfig<'a>> {
        let mut conf = CompileConfig::empty();
        for name in names {
            match transform::TRANSFORMS.get(name) {
                None => { return None; },
                Some(tr) => {
                    conf.trans.push((tr, LintMode::Warn));
                }
            }
        }

        Some(conf)
    }

    pub fn add_transform(&mut self, tr: &'a Transform, mode: LintMode) {
        self.trans.push((tr, mode));
    }
}

impl<'a> Default for CompileConfig<'a> {
    /// Creates a default `CompileConfig` that applies the default set of optimizers and lints
    fn default() -> CompileConfig<'a> {
        CompileConfig {
            trans: transform::TRANSFORMS_DEFAULT.iter()
                .map(|&(name, lvl)| (transform::TRANSFORMS.get(name).unwrap(), lvl)).collect(),
        }
    }
}

/// Output of a successful compilation.
pub struct CompileOutput<'a> {
    /// Warnings generated by Lints
    pub warns: Vec<Warning>,
    /// AST node of main function
    pub main: Function<'a>,
    /// Prototype of main function, to be instantiated and run by the VM
    pub mainproto: FnData,
}

/// Parses and checks the given source code (stops after compilation step 2). Returns a valid
/// `Function` node on success.
///
/// If this succeeds, this means that the program described by the source code is valid and can be
/// compiled.
pub fn parse_and_check(code: &str) -> CompileResult<Function> {
    match parser::parse_main(code) {
        Err(e) => Err(ErrParse(e)),
        Ok(main) => {
            let main = main.into();
            let res = check::check_func(&main);
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
pub fn parse_and_resolve(code: &str) -> CompileResult<Function> {
    let mut main = try!(parse_and_check(code));
    main = resolve::resolve_func(main);

    Ok(main)
}

/// Applies all transforms in the given `CompileConfig` to a `main` function. Returns all warnings
/// generated by the transforms. `main` must be resolved beforehand.
///
/// If any transform is configured to cause a compilation error, a corresponding error will be
/// returned.
pub fn apply_transforms<'a>(mut main: Function<'a>, conf: &CompileConfig) -> (Function<'a>, CompileResult<Vec<Warning>>) {
    let mut warnings: Vec<Warning> = Vec::new();
    let mut errwarns: Vec<Warning> = Vec::new();  // warnings handled as errors
    for &(tr, mode) in &conf.trans {
        let (newmain, new_warns) = tr(main);
        main = newmain;

        if mode == LintMode::Error && !new_warns.is_empty() {
            errwarns.extend(new_warns);
        } else {
            warnings.extend(new_warns);
        }
    }

    (main, if errwarns.is_empty() {
        Ok(warnings)
    } else {
        Err(ErrLint(errwarns))
    })
}

/// Compiles source code into an `FnData` instance. This executes the whole compilation process and
/// emits byte code.
///
/// Note that this will not return the `Function` node on error, even if parsing succeeded.
///
/// # Parameters
///
/// * `code` - The source code to compile
/// * `source_name` - The name of this source code. Used when reporting errors. This should be
///   a file name or something similar.
/// * `conf` - The `CompileConfig` to use for this compilation
pub fn compile_str<'a>(code: &'a str, source_name: &str, conf: &CompileConfig) -> CompileResult<CompileOutput<'a>> {
    let main = try!(parse_and_resolve(code));
    let (main, tr_res) = apply_transforms(main, &conf);
    let warnings = try!(tr_res);
    let emit_res = emit_func(&main, source_name);

    match emit_res {
        Ok(proto) => Ok(CompileOutput {
            warns: warnings,
            main: main,
            mainproto: proto,
        }),
        Err(errs) => Err(ErrEmit(errs)),
    }
}

/// Compiles an expression into an `FnData` object that will return the result(s) of the expression
/// to the caller.
pub fn compile_expr<'a>(expr: &'a str, source_name: &str, conf: &CompileConfig) -> CompileResult<CompileOutput<'a>> {
    let main = try!(parser::parse_expr_as_main(expr));
    let main = main.into();
    try!(check::check_func(&main));
    let main = resolve::resolve_func(main);
    let (main, tr_res) = apply_transforms(main, &conf);
    let warnings = try!(tr_res);
    let emit_res = emit_func(&main, source_name);

    match emit_res {
        Ok(proto) => Ok(CompileOutput {
            warns: warnings,
            main: main,
            mainproto: proto,
        }),
        Err(errs) => Err(ErrEmit(errs)),
    }
}
