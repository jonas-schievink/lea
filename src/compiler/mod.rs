//! This module contains the compiler, resolver and AST

mod expr_parser;
pub mod ast;
pub mod check;
pub mod parser;
pub mod prettyprint;
pub mod resolve;
pub mod span;
pub mod visit;

pub use self::CompileError::*;

use program::Program;

/// Kind of errors that can occur when compiling code
pub enum CompileError {
    /// The input source code could not be parsed
    ErrParse(parser::ParseError),

    /// The checker encountered one or more problems
    ErrCheck(Vec<check::CheckError>),
}

/// The result of a compilation.
///
/// If the compilation is successful, this returns the function id of the main function. This
/// function can be instantiated by the VM and will execute the program.
pub type CompileResult = Result<usize, CompileError>;

/// Compiles source code into a program.
///
/// # Parameters
///
/// * `p` - The `Program` to which the compiled functions and constants are added to
/// * `code` - The source code to compile
/// * `source_name` - The name of this source code. Used when reporting errors. This should be
///   a file name or something similar.
pub fn compile_str(_p: &mut Program, code: &str, _source_name: &str) -> CompileResult {
    match parser::parse_main(code) {
        Err(e) => Err(ErrParse(e)),
        Ok(_main) => {
            unimplemented!();   // TODO
        },
    }
}
