//! Contains AST transformers such as Linters and Optimizers that can be run on an AST and report
//! warnings or mutate it.

use compiler::ast::Function;
use compiler::Warning;

use std::collections::HashMap;


pub mod deprecated_ops;
pub mod fold;
pub mod globalwrite;

/// Transform function used by Linters and Optimizers
pub type Transform = fn(Function) -> (Function, Vec<Warning>);

/// Specifies how a Lint's (or Optimizer's) emitted warnings should be handled by the compiler.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum LintMode {
    /// The warnings will be ignored and not returned to the caller.
    Ignore,
    /// The warnings will be returned to the caller. It is up to them to handle / print them.
    Warn,
    /// Any warning emitted by this Lint will cause a compilation error.
    Error,
}


/// Creates a `HashMap` that maps the given name to the `run` function declared in the module with
/// the same name (`run` must have a compatible signature).
macro_rules! transform_map {
    ( $( $name:ident, )* ) => {{
        let mut map = HashMap::new();
        $( map.insert(stringify!($name), $name::run as Transform); )*

        map
    }};
}

/// Creates a vector of 2-tuples, containing the transform function and default severity of
/// warnings returned by the function.
macro_rules! transform_vec {
    ( $( $name:ident : $mode:ident, )* ) => {{
        vec![
            $( ( $name :: run as Transform, LintMode::$mode), )*
        ]
    }};
}

/// `TRANSFORMS` contains all transformations that can be applied to an AST.
lazy_static! {
    pub static ref TRANSFORMS: HashMap<&'static str, Transform> = transform_map! {
        deprecated_ops,
        fold,
        globalwrite,
    };
}

/// `TRANSFORMS_DEFAULT` is the default set of transforms to apply when compiling Lea code.
lazy_static! {
    pub static ref TRANSFORMS_DEFAULT: Vec<(Transform, LintMode)> = transform_vec! [
        deprecated_ops: Warn,
        globalwrite: Warn,
        fold: Ignore,
    ];
}

/// `TRANSFORMS_COMPAT` is a list of transforms to apply when compiling in compatibility mode (ie
/// Lua code).
lazy_static! {
    pub static ref TRANSFORMS_COMPAT: Vec<(Transform, LintMode)> = transform_vec! [
        fold: Ignore,
    ];
}
