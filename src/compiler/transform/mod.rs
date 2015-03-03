//! Contains AST transformers such as Linters and Optimizers that can be run on an AST and report
//! warnings or mutate it.

use compiler::ast::Function;
use compiler::Warning;

pub mod globalwrite;


pub type Transform = fn(&mut Function) -> Vec<Warning>;

pub static TRANSFORMS: ::phf::Map<&'static str, Transform> = phf_map! {
    "globalwrite" => globalwrite::run as Transform,
};
