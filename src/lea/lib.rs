//! The `lea` crate bundles the common set of libraries used by client code: The compiler, VM and
//! runtime library.
//!
//! There is not much interesting code here, so you might want to check out the
//! [compiler](../lea_compiler) or the [vm](../lea_vm).

extern crate lea_compiler as compiler;
extern crate lea_vm as vm;
extern crate lea_liblang as liblang;

use vm::table::Table;
use vm::mem::GcStrategy;
use vm::value::Value;

/// Returns a version string for the main Lea crate (this crate).
pub fn version_str() -> &'static str {
    option_env!("CARGO_PKG_VERSION").unwrap_or("<unknown version>")
}

/// Initializes the current version of the standard library. Returns the global environment that
/// contains the library.
pub fn build_stdlib<G: GcStrategy>(gc: &mut G) -> Value {
    let mut env = Table::default();
    liblang::init(&mut env, gc);

    Value::Table(gc.register_obj(env))
}
