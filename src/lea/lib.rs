//! The `lea` crate bundles the common set of libraries used by client code: The compiler, VM and
//! runtime library.
//!
//! There is not much interesting code here, so you might want to check out the
//! [compiler](../lea_compiler) or the [vm](../lea_vm).

extern crate lea_compiler as compiler;
extern crate lea_vm as vm;
