//! Linter that emits a warning for all writes to globals. Note that this doesn't warn for writes
//! to anything else, so `math.s = ...` is fine, while `math = ...` would cause a warning.
//!
//! Explicit environment accesses with `_ENV.name` will not cause a warning, and should be used if
//! the global access is intended (mostly when setting up new function environments / sandboxes).

use ::Warning;

use ast::visit::*;
use ast::*;

use std::ops::{Deref, DerefMut};


struct GlobalWrite(Vec<Warning>);

impl Deref for GlobalWrite {
    type Target = Vec<Warning>;
    fn deref(&self) -> &Vec<Warning> {
        &self.0
    }
}

impl DerefMut for GlobalWrite {
    fn deref_mut(&mut self) -> &mut Vec<Warning> {
        &mut self.0
    }
}

impl <'a> Visitor<'a> for GlobalWrite {
    fn visit_stmt(&mut self, s: &Stmt) {
        match s.value {
            SAssign(ref targets, _) => {
                for v in targets {
                    match v.value {
                        VResGlobal(_, ref name) => {
                            let message = format!("write to global variable `{}` (you should prefer locals)", name);
                            self.push(Warning::with_info(v.span, message, vec![
                                format!("declare a local variable with `local {}`", name),
                                format!("or explicitly access the global with `_ENV.{}`", name),
                            ]));
                        },
                        _ => {},
                    }
                }
            },
            _ => walk_stmt_ref(s, self),
        }
    }
}

pub fn run(main: Function) -> (Function, Vec<Warning>) {
    let mut v = GlobalWrite(vec![]);
    v.visit_func(&main);

    (main, v.0)
}
