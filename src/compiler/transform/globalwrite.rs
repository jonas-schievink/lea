//! Linter that emits a warning for all writes to globals. Note that this doesn't warn for writes
//! to anything else, so `math.s = ...` is fine, while `math = ...` would cause a warning.

use compiler::Warning;
use compiler::visit::*;
use compiler::ast::*;

struct GlobalWrite {
    warns: Vec<Warning>,
}

impl Visitor for GlobalWrite {
    fn visit_stmt(&mut self, s: &mut Stmt) {
        match s.value {
            SAssign(ref mut targets, _) => {
                for v in targets {
                    match v.value {
                        VResGlobal(_, ref name) => {
                            self.warns.push(Warning::new(v.span, format!("write to global variable \"{}\" (you should prefer locals)", name)));
                        },
                        _ => {},
                    };
                }
            },
            _ => {
                walk_stmt(s, self);
            }
        };
    }
}

pub fn run(main: &mut Function) -> Vec<Warning> {
    let mut v = GlobalWrite {
        warns: vec![],
    };

    walk_block(&mut main.body, &mut v);
    v.warns
}
