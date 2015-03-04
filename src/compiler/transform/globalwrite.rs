//! Linter that emits a warning for all writes to globals. Note that this doesn't warn for writes
//! to anything else, so `math.s = ...` is fine, while `math = ...` would cause a warning.

use compiler::Warning;
use compiler::visit::*;
use compiler::ast::*;

struct GlobalWrite {
    warns: Vec<Warning>,
}

impl Visitor for GlobalWrite {
    fn visit_stmt(&mut self, mut s: Stmt) -> Stmt {
        s.value = match s.value {
            SAssign(targets, values) => {
                for v in &targets {
                    match v.value {
                        VResGlobal(_, ref name) => {
                            self.warns.push(Warning::new(v.span, format!("write to global variable \"{}\" (you should prefer locals)", name)));
                        },
                        _ => {},
                    };
                }

                SAssign(targets, values)
            },
            _ => { return walk_stmt(s, self) },
        };

        s
    }
}

pub fn run(mut main: Function) -> (Function, Vec<Warning>) {
    let mut v = GlobalWrite {
        warns: vec![],
    };

    main = walk_func(main, &mut v);
    (main, v.warns)
}
