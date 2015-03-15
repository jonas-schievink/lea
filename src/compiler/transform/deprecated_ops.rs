//! Warns when a deprecated Lua operator is used (such as `~=` or `not`).

use compiler::Warning;
use compiler::visit::*;
use compiler::ast::*;
use op::*;

use std::ops::{Deref, DerefMut};


struct DeprOps(Vec<Warning>);
impl Deref for DeprOps {
    type Target = Vec<Warning>;
    fn deref(&self) -> &Vec<Warning> {
        &self.0
    }
}
impl DerefMut for DeprOps {
    fn deref_mut(&mut self) -> &mut Vec<Warning> {
        &mut self.0
    }
}

impl Visitor for DeprOps {
    fn visit_expr(&mut self, mut e: Expr) -> Expr {
        e.value = match e.value {
            EBinOp(mut lhs, op, mut rhs) => {
                lhs = Box::new(self.visit_expr(*lhs));
                rhs = Box::new(self.visit_expr(*rhs));
                match op {
                    BinOp::NEqLua => {
                        self.push(Warning::with_info(e.span, "use of deprecated operator `~=`".to_string(),
                            vec!["use `!=` instead".to_string()]));
                    }
                    _ => {},
                };

                EBinOp(lhs, op, rhs)
            },
            EUnOp(op, mut p) => {
                p = Box::new(self.visit_expr(*p));
                match op {
                    UnOp::LNotLua => {
                        self.push(Warning::with_info(e.span, "use of deprecated operator `not`".to_string(),
                            vec!["use `!` instead".to_string()]));
                    },
                    _ => {},
                };

                EUnOp(op, p)
            },
            _ => { return walk_expr(e, self); },
        };

        e
    }
}

pub fn run(mut main: Function) -> (Function, Vec<Warning>) {
    let mut v = DeprOps(vec![]);

    main = walk_func(main, &mut v);
    (main, v.0)
}
