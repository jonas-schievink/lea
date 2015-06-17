//! Warns when a deprecated Lua operator is used (such as `~=` or `not`).

use ::Warning;
use visit::*;
use ast::*;
use op::*;

use std::ops::{Deref, DerefMut};
use std::collections::HashMap;


// Maps deprecated ops to their non-deprecated replacements
lazy_static! {
    pub static ref DEPR_BINOPS: HashMap<BinOp, BinOp> = {
        let mut m = HashMap::new();
        m.insert(BinOp::NEqLua, BinOp::NEq);
        m.insert(BinOp::LAndLua, BinOp::LAnd);
        m.insert(BinOp::LOrLua, BinOp::LOr);
        m
    };

    pub static ref DEPR_UNOPS: HashMap<UnOp, UnOp> = {
        let mut m = HashMap::new();
        m.insert(UnOp::LNotLua, UnOp::LNot);
        m
    };
}

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

impl <'a> Visitor<'a> for DeprOps {
    fn visit_expr(&mut self, e: &Expr) {
        match e.value {
            EBinOp(ref lhs, op, ref rhs) => {
                self.visit_expr(&**lhs);
                self.visit_expr(&**rhs);

                match DEPR_BINOPS.get(&op) {
                    Some(newop) => {
                        self.push(Warning::with_info(e.span,
                            format!("use of deprecated operator `{}`", op),
                            vec![format!("use `{}` instead", newop)]
                        ));
                    }
                    _ => {},
                }
            },
            EUnOp(op, ref p) => {
                self.visit_expr(&**p);

                match DEPR_UNOPS.get(&op) {
                    Some(newop) => {
                        self.push(Warning::with_info(e.span,
                            format!("use of deprecated operator `{}`", op),
                            vec![format!("use `{}` instead", newop)]
                        ));
                    },
                    _ => {},
                }
            },
            _ => { walk_expr_ref(e, self); },
        };
    }
}

pub fn run(main: Function) -> (Function, Vec<Warning>) {
    let mut v = DeprOps(vec![]);
    v.visit_func(&main);

    (main, v.0)
}
