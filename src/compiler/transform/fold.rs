//! Performs constant folding on literals

use compiler::Warning;
use compiler::visit::*;
use compiler::ast::*;
use compiler::span::Spanned;
use op::*;

use std::mem;

struct Folder {
    warns: Vec<Warning>,
}

/// Tries to fold a unary operator applied to a literal.
///
/// Returns `Some(Literal)` on success and `None` if the type of the literal mismatched the
/// operator's expected type. This causes a warning to be emitted.
fn fold_unop(op: UnOp, lit: &Literal) -> Option<Literal> {
    match op {
        Negate => match *lit { // -
            TInt(i) => Some(TInt(-i)),
            TFloat(f) => Some(TFloat(-f)),
            TStr(_) | TBool(_) | TNil => None,
        },
        LNot => match *lit { // ! / not
            TInt(_) | TFloat(_) | TStr(_) => Some(TBool(false)),  // all these evaluate to true
            TBool(b) => Some(TBool(!b)),
            TNil => None,
        },
        BNot => match *lit { // ~
            TInt(i) => Some(TInt(!i)),
            TFloat(_) | TStr(_) | TBool(_) | TNil => None,
        },
        Len => match *lit { // #
            TInt(_) | TFloat(_) | TBool(_) | TNil => None,

            // # of graphemes is the way to go
            TStr(ref s) => Some(TInt(s.as_slice().graphemes(true).count() as i64)),
        },
    }
}

impl Visitor for Folder {
    fn visit_expr(&mut self, e: &mut Expr) {
        match e.value {
            EBinOp(ref mut lhs, op, ref mut rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            },
            EUnOp(op, ref mut arg) => {
                self.visit_expr(arg);

                if let ELit(ref lit) = arg.value {
                    match fold_unop(op, lit) {
                        Some(newlit) => {
                            //mem::replace(&mut e.value, ELit(newlit));
                        },
                        None => {
                            // TODO print operand type
                            let msg = format!("invalid use of \"{}\" operator", op);
                            self.warns.push(Warning::new(e.span, msg));
                        }
                    }
                }
            },
            _ => {
                walk_expr(e, self);
            }
        }
    }
}

pub fn run(main: &mut Function) -> Vec<Warning> {
    let mut v = Folder {
        warns: vec![],
    };

    walk_block(&mut main.body, &mut v);
    v.warns
}
