//! Performs constant folding on literals

use compiler::Warning;
use compiler::visit::*;
use compiler::ast::*;
use compiler::span::Spanned;
use op::*;

struct Folder {
    warns: Vec<Warning>,
}

/// Tries to fold a unary operator applied to a literal.
///
/// Returns `Some(Literal)` on success and `None` if the type of the literal mismatched the
/// operator's expected type. This causes a warning to be emitted.
fn fold_unop(op: UnOp, lit: &Literal) -> Option<Literal> {
    match op {
        UnOp::Negate => match *lit { // -
            TInt(i) => Some(TInt(-i)),
            TFloat(f) => Some(TFloat(-f)),
            TStr(_) | TBool(_) | TNil => None,
        },
        UnOp::LNot => match *lit { // ! / not
            TInt(_) | TFloat(_) | TStr(_) => Some(TBool(false)),  // all these evaluate to true
            TBool(b) => Some(TBool(!b)),
            TNil => None,
        },
        UnOp::BNot => match *lit { // ~
            TInt(i) => Some(TInt(!i)),
            TFloat(_) | TStr(_) | TBool(_) | TNil => None,
        },
        UnOp::Len => match *lit { // #
            TInt(_) | TFloat(_) | TBool(_) | TNil => None,

            // # of graphemes is the way to go
            TStr(ref s) => Some(TInt(s.as_slice().graphemes(true).count() as i64)),
        },
    }
}

impl Visitor for Folder {
    fn visit_expr(&mut self, mut e: Expr) -> Expr {
        e.value = match e.value {
            EBinOp(mut lhs, op, mut rhs) => {
                // TODO fold these too
                lhs = Box::new(self.visit_expr(*lhs));
                rhs = Box::new(self.visit_expr(*rhs));
                EBinOp(lhs, op, rhs)
            },
            EUnOp(op, mut arg) => {
                arg = Box::new(self.visit_expr(*arg));

                let span = arg.span;
                if let ELit(lit) = arg.value {
                    match fold_unop(op, &lit) {
                        Some(newlit) => ELit(newlit),
                        None => {
                            let msg = format!("invalid use of \"{}\" operator on literal of type \"{}\"", op, lit.get_type_str());
                            self.warns.push(Warning::new(e.span, msg));
                            EUnOp(op, Box::new(Spanned::new(span, ELit(lit))))
                        }
                    }
                } else {
                    EUnOp(op, arg)
                }
            },
            _ => { return walk_expr(e, self) },
        };

        e
    }
}

pub fn run(mut main: Function) -> (Function, Vec<Warning>) {
    let mut v = Folder {
        warns: vec![],
    };

    main = walk_func(main, &mut v);
    (main, v.warns)
}
