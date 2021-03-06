//! Performs constant folding on literals

use ::Warning;

use ast::*;
use ast::visit::*;
use parser::span::Spanned;
use parser::op::*;

use lea_core::Const;


struct Folder {
    warns: Vec<Warning>,
}

fn unary_err(op: UnOp, lit: &Const) -> String {
    format!("attempt to apply unary `{}` to {}", op, lit.get_type_str())
}

fn bin_err(lhs: &Const, op: BinOp, rhs: &Const) -> String {
    format!("attempt to apply binary `{}` to {} and {}", op, lhs.get_type_str(), rhs.get_type_str())
}

/// Folds a literal used as a truth value (eg. as an if condition or in a logical operator).
fn fold_truth(lit: &Const) -> bool {
    match *lit {
        Const::Number(_) | Const::Str(_) | Const::Bool(true) => true,
        Const::Bool(false) | Const::Nil => false,
    }
}

/// Tries to fold a unary operator applied to a literal.
///
/// Returns `Ok(Const)` on success and `Err(String)` with an appropriate error message if the
/// type of the literal mismatched the operator's expected type or domain.
fn fold_unop(op: UnOp, lit: &Const) -> Result<Const, String> {
    match op {
        UnOp::Negate => match *lit { // -
            Const::Number(i) => Ok(Const::Number(-i)),
            Const::Str(_) | Const::Bool(_) | Const::Nil => Err(unary_err(op, lit)),
        },
        UnOp::LNot | UnOp::LNotLua => Ok(Const::Bool(!fold_truth(lit))),
        UnOp::BNot => match *lit { // ~
            Const::Number(i) => Ok(Const::Number(!i)),
            Const::Str(_) | Const::Bool(_) | Const::Nil => Err(unary_err(op, lit)),
        },
        UnOp::Len => match *lit { // #
            Const::Number(_) | Const::Bool(_) | Const::Nil => Err(unary_err(op, lit)),

            // Return number of bytes in the string (compat. to Lua)
            Const::Str(ref s) => Ok(Const::Number((s.len() as i64).into())),
        },
    }
}

/// Tries to fold a binary operator applied to 2 literals.
///
/// Returns `Ok(Const)` on success and `Err(String)` if the operator is applied to invalid types
/// or values.
fn fold_binop(lhs: &Const, op: BinOp, rhs: &Const) -> Result<Const, String> {
    match op {
        BinOp::Add => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i + j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Sub => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i - j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Mul => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i * j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Div => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i / j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Mod => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i % j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Pow => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i.pow(j))),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Concat => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Str(format!("{}{}", i, j))),
            (&Const::Str(ref s), &Const::Number(i)) => Ok(Const::Str(format!("{}{}", s, i))),
            (&Const::Number(i), &Const::Str(ref s)) => Ok(Const::Str(format!("{}{}", i, s))),
            (&Const::Str(ref a), &Const::Str(ref b)) => Ok(Const::Str(format!("{}{}", a, b))),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Eq | BinOp::NEq | BinOp::NEqLua => {
            let res = match (lhs, rhs) {
                (&Const::Number(i), &Const::Number(j)) => i == j,
                (&Const::Bool(i), &Const::Bool(j)) => i == j,
                (&Const::Str(ref a), &Const::Str(ref b)) => a == b,
                (&Const::Nil, &Const::Nil) => true,
                _ => false,
            };
            if op == BinOp::Eq { Ok(Const::Bool(res)) } else { Ok(Const::Bool(!res)) }
        },
        BinOp::LEq => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Bool(i <= j)),
            (&Const::Str(ref a), &Const::Str(ref b)) => Ok(Const::Bool(a <= b)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::GEq => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Bool(i >= j)),
            (&Const::Str(ref a), &Const::Str(ref b)) => Ok(Const::Bool(a >= b)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Less => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Bool(i < j)),
            (&Const::Str(ref a), &Const::Str(ref b)) => Ok(Const::Bool(a < b)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Greater => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Bool(i > j)),
            (&Const::Str(ref a), &Const::Str(ref b)) => Ok(Const::Bool(a > b)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::LAnd | BinOp::LAndLua => {
            if fold_truth(lhs) {
                Ok(rhs.clone())
            } else {
                Ok(lhs.clone())
            }
        },
        BinOp::LOr | BinOp::LOrLua => {
            if fold_truth(lhs) {
                Ok(lhs.clone())
            } else {
                Ok(rhs.clone())
            }
        },
        BinOp::BAnd => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i & j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::BOr => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i | j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::BXor => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i ^ j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::ShiftL => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i << j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::ShiftR => match (lhs, rhs) {
            (&Const::Number(i), &Const::Number(j)) => Ok(Const::Number(i >> j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
    }
}

impl<'a> Transform<'a> for Folder {
    fn visit_expr(&mut self, mut e: Expr<'a>) -> Expr<'a> {
        e.value = match e.value {
            ExprKind::BinOp(mut lhs, op, mut rhs) => {
                lhs = Box::new(self.visit_expr(*lhs));
                rhs = Box::new(self.visit_expr(*rhs));

                let lspan = lhs.span;
                let rspan = rhs.span;
                if let ExprKind::Lit(lit_lhs) = lhs.value {
                    if let ExprKind::Lit(lit_rhs) = rhs.value {
                        match fold_binop(&lit_lhs, op, &lit_rhs) {
                            Ok(newlit) => ExprKind::Lit(newlit),
                            Err(msg) => {
                                self.warns.push(Warning::new(e.span, msg));
                                ExprKind::BinOp(Box::new(Spanned::new(lspan, ExprKind::Lit(lit_lhs))), op, Box::new(Spanned::new(rspan, ExprKind::Lit(lit_rhs))))
                            }
                        }
                    } else {
                        ExprKind::BinOp(Box::new(Spanned::new(lspan, ExprKind::Lit(lit_lhs))), op, rhs)
                    }
                } else {
                    ExprKind::BinOp(lhs, op, rhs)
                }
            },
            ExprKind::UnOp(op, mut arg) => {
                arg = Box::new(self.visit_expr(*arg));

                let span = arg.span;
                if let ExprKind::Lit(lit) = arg.value {
                    match fold_unop(op, &lit) {
                        Ok(newlit) => ExprKind::Lit(newlit),
                        Err(msg) => {
                            self.warns.push(Warning::new(e.span, msg));
                            ExprKind::UnOp(op, Box::new(Spanned::new(span, ExprKind::Lit(lit))))
                        }
                    }
                } else {
                    ExprKind::UnOp(op, arg)
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

#[cfg(test)]
mod tests {
    use super::*;
    use ::{CompileConfig, Warning, parse_and_resolve, apply_transforms};
    use transform::{Transform, LintMode};
    use ast::Function;

    fn parse_fold<'a>(code: &'a str) -> (Function<'a>, Vec<Warning>) {
        let tr = run as Transform;
        let mut conf = CompileConfig::empty();
        conf.add_transform(&tr, LintMode::Warn);

        let main = parse_and_resolve(code).unwrap();
        let (main, res) = apply_transforms(main, &conf);

        (main, res.unwrap())
    }

    /// Parses and folds `raw`, then parses `folded`. Asserts that the ASTs are equal.
    fn test(raw: &str, folded: &str) {
        let (main, warns) = parse_fold(raw);
        let expected = parse_and_resolve(folded).unwrap();

        assert_eq!(main, expected);
        assert_eq!(warns.len(), 0);
    }

    #[test]
    fn basic() {
        test("return 1+1, -0, -(4.5), -(2), #\"TEST\", ~0", "return 2, 0, -4.5, -2, 4, -1");
        test("return not true, not false", "return false, true");
        test("return 1-1+3*(1.0-1)/1^1+1.0", "return 1.0");
        test("return 1&&2, 1||0, false||3, true&&false", "return 2, 1, 3, false");
        test("return false&&3||2, true&&3||2", "return 2, 3");
        test("return 2|1, 2&3, -1~1", "return 3, 2, -2");
        test("return !(true && (true || nil))", "return false");
        test("return 1<<2, 4>>1, 4>>-1", "return 4, 2, 8");
        test("return !0, \"test\"..1..\"a\", 2<3 and 1>=0.5", "return false, \"test1a\", true");
        test("return not nil, 0.5*2.0, 0.5*2, 2%3, 2^2.0", "return true, 1.0, 1.0, 2, 4.0");
    }

    #[test]
    fn warnings() {
        fn test_warn(code: &str, msgs: &[&str]) {
            let mut i = 0;
            let (_, warns) = parse_fold(code);
            assert_eq!(warns.len(), msgs.len());

            for warn in warns {
                let msg = warn.get_message();
                assert!(msg.contains(msgs[i]), format!("got: {}; exp: {}", msg, msgs[i]));
                i += 1;
            }
        }

        test_warn("return 3 << \"\", #5", &["number and string", "unary `#` to number"]);
        test_warn("return -\"\", nil - true", &["unary `-` to string", "`-` to nil and boolean"]);
        test_warn("return \"1\" + 1", &["binary `+` to string and number"]);   // NOPE.avi
    }
}
