//! Performs constant folding on literals

use compiler::Warning;
use compiler::visit::*;
use compiler::ast::*;
use compiler::span::Spanned;
use op::*;

use std::num::{Int, Float};

struct Folder {
    warns: Vec<Warning>,
}

fn unary_err(op: UnOp, lit: &Literal) -> String {
    format!("attempt to apply unary {} to type \"{}\"", op, lit.get_type_str())
}

fn bin_err(lhs: &Literal, op: BinOp, rhs: &Literal) -> String {
    format!("attempt to apply binary {} to {} and {}", op, lhs.get_type_str(), rhs.get_type_str())
}

/// Folds a literal used as a truth value (eg. as an if condition or in a logical operator).
fn fold_truth(lit: &Literal) -> bool {
    match *lit {
        TInt(_) | TFloat(_) | TStr(_) | TBool(true) => true,
        TBool(false) | TNil => false,
    }
}

/// Tries to fold a unary operator applied to a literal.
///
/// Returns `Ok(Literal)` on success and `Err(String)` with an appropriate error message if the
/// type of the literal mismatched the operator's expected type or domain. This causes a warning to
/// be emitted.
fn fold_unop(op: UnOp, lit: &Literal) -> Result<Literal, String> {
    match op {
        UnOp::Negate => match *lit { // -
            TInt(i) => Ok(TInt(-i)),
            TFloat(f) => Ok(TFloat(-f)),
            TStr(_) | TBool(_) | TNil => Err(unary_err(op, lit)),
        },
        UnOp::LNot => match *lit { // ! / not
            TInt(_) | TFloat(_) | TStr(_) => Ok(TBool(false)),  // all these evaluate to true
            TBool(b) => Ok(TBool(!b)),
            TNil => Err(unary_err(op, lit)),
        },
        UnOp::BNot => match *lit { // ~
            TInt(i) => Ok(TInt(!i)),
            TFloat(_) | TStr(_) | TBool(_) | TNil => Err(unary_err(op, lit)),
        },
        UnOp::Len => match *lit { // #
            TInt(_) | TFloat(_) | TBool(_) | TNil => Err(unary_err(op, lit)),

            // # of graphemes is the way to go
            TStr(ref s) => Ok(TInt(s.as_slice().graphemes(true).count() as i64)),
        },
    }
}

/// `fold_unop` for binary operators
fn fold_binop(lhs: &Literal, op: BinOp, rhs: &Literal) -> Result<Literal, String> {
    match op {
        BinOp::Add => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i + j)),
            (&TInt(i), &TFloat(j)) => Ok(TFloat(i as f64 + j)),
            (&TFloat(i), &TInt(j)) => Ok(TFloat(i + j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TFloat(i + j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Sub => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i - j)),
            (&TInt(i), &TFloat(j)) => Ok(TFloat(i as f64 - j)),
            (&TFloat(i), &TInt(j)) => Ok(TFloat(i - j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TFloat(i - j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Mul => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i * j)),
            (&TInt(i), &TFloat(j)) => Ok(TFloat(i as f64 * j)),
            (&TFloat(i), &TInt(j)) => Ok(TFloat(i * j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TFloat(i * j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Div => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i / j)),
            (&TInt(i), &TFloat(j)) => Ok(TFloat(i as f64 / j)),
            (&TFloat(i), &TInt(j)) => Ok(TFloat(i / j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TFloat(i / j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Mod => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i % j)),
            (&TInt(i), &TFloat(j)) => Ok(TFloat(i as f64 % j)),
            (&TFloat(i), &TInt(j)) => Ok(TFloat(i % j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TFloat(i % j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Pow => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => {
                if j > u32::max_value() as i64 {
                    Err(format!("exponent {} is out of range", j))
                } else {
                    Ok(TInt(i.pow(j as u32)))
                }
            },
            (&TInt(i), &TFloat(j)) => {
                Ok(TFloat((i as f64).powf(j)))
            },
            (&TFloat(i), &TInt(j)) => {
                if j > i32::max_value() as i64 {
                    Err(format!("exponent {} is out of range", j))
                } else {
                    Ok(TFloat(i.powi(j as i32)))
                }
            },
            (&TFloat(i), &TFloat(j)) => {
                Ok(TFloat(i.powf(j)))
            },
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Concat => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TStr(format!("{}{}", i, j))),
            (&TInt(i), &TFloat(j)) => Ok(TStr(format!("{}{}", i, j))),
            (&TFloat(i), &TInt(j)) => Ok(TStr(format!("{}{}", i, j))),
            (&TFloat(i), &TFloat(j)) => Ok(TStr(format!("{}{}", i, j))),
            (&TStr(ref s), &TInt(i)) => Ok(TStr(format!("{}{}", s, i))),
            (&TInt(i), &TStr(ref s)) => Ok(TStr(format!("{}{}", i, s))),
            (&TStr(ref s), &TFloat(f)) => Ok(TStr(format!("{}{}", s, f))),
            (&TFloat(f), &TStr(ref s)) => Ok(TStr(format!("{}{}", f, s))),
            (&TStr(ref a), &TStr(ref b)) => Ok(TStr(format!("{}{}", a, b))),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Eq | BinOp::NEq | BinOp::NEqLua => {
            let res = match (lhs, rhs) {
                (&TInt(i), &TInt(j)) => i == j,
                (&TInt(i), &TFloat(j)) => i as f64 == j,
                (&TFloat(i), &TInt(j)) => i == j as f64,
                (&TFloat(i), &TFloat(j)) => i == j,
                (&TBool(i), &TBool(j)) => i == j,
                (&TStr(ref a), &TStr(ref b)) => a == b,
                (&TNil, &TNil) => true,
                _ => false,
            };
            if op == BinOp::Eq { Ok(TBool(res)) } else { Ok(TBool(!res)) }
        },
        BinOp::LEq => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TBool(i <= j)),
            (&TInt(i), &TFloat(j)) => Ok(TBool(i as f64 <= j)),
            (&TFloat(i), &TInt(j)) => Ok(TBool(i <= j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TBool(i <= j)),
            (&TStr(ref a), &TStr(ref b)) => Ok(TBool(a <= b)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::GEq => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TBool(i >= j)),
            (&TInt(i), &TFloat(j)) => Ok(TBool(i as f64 >= j)),
            (&TFloat(i), &TInt(j)) => Ok(TBool(i >= j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TBool(i >= j)),
            (&TStr(ref a), &TStr(ref b)) => Ok(TBool(a >= b)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Less => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TBool(i < j)),
            (&TInt(i), &TFloat(j)) => Ok(TBool((i as f64) < j)),
            (&TFloat(i), &TInt(j)) => Ok(TBool(i < j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TBool(i < j)),
            (&TStr(ref a), &TStr(ref b)) => Ok(TBool(a < b)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::Greater => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TBool(i > j)),
            (&TInt(i), &TFloat(j)) => Ok(TBool(i as f64 > j)),
            (&TFloat(i), &TInt(j)) => Ok(TBool(i > j as f64)),
            (&TFloat(i), &TFloat(j)) => Ok(TBool(i > j)),
            (&TStr(ref a), &TStr(ref b)) => Ok(TBool(a > b)),
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
        _ => Err(bin_err(lhs, op, rhs)),
    }
}

impl Visitor for Folder {
    fn visit_expr(&mut self, mut e: Expr) -> Expr {
        e.value = match e.value {
            EBinOp(mut lhs, op, mut rhs) => {
                // TODO fold these too
                lhs = Box::new(self.visit_expr(*lhs));
                rhs = Box::new(self.visit_expr(*rhs));

                let lspan = lhs.span;
                let rspan = rhs.span;
                if let ELit(lit_lhs) = lhs.value {
                    if let ELit(lit_rhs) = rhs.value {
                        match fold_binop(&lit_lhs, op, &lit_rhs) {
                            Ok(newlit) => ELit(newlit),
                            Err(msg) => {
                                self.warns.push(Warning::new(e.span, msg));
                                EBinOp(Box::new(Spanned::new(lspan, ELit(lit_lhs))), op, Box::new(Spanned::new(rspan, ELit(lit_rhs))))
                            }
                        }
                    } else {
                        EBinOp(Box::new(Spanned::new(lspan, ELit(lit_lhs))), op, rhs)
                    }
                } else {
                    EBinOp(lhs, op, rhs)
                }
            },
            EUnOp(op, mut arg) => {
                arg = Box::new(self.visit_expr(*arg));

                let span = arg.span;
                if let ELit(lit) = arg.value {
                    match fold_unop(op, &lit) {
                        Ok(newlit) => ELit(newlit),
                        Err(msg) => {
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

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::*;

    /// Parses and folds `raw`, then parses `folded`. Asserts that the ASTs are equal.
    fn test(raw: &str, folded: &str) {
        let mut conf = CompileConfig::empty();
        conf.add_transform(run, LintMode::Warn);

        let main = parse_and_resolve(raw).unwrap();
        let (main, _warns) = apply_transforms(main, &conf).unwrap();
        let expected = parse_and_resolve(folded).unwrap();

        assert_eq!(main, expected);
    }

    #[test]
    fn basic() {
        test("return -0, -4.5, #\"TEST\"", "return 0, -4.5, 4");
        test("return 1-1+3*(1-1)/1^1+1", "return 1");
    }
}
