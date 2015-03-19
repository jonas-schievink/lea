//! Performs constant folding on literals

use compiler::Warning;
use compiler::visit::*;
use compiler::ast::*;
use compiler::span::Spanned;
use op::*;

use std::num::{Int, Float};
use std::i64::BITS;

struct Folder {
    warns: Vec<Warning>,
}

fn unary_err(op: UnOp, lit: &Literal) -> String {
    format!("attempt to apply unary `{}` to {}", op, lit.get_type_str())
}

fn bin_err(lhs: &Literal, op: BinOp, rhs: &Literal) -> String {
    format!("attempt to apply binary `{}` to {} and {}", op, lhs.get_type_str(), rhs.get_type_str())
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
        UnOp::LNot | UnOp::LNotLua => match *lit { // ! / not
            TInt(_) | TFloat(_) | TStr(_) => Ok(TBool(false)),  // all these evaluate to true
            TBool(b) => Ok(TBool(!b)),
            TNil => Ok(TBool(true)),
        },
        UnOp::BNot => match *lit { // ~
            TInt(i) => Ok(TInt(!i)),
            TFloat(_) | TStr(_) | TBool(_) | TNil => Err(unary_err(op, lit)),
        },
        UnOp::Len => match *lit { // #
            TInt(_) | TFloat(_) | TBool(_) | TNil => Err(unary_err(op, lit)),

            // Return number of bytes in the string (compat. to Lua)
            TStr(ref s) => Ok(TInt(s.len() as i64)),
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
        BinOp::BAnd => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i & j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::BOr => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i | j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::BXor => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => Ok(TInt(i ^ j)),
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::ShiftL => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => {
                if j < 0 {
                    fold_binop(lhs, BinOp::ShiftR, &TInt(-j))
                } else {
                    if j <= BITS as i64 {
                        Ok(TInt(i << j))
                    } else {
                        Ok(TInt(0))
                    }
                }
            },
            _ => Err(bin_err(lhs, op, rhs)),
        },
        BinOp::ShiftR => match (lhs, rhs) {
            (&TInt(i), &TInt(j)) => {
                if j < 0 {
                    fold_binop(lhs, BinOp::ShiftL, &TInt(-j))
                } else {
                    if j <= BITS as i64 {
                        Ok(TInt(i >> j))
                    } else {
                        Ok(TInt(0))
                    }
                }
            },
            _ => Err(bin_err(lhs, op, rhs)),
        },
    }
}

impl Visitor for Folder {
    fn visit_expr(&mut self, mut e: Expr) -> Expr {
        e.value = match e.value {
            EBinOp(mut lhs, op, mut rhs) => {
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
    use compiler::transform::LintMode;
    use compiler::ast::Function;

    use std::str::StrExt;

    fn parse_fold(code: &str) -> (Function, Vec<Warning>) {
        let mut conf = CompileConfig::empty();
        conf.add_transform(run, LintMode::Warn);

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
        test("return 1<<2, 4>>1, 1>>90, 1<<90, 4>>-1", "return 4, 2, 0, 0, 8");
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

        test_warn("return 0.5 >> 1", &["attempt to apply binary `>>` to float and integer"]);
        test_warn("return 3 << \"\", #5", &["integer and string", "unary `#` to integer"]);
        test_warn("return 2^10000000000", &["out of range"]);
        test_warn("return -\"\", nil - true", &["unary `-` to string", "`-` to nil and boolean"]);
        test_warn("return \"1\" + 1", &["binary `+` to string and integer"]);   // NOPE.avi
    }
}
