use parser::{ident, integer, float, string, literal, expression};

#[test]
fn integers() {
    assert_eq!(integer("0"), Ok(0));
    assert_eq!(integer("1"), Ok(1));
    assert_eq!(integer("-1"), Ok(-1));
    assert_eq!(integer("9999999999"), Ok(9999999999i64));
    assert_eq!(integer("-9999999999"), Ok(-9999999999i64));

    assert_eq!(integer("0x00"), Ok(0));
    assert_eq!(integer("-0x00"), Ok(0));
    assert_eq!(integer("0xff"), Ok(255));
    assert_eq!(integer("-0xff"), Ok(-255));
    assert_eq!(integer("0xffffffffff"), Ok(0xffffffffff));

    assert_eq!(integer("0o76"), Ok(0o76));
    assert_eq!(integer("-0o76"), Ok(-0o76));
}

#[test]
fn floats() {
    assert_eq!(float("0.0"), Ok(0.0));
    assert_eq!(float("-0.0"), Ok(-0.0));
    assert_eq!(float("10000.0"), Ok(10000.0));

    assert_eq!(float("1e4"), Ok(1e+4));
    assert_eq!(float("1e+4"), Ok(1e+4));
    assert_eq!(float("-1e+4"), Ok(-1e+4));
    assert_eq!(float("2e-4"), Ok(2e-4));
}

#[test]
fn idents() {
    assert_eq!(ident("test"), Ok("test"));
    assert_eq!(ident("_a0_0_"), Ok("_a0_0_"));
    assert_eq!(ident("_"), Ok("_"));
    assert_eq!(ident("a"), Ok("a"));
    assert_eq!(ident("_0"), Ok("_0"));

    assert!(ident("0a").is_err());
    assert!(ident("ä").is_err());
    assert!(ident("7").is_err());
}

#[test]
fn strings() {
    assert_eq!(string("\"\""), Ok("".to_string()));
    assert_eq!(string("\" test_string \""), Ok(" test_string ".to_string()));
    assert_eq!(string("\"\\n\\r\\t\\\\\""), Ok("\n\r\t\\".to_string()));

    // TODO: unescaped "\n" is accepted; keep? change? remove? multiline str?
    assert_eq!(string("\"\n\""), Ok("\n".to_string()));

    // invalid escape seq
    assert!(string("\"\\q\"").is_err());
}

#[test]
fn literals() {
    use lexer::Literal::*;

    assert_eq!(literal("nil"), Ok(TNil));
    assert_eq!(literal("false"), Ok(TBool(false)));
    assert_eq!(literal("true"), Ok(TBool(true)));
    assert_eq!(literal("\"false\\n\""), Ok(TStr("false\n".to_string())));
    assert_eq!(literal("0"), Ok(TInt(0)));
    assert_eq!(literal("0.0"), Ok(TFloat(0.0)));
    assert_eq!(literal("7.5e+5"), Ok(TFloat(7.5e+5)));
}

#[test]
fn expr_simple() {
    use lexer::Literal::*;
    use ast::Expr::*;
    use ast::BinOp;

    // Test simple expressions

    assert_eq!(expression("4+2"), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::Add, Box::new(ELit(TInt(2))))));
    assert_eq!(expression("4 +2"), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::Add, Box::new(ELit(TInt(2))))));
    assert_eq!(expression("4 <<2"), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::ShiftL, Box::new(ELit(TInt(2))))));
    assert_eq!(expression("4% 2 "), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::Mod, Box::new(ELit(TInt(2))))));
    assert_eq!(expression(" 4^2 "), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::BXor, Box::new(ELit(TInt(2))))));
    assert_eq!(expression(" 4 + 2 "), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::Add, Box::new(ELit(TInt(2))))));
    assert_eq!(expression("4 - 2"), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::Sub, Box::new(ELit(TInt(2))))));
    assert_eq!(expression("4-2"), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::Sub, Box::new(ELit(TInt(2))))));
    assert_eq!(expression("4 -2"), Ok(EBinOp(Box::new(ELit(TInt(4))), BinOp::Sub, Box::new(ELit(TInt(2))))));
}

#[test]
fn expr_prec() {
    use lexer::Literal::*;
    use ast::Expr::*;
    use ast::BinOp;

    // Test operator precedences

    assert_eq!(expression("4+1*2"), Ok(
        EBinOp(Box::new(ELit(TInt(4))), BinOp::Add, Box::new(
            EBinOp(Box::new(ELit(TInt(1))), BinOp::Mul, Box::new(ELit(TInt(2))))
        ))
    ));
    assert_eq!(expression("4*1+2"), Ok(
        EBinOp(Box::new(
            EBinOp(Box::new(ELit(TInt(4))), BinOp::Mul, Box::new(ELit(TInt(1))))
        ), BinOp::Add, Box::new(
            ELit(TInt(2))
        ))
    ));

    // This requires reversing everything because of op. precedences
    assert_eq!(expression("9==8||7&6^5|4>>3+2*1"), Ok(
        EBinOp(Box::new(
            ELit(TInt(9))
        ), BinOp::Eq, Box::new(
            EBinOp(Box::new(
                ELit(TInt(8))
            ), BinOp::LOr, Box::new(
                EBinOp(Box::new(
                    ELit(TInt(7))
                ), BinOp::BAnd, Box::new(
                    EBinOp(Box::new(
                        ELit(TInt(6))
                    ), BinOp::BXor, Box::new(
                        EBinOp(Box::new(
                            ELit(TInt(5))
                        ), BinOp::BOr, Box::new(
                            EBinOp(Box::new(
                                ELit(TInt(4))
                            ), BinOp::ShiftR, Box::new(
                                EBinOp(Box::new(
                                    ELit(TInt(3))
                                ), BinOp::Add, Box::new(
                                    EBinOp(Box::new(
                                        ELit(TInt(2))
                                    ), BinOp::Mul, Box::new(
                                        ELit(TInt(1))
                                    ))
                                ))
                            ))
                        ))
                    ))
                ))
            ))
        ))
    ));
}

#[test]
fn expr_complex() {
    use lexer::Literal::*;
    use ast::Expr::*;
    use ast::{BinOp, UnOp};

    // Test more complex expressions involving parentheses, unary operators, different literals

    assert_eq!(expression("-(5)"), Ok(EUnOp(UnOp::Negate, Box::new(ELit(TInt(5))))));
    assert_eq!(expression("-(5+1)"), Ok(EUnOp(UnOp::Negate, Box::new(
        EBinOp(Box::new(ELit(TInt(5))), BinOp::Add, Box::new(ELit(TInt(1))))
    ))));
    assert_eq!(expression("(1+2)*3"), Ok(
        EBinOp(Box::new(
            EBinOp(Box::new(ELit(TInt(1))), BinOp::Add, Box::new(ELit(TInt(2))))
        ), BinOp::Mul,
            Box::new(ELit(TInt(3)))
        )
    ));
    assert_eq!(expression("--5"), Ok(EUnOp(UnOp::Negate, Box::new(ELit(TInt(-5))))));
    assert_eq!(expression("-!~(#5)"), Ok(
        EUnOp(UnOp::Negate, Box::new(
            EUnOp(UnOp::LNot, Box::new(
                EUnOp(UnOp::BNot, Box::new(
                    EUnOp(UnOp::Len, Box::new(ELit(TInt(5))))
                ))
            ))
        ))
    ));
}
