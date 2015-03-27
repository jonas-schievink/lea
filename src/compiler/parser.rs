//! Wrapper around rust-peg generated parser methods.

peg_file! parse("lea.rustpeg");

pub use self::parse::{ident, literal};

use super::ast::*;
use super::span::*;
use super::visit;
use super::visit::Visitor;
use super::expr_parser::ExprParser;

use term::Terminal;

use std::error::FromError;
use std::io::{self, Write};

/// Custom error type adding a dummy span and providing a `format` method.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    err: parse::ParseError,
    span: Span,
}

impl ParseError {
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<W>)
    -> io::Result<()> {
        try!(self.span.print_with_err(code, source_name, format!("{}", self.err).as_ref(), t));

        Ok(())
    }
}

impl FromError<parse::ParseError> for ParseError {
    fn from_error(err: parse::ParseError) -> ParseError {
        ParseError {
            span: Span::new(err.offset, err.offset),
            err: err,
        }
    }
}

/// Parses an expression
pub fn expression(input: &str) -> Result<Expr, ParseError> {
    let mut e = try!(parse::expression(input));
    e = ExprParser.visit_expr(e);

    Ok(e)
}

/// Parses a statement
pub fn statement(input: &str) -> Result<Stmt, ParseError> {
    let mut s = try!(parse::statement(input));
    s = ExprParser.visit_stmt(s);

    Ok(s)
}

/// Parses a block of statements
pub fn block(input: &str) -> Result<Block, ParseError> {
    let mut b = try!(parse::block(input));
    b = visit::walk_block(b, &mut ExprParser);

    Ok(b)
}

/// Parses a block of statements and builds a function wrapper that can be run as the main function
/// around it.
pub fn parse_main(input: &str) -> Result<Function, ParseError> {
    let blk = try!(block(input));

    Ok(Function::new(vec![], true, blk))
}

/// Parses a raw expression. This only runs the PEG parser, not the dedicated expression parser.
///
/// The returned expression will be of the variant `ERawOp`.
#[inline(always)]
pub fn expression_raw(input: &str) -> Result<Expr, ParseError> {
    Ok(try!(parse::expression(input)))
}

#[cfg(test)]
mod tests {
    use super::*;

    use compiler::ast::*;
    use compiler::span::Spanned;

    use op::*;

    use std::default::Default;

    #[test]
    fn literals() {
        assert_eq!(literal("0").unwrap().value, TInt(0));
        assert_eq!(literal("1").unwrap().value, TInt(1));
        assert_eq!(literal("-1").unwrap().value, TInt(-1));
        assert_eq!(literal("9999999999").unwrap().value, TInt(9999999999i64));
        assert_eq!(literal("-9999999999").unwrap().value, TInt(-9999999999i64));

        assert_eq!(literal("0x00").unwrap().value, TInt(0));
        assert_eq!(literal("-0x00").unwrap().value, TInt(0));
        assert_eq!(literal("0xff").unwrap().value, TInt(255));
        assert_eq!(literal("-0xff").unwrap().value, TInt(-255));
        assert_eq!(literal("0xffffffffff").unwrap().value, TInt(0xffffffffff));

        assert_eq!(literal("0o76").unwrap().value, TInt(0o76));
        assert_eq!(literal("-0o76").unwrap().value, TInt(-0o76));

        assert_eq!(literal("0.0").unwrap().value, TFloat(0.0));
        assert_eq!(literal("-0.0").unwrap().value, TFloat(-0.0));
        assert_eq!(literal("10000.0").unwrap().value, TFloat(10000.0));

        assert_eq!(literal("1e4").unwrap().value, TFloat(1e+4));
        assert_eq!(literal("1e+4").unwrap().value, TFloat(1e+4));
        assert_eq!(literal("-1e+4").unwrap().value, TFloat(-1e+4));
        assert_eq!(literal("2e-4").unwrap().value, TFloat(2e-4));

        assert_eq!(literal("nil").unwrap().value, TNil);
        assert_eq!(literal("false").unwrap().value, TBool(false));
        assert_eq!(literal("true").unwrap().value, TBool(true));

        assert_eq!(literal("\"\"").unwrap().value, TStr("".to_string()));
        assert_eq!(literal("\" test_string \"").unwrap().value, TStr(" test_string ".to_string()));
        assert_eq!(literal("\"hi\\n\\r\\t\\\\ \\\"\"").unwrap().value, TStr("hi\n\r\t\\ \"".to_string()));
        assert_eq!(literal("\"\\127\\0\\1\\255\\xff\"").unwrap().value, TStr("\x7f\0\x01\u{ff}\u{ff}".to_string()));

        assert_eq!(literal("''").unwrap().value, TStr("".to_string()));
        assert_eq!(literal("'\\n\\r'").unwrap().value, TStr("\n\r".to_string()));
        assert_eq!(literal("'\\z   \n'").unwrap().value, TStr("".to_string()));

        assert!(literal("\"\n\"").is_err());

        // invalid escape seq
        assert!(literal("\"\\q\"").is_err());

        // TODO implement long strings
        /*
        assert_eq!(literal("[[test]]").unwrap().value, TStr("test".to_string()));
        assert_eq!(literal("[=[test]=]").unwrap().value, TStr("test".to_string()));
        assert_eq!(literal("[======[test]======]").unwrap().value, TStr("test".to_string()));

        assert_eq!(literal("[=[test]]]=]").unwrap().value, TStr("test]]".to_string()));
        assert_eq!(expression("[=[test]=] + [=[bla]=]").unwrap().value, EBinOp(
            Box::new(Spanned::default(ELit(TStr("test".to_string())))),
            BinOp::Add,
            Box::new(Spanned::default(ELit(TStr("bla".to_string())))),
        ));
        */
    }

    #[test]
    fn idents() {
        assert_eq!(ident("test").unwrap().value, "test");
        assert_eq!(ident("_a0_0_").unwrap().value, "_a0_0_");
        assert_eq!(ident("_").unwrap().value, "_");
        assert_eq!(ident("a").unwrap().value, "a");
        assert_eq!(ident("_0").unwrap().value, "_0");

        assert!(ident("0a").is_err());
        assert!(ident("ä").is_err());
        assert!(ident("7").is_err());
    }

    #[test]
    fn expr() {
        assert_eq!(expression("1+2+3").unwrap().value, EBinOp(
            Box::new(Spanned::default(EBinOp(
                Box::new(Spanned::default(ELit(TInt(1)))),
                BinOp::Add,
                Box::new(Spanned::default(ELit(TInt(2))))
            ))), BinOp::Add,
                Box::new(Spanned::default(ELit(TInt(3))))
            )
        );
        assert_eq!(expression("4+1*2").unwrap().value, EBinOp(
            Box::new(Spanned::default(ELit(TInt(4)))),
            BinOp::Add,
            Box::new(Spanned::default(EBinOp(
                Box::new(Spanned::default(ELit(TInt(1)))),
                BinOp::Mul,
                Box::new(Spanned::default(ELit(TInt(2))))
            ))
        )));
        assert_eq!(expression("4*1+2").unwrap().value, EBinOp(
            Box::new(Spanned::default(EBinOp(
                Box::new(Spanned::default(ELit(TInt(4)))),
                BinOp::Mul,
                Box::new(Spanned::default(ELit(TInt(1))))
            ))),
            BinOp::Add,
            Box::new(Spanned::default(ELit(TInt(2))))
        ));

        assert_eq!(expression("-(5)").unwrap().value, EUnOp(
            UnOp::Negate,
            Box::new(Spanned::default(ELit(TInt(5))))
        ));
        assert_eq!(expression("-(5+1)").unwrap().value, EUnOp(
            UnOp::Negate,
            Box::new(Spanned::default(EBinOp(
                Box::new(Spanned::default(ELit(TInt(5)))),
                BinOp::Add,
                Box::new(Spanned::default(ELit(TInt(1))))
            )))
        ));
        assert_eq!(expression("(1+2)*3").unwrap().value, EBinOp(
            Box::new(Spanned::default(EBinOp(
                Box::new(Spanned::default(ELit(TInt(1)))),
                BinOp::Add,
                Box::new(Spanned::default(ELit(TInt(2))))
            ))),
            BinOp::Mul,
            Box::new(Spanned::default(ELit(TInt(3))))
        ));
        assert_eq!(expression("-!~(#5)").unwrap().value, EUnOp(
            UnOp::Negate, Box::new(Spanned::default(EUnOp(
                UnOp::LNot, Box::new(Spanned::default(EUnOp(
                    UnOp::BNot, Box::new(Spanned::default(EUnOp(
                        UnOp::Len, Box::new(Spanned::default(ELit(TInt(5))))
                    )))
                )))
            )))
        ));

        // right-associativity
        assert_eq!(expression("1^2^3+4").unwrap().value, EBinOp(
            Box::new(Spanned::default(EBinOp(
                Box::new(Spanned::default(ELit(TInt(1)))),
                BinOp::Pow,
                Box::new(Spanned::default(EBinOp(
                    Box::new(Spanned::default(ELit(TInt(2)))),
                    BinOp::Pow,
                    Box::new(Spanned::default(ELit(TInt(3)))),
                )))
            ))),
            BinOp::Add,
            Box::new(Spanned::default(ELit(TInt(4)))),
        ));

        // This requires reversing everything because of op. precedences
        assert_eq!(expression("9||8==7&6~5|4>>3+2*1").unwrap().value, EBinOp(
            Box::new(Spanned::default(ELit(TInt(9)))),
            BinOp::LOr,
            Box::new(Spanned::default(EBinOp(
                Box::new(Spanned::default(ELit(TInt(8)))),
                BinOp::Eq,
                Box::new(Spanned::default(EBinOp(
                    Box::new(Spanned::default(ELit(TInt(7)))),
                    BinOp::BAnd,
                    Box::new(Spanned::default(EBinOp(
                        Box::new(Spanned::default(ELit(TInt(6)))),
                        BinOp::BXor,
                        Box::new(Spanned::default(EBinOp(
                            Box::new(Spanned::default(ELit(TInt(5)))),
                            BinOp::BOr,
                            Box::new(Spanned::default(EBinOp(
                                Box::new(Spanned::default(ELit(TInt(4)))),
                                BinOp::ShiftR,
                                Box::new(Spanned::default(EBinOp(
                                    Box::new(Spanned::default(ELit(TInt(3)))),
                                    BinOp::Add,
                                    Box::new(Spanned::default(EBinOp(
                                        Box::new(Spanned::default(ELit(TInt(2)))),
                                        BinOp::Mul,
                                        Box::new(Spanned::default(ELit(TInt(1))))
                                    )))
                                )))
                            )))
                        )))
                    )))
                )))
            )))
        ));
    }

    #[test]
    fn expr_idx() {
        assert_eq!(expression("t").unwrap().value,
            EVar(Spanned::default(VNamed("t".to_string()))));
        assert!(expression("t.i") != expression("t[\"i\"]"));

        assert_eq!(expression("t.i.j").unwrap().value, EVar(Spanned::default(VDotIndex(
            Box::new(Spanned::default(VDotIndex(
                Box::new(Spanned::default(VNamed("t".to_string()))),
                "i".to_string(),
            ))),
            "j".to_string(),
        ))));
    }

    #[test]
    fn expr_special() {
        assert_eq!(expression("[]").unwrap().value, EArray(vec![]));
        assert_eq!(expression("[1]").unwrap().value, EArray(vec![
            Spanned::default(ELit(TInt(1))),
        ]));
        assert_eq!(expression("[1,]").unwrap().value, EArray(vec![
            Spanned::default(ELit(TInt(1))),
        ]));
        assert_eq!(expression("[1,2]").unwrap().value, EArray(vec![
            Spanned::default(ELit(TInt(1))),
            Spanned::default(ELit(TInt(2))),
        ]));
        assert_eq!(expression("[1,2,]").unwrap().value, EArray(vec![
            Spanned::default(ELit(TInt(1))),
            Spanned::default(ELit(TInt(2))),
        ]));

        assert_eq!(expression("{}").unwrap().value, ETable(vec![]));
        assert_eq!(expression("[{k=[1,2,],}]").unwrap().value, EArray(vec![
            Spanned::default(ETable(vec![
                TableEntry::Pair(
                    Spanned::default(ELit(TStr("k".to_string()))),
                    Spanned::default(EArray(vec![
                        Spanned::default(ELit(TInt(1))),
                        Spanned::default(ELit(TInt(2))),
                    ]))
                ),
            ]))
        ]));
        assert_eq!(expression("{ [9] = 0, [9] }").unwrap().value, ETable(vec![
            TableEntry::Pair(
                Spanned::default(ELit(TInt(9))),
                Spanned::default(ELit(TInt(0))),
            ),
            TableEntry::Elem(Spanned::default(EArray(vec![
                Spanned::default(ELit(TInt(9)))
            ]))),
        ]));

        assert_eq!(expression("function()end").unwrap().value,
            EFunc(Function {
                params: vec![],
                locals: vec![],
                upvalues: vec![],
                varargs: false,
                body: Block::new(vec![], Default::default()),
        }));
        assert_eq!(expression("function(i, j, ...) break end").unwrap().value,
            EFunc(Function {
                params: vec!["i".to_string(), "j".to_string()],
                locals: vec![],
                upvalues: vec![],
                varargs: true,
                body: Block::new(vec![
                    Spanned::default(SBreak),
                ], Default::default()),
        }));

        assert!(expression("function(...)end").is_ok());
    }

    #[test]
    fn call() {
        assert!(expression("f(1)").is_ok());
        assert_eq!(statement("f(1, 2)"), statement("f ( 1 , 2 )"));
        assert_eq!(statement("f ( 1 , 2 )").unwrap().value, SCall(SimpleCall(
            Box::new(Spanned::default(EVar(Spanned::default(VNamed("f".to_string()))))),
            CallArgs::Normal(vec![
                Spanned::default(ELit(TInt(1))),
                Spanned::default(ELit(TInt(2))),
            ]),
        )));
        assert_eq!(expression("f()").unwrap().value, ECall(SimpleCall(
            Box::new(Spanned::default(EVar(Spanned::default(VNamed("f".to_string()))))),
            CallArgs::Normal(vec![]),
        )));
        assert_eq!(expression("f()()").unwrap().value, ECall(SimpleCall(
            Box::new(Spanned::default(ECall(SimpleCall(
                Box::new(Spanned::default(EVar(Spanned::default(VNamed("f".to_string()))))),
                CallArgs::Normal(vec![]),
            )))),
            CallArgs::Normal(vec![]),
        )));

        assert_eq!(expression("f ''").unwrap().value, ECall(SimpleCall(
            Box::new(Spanned::default(EVar(Spanned::default(VNamed("f".to_string()))))),
            CallArgs::String("".to_string()),
        )));

        assert!(expression("f(1,2,)").is_err());
        assert!(expression("f(,1)").is_err());
    }

    #[test]
    fn stmt() {
        assert_eq!(statement("break").unwrap().value, SBreak);
        assert_eq!(statement("do end").unwrap().value, SDo(Block::new(
            vec![], Default::default()
        )));
        assert_eq!(statement("do break end").unwrap().value, SDo(
            Block::new(vec![Spanned::default(SBreak)], Default::default())
        ));
        assert_eq!(statement("do do end break end").unwrap().value, SDo(
            Block::new(vec![
                Spanned::default(SDo(Block::new(vec![], Default::default()))),
                Spanned::default(SBreak),
            ], Default::default())
        ));

        assert_eq!(statement("i, j = k, l").unwrap().value, SAssign(vec![
            Spanned::default(VNamed("i".to_string())), Spanned::default(VNamed("j".to_string())),
        ], vec![
            Spanned::default(EVar(Spanned::default(VNamed("k".to_string())))),
            Spanned::default(EVar(Spanned::default(VNamed("l".to_string())))),
        ]));
        assert_eq!(statement("i, j = 1, 2, 3").unwrap().value, SAssign(vec![
            Spanned::default(VNamed("i".to_string())), Spanned::default(VNamed("j".to_string())),
        ], vec![
            Spanned::default(ELit(TInt(1))),
            Spanned::default(ELit(TInt(2))),
            Spanned::default(ELit(TInt(3))),
        ]));

        assert_eq!(statement("local\nfunction\nt()\nend").unwrap().value, SLFunc("t".to_string(),
            Function {
                params: vec![],
                locals: vec![],
                upvalues: vec![],
                varargs: false,
                body: Block::new(vec![], Default::default()),
            }
        ));

        assert_eq!(statement("function g.f(i,j) end").unwrap().value, SFunc(
            Spanned::default(VDotIndex(Box::new(Spanned::default(VNamed("g".to_string()))), "f".to_string())),
            Function {
                params: vec!["i".to_string(), "j".to_string()],
                locals: vec![],
                upvalues: vec![],
                varargs: false,
                body: Block::new(vec![], Default::default()),
            }
        ));

        assert_eq!(statement("function g.f:j(i,j) end").unwrap().value, SMethod(
            Spanned::default(VDotIndex(Box::new(Spanned::default(VNamed("g".to_string()))), "f".to_string())),
            "j".to_string(),
            Function {
                params: vec!["i".to_string(), "j".to_string()],
                locals: vec![],
                upvalues: vec![],
                varargs: false,
                body: Block::new(vec![], Default::default()),
            }
        ));

        assert!(statement("function f(i,) end").is_err());
        assert!(statement("function f(,i) end").is_err());
        assert!(statement("function f(i,,j) end").is_err());

        assert_eq!(statement("local i").unwrap().value, SDecl(vec!["i".to_string()], vec![]));
        assert_eq!(statement("local j,k").unwrap().value, SDecl(vec!["j".to_string(), "k".to_string()], vec![]));
        assert_eq!(statement("local i = nil").unwrap().value, SDecl(vec!["i".to_string()], vec![
            Spanned::default(ELit(TNil))
        ]));
        assert_eq!(statement("local i,j = 0, 2").unwrap().value, SDecl(vec![
            "i".to_string(), "j".to_string(),
        ], vec![
            Spanned::default(ELit(TInt(0))), Spanned::default(ELit(TInt(2))),
        ]));

        assert_eq!(statement("for i in j do end").unwrap().value, SForIn {
            vars: vec!["i".to_string()],
            iter: vec![Spanned::default(EVar(Spanned::default(VNamed("j".to_string()))))],
            body: Block::new(vec![], Default::default()),
        });
        assert_eq!(statement(" for  i,j, k , l in 1, 2,3 , 4 do break end").unwrap().value, SForIn {
            vars: vec!["i".to_string(), "j".to_string(), "k".to_string(), "l".to_string()],
            iter: vec![
                Spanned::default(ELit(TInt(1))),
                Spanned::default(ELit(TInt(2))),
                Spanned::default(ELit(TInt(3))),
                Spanned::default(ELit(TInt(4))),
            ],
            body: Block::new(vec![
                Spanned::default(SBreak),
            ], Default::default()),
        });

        assert_eq!(statement("for i = 1, #t do do end break end").unwrap().value, SFor {
            var: "i".to_string(),
            start: Spanned::default(ELit(TInt(1))),
            end: Spanned::default(EUnOp(
                UnOp::Len,
                Box::new(Spanned::default(EVar(Spanned::default(VNamed("t".to_string())))))
            )),
            step: Spanned::default(ELit(TInt(1))),
            body: Block::new(vec![
                Spanned::default(SDo(Block::new(vec![], Default::default()))),
                Spanned::default(SBreak),
            ], Default::default()),
        });
        assert_eq!(statement("for \n\t_\n=\n2,3,4\ndo\nend"), statement("for _=2,3,4 do end"));

        assert_eq!(statement("repeat break until 1").unwrap().value, SRepeat {
            abort_on: Spanned::default(ELit(TInt(1))),
            body: Block::new(vec![Spanned::default(SBreak)], Default::default()),
        });

        assert_eq!(statement("while 1 do break end").unwrap().value, SWhile {
            cond: Spanned::default(ELit(TInt(1))),
            body: Block::new(vec![Spanned::default(SBreak)], Default::default()),
        });
        assert_eq!(statement("while 1 do end"), statement(" while   1  do  end  "));
        assert_eq!(statement("while 1 do break end"), statement(" while \n1\n do break\t\n end "));

        assert_eq!(statement("if 5 then end").unwrap().value, SIf {
            cond: Spanned::default(ELit(TInt(5))),
            body: Block::new(vec![], Default::default()),
            el: Block::new(vec![], Default::default()),
        });
        assert_eq!(statement("if 5 then end"), statement("if 5 then else end"));
        assert_eq!(statement("if 5 then end"), statement("if 5 then\nelse\nend"));
        assert_eq!(statement("if 5 then end"), statement("if  5  then  else  end"));
        assert_eq!(statement("if 5 then end"), statement("if\t5\tthen\telse\tend"));
        assert_eq!(statement("if 5 then end"), statement("if 5 then else\n end"));

        assert_eq!(statement("if 1 then break elseif 2 then break break else break end").unwrap().value,
        SIf {
            cond: Spanned::default(ELit(TInt(1))),
            body: Block::new(vec![Spanned::default(SBreak)], Default::default()),
            el: Block::new(vec![Spanned::default(SIf {
                cond: Spanned::default(ELit(TInt(2))),
                body: Block::new(vec![
                    Spanned::default(SBreak), Spanned::default(SBreak),
                ], Default::default()),
                el: Block::new(vec![Spanned::default(SBreak)], Default::default()),
            })], Default::default()),
        });

        assert_eq!(statement("return").unwrap().value, SReturn(vec![]));
        assert_eq!(statement("return 1").unwrap().value, SReturn(vec![Spanned::default(ELit(TInt(1)))]));
        assert_eq!(statement("return 1, 2"), statement("return \t\n1 \n,  \t2"));

        assert!(statement("return 1,").is_err());
    }

    #[test]
    fn comments() {
        assert_eq!(statement("break //").unwrap().value, SBreak);
        assert_eq!(statement("break // ").unwrap().value, SBreak);
        assert_eq!(statement("break //////").unwrap().value, SBreak);
        assert_eq!(statement("break // test\\\\aaa").unwrap().value, SBreak);
        assert_eq!(statement("break --").unwrap().value, SBreak);
        assert_eq!(statement("break ------asdsa\n").unwrap().value, SBreak);
        assert_eq!(statement("break /* test */").unwrap().value, SBreak);
        assert_eq!(statement("break /**///").unwrap().value, SBreak);
        assert_eq!(statement("do break /*aeurebv// */break end").unwrap().value, SDo(
            Block::new(vec![
                Spanned::default(SBreak), Spanned::default(SBreak),
            ], Default::default())
        ));
    }

    #[test]
    fn parse_block() {
        assert_eq!(block(r#"
    // Test program
    t = 1
    local r, s = 4, 2, 1
    function f(g, ...) do end end
    "#).unwrap(), Block::new(vec![
            Spanned::default(SAssign(
                vec![Spanned::default(VNamed("t".to_string()))],
                vec![Spanned::default(ELit(TInt(1)))]
            )),
            Spanned::default(SDecl(vec!["r".to_string(), "s".to_string()], vec![
                Spanned::default(ELit(TInt(4))),
                Spanned::default(ELit(TInt(2))),
                Spanned::default(ELit(TInt(1)))
            ])),
            Spanned::default(SFunc(Spanned::default(VNamed("f".to_string())), Function {
                params: vec!["g".to_string()],
                locals: vec![],
                upvalues: vec![],
                varargs: true,
                body: Block::new(vec![
                    Spanned::default(SDo(Block::new(vec![], Default::default())))
                ], Default::default()),
            })),
        ], Default::default()));
    }
}
