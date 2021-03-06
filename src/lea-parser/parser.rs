//! Wrapper around rust-peg generated parser methods.

use parsetree::*;
use span::*;

use term::Terminal;

use std::convert::From;
use std::io::{self, Write};


peg_file! parse("lea.peg.rs");

pub use self::parse::{ident, literal};


/// Custom error type adding a dummy span and providing a `format` method.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    err: parse::ParseError,
    span: Span,
}

impl ParseError {
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<Output=W>)
    -> io::Result<()> {
        try!(self.span.print_with_err(code, source_name, &format!("{}", self.err), t));

        Ok(())
    }
}

impl From<parse::ParseError> for ParseError {
    fn from(err: parse::ParseError) -> ParseError {
        ParseError {
            span: Span::new(err.offset, err.offset),
            err: err,
        }
    }
}

/// Parses an expression
pub fn expression(input: &str) -> Result<Expr, ParseError> {
    Ok(try!(parse::expression(input)))
}

/// Parses a statement
pub fn statement(input: &str) -> Result<Stmt, ParseError> {
    Ok(try!(parse::statement(input)))
}

/// Parses a block of statements
pub fn block(input: &str) -> Result<Block, ParseError> {
    Ok(try!(parse::block(input)))
}

/// Parses a block of statements and builds a function wrapper that can be run as the main function
/// around it.
pub fn parse_main(input: &str) -> Result<Function, ParseError> {
    let blk = try!(block(input));

    Ok(Function::new(vec![], true, blk))
}

/// Parses an expression and builds a `Function` node that will evalute and return the expression
/// when executed.
pub fn parse_expr_as_main(expr: &str) -> Result<Function, ParseError> {
    let expr = try!(expression(expr));
    let block = Block {
        span: expr.span,
        stmts: vec![
            Spanned::new(expr.span, StmtKind::Return(vec![expr])),
        ],
    };

    Ok(Function::new(vec![], true, block))
}

#[cfg(test)]
mod tests {
    use super::*;

    use parsetree::*;
    use span::Spanned;
    use op::*;

    use lea_core::Const;

    use std::default::Default;

    #[test]
    fn literals() {
        assert_eq!(literal("0").unwrap().value, Const::Number(0.into()));
        assert_eq!(literal("1").unwrap().value, Const::Number(1.into()));
        assert_eq!(literal("-1").unwrap().value, Const::Number((-1).into()));

        assert_eq!(literal("0x00").unwrap().value, Const::Number(0.into()));
        assert_eq!(literal("-0x00").unwrap().value, Const::Number(0.into()));
        assert_eq!(literal("0xff").unwrap().value, Const::Number(255.into()));
        assert_eq!(literal("-0xff").unwrap().value, Const::Number((-255).into()));
        assert_eq!(literal("0xffffffff").unwrap().value, Const::Number(0xffffffff.into()));

        assert_eq!(literal("0o76").unwrap().value, Const::Number(0o76.into()));
        assert_eq!(literal("-0o76").unwrap().value, Const::Number((-0o76).into()));

        assert_eq!(literal("0.0").unwrap().value, Const::Number(0.0.into()));
        assert_eq!(literal("-0.0").unwrap().value, Const::Number((-0.0).into()));
        assert_eq!(literal("10000.0").unwrap().value, Const::Number(10000.0.into()));

        assert_eq!(literal("1e4").unwrap().value, Const::Number(1e+4.into()));
        assert_eq!(literal("1e+4").unwrap().value, Const::Number(1e+4.into()));
        assert_eq!(literal("-1e+4").unwrap().value, Const::Number((-1e+4).into()));
        assert_eq!(literal("2e-4").unwrap().value, Const::Number(2e-4.into()));

        assert_eq!(literal("nil").unwrap().value, Const::Nil);
        assert_eq!(literal("false").unwrap().value, Const::Bool(false));
        assert_eq!(literal("true").unwrap().value, Const::Bool(true));

        assert_eq!(literal("\"\"").unwrap().value, Const::Str("".to_owned()));
        assert_eq!(literal("\" test_string \"").unwrap().value, Const::Str(" test_string ".to_owned()));
        assert_eq!(literal("\"hi\\n\\r\\t\\\\ \\\"\"").unwrap().value, Const::Str("hi\n\r\t\\ \"".to_owned()));
        assert_eq!(literal("\"\\127\\0\\1\\255\\xff\"").unwrap().value, Const::Str("\x7f\0\x01\u{ff}\u{ff}".to_owned()));

        assert_eq!(literal("''").unwrap().value, Const::Str("".to_owned()));
        assert_eq!(literal("'\\n\\r'").unwrap().value, Const::Str("\n\r".to_owned()));
        assert_eq!(literal("'\\z   \n\"'").unwrap().value, Const::Str("\"".to_owned()));

        assert!(literal("\"\n\"").is_err());
        assert!(literal("\"\\q\"").is_err());       // invalid escape seq

        assert_eq!(literal("[[test]]").unwrap().value, Const::Str("test".to_owned()));
        assert_eq!(literal("[=[test]=]").unwrap().value, Const::Str("test".to_owned()));
        assert_eq!(literal("[======[test]======]").unwrap().value, Const::Str("test".to_owned()));

        assert_eq!(literal("[=[test]]]=]").unwrap().value, Const::Str("test]]".to_owned()));
        assert_eq!(expression("[=[test]=] + [=[bla]=]").unwrap().value, ExprKind::BinOp(
            Box::new(Spanned::default(ExprKind::Lit(Const::Str("test".to_owned())))),
            BinOp::Add,
            Box::new(Spanned::default(ExprKind::Lit(Const::Str("bla".to_owned())))),
        ));
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
        assert_eq!(expression("1+2+3").unwrap().value, ExprKind::BinOp(
            Box::new(Spanned::default(ExprKind::BinOp(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(1.into())))),
                BinOp::Add,
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(2.into()))))
            ))), BinOp::Add,
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(3.into()))))
            )
        );
        assert_eq!(expression("4+1*2").unwrap().value, ExprKind::BinOp(
            Box::new(Spanned::default(ExprKind::Lit(Const::Number(4.into())))),
            BinOp::Add,
            Box::new(Spanned::default(ExprKind::BinOp(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(1.into())))),
                BinOp::Mul,
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(2.into()))))
            ))
        )));
        assert_eq!(expression("4*1+2").unwrap().value, ExprKind::BinOp(
            Box::new(Spanned::default(ExprKind::BinOp(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(4.into())))),
                BinOp::Mul,
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(1.into()))))
            ))),
            BinOp::Add,
            Box::new(Spanned::default(ExprKind::Lit(Const::Number(2.into()))))
        ));

        assert_eq!(expression("-(5)").unwrap().value, ExprKind::UnOp(
            UnOp::Negate,
            Box::new(Spanned::default(ExprKind::Braced(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(5.into()))))
            )))
        ));
        assert_eq!(expression("-(5+1)").unwrap().value, ExprKind::UnOp(
            UnOp::Negate,
            Box::new(Spanned::default(ExprKind::Braced(Box::new(Spanned::default(ExprKind::BinOp(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(5.into())))),
                BinOp::Add,
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(1.into()))))
            ))))))
        ));
        assert_eq!(expression("(1+2)*3").unwrap().value, ExprKind::BinOp(
            Box::new(Spanned::default(ExprKind::Braced(Box::new(Spanned::default(ExprKind::BinOp(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(1.into())))),
                BinOp::Add,
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(2.into()))))
            )))))),
            BinOp::Mul,
            Box::new(Spanned::default(ExprKind::Lit(Const::Number(3.into()))))
        ));
        assert_eq!(expression("-!~(#5)").unwrap().value, ExprKind::UnOp(
            UnOp::Negate, Box::new(Spanned::default(ExprKind::UnOp(
                UnOp::LNot, Box::new(Spanned::default(ExprKind::UnOp(
                    UnOp::BNot, Box::new(Spanned::default(ExprKind::Braced(Box::new(Spanned::default(ExprKind::UnOp(
                        UnOp::Len, Box::new(Spanned::default(ExprKind::Lit(Const::Number(5.into()))))
                    ))))))
                )))
            )))
        ));

        // right-associativity
        assert_eq!(expression("1^2^3+4").unwrap().value, ExprKind::BinOp(
            Box::new(Spanned::default(ExprKind::BinOp(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(1.into())))),
                BinOp::Pow,
                Box::new(Spanned::default(ExprKind::BinOp(
                    Box::new(Spanned::default(ExprKind::Lit(Const::Number(2.into())))),
                    BinOp::Pow,
                    Box::new(Spanned::default(ExprKind::Lit(Const::Number(3.into())))),
                )))
            ))),
            BinOp::Add,
            Box::new(Spanned::default(ExprKind::Lit(Const::Number(4.into())))),
        ));

        // This requires reversing everything because of op. precedences
        assert_eq!(expression("9||8==7&6~5|4>>3+2*1").unwrap().value, ExprKind::BinOp(
            Box::new(Spanned::default(ExprKind::Lit(Const::Number(9.into())))),
            BinOp::LOr,
            Box::new(Spanned::default(ExprKind::BinOp(
                Box::new(Spanned::default(ExprKind::Lit(Const::Number(8.into())))),
                BinOp::Eq,
                Box::new(Spanned::default(ExprKind::BinOp(
                    Box::new(Spanned::default(ExprKind::Lit(Const::Number(7.into())))),
                    BinOp::BAnd,
                    Box::new(Spanned::default(ExprKind::BinOp(
                        Box::new(Spanned::default(ExprKind::Lit(Const::Number(6.into())))),
                        BinOp::BXor,
                        Box::new(Spanned::default(ExprKind::BinOp(
                            Box::new(Spanned::default(ExprKind::Lit(Const::Number(5.into())))),
                            BinOp::BOr,
                            Box::new(Spanned::default(ExprKind::BinOp(
                                Box::new(Spanned::default(ExprKind::Lit(Const::Number(4.into())))),
                                BinOp::ShiftR,
                                Box::new(Spanned::default(ExprKind::BinOp(
                                    Box::new(Spanned::default(ExprKind::Lit(Const::Number(3.into())))),
                                    BinOp::Add,
                                    Box::new(Spanned::default(ExprKind::BinOp(
                                        Box::new(Spanned::default(ExprKind::Lit(Const::Number(2.into())))),
                                        BinOp::Mul,
                                        Box::new(Spanned::default(ExprKind::Lit(Const::Number(1.into()))))
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
            ExprKind::Var(Spanned::default(VarKind::Named("t"))));
        assert!(expression("t.i") != expression("t[\"i\"]"));

        assert_eq!(expression("t.i.j").unwrap().value, ExprKind::Var(Spanned::default(VarKind::Indexed(
            Box::new(Spanned::default(VarKind::Indexed(
                Box::new(Spanned::default(VarKind::Named("t"))),
                VarIndex::DotIndex(Spanned::default("i")),
            ))),
            VarIndex::DotIndex(Spanned::default("j"))
        ))));
    }

    #[test]
    fn expr_special() {
        assert_eq!(expression("[]").unwrap().value, ExprKind::Array(vec![]));
        assert_eq!(expression("[1]").unwrap().value, ExprKind::Array(vec![
            Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
        ]));
        assert_eq!(expression("[1,]").unwrap().value, ExprKind::Array(vec![
            Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
        ]));
        assert_eq!(expression("[1,2]").unwrap().value, ExprKind::Array(vec![
            Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
        ]));
        assert_eq!(expression("[1,2,]").unwrap().value, ExprKind::Array(vec![
            Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
        ]));

        assert_eq!(expression("{}").unwrap().value, ExprKind::Table(vec![]));
        assert_eq!(expression("[{k=[1,2,],}]").unwrap().value, ExprKind::Array(vec![
            Spanned::default(ExprKind::Table(vec![
                TableEntry::IdentPair(
                    Spanned::default("k"),
                    Spanned::default(ExprKind::Array(vec![
                        Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
                        Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
                    ]))
                ),
            ]))
        ]));
        assert_eq!(expression("{ [9] = 0, [9] }").unwrap().value, ExprKind::Table(vec![
            TableEntry::Pair(
                Spanned::default(ExprKind::Lit(Const::Number(9.into()))),
                Spanned::default(ExprKind::Lit(Const::Number(0.into()))),
            ),
            TableEntry::Elem(Spanned::default(ExprKind::Array(vec![
                Spanned::default(ExprKind::Lit(Const::Number(9.into())))
            ]))),
        ]));

        assert_eq!(expression("function()end").unwrap().value,
            ExprKind::Func(Function {
                params: vec![],
                varargs: false,
                body: Default::default(),
            })
        );
        assert_eq!(expression("function(i, j, ...) break end").unwrap().value,
            ExprKind::Func(Function {
                params: vec![Spanned::default("i"), Spanned::default("j")],
                varargs: true,
                body: Block {
                    stmts: vec![
                        Spanned::default(StmtKind::Break),
                    ],
                    span: Default::default()
                },
        }));

        assert!(expression("function(...)end").is_ok());
    }

    #[test]
    fn call() {
        assert!(expression("f(1)").is_ok());
        assert_eq!(statement("f(1, 2)"), statement("f ( 1 , 2 )"));
        assert_eq!(statement("f ( 1 , 2 )").unwrap().value, StmtKind::Call(Call::Normal(
            Box::new(Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("f"))))),
            CallArgs::Normal(vec![
                Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
                Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
            ]),
        )));
        assert_eq!(expression("f()").unwrap().value, ExprKind::Call(Call::Normal(
            Box::new(Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("f"))))),
            CallArgs::Normal(vec![]),
        )));
        assert_eq!(expression("f()()").unwrap().value, ExprKind::Call(Call::Normal(
            Box::new(Spanned::default(ExprKind::Call(Call::Normal(
                Box::new(Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("f"))))),
                CallArgs::Normal(vec![]),
            )))),
            CallArgs::Normal(vec![]),
        )));

        assert_eq!(expression("f ''").unwrap().value, ExprKind::Call(Call::Normal(
            Box::new(Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("f"))))),
            CallArgs::String("".to_owned()),
        )));

        assert_eq!(expression("(function()end)()").unwrap().value, ExprKind::Call(Call::Normal(
            Box::new(Spanned::default(ExprKind::Braced(Box::new(Spanned::default(ExprKind::Func(Function {
                params: vec![],
                varargs: false,
                body: Default::default(),
            })))))),
            CallArgs::Normal(vec![]),
        )));

        assert!(expression("f(1,2,)").is_err());
        assert!(expression("f(,1)").is_err());
    }

    #[test]
    fn stmt() {
        assert_eq!(statement("break").unwrap().value, StmtKind::Break);
        assert_eq!(statement("do end").unwrap().value, StmtKind::Do(Default::default()));
        assert_eq!(statement("do break end").unwrap().value, StmtKind::Do(
            Block { stmts: vec![Spanned::default(StmtKind::Break)], span: Default::default() }
        ));
        assert_eq!(statement("do do end break end").unwrap().value, StmtKind::Do(
            Block {
                stmts: vec![
                    Spanned::default(StmtKind::Do(Default::default())),
                    Spanned::default(StmtKind::Break),
                ],
                span: Default::default()
            }
        ));

        assert_eq!(statement("i, j = k, l").unwrap().value, StmtKind::Assign(vec![
            Spanned::default(VarKind::Named("i")), Spanned::default(VarKind::Named("j")),
        ], vec![
            Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("k")))),
            Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("l")))),
        ]));
        assert_eq!(statement("i, j = 1, 2, 3").unwrap().value, StmtKind::Assign(vec![
            Spanned::default(VarKind::Named("i")), Spanned::default(VarKind::Named("j")),
        ], vec![
            Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
            Spanned::default(ExprKind::Lit(Const::Number(3.into()))),
        ]));

        assert_eq!(statement("local\nfunction\nt()\nend").unwrap().value, StmtKind::LocalFunc(
            Spanned::default("t"),
            Function {
                params: vec![],
                varargs: false,
                body: Default::default(),
            }
        ));

        assert_eq!(statement("function g.f(i,j) end").unwrap().value, StmtKind::Func(
            Spanned::default(VarKind::Indexed(
                Box::new(Spanned::default(VarKind::Named("g"))),
                VarIndex::DotIndex(Spanned::default("f")),
            )),
            Function {
                params: vec![Spanned::default("i"), Spanned::default("j")],
                varargs: false,
                body: Default::default(),
            }
        ));

        assert_eq!(statement("function g.f:j(i,j) end").unwrap().value, StmtKind::Method(
            Spanned::default(VarKind::Indexed(
                Box::new(Spanned::default(VarKind::Named("g"))),
                VarIndex::DotIndex(Spanned::default("f")),
            )),
            Spanned::default("j"),
            Function {
                params: vec![Spanned::default("i"), Spanned::default("j")],
                varargs: false,
                body: Default::default(),
            }
        ));

        assert!(statement("function f(i,) end").is_err());
        assert!(statement("function f(,i) end").is_err());
        assert!(statement("function f(i,,j) end").is_err());

        assert_eq!(statement("local i").unwrap().value, StmtKind::Decl(vec![Spanned::default("i")], vec![]));
        assert_eq!(statement("local j,k").unwrap().value, StmtKind::Decl(vec![Spanned::default("j"), Spanned::default("k")], vec![]));
        assert_eq!(statement("local i = nil").unwrap().value, StmtKind::Decl(vec![Spanned::default("i")], vec![
            Spanned::default(ExprKind::Lit(Const::Nil))
        ]));
        assert_eq!(statement("local i,j = 0, 2").unwrap().value, StmtKind::Decl(vec![
            Spanned::default("i"), Spanned::default("j"),
        ], vec![
            Spanned::default(ExprKind::Lit(Const::Number(0.into()))),
            Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
        ]));

        assert_eq!(statement("for i in j do end").unwrap().value, StmtKind::ForIn {
            vars: vec![Spanned::default("i")],
            iter: vec![Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("j"))))],
            body: Default::default(),
        });
        assert_eq!(statement(" for  i,j, k , l in 1, 2,3 , 4 do break end").unwrap().value, StmtKind::ForIn {
            vars: vec![Spanned::default("i"), Spanned::default("j"), Spanned::default("k"), Spanned::default("l")],
            iter: vec![
                Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
                Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
                Spanned::default(ExprKind::Lit(Const::Number(3.into()))),
                Spanned::default(ExprKind::Lit(Const::Number(4.into()))),
            ],
            body: Block {
                stmts: vec![
                    Spanned::default(StmtKind::Break),
                ],
                span: Default::default()
            },
        });

        assert_eq!(statement("for i = 1, #t do do end break end").unwrap().value, StmtKind::For {
            var: Spanned::default("i"),
            start: Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            end: Spanned::default(ExprKind::UnOp(
                UnOp::Len,
                Box::new(Spanned::default(ExprKind::Var(Spanned::default(VarKind::Named("t")))))
            )),
            step: None,
            body: Block {
                stmts: vec![
                    Spanned::default(StmtKind::Do(Default::default())),
                    Spanned::default(StmtKind::Break),
                ],
                span: Default::default()
            },
        });
        assert_eq!(statement("for i = 1,2,3 do do end break end").unwrap().value, StmtKind::For {
            var: Spanned::default("i"),
            start: Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            end: Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
            step: Some(Spanned::default(ExprKind::Lit(Const::Number(3.into())))),
            body: Block {
                stmts: vec![
                    Spanned::default(StmtKind::Do(Default::default())),
                    Spanned::default(StmtKind::Break),
                ],
                span: Default::default()
            },
        });

        assert_eq!(statement("repeat break until 1").unwrap().value, StmtKind::Repeat {
            abort_on: Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            body: Block {
                stmts: vec![Spanned::default(StmtKind::Break)],
                span: Default::default(),
            }
        });

        assert_eq!(statement("while 1 do break end").unwrap().value, StmtKind::While {
            cond: Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            body: Block {
                stmts: vec![Spanned::default(StmtKind::Break)],
                span: Default::default(),
            }
        });
        assert_eq!(statement("while 1 do end"), statement(" while   1  do  end  "));
        assert_eq!(statement("while 1 do break end"), statement(" while \n1\n do break\t\n end "));

        assert_eq!(statement("if 5 then end").unwrap().value, StmtKind::If {
            cond: Spanned::default(ExprKind::Lit(Const::Number(5.into()))),
            body: Default::default(),
            elifs: Vec::new(),
            el: None,
        });
        assert_eq!(statement("if 5 then else end"), statement("if\t5\tthen\telse\tend"));

        assert_eq!(statement("if 1 then break elseif 2 then break break else break end").unwrap().value,
        StmtKind::If {
            cond: Spanned::default(ExprKind::Lit(Const::Number(1.into()))),
            body: Block {
                stmts: vec![Spanned::default(StmtKind::Break)],
                span: Default::default(),
            },
            elifs: vec![
                Spanned::default((Spanned::default(ExprKind::Lit(Const::Number(2.into()))), Block {
                    stmts: vec![Spanned::default(StmtKind::Break), Spanned::default(StmtKind::Break)],
                    span: Default::default(),
                })),
            ],
            el: Some(Block {
                stmts: vec![Spanned::default(StmtKind::Break)],
                span: Default::default(),
            }),
        });

        assert_eq!(statement("return").unwrap().value, StmtKind::Return(vec![]));
        assert_eq!(statement("return 1").unwrap().value, StmtKind::Return(vec![
            Spanned::default(ExprKind::Lit(Const::Number(1.into())))
        ]));
        assert_eq!(statement("return 1, 2"), statement("return \t\n1 \n,  \t2"));

        assert!(statement("return 1,").is_err());
    }

    #[test]
    fn comments() {
        assert_eq!(statement("break -- test\\\\aaa").unwrap().value, StmtKind::Break);
        assert_eq!(statement("break --").unwrap().value, StmtKind::Break);
        assert_eq!(statement("break ------asdsa\n").unwrap().value, StmtKind::Break);

        assert_eq!(statement("break --[[\ntest ]]").unwrap().value, StmtKind::Break);
        assert_eq!(statement("break --[[  \r\n\t  ]]").unwrap().value, StmtKind::Break);
        assert_eq!(statement("break --[===[\n--[===[]]]]-- ]===]").unwrap().value, StmtKind::Break);
    }

    #[test]
    fn parse_block() {
        assert_eq!(block(r#"
    -- Test program
    t = 1
    local r, s = 4, 2, 1
    function f(g, ...) do end end
    "#).unwrap(), Block {
            stmts: vec![
                Spanned::default(StmtKind::Assign(
                    vec![Spanned::default(VarKind::Named("t"))],
                    vec![Spanned::default(ExprKind::Lit(Const::Number(1.into())))]
                )),
                Spanned::default(StmtKind::Decl(vec![Spanned::default("r"), Spanned::default("s")], vec![
                    Spanned::default(ExprKind::Lit(Const::Number(4.into()))),
                    Spanned::default(ExprKind::Lit(Const::Number(2.into()))),
                    Spanned::default(ExprKind::Lit(Const::Number(1.into())))
                ])),
                Spanned::default(StmtKind::Func(Spanned::default(VarKind::Named("f")), Function {
                    params: vec![Spanned::default("g")],
                    varargs: true,
                    body: Block {
                        stmts: vec![Spanned::default(StmtKind::Do(Default::default()))],
                        span: Default::default(),
                    },
                })),
            ],
            span: Default::default()
        });
    }
}
