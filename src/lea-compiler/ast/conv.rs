//! Parse tree -> AST conversion

use super::*;
use parser::parsetree;
use parser::span::Spanned;

use ::lea_core::literal;

impl<'a> From<parsetree::Function<'a>> for Function<'a> {
    fn from(func: parsetree::Function<'a>) -> Self {
        Function {
            params: func.params,
            varargs: func.varargs,
            body: func.body.into(),
            locals: Default::default(),
            upvalues: Default::default(),
        }
    }
}

impl<'a> From<parsetree::Block<'a>> for Block<'a> {
    fn from(block: parsetree::Block<'a>) -> Self {
        Block {
            span: block.span,
            stmts: conv_vec(block.stmts),
            localmap: Default::default(),
        }
    }
}

impl<'a> From<parsetree::_Variable<'a>> for _Variable<'a> {
    fn from(var: parsetree::_Variable<'a>) -> Self {
        match var {
            parsetree::VNamed(name) => {
                VNamed(name)
            }
            parsetree::VIndex(var, index) => {
                VIndex(Box::new(spanned_into(*var)), match index {
                    parsetree::VarIndex::DotIndex(name) => {
                        Box::new(Spanned::new(name.span,
                            ELit(literal::TStr(name.value.to_owned()))))
                    }
                    parsetree::VarIndex::ExprIndex(expr) => {
                        Box::new(spanned_into(*expr))
                    }
                })
            }
        }
    }
}

impl<'a> From<parsetree::_Expr<'a>> for _Expr<'a> {
    fn from(expr: parsetree::_Expr<'a>) -> Self {
        match expr {
            parsetree::ELit(lit) => ELit(lit),
            parsetree::EBinOp(lhs, op, rhs) =>
                EBinOp(Box::new(spanned_into(*lhs)), op, Box::new(spanned_into(*rhs))),
            parsetree::EUnOp(op, expr) => EUnOp(op, Box::new(spanned_into(*expr))),
            parsetree::EBraced(expr) => expr.value.into(),

            parsetree::EVar(var) => EVar(spanned_into(var)),

            parsetree::ECall(call) => ECall(match call {
                parsetree::SimpleCall(callee, args) => {
                    SimpleCall(Box::new(spanned_into(*callee)), conv_args(args))
                }
                parsetree::MethodCall(obj, name, args) => {
                    MethodCall(Box::new(spanned_into(*obj)), name, conv_args(args))
                }
            }),

            parsetree::ETable(cons) => {
                ETable(conv_table(cons))
            }

            parsetree::EFunc(func) => EFunc(func.into()),
            parsetree::EArray(elems) => EArray(vec_into(elems)),
            parsetree::EVarArgs => EVarArgs,
        }
    }
}

/// Implemented for parse tree nodes. Converts them into `n` instances of `Target`.
trait Conv<'a> {
    type Target;

    fn conv<F>(self, push: F) where F: FnMut(Self::Target);
}

fn conv_table<'a>(cons: parsetree::TableCons<'a>) -> Vec<(Expr<'a>, Expr<'a>)> {
    let mut entries: Vec<(Expr<'a>, Expr<'a>)> = Vec::new();
    let mut i = 0;  // XXX 1?
    for entry in cons {
        entries.push(match entry {
            parsetree::TableEntry::Pair(key, value) => {
                (spanned_into(key), spanned_into(value))
            }
            parsetree::TableEntry::Elem(value) => {
                let idx = i;
                i += 1;
                (Spanned::new(value.span, ELit(literal::TInt(idx))), spanned_into(value))
            }
        });
    }

    entries
}

fn conv_vec<'a, T: Conv<'a>>(v: Vec<Spanned<T>>) -> Vec<Spanned<T::Target>> {
    let mut res = Vec::new();
    for item in v.into_iter() {
        let span = item.span;
        item.value.conv(|i| res.push(Spanned::new(span, i)));
    }

    res
}

fn conv_args<'a>(args: parsetree::CallArgs<'a>) -> Vec<Expr<'a>> {
    let mut v = Vec::new();
    args.conv(|e| v.push(e));

    v
}

fn vec_into<'a, U, T: Into<U>>(v: Vec<Spanned<T>>) -> Vec<Spanned<U>> {
    v.into_iter().map(|item| spanned_into(item)).collect()
}

// Needed because Spanned<T> cannot impl Into. Not even specialization can help there.
fn spanned_into<U, T: Into<U>>(item: Spanned<T>) -> Spanned<U> {
    Spanned::new(item.span, item.value.into())
}

impl<'a> Conv<'a> for parsetree::_Stmt<'a> {
    type Target = _Stmt<'a>;

    fn conv<F>(self, mut push: F) where F: FnMut(Self::Target) {
        match self {
            parsetree::SDecl(names, exprs) => {
                push(_Stmt::SDecl(names, vec_into(exprs)));
            }

            parsetree::SAssign(vars, exprs) => {
                push(_Stmt::SAssign(vec_into(vars), vec_into(exprs)));
            }

            parsetree::SDo(block) => {
                push(_Stmt::SDo(block.into()));
            }

            parsetree::SBreak => {
                push(_Stmt::SBreak);
            }

            parsetree::SReturn(exprs) => {
                push(_Stmt::SReturn(vec_into(exprs)));
            }

            parsetree::SCall(call) => {
                push(_Stmt::SCall(match call {
                    parsetree::SimpleCall(callee, args) => {
                        SimpleCall(Box::new(spanned_into(*callee)), conv_args(args))
                    }
                    parsetree::MethodCall(obj, name, args) => {
                        MethodCall(Box::new(spanned_into(*obj)), name, conv_args(args))
                    }
                }));
            }

            parsetree::SFunc(var, func) => {
                push(_Stmt::SAssign(
                    vec![spanned_into(var)],
                    vec![Spanned::new(func.body.span, EFunc(func.into()))]
                ));
            }

            parsetree::SMethod(var, name, func) => {
                // var:name(args...)   =>   var.name = function(self, args...)
                let mut func: Function<'a> = func.into();
                func.params.insert(0, Spanned::new(name.span, "self"));

                push(_Stmt::SAssign(
                    vec![Spanned::new(var.span, VIndex(
                        Box::new(spanned_into(var)),
                        Box::new(Spanned::new(name.span, ELit(literal::TStr(name.to_string()))))
                    ))],
                    vec![Spanned::new(func.body.span, EFunc(func))]
                ));
            }

            parsetree::SLFunc(local, func) => {
                push(_Stmt::SDecl(vec![local], vec![]));
                push(_Stmt::SAssign(
                    vec![Spanned::new(local.span, VNamed(local.value))],
                    vec![Spanned::new(func.body.span, EFunc(func.into()))]
                ));
            }

            parsetree::SIf { cond, body, elifs, el } => {
                // Build else block for this if statement
                // Start with "pure" else at the end
                let mut myelse: Option<Block> = el.map(|el| el.into());

                for elif in elifs {
                    let (cond, body) = elif.value;

                    myelse = Some(Block {
                        localmap: Default::default(),
                        span: elif.span,
                        stmts: vec![
                            Spanned::new(elif.span, SIf {
                                cond: spanned_into(cond),
                                body: body.into(),
                                el: myelse, // Old else block here
                            }),
                        ],
                    });
                }

                push(_Stmt::SIf {
                    cond: spanned_into(cond),
                    body: body.into(),
                    el: myelse,
                });
            }

            parsetree::SWhile { cond, body } => {
                push(_Stmt::SWhile {
                    cond: spanned_into(cond),
                    body: body.into(),
                });
            }

            parsetree::SRepeat { abort_on, body } => {
                push(_Stmt::SRepeat {
                    abort_on: spanned_into(abort_on),
                    body: body.into(),
                });
            }

            parsetree::SFor { var, start, step, end, body } => {
                push(_Stmt::SFor {
                    var: var,
                    start: spanned_into(start),
                    step: step.map(|e| spanned_into(e)),
                    end: spanned_into(end),
                    body: body.into(),
                });
            }

            parsetree::SForIn { vars, iter, body } => {
                push(_Stmt::SForIn {
                    vars: vars,
                    iter: vec_into(iter),
                    body: body.into(),
                });
            }
        }
    }
}

impl<'a> Conv<'a> for parsetree::CallArgs<'a> {
    type Target = Expr<'a>;

    fn conv<F>(self, mut push: F) where F: FnMut(Self::Target) {
        match self {
            parsetree::CallArgs::Normal(exprs) => {
                for expr in exprs {
                    push(spanned_into(expr))
                }
            }
            parsetree::CallArgs::String(s) => {
                // TODO use real span
                push(Spanned::default(ELit(literal::TStr(s))))
            }
            parsetree::CallArgs::Table(cons) => {
                push(Spanned::default(ETable(conv_table(cons))))
            }
        }
    }
}
