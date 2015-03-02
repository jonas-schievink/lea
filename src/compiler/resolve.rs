//! This module contains the logic used to resolve identifiers to their locals or globals.
//!
//! Internally, this module also performs the register allocation for all locals. This makes it
//! easy to reference upvalues (see `UpvalDesc` in program.rs) and makes the bytecode emitter do
//! less work.

use super::ast::*;
use super::visit::*;
use super::span::{Span, Spanned};
use program::UpvalDesc;

use std::mem;
use std::collections::HashMap;
use std::default::Default;


/// Data used by the resolver, associated to a function
struct FuncData {
    locals: Vec<String>,

    /// Upvalues referenced from within the function
    upvals: Vec<UpvalDesc>,
    upval_names: Vec<String>,
    /// Maps known upvalue names to their id
    upval_map: HashMap<String, usize>,

    /// Stack of lists of locals. Tracks all active scopes and thus all available locals.
    scopes: Vec<Vec<usize>>,
}

impl FuncData {
    fn new(locals: Vec<String>) -> FuncData {
        FuncData {
            locals: locals,
            upvals: vec![],
            upval_names: vec![],
            upval_map: Default::default(),
            scopes: vec![],
        }
    }

    /// Registers an upvalue and returns its id
    fn add_upval(&mut self, name: String, desc: UpvalDesc) -> usize {
        let id = self.upvals.len();
        self.upvals.push(desc);
        self.upval_names.push(name.clone());
        self.upval_map.insert(name, id);

        id
    }

    /// Finds a reachable local (no upvalues are considered) with the given name and returns its id
    fn get_local(&self, name: &String) -> Option<usize> {
        // scan backwards through scopes
        let mut level = self.scopes.len() - 1;
        loop {
            for id in &self.scopes[level] {
                if *name == self.locals[*id] {
                    return Some(*id);
                }
            }

            if level == 0 { break; }
            level -= 1;
        }

        None
    }
}

/// A resolver will resolve any `VNamed` references in a function
struct Resolver {
    /// Stack of active functions
    funcs: Vec<FuncData>,
}

impl Resolver {
    /// Finds an upvalue in a parent function
    fn find_upval(&mut self, name: &String, userlvl: usize) -> Option<usize> {
        if userlvl == 0 {
            return None;
        }

        // search parent functions for locals / upvalues that match
        if let Some(id) = self.funcs[userlvl - 1].get_local(name) {
            return Some(self.funcs[userlvl].add_upval(name.clone(), UpvalDesc::Local(id)));
        } else {
            if let Some(id) = self.find_upval(name, userlvl - 1) {
                return Some(self.funcs[userlvl].add_upval(name.clone(), UpvalDesc::Upval(id)));
            } else {
                return None;
            }
        }

        None
    }

    /// Searches for an upvalue with the given name
    fn get_upval(&mut self, name: &String) -> Option<usize> {
        let level = self.funcs.len() - 1;

        {
            // search known upvals first
            let data = &self.funcs[level];
            if let Some(id) = data.upval_map.get(name) {
                return Some(*id);
            }
        }

        self.find_upval(name, level)
    }

    /// Declares a new local with the given name in the currently active scope and function.
    fn add_local(&mut self, name: String) -> usize {
        let level = self.funcs.len() - 1;

        // this might be a redeclaration, in which case we ignore it (the emitter handles it)
        let func = &mut self.funcs[level];
        let scopelvl = func.scopes.len() - 1;

        {
            let scope = &func.scopes[scopelvl];
            for id in scope {
                let lname = &func.locals[*id];
                if *lname == name {
                    // already declared
                    return *id;
                }
            }
        }

        // create new local
        let scope = &mut func.scopes[scopelvl];
        let id = func.locals.len();
        func.locals.push(name);
        scope.push(id);

        id
    }

    /// Resolves a block and declares a list of locals inside of it
    fn resolve_block(&mut self, b: &mut Block, locals: Vec<String>) {
        let level = self.funcs.len() - 1;
        self.funcs[level].scopes.push(vec![]);
        for name in locals {
            self.add_local(name);
        }

        walk_block(b, self);

        let data = &mut self.funcs[level];
        let scope = data.scopes.pop().unwrap();
        for id in scope {
            let name = &data.locals[id];
            b.localmap.insert(name.clone(), id);
        }
    }

    /// Resolves the given named variable
    fn resolve_var(&mut self, name: &String, span: Span) -> _Variable {
        // first, try a local with that name
        if let Some(id) = self.funcs[self.funcs.len() - 1].get_local(&name) {
            VLocal(id)
        } else {
            // find an upvalue
            if let Some(id) = self.get_upval(&name) {
                VUpval(id)
            } else {
                // fall back to global access; resolve environment
                let envvar = Box::new(Spanned::new(span,
                    self.resolve_var(&"_ENV".to_string(), span)));

                VResGlobal(envvar, name.clone())
            }
        }
    }
}

impl Visitor for Resolver {
    fn visit_stmt(&mut self, s: &mut Stmt) {
        match s.value {
            SDecl(ref mut names, ref mut exprs) => {
                for expr in exprs {
                    walk_expr(expr, self);
                }

                for name in names {
                    self.add_local(name.clone());
                }
            },
            SLFunc(ref mut name, ref mut f) => {
                self.add_local(name.clone());
                self.visit_func(f);
            },
            SFor{ref var, ref mut body, ..} => {
                self.resolve_block(body, vec![var.clone()]);
            },
            SForIn{ref vars, ref mut body, ..} => {
                self.resolve_block(body, vars.clone());
            },
            _ => {
                walk_stmt(s, self);
            }
        }
    }

    fn visit_var(&mut self, v: &mut Variable) {
        {
            let ref mut var = v.value;

            if let VNamed(..) = *var {
                let newvar = if let VNamed(ref name) = *var {
                    self.resolve_var(name, v.span)
                } else { unreachable!(); };

                mem::replace(var, newvar);
                return;
            }
        }

        walk_var(v, self);
    }

    fn visit_block(&mut self, b: &mut Block) {
        self.resolve_block(b, vec![]);
    }

    fn visit_func(&mut self, f: &mut Function) {
        let mut data = FuncData::new(f.params.clone());
        if self.funcs.len() == 0 {
            // root function, add implicit _ENV upvalue
            data.add_upval("_ENV".to_string(), UpvalDesc::Upval(0));    // the UpvalDesc is ignored
        }

        self.funcs.push(data);
        self.visit_block(&mut f.value.body);

        let data = self.funcs.pop().unwrap();
        f.locals = data.locals;
        f.upvalues = data.upvals;
    }
}

/// Resolves all locals used in the given block. Recursively resolves all blocks found inside.
///
/// Allows blocks inside the given block to access locals declared within the parent block
/// (assuming they are declared before the block). Does not allow the given block to access outer
/// locals.
///
/// This also resolves any `VGlobal` to `VResGlobal` and resolves the function environments.
pub fn resolve_func(f: &mut Function) {
    Resolver {
        funcs: vec![],
    }.visit_func(f);
}

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parser::parse_main;
    use compiler::span::Spanned;
    use compiler::ast::*;
    use program::UpvalDesc;

    use std::default::Default;

    macro_rules! localmap {
        () => {{
            ::std::collections::HashMap::<String, usize>::new()
        }};
        ( $($key:ident: $e:expr),* ) => {{
            let mut m = ::std::collections::HashMap::<String, usize>::new();
            $( m.insert(stringify!($key).to_string(), $e); )*

            m
        }};
    }

    #[test]
    fn simple() {
        let mut f = parse_main(r#"
i = 0
local a
do
    local i
    local j = i
    i[j] = a
end
j = i
"#).unwrap();
        resolve_func(&mut f);

        assert_eq!(f.value.body, Block::with_locals(vec![
            Spanned::default(SAssign(
                vec![Spanned::default(VResGlobal(Box::new(Spanned::default(VUpval(0))), "i".to_string()))],
                vec![Spanned::default(ELit(TInt(0)))],
            )),
            Spanned::default(SDecl(vec!["a".to_string()], vec![])),
            Spanned::default(SDo(Block::with_locals(vec![
                Spanned::default(SDecl(vec!["i".to_string()], vec![])),
                Spanned::default(SDecl(vec!["j".to_string()], vec![
                    Spanned::default(EVar(Spanned::default(VLocal(1)))),
                ])),
                Spanned::default(SAssign(
                    vec![Spanned::default(VIndex(
                        Box::new(Spanned::default(VLocal(1))),
                        Box::new(Spanned::default(EVar(Spanned::default(VLocal(2)))))
                    ))],
                    vec![Spanned::default(EVar(Spanned::default(VLocal(0))))],
                )),
            ], Default::default(), localmap!{ i: 1, j: 2 }))),
            Spanned::default(SAssign(
                vec![Spanned::default(
                    VResGlobal(Box::new(Spanned::default(VUpval(0))), "j".to_string())
                )],
                vec![Spanned::default(EVar(Spanned::default(
                    VResGlobal(Box::new(Spanned::default(VUpval(0))), "i".to_string())
                )))],
            )),
        ], Default::default(), localmap!{ a: 0 }));
    }

    #[test]
    fn complex() {
        let mut f = parse_main(r#"
local a
local function f()
    f = nil     // upvalue
    local f = f // both

    local function g() f = a end    // chained upvalue + normal upvalue
end
"#).unwrap();
        resolve_func(&mut f);

        assert_eq!(f.value, _Function {
            params: vec![],
            varargs: true,
            locals: vec!["a".to_string(), "f".to_string()],
            upvalues: vec![UpvalDesc::Upval(0)],    // `_ENV`; the UpvalDesc is ignored
            body: Block::with_locals(vec![
                Spanned::default(SDecl(vec!["a".to_string()], vec![])),
                Spanned::default(SLFunc("f".to_string(), Spanned::default(_Function {
                    params: vec![],
                    varargs: false,
                    locals: vec!["f".to_string(), "g".to_string()],
                    upvalues: vec![UpvalDesc::Local(1), UpvalDesc::Local(0)],
                    body: Block::with_locals(vec![
                        Spanned::default(SAssign(vec![
                            Spanned::default(VUpval(0))
                        ], vec![
                            Spanned::default(ELit(TNil))
                        ])),
                        Spanned::default(SDecl(vec!["f".to_string()], vec![
                            Spanned::default(EVar(Spanned::default(VUpval(0))))
                        ])),
                        Spanned::default(SLFunc("g".to_string(), Spanned::default(_Function {
                            params: vec![],
                            varargs: false,
                            locals: vec![],
                            upvalues: vec![UpvalDesc::Local(0), UpvalDesc::Upval(1)],
                            body: Block::with_locals(vec![
                                Spanned::default(SAssign(vec![
                                    Spanned::default(VUpval(0))
                                ], vec![
                                    Spanned::default(EVar(Spanned::default(VUpval(1))))
                                ])),
                            ], Default::default(), localmap!{}),
                        }))),
                    ], Default::default(), localmap!{ f: 0, g: 1 }),
                }))),
            ], Default::default(), localmap!{ a: 0, f: 1 }),
        });
    }

    #[test]
    fn env_simple() {
        let mut f = parse_main("_ENV = 0").unwrap();
        resolve_func(&mut f);

        assert_eq!(f.value.body, Block::new(vec![
            Spanned::default(SAssign(vec![
                Spanned::default(VUpval(0))
            ], vec![
                Spanned::default(ELit(TInt(0)))
            ])),
        ], Default::default()));
    }

    #[test]
    fn env_complex() {
        let mut f = parse_main(r#"
_ENV = nil
local _ENV
local function f() local _ENV i = nil end
local function g() _ENV = nil end
local function h() local function h1() r = nil end end
"#).unwrap();
        resolve_func(&mut f);

        assert_eq!(f.value, _Function {
            params: vec![],
            varargs: true,
            locals: vec!["_ENV".to_string(), "f".to_string(), "g".to_string(), "h".to_string()],
            upvalues: vec![UpvalDesc::Upval(0)],
            body: Block::with_locals(vec![
                Spanned::default(SAssign(vec![
                    Spanned::default(VUpval(0))
                ], vec![
                    Spanned::default(ELit(TNil))
                ])),
                Spanned::default(SDecl(vec!["_ENV".to_string()], vec![])),
                Spanned::default(SLFunc("f".to_string(), Spanned::default(_Function {
                    params: vec![],
                    varargs: false,
                    locals: vec!["_ENV".to_string()],
                    upvalues: vec![],
                    body: Block::with_locals(vec![
                        Spanned::default(SDecl(vec!["_ENV".to_string()], vec![])),
                        Spanned::default(SAssign(vec![
                            Spanned::default(VResGlobal(
                                Box::new(Spanned::default(VLocal(0))), "i".to_string()
                            )),
                        ], vec![Spanned::default(ELit(TNil))])),
                    ], Default::default(), localmap!{ _ENV: 0 }),
                }))),
                Spanned::default(SLFunc("g".to_string(), Spanned::default(_Function {
                    params: vec![],
                    varargs: false,
                    locals: vec![],
                    upvalues: vec![UpvalDesc::Local(0)],
                    body: Block::with_locals(vec![
                        Spanned::default(SAssign(vec![
                            Spanned::default(VUpval(0))
                        ], vec![
                            Spanned::default(ELit(TNil))
                        ])),
                    ], Default::default(), localmap!{}),
                }))),
                Spanned::default(SLFunc("h".to_string(), Spanned::default(_Function {
                    params: vec![],
                    varargs: false,
                    locals: vec!["h1".to_string()],
                    upvalues: vec![UpvalDesc::Local(0)],
                    body: Block::with_locals(vec![
                        Spanned::default(SLFunc("h1".to_string(), Spanned::default(_Function {
                            params: vec![],
                            varargs: false,
                            locals: vec![],
                            upvalues: vec![UpvalDesc::Upval(0)],
                            body: Block::new(vec![
                                Spanned::default(SAssign(vec![
                                    Spanned::default(VResGlobal(
                                        Box::new(Spanned::default(VUpval(0))), "r".to_string()
                                    ))
                                ], vec![
                                    Spanned::default(ELit(TNil)),
                                ])),
                            ], Default::default()),
                        }))),
                    ], Default::default(), localmap!{ h1: 0 }),
                }))),
            ], Default::default(), localmap!{ _ENV: 0, f: 1, g: 2, h: 3 }),
        });
    }
}
