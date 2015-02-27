//! This module contains the logic used to resolve identifiers to their locals or globals.
//!
//! Internally, this module also performs the register allocation for all locals. This makes it
//! easy to reference upvalues (see `UpvalDesc` in program.rs) and makes the bytecode emitter do
//! less work.


use ast::*;
use visit::*;
use program::UpvalDesc;

use std::mem;
use std::collections::HashMap;
use std::default::Default;


struct Scope {
    start_slot: usize,
    locals: Vec<String>,
}

impl Scope {
    fn new(start_slot: usize) -> Scope {
        Scope {
            start_slot: start_slot,
            locals: vec![],
        }
    }
}

/// Data used by the resolver, associated to a function
struct FuncData {
    /// Upvalues referenced from within the function
    upvals: Vec<UpvalDesc>,
    upval_names: Vec<String>,

    /// Maps known upvalue names to their id
    upval_map: HashMap<String, usize>,

    /// Stack of lists of locals. Tracks all active scopes and thus all available locals.
    scopes: Vec<Scope>,

    /// Number of stack slots needed to execute this function
    stacksz: usize,
}

impl FuncData {
    fn new() -> FuncData {
        FuncData {
            upvals: vec![],
            upval_names: vec![],
            upval_map: Default::default(),
            scopes: vec![],
            stacksz: 0,
        }
    }

    /// Registers an upvalue
    fn add_upval(&mut self, name: String, desc: UpvalDesc) {
        let id = upvals.len();
        upvals.push(desc);
        upval_names.push(name);
        upval_map.insert(name, id);
    }

    /// Finds a reachable local (no upvalues are considered) with the given name and returns its
    /// stack slot
    fn get_local(&self, name: String) -> Option<usize> {
        // scan backwards through scopes
        let mut level = self.scopes.len() - 1;
        while level >= 0 {
            let scope = &self.scopes[level];
            for id in range(0, scope.locals.len()) {
                if name == scope.locals[id] {
                    return Some(id);
                }
            }
        }

        None
    }
}

/// A resolver will resolve any `VNamed` references in a function
struct Resolver<'a> {
    /// Stack of active functions
    funcs: Vec<FuncData>,
}

impl <'a> Resolver<'a> {
    /// Resolves the given function, assuming it doesn't have a parent function.
    pub fn resolve(f: &mut Function) {
        Resolver {
            funcs: vec![],
        }.visit_func(f);
    }

    /// Finds an upvalue in a parent function
    fn find_upval(&mut self, name: String, ref_level: usize, level: usize) -> Option<usize> {
        // search parent functions for locals / upvalues that match
        let mut level = self.funcs.len() - 2;   // start at parent
        while level >= 0 {
            let pdata = &self.funcs[level];
            if let Some(slot) = pdata.get_local(name) {
                // add upval to the using function
            }

            level -= 1;
        }

        None
    }

    /// Searches for an upvalue with the given name
    fn get_upval(&mut self, name: String) -> Option<usize> {
        // search known upvals first
        let data = &self.funcs[self.funcs.len() - 1];

        if let Some(id) = data.upval_map.get(name) {
            return Some(id);
        }

        self.find_upval(name)
    }
}

impl <'a> Visitor for Resolver<'a> {
    fn visit_stmt(&mut self, s: &mut Stmt) {
        match s.value {
            SDecl(ref mut names, ref mut exprs) => {
                for name in names {
                    self.add_local(name.clone());
                }

                for expr in exprs {
                    walk_expr(expr, self);
                }
            },
            SFor{ref var, ref mut body, ..} => {
                resolve_with_locals(self, body, vec![var.clone()]);
            },
            SForIn{ref vars, ref mut body, ..} => {
                resolve_with_locals(self, body, vars.clone());
            },
            _ => {
                walk_stmt(s, self);
            }
        }
    }

    fn visit_var(&mut self, v: &mut Variable) {
        self.resolve_var(v);
    }

    fn visit_block(&mut self, b: &mut Block) {
        let mut res = Resolver {
            reachable: HashMap::new(),
            owned: HashMap::new(),
            pfunc: self.pfunc,
        };

        resolve_block_with(b, res);
    }

    fn visit_func(&mut self, f: &mut Function) {
        // TODO
        self.visit_block(&mut f.value.body);
    }
}

/// Resolves all locals used in the given block. Recursively resolves all blocks found inside.
///
/// Allows blocks inside the given block to access locals declared within the parent block
/// (assuming they are declared before the block). Does not allow the given block to access outer
/// locals.
pub fn resolve_func(f: &mut Function) {
    FuncResolver::resolve(f);
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::parse_main;
    use span::Spanned;
    use ast::*;

    use std::default::Default;

    #[test]
    fn test() {
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
                vec![Spanned::default(VGlobal("i".to_string()))],
                vec![Spanned::default(ELit(TInt(0)))],
            )),
            Spanned::default(SDecl(vec!["a".to_string()], vec![])),
            Spanned::default(SDo(Block::with_locals(vec![
                Spanned::default(SDecl(vec!["i".to_string()], vec![])),
                Spanned::default(SDecl(vec!["j".to_string()], vec![
                    Spanned::default(EVar(Spanned::default(VLocal("i".to_string())))),
                ])),
                Spanned::default(SAssign(
                    vec![Spanned::default(VIndex(
                        Box::new(Spanned::default(VLocal("i".to_string()))),
                        Box::new(Spanned::default(EVar(Spanned::default(VLocal("j".to_string())))))
                    ))],
                    vec![Spanned::default(EVar(Spanned::default(VLocal("a".to_string()))))],
                )),
            ], vec!["i".to_string(), "j".to_string()], Default::default()))),
            Spanned::default(SAssign(
                vec![Spanned::default(VGlobal("j".to_string()))],
                vec![Spanned::default(EVar(Spanned::default(VGlobal("i".to_string()))))],
            )),
        ], vec!["a".to_string()], Default::default()));
    }
}
