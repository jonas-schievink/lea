//! This module contains the logic used to resolve identifiers to their locals or globals.

use ast::*;
use visit::*;

use std::mem;
use std::collections::HashSet;

/// A resolver will work on a single scope and resolve any `VNamed` references
struct Resolver<'a> {
    /// Set of locals reachable from within the currently resolved block (contains all locals from
    /// parent scopes)
    reachable: HashSet<String>,
    /// Set of locals declared within this block
    owned: HashSet<String>,
}

impl <'a> Resolver<'a> {
    fn is_declared(&mut self, name: &String) -> bool {
        self.reachable.contains(name)
    }

    fn add_local(&mut self, name: String) {
        self.owned.insert(name.clone());
        self.reachable.insert(name);
    }

    fn resolve_var(&mut self, v: &mut Variable) {
        match v.value {
            VNamed(..) => {
                let mut name = String::new();

                if let VNamed(ref mut vname) = v.value {
                    mem::swap(vname, &mut name);
                }

                mem::replace(&mut v.value, if self.is_declared(&name) {
                    VLocal(name)
                } else {
                    VGlobal(name)
                });
            },
            VIndex(..) => {
                walk_var(v, self);
            },
            _ => {
                panic!("unexpected variable: {:?}", v.value);
            }
        }
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
            _ => {
                walk_stmt(s, self);
            }
        }
    }

    fn visit_var(&mut self, v: &mut Variable) {
        self.resolve_var(v);
    }

    fn visit_block(&mut self, b: &mut Block) {
        let res = Resolver {
            reachable: self.reachable.clone(),    // inner scopes have access to outer locals
            owned: HashSet::new(),
        };

        resolve_block_with(b, res);
    }
}

fn resolve_block_with<'a>(b: &mut Block, mut res: Resolver<'a>) {
    walk_block(b, &mut res);

    // Tell the block about its locals
    for local in res.owned {
        b.locals.push(local);
    }
}

/// Resolves all locals used in the given block. Recursively resolves all blocks found inside.
///
/// Allows blocks inside the given block to access locals declared within the parent block
/// (assuming they are declared before the block). Does not allow the given block to access outer
/// locals.
pub fn resolve_block(b: &mut Block) {
    let res = Resolver {
        reachable: HashSet::new(),
        owned: HashSet::new(),
    };

    resolve_block_with(b, res);
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::block;
    use span::Spanned;
    use ast::*;

    #[test]
    fn test() {
        let mut b = block(r#"
i = 0
local a
do
    local i
    local j = i
    i[j] = a
end
j = i
"#).unwrap();
        resolve_block(&mut b);

        assert_eq!(b, Spanned::default(_Block::with_locals(vec![
            Spanned::default(SAssign(
                vec![Spanned::default(VGlobal("i".to_string()))],
                vec![Spanned::default(ELit(TInt(0)))],
            )),
            Spanned::default(SDecl(vec!["a".to_string()], vec![])),
            Spanned::default(SDo(Spanned::default(_Block::with_locals(vec![
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
            ], vec!["i".to_string(), "j".to_string()])))),
            Spanned::default(SAssign(
                vec![Spanned::default(VGlobal("j".to_string()))],
                vec![Spanned::default(EVar(Spanned::default(VGlobal("i".to_string()))))],
            )),
        ], vec!["a".to_string()])));
    }
}
