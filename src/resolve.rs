//! This module contains the logic used to resolve identifiers to their locals or globals.

use ast::*;
use visit::*;

use std::mem;
use std::collections::HashSet;

/// A resolver will work on a single scope and resolve any `VNamed` references
struct Resolver<'a> {
    locals: HashSet<String>,
}

impl <'a> Resolver<'a> {
    fn is_declared(&mut self, name: &String) -> bool {
        self.locals.contains(name)
    }

    fn add_local(&mut self, name: String) {
        self.locals.insert(name);
    }

    fn resolve_var(&mut self, v: &mut Variable) {
        if let VNamed(..) = v.value {
            let mut name = String::new();

            if let VNamed(ref mut vname) = v.value {
                mem::swap(vname, &mut name);
            }

            mem::replace(&mut v.value, if self.is_declared(&name) {
                VLocal(name)
            } else {
                VGlobal(name)
            });
        } else {
            // Only VNamed variables should be existing at this point
            unreachable!();
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
        resolve_block(b);
    }
}

pub fn resolve_block(b: &mut Block) {
    let mut res = Resolver {
        locals: HashSet::new(),
    };

    walk_block(b, &mut res);

    for local in res.locals {
        b.locals.push(local);
    }
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
do
    local i
    local j = i
    j = i
end
j = i
"#).unwrap();
        resolve_block(&mut b);

        assert_eq!(b, Spanned::default(_Block::new(vec![
            Spanned::default(SAssign(
                vec![Spanned::default(VGlobal("i".to_string()))],
                vec![Spanned::default(ELit(TInt(0)))],
            )),
            Spanned::default(SDo(Spanned::default(_Block::with_locals(vec![
                Spanned::default(SDecl(vec!["i".to_string()], vec![])),
                Spanned::default(SDecl(vec!["j".to_string()], vec![
                    Spanned::default(EVar(Spanned::default(VLocal("i".to_string())))),
                ])),
                Spanned::default(SAssign(
                    vec![Spanned::default(VLocal("j".to_string()))],
                    vec![Spanned::default(EVar(Spanned::default(VLocal("i".to_string()))))],
                )),
            ], vec!["i".to_string(), "j".to_string()])))),
            Spanned::default(SAssign(
                vec![Spanned::default(VGlobal("j".to_string()))],
                vec![Spanned::default(EVar(Spanned::default(VGlobal("i".to_string()))))],
            )),
        ])));
    }
}
