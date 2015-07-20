//! Checks if an AST is valid

use ast::*;
use ast::span::*;
use ast::visit::*;

use term::Terminal;

use std::io::{self, Write};


#[derive(Clone, Debug)]
pub struct CheckError {
    pub msg: &'static str,
    pub span: Span,
}

impl CheckError {
    /// Prints this `CheckError` to a terminal.
    pub fn format<W: Write>(&self, code: &str, source_name: &str, t: &mut Terminal<Output=W>)
    -> io::Result<()> {
        try!(self.span.print_with_err(code, source_name, self.msg, t));

        Ok(())
    }
}


struct Checker {
    errs: Vec<CheckError>,
    vararg_func: bool,
    looplvl: u32,
}

impl<'a> Visitor<'a> for Checker {
    fn visit_stmt(&mut self, s: &Stmt) {
        match s.value {
            SBreak => {
                if self.looplvl == 0 {
                    self.errs.push(CheckError {
                        msg: "use of `break` outside of loop",
                        span: s.span,
                    });
                }
            },
            SFor{..} | SWhile{..} | SRepeat{..} => {
                self.looplvl += 1;
                walk_stmt_ref(s, self);
                self.looplvl -= 1;
            },
            _ => walk_stmt_ref(s, self),
        }
    }

    fn visit_expr(&mut self, e: &Expr) {
        match e.value {
            EVarArgs => {
                if !self.vararg_func {
                    self.errs.push(CheckError {
                        msg: "use of ... outside varargs function",
                        span: e.span,
                    });
                }
            },
            _ => {},
        }
    }

    fn visit_func(&mut self, func: &Function) {
        let varargs_old = self.vararg_func;
        let looplvl_old = self.looplvl;
        self.vararg_func = func.varargs;
        self.looplvl = 0;

        self.visit_block(&func.body);

        self.vararg_func = varargs_old;
        self.looplvl = looplvl_old;
    }
}

/// Checks that the passed function contains only valid statements.
///
/// Returns a non-empty `Vec<CheckError>` if any errors were found.
pub fn check_func(func: &Function) -> Result<(), Vec<CheckError>> {
    let mut ch = Checker {
        errs: vec![],
        vararg_func: func.varargs,
        looplvl: 0,
    };

    ch.visit_block(&func.body);

    if ch.errs.len() == 0 {
        Ok(())
    } else {
        Err(ch.errs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::parse_main;

    #[test]
    fn test() {
        assert!(check_func(&parse_main("break").unwrap()).is_err());
        assert!(check_func(&parse_main("for i=0,1 do break end").unwrap()).is_ok());
        assert!(check_func(&parse_main("i = ...").unwrap()).is_ok());
        assert!(check_func(&parse_main(r#"
for i=0,1 do
    function i() break end
end
"#).unwrap()).is_err());
        assert!(check_func(&parse_main(r#"
function f(a)
    a = ...
end
"#).unwrap()).is_err());
    }
}
