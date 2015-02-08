//! Checks if an AST is valid

use std::fmt;

use ast::*;
use visit::*;

pub struct CheckError {
    msg: &'static str,
    detail: Option<String>,
}

impl fmt::Display for CheckError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.detail {
            None => {
                fmt.write_fmt(format_args!("{}", self.msg))
            },
            Some(ref detail) => {
                fmt.write_fmt(format_args!("{}: {}", self.msg, detail))
            }
        }
    }
}

fn mkerr(msg: &'static str, detail: Option<String>) -> CheckError {
    CheckError {
        msg: msg,
        detail: detail,
    }
}

struct Checker {
    res: Result<(), CheckError>,
    vararg_func: bool,
    looplvl: u32,
}

impl Visitor for Checker {
    fn visit_stmt(&mut self, s: &mut Stmt) {
        if let Ok(..) = self.res {
            match s.value {
                SBreak => {
                    if self.looplvl == 0 {
                        self.res = Err(mkerr("Use of `break` outside of loop", None));
                    }
                },
                SFor{..} | SWhile{..} | SRepeat{..} => {
                    self.looplvl += 1;
                    walk_stmt(s, self);
                    self.looplvl -= 1;
                },
                _ => {
                    walk_stmt(s, self);
                },
            };
        }
    }

    fn visit_expr(&mut self, e: &mut Expr) {
        match e.value {
            EVarArgs => {
                if let Ok(..) = self.res {
                    if !self.vararg_func {
                        self.res = Err(mkerr("Use of `...` outside varargs function", None));
                    }
                }
            },
            _ => {},
        }
    }

    fn visit_func(&mut self, func: &mut Function) {
        if let Ok(..) = self.res {
            self.res = check_func(func);
        }
    }
}

/// Checks that the passed block contains only valid statements.
pub fn check_func(func: &mut Function) -> Result<(), CheckError> {
    let mut ch = Checker {
        res: Ok(()),
        vararg_func: func.value.varargs,
        looplvl: 0,
    };
    walk_block(&mut func.body, &mut ch);

    ch.res
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::parse_main;

    #[test]
    fn test() {
        assert!(check_func(&mut parse_main("break").unwrap()).is_err());
        assert!(check_func(&mut parse_main("for i=0,1 do break end").unwrap()).is_ok());
        assert!(check_func(&mut parse_main("i = ...").unwrap()).is_ok());
        assert!(check_func(&mut parse_main(r#"
function f(a)
    a = ...
end
"#).unwrap()).is_err());
    }
}
