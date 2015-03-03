//! Checks if an AST is valid

use std::fmt;

use super::ast::*;
use super::visit::*;
use super::span::Span;

#[derive(Clone, Debug)]
pub struct CheckError {
    pub msg: &'static str,
    pub detail: Option<String>,
    pub span: Span,
}

impl CheckError {
    /// Converts this `CheckError` to a string. Note that multiple lines are printed.
    ///
    /// # Parameters
    /// * `code` - The source code from which the checked function was compiled
    /// * `source_name` - The name of the code source for this piece of source code
    pub fn format(&self, code: &str, source_name: &str) -> String {
        let (start, _end) = self.span.get_lines(code);
        let mut res = format!("{}:{}: {}", source_name, start, self.msg);

        if let Some(ref detail) = self.detail {
            res.push_str(format!(" ({})", detail).as_slice());
        }

        res.push('\n');
        res.push_str(self.span.format(code, source_name).as_slice());

        res
    }
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

struct Checker {
    errs: Vec<CheckError>,
    vararg_func: bool,
    looplvl: u32,
}

impl Visitor for Checker {
    fn visit_stmt(&mut self, s: &mut Stmt) {
        match s.value {
            SBreak => {
                if self.looplvl == 0 {
                    self.errs.push(CheckError {
                        msg: "Use of `break` outside of loop",
                        detail: None,
                        span: s.span,
                    });
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
        }
    }

    fn visit_expr(&mut self, e: &mut Expr) {
        match e.value {
            EVarArgs => {
                if !self.vararg_func {
                    self.errs.push(CheckError {
                        msg: "Use of ... outside varargs function",
                        detail: None,
                        span: e.span,
                    });
                }
            },
            _ => {},
        }
    }

    fn visit_func(&mut self, func: &mut Function) {
        if let Err(ref mut new_errs) = check_func(func) {
            self.errs.append(new_errs);
        }
    }
}

/// Checks that the passed function contains only valid statements.
///
/// Returns a non-empty `Vec<CheckError>` if any errors were found.
pub fn check_func(func: &mut Function) -> Result<(), Vec<CheckError>> {
    let mut ch = Checker {
        errs: vec![],
        vararg_func: func.value.varargs,
        looplvl: 0,
    };
    walk_block(&mut func.body, &mut ch);

    if ch.errs.len() == 0 {
        Ok(())
    } else {
        Err(ch.errs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parser::parse_main;

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
