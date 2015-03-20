//! Checks if an AST is valid

use super::ast::*;
use super::visit::*;
use super::span::*;

use std::fmt;
use std::io::{self, Write};


#[derive(Clone, Debug)]
pub struct CheckError {
    pub msg: &'static str,
    pub detail: Option<String>,
    pub span: Span,
}

impl CheckError {
    /// Converts this `CheckError` to a string. Note that multiple lines are printed.
    ///
    /// See `Span::format` for details.
    pub fn format<W: Write>(&self, code: &str, source_name: &str, fmt: &mut FormatTarget<W>)
    -> io::Result<()> {
        let (start, _end) = self.span.get_lines(code);
        try!(write!(fmt, "{}:{}: {}", source_name, start, self.msg));

        if let Some(ref detail) = self.detail {
            try!(write!(fmt, " ({})", detail));
        }

        try!(write!(fmt, "\n"));
        try!(self.span.format(code, source_name, fmt));

        Ok(())
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
    fn visit_stmt(&mut self, mut s: Stmt) -> Stmt {
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
                s = walk_stmt(s, self);
                self.looplvl -= 1;
            },
            _ => { return walk_stmt(s, self); },
        };

        s
    }

    fn visit_expr(&mut self, e: Expr) -> Expr {
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
        };

        e
    }

    fn visit_func(&mut self, func: Function) -> Function {
        let (func, res) = check_func(func);
        if let Err(mut new_errs) = res {
            self.errs.append(&mut new_errs);
        };

        func
    }
}

/// Checks that the passed function contains only valid statements.
///
/// Returns a non-empty `Vec<CheckError>` if any errors were found.
pub fn check_func(mut func: Function) -> (Function, Result<(), Vec<CheckError>>) {
    let mut ch = Checker {
        errs: vec![],
        vararg_func: func.varargs,
        looplvl: 0,
    };

    func = walk_func(func, &mut ch);

    if ch.errs.len() == 0 {
        (func, Ok(()))
    } else {
        (func, Err(ch.errs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parser::parse_main;

    #[test]
    fn test() {
        assert!(check_func(parse_main("break").unwrap()).1.is_err());
        assert!(check_func(parse_main("for i=0,1 do break end").unwrap()).1.is_ok());
        assert!(check_func(parse_main("i = ...").unwrap()).1.is_ok());
        assert!(check_func(parse_main(r#"
function f(a)
    a = ...
end
"#).unwrap()).1.is_err());
    }
}
