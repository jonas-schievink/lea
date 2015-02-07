//! Checks if an AST is valid

use ast::*;
use visit::*;

pub struct CheckError {
    msg: &'static str,
    detail: String,
}

fn mkerr(msg: &'static str, detail: String) -> CheckError {
    CheckError {
        msg: msg,
        detail: detail,
    }
}

struct Checker {
    res: Result<(), CheckError>,
    forlvl: u32,
}

impl Visitor for Checker {
    fn visit_stmt(&mut self, s: &mut Stmt) {
        if let Ok(..) {
            self.res = match *s {
                SBreak => {
                    if self.forlvl == 0 {
                        Err(mkerr("Use of `break` outside of for loop"))
                    } else { Ok(()) }
                },
                _ => { Ok(()) },
            };

            walk_stmt(s, self);
        }
    }
}

/// Checks that the passed block contains only valid statements.
pub fn check_block(block: &mut Block) -> Result<(), CheckError> {
    let ch = Checker {
        res: Ok(()),
        forlvl: 0,
    };
    walk_block(block, &mut ch);
    ch.res
}

#[cfg(test)]
mod tests {
    use super::*;
    use parse::block;

    #[test]
    fn test() {
        assert!(check_block(block("break")).is_err());
    }
}
