//! AST pretty printer

use super::parser;

use lea_ast::*;
use lea_ast::visit::*;

use lea_core::literal::*;

use std::io::{self, Write};


pub struct PrettyPrinter<'a, W: Write + 'a> {
    writer: &'a mut W,
    indent: u16,
    indentstr: &'a str,
    lineend: &'a str,
}

impl<'a, W: Write> PrettyPrinter<'a, W> {
    pub fn new(writer: &'a mut W) -> PrettyPrinter<'a, W> {
        PrettyPrinter {
            writer: writer,
            indent: 0,
            indentstr: "    ",
            lineend: "\n",
        }
    }

    pub fn with_indent_str(writer: &'a mut W, indentstr: &'a str) -> PrettyPrinter<'a, W> {
        PrettyPrinter {
            writer: writer,
            indent: 0,
            indentstr: indentstr,
            lineend: "\n",
        }
    }

    pub fn with_line_end(writer: &'a mut W, lineend: &'a str) -> PrettyPrinter<'a, W> {
        PrettyPrinter {
            writer: writer,
            indent: 0,
            indentstr: "    ",
            lineend: lineend,
        }
    }

    pub fn with_line_end_and_indent_str(writer: &'a mut W, lineend: &'a str, indentstr: &'a str)
    -> PrettyPrinter<'a, W> {
        // TODO no_this_isnt_verbose_at_all_but_maybe_a_builder_would_be_appropriate_here
        PrettyPrinter {
            writer: writer,
            indent: 0,
            indentstr: indentstr,
            lineend: lineend,
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn unindent(&mut self) {
        assert!(self.indent > 0);
        self.indent -= 1;
    }

    #[allow(unused_must_use)]
    fn print_indent(&mut self) {
        for _ in 0..self.indent {
            write!(self.writer, "{}", self.indentstr);
        }
    }

    /// Prints a string in quotes and escapes all chars that need an escape sequence.
    fn print_string(&mut self, s: &str) -> io::Result<()> {
        write!(self.writer, "\"{}\"", s)   // TODO escape
    }

    #[allow(unused_must_use)]
    fn print_table(&mut self, cons: &TableCons) {
        write!(self.writer, "{{{}", self.lineend);
        self.indent();

        for entry in cons {
            self.print_indent();

            match *entry {
                TableEntry::Pair(ref key, ref val) => {
                    let mut key_full = true;    // Print the key in "[expr] = " notation
                    match key.value {
                        ELit(TStr(ref s)) => {
                            // If it's an identifier, don't use "[expr] = " syntax
                            match parser::ident(s.as_ref()) {
                                Ok(..) => {
                                    let s: &str = s.as_ref();
                                    write!(self.writer, "{}", s);
                                    key_full = false;
                                },
                                _ => {},
                            };
                        },
                        _ => {},
                    }

                    if key_full {
                        write!(self.writer, "[");
                        self.visit_expr(key);
                        write!(self.writer, "]");
                    }

                    write!(self.writer, " = ");
                    self.visit_expr(val);
                    write!(self.writer, ",{}", self.lineend);
                }
                TableEntry::Elem(ref elem) => {
                    self.visit_expr(elem);
                    write!(self.writer, ",{}", self.lineend);
                }
            }
        }

        self.unindent();
        self.print_indent();
        write!(self.writer, "}}");
    }

    /// Prints the parameter list and the body of the given function
    #[allow(unused_must_use)]
    fn print_funcbody(&mut self, f: &Function) {
        write!(self.writer, "(");
        let mut first = true;
        for param in &f.params {
            if first { first = false; } else { write!(self.writer, ", "); }
            write!(self.writer, "{}", param.value);
        }

        if f.varargs {
            if f.params.len() == 0 {
                write!(self.writer, "...");
            } else {
                write!(self.writer, ", ...");
            }
        }

        write!(self.writer, "){}", self.lineend);
        // TODO Decide if the function requires multiple lines

        self.indent();
        walk_block_ref(&f.body, self);
        self.unindent();

        self.print_indent();
        write!(self.writer, "end");
    }

    #[allow(unused_must_use)]
    fn print_call_args(&mut self, args: &CallArgs) {
        match *args {
            CallArgs::Normal(ref argv) => {
                write!(self.writer, "(");

                let mut first = true;
                for arg in argv {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(arg);
                }

                write!(self.writer, ")");
            },
            CallArgs::String(ref s) => {
                write!(self.writer, " ");
                self.print_string(s.as_ref());
            },
            CallArgs::Table(ref tbl) => {
                self.print_table(tbl);
            }
        }
    }

    #[allow(unused_must_use)]
    fn print_call(&mut self, c: &Call) {
        match *c {
            SimpleCall(ref callee, ref argv) => {
                self.visit_expr(callee);
                self.print_call_args(argv);
            },
            MethodCall(ref callee, ref name, ref argv) => {
                self.visit_expr(callee);
                write!(self.writer, ":{}", name.value);
                self.print_call_args(argv);
            },
        };
    }
}

impl<'a, 'v, W: Write> Visitor<'v> for PrettyPrinter<'a, W> {
    #[allow(unused_must_use)]
    fn visit_stmt(&mut self, stmt: &Stmt) {
        self.print_indent();

        match stmt.value {
            SDecl(ref names, ref vals) => {
                write!(self.writer, "local ");
                let mut first = true;
                for name in names {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    write!(self.writer, "{}", name.value);
                }

                if vals.len() > 0 {
                    write!(self.writer, " = ");

                    let mut first = true;
                    for val in vals {
                        if first { first = false; } else { write!(self.writer, ", "); }
                        self.visit_expr(val);
                    }
                }
            },
            SAssign(ref vars, ref vals) => {
                let mut first = true;
                for var in vars {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_var(var);
                }

                write!(self.writer, " = ");

                let mut first = true;
                for val in vals {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(val);
                }
            },
            SDo(ref block) => {
                write!(self.writer, "do{}", self.lineend);
                self.indent();
                self.visit_block(block);
                self.unindent();
                write!(self.writer, "end{}", self.lineend);
            },
            SReturn(ref vals) => {
                write!(self.writer, "return");
                if vals.len() > 0 { write!(self.writer, " "); }

                let mut first = true;
                for val in vals {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(val);
                }
            },
            SCall(ref c) => {
                self.print_call(c);
            },
            SFunc(ref var, ref f) => {
                write!(self.writer, "function ");
                self.visit_var(var);
                self.print_funcbody(f);
            },
            SMethod(ref var, ref name, ref f) => {
                write!(self.writer, "function ");
                self.visit_var(var);
                write!(self.writer, ":{}", name.value);
                self.print_funcbody(f);
            },
            SLFunc(ref name, ref f) => {
                write!(self.writer, "local function {}", name.value);
                self.print_funcbody(f);
            },
            SIf {ref cond, ref body, ref el} => {
                write!(self.writer, "if ");
                self.visit_expr(cond);
                write!(self.writer, " then{}", self.lineend);

                self.indent();
                self.visit_block(body);
                self.unindent();

                // TODO handle elseif
                if el.stmts.len() > 0 {
                    write!(self.writer, "else");
                    self.indent();
                    walk_block_ref(el, self);
                    self.unindent();
                }

                write!(self.writer, "end");
            },
            SWhile {ref cond, ref body} => {
                write!(self.writer, "while ");
                self.visit_expr(cond);
                write!(self.writer, " do{}", self.lineend);
                self.indent();
                self.visit_block(body);
                self.unindent();
                write!(self.writer, "end");
            },
            SRepeat {ref abort_on, ref body} => {
                write!(self.writer, "repeat{}", self.lineend);
                self.indent();
                self.visit_block(body);
                self.unindent();
                write!(self.writer, "until ");
                self.visit_expr(abort_on);
            },
            SFor {ref var, ref start, ref step, ref end, ref body} => {
                write!(self.writer, "for {} = ", var.value);
                self.visit_expr(start);
                write!(self.writer, ", ");
                self.visit_expr(end);

                step.as_ref().map(|stepexpr| {
                    write!(self.writer, ", ");
                    self.visit_expr(stepexpr);
                });

                write!(self.writer, " do{}", self.lineend);
                self.indent();
                self.visit_block(body);
                self.unindent();
                write!(self.writer, "end");
            },
            SForIn {ref vars, ref iter, ref body} => {
                write!(self.writer, "for ");

                let mut first = true;
                for var in vars {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    write!(self.writer, "{}", var.value);
                }

                write!(self.writer, " in ");
                let mut first = true;
                for val in iter {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(val);
                }

                write!(self.writer, " do{}", self.lineend);
                self.indent();
                self.visit_block(body);
                self.unindent();
                write!(self.writer, "end");
            },
            SBreak => {
                write!(self.writer, "break{}", self.lineend);
            },
        };

        write!(self.writer, "{}", self.lineend);
    }

    #[allow(unused_must_use)]
    fn visit_expr(&mut self, expr: &Expr) {
        match expr.value {
            ERawOp(ref lhs, ref rest) => {
                self.visit_expr(lhs);

                for &(ref op, ref r) in rest {
                    write!(self.writer, " {} ", op);
                    self.visit_expr(r);
                }
            },
            EBinOp(ref lhs, op, ref rhs) => {
                let prec = op.get_precedence();
                let mut left_paren = false;     // add parentheses around lhs
                let mut right_paren = false;    // add parentheses around rhs

                // add parentheses if the precedence of lhs is lower / rhs is higher
                //
                // this isn't necessary when prettyprinting an AST that was directly parsed (since
                // EBraced is used for braced expressions), but is required when some
                // transformations have been applied (for example, fold.rs replaces `EBraced`).
                match lhs.value {
                    EBinOp(_, lop, _) => {
                        if lop.get_precedence() < prec { left_paren = true; }
                    },
                    _ => {},
                }
                match rhs.value {
                    EBinOp(_, rop, _) => {
                        if rop.get_precedence() > prec { right_paren = true; }
                    }
                    _ => {},
                }

                // braces are printed explicitly
                if let EBraced(_) = lhs.value {
                    left_paren = false;
                }
                if let EBraced(_) = rhs.value {
                    right_paren = false;
                }

                if left_paren { write!(self.writer, "("); }
                self.visit_expr(lhs);
                if left_paren { write!(self.writer, ")"); }

                write!(self.writer, " {} ", op);

                if right_paren { write!(self.writer, "("); }
                self.visit_expr(rhs);
                if right_paren { write!(self.writer, ")"); }
            },
            EBraced(ref e) => {
                write!(self.writer, "(");
                self.visit_expr(&**e);
                write!(self.writer, ")");
            }
            EUnOp(op, ref operand) => {
                match operand.value {
                    EBinOp(..) => {
                        write!(self.writer, "{}(", op);
                        self.visit_expr(operand);
                        write!(self.writer, ")");
                    }
                    _ => {
                        write!(self.writer, "{}", op);
                        self.visit_expr(operand);
                    }
                };
            },
            EVar(ref var) => {
                self.visit_var(var);
            },
            ECall(ref c) => {
                self.print_call(c);
            },
            EFunc(ref f) => {
                write!(self.writer, "function(");
                let mut first = true;
                for param in &f.params {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    write!(self.writer, "{}", param.value);
                }
                if f.varargs { write!(self.writer, "..."); }
                writeln!(self.writer, ")");
                // TODO Decide if the function requires multiple lines

                self.indent();
                self.visit_block(&f.body);
                self.unindent();

                self.print_indent();
                write!(self.writer, "end");
            },
            ETable(ref pairs) => {
                self.print_table(pairs);
            },
            EArray(ref exprs) => {
                write!(self.writer, "[");

                let mut first = true;
                for e in exprs {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(e);
                }

                write!(self.writer, "]");
            },
            EVarArgs => {
                write!(self.writer, "...");
            },
            ELit(ref lit) => {
                match *lit {
                    TInt(i) => write!(self.writer, "{}", i),
                    TFloat(f) => write!(self.writer, "{}", f),
                    TStr(ref s) => self.print_string(s.as_ref()),
                    TBool(b) => write!(self.writer, "{}", b),
                    TNil => write!(self.writer, "nil"),
                };
            },
        };
    }

    #[allow(unused_must_use)]
    fn visit_var(&mut self, var: &Variable) {
        match var.value {
            VNamed(s) => {
                write!(self.writer, "{}", s);
            }
            VResGlobal(_, ref s) => {
                write!(self.writer, "{}", s);
            }
            VLocal(..) | VUpval(..) => {
                // TODO resolve name
                panic!("VLocal not supported in pretty-printer");
            }
            VIndex(ref var, ref expr) => {
                self.visit_var(var);
                write!(self.writer, "[");
                self.visit_expr(expr);
                write!(self.writer, "]");
            }
            VDotIndex(ref var, ref strn) => {
                self.visit_var(var);
                write!(self.writer, ".{}", strn);
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::block;
    use lea_ast::Block;
    use lea_ast::visit::walk_block_ref;

    fn print_block(block: &Block) -> String {
        let mut v = Vec::new();

        {
            let mut pp = PrettyPrinter::new(&mut v);
            walk_block_ref(block, &mut pp);
        }

        String::from_utf8(v).unwrap()
    }

    fn print(code: &str) -> String {
        print_block(&block(code).unwrap())
    }

    fn test(code: &str, expect: &str) {
        assert_eq!(print(&code[1..]), &expect[1..]);    // strip the \n at the start
    }

    /// Parses and prints the given source code. Then parses the print result and asserts that the
    /// two syntax trees are equal.
    fn test_auto(code: &str) {
        let expblock = block(code).unwrap();
        let printed = print_block(&expblock);
        println!("{}", printed);

        let newblock = block(printed.as_ref()).unwrap();

        assert_eq!(expblock, newblock);
    }

    #[test]
    fn basic() {
        test(r#"
local i, j, k = 0, 1, 2*3+4, []
function f(i, j) return i * j end
"#, r#"
local i, j, k = 0, 1, 2 * 3 + 4, []
function f(i, j)
    return i * j
end
"#);
    }

    #[test]
    fn auto_simple() {
        test_auto(r#"
local i, j = 0
"#);
    }

    #[test]
    fn auto() {
        test_auto(r#"
g()
g(0.25, 1)
g(f(1))
function g(...) end
local function f(a, ...) end
f(0, 1.5, g)
return 0, 1, ...
break
do break end
i = 0
i, j, k = [0, 1, 2], {i = 0, j = k}, nil

if true then break else do break end end
for i=1,2,3 do break end
for j=1,2 do break end
while 4>>4 do end
repeat
    do end
until false
for k, v, w in dings(), 1, 2 do end
"#)
    }
}
