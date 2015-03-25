//! AST pretty printer

use super::parser;
use super::ast::*;
use super::visit::*;

use std::io::Write;

pub struct PrettyPrinter<'a, 'b, W: Write + 'a> {
    writer: &'a mut W,
    indent: u16,
    indentstr: &'b str,
    lineend: &'b str,
}

impl <'a, 'b, W: Write> PrettyPrinter<'a, 'b, W> {
    pub fn new(writer: &'a mut W) -> PrettyPrinter<'a, 'b, W> {
        PrettyPrinter {
            writer: writer,
            indent: 0,
            indentstr: "    ",
            lineend: "\n",
        }
    }

    pub fn with_indent_str(writer: &'a mut W, indentstr: &'b str) -> PrettyPrinter<'a, 'b, W> {
        PrettyPrinter {
            writer: writer,
            indent: 0,
            indentstr: indentstr,
            lineend: "\n",
        }
    }

    pub fn with_line_end(writer: &'a mut W, lineend: &'b str) -> PrettyPrinter<'a, 'b, W> {
        PrettyPrinter {
            writer: writer,
            indent: 0,
            indentstr: "    ",
            lineend: lineend,
        }
    }

    pub fn with_line_end_and_indent_str(writer: &'a mut W, lineend: &'b str, indentstr: &'b str) ->
    PrettyPrinter<'a, 'b, W> {
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

    /// Prints the parameter list and the body of the given function
    #[allow(unused_must_use)]
    fn print_funcbody(&mut self, mut f: Function) -> Function {
        write!(self.writer, "(");
        for i in 0..f.params.len() {
            write!(self.writer, "{}", &f.params[i]);

            if i < f.params.len() - 1 {
                write!(self.writer, ", ");
            }
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
        f = walk_func(f, self);
        self.unindent();

        self.print_indent();
        write!(self.writer, "end");

        f
    }

    #[allow(unused_must_use)]
    fn print_call_args(&mut self, mut argv: Vec<Expr>) -> Vec<Expr> {
        write!(self.writer, "(");

        let mut first = true;
        argv = argv.map_in_place(|arg| {
            if first { first = false; } else { write!(self.writer, ", "); }
            self.visit_expr(arg)
        });

        write!(self.writer, ")");
        argv
    }

    #[allow(unused_must_use)]
    fn print_call(&mut self, mut c: Call) -> Call {
        c = match c {
            SimpleCall(mut callee, mut argv) => {
                callee = Box::new(self.visit_expr(*callee));
                argv = self.print_call_args(argv);
                SimpleCall(callee, argv)
            },
            MethodCall(mut callee, name, mut argv) => {
                callee = Box::new(self.visit_expr(*callee));
                write!(self.writer, ":{}", name.value);
                argv = self.print_call_args(argv);
                MethodCall(callee, name, argv)
            },
        };

        c
    }
}

impl <'a, 'b, W: Write> Visitor for PrettyPrinter<'a, 'b, W> {
    #[allow(unused_must_use)]   // Possibly Rust's fault
    fn visit_stmt(&mut self, mut stmt: Stmt) -> Stmt {
        self.print_indent();

        stmt.value = match stmt.value {
            SDecl(names, mut vals) => {
                write!(self.writer, "local ");
                for i in 0..names.len() {
                    let name = &names[i];

                    write!(self.writer, "{}", name);
                    if i < names.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }

                if vals.len() > 0 {
                    write!(self.writer, " = ");

                    let mut first = true;
                    vals = vals.into_iter().map(|val| {
                        if first { first = false; } else { write!(self.writer, ", "); }
                        self.visit_expr(val)
                    }).collect();
                }

                SDecl(names, vals)
            },
            SAssign(mut vars, mut vals) => {
                let mut first = true;
                vars = vars.into_iter().map(|var| {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_var(var)
                }).collect();

                write!(self.writer, " = ");

                let mut first = true;
                vals = vals.into_iter().map(|val| {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(val)
                }).collect();

                SAssign(vars, vals)
            },
            SDo(mut block) => {
                write!(self.writer, "do{}", self.lineend);
                self.indent();
                block = self.visit_block(block);
                self.unindent();
                write!(self.writer, "end{}", self.lineend);

                SDo(block)
            },
            SReturn(mut vals) => {
                write!(self.writer, "return");
                if vals.len() > 0 { write!(self.writer, " "); }

                let mut first = true;
                vals = vals.into_iter().map(|val| {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(val)
                }).collect();

                SReturn(vals)
            },
            SCall(mut c) => {
                c = self.print_call(c);
                SCall(c)
            },
            SFunc(mut var, mut f) => {
                write!(self.writer, "function ");
                var = self.visit_var(var);
                f = self.print_funcbody(f);
                SFunc(var, f)
            },
            SMethod(mut var, name, mut f) => {
                write!(self.writer, "function ");
                var = self.visit_var(var);
                write!(self.writer, ":{}", name);
                f = self.print_funcbody(f);
                SMethod(var, name, f)
            },
            SLFunc(name, mut f) => {
                write!(self.writer, "local function {}", name);
                f = self.print_funcbody(f);
                SLFunc(name, f)
            },
            SIf {mut cond, mut body, mut el} => {
                write!(self.writer, "if ");
                cond = self.visit_expr(cond);
                write!(self.writer, " then{}", self.lineend);

                self.indent();
                body = self.visit_block(body);
                self.unindent();

                // TODO handle elseif
                if el.stmts.len() > 0 {
                    write!(self.writer, "else");
                    self.indent();
                    el = walk_block(el, self);
                    self.unindent();
                }

                write!(self.writer, "end");

                SIf {cond: cond, body: body, el: el}
            },
            SWhile {mut cond, mut body} => {
                write!(self.writer, "while ");
                cond = self.visit_expr(cond);
                write!(self.writer, " do{}", self.lineend);
                self.indent();
                body = self.visit_block(body);
                self.unindent();
                write!(self.writer, "end");

                SWhile {cond: cond, body: body}
            },
            SRepeat {mut abort_on, mut body} => {
                write!(self.writer, "repeat{}", self.lineend);
                self.indent();
                body = self.visit_block(body);
                self.unindent();
                write!(self.writer, "until ");
                abort_on = self.visit_expr(abort_on);

                SRepeat {abort_on: abort_on, body: body}
            },
            SFor {var, mut start, mut step, mut end, mut body} => {
                write!(self.writer, "for {} = ", var);
                start = self.visit_expr(start);
                write!(self.writer, ", ");
                end = self.visit_expr(end);

                match step.value {
                    ELit(TInt(1)) => {},    // default step, ignore
                    _ => {
                        write!(self.writer, ", ");
                        step = self.visit_expr(step);
                    },
                }

                write!(self.writer, " do{}", self.lineend);
                self.indent();
                body = self.visit_block(body);
                self.unindent();
                write!(self.writer, "end");

                SFor {var: var, start: start, step: step, end: end, body: body}
            },
            SForIn {vars, mut iter, mut body} => {
                write!(self.writer, "for ");
                for i in 0..vars.len() {
                    write!(self.writer, "{}", &vars[i]);

                    if i < vars.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }
                write!(self.writer, " in ");
                let mut first = true;
                iter = iter.into_iter().map(|val| {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(val)
                }).collect();
                write!(self.writer, " do{}", self.lineend);
                self.indent();
                body = self.visit_block(body);
                self.unindent();
                write!(self.writer, "end");

                SForIn {vars: vars, iter: iter, body: body}
            },
            SBreak => {
                write!(self.writer, "break{}", self.lineend);
                SBreak
            },
        };

        write!(self.writer, "{}", self.lineend);
        stmt
    }

    #[allow(unused_must_use)]
    fn visit_expr(&mut self, mut expr: Expr) -> Expr {
        expr.value = match expr.value {
            ERawOp(mut lhs, mut rest) => {
                lhs = Box::new(self.visit_expr(*lhs));

                rest = rest.into_iter().map(|(op, r)| {
                    write!(self.writer, " {} ", op);
                    (op, self.visit_expr(r))
                }).collect();

                ERawOp(lhs, rest)
            },
            EBinOp(mut lhs, op, mut rhs) => {
                let prec = op.get_precedence();
                let mut left_paren = false;     // add parentheses around lhs
                let mut right_paren = false;    // add parentheses around rhs

                // add parentheses if the precedence of lhs is lower / rhs is higher
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

                if left_paren { write!(self.writer, "("); }
                lhs = Box::new(self.visit_expr(*lhs));
                if left_paren { write!(self.writer, ")"); }

                write!(self.writer, " {} ", op);

                if right_paren { write!(self.writer, "("); }
                rhs = Box::new(self.visit_expr(*rhs));
                if right_paren { write!(self.writer, ")"); }

                EBinOp(lhs, op, rhs)
            },
            EUnOp(op, mut operand) => {
                match operand.value {
                    EBinOp(..) => {
                        write!(self.writer, "{}(", op);
                        operand = Box::new(self.visit_expr(*operand));
                        write!(self.writer, ")");
                    }
                    _ => {
                        write!(self.writer, "{}", op);
                        operand = Box::new(self.visit_expr(*operand));
                    }
                };

                EUnOp(op, operand)
            },
            EVar(mut var) => {
                var = self.visit_var(var);
                EVar(var)
            },
            ECall(mut c) => {
                c = self.print_call(c);
                ECall(c)
            },
            EFunc(mut f) => {
                write!(self.writer, "function(");
                let mut first = true;
                f.params.iter().map(|param| {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    write!(self.writer, "{}", &param);
                });
                if f.varargs { write!(self.writer, "..."); }
                writeln!(self.writer, ")");
                // TODO Decide if the function requires multiple lines

                self.indent();
                f.body = self.visit_block(f.body);
                self.unindent();

                self.print_indent();
                write!(self.writer, "end");

                EFunc(f)
            },
            ETable(mut pairs) => {
                write!(self.writer, "{{");
                self.indent();

                pairs = pairs.into_iter().map(|(mut key, mut val)| {
                    self.print_indent();

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
                        key = self.visit_expr(key);
                        write!(self.writer, "]");
                    }

                    write!(self.writer, " = ");
                    val = self.visit_expr(val);
                    write!(self.writer, ",{}", self.lineend);

                    (key, val)
                }).collect();

                self.unindent();
                self.print_indent();
                write!(self.writer, "}}");

                ETable(pairs)
            },
            EArray(mut exprs) => {
                write!(self.writer, "[");

                let mut first = true;
                exprs = exprs.into_iter().map(|e| {
                    if first { first = false; } else { write!(self.writer, ", "); }
                    self.visit_expr(e)
                }).collect();

                write!(self.writer, "]");
                EArray(exprs)
            },
            EVarArgs => {
                write!(self.writer, "...");
                EVarArgs
            },
            ELit(lit) => {
                match lit {
                    TInt(i) => write!(self.writer, "{}", i),
                    TFloat(f) => write!(self.writer, "{}", f),
                    TStr(ref s) => write!(self.writer, "{}", s),
                    TBool(b) => write!(self.writer, "{}", b),
                    TNil => write!(self.writer, "nil"),
                };

                ELit(lit)
            },
        };

        expr
    }

    #[allow(unused_must_use)]
    fn visit_var(&mut self, mut var: Variable) -> Variable {
        match var.value {
            VNamed(ref s) | VGlobal(ref s) | VResGlobal(_, ref s) => {
                write!(self.writer, "{}", s);
            },
            VLocal(..) | VUpval(..) => {
                // TODO resolve name
                panic!("VLocal not supported in pretty-printer");
            },
            _ => {
                var.value = match var.value {
                    VIndex(mut var, mut expr) => {
                        var = Box::new(self.visit_var(*var));
                        write!(self.writer, "[");
                        expr = Box::new(self.visit_expr(*expr));
                        write!(self.writer, "]");
                        VIndex(var, expr)
                    },
                    VDotIndex(mut var, strn) => {
                        var = Box::new(self.visit_var(*var));
                        write!(self.writer, ".{}", strn);
                        VDotIndex(var, strn)
                    },
                    _ => { return var; }
                };
            }
        };

        var
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use compiler::parser::block;
    use compiler::visit::walk_block;
    use compiler::ast::Block;

    fn print_block(mut block: Block) -> (Block, String) {
        let mut v = Vec::new();

        {
            let mut pp = PrettyPrinter::new(&mut v);
            block = walk_block(block, &mut pp);
        }

        (block, String::from_utf8(v).unwrap())
    }

    fn print(code: &str) -> String {
        print_block(block(code).unwrap()).1
    }

    fn test(code: &str, expect: &str) {
        assert_eq!(print(&code[1..]), &expect[1..]);    // strip the \n at the start
    }

    /// Parses and prints the given source code. Then parses the print result and asserts that the
    /// two syntax trees are equal.
    fn test_auto(code: &str) {
        let expblock = block(code).unwrap();
        let (expblock, printed) = print_block(expblock);

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

    #[test] #[should_panic]
    fn auto_fail() {
        test_auto(r#"
7
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
