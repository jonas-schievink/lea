//! AST pretty printer

use parser;
use ast::*;
use visit::*;

pub struct PrettyPrinter<'a, 'b, W: Writer + 'a> {
    writer: &'a mut W,
    indent: u16,
    indentstr: &'b str,
    lineend: &'b str,
}

impl <'a, 'b, W: Writer> PrettyPrinter<'a, 'b, W> {
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
        for _ in range(0, self.indent) {
            self.writer.write_str(self.indentstr);
        }
    }

    /// Prints the parameter list and the body of the given function
    #[allow(unused_must_use)]
    fn print_funcbody(&mut self, f: &mut Function) {
        write!(self.writer, "(");
        for i in range(0, f.params.len()) {
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
        for stmt in &mut f.body.stmts {
            self.visit_stmt(stmt);
        }
        self.unindent();

        self.print_indent();
        write!(self.writer, "end");
    }

    /// Prints a function call
    #[allow(unused_must_use)]
    fn print_call(&mut self, c: &mut Call) {
        let Call(ref mut expr, ref mut args) = *c;

        self.visit_expr(expr);
        self.writer.write_str("(");

        for i in range(0, args.len()) {
            self.visit_expr(&mut args[i]);

            if i < args.len() - 1 {
                self.writer.write_str(", ");
            }
        }

        self.writer.write_str(")");
    }
}

impl <'a, 'b, W: Writer> Visitor for PrettyPrinter<'a, 'b, W> {
    #[allow(unused_must_use)]   // Possibly Rust's fault
    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        self.print_indent();

        match *stmt {
            SDecl(ref names, ref mut vals) => {
                write!(self.writer, "local ");
                for i in range(0, names.len()) {
                    let name = &names[i];

                    write!(self.writer, "{}", name);
                    if i < names.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }

                if vals.len() > 0 {
                    write!(self.writer, " = ");

                    for i in range(0, vals.len()) {
                        self.visit_expr(&mut vals[i]);

                        if i < vals.len() - 1 {
                            write!(self.writer, ", ");
                        }
                    }
                }
            },
            SAssign(ref mut vars, ref mut vals) => {
                for i in range(0, vars.len()) {
                    self.visit_var(&mut vars[i]);
                    if i < vars.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }

                write!(self.writer, " = ");

                for i in range(0, vals.len()) {
                    self.visit_expr(&mut vals[i]);
                    if i < vals.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }
            },
            SDo(Block {ref mut stmts, ..}) => {
                write!(self.writer, "do{}", self.lineend);
                self.indent();

                for stmt in stmts {
                    self.visit_stmt(stmt);
                }

                self.unindent();
                write!(self.writer, "end{}", self.lineend);
            },
            SReturn(ref mut vals) => {
                write!(self.writer, "return");
                if vals.len() > 0 { write!(self.writer, " "); }

                for i in range(0, vals.len()) {
                    self.visit_expr(&mut vals[i]);

                    if i < vals.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }
            },
            SCall(ref mut c) => {
                self.print_call(c);
            },
            SFunc(ref mut var, ref mut f) => {
                write!(self.writer, "function ");
                self.visit_var(var);

                self.print_funcbody(f);
            },
            SLFunc(ref name, ref mut f) => {
                write!(self.writer, "local function {}", name);
                self.print_funcbody(f);
            },
            SIf {ref mut cond, ref mut body, ref mut el} => {
                write!(self.writer, "if ");
                self.visit_expr(cond);
                write!(self.writer, " then{}", self.lineend);

                self.indent();
                for stmt in &mut body.stmts {
                    self.visit_stmt(stmt);
                }
                self.unindent();

                let els = &mut el.stmts;
                if els.len() > 0 {
                    write!(self.writer, "else");
                    self.indent();
                    for stmt in els {
                        self.visit_stmt(stmt);
                    }
                    self.unindent();
                }

                write!(self.writer, "end");
            },
            SWhile {ref mut cond, ref mut body} => {
                write!(self.writer, "while ");
                self.visit_expr(cond);
                write!(self.writer, " do{}", self.lineend);
                self.indent();
                for stmt in &mut body.stmts {
                    self.visit_stmt(stmt);
                }
                self.unindent();
                write!(self.writer, "end");
            },
            SRepeat {ref mut abort_on, ref mut body} => {
                write!(self.writer, "repeat{}", self.lineend);
                self.indent();
                for stmt in &mut body.stmts {
                    self.visit_stmt(stmt);
                }
                self.unindent();
                write!(self.writer, "until ");
                self.visit_expr(abort_on);
            },
            SFor {ref var, ref mut start, ref mut step, ref mut end, ref mut body, ..} => {
                write!(self.writer, "for {} = ", var);
                self.visit_expr(start);
                write!(self.writer, ", ");
                self.visit_expr(end);

                match *step {
                    ELit(TInt(1)) => {},    // default step, ignore
                    _ => {
                        write!(self.writer, ", ");
                        self.visit_expr(step);
                    },
                }

                write!(self.writer, " do{}", self.lineend);
                self.indent();
                for stmt in &mut body.stmts {
                    self.visit_stmt(stmt);
                }
                self.unindent();
                write!(self.writer, "end");
            },
            SForIn {ref vars, ref mut iter, ref mut body, ..} => {
                write!(self.writer, "for ");
                for i in range(0, vars.len()) {
                    write!(self.writer, "{}", &vars[i]);

                    if i < vars.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }
                write!(self.writer, " in ");
                for i in range(0, iter.len()) {
                    self.visit_expr(&mut iter[i]);

                    if i < vars.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }
                write!(self.writer, " do{}", self.lineend);
                self.indent();
                for stmt in &mut body.stmts {
                    self.visit_stmt(stmt);
                }
                self.unindent();
                write!(self.writer, "end");
            },
            SBreak => {
                write!(self.writer, "break{}", self.lineend);
            },
        }

        write!(self.writer, "{}", self.lineend);
    }

    #[allow(unused_must_use)]
    fn visit_expr(&mut self, expr: &mut Expr) {
        match *expr {
            EBinOp(ref mut lhs, op, ref mut rhs) => {
                let prec = op.get_precedence();
                let mut left_paren = false;     // Add parentheses to lhs
                let mut right_paren = false;    // Add parentheses to rhs

                // Add parentheses if the precedence of lhs is lower / rhs is higher
                match **lhs {
                    EBinOp(_, lop, _) => {
                        if lop.get_precedence() < prec { left_paren = true; }
                    },
                    _ => {},    // No need for parentheses
                }
                match **rhs {
                    EBinOp(_, rop, _) => {
                        if rop.get_precedence() > prec { right_paren = true; }
                    }
                    _ => {},    // No need for parentheses
                }

                if left_paren { write!(self.writer, "("); }
                self.visit_expr(lhs);
                if left_paren { write!(self.writer, ")"); }

                write!(self.writer, " {} ", op);

                if right_paren { write!(self.writer, "("); }
                self.visit_expr(rhs);
                if right_paren { write!(self.writer, ")"); }
            },
            EUnOp(op, ref mut operand) => {
                match **operand {
                    EBinOp(..) => {
                        write!(self.writer, "{}(", op);
                        self.visit_expr(operand);
                        write!(self.writer, ")");
                    }
                    _ => {
                        write!(self.writer, "{}", op);
                        self.visit_expr(operand);
                    }
                }
            },
            EVar(ref mut var) => {
                self.visit_var(var);
            },
            ECall(ref mut c) => {
                self.print_call(c);
            },
            EFunc(Function {ref mut body, ref params, varargs}) => {
                write!(self.writer, "function(");
                for i in range(0, params.len()) {
                    write!(self.writer, "{}", &params[i]);

                    if i < params.len() - 1 {
                        write!(self.writer, ", ");
                    }
                }
                if varargs { write!(self.writer, "..."); }
                writeln!(self.writer, ")");
                // TODO Decide if the function requires multiple lines

                self.indent();
                for stmt in &mut body.stmts {
                    self.visit_stmt(stmt);
                }
                self.unindent();

                self.print_indent();
                write!(self.writer, "end");
            },
            ETable(ref mut pairs) => {
                self.writer.write_str("{");
                self.indent();

                for &mut (ref mut key, ref mut val) in pairs {
                    self.print_indent();

                    let mut key_full = true;    // Print the key in "[expr] = " notation
                    match *key {
                        ELit(TStr(ref s)) => {
                            // If it's an identifier, don't use "[expr] = " syntax
                            match parser::ident(s.as_slice()) {
                                Ok(..) => {
                                    self.writer.write_str(s.as_slice());
                                    key_full = false;
                                },
                                _ => {},
                            };
                        },
                        _ => {},
                    }

                    if key_full {
                        self.writer.write_str("[");
                        self.visit_expr(key);
                        self.writer.write_str("]");
                    }

                    self.writer.write_str(" = ");
                    self.visit_expr(val);
                    write!(self.writer, ",{}", self.lineend);
                }

                self.unindent();
                self.print_indent();
                self.writer.write_str("}");
            },
            EArray(ref mut exprs) => {
                write!(self.writer, "[");

                for i in range(0, exprs.len()) {
                    self.visit_expr(&mut exprs[i]);

                    if i < exprs.len() - 1 {
                        write!(self.writer, ", ");
                    }
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
                    TStr(ref s) => write!(self.writer, "{}", s),
                    TBool(b) => write!(self.writer, "{}", b),
                    TNil => write!(self.writer, "nil"),
                };
            },
        }
    }

    #[allow(unused_must_use)]
    fn visit_var(&mut self, var: &mut Variable) {
        match *var {
            VNamed(ref s) | VLocal(ref s) | VGlobal(ref s) => {
                write!(self.writer, "{}", s);
            },
            VIndex(ref mut var, ref mut expr) => {
                self.visit_var(var);
                write!(self.writer, "[");
                self.visit_expr(expr);
                write!(self.writer, "]");
            },
            VDotIndex(ref mut var, ref strn) => {
                self.visit_var(var);
                write!(self.writer, ".{}", strn);
            },
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::block;
    use visit::walk_block;
    use ast::Block;

    fn print_block(block: &mut Block) -> String {
        let mut v = Vec::new();

        {
            let mut pp = PrettyPrinter::new(&mut v);
            walk_block(block, &mut pp);
        }

        String::from_utf8(v).unwrap()
    }

    fn print(code: &str) -> String {
        let mut block = block(code).unwrap();

        print_block(&mut block)
    }

    fn test(code: &str, expect: &str) {
        assert_eq!(print(&code[1..]), &expect[1..]);
    }

    /// Parses and prints the given source code. Then parses tihe print result and asserts that the
    /// two syntax trees are the same.
    fn test_auto(code: &str) {
        let mut expblock = block(code).unwrap();
        let printed = print_block(&mut expblock);

        let newblock = block(printed.as_slice()).unwrap();

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

    #[test] #[should_fail]
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
