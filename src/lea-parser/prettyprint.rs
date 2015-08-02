//! Parse tree pretty printer

use super::parser;

use parsetree::*;

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

    pub fn set_indent_str(&mut self, indent_str: &'a str) {
        self.indentstr = indent_str;
    }

    pub fn set_line_end(&mut self, line_end: &'a str) {
        self.lineend = line_end;
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn unindent(&mut self) {
        assert!(self.indent > 0);
        self.indent -= 1;
    }

    fn print_indent(&mut self) -> io::Result<()> {
        for _ in 0..self.indent {
            try!(write!(self.writer, "{}", self.indentstr));
        }

        Ok(())
    }

    /// Prints a string in quotes and escapes all chars that need an escape sequence.
    fn print_string(&mut self, s: &str) -> io::Result<()> {
        write!(self.writer, "\"{}\"", s)   // TODO escape
    }

    fn print_table(&mut self, cons: &TableCons) -> io::Result<()> {
        try!(write!(self.writer, "{{{}", self.lineend));
        self.indent();

        for entry in cons {
            try!(self.print_indent());

            match *entry {
                TableEntry::Pair(ref key, ref val) => {
                    match key.value {
                        ELit(TStr(ref s)) if parser::ident(s.as_ref()).is_ok() => {
                            // If it's an identifier, don't use "[expr] = " syntax
                            try!(write!(self.writer, "{}", s));
                        }
                        _ => {
                            try!(write!(self.writer, "["));
                            try!(self.print_expr(key));
                            try!(write!(self.writer, "]"));
                        }
                    }

                    try!(write!(self.writer, " = "));
                    try!(self.print_expr(val));
                    try!(write!(self.writer, ",{}", self.lineend));
                }
                TableEntry::Elem(ref elem) => {
                    try!(self.print_expr(elem));
                    try!(write!(self.writer, ",{}", self.lineend));
                }
            }
        }

        self.unindent();
        try!(self.print_indent());
        try!(write!(self.writer, "}}"));

        Ok(())
    }

    /// Prints the parameter list and the body of the given function
    fn print_funcbody(&mut self, f: &Function) -> io::Result<()> {
        try!(write!(self.writer, "("));
        for (idx, param) in f.params.iter().enumerate() {
            if idx != 0 {
                try!(write!(self.writer, ", "));
            }
            try!(write!(self.writer, "{}", param.value));
        }

        if f.varargs {
            if f.params.len() == 0 {
                try!(write!(self.writer, "..."));
            } else {
                try!(write!(self.writer, ", ..."));
            }
        }

        try!(write!(self.writer, "){}", self.lineend));
        // TODO Decide if the function requires multiple lines

        self.indent();
        for stmt in &f.body.stmts {
            try!(self.print_stmt(stmt));
        }
        self.unindent();

        try!(self.print_indent());
        try!(write!(self.writer, "end"));

        Ok(())
    }

    fn print_call_args(&mut self, args: &CallArgs) -> io::Result<()> {
        match *args {
            CallArgs::Normal(ref argv) => {
                try!(write!(self.writer, "("));

                for (i, arg) in argv.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(self.print_expr(arg));
                }

                try!(write!(self.writer, ")"));
            }
            CallArgs::String(ref s) => {
                try!(write!(self.writer, " "));
                try!(self.print_string(s.as_ref()));
            }
            CallArgs::Table(ref tbl) => {
                try!(self.print_table(tbl));
            }
        }

        Ok(())
    }

    fn print_call(&mut self, c: &Call) -> io::Result<()> {
        match *c {
            SimpleCall(ref callee, ref argv) => {
                try!(self.print_expr(callee));
                try!(self.print_call_args(argv));
            }
            MethodCall(ref callee, ref name, ref argv) => {
                try!(self.print_expr(callee));
                try!(write!(self.writer, ":{}", name.value));
                try!(self.print_call_args(argv));
            }
        }

        Ok(())
    }

    pub fn print_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        try!(self.print_indent());

        match stmt.value {
            SDecl(ref names, ref vals) => {
                try!(write!(self.writer, "local "));
                for (i, name) in names.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(write!(self.writer, "{}", name.value));
                }

                if vals.len() > 0 {
                    try!(write!(self.writer, " = "));

                    for (i, val) in vals.iter().enumerate() {
                        if i != 0 {
                            try!(write!(self.writer, ", "));
                        }
                        try!(self.print_expr(val));
                    }
                }
            }
            SAssign(ref vars, ref vals) => {
                for (i, var) in vars.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(self.print_var(var));
                }

                try!(write!(self.writer, " = "));

                for (i, val) in vals.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(self.print_expr(val));
                }
            }
            SDo(ref block) => {
                try!(write!(self.writer, "do{}", self.lineend));
                self.indent();
                try!(self.print_block(block));
                self.unindent();
                try!(write!(self.writer, "end{}", self.lineend));
            }
            SReturn(ref vals) => {
                try!(write!(self.writer, "return"));
                if vals.len() > 0 {
                    try!(write!(self.writer, " "));
                }

                for (i, val) in vals.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(self.print_expr(val));
                }
            }
            SCall(ref c) => {
                try!(self.print_call(c));
            }
            SFunc(ref var, ref f) => {
                try!(write!(self.writer, "function "));
                try!(self.print_var(var));
                try!(self.print_funcbody(f));
            }
            SMethod(ref var, ref name, ref f) => {
                try!(write!(self.writer, "function "));
                try!(self.print_var(var));
                try!(write!(self.writer, ":{}", name.value));
                try!(self.print_funcbody(f));
            }
            SLFunc(ref name, ref f) => {
                try!(write!(self.writer, "local function {}", name.value));
                try!(self.print_funcbody(f));
            }
            SIf {ref cond, ref body, ref el} => {
                try!(write!(self.writer, "if "));
                try!(self.print_expr(cond));
                try!(write!(self.writer, " then{}", self.lineend));

                self.indent();
                try!(self.print_block(body));
                self.unindent();

                // TODO handle elseif
                if el.stmts.len() > 0 {
                    try!(write!(self.writer, "else"));
                    self.indent();
                    for stmt in &el.stmts {
                        try!(self.print_stmt(stmt));
                    }
                    self.unindent();
                }

                try!(write!(self.writer, "end"));
            }
            SWhile {ref cond, ref body} => {
                try!(write!(self.writer, "while "));
                try!(self.print_expr(cond));
                try!(write!(self.writer, " do{}", self.lineend));
                self.indent();
                try!(self.print_block(body));
                self.unindent();
                try!(write!(self.writer, "end"));
            }
            SRepeat {ref abort_on, ref body} => {
                try!(write!(self.writer, "repeat{}", self.lineend));
                self.indent();
                try!(self.print_block(body));
                self.unindent();
                try!(write!(self.writer, "until "));
                try!(self.print_expr(abort_on));
            }
            SFor {ref var, ref start, ref step, ref end, ref body} => {
                try!(write!(self.writer, "for {} = ", var.value));
                try!(self.print_expr(start));
                try!(write!(self.writer, ", "));
                try!(self.print_expr(end));

                if let Some(ref step) = *step {
                    try!(write!(self.writer, ", "));
                    try!(self.print_expr(step));
                }

                try!(write!(self.writer, " do{}", self.lineend));
                self.indent();
                try!(self.print_block(body));
                self.unindent();
                try!(write!(self.writer, "end"));
            }
            SForIn {ref vars, ref iter, ref body} => {
                try!(write!(self.writer, "for "));

                for (i, var) in vars.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(write!(self.writer, "{}", var.value));
                }

                try!(write!(self.writer, " in "));
                for (i, val) in iter.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(self.print_expr(val));
                }

                try!(write!(self.writer, " do{}", self.lineend));
                self.indent();
                try!(self.print_block(body));
                self.unindent();
                try!(write!(self.writer, "end"));
            }
            SBreak => {
                try!(write!(self.writer, "break{}", self.lineend));
            }
        }

        try!(write!(self.writer, "{}", self.lineend));
        Ok(())
    }

    pub fn print_expr(&mut self, expr: &Expr) -> io::Result<()> {
        match expr.value {
            EBinOp(ref lhs, op, ref rhs) => {
                try!(self.print_expr(lhs));
                try!(write!(self.writer, " {} ", op));
                try!(self.print_expr(rhs));
            }
            EBraced(ref e) => {
                try!(write!(self.writer, "("));
                try!(self.print_expr(&**e));
                try!(write!(self.writer, ")"));
            }
            EUnOp(op, ref operand) => {
                match operand.value {
                    EBinOp(..) => {
                        try!(write!(self.writer, "{}(", op));
                        try!(self.print_expr(operand));
                        try!(write!(self.writer, ")"));
                    }
                    _ => {
                        try!(write!(self.writer, "{}", op));
                        try!(self.print_expr(operand));
                    }
                }
            }
            EVar(ref var) => {
                try!(self.print_var(var));
            }
            ECall(ref c) => {
                try!(self.print_call(c));
            }
            EFunc(ref f) => {
                try!(write!(self.writer, "function("));
                for (i, param) in f.params.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(write!(self.writer, "{}", param.value));
                }
                if f.varargs {
                    try!(write!(self.writer, "..."));
                }
                try!(write!(self.writer, "){}", self.lineend));
                // TODO Decide if the function requires multiple lines

                self.indent();
                try!(self.print_block(&f.body));
                self.unindent();

                try!(self.print_indent());
                try!(write!(self.writer, "end"));
            }
            ETable(ref pairs) => {
                try!(self.print_table(pairs));
            }
            EArray(ref exprs) => {
                try!(write!(self.writer, "["));

                for (i, expr) in exprs.iter().enumerate() {
                    if i != 0 {
                        try!(write!(self.writer, ", "));
                    }
                    try!(self.print_expr(expr));
                }

                try!(write!(self.writer, "]"));
            }
            EVarArgs => {
                try!(write!(self.writer, "..."));
            }
            ELit(ref lit) => {
                match *lit {
                    TInt(i) => try!(write!(self.writer, "{}", i)),
                    TFloat(f) => try!(write!(self.writer, "{}", f)),
                    TStr(ref s) => try!(self.print_string(s.as_ref())),
                    TBool(b) => try!(write!(self.writer, "{}", b)),
                    TNil => try!(write!(self.writer, "nil")),
                }
            }
        }

        Ok(())
    }

    fn print_var(&mut self, var: &Variable) -> io::Result<()> {
        match var.value {
            VNamed(s) => {
                try!(write!(self.writer, "{}", s));
            }
            VIndex(ref var, ref idx) => {
                try!(self.print_var(var));

                match *idx {
                    VarIndex::DotIndex(ref s) => {
                        try!(write!(self.writer, ".{}", s.value));
                    }
                    VarIndex::ExprIndex(ref expr) => {
                        try!(write!(self.writer, "["));
                        try!(self.print_expr(expr));
                        try!(write!(self.writer, "]"));
                    }
                }
            }
        }

        Ok(())
    }

    pub fn print_block(&mut self, block: &Block) -> io::Result<()> {
        for stmt in &block.stmts {
            try!(self.print_stmt(stmt));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::block;
    use parsetree::Block;

    fn print_block(block: &Block) -> String {
        let mut v = Vec::new();

        {
            let mut pp = PrettyPrinter::new(&mut v);
            pp.print_block(block).unwrap();
        }

        String::from_utf8(v).unwrap()
    }

    fn print(code: &str) -> String {
        print_block(&block(code).unwrap().into())
    }

    fn test(code: &str, expect: &str) {
        assert_eq!(print(&code[1..]), &expect[1..]);    // strip the \n at the start
    }

    /// Parses and prints the given source code. Then parses the print result and asserts that the
    /// two syntax trees are equal.
    fn test_auto(code: &str) {
        let expblock = block(code).unwrap().into();
        let printed = print_block(&expblock);
        println!("{}", printed);

        let newblock = block(printed.as_ref()).unwrap().into();

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
