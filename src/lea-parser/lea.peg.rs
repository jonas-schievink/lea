//! Parsing Expression Grammar for Lea
//!
//! rust-peg generates a parser from this definition at compile time

use std::mem;

use expr_parser::build_tree;
use span::{Span, Spanned, mkspanned};
use op::*;
use parsetree::*;

use lea_core::Const;


index -> VarIndex<'input>
    = __* "." __* id:ident      { VarIndex::DotIndex(id) }
    / __* "[" e:expression "]"  { VarIndex::ExprIndex(Box::new(e)) }

variable -> Variable<'input>
    = i:ident idxs:index* {
        let mut v = Spanned::new(i.span, VarKind::Named(i.value));
        for idx in idxs {
            let start = v.span.start;
            let end = match idx {
                VarIndex::ExprIndex(ref e) => e.span.start + e.span.len,
                VarIndex::DotIndex(ref id) => id.span.start + id.span.len,
            };

            v = mkspanned(VarKind::Indexed(Box::new(v), idx), start, end);
        }

        v
    }

/// Returns the expressions to pass as arguments to an invoked function
callargs -> CallArgs<'input>
    = __* "(" args:expression ** listsep __* ")"    { CallArgs::Normal(args) }
    / __* strn:string                               { CallArgs::String(strn) }
    / __* tbl:tablecons                             { CallArgs::Table(tbl) }

methodname -> Spanned<&'input str>
    = ":" __* name:ident __* { name }

callee -> Expr<'input>
    = v:variable            { mkspanned(ExprKind::Var(v), start_pos, pos) }
    / "(" e:expression ")"  { mkspanned(ExprKind::Braced(Box::new(e)), start_pos, pos) }

call -> Call<'input>
    = callee:callee name:methodname? args:callargs+ {
        let mut it = args.into_iter();
        let first_args = it.next().unwrap();
        let callee_span = callee.span.clone();
        let callee = Box::new(callee);
        let mut what = Spanned::new(callee_span, match name {
            None => Call::Normal(callee, first_args),
            Some(name) => Call::Method(callee, name, first_args),
        });

        for list in it {
            what = Spanned::new(callee_span,
                Call::Normal(Box::new(Spanned::new(what.span, ExprKind::Call(what.value))), list));
        }

        what.value
    }

atom_inner -> Expr<'input>
    = lit:literal           { Spanned::new(lit.span, ExprKind::Lit(lit.value)) }
    / op:unop a:atom        { mkspanned(ExprKind::UnOp(op, Box::new(a)), start_pos, pos) }
    / t:tablecons           { mkspanned(ExprKind::Table(t), start_pos, pos) }
    / a:arraycons           { mkspanned(ExprKind::Array(a), start_pos, pos) }
    / "function" f:funcbody { mkspanned(ExprKind::Func(f), start_pos, pos) }
    / "..."                 { mkspanned(ExprKind::VarArgs, start_pos, pos) }
    / c:call                { mkspanned(ExprKind::Call(c), start_pos, pos) }
    / callee

// Atomic expression. Either a literal, a unary operator applied to another atom or a full expr
// inside parentheses.
atom -> Expr<'input>
    = __* a:atom_inner __* { a }

tableentry -> TableEntry<'input>
    = __* k:ident __* "=" v:expression              { TableEntry::IdentPair(k, v) }
    / __* "[" k:expression "]" __* "=" v:expression { TableEntry::Pair(k, v) }
    / e:expression                                  { TableEntry::Elem(e) }

tablecons -> TableCons<'input>
    = "{" pairs:tableentry ++ listsep listsep? "}"  { pairs }
    / "{" __* "}"                                   { Vec::new() }

arraycons -> Vec<Expr<'input>>
    = "[" vals:expression_list listsep? "]" { vals }
    / "[" __* "]"                           { Vec::new() }

#[pub]
expression -> Expr<'input>
    = __* left:atom rest:(op:binop r:atom { (op, r) })* __* {
        build_tree(left, rest)
    }

// Comma-separated list of expressions (at least one expression is required)
expression_list -> Vec<Expr<'input>>
    = e:expression ++ listsep { e }

elseif -> Spanned<(Expr<'input>, Block<'input>)>
    = "elseif" e:expression "then" __+ body:block { mkspanned((e, body), start_pos, pos) }

stmt_if -> Stmt<'input>
    = "if" cond:expression "then" __+ body:block elifs:elseif* el:("else" __+ block)? "end" {
        mkspanned(StmtKind::If {
            cond: cond,
            body: body,
            elifs: elifs,
            el: el,
        }, start_pos, pos)
    }

// List of comma-separated identifiers with at least one element
identlist -> Vec<Spanned<&'input str>>
    = ids:ident ++ listsep { ids }

varlist -> Vec<Variable<'input>>
    = vars:variable ++ listsep { vars }

// Returns the parameter list and a boolean indicating whether the function takes variable args
funcparams -> (Vec<Spanned<&'input str>>, bool)
    = "(" __* params:identlist va:(__* ',' __* "...")? __* ")" {
        (params, va.is_some())
    }
    / "(" __* "..." __* ")" { (Vec::new(), true) }
    / "(" __* ")" { (Vec::new(), false) }

funcbody -> Function<'input>
    = __* params:funcparams __* body:block "end" {
        let (pars, varargs) = params;
        Function::<'input>::new(pars, varargs, body)
    }

stmt_inner -> Stmt<'input>
    = stmt_if
    / "while" cond:expression "do" __+ body:block "end" {
        mkspanned(StmtKind::While { cond: cond, body: body }, start_pos, pos)
    }
    / "repeat" __+ body:block "until" cond:expression {
        mkspanned(StmtKind::Repeat { abort_on: cond, body: body }, start_pos, pos)
    }
    / "for" __+ vars:identlist __+ "in" it:expression_list "do" __+ body:block "end" {
        mkspanned(StmtKind::ForIn {
            vars: vars,
            iter: it,
            body: body,
        }, start_pos, pos)
    }
    / "for" __+ var:ident __* "=" start:expression "," end:expression step:("," expression)? "do" __+ body:block "end" {
        mkspanned(StmtKind::For {
            var: var,
            start: start,
            step: step,
            end: end,
            body: body,
        }, start_pos, pos)
    }
    / "function" __+ var:variable method:methodname? f:funcbody {
        mkspanned(match method {
            None => StmtKind::Func(var, f),
            Some(method) => StmtKind::Method(var, method, f),
        }, start_pos, pos)
    }
    / "local" __+ "function" __+ name:ident f:funcbody {
        mkspanned(StmtKind::LocalFunc(name, f), start_pos, pos)
    }
    / "do" __+ b:block "end" { mkspanned(StmtKind::Do(b), start_pos, pos) }
    / "break" { mkspanned(StmtKind::Break, start_pos, pos) }
    / ";" { mkspanned(StmtKind::Semi, start_pos, pos) }
    / "return" vals:expression_list { mkspanned(StmtKind::Return(vals), start_pos, pos) }
    / "return" { mkspanned(StmtKind::Return(vec![]), start_pos, pos) }
    / "local" __+ locals:identlist __* "=" exprs:expression_list {
        mkspanned(StmtKind::Decl(locals, exprs), start_pos, pos)
    }
    / "local" __+ locals:identlist { mkspanned(StmtKind::Decl(locals, vec![]), start_pos, pos) }
    / c:call { mkspanned(StmtKind::Call(c), start_pos, pos) }
    / vars:varlist __* "=" vals:expression_list {
        mkspanned(StmtKind::Assign(vars, vals), start_pos, pos)
    }

#[pub]
statement -> Stmt<'input>
    = __* s:stmt_inner __* { s }

#[pub]
block -> Block<'input>
    = __* s:statement* {
        Block {
            stmts: s,
            span: Span::new(start_pos, pos),
        }
    }


/// Lexical elements


/// Parses Lua's "long bracket" and returns the content. A long bracket is opened with '[',
/// followed by any number of '=' (including 0), followed by '[' and is closed with ']', followed
/// by the same number of '=' used in the opening bracket, followed by ']'.
longbracket -> &'input str
    = "[" open:"="* "[" ( !("]" close:"="* "]" {?
        if open.len() == close.len() { Ok(()) } else { Err("") }
    }) . )* "]" "="* "]" { &match_str[open.len()+2 .. match_str.len() - open.len() - 2] }

comment -> ()
    = "--" longbracket { () }
    / "--" [^\n]*

real_whitespace = " " / "\t" / "\n" / "\r"

__ = real_whitespace / comment

#[pub]
ident -> Spanned<&'input str>
    = [_a-zA-Z][_a-zA-Z0-9]* { mkspanned(match_str, start_pos, pos) }

decstr -> &'input str
    = [0-9]+ { match_str }

hexdigit -> char
    = [0-9a-fA-F] { match_str.chars().next().unwrap() }

hexstr -> &'input str
    = [0-0a-fA-F]+ { match_str }

octstr -> &'input str
    = [0-7]+ { match_str }

binstr -> &'input str
    = [01]+ { match_str }

// integer without preceding sign
uinteger -> i64
    = "0x" hex:hexstr   {? i64::from_str_radix(hex, 16).map_err(|_| "hex string") }
    / "0o" oct:octstr   {? i64::from_str_radix(oct, 8).map_err(|_| "oct string") }
    / "0b" bin:binstr   {? i64::from_str_radix(bin, 2).map_err(|_| "bin string") }
    / [0-9]+            {? match_str.parse().map_err(|_| "integer literal") }

integer -> i64
    = minus:"-"? i:uinteger {
        match minus {
            None => i,
            Some(_) => -i,
        }
    }

/// C-like float (only one part before/after the decimal point is required)
cfloat -> &'input str
    = [0-9]* "." decstr { match_str }
    / decstr "." ! "."  { match_str }   // no second "." allowed, since that's the concat operator

/// Float in exponential notation
efloat -> &'input str
    = ( cfloat / decstr ) "e" [+-]? decstr { match_str }

float -> f64
    = "-"? ( efloat / cfloat ) { match_str.parse().unwrap() }

/// Matches the decimal notation of a single byte: 0-255
/// (leading '0's are ignored, but bytes are limited to at most 3 digits, so "0001" for example
/// will return 0 here and parse the "000", but stop at the "1")
dec_byte -> u8
    = [0-9]{,3} {?
        match match_str.parse() {
            Ok(num) => Ok(num),
            Err(_) => Err("decimal byte"),
        }
    }

esc_seq -> char
    = "\\a"     { '\x07' }  // bell
    / "\\b"     { '\x08' }  // backspace
    / "\\f"     { '\x0c' }  // form feed
    / "\\n"     { '\n' }
    / "\\r"     { '\r' }
    / "\\t"     { '\t' }
    / "\\v"     { '\x0b' }  // vertical tab
    / "\\\\"    { '\\' }
    / "\\\""    { '\"' }
    / "\\'"     { '\'' }
    / "\\\n"    { '\n' }    // '\' at the end of a line continues the string and embeds a "\n"
    / "\\\r\n"  { '\n' }    // There's a bug in Windows where a newline is preceded by a "\r"
    / "\\x" digits:hexdigit{2} {
        let string: String = digits.into_iter().collect();
        let i = u8::from_str_radix(&string, 16).unwrap();

        i as char
    }
    / "\\0"     { 0 as char }
    / "\\" val:dec_byte { val as char }

/// \z will skip all whitespace that follows it. Useful for breaking string literals into multiple
/// lines: "bla\z
///         bla" == "blabla"
blank_esc -> ()
    = "\\z" real_whitespace*

_double_quote_content -> char
    = esc_seq
    / blank_esc _double_quote_content
    / [^\\"\n]  { match_str.chars().next().unwrap() }   //" to fix syntax highlighting

_single_quote_content -> char
    = esc_seq
    / blank_esc _single_quote_content
    / [^\\'\n]  { match_str.chars().next().unwrap() }

#[pub]
string -> String
    = '"' chars:_double_quote_content* blank_esc? '"'   { chars.into_iter().collect() }
    / '\'' chars:_single_quote_content* blank_esc? '\'' { chars.into_iter().collect() }
    / content:longbracket                               { content.to_owned() }

boolean -> bool
    = "true" { true } / "false" { false }

_literal -> Const
    = f:float   { Const::Number(f.into()) }
    / i:integer { Const::Number(i.into()) }
    / s:string  { Const::Str(s) }
    / b:boolean { Const::Bool(b) }
    / "nil"     { Const::Nil }

#[pub]
literal -> Spanned<Const>
    = l:_literal { mkspanned(l, start_pos, pos) }

binop -> BinOp
    = "==" {BinOp::Eq} / "!=" {BinOp::NEq} / "~=" {BinOp::NEqLua} / ">=" {BinOp::GEq}
    / "<=" {BinOp::LEq} / ">>" {BinOp::ShiftR} / "<<" {BinOp::ShiftL} / "<" {BinOp::Less}
    / ">" {BinOp::Greater} / "&&" {BinOp::LAnd} / "and" {BinOp::LAndLua} / "||" {BinOp::LOr}
    / "or" {BinOp::LOrLua} / "&" {BinOp::BAnd} / "~" {BinOp::BXor} / "|" {BinOp::BOr}
    / "+" {BinOp::Add} / "-" {BinOp::Sub} / "*" {BinOp::Mul} / "/" {BinOp::Div} / "%" {BinOp::Mod}
    / "^" {BinOp::Pow} / ".." {BinOp::Concat}

unop -> UnOp
    = "-" {UnOp::Negate} / "!" {UnOp::LNot} / "not" {UnOp::LNotLua} / "~" {UnOp::BNot}
    / "#" {UnOp::Len}

listsep = __* ',' __*
