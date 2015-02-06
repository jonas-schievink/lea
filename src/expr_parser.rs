//! This module contains an AST visitor that turns all "ERawBinOp"s into "EBinOp"s, ensuring
//! proper operator precedences are used.
//!
//! This makes a tree out of the "raw" expressions parsed by the generated parser.

use visit::*;
use ast::{Expr, BinOp};
use ast::Expr::*;

#[derive(Copy)]
pub struct ExprParser;

impl Visitor for ExprParser {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match *expr {
            ERawOp(left, rest) => {
                println!("got raw op: {:?} | {:?}", left, rest);

                // Build a tree out of the expression
                let mut operands: Vec<Expr> = Vec::new();
                let mut operators: Vec<BinOp> = Vec::new();

                operands.push(**left);
                for pair in rest {
                    let (op, rhs) = *pair;

                    // If the operator on the stack has higher or equal precedence, pop it, pop 2
                    // operands and build a new operand node
                    let stackop = operators[-1];

                    if stackop.get_precedence() >= op.get_precedence() {
                        operators.pop();
                        let (lhs, rhs) = (operands.pop().unwrap(), operands.pop().unwrap());

                        let expr = EBinOp(Box::new(lhs), stackop, Box::new(rhs));
                        println!("l>=r | built node: {:?}", &expr);
                    } else {
                        // Put the new operator and operand on the stack
                        operators.push(op);
                        operands.push(rhs);
                    }
                }

                // Pop all operators and build nodes
            },
            _ => { walk_expr(expr, self) },
        }
    }
}
