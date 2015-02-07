//! This module contains an AST visitor that turns all "ERawOp"s into "proper" expression nodes,
//! ensuring proper operator precedences are used.
//!
//! This makes a tree out of the "raw" expressions parsed by the generated parser.

use std::mem;

use visit::*;
use ast::{Expr, BinOp};
use ast::Expr::*;

#[derive(Copy)]
pub struct ExprParser;

impl Visitor for ExprParser {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match *expr {
            ERawOp(..) => {
                // Build a tree out of the expression
                let mut operands: Vec<Expr> = Vec::new();
                let mut operators: Vec<BinOp> = Vec::new();
                let mut rest = Vec::new();

                if let ERawOp(ref mut left, ref mut old_rest) = *expr {
                    println!("got raw op: {:?} | {:?}", left, old_rest);

                    // take ownership of `left` and `rest` by replacing with an empty Vec<> (saves us
                    // from copying the content). We replace the whole expr anyways, so it doesn't
                    // matter.
                    operands.push(mem::replace(left, EVarArgs));
                    mem::swap(old_rest, &mut rest);
                } else { unreachable!(); }

                for pair in rest {
                    let (op, rhs) = pair;

                    // If the operator on the stack has higher or equal precedence, pop it, pop 2
                    // operands and build a new operand node
                    if operators.len() > 0 {
                        let stackop = operators[operators.len() - 1];
                        if stackop.get_precedence() >= op.get_precedence() {
                            operators.pop();
                            let rhs = operands.pop().unwrap();
                            let lhs = operands.pop().unwrap();

                            let expr = EBinOp(Box::new(lhs), stackop, Box::new(rhs));
                            println!("l>=r | built node: {:?}", &expr);
                            operands.push(expr);
                        }
                    }

                    // Can't build node, put the new operator and operand on the stack
                    operators.push(op);
                    operands.push(rhs);
                }

                // Pop all operators and build nodes until no operators left
                while operators.len() > 0 {
                    let op = operators.pop().unwrap();
                    let rhs = operands.pop().unwrap();
                    let lhs = operands.pop().unwrap();

                    // PUSH UNTIL THERE IS NOTHING LEFT TO PUSH
                    operands.push(EBinOp(Box::new(lhs), op, Box::new(rhs)));
                }

                // If this algorithm works correctly, this always holds, since the input data can
                // always be turned into a tree (since there is always one operand and any number
                // of operator-operand pairs)
                assert_eq!(operands.len(), 1);

                println!("result | {:?}", &operands[0]);

                mem::replace(expr, operands.pop().unwrap());
            },
            _ => {},
        }

        // If this expr contains any subexpressions, convert them as well
        walk_expr(expr, self);
    }
}
