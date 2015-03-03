//! This module contains an AST visitor that turns all `ERawOp`s into `EBinOp` expression trees,
//! ensuring proper operator precedences are used.
//!
//! This makes a tree out of the "raw" expressions parsed by the generated parser.

use std::mem;

use super::visit::*;
use super::ast::*;
use super::span::*;
use op::*;

#[derive(Copy)]
pub struct ExprParser;

/// Builds a `EBinOp` node and attaches the correct span. This requires that `lhs` and `rhs` are in
/// the right "order" concerning their spans.
fn mknode(lhs: Expr, op: BinOp, rhs: Expr) -> Expr {
    let start = lhs.span.start;
    let end = rhs.span.start + rhs.span.len;
    mkspanned(EBinOp(Box::new(lhs), op, Box::new(rhs)), start, end)
}

impl Visitor for ExprParser {
    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr.value {
            ERawOp(..) => {
                let mut operands: Vec<Expr> = Vec::new();
                let mut operators: Vec<BinOp> = Vec::new();
                let mut rest = Vec::new();

                if let ERawOp(ref mut left, ref mut old_rest) = expr.value {
                    // take ownership of `left` and `rest` by replacing with an empty Vec<> (saves
                    // us from copying the content). We replace the whole expr anyways, so it
                    // doesn't matter.
                    operands.push(mem::replace(left, mkspanned(EVarArgs, 0, 0)));
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

                            operands.push(mknode(lhs, stackop, rhs));
                        }
                    }

                    // Can't build node, put the new operator and operand on the stack
                    operators.push(op);
                    operands.push(rhs);
                }

                // Pop all operators and build nodes
                while operators.len() > 0 {
                    let op = operators.pop().unwrap();
                    let rhs = operands.pop().unwrap();
                    let lhs = operands.pop().unwrap();

                    // PUSH UNTIL THERE IS NOTHING LEFT TO PUSH
                    operands.push(mknode(lhs, op, rhs));
                }

                // If this algorithm works correctly, this always holds, since the input data can
                // always be turned into a tree (since there is always one operand and any number
                // of operator-operand pairs)
                assert_eq!(operands.len(), 1);

                mem::replace(expr, operands.pop().unwrap());
            },
            _ => {},
        }

        walk_expr(expr, self);
    }
}
