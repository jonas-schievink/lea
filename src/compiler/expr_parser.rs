//! This module contains an AST visitor that turns all `ERawOp`s into `EBinOp` expression trees,
//! ensuring proper operator precedences are used.
//!
//! This makes a tree out of the "raw" expressions parsed by the generated parser.

use super::visit::*;
use super::ast::*;
use super::span::*;
use op::*;

#[derive(Copy, Clone)]
pub struct ExprParser;

/// Builds an `EBinOp` node and attaches the correct span. This requires that `lhs` and `rhs` are
/// in the right "order" concerning their spans.
fn mknode(lhs: Expr, op: BinOp, rhs: Expr) -> Expr {
    let start = lhs.span.start;
    let end = rhs.span.start + rhs.span.len;
    mkspanned(EBinOp(Box::new(lhs), op, Box::new(rhs)), start, end)
}

impl Transform for ExprParser {
    fn visit_expr(&mut self, mut expr: Expr) -> Expr {
        expr = match expr.value {
            ERawOp(left, rest) => {
                let mut operands: Vec<Expr> = Vec::new();
                let mut operators: Vec<BinOp> = Vec::new();

                operands.push(*left);

                for (op, rhs) in rest {
                    // While the operator on the stack has higher or equal precedence, pop it, pop
                    // 2 operands and replace the operands with a newly built operand node
                    while operators.len() > 0 {
                        let stackop = operators[operators.len() - 1];
                        let stackprec = stackop.get_precedence();
                        let opprec = op.get_precedence();
                        if opprec < stackprec || (opprec == stackprec && op.get_assoc() == Assoc::Left) {
                            operators.pop();
                            let rhs = operands.pop().unwrap();
                            let lhs = operands.pop().unwrap();

                            operands.push(mknode(lhs, stackop, rhs));
                        } else { break; }
                    }

                    // Always the new operator and operand on the stack
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

                operands.pop().unwrap()
            },
            _ => expr,
        };

        walk_expr(expr, self)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        // TODO
    }
}
