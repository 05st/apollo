use crate::parser::*;
use std::collections::HashMap;

pub struct Interpreter {
    table: HashMap<String, f64>,
    root: ASTNode,
}

impl Interpreter {
    fn eval_expression(&self, node: ASTNode) -> f64 {
        match node {
            ASTNode::Number(value) => value,
            ASTNode::Unary(operator, operand) => {
                let operand_res = self.eval_expression(*operand);
                match operator {
                    Operator::Subtract => -operand_res,
                    _ => operand_res,
                }
            },
            ASTNode::Binary(operator, left, right) => {
                let (left_res, right_res) = (self.eval_expression(*left), self.eval_expression(*right));
                match operator {
                    Operator::Add => left_res + right_res,
                    Operator::Subtract => left_res - right_res,
                    Operator::Multiply => left_res * right_res,
                    Operator::Divide => left_res / right_res,
                    Operator::Modulo => left_res % right_res,
                    Operator::Exponent => left_res.powf(right_res),
                }
            }
            ASTNode::Variable(id) => *self.table.get(&id).unwrap_or(&0f64),
            _ => 0f64,
        }
    }

    fn eval_compound(&mut self, node: ASTNode) {
        if let ASTNode::Compound(statements) = node {
            for statement in statements.iter() {
                let s = statement.clone();
                match s {
                    ASTNode::Assign(variable, expression) => {
                        let res = self.eval_expression(*expression);
                        if let ASTNode::Variable(id) = *variable {
                            if self.table.contains_key(&id) {
                                *self.table.get_mut(&id).unwrap() = res;
                            } else {
                                self.table.insert(id, res);
                            }
                        }
                    },
                    ASTNode::Compound(_) => self.eval_compound(s),
                    _ => (),
                }
            }
        }
    }

    pub fn interpret(&mut self) {
        self.eval_compound(self.root.clone());
        println!("{:?}", self.table);
    }

    pub fn new(root: ASTNode) -> Interpreter {
        Interpreter {
            table: HashMap::new(),
            root: root,
        }
    }
}
