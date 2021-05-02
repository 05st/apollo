use crate::parser::*;
use std::collections::HashMap;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Str(String),
    Null,
}

type RValue = Result<Value, String>;

#[derive(Clone)]
struct Environment {
    parent: Option<Box<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            parent: Option::None,
            values: HashMap::new(),
        }
    }

    fn as_child(parent: Environment) -> Environment {
        Environment {
            parent: Option::Some(Box::new(parent)),
            values: HashMap::new(),
        }
    }

    fn get(&self, id: String) -> RValue {
        if self.values.contains_key(&id) {
            Ok(self.values.get(&id).unwrap().clone())
        } else {
            if let Option::Some(p) = &self.parent {
                p.get(id)
            } else {
                Err(format!("Undefined variable {}", id))
            }
        }
    }

    fn set(&mut self, id: String, val: Value) -> RValue {
        if self.values.contains_key(&id) {
            *self.values.get_mut(&id).unwrap() = val.clone();
            Ok(val)
        } else {
            if let Option::Some(p) = &mut self.parent {
                p.set(id, val)
            } else{
                Err(format!("Undeclared variable {}", id))
            }
        }
    }

    fn insert(&mut self, id: String, val: Value) {
        if self.values.contains_key(&id) {
            self.set(id, val).unwrap();
        } else {
            self.values.insert(id, val.clone());
        }
    }
}

pub struct Interpreter {
    environment: Environment,
}

fn number_op(op: Operator, a: Value, b: Value, f: Box<dyn Fn(f64, f64) -> f64>) -> RValue {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(f(x, y))),
        _ => Err(format!("Invalid types for {:?} operator, expected Numbers", op)),
    }
}

fn bool_op(op: Operator, a: Value, b: Value, f: Box<dyn Fn(f64, f64) -> bool>) -> RValue {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(f(x, y))),
        _ => Err(format!("Invalid types for {:?} operator, expected Numbers", op)),
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Number(x) => (x > &0f64),
        Value::Bool(x) => (x == &true),
        Value::Str(_) => true,
        Value::Null => false,
    }
}

impl Interpreter {
    fn expression(&mut self, node: ASTNode) -> RValue {
        match node {
            ASTNode::Assign(id, val) => {
                let eval = self.expression(*val)?;
                self.environment.set(id, eval)
            },
            ASTNode::Binary(op, left, right) => { // Eventually rewrite
                let (eleft, eright) = (self.expression(*left)?, self.expression(*right)?);
                match op {
                    Operator::Add => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x + y)),
                            (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
                            _ => Err(String::from("Invalid types for Add operator")),
                        }
                    },
                    Operator::Subtract => number_op(op, eleft, eright, Box::new(|a, b| a - b)),
                    Operator::Multiply => number_op(op, eleft, eright, Box::new(|a, b| a * b)),
                    Operator::Divide => number_op(op, eleft, eright, Box::new(|a, b| a / b)),
                    Operator::Modulo => number_op(op, eleft, eright, Box::new(|a, b| a % b)),
                    Operator::Exponent => number_op(op, eleft, eright, Box::new(|a, b| a.powf(b))),
                    Operator::Equal => Ok(Value::Bool(eleft == eright)),
                    Operator::NotEqual => Ok(Value::Bool(eleft != eright)),
                    Operator::Greater => bool_op(op, eleft, eright, Box::new(|a, b| a > b)),
                    Operator::GreaterEqual => bool_op(op, eleft, eright, Box::new(|a, b| a >= b)),
                    Operator::Lesser => bool_op(op, eleft, eright, Box::new(|a, b| a < b)),
                    Operator::LesserEqual => bool_op(op, eleft, eright, Box::new(|a, b| a <= b)),
                    Operator::LogicOr => {
                        if is_truthy(&eleft) {
                            Ok(eleft)
                        } else {
                            Ok(eright)
                        }
                    },
                    Operator::LogicAnd => {
                        if !is_truthy(&eleft) {
                            Ok(eleft)
                        } else {
                            Ok(eright)
                        }
                    },
                    _ => Err(format!("Invalid binary operator {:?}", op)),
                }
            },
            ASTNode::Unary(op, val) => {
                let eval = self.expression(*val)?;
                match op {
                    Operator::Subtract => {
                        match eval {
                            Value::Number(x) => Ok(Value::Number(-x)),
                            _ => Err(String::from("Invalid type for Negation operator")),
                        }
                    },
                    Operator::Not => Ok(Value::Bool(!is_truthy(&eval))),
                    _ => Err(format!("Invalid unary operator {:?}", op)),
                }
            },
            ASTNode::Variable(id) => self.environment.get(id),
            ASTNode::Number(x) => Ok(Value::Number(x)),
            ASTNode::Bool(x) => Ok(Value::Bool(x)),
            ASTNode::Str(x) => Ok(Value::Str(x)),
            ASTNode::Null => Ok(Value::Null),
            _ => Err(format!("Invalid expression {:?}", node)),
        }
    }

    fn statement(&mut self, node: ASTNode) -> String {
        match node {
            ASTNode::Compound(decls) => {
                for decl in decls {
                    let err = self.statement(decl);
                    if err != String::new() {
                        return err;
                    }
                }
            },
            ASTNode::Block(decls) => {
                self.environment = Environment::as_child(self.environment.clone()); 
                for decl in decls {
                    let err = self.statement(decl);
                    if err != String::new() {
                        return err;
                    }
                }
                self.environment = *self.environment.parent.clone().unwrap();
            },
            ASTNode::If(cond, stmt1, stmt2) => {
                let econd = match self.expression(*cond) {
                    Ok(x) => x,
                    Err(m) => return m,
                };
                if is_truthy(&econd) {
                    self.statement(*stmt1);
                } else if let Some(s) = *stmt2 {
                    self.statement(s);
                }
            },
            ASTNode::While(cond, stmt) => {
                while is_truthy(&match self.expression(*cond.clone()) {
                    Ok(x) => x,
                    Err(m) => return m,
                }) {
                    self.statement(*stmt.clone());
                }
            },
            ASTNode::VarDecl(id, val) => {
                let eval = match *val {
                    Some(expr) => {
                        match self.expression(expr) {
                            Ok(x) => x,
                            Err(m) => return m,
                        }
                    },
                    None => Value::Null,
                };
                self.environment.insert(id, eval);
            },
            ASTNode::ExprStmt(expr) => {
                match self.expression(*expr) {
                    Ok(_) => (),
                    Err(m) => return m,
                }
            }
            ASTNode::Print(expr) => {
                let eval = match self.expression(*expr) {
                    Ok(x) => x,
                    Err(m) => return m,
                };
                match eval {
                    Value::Number(x) => println!("{}", x),
                    Value::Bool(x) => println!("{}", x),
                    Value::Str(x) => println!("{}", x),
                    Value::Null => println!("null"),
                }
            },
            _ => return String::from("Invalid statement"),
        }
        String::new()
    }

    pub fn interpret(&mut self, root: ASTNode) -> String {
        self.statement(root)
    }

    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new(),
        }
    }
}
