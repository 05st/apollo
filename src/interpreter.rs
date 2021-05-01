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

    fn new_child(parent: Environment) -> Environment {
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
                    Operator::Subtract => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x - y)),
                            _ => Err(String::from("Invalid types for Subtract operator")),
                        }
                    },
                    Operator::Multiply => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x * y)),
                            _ => Err(String::from("Invalid types for Multiply operator")),
                        }
                    },
                    Operator::Divide => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x / y)),
                            _ => Err(String::from("Invalid types for Divide operator")),
                        }
                    },
                    Operator::Modulo => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x % y)),
                            _ => Err(String::from("Invalid types for Modulo operator")),
                        }
                    },
                    Operator::Exponent => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Number(x.powf(y))),
                            _ => Err(String::from("Invalid types for Exponent operator")),
                        }
                    },
                    Operator::Equal => Ok(Value::Bool(eleft == eright)),
                    Operator::NotEqual => Ok(Value::Bool(eleft != eright)),
                    Operator::Greater => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x > y)),
                            _ => Err(String::from("Invalid types for Greater operator")),
                        }                       
                    },
                    Operator::GreaterEqual => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x >= y)),
                            _ => Err(String::from("Invalid types for GreaterEqual operator")),
                        }                       
                    },
                    Operator::Lesser => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x < y)),
                            _ => Err(String::from("Invalid types for Lesser operator")),
                        }                       
                    },
                    Operator::LesserEqual => {
                        match (eleft, eright) {
                            (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(x <= y)),
                            _ => Err(String::from("Invalid types for LesserEqual operator")),
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
                    Operator::Not => {
                        match eval {
                            Value::Bool(x) => Ok(Value::Bool(!x)),
                            _ => Err(String::from("Invalid type for Not operator")),
                        }
                    },
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
                let prev = self.environment.clone();
                self.environment = Environment::new_child(prev); 
                for decl in decls {
                    let err = self.statement(decl);
                    if err != String::new() {
                        return err;
                    }
                }
                self.environment = *self.environment.parent.clone().unwrap();
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
            ASTNode::Write(expr) => {
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
