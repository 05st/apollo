use crate::parser::*;
use std::collections::HashMap;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Str(String),
    Function(Vec<String>, ASTNode),
    Null,
}

enum Message {
    Break,
    Continue,
    Return(Value),
    None,
}

type RValue = Result<Value, String>;
type RMessage = Result<Message, String>;

#[derive(Clone)]
struct Environment {
    parent: Option<Box<Environment>>,
    values: HashMap<String, Value>,
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
        Value::Function(_, _) => true,
        Value::Null => false,
    }
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

    fn get(&self, id: &String) -> RValue {
        if self.values.contains_key(id) {
            Ok(self.values.get(id).unwrap().clone())
        } else {
            if let Option::Some(p) = &self.parent {
                p.get(id)
            } else {
                Err(format!("Undefined variable {}", id))
            }
        }
 
   }

    fn set(&mut self, id: &String, val: Value) -> RValue {
        if self.values.contains_key(id) {
            *self.values.get_mut(id).unwrap() = val.clone();
            Ok(val)
        } else {
            if let Option::Some(p) = &mut self.parent {
                p.set(id, val)
            } else{
                Err(format!("Undeclared variable {}", id))
            }
        }
    }

    fn define(&mut self, id: String, val: Value) {
        if self.values.contains_key(&id) {
            self.set(&id, val).unwrap();
        } else {
            self.values.insert(id, val.clone());
        }
    }
}

impl Interpreter {
    fn expression(&mut self, node: ASTNode) -> RValue {
        match node {
            ASTNode::Call(id, mut args) => {
                if let Value::Function(mut params, stmt) = self.environment.get(&id)? {
                    if args.len() == params.len() {
                        let mut env = Environment::as_child(self.environment.clone());
                        for i in 0..args.len() {
                            env.define(params.remove(i), self.expression(args.remove(i))?);
                        }
                        self.environment = env;
                        let msg = self.statement(stmt)?;
                        self.environment = *self.environment.parent.clone().unwrap();
                        match msg {
                            Message::Return(v) => Ok(v),
                            _ => Ok(Value::Null)
                        }
                    } else {
                        Err(format!("Invalid number of arguments when calling {}", id))
                    }
                } else {
                    Err(format!("Undefined function {}", id))
                }
            },
            ASTNode::Assign(id, val) => {
                let eval = self.expression(*val)?;
                self.environment.set(&id, eval)
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
            ASTNode::Variable(id) => self.environment.get(&id),
            ASTNode::Number(x) => Ok(Value::Number(x)),
            ASTNode::Bool(x) => Ok(Value::Bool(x)),
            ASTNode::Str(x) => Ok(Value::Str(x)),
            ASTNode::Null => Ok(Value::Null),
            _ => Err(format!("Invalid expression {:?}", node)),
        }
    }

    fn statement(&mut self, node: ASTNode) -> RMessage {
        match node {
            ASTNode::Compound(decls) => {
                for decl in decls {
                    match self.statement(decl)? {
                        Message::None => (),
                        msg => return Ok(msg),
                    };
                }
                Ok(Message::None)
            },
            ASTNode::Block(decls) => {
                self.environment = Environment::as_child(self.environment.clone());
                for decl in decls {
                    match self.statement(decl)? {
                        Message::None => (),
                        msg => return Ok(msg),
                    }
                }
                self.environment = *self.environment.parent.clone().unwrap();
                Ok(Message::None)
            },
            ASTNode::If(cond, stmt1, stmt2) => {
                if is_truthy(&self.expression(*cond)?) {
                    Ok(self.statement(*stmt1)?)
                } else if let Some(stmt2) = *stmt2 {
                    Ok(self.statement(stmt2)?)
                } else {
                    Ok(Message::None)
                }
            },
            ASTNode::While(cond, stmt) => {
                while is_truthy(&self.expression(*cond.clone())?) {
                    match self.statement(*stmt.clone())? {
                        Message::None => (),
                        Message::Break => break,
                        Message::Continue => continue,
                        Message::Return(v) => return Ok(Message::Return(v)),
                    };
                }
                Ok(Message::None)
            },
            ASTNode::VarDecl(id, expr) => {
                let eval = self.expression((*expr).unwrap_or(ASTNode::Null))?;
                self.environment.define(id, eval);
                Ok(Message::None)
            },
            ASTNode::Function(id, params, block) => {
                self.environment.define(id, Value::Function(params, *block));
                Ok(Message::None)
            },
            ASTNode::ExprStmt(expr) => {
                self.expression(*expr)?;
                Ok(Message::None)
            },
            ASTNode::Print(expr) => {
                match self.expression(*expr)? {
                    Value::Number(x) => println!("{}", x),
                    Value::Bool(x) => println!("{}", x),
                    Value::Str(x) => println!("{}", x),
                    Value::Function(_, _) => println!("function"),
                    Value::Null => println!("null"),
                };
                Ok(Message::None)
            },
            ASTNode::Break => Ok(Message::Break),
            ASTNode::Continue => Ok(Message::Continue),
            ASTNode::Return(expr) => Ok(Message::Return(self.expression(*expr)?)),
            _ => Err(format!("Invalid statement {:?}", node)),
        }
    }

    pub fn interpret(&mut self, root: ASTNode) {
        if let Err(m) = self.statement(root) {
            println!("{}", m);
        }
    }

    pub fn new() -> Interpreter {
        let mut env = Environment::new();
        env.define("test".to_string(), Value::Function(vec!["out".to_string()], ASTNode::Print(Box::new(ASTNode::Variable("out".to_string())))));
        Interpreter {
            environment: env,
        }
    }
}
