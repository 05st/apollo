use crate::parser::*;
use std::collections::HashMap;

use std::time::SystemTime;
use std::time::UNIX_EPOCH;

#[derive(Clone, PartialEq)]
enum FunctionType {
    User(ASTNode),
    BuiltIn(fn(Vec<Value>) -> RValue),
}

#[derive(Clone, PartialEq)]
enum Value {
    Number(f64),
    Bool(bool),
    Str(String),
    Function(Vec<String>, FunctionType),
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

#[derive(Clone, PartialEq)]
struct Environment {
    parent: Option<Box<Environment>>,
    values: HashMap<String, Value>,
}

pub struct Interpreter {
    environment: Environment,
}

fn number_op(op: Operator, a: Value, b: Value, f: fn(f64, f64) -> f64) -> RValue {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(f(x, y))),
        _ => Err(format!("Invalid types for {:?} operator, expected Numbers", op)),
    }
}

fn bool_op(op: Operator, a: Value, b: Value, f: fn(f64, f64) -> bool) -> RValue {
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

    fn get(&self, id: &str) -> RValue {
        if self.values.contains_key(id) {
            Ok(self.values.get(id).unwrap().clone())
        } else if let Option::Some(p) = &self.parent {
            p.get(id)
        } else {
            Err(format!("Undefined variable {}", id))
        }
 
   }

    fn set(&mut self, id: &str, val: Value) -> RValue {
        if self.values.contains_key(id) {
            *self.values.get_mut(id).unwrap() = val.clone();
            Ok(val)
        } else if let Option::Some(p) = &mut self.parent {
            p.set(id, val)
        } else {
            Err(format!("Undeclared variable {}", id))
        }
    }

    fn define(&mut self, id: String, val: Value) {
        self.values.insert(id, val);
    }
}

impl Interpreter {
    fn expression(&mut self, node: ASTNode) -> RValue {
        match node {
            ASTNode::Call(id, mut args) => {
                if let Value::Function(mut params, ftype) = self.environment.get(&id)? {
                    if args.len() == params.len() {
                        match ftype {
                            FunctionType::User(stmt) => {
                                let mut env = Environment::as_child(self.environment.clone());
                                for _ in 0..args.len() {
                                    env.define(params.remove(0), self.expression(args.remove(0))?);
                                }
                                let mut ret = Value::Null;
                                if let ASTNode::Block(decls) = stmt {
                                    match self.block(decls, env)? {
                                        Message::Return(v) => ret = v,
                                        _ => (),
                                    }
                                }
                                Ok(ret)
                            },
                            FunctionType::BuiltIn(f) => {
                                let mut eval_args = Vec::new();
                                for _ in 0..args.len() {
                                    eval_args.push(self.expression(args.remove(0))?);
                                }
                                Ok(f(eval_args)?)
                            },
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
                    Operator::Subtract => number_op(op, eleft, eright, |a, b| a - b),
                    Operator::Multiply => number_op(op, eleft, eright, |a, b| a * b),
                    Operator::Divide => number_op(op, eleft, eright, |a, b| a / b),
                    Operator::Modulo => number_op(op, eleft, eright, |a, b| a % b),
                    Operator::Exponent => number_op(op, eleft, eright, |a, b| a.powf(b)),
                    Operator::Equal => Ok(Value::Bool(eleft == eright)),
                    Operator::NotEqual => Ok(Value::Bool(eleft != eright)),
                    Operator::Greater => bool_op(op, eleft, eright, |a, b| a > b),
                    Operator::GreaterEqual => bool_op(op, eleft, eright, |a, b| a >= b),
                    Operator::Lesser => bool_op(op, eleft, eright, |a, b| a < b),
                    Operator::LesserEqual => bool_op(op, eleft, eright, |a, b| a <= b),
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

    fn block(&mut self, decls: Vec<ASTNode>, env: Environment) -> RMessage {
        self.environment = env;
        let mut msg = Message::None;
        for decl in decls {
            match self.statement(decl)? {
                Message::None => (),
                other => {
                    msg = other;
                    break;
                }
            }
        }
        self.environment = *(self.environment.clone().parent.unwrap());
        Ok(msg)
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
            ASTNode::Block(decls) => self.block(decls, Environment::as_child(self.environment.clone())),
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
                self.environment.define(id, Value::Function(params, FunctionType::User(*block)));
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

    pub fn interpret(&mut self, root: ASTNode) -> Result<(), String> {
        match self.statement(root) {
            Ok(_) => Ok(()),
            Err(msg) => Err(msg),
        }
    }

    pub fn new() -> Interpreter {
        let mut env = Environment::new();
        env.define("time".to_string(), Value::Function(vec![], FunctionType::BuiltIn(
            |_args| Ok(Value::Number(SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64()))
        )));
        env.define("floor".to_string(), Value::Function(vec!["x".to_string()], FunctionType::BuiltIn(
            |args| match &args[0] {
                Value::Number(x) => Ok(Value::Number(x.floor())),
                _ => Err("Invalid type for floor function".to_string()),
            }
        )));
        env.define("ceil".to_string(), Value::Function(vec!["x".to_string()], FunctionType::BuiltIn(
            |args| match &args[0] {
                Value::Number(x) => Ok(Value::Number(x.ceil())),
                _ => Err("Invalid type for ceil function".to_string()),
            }
        )));
        Interpreter {
            environment: env,
        }
    }
}
