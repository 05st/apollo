use crate::parser::*;

use std::{
    collections::HashMap,
    cell::RefCell,
    rc::Rc,
};

#[derive(Debug, PartialEq, Clone)]
enum FunctionType {
    User(Vec<String>, ASTNode, EnvRef),
    Native(usize, fn(Vec<Value>) -> RValue),
}

type RValue = Result<Value, String>;
#[derive(Debug, PartialEq, Clone)]
enum Value {
    Number(f64),
    Bool(bool),
    String(String),
    Function(FunctionType),
    Null,
}

type RMessage = Result<Message, String>;
#[derive(Debug)]
enum Message {
    None,
    Break,
    Continue,
    Return(Value),
}

type EnvRef = Rc<RefCell<Environment>>;
#[derive(Debug, Clone, PartialEq)]
struct Environment {
    values: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

pub struct Interpreter {
    environment: EnvRef,
}

impl Environment {
    fn get(&self, id: &str) -> RValue {
        match self.values.get(id) {
            Some(value) => Ok(value.clone()),
            None => {
                if let Some(parent) = &self.parent {
                    parent.borrow().get(id)
                } else {
                    Err(format!("Attempt to get '{}' which is not defined", id))
                }
            }
        }
    }

    fn set(&mut self, id: &str, val: Value) -> RValue {
        match self.values.get_mut(id) {
            Some(value) => {
                *value = val.clone();
                Ok(val)
            },
            None => {
                if let Some(parent) = &self.parent {
                    Rc::clone(&parent).borrow_mut().set(id ,val)
                } else {
                    Err(format!("Attempt to set '{}' which is not declared", id))
                }
            }
        }
    }

    fn define(&mut self, id: &str, val: Value) {
        self.values.insert(id.to_string(), val);
    }

    fn new(parent: Option<EnvRef>) -> Environment {
        Environment {
            values: HashMap::new(),
            parent,
        }
    }
}

fn number_op(op: Operator, left: Value, right: Value, func: fn(f64, f64) -> f64) -> RValue {
    match (left.clone(), right.clone()) {
        (Value::Number(x), Value::Number(y)) => Ok(Value::Number(func(x, y))),
        _ => Err(format!("Invalid types for binary {:?}: {:?}, {:?}", op, left, right)),
    }
}
fn bool_op(op: Operator, left: Value, right: Value, func: fn(f64, f64) -> bool) -> RValue {
    match (left.clone(), right.clone()) {
        (Value::Number(x), Value::Number(y)) => Ok(Value::Bool(func(x, y))),
        _ => Err(format!("Invalid types for binary {:?}: {:?}, {:?}", op, left, right)),
    }
}

fn is_truthy(val: &Value) -> bool {
    match val {
        Value::Number(n) => *n > 0f64,
        Value::Bool(b) => *b,
        Value::String(_) => true,
        Value::Function(_) => true,
        Value::Null => false,
    }
}

impl Interpreter {
    fn expression(&mut self, node: ASTNode) -> RValue {
        match node {
            ASTNode::Number(val) => Ok(Value::Number(val)),
            ASTNode::Bool(val) => Ok(Value::Bool(val)),
            ASTNode::String(val) => Ok(Value::String(val)),
            ASTNode::Null => Ok(Value::Null),
            ASTNode::Variable(id) => self.environment.clone().borrow().get(&id),
            ASTNode::Assign(id, expr) => {
                let eval = self.expression(*expr)?;
                self.environment.clone().borrow_mut().set(&id, eval)
            },
            ASTNode::Unary(op, expr) => {
                match op {
                    Operator::Subtract => {
                        match self.expression(*expr)? {
                            Value::Number(val) => Ok(Value::Number(-val)),
                            other => Err(format!("Invalid type for unary {:?}: {:?}", op, other)),
                        }
                    }
                    _ => Err(format!("Invalid unary operator {:?}", op)),
                }
            },
            ASTNode::Binary(op, left_expr, right_expr) => {
                let (left, right) = (self.expression(*left_expr)?, self.expression(*right_expr)?);
                match op {
                    Operator::Add => {
                        match (left.clone(), right.clone()) {
                            (Value::Number(val1), Value::Number(val2)) => Ok(Value::Number(val1 + val2)),
                            (Value::String(val1), Value::String(val2)) => Ok(Value::String(format!("{}{}", val1, val2))),
                            _ => Err(format!("Invalid types for binary Add: {:?}, {:?}", left, right)),
                        }
                    },
                    Operator::Subtract => number_op(op, left, right, |a, b| a - b),
                    Operator::Multiply => number_op(op, left, right, |a, b| a * b),
                    Operator::Divide => number_op(op, left, right, |a, b| a / b),
                    Operator::Modulo => number_op(op, left, right, |a, b| a % b),
                    Operator::Exponent => number_op(op, left, right, |a, b| a.powf(b)),
                    Operator::Equal => Ok(Value::Bool(left == right)),
                    Operator::NotEqual => Ok(Value::Bool(left != right)),
                    Operator::Greater => bool_op(op, left, right, |a, b| a > b),
                    Operator::GreaterEqual => bool_op(op, left, right, |a, b| a >= b),
                    Operator::Lesser => bool_op(op, left, right, |a, b| a < b),
                    Operator::LesserEqual => bool_op(op, left, right, |a, b| a <= b),
                    Operator::LogicOr => if is_truthy(&left) { Ok(left) } else { Ok(right) },
                    Operator::LogicAnd => if !is_truthy(&left) { Ok(left) } else { Ok(right) },
                    _ => Err(format!("Invalid binary operator {:?}", op)),
                }
            },
            ASTNode::Lambda(params, def) => Ok(Value::Function(FunctionType::User(params, *def, self.environment.clone()))),
            ASTNode::Call(func, args) => {
                let func_type = self.expression(*func)?;
                if let Value::Function(func_type) = func_type {
                    let arg_count = match &func_type {
                        FunctionType::User(params, _, _) => params.len(),
                        FunctionType::Native(count, _) => *count,
                    };
                    if arg_count == args.len() {
                        match func_type {
                            FunctionType::User(params, def, closure) => {
                                let mut child_env = Environment::new(Some(closure.clone()));
                                for (i, arg) in args.iter().enumerate() {
                                    let eval = self.expression(arg.clone())?;
                                    child_env.define(&params[i], eval);
                                }
                                let parent_env = self.environment.clone();
                                self.environment = Rc::new(RefCell::new(child_env));
                                let mut ret = Value::Null;
                                if let ASTNode::Block(decls) = def {
                                    for decl in decls.iter() {
                                        match self.statement(decl.clone())? {
                                            Message::Return(val) => {
                                                ret = val;
                                                break;
                                            },
                                            _ => (),
                                        }
                                    }
                                }
                                self.environment = parent_env;
                                Ok(ret)
                            },
                            FunctionType::Native(_, def) => {
                                let mut eval_args = Vec::new();
                                for arg in args {
                                    eval_args.push(self.expression(arg.clone())?);
                                }
                                def(eval_args)
                            },
                        }
                    } else {
                        Err(format!("Expected {} arguments when calling function, got {}", arg_count, args.len()))
                    }
                } else {
                    Err("Undefined function".to_string())
                }
            },
            _ => Err(format!("Invalid expression {:?}", node)),
        }
    }

    fn statement(&mut self, node: ASTNode) -> RMessage {
        match node {
            ASTNode::Compound(decls) => {
                for decl in decls {
                    match self.statement(decl)? {
                        Message::None => (),
                        other => return Ok(other),
                    }
                }
                Ok(Message::None)
            },
            ASTNode::Block(decls) => {
                let child_env = RefCell::new(Environment::new(Some(self.environment.clone())));
                let parent_env = self.environment.clone();
                self.environment = Rc::new(child_env);
                let mut msg = Message::None;
                for decl in decls {
                    match self.statement(decl)? {
                        Message::None => (),
                        other => {
                            msg = other;
                            break;
                        },
                    }
                }
                self.environment = parent_env;
                Ok(msg)
            },
            ASTNode::ExprStmt(expr) => self.expression(*expr).and(Ok(Message::None)),
            ASTNode::VarDecl(id, init) => {
                let eval = self.expression((*init).unwrap_or(ASTNode::Null))?;
                self.environment.borrow_mut().define(&id, eval);
                Ok(Message::None)
            },
            ASTNode::Function(id, params, def) => {
                self.environment.borrow_mut().define(&id, Value::Function(FunctionType::User(params, *def, self.environment.clone())));
                Ok(Message::None)
            },
            ASTNode::If(cond, stmt1, stmt2) => if is_truthy(&self.expression(*cond)?) { self.statement(*stmt1) } else {
                if let Some(stmt) = *stmt2 {
                    self.statement(stmt)
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
                        Message::Return(val) => return Ok(Message::Return(val)),
                    }
                }
                Ok(Message::None)
            },
            ASTNode::Break => Ok(Message::Break),
            ASTNode::Continue => Ok(Message::Continue),
            ASTNode::Return(expr) => Ok(Message::Return(self.expression(*expr)?)),
            _ => Err(format!("Invalid statement {:#?}", node)),
        }
    }

    pub fn interpret(&mut self, root: ASTNode) -> Result<(), String> {
        match self.statement(root) {
            Ok(_) => Ok(()),
            Err(msg) => Err(msg),
        }
    }

    pub fn new() -> Interpreter {
        let mut env = Environment::new(None);
        let natives: Vec<(&str, usize, fn(Vec<Value>) -> RValue)> = vec![
            ("print", 1, |args| {
                match &args[0] {
                    Value::Number(x) => println!("{}", x),
                    Value::Bool(x) => println!("{}", x),
                    Value::String(x) => println!("{}", x),
                    Value::Function(_) => println!("function"),
                    Value::Null => println!("null"),               
                };
                Ok(Value::Null)
            }),
            ("input", 0, |_args| {
                let mut input = String::new();
                match std::io::stdin().read_line(&mut input) {
                    Ok(_) => Ok(Value::String(input.trim().to_string())),
                    Err(msg) => Err(format!("Failed to get input: {}", msg)),
                }
            }),
            ("number", 1, |args| match &args[0] {
                Value::String(val) => match val.parse::<f64>() {
                    Ok(num) => Ok(Value::Number(num)),
                    Err(_) => Err(format!("Invalid value for 'number' function: {:?}", val)),
                },
                other => Err(format!("Invalid type for 'number' function: {:?}", other)),
            }),
            ("time", 0, |_args| Ok(Value::Number(std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs_f64())))
        ];
        for (id, args, func) in natives.iter() {
            env.define(id, Value::Function(FunctionType::Native(*args, *func)));
        }
        Interpreter {
            environment: Rc::new(RefCell::new(env)),
        }
    }
}

