use crate::parser::*;

use std::{
    collections::HashMap,
    cell::RefCell,
    rc::Rc,
};

#[derive(Debug, PartialEq, Clone)]
enum FunctionType {
    User(Vec<String>, ASTNode, EnvRef),
    Native(usize, fn(Vec<Value>) -> Value),
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
#[derive(Debug, PartialEq)]
struct Environment {
    values: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

pub struct Interpreter {
    global: EnvRef,
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
    fn expression(&mut self, node: ASTNode, env: EnvRef) -> RValue {
        match node {
            ASTNode::Number(val) => Ok(Value::Number(val)),
            ASTNode::Bool(val) => Ok(Value::Bool(val)),
            ASTNode::String(val) => Ok(Value::String(val)),
            ASTNode::Null => Ok(Value::Null),
            ASTNode::Variable(id) => Rc::clone(&env).borrow().get(&id),
            ASTNode::Assign(id, expr) => {
                let eval = self.expression(*expr, Rc::clone(&env))?;
                Rc::clone(&env).borrow_mut().set(&id, eval)
            },
            ASTNode::Unary(op, expr) => {
                match op {
                    Operator::Subtract => {
                        match self.expression(*expr, env)? {
                            Value::Number(val) => Ok(Value::Number(-val)),
                            other => Err(format!("Invalid type for unary {:?}: {:?}", op, other)),
                        }
                    }
                    _ => Err(format!("Invalid unary operator {:?}", op)),
                }
            },
            ASTNode::Binary(op, left_expr, right_expr) => {
                let (left, right) = (self.expression(*left_expr, Rc::clone(&env))?, self.expression(*right_expr, env)?);
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
            ASTNode::Call(id, args) => {
                let func_type = Rc::clone(&env).borrow().get(&id)?;
                if let Value::Function(func_type) = func_type {
                    let arg_count = match &func_type {
                        FunctionType::User(params, _, _) => params.len(),
                        FunctionType::Native(count, _) => *count,
                    };
                    if arg_count == args.len() {
                        match func_type {
                            FunctionType::User(params, def, closure) => {
                                closure.borrow_mut().parent = Some(env);
                                let child_env = Rc::new(RefCell::new(Environment::new(Some(closure))));
                                for (i, arg) in args.iter().enumerate() {
                                    let eval = self.expression(arg.clone(), Rc::clone(&child_env))?;
                                    child_env.borrow_mut().define(&params[i], eval);
                                }
                                if let ASTNode::Block(decls) = def {
                                    for decl in decls {
                                        match self.statement(decl, Rc::clone(&child_env))? {
                                            Message::Return(val) => return Ok(val),
                                            _ => (),
                                        }
                                    }
                                }
                                Ok(Value::Null)
                            },
                            FunctionType::Native(_, def) => {
                                let mut eval_args = Vec::new();
                                for arg in args {
                                    eval_args.push(self.expression(arg.clone(), Rc::clone(&env))?);
                                }
                                Ok(def(eval_args))
                            },
                        }
                    } else {
                        Err(format!("Expected {} arguments when calling '{}', got {}", arg_count, id, args.len()))
                    }
                } else {
                    Err(format!("Undefined function '{}'", id))
                }
            },
            _ => Err(format!("Invalid expression {:?}", node)),
        }
    }

    fn statement(&mut self, node: ASTNode, env: EnvRef) -> RMessage {
        match node {
            ASTNode::Compound(decls) => {
                for decl in decls {
                    match self.statement(decl, Rc::clone(&env))? {
                        Message::None => (),
                        other => return Ok(other),
                    }
                }
                Ok(Message::None)
            },
            ASTNode::Block(decls) => {
                let child_env = Rc::new(RefCell::new(Environment::new(Some(env))));
                for decl in decls {
                    match self.statement(decl, Rc::clone(&child_env))? {
                        Message::None => (),
                        other => return Ok(other),
                    }
                }
                Ok(Message::None)
            },
            ASTNode::ExprStmt(expr) => self.expression(*expr, env).and(Ok(Message::None)),
            ASTNode::VarDecl(id, init) => {
                let eval = self.expression((*init).unwrap_or(ASTNode::Null), Rc::clone(&env))?;
                Rc::clone(&env).borrow_mut().define(&id, eval);
                Ok(Message::None)
            },
            ASTNode::Function(id, params, def) => {
                Rc::clone(&env).borrow_mut().define(&id, Value::Function(FunctionType::User(params, *def, env.clone())));
                Ok(Message::None)
            },
            ASTNode::If(cond, stmt1, stmt2) => if is_truthy(&self.expression(*cond, Rc::clone(&env))?) { self.statement(*stmt1, Rc::clone(&env)) } else {
                if let Some(stmt) = *stmt2 {
                    self.statement(stmt, env)
                } else {
                    Ok(Message::None)
                }
            },
            ASTNode::While(cond, stmt) => {
                while is_truthy(&self.expression(*cond.clone(), Rc::clone(&env))?) {
                    match self.statement(*stmt.clone(), Rc::clone(&env))? {
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
            ASTNode::Return(expr) => Ok(Message::Return(self.expression(*expr, env)?)),
            _ => Err(format!("Invalid statement {:#?}", node)),
        }
    }

    pub fn interpret(&mut self, root: ASTNode) -> Result<(), String> {
        match self.statement(root, Rc::clone(&self.global)) {
            Ok(_) => Ok(()),
            Err(msg) => Err(msg),
        }
    }

    pub fn new() -> Interpreter {
        let mut env = Environment::new(None);
        env.define("print", Value::Function(FunctionType::Native(1,
            |args| {
                match &args[0] {
                    Value::Number(x) => println!("{}", x),
                    Value::Bool(x) => println!("{}", x),
                    Value::String(x) => println!("{}", x),
                    Value::Function(_) => println!("function"),
                    Value::Null => println!("null"),               
                };
                Value::Null
            }
        )));
        Interpreter {
            global: Rc::new(RefCell::new(env)),
        }
    }
}

/*use crate::parser::*;
use std::collections::HashMap;

use std::time::SystemTime;
use std::time::UNIX_EPOCH;

use std::io;

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
        env.define("print".to_string(), Value::Function(vec!["x".to_string()], FunctionType::BuiltIn(
            |args| {
                match &args[0] {
                    Value::Number(x) => println!("{}", x),
                    Value::Bool(x) => println!("{}", x),
                    Value::Str(x) => println!("{}", x),
                    Value::Function(_, _) => println!("function"),
                    Value::Null => println!("null"),               
                };
                Ok(Value::Null)
            }
        )));
        env.define("input".to_string(), Value::Function(vec![], FunctionType::BuiltIn(
            |_args| {
                let mut input = String::new();
                match io::stdin().read_line(&mut input) {
                    Ok(_) => Ok(Value::Str(input.trim().to_string())),
                    Err(_) => Err("Failed to get input".to_string()),
                }
            }
        )));
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
        env.define("number".to_string(), Value::Function(vec!["x".to_string()], FunctionType::BuiltIn(
            |args| match &args[0] {
                Value::Number(x) => Ok(Value::Number(*x)),
                Value::Str(x) => match x.parse::<f64>() {
                    Ok(v) => Ok(Value::Number(v)),
                    Err(_) => Err("Invalid value for number function".to_string()),
                },
                _ => Err("Invalid type for number function".to_string()),
            }
        )));       
        Interpreter {
            environment: env,
        }
    }
}
*/
