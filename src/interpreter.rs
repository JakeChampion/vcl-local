use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::expr::{self, Symbol};
use std::fmt;

static INIT: &str = "init";

trait Callable {
    fn arity(&self, interpreter: &Interpreter) -> u8;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>;
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub callable: fn(&mut Interpreter, &[Value]) -> Result<Value, String>,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

impl Callable for NativeFunction {
    fn arity(&self, _interpreter: &Interpreter) -> u8 {
        self.arity
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        (self.callable)(interpreter, args)
    }
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    pub id: u64,
    pub name: expr::Symbol,
    pub parameters: Vec<expr::Symbol>,
    pub body: Vec<expr::Stmt>,
}

impl Callable for LoxFunction {
    fn arity(&self, _interpreter: &Interpreter) -> u8 {
        self.parameters.len().try_into().unwrap()
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        let args_env: HashMap<_, _> = self
            .parameters
            .iter()
            .zip(args.iter())
            .map(|(param, arg)| {
                (
                    param.name.clone(),
                    (
                        Some(arg.clone()),
                        SourceLocation {
                            line: param.line,
                            col: param.col,
                        },
                    ),
                )
            })
            .collect();

        let saved_env = interpreter.env.clone();
        let saved_retval = interpreter.retval.clone();

        let mut env = interpreter.env.clone();
        env.venv.extend(saved_env.venv.clone());
        env.venv.extend(args_env);
        interpreter.env = env;
        interpreter.backtrace.push((0, self.name.name.clone()));
        interpreter.interpret(&self.body)?;

        let retval = interpreter.retval.clone();
        interpreter.backtrace.pop();
        interpreter.retval = saved_retval;
        interpreter.env = saved_env;

        match retval {
            Some(val) => {
                Ok(val)
            }
            None => {
                Ok(Value::Nil)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    NativeFunction(NativeFunction),
    LoxFunction(
        expr::Symbol,
        /*id*/ u64,
    ),
    Nil,
}

fn as_callable(interpreter: &Interpreter, value: &Value) -> Option<Box<dyn Callable>> {
    match value {
        Value::NativeFunction(f) => Some(Box::new(f.clone())),
        Value::LoxFunction(_, id) => match interpreter.lox_functions.get(id) {
            Some(f) => {
                let f_copy = f.clone();
                Some(Box::new(f_copy))
            }
            None => panic!(
                "Internal interpreter error! Could not find loxFunction with id {}.",
                id
            ),
        },
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Float,
    Integer,
    String,
    Bool,
    Nil,
    NativeFunction,
    LoxFunction,
    LoxClass,
    LoxInstance,
    List,
}

pub fn type_of(val: &Value) -> Type {
    match val {
        Value::Float(_) => Type::Float,
        Value::Integer(_) => Type::Integer,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::NativeFunction(_) => Type::NativeFunction,
        Value::LoxFunction(_, _) => Type::LoxFunction,
        Value::Nil => {Type::Nil}
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Value::Float(n) => write!(f, "{}", n),
            Value::Integer(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "'{}'", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::NativeFunction(func) => write!(f, "NativeFunction({})", func.name),
            Value::LoxFunction(sym, _) => write!(f, "LoxFunction({})", sym.name),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    line: usize,
    col: i64,
}

#[derive(Debug, Default, Clone)]
pub struct Environment {
    // SourceLocation is the location of a declaration
    venv: HashMap<String, (Option<Value>, SourceLocation)>,
}

pub enum LookupResult<'a> {
    Ok(&'a Value),
    UndefButDeclared(SourceLocation),
    UndefAndNotDeclared,
}

impl Environment {
    pub fn define(&mut self, sym: expr::Symbol, maybe_val: Option<Value>) {
        self.venv.insert(
            sym.name,
            (
                maybe_val,
                SourceLocation {
                    line: sym.line,
                    col: sym.col,
                },
            ),
        );
    }

    pub fn lookup(&self, sym: &expr::Symbol) -> LookupResult {
        match self.venv.get(&sym.name) {
            Some((maybe_val, defn_source_location)) => match maybe_val {
                Some(val) => LookupResult::Ok(&val),
                None => LookupResult::UndefButDeclared(SourceLocation {
                    line: defn_source_location.line,
                    col: defn_source_location.col,
                }),
            },
            None => LookupResult::UndefAndNotDeclared,
        }
    }

    pub fn get(&self, sym: &expr::Symbol) -> Result<&Value, String> {
        match self.lookup(&sym) {
            LookupResult::Ok(val) => Ok(&val),
            LookupResult::UndefButDeclared(source_location) => Err(format!(
                "Use of undefined variable '{}' at line={},col={}.\
                \nNote: {} was previously declared at line={},col={}, \
                but was never defined.",
                &sym.name, sym.line, sym.col, &sym.name, source_location.line, source_location.col
            )),
            LookupResult::UndefAndNotDeclared => Err(format!(
                "Use of undefined variable {} at line={},col={}.\nNote: {} was never declared.",
                &sym.name, sym.line, sym.col, &sym.name
            )),
        }
    }

    pub fn get_sym(&mut self, sym: expr::Expr) -> Result<Symbol, String> {
        let s = match sym {
            expr::Expr::Get(s, _) => {self.get_sym(s.as_ref().clone())?}
            expr::Expr::Variable(sym) => {sym}
            _ => {panic!("nooo")}
        };
        Ok(s)
    }

    pub fn assign(&mut self, sym: expr::Expr, val: &Value) -> Result<(), String> {
        let sym = self.get_sym(sym)?;
        if self.venv.contains_key(&sym.name) {
            self.define(sym, Some(val.clone()));
            return Ok(());
        }

        Err(format!(
            "attempting to assign to undeclared variable at line={},col={}",
            sym.line, sym.col
        ))
    }
}

pub struct Interpreter {
    pub counter: u64,
    pub lox_functions: HashMap<u64, LoxFunction>,
    pub env: Environment,
    pub globals: Environment,
    pub retval: Option<Value>,
    pub output: Vec<String>,
    pub interrupted: Arc<AtomicBool>,
    pub backtrace: Vec<(u64, String)>,
}

impl Default for Interpreter {
    fn default() -> Interpreter {
        let globals_venv = HashMap::new();
        let globals = Environment {
            venv: globals_venv,
        };

        Interpreter {
            counter: 0,
            lox_functions: Default::default(),
            env: Default::default(),
            globals,
            retval: None,
            output: Default::default(),
            interrupted: Arc::new(AtomicBool::new(false)),
            backtrace: vec![(0, "script".to_string())],
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: &[expr::Stmt]) -> Result<(), String> {
        self.interrupted.store(false, Ordering::Release);
        for stmt in stmts {
            // println!("stmt: {:?}", stmt);
            self.execute(stmt)?
        }
        Ok(())
    }

    pub fn format_backtrace(&self) -> String {
        let lines: Vec<_> = self
            .backtrace
            .iter()
            .map(|(_, funname)| format!("[line ??] in {}", funname))
            .collect();
        format!("Backtrace (most recent call last):\n\n{}", lines.join("\n"))
    }

    fn alloc_id(&mut self) -> u64 {
        let res = self.counter;
        self.counter += 1;
        res
    }

    fn execute(&mut self, stmt: &expr::Stmt) -> Result<(), String> {
        if self.retval.is_some() {
            return Ok(());
        }

        match stmt {
            expr::Stmt::Expr(e) => match self.interpret_expr(e) {
                Ok(_) => Ok(()),
                Err(err) => Err(err),
            },
            expr::Stmt::If(cond, if_true, maybe_if_false) => {
                if Interpreter::is_truthy(&self.interpret_expr(cond)?) {
                    return self.execute(if_true);
                }
                if let Some(if_false) = maybe_if_false {
                    return self.execute(if_false);
                }
                Ok(())
            }
            expr::Stmt::Log(e) => match self.interpret_expr(e) {
                Ok(val) => {
                    println!("{}", val);
                    self.output.push(format!("{}", val));
                    Ok(())
                }
                Err(err) => Err(err),
            },
            expr::Stmt::VarDecl(sym) => {
                // println!("vardecl: {:?}", sym);
                self.env.define(sym.clone(), None);
                Ok(())
            }
            expr::Stmt::Block(stmts) => {

                for stmt in stmts.iter() {
                    self.execute(stmt)?;
                }

                Ok(())
            }
            expr::Stmt::Return(_, maybe_res) => {
                self.retval = Some(if let Some(res) = maybe_res {
                    self.interpret_expr(res)?
                } else {
                    Value::Nil
                });
                Ok(())
            }
            expr::Stmt::SubDecl(_) => {todo!()}
            expr::Stmt::Backend(_) => {todo!()}
            expr::Stmt::Director(_) => {todo!()}
            expr::Stmt::Table(_) => {todo!()}
            expr::Stmt::Acl(_) => {todo!()}
            expr::Stmt::Restart(_) => {todo!()}
            expr::Stmt::Error(_) => {todo!()}
            expr::Stmt::Add(_, _) => {todo!()}
            expr::Stmt::Set(_, _) => {todo!()}
            expr::Stmt::Unset(_) => {todo!()}
            expr::Stmt::Synthetic(_) => {todo!()}
            expr::Stmt::SyntheticBase64(_) => {todo!()}
        }
    }

    fn lookup(&self, sym: &expr::Symbol) -> Result<&Value, String> {
        match self.env.get(sym) {
            Ok(val) => Ok(val),
            Err(_) => self.globals.get(sym),
        }
    }

    fn interpret_expr(&mut self, expr: &expr::Expr) -> Result<Value, String> {
        if self.interrupted.load(Ordering::Acquire) {
            return Ok(Value::Nil);
        }

        match expr {
            expr::Expr::Literal(lit) => Ok(Interpreter::interpret_literal(lit)),
            expr::Expr::Unary(op, e) => self.interpret_unary(*op, e),
            expr::Expr::Binary(lhs, op, rhs) => self.interpret_binary(lhs, *op, rhs),
            expr::Expr::Call(callee, loc, args) => self.call(callee, loc, args),
            expr::Expr::Get(_lhs, _attr) => todo!(),//self.getattr(lhs, &attr.name),
            expr::Expr::Set(_lhs, _attr, _rhs) => todo!(),//self.setattr(lhs, attr, rhs),
            expr::Expr::Grouping(e) => self.interpret_expr(e),
            expr::Expr::Variable(sym) => match self.lookup(sym) {
                Ok(val) => Ok(val.clone()),
                Err(err) => Err(err),
            },
            expr::Expr::Assign(sym, val_expr) => {
                let val = self.interpret_expr(val_expr)?;

                if let Err(err) = self.env.assign(sym.as_ref().clone(), &val) {
                    return Err(err);
                }

                Ok(val)
            }
            expr::Expr::Logical(left_expr, expr::LogicalOp::Or, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                if Interpreter::is_truthy(&left) {
                    Ok(left)
                } else {
                    Ok(self.interpret_expr(right_expr)?)
                }
            }
            expr::Expr::Logical(left_expr, expr::LogicalOp::And, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                if !Interpreter::is_truthy(&left) {
                    Ok(left)
                } else {
                    Ok(self.interpret_expr(right_expr)?)
                }
            }
            expr::Expr::Addition(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("12")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2121")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a+b)
                    }
                    (Value::Float(a), expr::Literal::Float(b)) => {
                        Value::Float(a+b)
                    }
                    _ => {panic!("no please don't make me += things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)
            }
            expr::Expr::Subtraction(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("12")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2121")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a-b)
                    }
                    (Value::Float(a), expr::Literal::Float(b)) => {
                        Value::Float(a-b)
                    }
                    _ => {panic!("no please don't make me -= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::Multiplication(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("12")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2121")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a*b)
                    }
                    (Value::Float(a), expr::Literal::Float(b)) => {
                        Value::Float(a*b)
                    }
                    _ => {panic!("no please don't make me *= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::Division(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("12")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2121")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a/b)
                    }
                    (Value::Float(a), expr::Literal::Float(b)) => {
                        Value::Float(a/b)
                    }
                    _ => {panic!("no please don't make me /= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::Modulus(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer((a).rem_euclid(*b))
                    }
                    (Value::Float(a), expr::Literal::Float(b)) => {
                        Value::Float((a).rem_euclid(*b))
                    }
                    _ => {panic!("no please don't make me %= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::BitwiseOr(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a|b)
                    }
                    (Value::Float(_), expr::Literal::Float(_)) => {
                        panic!("Assignment operator |= not possible for type FLOAT");
                    }
                    _ => {panic!("no please don't make me |= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::BitwiseAnd(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a&b)
                    }
                    (Value::Float(_), expr::Literal::Float(_)) => {
                        panic!("Assignment operator &= not possible for type FLOAT");
                    }
                    _ => {panic!("no please don't make me &= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::BitwiseXor(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a^b)
                    }
                    (Value::Float(_), expr::Literal::Float(_)) => {
                        panic!("Assignment operator ^= not possible for type FLOAT");
                    }
                    _ => {panic!("no please don't make me ^= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::LeftShift(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a<<b)
                    }
                    (Value::Float(_), expr::Literal::Float(_)) => {
                        panic!("Assignment operator <<= not possible for type FLOAT");
                    }
                    _ => {panic!("no please don't make me <<= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::RightShift(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a>>b)
                    }
                    (Value::Float(_), expr::Literal::Float(_)) => {
                        panic!("Assignment operator >>= not possible for type FLOAT");
                    }
                    _ => {panic!("no please don't make me >>= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::LeftRotate(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a.rotate_left(*b as u32))
                    }
                    (Value::Float(_), expr::Literal::Float(_)) => {
                        panic!("Assignment operator rol= not possible for type FLOAT");
                    }
                    _ => {panic!("no please don't make me rol= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::RightRotate(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {a}
                    _ => {unreachable!("2222")}
                };

                let v = match (sym_val, int) {
                    (Value::Integer(a), expr::Literal::Integer(b)) => {
                        Value::Integer(a.rotate_right(*b as u32))
                    }
                    (Value::Float(_), expr::Literal::Float(_)) => {
                        panic!("Assignment operator ror= not possible for type FLOAT");
                    }
                    _ => {panic!("no please don't make me ror= things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::LogicalAnd(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {
                        match a {
                            expr::Literal::True => {Value::Bool(true)}
                            expr::Literal::False => {Value::Bool(false)}
                            _ => {panic!("no please don't make me &&= things of different types, a: {:?} b: {:?}", sym_val, val_expr.as_ref())}
                        }
                    }
                    expr::Expr::Variable(a) => {self.lookup(a)?.clone()}
                    _ => {panic!("no please don't make me &&= things of different types, a: {:?} b: {:?}", sym_val, val_expr.as_ref())}
                };

                let v = match (sym_val, int) {
                    (Value::Bool(a), Value::Bool(b)) => {
                        Value::Bool(*a && b)
                    }
                    _ => {panic!("no please don't make me &&= things of different types, a: {:?} b: {:?}", sym_val, val_expr.as_ref())}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::LogicalOr(sym, val_expr) => {
                let sym_inner = match sym.as_ref() {
                    expr::Expr::Variable(s) => {s}
                    _ => {unreachable!("22")}
                };
                let sym_val = self.lookup(sym_inner)?;
                let int = match val_expr.as_ref() {
                    expr::Expr::Literal(a) => {
                        match a {
                            expr::Literal::True => {Value::Bool(true)}
                            expr::Literal::False => {Value::Bool(false)}
                            _ => {panic!("no please don't make me ||= things of different types, a: {:?} b: {:?}", sym_val, val_expr.as_ref())}
                        }
                    }
                    expr::Expr::Variable(a) => {self.lookup(a)?.clone()}
                    _ => {panic!("no please don't make me ||= things of different types, a: {:?} b: {:?}", sym_val, val_expr.as_ref())}
                };

                let v = match (sym_val, int) {
                    (Value::Bool(a), Value::Bool(b)) => {
                        Value::Bool(*a || b)
                    }
                    _ => {panic!("no please don't make me ||= things of different types, a: {:?} b: {:?}", sym_val, val_expr.as_ref())}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::If(_, _, _) => {todo!()}
        }
    }

    fn call(
        &mut self,
        callee_expr: &expr::Expr,
        loc: &expr::SourceLocation,
        arg_exprs: &[expr::Expr],
    ) -> Result<Value, String> {
        let callee = self.interpret_expr(callee_expr)?;

        match as_callable(&self, &callee) {
            Some(callable) => {
                let maybe_args: Result<Vec<_>, _> = arg_exprs
                    .iter()
                    .map(|arg| self.interpret_expr(arg))
                    .collect();

                match maybe_args {
                    Ok(args) => {
                        if args.len() != callable.arity(self).into() {
                            Err(format!(
                                "Invalid call at line={},col={}: callee has arity {}, but \
                                         was called with {} arguments",
                                loc.line,
                                loc.col,
                                callable.arity(self),
                                args.len()
                            ))
                        } else {
                            callable.call(self, &args)
                        }
                    }
                    Err(err) => Err(err),
                }
            }
            None => Err(format!(
                "value {:?} is not callable at line={},col={}",
                callee, loc.line, loc.col
            )),
        }
    }

    fn interpret_binary(
        &mut self,
        lhs_expr: &expr::Expr,
        op: expr::BinaryOp,
        rhs_expr: &expr::Expr,
    ) -> Result<Value, String> {
        let lhs = self.interpret_expr(lhs_expr)?;
        let rhs = self.interpret_expr(rhs_expr)?;

        match (&lhs, op.ty, &rhs) {
            (Value::Integer(n1), expr::BinaryOpTy::Less, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 < n2))
            }
            (Value::Integer(n1), expr::BinaryOpTy::LessEqual, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 <= n2))
            }
            (Value::Integer(n1), expr::BinaryOpTy::Greater, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 > n2))
            }
            (Value::Integer(n1), expr::BinaryOpTy::GreaterEqual, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 >= n2))
            }
            (Value::String(s1), expr::BinaryOpTy::Plus, Value::String(s2)) => {
                Ok(Value::String(format!("{}{}", s1, s2)))
            }
            (_, expr::BinaryOpTy::EqualEqual, _) => {
                Ok(Value::Bool(Interpreter::equals(&lhs, &rhs)))
            }
            (_, expr::BinaryOpTy::NotEqual, _) => Ok(Value::Bool(!Interpreter::equals(&lhs, &rhs))),
            _ => Err(format!(
                "invalid operands in binary operator {:?} of type {:?} and {:?} at line={},col={}",
                op.ty,
                type_of(&lhs),
                type_of(&rhs),
                op.line,
                op.col
            )),
        }
    }

    fn equals(lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::Float(n1), Value::Float(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (Value::Integer(s1), Value::Integer(s2)) => s1 == s2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Nil, Value::Nil) => true,
            (_, _) => false,
        }
    }

    fn interpret_unary(&mut self, op: expr::UnaryOp, expr: &expr::Expr) -> Result<Value, String> {
        let val = self.interpret_expr(expr)?;

        match (op.ty, &val) {
            (expr::UnaryOpTy::Bang, _) => Ok(Value::Bool(!Interpreter::is_truthy(&val))),
        }
    }

    fn is_truthy(val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    fn interpret_literal(lit: &expr::Literal) -> Value {
        match lit {
            // expr::Literal::Number(n) => Value::Number(*n),
            expr::Literal::String(s) => Value::String(s.clone()),
            expr::Literal::True => Value::Bool(true),
            expr::Literal::False => Value::Bool(false),
            expr::Literal::Float(n) => {Value::Float(*n)}
            expr::Literal::Integer(n) => {Value::Integer(*n)}
            expr::Literal::Duration(_, _) => {todo!()}
            expr::Literal::AclEntry(_, _) => {todo!()}
            expr::Literal::Percent(_) => {todo!()}
        }
    }
}
