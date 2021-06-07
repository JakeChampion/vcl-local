use std::num::Wrapping;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::expr::{self, Program, Symbol};
use std::fmt;

static INIT: &str = "init";

trait Callable {
    fn arity(&self, interpreter: &Interpreter) -> u8;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String>;
}

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub arity: u8,
    pub callable: fn(&mut Interpreter, &[Value]) -> Result<Value, String>,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Function({})", self.name)
    }
}

impl Callable for Function {
    fn arity(&self, _interpreter: &Interpreter) -> u8 {
        self.arity
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value, String> {
        (self.callable)(interpreter, args)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(Wrapping<i64>),
    Float(f64),
    String(String),
    Bool(bool),
    Function(Function),
    Nil,
}

fn as_callable(_interpreter: &Interpreter, value: &Value) -> Option<Box<dyn Callable>> {
    match value {
        Value::Function(f) => Some(Box::new(f.clone())),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Id,
    Ip,
    Time,
    Rtime,
    Float,
    Integer,
    String,
    Bool,
    Nil,
    Function,
}

pub const fn type_of(val: &Value) -> Type {
    match val {
        Value::Float(_) => Type::Float,
        Value::Integer(_) => Type::Integer,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::Function(_) => Type::Function,
        Value::Nil => Type::Nil,
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Value::Float(n) => write!(f, "{}", n),
            Value::Integer(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "'{}'", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Function(func) => write!(f, "Function({})", func.name),
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
    venv: HashMap<String, (Type, Option<Value>, SourceLocation)>,
}

pub enum LookupResult<'a> {
    Ok(&'a Value),
    UndefButDeclared(SourceLocation),
    UndefAndNotDeclared,
}
fn int_to_float (b:i64) -> f64 {
    let bf: Option<f64> = num_traits::cast::FromPrimitive::from_i64(b);
    if let Some(bf) = bf {
        if bf as i64 == b {
            return bf;
        }
    }
    panic!("Integer {} is not exactly representable as a floating-point number", b);
}
fn expr_type_to_interpreter_type(expr_type: &expr::Type) -> Type {
    match expr_type {
        expr::Type::Id => Type::Id,
        expr::Type::Ip => Type::Ip,
        expr::Type::Rtime => Type::Rtime,
        expr::Type::Time => Type::Time,
        expr::Type::Float => Type::Float,
        expr::Type::Integer => Type::Integer,
        expr::Type::String => Type::String,
        expr::Type::Bool => Type::Bool,
        // expr::Type::Function(_) => Type::Function,
        _ => {
            panic!(
                "Can not convert parser type {:?} to an interpreter type",
                expr_type
            )
        }
    }
}

impl Environment {
    pub fn define(&mut self, sym: expr::Symbol, var_type: Type, val: Option<Value>) {
        self.venv.insert(
            sym.name,
            (
                var_type,
                val,
                SourceLocation {
                    line: sym.line,
                    col: sym.col,
                },
            ),
        );
    }

    pub fn lookup(&self, sym: &expr::Symbol) -> LookupResult<'_> {
        match self.venv.get(&sym.name) {
            Some((_var_type, maybe_val, defn_source_location)) => match maybe_val {
                Some(val) => LookupResult::Ok(val),
                None => LookupResult::UndefButDeclared(SourceLocation {
                    line: defn_source_location.line,
                    col: defn_source_location.col,
                }),
            },
            None => LookupResult::UndefAndNotDeclared,
        }
    }

    pub fn get(&self, sym: &expr::Symbol) -> Result<&Value, String> {
        match self.lookup(sym) {
            LookupResult::Ok(val) => Ok(val),
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
            expr::Expr::Get(s, _) => self.get_sym(s.as_ref().clone())?,
            expr::Expr::Variable(sym) => sym,
            _ => {
                panic!("nooo")
            }
        };
        Ok(s)
    }

    pub fn assign(&mut self, sym: expr::Expr, val: &Value) -> Result<(), String> {
        let sym = self.get_sym(sym)?;
        if let Some(entry) = self.venv.clone().get(&sym.name) {
            if entry.0 == type_of(val) {
                self.define(sym, entry.0, Some(val.clone()));
                return Ok(());
            }
            if let Value::Integer(b) = val {
                if entry.0 == Type::Float {
                    self.define(sym, entry.0, Some(Value::Float(int_to_float(b.0))));
                    return Ok(());
                }
            }
            return Err(format!(
                "attempting to assign to variable of type {:?} with a vale of type {:?} at line={},col={}",
                entry.0, type_of(val), sym.line, sym.col
            ));
        }

        Err(format!(
            "attempting to assign to undeclared variable at line={},col={}",
            sym.line, sym.col
        ))
    }
}

pub struct Interpreter {
    pub counter: u64,
    pub env: Environment,
    pub globals: Environment,
    pub retval: Option<Value>,
    pub output: Vec<String>,
    pub interrupted: Arc<AtomicBool>,
    pub backtrace: Vec<(u64, String)>,
}

impl Default for Interpreter {
    fn default() -> Self {
        let globals_venv = HashMap::new();
        let globals = Environment { venv: globals_venv };

        Self {
            counter: 0,
            env: Environment::default(),
            globals,
            retval: None,
            output: Vec::default(),
            interrupted: Arc::new(AtomicBool::new(false)),
            backtrace: vec![(0, "script".to_string())],
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, program: &Program) -> Result<(), String> {
        self.interrupted.store(false, Ordering::Release);
        for _stmt in &program.body {
            // println!("stmt: {:?}", stmt);
            // 1. setup backends + healthchecks
            // 2. setup directors
            // 3. setup acls
            // 4. setup imports
            // 
            // 5. setup tables
            // 6. setup subroutine state machine
            // 
            // self.execute(stmt)?
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
                if Self::is_truthy(&self.interpret_expr(cond)?) {
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
                self.env.define(
                    sym.clone(),
                    expr_type_to_interpreter_type(sym.var_type.as_ref().unwrap()),
                    None,
                );
                Ok(())
            }
            expr::Stmt::Block(stmts) => {
                for stmt in stmts.iter() {
                    self.execute(stmt)?;
                }

                Ok(())
            }
            expr::Stmt::Set(identifier, assignment, value) => {
                self.interpret_set(identifier, assignment, value)?;
                Ok(())
            }
            expr::Stmt::Return(_, _)
            | expr::Stmt::Esi
            | expr::Stmt::Include(_)
            | expr::Stmt::Call(_)
            | expr::Stmt::Restart(_)
            | expr::Stmt::Error(_)
            | expr::Stmt::Add(_, _)
            | expr::Stmt::Unset(_)
            | expr::Stmt::Synthetic(_)
            | expr::Stmt::SyntheticBase64(_) => {
                todo!()
            }
        }
    }

    fn interpret_set(
        &mut self,
        sym: &expr::Expr,
        assignment: &expr::Assignment,
        val_expr: &expr::Expr,
    ) -> Result<Value, String> {
        if self.interrupted.load(Ordering::Acquire) {
            return Ok(Value::Nil);
        }

        match assignment {
            expr::Assignment::Assign => {
                let val = self.interpret_expr(val_expr)?;

                if let Err(err) = self.env.assign(sym.clone(), &val) {
                    return Err(err);
                }

                Ok(val)
            }
            expr::Assignment::Addition => self.addition(sym, val_expr),
            expr::Assignment::Subtraction => self.subtraction(sym, val_expr),
            expr::Assignment::Multiplication => self.multiplication(sym, val_expr),
            expr::Assignment::Division => self.division(sym, val_expr),
            expr::Assignment::Modulus => self.modulus(sym, val_expr),
            expr::Assignment::BitwiseOr => self.bitwise_or(sym, val_expr),
            expr::Assignment::BitwiseAnd => self.bitwise_and(sym, val_expr),
            expr::Assignment::BitwiseXor => self.bitwise_xor(sym, val_expr),
            expr::Assignment::LeftShift => self.left_shift(sym, val_expr),
            expr::Assignment::RightShift => self.right_shift(sym, val_expr),
            expr::Assignment::LeftRotate => self.rotate_left(sym, val_expr),
            expr::Assignment::RightRotate => self.rotate_right(sym, val_expr),
            expr::Assignment::LogicalAnd => self.logical_and(sym, val_expr),
            expr::Assignment::LogicalOr => self.logical_or(sym, val_expr),
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
            expr::Expr::Literal(lit) => Ok(Self::interpret_literal(lit)),
            expr::Expr::Unary(op, e) => self.interpret_unary(*op, e),
            expr::Expr::Binary(lhs, op, rhs) => self.interpret_binary(lhs, *op, rhs),
            expr::Expr::Call(callee, loc, args) => self.call(callee, loc, args),
            expr::Expr::Get(_lhs, _attr) => todo!(), //self.getattr(lhs, &attr.name),
            expr::Expr::Grouping(e) => self.interpret_expr(e),
            expr::Expr::Variable(sym) => match self.lookup(sym) {
                Ok(val) => Ok(val.clone()),
                Err(err) => Err(err),
            },
            expr::Expr::Logical(left_expr, expr::LogicalOp::Or, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                if Self::is_truthy(&left) {
                    Ok(left)
                } else {
                    Ok(self.interpret_expr(right_expr)?)
                }
            }
            expr::Expr::Logical(left_expr, expr::LogicalOp::And, right_expr) => {
                let left = self.interpret_expr(left_expr)?;
                if Self::is_truthy(&left) {
                    Ok(self.interpret_expr(right_expr)?)
                } else {
                    Ok(left)
                }
            }
            expr::Expr::If(_, _, _) => {
                todo!()
            }
        }
    }

    fn addition(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("12")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2121")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a + Wrapping(*b)),
            (Value::Float(a), expr::Literal::Float(b)) => Value::Float(a + b),
            (Value::Float(a), expr::Literal::Integer(b)) => Value::Float(a + int_to_float(*b)),
            _ => {
                panic!("no please don't make me += things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn subtraction(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("12")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => match a {
                expr::Literal::Float(a) => Value::Float(*a),
                expr::Literal::Integer(a) => Value::Integer(Wrapping(*a)),
                _ => {
                    panic!("no please don't make me -= things of different types")
                }
            },
            expr::Expr::Variable(s) => self.lookup(s)?.clone(),
            _ => {
                todo!("2121")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            _ => {
                panic!("no please don't make me -= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn multiplication(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("12")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2121")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a * Wrapping(*b)),
            (Value::Float(a), expr::Literal::Float(b)) => Value::Float(a * b),
            _ => {
                panic!("no please don't make me *= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn division(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("12")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2121")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a / Wrapping(*b)),
            (Value::Float(a), expr::Literal::Float(b)) => Value::Float(a / b),
            _ => {
                panic!("no please don't make me /= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn modulus(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(Wrapping((a.0).rem_euclid(*b))),
            (Value::Float(a), expr::Literal::Float(b)) => Value::Float(a.rem_euclid(*b)),
            _ => {
                panic!("no please don't make me %= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn bitwise_or(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a | Wrapping(*b)),
            (Value::Float(_), expr::Literal::Float(_)) => {
                panic!("Assignment operator |= not possible for type FLOAT");
            }
            _ => {
                panic!("no please don't make me |= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn bitwise_and(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a & Wrapping(*b)),
            (Value::Float(_), expr::Literal::Float(_)) => {
                panic!("Assignment operator &= not possible for type FLOAT");
            }
            _ => {
                panic!("no please don't make me &= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn bitwise_xor(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a ^ Wrapping(*b)),
            (Value::Float(_), expr::Literal::Float(_)) => {
                panic!("Assignment operator ^= not possible for type FLOAT");
            }
            _ => {
                panic!("no please don't make me ^= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn left_shift(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a << *b as usize), //TODO: make this work with an i64
            (Value::Float(_), expr::Literal::Float(_)) => {
                panic!("Assignment operator <<= not possible for type FLOAT");
            }
            _ => {
                panic!("no please don't make me <<= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn right_shift(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => Value::Integer(a >> *b as usize), //TODO: make this work with an i64
            (Value::Float(_), expr::Literal::Float(_)) => {
                panic!("Assignment operator >>= not possible for type FLOAT");
            }
            _ => {
                panic!("no please don't make me >>= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn rotate_left(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => {
                Value::Integer(a.rotate_left(*b as u32)) //TODO: make this work with an i64
            }
            (Value::Float(_), expr::Literal::Float(_)) => {
                panic!("Assignment operator rol= not possible for type FLOAT");
            }
            _ => {
                panic!("no please don't make me rol= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn rotate_right(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => a,
            _ => {
                unreachable!("2222")
            }
        };

        let v = match (sym_val, int) {
            (Value::Integer(a), expr::Literal::Integer(b)) => {
                Value::Integer(a.rotate_right(*b as u32)) //TODO: make this work with an i64
            }
            (Value::Float(_), expr::Literal::Float(_)) => {
                panic!("Assignment operator ror= not possible for type FLOAT");
            }
            _ => {
                panic!("no please don't make me ror= things of different types")
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn logical_and(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => {
                match a {
                    expr::Literal::True => Value::Bool(true),
                    expr::Literal::False => Value::Bool(false),
                    _ => {
                        panic!("no please don't make me &&= things of different types, a: {:?} b: {:?}", sym_val, val_expr)
                    }
                }
            }
            expr::Expr::Variable(a) => self.lookup(a)?.clone(),
            _ => {
                panic!(
                    "no please don't make me &&= things of different types, a: {:?} b: {:?}",
                    sym_val, val_expr
                )
            }
        };

        let v = match (sym_val, int) {
            (Value::Bool(a), Value::Bool(b)) => Value::Bool(*a && b),
            _ => {
                panic!(
                    "no please don't make me &&= things of different types, a: {:?} b: {:?}",
                    sym_val, val_expr
                )
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn logical_or(&mut self, sym: &expr::Expr, val_expr: &expr::Expr) -> Result<Value, String> {
        let sym_inner = match sym {
            expr::Expr::Variable(s) => s,
            _ => {
                unreachable!("22")
            }
        };
        let sym_val = self.lookup(sym_inner)?;
        let int = match val_expr {
            expr::Expr::Literal(a) => {
                match a {
                    expr::Literal::True => Value::Bool(true),
                    expr::Literal::False => Value::Bool(false),
                    _ => {
                        panic!("no please don't make me ||= things of different types, a: {:?} b: {:?}", sym_val, val_expr)
                    }
                }
            }
            expr::Expr::Variable(a) => self.lookup(a)?.clone(),
            _ => {
                panic!(
                    "no please don't make me ||= things of different types, a: {:?} b: {:?}",
                    sym_val, val_expr
                )
            }
        };

        let v = match (sym_val, int) {
            (Value::Bool(a), Value::Bool(b)) => Value::Bool(*a || b),
            _ => {
                panic!(
                    "no please don't make me ||= things of different types, a: {:?} b: {:?}",
                    sym_val, val_expr
                )
            }
        };
        self.env.assign(sym.clone(), &v)?;

        Ok(v)
    }

    fn call(
        &mut self,
        callee_expr: &expr::Expr,
        loc: &expr::SourceLocation,
        arg_exprs: &[expr::Expr],
    ) -> Result<Value, String> {
        let callee = self.interpret_expr(callee_expr)?;

        match as_callable(self, &callee) {
            Some(callable) => {
                let maybe_args: Result<Vec<_>, _> = arg_exprs
                    .iter()
                    .map(|arg| self.interpret_expr(arg))
                    .collect();

                match maybe_args {
                    Ok(args) => {
                        let arity: usize = callable.arity(self).into();
                        if args.len() == arity {
                            callable.call(self, &args)
                        } else {
                            Err(format!(
                                "Invalid call at line={},col={}: callee has arity {}, but \
                                         was called with {} arguments",
                                loc.line,
                                loc.col,
                                callable.arity(self),
                                args.len()
                            ))
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
            (_, expr::BinaryOpTy::EqualEqual, _) => Ok(Value::Bool(Self::equals(&lhs, &rhs))),
            (_, expr::BinaryOpTy::NotEqual, _) => Ok(Value::Bool(!Self::equals(&lhs, &rhs))),
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
            (expr::UnaryOpTy::Bang, _) => Ok(Value::Bool(!Self::is_truthy(&val))),
        }
    }

    const fn is_truthy(val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    fn interpret_literal(lit: &expr::Literal) -> Value {
        match lit {
            expr::Literal::String(s) => Value::String(s.clone()),
            expr::Literal::True => Value::Bool(true),
            expr::Literal::False => Value::Bool(false),
            expr::Literal::Float(n) => Value::Float(*n),
            expr::Literal::Integer(n) => Value::Integer(Wrapping(*n)),
            expr::Literal::Duration(_, _)
            | expr::Literal::AclEntry(_, _)
            | expr::Literal::Percent(_) => {
                todo!()
            }
        }
    }
}
