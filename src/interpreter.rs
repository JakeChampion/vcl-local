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
                    _ => {panic!("no please don't make me + things of different types")}
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
                    _ => {panic!("no please don't make me - things of different types")}
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
                    _ => {panic!("no please don't make me * things of different types")}
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
                    _ => {panic!("no please don't make me / things of different types")}
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
                    _ => {panic!("no please don't make me / things of different types")}
                };
                self.env.assign(sym.as_ref().clone(), &v)?;

                Ok(v)}
            expr::Expr::BitwiseOr(_, _) => {todo!()}
            expr::Expr::BitwiseAnd(_, _) => {todo!()}
            expr::Expr::BitwiseXor(_, _) => {todo!()}
            expr::Expr::LeftShift(_, _) => {todo!()}
            expr::Expr::RightShift(_, _) => {todo!()}
            expr::Expr::LeftRotate(_, _) => {todo!()}
            expr::Expr::RightRotate(_, _) => {todo!()}
            expr::Expr::LogicalAnd(_, _) => {todo!()}
            expr::Expr::LogicalOr(_, _) => {todo!()}
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

#[cfg(test)]
mod tests {
    use crate::parser;
    use crate::scanner;
    use crate::interpreter;

    fn evaluate(code: &str) -> Result<String, String> {
        let tokens = scanner::scan_tokens(code.to_string()).unwrap();

        match parser::parse(tokens) {
            Ok(stmts) => {
                let mut interp = interpreter::Interpreter::default();
                let res = interp.interpret(&stmts);
                match res {
                    Ok(()) => Ok(interp.output.join("\n")),
                    Err(err) => Err(err),
                }
            }
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    #[test]
    fn test_fact() {
        fn fact(n: i32) -> i32 {
            if n <= 1 {
                return 1;
            }
            n * fact(n - 1)
        }

        let res = evaluate(
            "fun fact(n) { \n\
               if (n <= 1) {\n\
                   return 1; \n\
               }\n\
               return n * fact(n - 1); \n\
             } \n\
             print fact(10); ",
        );
        match res {
            Ok(output) => assert_eq!(output, format!("{}", fact(10))),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_invalid_binary_operands() {
        let res = evaluate("1 + \"string\";");

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => assert!(err.starts_with("invalid operands in binary operator")),
        }
    }

    #[test]
    fn test_invalid_unary_operand() {
        let res = evaluate("-\"cat\";");

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => {
                assert!(err
                    .starts_with("invalid application of unary op Minus to object of type String"))
            }
        }
    }

    #[test]
    fn return_not_enclosed_in_sub_decl() {
        let res = evaluate("return 1;");

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => assert!(err.starts_with("return statement not enclosed in a SubDecl at")),
        }
    }

    #[test]
    fn test_clock() {
        let res = evaluate("print clock();");

        match res {
            Ok(_) => {}
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_for() {
        let res = evaluate(
            "for (var i = 0; i < 5; i = i + 1) \n\
             { \n\
                 print(i); \n\
             }",
        );

        match res {
            Ok(output) => assert_eq!(output, "0\n1\n2\n3\n4"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_lox_funcs() {
        let res = evaluate(
            "fun sayHi(first, last) {\n\
               return \"Hi, \" + first + \" \" + last + \"!\";\n\
             }\n\
             \n\
             print sayHi(\"Dear\", \"Reader\");\n\
             \n\
             fun add(x,y,z) {\n\
                 return x + y + z;\n\
             }\n\
             \n\
             print add(1,2,3);",
        );

        match res {
            Ok(output) => assert_eq!(output, "'Hi, Dear Reader!'\n6"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_implict_nil_return_1() {
        let res = evaluate(
            "fun f() { return; }\n\
             print f();",
        );

        match res {
            Ok(output) => assert_eq!(output, "nil"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_implict_nil_return_2() {
        let res = evaluate(
            "fun f() { }\n\
             print f();",
        );

        match res {
            Ok(output) => assert_eq!(output, "nil"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_scopes() {
        let res = evaluate(
            "var a = \"global a\";\
                            var b = \"global b\";\n\
                            var c = \"global c\";\n\
                            {
                              var a = \"outer a\";\n\
                              var b = \"outer b\";\n\
                              {
                                var a = \"inner a\";\n\
                                print a;\n\
                                print b;\n\
                                print c;\n\
                              }
                              print a;\n\
                              print b;\n\
                              print c;\n\
                            }
                            print a;\n\
                            print b;\n\
                            print c;\n",
        );

        match res {
            Ok(output) => assert_eq!(
                output,
                "'inner a'\n\
                 'outer b'\n\
                 'global c'\n\
                 'outer a'\n\
                 'outer b'\n\
                 'global c'\n\
                 'global a'\n\
                 'global b'\n\
                 'global c'"
            ),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_implicit_return_nil() {
        let res = evaluate("fun f() {} print f();");

        match res {
            Ok(output) => assert_eq!(output, "nil"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_closures_1() {
        let res = evaluate(
            "fun f(n) {\n\
               var m = 2;\n\
               fun g(p) {\n\
                 return p + m;\n\
               }\n\
               return g(n);\n\
             }\n\
             print f(1);",
        );

        match res {
            Ok(output) => assert_eq!(output, "3"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_closures_2() {
        let res = evaluate(
            "fun mkfun(n) {\n\
               fun f(m) {\n\
                 return m + n;\n\
                 }\n\
               return f;\n\
               }\n\
             print mkfun(2)(3);",
        );
        match res {
            Ok(output) => assert_eq!(output, "5"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_classes_1() {
        let res = evaluate(
            "class DevonshireCream {\n\
               serveOn() {\n\
                 return \"Scones\";\n\
               }\n\
             }\n\
             \n\
             print DevonshireCream;",
        );

        match res {
            Ok(output) => assert_eq!(output, "LoxClass(DevonshireCream)"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_classes_2() {
        let res = evaluate(
            "class DevonshireCream {\n\
               serveOn() {\n\
                 return \"Scones\";\n\
               }\n\
             }\n\
             \n\
             var inst = DevonshireCream();\n\
             print inst;",
        );

        match res {
            Ok(output) => assert_eq!(output, "LoxInstance(DevonshireCream)"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_setattr_1() {
        let res = evaluate(
            "class Foo {}\n\
             var foo = Foo();\n\
             foo.attr = 42;\n\
             print foo.attr;",
        );

        match res {
            Ok(output) => assert_eq!(output, "42"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_setattr_2() {
        let res = evaluate(
            "class Bar {}\n\
             class Foo {}\n\
             var foo = Foo();\n\
             foo.bar = Bar();\n\
             foo.bar.baz = \"baz\";\n\
             print foo.bar.baz;",
        );

        match res {
            Ok(output) => assert_eq!(output, "\'baz\'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_methods_1() {
        let res = evaluate(
            "class Bacon {\
                eat() {\
                  print \"Crunch crunch crunch!\";\
                }\
              }\
              \
              Bacon().eat();",
        );

        match res {
            Ok(output) => assert_eq!(output, "\'Crunch crunch crunch!\'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_method_this_binding_1() {
        let res = evaluate(
            "class Cake {\
               taste() {\
                 var adjective = \"delicious\";\
                 print \"The \" + this.flavor + \" cake is \" + adjective + \"!\";\
               }\
             }\
             \
             var cake = Cake();\
             cake.flavor = \"German chocolate\";\
             cake.taste();",
        );

        match res {
            Ok(output) => assert_eq!(output, "\'The German chocolate cake is delicious!\'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_method_this_binding_2() {
        let res = evaluate(
            "class Thing {\
               getCallback() {\
                 fun localFunction() {\
                   print this;\
                 }\
                 \
                 return localFunction;\
               }\
             }\
             \
             var callback = Thing().getCallback();\
             callback();",
        );

        match res {
            Ok(output) => assert_eq!(output, "LoxInstance(Thing)"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_method_this_binding_3() {
        let res = evaluate(
            "class Foo {\n
               init(x) {\n\
                 this.x = x;\n\
               }\n\
               getX() {\n\
                 return this.x;\n\
               }\n\
             }\n\
             \n\
             var foo = Foo(42);
             print foo.getX();",
        );

        match res {
            Ok(output) => assert_eq!(output, "42"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_init_1() {
        let res = evaluate(
            "class Foo {\
               init(val) {\
                 this.val = val;\
               }\
             }\
             \
             var foo = Foo(42);\
             print foo.val;",
        );

        match res {
            Ok(output) => assert_eq!(output, "42"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_explicit_call_init() {
        let res = evaluate(
            "class Foo {\
               init(val) {\
                 this.val = val;\
               }\
             }\
             \
             var foo1 = Foo(42);\
             print foo1.val;\
             var foo2 = foo1.init(1337);\
             print foo2.val;\
             print foo1.val;",
        );

        match res {
            Ok(output) => assert_eq!(output, "42\n1337\n1337"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_early_return_init() {
        let res = evaluate(
            "class Foo {\n\
               init(val) {\n\
                 if (val > 100) {\n\
                   this.val = 100;\n\
                   return;\n\
                 }\n\
                 this.val = val;\n\
               }\n\
             }\n\
             \n\
             var foo1 = Foo(42);\n\
             print foo1.val;\n\
             var foo2 = Foo(200);\n\
             print foo2.val;",
        );

        match res {
            Ok(output) => assert_eq!(output, "42\n100"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_return_non_nil_in_init() {
        let res = evaluate(
            "class Foo {\n\
               init(val) {\n\
                 return 42;\n\
               }\n\
             }\n\
             \n\
             var foo = Foo(42);",
        );

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => assert_eq!(
                err,
                "TypeError: init should only return nil (perhaps implicitly), not Number"
            ),
        }
    }

    #[test]
    fn class_cannot_inherit_from_itself() {
        let res = evaluate("class Oops < Oops {}");

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => assert!(err.starts_with("A class cannot inerit from itself")),
        }
    }

    #[test]
    fn only_classes_can_be_superclasses() {
        let res = evaluate("var x = 42; class Oops < x {}");

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => assert!(err.starts_with("Only classes should appear as superclasses.")),
        }
    }

    #[test]
    fn method_inheritance_1() {
        let res = evaluate(
            "class A {\n\
               f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B < A {}\n\
             var b = B();\n\
             print b.f();",
        );

        match res {
            Ok(output) => assert_eq!(output, "\'cat\'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn method_inheritance_2() {
        let res = evaluate(
            "class A {\n\
               f() {\n\
                 return \"cat\";\n\
               }\n\
             }\n\
             class B < A {}\n\
             class C < B {}\n\
             var c = C();\n\
             print c.f();",
        );

        match res {
            Ok(output) => assert_eq!(output, "\'cat\'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn method_inheritance_3() {
        let res = evaluate(
            "class A {\n\
               f() {\n\
                 return this.attr;
               }\n\
             }\n\
             class B < A {\n\
               init(attr) {\n\
                 this.attr = attr;\n\
               }\n\
             }\n\
             var b = B(42);\n\
             print b.f();",
        );

        match res {
            Ok(output) => assert_eq!(output, "42"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn method_inheritance_4() {
        let res = evaluate(
            "class A {\n\
               f() {\n\
                 return this.attr;
               }\n\
             }\n\
             class B < A {\n\
             }\n\
             var b = B();\n\
             b.attr = 42;
             print b.f();",
        );

        match res {
            Ok(output) => assert_eq!(output, "42"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn illegal_super_expressions_1() {
        let res = evaluate("super + 1");

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => assert!(err.starts_with("Expected token Dot")),
        }
    }

    #[test]
    fn illegal_super_expressions_2() {
        let res = evaluate("fun f() { return super.g(); }\nprint f();");

        match res {
            Ok(output) => panic!("{}", output),
            Err(err) => {
                assert!(err.starts_with("Super expression not enclosed in a method definition"))
            }
        }
    }

    #[test]
    fn test_super_1() {
        let res = evaluate(
            "class A {\n\
               method() {\n\
                 print \"A method\";\n\
               }\n\
             }\n\
             \n\
             class B < A {\n\
               method() {\n\
                 print \"B method\";\n\
               }\n\
               \n\
               test() {\n\
                 super.method();\n\
               }\n\
             }\n\
             \n\
             class C < B {}\n\
             \n\
             C().test();",
        );

        match res {
            Ok(output) => assert_eq!(output, "'A method'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_super_2() {
        let res = evaluate(
            "class A {\n\
               method() {\n\
                 print \"A method\";\n\
               }\n\
             }\n\
             \n\
             class B < A {\n\
               method() {\n\
                 print \"B method\";\n\
               }\n\
               \n\
               test() {\n\
                 var method = super.method;\n\
                 method();\n\
               }\n\
             }\n\
             \n\
             class C < B {}\n\
             \n\
             C().test();",
        );

        match res {
            Ok(output) => assert_eq!(output, "'A method'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_super_3() {
        let res = evaluate(
            "class A {\n\
               f() {\n\
                 return this.attr;
               }\n\
             }\n\
             class B < A {\n\
               init(attr) {\n\
                 this.attr = attr;\n\
               }\n\
               f() {\n\
                 return 1337;
               }\n\
               g() {\n\
                 return super.f();\n\
               }\n\
             }\n\
             var b = B(42);\n\
             print b.g();",
        );

        match res {
            Ok(output) => assert_eq!(output, "42"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_late_binding() {
        let res = evaluate(
            "fun a() { b(); }\n\
             fun b() { print \"hello world\"; }\n\
             \n\
             a();\n",
        );

        match res {
            Ok(output) => assert_eq!(output, "'hello world'"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_list_construction() {
        let res = evaluate("print([1,2,3]);");

        match res {
            Ok(output) => assert_eq!(output, "[1, 2, 3]"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_empty_list_construction() {
        let res = evaluate("print([]);");

        match res {
            Ok(output) => assert_eq!(output, "[]"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_list_concat() {
        let res = evaluate("print([1,2,3] + [4,5,6]);");

        match res {
            Ok(output) => assert_eq!(output, "[1, 2, 3, 4, 5, 6]"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_len() {
        let res = evaluate(
            "print(len(\"\")); \n\
             print(len(\"cat\")); \n\
             print(len([])); \n\
             print(len([1,2,3,4]));",
        );

        match res {
            Ok(output) => assert_eq!(output, "0\n3\n0\n4"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_for_each() {
        let res = evaluate(
            "fun f(arg) { print arg; } \n\
             forEach([1,2,3,4], f);",
        );

        match res {
            Ok(output) => assert_eq!(output, "1\n2\n3\n4"),
            Err(err) => panic!("{}", err),
        }
    }

    #[test]
    fn test_map() {
        let res = evaluate(
            "fun incr(x) { return x + 1; } \n\
             print(map(incr, [1,2,3,4]));",
        );

        match res {
            Ok(output) => assert_eq!(output, "[2, 3, 4, 5]"),
            Err(err) => panic!("{}", err),
        }
    }
}
