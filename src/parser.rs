use crate::expr;
use crate::scanner;

use std::fmt;

struct Parser {
    tokens: Vec<scanner::Token>,
    current: usize,
    in_subdec: bool,
    statements: Vec<expr::Stmt>,
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            tokens: Vec::new(),
            current: 0,
            in_subdec: false,
            statements: Vec::new(),
        }
    }
}

pub enum Error {
    UnexpectedToken(scanner::Token),
    TokenMismatch {
        expected: scanner::TokenType,
        found: scanner::Token,
        maybe_on_err_string: Option<String>,
    },
    ReturnNotInSub {
        line: usize,
        col: i64,
    },
    RestartNotInSub {
        line: usize,
        col: i64,
    },
    InvalidAssignment {
        line: usize,
        col: i64,
    },
    TooManyArguments {
        line: usize,
        col: i64,
    },
    ExpectedExpression {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
    DuplicateProperty {
        property: String,
        line: usize,
        col: i64,
    },
    UnkownProperty {
        property: String,
        line: usize,
        col: i64,
    },
    InvalidTokenInUnaryOp {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
    InvalidTokenInBinaryOp {
        token_type: scanner::TokenType,
        line: usize,
        col: i64,
    },
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Error::UnexpectedToken(tok) => write!(
                f,
                "Unexpected token {:?} at line={},col={}",
                tok.ty, tok.line, tok.col
            ),
            Error::TokenMismatch {
                maybe_on_err_string,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Expected token {:?} but found {:?} at line={},col={}",
                    expected, found.ty, found.line, found.col
                )?;
                if let Some(on_err_string) = maybe_on_err_string {
                    write!(f, ": {}", on_err_string)?;
                }
                fmt::Result::Ok(())
            }
            Error::RestartNotInSub { line, col } => write!(
                f,
                "restart statement not enclosed in a SubDecl at line={},col={}",
                line, col
            ),
            Error::ReturnNotInSub { line, col } => write!(
                f,
                "return statement not enclosed in a SubDecl at line={},col={}",
                line, col
            ),
            Error::InvalidAssignment { line, col } => {
                write!(f, "invalid assignment target at line={},col={}", line, col)
            }
            Error::TooManyArguments { line, col } => write!(
                f,
                "Cannot have more than 255 arguments to a function call. Line={},col={}",
                line, col
            ),
            Error::ExpectedExpression {
                token_type,
                line,
                col,
            } => write!(
                f,
                "Expected expression, but found token {:?} at line={},col={}",
                token_type, line, col
            ),
            Error::DuplicateProperty {
                property,
                line,
                col,
            } => write!(
                f,
                "Duplicate property {:?} at line={},col={}",
                property, line, col
            ),
            Error::UnkownProperty {
                property,
                line,
                col,
            } => write!(
                f,
                "Unknown property {:?} at line={},col={}",
                property, line, col
            ),
            Error::InvalidTokenInUnaryOp {
                token_type,
                line,
                col,
            } => write!(
                f,
                "invalid token in unary op {:?} at line={},col={}",
                token_type, line, col
            ),
            Error::InvalidTokenInBinaryOp {
                token_type,
                line,
                col,
            } => write!(
                f,
                "invalid token in binary op {:?} at line={},col={}",
                token_type, line, col
            ),
        }
    }
}

#[derive(Debug)]
pub enum SubKind {
    Function,
}

pub fn parse(tokens: Vec<scanner::Token>) -> Result<Vec<expr::Stmt>, Error> {
    let mut p = Parser {
        tokens,
        ..Parser::default()
    };
    let stmts_or_err = p.parse();

    match stmts_or_err {
        Ok(stmts_or_err) => {
            if p.is_at_end() {
                Ok(stmts_or_err)
            } else {
                let tok = &p.tokens[p.current];
                Err(Error::UnexpectedToken(tok.clone()))
            }
        }
        Err(err) => Err(err),
    }
}

impl Parser {
    pub fn parse(&mut self) -> Result<Vec<expr::Stmt>, Error> {
        // let mut statements: Vec<expr::Stmt> = Vec::new();

        while !self.is_at_end() {
            let stmt = self.declaration()?;
            self.statements.push(stmt);
        }

        Ok(self.statements.clone())
    }

    fn declaration(&mut self) -> Result<expr::Stmt, Error> {
        if self.matches(scanner::TokenType::Sub) {
            return Ok(expr::Stmt::SubDecl(self.sub_decl(&SubKind::Function)?));
        }

        if self.matches(scanner::TokenType::Acl) {
            // TODO: finish
            // return Ok(expr::Stmt::Acl(self.acl_decl()?));
        }

        if self.matches(scanner::TokenType::Backend) {
            return Ok(expr::Stmt::Backend(Box::new(self.backend_decl()?)));
        }

        if self.matches(scanner::TokenType::Director) {
            return Ok(expr::Stmt::Director(Box::new(self.director_decl()?)));
        }
        
        if self.matches(scanner::TokenType::Table) {
            return Ok(expr::Stmt::Table(self.table_decl()?));
        }

        self.statement()
    }

    fn declare_statement(&mut self) -> Result<expr::Stmt, Error> {
        let var_token = self
            .consume(scanner::TokenType::Identifier, "Expected var. prefix")?
            .clone();
        if var_token.lexeme != b"var" {
            return Err(Error::TokenMismatch {
                expected: scanner::TokenType::Identifier,
                found: self.peek().clone(),
                maybe_on_err_string: Some("Expected var. prefix".to_string()),
            });
        }

        self.consume(scanner::TokenType::Dot, "Expected . after var prefix")?;

        let name_token = self
            .consume(scanner::TokenType::Identifier, "Expected variable name")?
            .clone();

        let type_token = self
            .consume(scanner::TokenType::Identifier, "Expected variable name")?
            .clone();
        let type_token = String::from_utf8(type_token.lexeme).unwrap();
        let var_type = match type_token.as_str() {
            "ACL" => expr::Type::Acl,
            "BACKEND" => expr::Type::Backend,
            "BOOL" => expr::Type::Bool,
            "FLOAT" => expr::Type::Float,
            "ID" => expr::Type::Id,
            "INTEGER" => expr::Type::Integer,
            "IP" => expr::Type::Ip,
            "RTIME" => expr::Type::Rtime,
            "STRING" => expr::Type::String,
            "TIME" => expr::Type::Time,
            _ => {
                return Err(Error::TokenMismatch {
                    expected: scanner::TokenType::Identifier,
                    found: self.peek().clone(),
                    maybe_on_err_string: Some(format!("Invalid Type: {} - Needs to be one of ACL BACKEND BOOL FLOAT ID INTEGER IP RTIME STRING TIME", type_token)),
                });
            }
        };

        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after variable declaration",
        )?;

        Ok(expr::Stmt::VarDecl(expr::Symbol {
            name: String::from_utf8(name_token.lexeme).unwrap(),
            line: name_token.line,
            col: name_token.col,
            var_type: Some(var_type),
        }))
    }

    fn director_decl(&mut self) -> Result<expr::Director, Error> {
        let name_tok = self
            .consume(scanner::TokenType::Identifier, "Expected director name")?
            .clone();

        let director_symbol = expr::Symbol {
            name: String::from_utf8(name_tok.lexeme).unwrap(),
            line: name_tok.line,
            col: name_tok.col,
            var_type: Some(expr::Type::Director),
        };

        let type_tok = self
            .consume(scanner::TokenType::Identifier, "Expected director type")?
            .clone();

        let director_type = match String::from_utf8(type_tok.clone().lexeme).unwrap().as_ref() {
            "random" => expr::DirectorType::Random,
            "fallback" => expr::DirectorType::Fallback,
            "hash" => expr::DirectorType::Hash,
            "client" => expr::DirectorType::Client,
            "chash" => expr::DirectorType::ConsistentHash,
            invalid => {
                return Err(Error::TokenMismatch {
                    expected: scanner::TokenType::Identifier,
                    found: type_tok.clone(),
                    maybe_on_err_string: Some(format!(
                        "Invalid director type given: {} line: {} col: {}",
                        invalid, type_tok.line, type_tok.col
                    )),
                });
            }
        };

        self.consume(
            scanner::TokenType::LeftBrace,
            "Expected { before director body",
        )?;
        // let saved_is_in_subdec = self.in_subdec;
        // self.in_subdec = true;
        let body = self.director_block()?;
        // self.in_subdec = saved_is_in_subdec;

        Ok(expr::Director {
            name: director_symbol,
            body,
            director_type,
        })
    }

    fn director_block(&mut self) -> Result<expr::DirectorBody, Error> {
        let mut fields: Vec<String> = Vec::new();
        let mut builder = expr::DirectorBodyBuilder::default();
        let mut backends: Vec<expr::DirectorBackend> = Vec::new();

        while !self.check(scanner::TokenType::RightBrace) && !self.is_at_end() {
            if self.matches(scanner::TokenType::Dot) {
                let token = self
                    .consume(
                        scanner::TokenType::Identifier,
                        "Expected identifier for director property name",
                    )?
                    .clone();
                let field = String::from_utf8(token.lexeme.clone()).unwrap();
                if fields.contains(&field) {
                    return Err(Error::DuplicateProperty {
                        property: field,
                        line: token.line,
                        col: token.col,
                    });
                }
                self.consume(
                    scanner::TokenType::Equal,
                    "Expected = after director property name",
                )?;
                match field.as_str() {
                    "retries" => {
                        builder.retries(self.primary()?);
                    }
                    "quorum" => {
                        builder.quorum(self.primary()?);
                    }
                    "key" => {
                        builder.key(self.primary()?);
                    }
                    "seed" => {
                        builder.seed(self.primary()?);
                    }
                    "vnodes_per_node" => {
                        builder.vnodes_per_node(self.primary()?);
                    }
                    _ => {
                        return Err(Error::UnkownProperty {
                            property: field,
                            line: token.line,
                            col: token.col,
                        });
                    }
                }
                fields.push(field);
                self.consume(
                    scanner::TokenType::Semicolon,
                    "Expected ; after director property value",
                )?;
            } else if self.matches(scanner::TokenType::LeftBrace) {
                let backend = self.director_backend()?;
                backends.push(backend);
                self.consume(
                    scanner::TokenType::RightBrace,
                    "Expected } after director backend block",
                )?;
            }
        }
        builder.backends(backends);

        self.consume(
            scanner::TokenType::RightBrace,
            "Expected } after director block.",
        )?;

        Ok(builder.build().unwrap())
    }

    fn director_backend(&mut self) -> Result<expr::DirectorBackend, Error> {
        let mut fields = Vec::new();
        let mut builder = expr::DirectorBackendBuilder::default();

        while !self.check(scanner::TokenType::RightBrace) && !self.is_at_end() {
            self.consume(
                scanner::TokenType::Dot,
                "Expected . before director backend property",
            )?;
            let token = self
                .consume(
                    scanner::TokenType::Identifier,
                    "Expected identifier for director backend property name",
                )?
                .clone();
            let field = String::from_utf8(token.lexeme.clone()).unwrap();
            if fields.contains(&field) {
                return Err(Error::DuplicateProperty {
                    property: field,
                    line: token.line,
                    col: token.col,
                });
            }
            self.consume(
                scanner::TokenType::Equal,
                "Expected = after director backend property name",
            )?;
            match field.as_str() {
                "id" => {
                    builder.id(self.primary()?);
                }
                "backend" => {
                    builder.backend(self.primary()?);
                }
                "weight" => {
                    builder.weight(self.primary()?);
                }
                _ => {
                    return Err(Error::UnkownProperty {
                        property: field,
                        line: token.line,
                        col: token.col,
                    });
                }
            }
            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ; after director backend property value",
            )?;
            fields.push(field);
        }

        Ok(builder.build().unwrap())
    }

    fn backend_decl(&mut self) -> Result<expr::Backend, Error> {
        let name_tok = self
            .consume(scanner::TokenType::Identifier, "Expected backend name")?
            .clone();

        let backend_symbol = expr::Symbol {
            name: String::from_utf8(name_tok.lexeme).unwrap(),
            line: name_tok.line,
            col: name_tok.col,
            var_type: Some(expr::Type::Backend),
        };

        self.consume(
            scanner::TokenType::LeftBrace,
            "Expected { before backend body",
        )?;
        // let saved_is_in_subdec = self.in_subdec;
        // self.in_subdec = true;
        let body = self.backend_block()?;
        // self.in_subdec = saved_is_in_subdec;

        Ok(expr::Backend {
            name: backend_symbol,
            body,
        })
    }

    fn backend_block(&mut self) -> Result<expr::BackendBody, Error> {
        let mut fields = Vec::new();
        let mut builder = expr::BackendBodyBuilder::default();

        while !self.check(scanner::TokenType::RightBrace) && !self.is_at_end() {
            self.consume(
                scanner::TokenType::Dot,
                "Expected . before backend property",
            )?;
            let token = self
                .consume(
                    scanner::TokenType::Identifier,
                    "Expected identifier for backend property name",
                )?
                .clone();
            let field = String::from_utf8(token.lexeme.clone()).unwrap();
            if fields.contains(&field) {
                return Err(Error::DuplicateProperty {
                    property: field,
                    line: token.line,
                    col: token.col,
                });
            }
            self.consume(
                scanner::TokenType::Equal,
                "Expected = after backend property name",
            )?;
            if field == "probe" {
                self.consume(
                    scanner::TokenType::LeftBrace,
                    "Expected { before probe block",
                )?;
                let value = self.probe()?;
                self.consume(
                    scanner::TokenType::RightBrace,
                    "Expected } after probe block",
                )?;
                builder.probe(value);
            } else {
                match field.as_str() {
                    "dynamic" => {
                        builder.dynamic(self.primary()?);
                    }
                    "share_key" => {
                        builder.share_key(self.primary()?);
                    }
                    "host" => {
                        builder.host(self.primary()?);
                    }
                    "port" => {
                        builder.port(self.primary()?);
                    }
                    "ssl" => {
                        builder.ssl(self.primary()?);
                    }
                    "ssl_cert_hostname" => {
                        builder.ssl_cert_hostname(self.primary()?);
                    }
                    "ssl_check_cert" => {
                        builder.ssl_check_cert(self.primary()?);
                    }
                    "ssl_sni_hostname" => {
                        builder.ssl_sni_hostname(self.primary()?);
                    }
                    "between_bytes_timeout" => {
                        builder.between_bytes_timeout(self.primary()?);
                    }
                    "connect_timeout" => {
                        builder.connect_timeout(self.primary()?);
                    }
                    "first_byte_timeout" => {
                        builder.first_byte_timeout(self.primary()?);
                    }
                    "max_connections" => {
                        builder.max_connections(self.primary()?);
                    }
                    "host_header" => {
                        builder.host_header(self.primary()?);
                    }
                    "always_use_host_header" => {
                        builder.always_use_host_header(self.primary()?);
                    }
                    _ => {
                        return Err(Error::UnkownProperty {
                            property: field,
                            line: token.line,
                            col: token.col,
                        });
                    }
                }
                self.consume(
                    scanner::TokenType::Semicolon,
                    "Expected ; after backend property value",
                )?;
            }
            fields.push(field);
        }

        self.consume(
            scanner::TokenType::RightBrace,
            "Expected } after backend block.",
        )?;

        Ok(builder.build().unwrap())
    }

    fn probe(&mut self) -> Result<expr::Probe, Error> {
        let mut fields = Vec::new();
        let mut builder = expr::ProbeBuilder::default();

        while !self.check(scanner::TokenType::RightBrace) && !self.is_at_end() {
            self.consume(scanner::TokenType::Dot, "Expected . before probe property")?;
            let token = self
                .consume(
                    scanner::TokenType::Identifier,
                    "Expected identifier for probe property name",
                )?
                .clone();
            let field = String::from_utf8(token.lexeme.clone()).unwrap();
            if fields.contains(&field) {
                return Err(Error::DuplicateProperty {
                    property: field,
                    line: token.line,
                    col: token.col,
                });
            }
            self.consume(
                scanner::TokenType::Equal,
                "Expected = after probe property name",
            )?;
            match field.as_str() {
                "dummy" => {
                    builder.dummy(self.primary()?);
                }
                "request" => {
                    builder.request(self.expression()?);
                }
                "expected_response" => {
                    builder.expected_response(self.primary()?);
                }
                "interval" => {
                    builder.interval(self.primary()?);
                }
                "timeout" => {
                    builder.timeout(self.primary()?);
                }
                "window" => {
                    builder.window(self.primary()?);
                }
                "initial" => {
                    builder.initial(self.primary()?);
                }
                "threshold" => {
                    builder.threshold(self.primary()?);
                }
                _ => {
                    return Err(Error::UnkownProperty {
                        property: field,
                        line: token.line,
                        col: token.col,
                    });
                }
            }
            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ; after probe property value",
            )?;
            fields.push(field);
        }

        Ok(builder.build().unwrap())
    }

    fn table_decl(&mut self) -> Result<expr::Table, Error> {
        let name_tok = self
            .consume(scanner::TokenType::Identifier, "Expected table name")?
            .clone();

        let table_symbol = expr::Symbol {
            name: String::from_utf8(name_tok.lexeme).unwrap(),
            line: name_tok.line,
            col: name_tok.col,
            var_type: None,
        };

        self.consume(
            scanner::TokenType::LeftBrace,
            "Expected { before table body",
        )?;
        // let saved_is_in_subdec = self.in_subdec;
        // self.in_subdec = true;
        let body = self.table_block()?;
        // self.in_subdec = saved_is_in_subdec;

        Ok(expr::Table {
            name: table_symbol,
            body,
        })
    }

    fn table_block(&mut self) -> Result<Vec<expr::TableEntry>, Error> {
        let mut stmts = Vec::new();

        while !self.check(scanner::TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.table_entry()?)
        }

        self.consume(
            scanner::TokenType::RightBrace,
            "Expected } after table block.",
        )?;

        Ok(stmts)
    }

    fn table_entry(&mut self) -> Result<expr::TableEntry, Error> {
        if self.check(scanner::TokenType::String) {
            let key = self.primary()?;
            self.consume(scanner::TokenType::Colon, "Expected : after table key")?;
            if self.check(scanner::TokenType::String) {
                let value = self.primary()?;
                if self.check(scanner::TokenType::Comma) {
                    self.advance();
                }

                Ok(expr::TableEntry { key, value })
            } else {
                return Err(Error::TokenMismatch {
                    expected: scanner::TokenType::String,
                    found: self.peek().clone(),
                    maybe_on_err_string: Some("Expected String for table-entry value.".into()),
                });
            }
        } else {
            return Err(Error::TokenMismatch {
                expected: scanner::TokenType::String,
                found: self.peek().clone(),
                maybe_on_err_string: Some("Expected String for table-entry key.".into()),
            });
        }
    }

    fn sub_decl(&mut self, kind: &SubKind) -> Result<expr::SubDecl, Error> {
        let name_tok = self
            .consume(
                scanner::TokenType::Identifier,
                format!("Expected {:?} name", kind).as_ref(),
            )?
            .clone();

        let sub_symbol = expr::Symbol {
            name: String::from_utf8(name_tok.lexeme).unwrap(),
            line: name_tok.line,
            col: name_tok.col,
            var_type: None,
        };

        self.consume(
            scanner::TokenType::LeftBrace,
            "Expected { before subroutine body",
        )?;
        let saved_is_in_subdec = self.in_subdec;
        self.in_subdec = true;
        let body = self.block()?;
        self.in_subdec = saved_is_in_subdec;

        Ok(expr::SubDecl {
            name: sub_symbol,
            body,
        })
    }

    fn statement(&mut self) -> Result<expr::Stmt, Error> {
        if self.matches(scanner::TokenType::Add) {
            return self.add_statement();
        }

        if self.matches(scanner::TokenType::Call) {
            return self.call_statement();
        }

        if self.matches(scanner::TokenType::Declare) {
            return self.declare_statement();
        }
        
        if self.matches(scanner::TokenType::Error) {
            return self.error_statement();
        }

        if self.matches(scanner::TokenType::Esi) {
            return self.esi_statement();
        }

        if self.matches(scanner::TokenType::If) {
            return self.if_statement();
        }

        if self.matches(scanner::TokenType::Include) {
            return self.include_statement();
        }

        if self.matches(scanner::TokenType::Log) {
            return self.log_statement();
        }

        if self.matches(scanner::TokenType::Remove) {
            return self.unset_statement();
        }
        
        if self.matches(scanner::TokenType::Restart) {
            return self.restart_statement();
        }
        
        if self.matches(scanner::TokenType::Return) {
            return self.return_statement();
        }

        if self.matches(scanner::TokenType::Set) {
            return self.set_statement();
        }

        if self.matches(scanner::TokenType::Synthetic) {
            return self.synthetic_statement();
        }

        if self.matches(scanner::TokenType::SyntheticBase64) {
            return self.synthetic_base64_statement();
        }

        if self.matches(scanner::TokenType::Unset) {
            return self.unset_statement();
        }

        if self.matches(scanner::TokenType::LeftBrace) {
            return Ok(expr::Stmt::Block(self.block()?));
        }

        self.expression_statement()
    }

    fn call_statement(&mut self) -> Result<expr::Stmt, Error> {
        let identifier = self.consume(
            scanner::TokenType::Identifier,
            "Expected identifier after call statement.",
        )?.clone();
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after call statement",
        )?;

        Ok(expr::Stmt::Call(expr::Symbol {
            name: String::from_utf8(identifier.lexeme).unwrap(),
            line: identifier.line,
            col: identifier.col,
            var_type: None,
        }))
    }

    fn esi_statement(&mut self) -> Result<expr::Stmt, Error> {
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after esi statement",
        )?;

        Ok(expr::Stmt::Esi)
    }

    fn include_statement(&mut self) -> Result<expr::Stmt, Error> {
        let filename = self.consume(
            scanner::TokenType::String,
            "Expected string after include statement.",
        )?.clone();
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after call statement",
        )?;

        Ok(expr::Stmt::Include(expr::Literal::String(String::from_utf8(filename.lexeme).unwrap())))
    }

    fn add_statement(&mut self) -> Result<expr::Stmt, Error> {
        let identifier = self.primary()?;
        self.consume(
            scanner::TokenType::Equal,
            "Expected = after add statement variable name.",
        )?;
        let value = self.addition()?;
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after add statement",
        )?;

        Ok(expr::Stmt::Add(identifier, value))
    }
    fn synthetic_statement(&mut self) -> Result<expr::Stmt, Error> {
        let value = self.addition()?;
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after synthetic statement",
        )?;

        Ok(expr::Stmt::Synthetic(value))
    }
    fn synthetic_base64_statement(&mut self) -> Result<expr::Stmt, Error> {
        let value = self.addition()?;
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after synthetic.base64 statement",
        )?;

        Ok(expr::Stmt::SyntheticBase64(value))
    }
    fn unset_statement(&mut self) -> Result<expr::Stmt, Error> {
        let identifier = self.primary()?;
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after unset statement",
        )?;

        Ok(expr::Stmt::Unset(identifier))
    }

    fn set_statement(&mut self) -> Result<expr::Stmt, Error> {
        let mut identifier = self.primary()?;
        loop {
            if self.matches(scanner::TokenType::Dot) {
                let name_token = self.peek().clone();
                let name_tok = match name_token.ty {
                    scanner::TokenType::Integer | scanner::TokenType::Identifier => name_token,
                    _ => {
                        return Err(Error::TokenMismatch {
                            expected: scanner::TokenType::Identifier,
                            found: name_token,
                            maybe_on_err_string: Some(
                                "Expected property name after '.'.".to_string(),
                            ),
                        });
                    }
                };
                self.advance();
                identifier = expr::Expr::Get(
                    Box::new(identifier),
                    expr::Symbol {
                        name: String::from_utf8(name_tok.lexeme).unwrap(),
                        line: name_tok.line,
                        col: name_tok.col,
                        var_type: None,
                    },
                );
            } else {
                break;
            }
        }
        if self.match_one_of(
            vec![
                scanner::TokenType::Equal,
                scanner::TokenType::Subtraction,
                scanner::TokenType::Addition,
                scanner::TokenType::Multiplication,
                scanner::TokenType::Division,
                scanner::TokenType::Modulus,
                scanner::TokenType::BitwiseOr,
                scanner::TokenType::BitwiseAnd,
                scanner::TokenType::BitwiseXor,
                scanner::TokenType::LeftShift,
                scanner::TokenType::RightShift,
                scanner::TokenType::LeftRotate,
                scanner::TokenType::RightRotate,
                scanner::TokenType::LogicalAnd,
                scanner::TokenType::LogicalOr,
            ]
            .as_ref(),
        ) {
            let token = self.previous().clone();
            let assignment_type = match token.ty {
                scanner::TokenType::Equal => expr::Assignment::Assign,
                scanner::TokenType::Addition => expr::Assignment::Addition,
                scanner::TokenType::Subtraction => expr::Assignment::Subtraction,
                scanner::TokenType::Multiplication => expr::Assignment::Multiplication,
                scanner::TokenType::Division => expr::Assignment::Division,
                scanner::TokenType::Modulus => expr::Assignment::Modulus,
                scanner::TokenType::BitwiseOr => expr::Assignment::BitwiseOr,
                scanner::TokenType::BitwiseAnd => expr::Assignment::BitwiseAnd,
                scanner::TokenType::BitwiseXor => expr::Assignment::BitwiseXor,
                scanner::TokenType::LeftShift => expr::Assignment::LeftShift,
                scanner::TokenType::RightShift => expr::Assignment::RightShift,
                scanner::TokenType::LeftRotate => expr::Assignment::LeftRotate,
                scanner::TokenType::RightRotate => expr::Assignment::RightRotate,
                scanner::TokenType::LogicalAnd => expr::Assignment::LogicalAnd,
                scanner::TokenType::LogicalOr => expr::Assignment::LogicalOr,
                _ => unreachable!(
                    "Reached a token in assignment that should not be reachable: {:?}",
                    token.ty
                ),
            };
            let value = self.addition()?;

            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ; after set statement",
            )?;

            match identifier {
                expr::Expr::Get(_, _) | expr::Expr::Variable(_) => {
                    return Ok(expr::Stmt::Set(identifier, assignment_type, value));
                    // return Ok(assignment_type(Box::new(expr), Box::new(value)));
                }
                _ => {
                    println!("fff");
                    return Err(Error::InvalidAssignment {
                        line: token.line,
                        col: token.col,
                    });
                }
            }
        }
        let token = self.previous().clone();
        println!("peak: {:?}", self.peek());
        println!("identifier: {:?}", identifier);
        Err(Error::InvalidAssignment {
            line: token.line,
            col: token.col,
        })
    }

    fn error_statement(&mut self) -> Result<expr::Stmt, Error> {
        let mut status = None;
        let mut message = None;
        if self.check(scanner::TokenType::Integer) {
            status = Some(self.primary()?);
            if self.check(scanner::TokenType::String) {
                message = Some(self.primary()?);
            }
        }
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after error statement",
        )?;

        Ok(expr::Stmt::Error(expr::Error { status, message }))
    }

    fn restart_statement(&mut self) -> Result<expr::Stmt, Error> {
        let prev_tok = self.previous().clone();

        if !self.in_subdec {
            return Err(Error::RestartNotInSub {
                line: prev_tok.line,
                col: prev_tok.col,
            });
        }

        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after restart statement",
        )?;

        Ok(expr::Stmt::Restart(expr::SourceLocation {
            line: prev_tok.line,
            col: prev_tok.col,
        }))
    }

    fn return_statement(&mut self) -> Result<expr::Stmt, Error> {
        let prev_tok = self.previous().clone();

        if !self.in_subdec {
            return Err(Error::ReturnNotInSub {
                line: prev_tok.line,
                col: prev_tok.col,
            });
        }

        let maybe_retval = if self.matches(scanner::TokenType::Semicolon) {
            None
        } else {
            self.consume(scanner::TokenType::LeftParen, "Expected left paren after return statement.")?;
            let token = self.consume(
                scanner::TokenType::Identifier,
                "Expected identifier after return statement.",
            )?.clone();
            self.consume(scanner::TokenType::RightParen, "Expected right paren after return statement state.")?;
            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ; after return value",
            )?;
            let name = String::from_utf8(token.lexeme.clone()).unwrap();
            let name = match name.as_str() {
                "lookup" | "pass" | "error" | "restart" | "hash" | "deliver" | "fetch" | "deliver_stale" => name,
                _ => return Err(Error::TokenMismatch {
                    expected: scanner::TokenType::Identifier,
                    found: token,
                    maybe_on_err_string: Some(format!(
                        "Found {} - Expected one of lookup, pass, error, restart, hash, deliver, fetch, deliver_stale",
                        name
                    )),
                })
            };
            Some(expr::Symbol {
                name,
                line: token.line,
                col: token.col,
                var_type: None,
            })
        };

        Ok(expr::Stmt::Return(
            expr::SourceLocation {
                line: prev_tok.line,
                col: prev_tok.col,
            },
            maybe_retval,
        ))
    }

    fn log_statement(&mut self) -> Result<expr::Stmt, Error> {
        let expr = self.expression()?;
        self.consume(
            scanner::TokenType::Semicolon,
            "Expected ; after log statement",
        )?;

        Ok(expr::Stmt::Log(expr))
    }

    fn if_statement(&mut self) -> Result<expr::Stmt, Error> {
        self.consume(scanner::TokenType::LeftParen, "Expected ( after if.")?;
        let cond = self.expression()?;
        self.consume(
            scanner::TokenType::RightParen,
            "Expected ) after if condition.",
        )?;
        let then_branch = Box::new(self.statement()?);
        let maybe_else_branch = if self.matches(scanner::TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else if self.matches(scanner::TokenType::ElseIf) {
            Some(Box::new(self.if_statement()?))
        } else {
            None
        };

        Ok(expr::Stmt::If(cond, then_branch, maybe_else_branch))
    }

    fn block(&mut self) -> Result<Vec<expr::Stmt>, Error> {
        let mut stmts = Vec::new();

        while !self.check(scanner::TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?)
        }

        self.consume(scanner::TokenType::RightBrace, "Expected } after block.")?;

        Ok(stmts)
    }

    fn expression_statement(&mut self) -> Result<expr::Stmt, Error> {
        let expr = self.expression()?;
        if self.peek().ty == scanner::TokenType::Semicolon {
            self.consume(
                scanner::TokenType::Semicolon,
                "Expected ; after expression/statement",
            )?;
        }
        Ok(expr::Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Result<expr::Expr, Error> {
        self.or()
    }

    fn or(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.and()?;

        while self.matches(scanner::TokenType::Or) {
            let right = self.and()?;
            expr = expr::Expr::Logical(Box::new(expr), expr::LogicalOp::Or, Box::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.equality()?;

        while self.matches(scanner::TokenType::And) {
            let right = self.equality()?;
            expr = expr::Expr::Logical(Box::new(expr), expr::LogicalOp::And, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.addition()?;

        while self.match_one_of(
            vec![
                scanner::TokenType::Greater,
                scanner::TokenType::GreaterEqual,
                scanner::TokenType::Less,
                scanner::TokenType::LessEqual,
            ]
            .as_ref(),
        ) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.addition()?);
            let binop_maybe = Self::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn addition(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.unary()?;

        while self.match_one_of(vec![scanner::TokenType::Plus, scanner::TokenType::String].as_ref())
        {
            let operator_token = self.previous().clone();
            let right = if operator_token.ty == scanner::TokenType::String {
                Box::new(Self::string(&operator_token)?)
            } else {
                Box::new(self.unary()?)
            };
            let binop_maybe = Self::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => {
                    if self.check(scanner::TokenType::Semicolon) {
                        return Ok(expr);
                    }
                    return Err(err);
                }
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<expr::Expr, Error> {
        if self.matches(scanner::TokenType::Bang) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.unary()?);
            let unary_op_maybe = Self::op_token_to_unary_op(&operator_token);

            return match unary_op_maybe {
                Ok(unary_op) => Ok(expr::Expr::Unary(unary_op, right)),
                Err(err) => Err(err),
            };
        }
        self.call()
    }

    fn call(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(scanner::TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.matches(scanner::TokenType::Dot) {
                let name_token = self.peek().clone();
                let name_tok = match name_token.ty {
                    scanner::TokenType::Integer | scanner::TokenType::Identifier => name_token,
                    _ => {
                        return Err(Error::TokenMismatch {
                            expected: scanner::TokenType::Identifier,
                            found: name_token,
                            maybe_on_err_string: Some(
                                "Expected property name after '.'.".to_string(),
                            ),
                        });
                    }
                };
                self.advance();
                expr = expr::Expr::Get(
                    Box::new(expr),
                    expr::Symbol {
                        name: String::from_utf8(name_tok.lexeme).unwrap(),
                        line: name_tok.line,
                        col: name_tok.col,
                        var_type: None,
                    },
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: expr::Expr) -> Result<expr::Expr, Error> {
        let mut arguments = Vec::new();

        if !self.check(scanner::TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let peek_tok = self.peek();
                    return Err(Error::TooManyArguments {
                        line: peek_tok.line,
                        col: peek_tok.col,
                    });
                }
                arguments.push(self.expression()?);
                if !self.matches(scanner::TokenType::Comma) {
                    break;
                }
            }
        }

        let token = self.consume(
            scanner::TokenType::RightParen,
            "Expected ) after arguments.",
        )?;

        Ok(expr::Expr::Call(
            Box::new(callee),
            expr::SourceLocation {
                line: token.line,
                col: token.col,
            },
            arguments,
        ))
    }

    fn integer(&mut self) -> Result<expr::Expr, Error> {
        match &self.previous().literal {
            Some(scanner::Literal::Integer(n)) => {
                Ok(expr::Expr::Literal(expr::Literal::Integer(*n)))
            }
            Some(l) => panic!(
                "internal error in parser: when parsing integer, found literal {:?}",
                l
            ),
            None => panic!("internal error in parser: when parsing integer, found no literal"),
        }
    }

    fn float(&mut self) -> Result<expr::Expr, Error> {
        match &self.previous().literal {
            Some(scanner::Literal::Float(n)) => Ok(expr::Expr::Literal(expr::Literal::Float(*n))),
            Some(l) => panic!(
                "internal error in parser: when parsing float, found literal {:?}",
                l
            ),
            None => panic!("internal error in parser: when parsing float, found no literal"),
        }
    }

    fn duration(&mut self) -> Result<expr::Expr, Error> {
        match &self.previous().literal {
            Some(scanner::Literal::Duration(n, unit)) => {
                Ok(expr::Expr::Literal(expr::Literal::Duration(
                    *n,
                    match *unit {
                        scanner::DurationUnit::Milliseconds => expr::DurationUnit::Milliseconds,
                        scanner::DurationUnit::Seconds => expr::DurationUnit::Seconds,
                        scanner::DurationUnit::Minutes => expr::DurationUnit::Minutes,
                        scanner::DurationUnit::Hours => expr::DurationUnit::Hours,
                        scanner::DurationUnit::Days => expr::DurationUnit::Days,
                        scanner::DurationUnit::Years => expr::DurationUnit::Years,
                    },
                )))
            }
            Some(l) => panic!(
                "internal error in parser: when parsing duration, found literal {:?}",
                l
            ),
            None => panic!("internal error in parser: when parsing duration, found no literal"),
        }
    }

    fn identifier(&mut self) -> Result<expr::Expr, Error> {
        let token = self.previous().clone();
        let v = String::from_utf8(token.lexeme.clone()).unwrap();
        let name_token;
        match v.as_str() {
            "req" | "bereq" | "obj" | "resp" | "beresp" => {
                let mut identifier = expr::Expr::Variable(expr::Symbol {
                    name: v.clone(),
                    line: self.previous().line,
                    col: self.previous().col,
                    var_type: None,
                });
                loop {
                    if self.matches(scanner::TokenType::Dot) {
                        let name_token = self.peek().clone();
                        let name_tok = match name_token.ty {
                            scanner::TokenType::Integer | scanner::TokenType::Identifier => name_token,
                            _ => {
                                return Err(Error::TokenMismatch {
                                    expected: scanner::TokenType::Identifier,
                                    found: name_token,
                                    maybe_on_err_string: Some(
                                        "Expected property name after '.'.".to_string(),
                                    ),
                                });
                            }
                        };
                        self.advance();
                        identifier = expr::Expr::Get(
                            Box::new(identifier),
                            expr::Symbol {
                                name: String::from_utf8(name_tok.lexeme).unwrap(),
                                line: name_tok.line,
                                col: name_tok.col,
                                var_type: None,
                            },
                        );
                    } else {
                        // break;
                        return Ok(identifier);
                    }
                }
            }
            "var" => {
                self.consume(scanner::TokenType::Dot, "Expected . after var prefix")?;
                name_token = self
                    .consume(scanner::TokenType::Identifier, "Expected variable name")?
                    .clone();
                    match &name_token.literal {
                        Some(scanner::Literal::Identifier(s)) => {
                            return Ok(expr::Expr::Variable(expr::Symbol {
                                name: s.clone(),
                                line: self.previous().line,
                                col: self.previous().col,
                                var_type: None,
                            }))
                        }
                        Some(l) => panic!(
                            "internal error in parser: when parsing identifier, found literal {:?}",
                            l
                        ),
                        None => {
                            panic!("internal error in parser: when parsing identifier, found no literal")
                        }
                    }
            }
            _ => {
                return Err(Error::TokenMismatch {
                    expected: scanner::TokenType::Identifier,
                    found: token,
                    maybe_on_err_string: Some(format!(
                        "Found {} - Expected one of var req bereq obj resp beresp",
                        v
                    )),
                });
            }
        }
    }

    fn if_expr(&mut self) -> Result<expr::Expr, Error> {
        self.consume(scanner::TokenType::LeftParen, "Expected ( after if.")?;
        let cond = self.expression()?;
        self.consume(
            scanner::TokenType::Comma,
            "Expected comma after if function condition.",
        )?;
        let then = self.expression()?;
        self.consume(
            scanner::TokenType::Comma,
            "Expected comma after if function then value.",
        )?;
        let else_branch = self.expression()?;

        self.consume(
            scanner::TokenType::RightParen,
            "Expected ')' after if function.",
        )?;

        Ok(expr::Expr::If(
            Box::new(cond),
            Box::new(then),
            Box::new(else_branch),
        ))
    }

    fn colon(&mut self) -> Result<expr::Expr, Error> {
        let prev = self.statements.pop().unwrap();
        let prev = match prev {
            expr::Stmt::Expr(e) => e,
            _ => {
                panic!("internal error in parser: can only use subfield accessor on expressions")
            }
        };
        let field = self.consume(
            scanner::TokenType::Identifier,
            "Expected identifier after subfield accessor.",
        )?;
        let field_name = match &field.literal {
            Some(scanner::Literal::Identifier(s)) => {
                expr::Expr::Literal(expr::Literal::String(s.clone()))
            }
            Some(l) => panic!(
                "internal error in parser: when parsing identifier, found literal {:?}",
                l
            ),
            None => {
                panic!("internal error in parser: when parsing identifier, found no literal")
            }
        };

        return Ok(expr::Expr::Call(
            Box::new(expr::Expr::Variable(expr::Symbol {
                name: "subfield".to_string(),
                line: self.previous().line,
                col: self.previous().col,
                var_type: None,
            })),
            expr::SourceLocation {
                line: self.previous().line,
                col: self.previous().col,
            },
            vec![prev, field_name],
        ));
    }

    fn primary(&mut self) -> Result<expr::Expr, Error> {
        if self.matches(scanner::TokenType::False) {
            return Ok(expr::Expr::Literal(expr::Literal::False));
        }
        if self.matches(scanner::TokenType::True) {
            return Ok(expr::Expr::Literal(expr::Literal::True));
        }
        if self.matches(scanner::TokenType::Integer) {
            return self.integer();
        }
        if self.matches(scanner::TokenType::Float) {
            return self.float();
        }
        if self.matches(scanner::TokenType::Duration) {
            return self.duration();
        }
        if self.matches(scanner::TokenType::String) {
            return Self::string(self.previous());
        }
        if self.matches(scanner::TokenType::Identifier) {
            return self.identifier();
        }
        if self.matches(scanner::TokenType::LeftParen) {
            let expr = Box::new(self.expression()?);
            if let Err(err) = self.consume(
                scanner::TokenType::RightParen,
                "Expected ')' after expression.",
            ) {
                return Err(err);
            }
            return Ok(expr::Expr::Grouping(expr));
        }
        if self.matches(scanner::TokenType::If) {
            return self.if_expr();
        }
        if self.matches(scanner::TokenType::Colon) {
            return self.colon();
        }

        Err(Error::ExpectedExpression {
            token_type: self.peek().ty,
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn string(token: &scanner::Token) -> Result<expr::Expr, Error> {
        match &token.literal {
            Some(scanner::Literal::Str(s)) => {
                Ok(expr::Expr::Literal(expr::Literal::String(s.clone())))
            }
            Some(l) => panic!(
                "internal error in parser: when parsing string, found literal {:?}",
                l
            ),
            None => panic!("internal error in parser: when parsing string, found no literal"),
        }
    }

    fn consume(
        &mut self,
        tok: scanner::TokenType,
        on_err_str: &str,
    ) -> Result<&scanner::Token, Error> {
        if self.check(tok) {
            return Ok(self.advance());
        }
        Err(Error::TokenMismatch {
            expected: tok,
            found: self.peek().clone(),
            maybe_on_err_string: Some(on_err_str.into()),
        })
    }

    const fn op_token_to_unary_op(tok: &scanner::Token) -> Result<expr::UnaryOp, Error> {
        match tok.ty {
            scanner::TokenType::Bang => Ok(expr::UnaryOp {
                ty: expr::UnaryOpTy::Bang,
                line: tok.line,
                col: tok.col,
            }),
            _ => Err(Error::InvalidTokenInUnaryOp {
                token_type: tok.ty,
                line: tok.line,
                col: tok.col,
            }),
        }
    }

    fn equality(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.match_regex_or_acl()?;

        while self.match_one_of(
            vec![
                scanner::TokenType::BangEqual,
                scanner::TokenType::EqualEqual,
            ]
            .as_ref(),
        ) {
            let operator_token = self.previous().clone();
            let right = Box::new(self.match_regex_or_acl()?);

            let binop_maybe = Self::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    fn match_regex_or_acl(&mut self) -> Result<expr::Expr, Error> {
        let mut expr = self.comparison()?;

        while self
            .match_one_of(vec![scanner::TokenType::BangTilde, scanner::TokenType::Tilde].as_ref())
        {
            let operator_token = self.previous().clone();
            let right = Box::new(self.comparison()?);

            let binop_maybe = Self::op_token_to_binop(&operator_token);

            match binop_maybe {
                Ok(binop) => {
                    let left = Box::new(expr);
                    expr = expr::Expr::Binary(left, binop, right);
                }
                Err(err) => return Err(err),
            }
        }
        Ok(expr)
    }

    const fn op_token_to_binop(tok: &scanner::Token) -> Result<expr::BinaryOp, Error> {
        match tok.ty {
            scanner::TokenType::EqualEqual => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::EqualEqual,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::BangEqual => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::NotEqual,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::BangTilde => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::NotMatch,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::Tilde => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Match,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::Less => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Less,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::LessEqual => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::LessEqual,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::Greater => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Greater,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::GreaterEqual => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::GreaterEqual,
                line: tok.line,
                col: tok.col,
            }),
            scanner::TokenType::String | scanner::TokenType::Plus => Ok(expr::BinaryOp {
                ty: expr::BinaryOpTy::Plus,
                line: tok.line,
                col: tok.col,
            }),
            _ => Err(Error::InvalidTokenInBinaryOp {
                token_type: tok.ty,
                line: tok.line,
                col: tok.col,
            }),
        }
    }

    fn match_one_of(&mut self, types: &[scanner::TokenType]) -> bool {
        for ty in types {
            if self.matches(*ty) {
                return true;
            }
        }
        false
    }

    fn matches(&mut self, ty: scanner::TokenType) -> bool {
        if self.check(ty) {
            self.advance();
            return true;
        }
        false
    }

    fn check(&self, ty: scanner::TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().ty == ty
    }

    fn advance(&mut self) -> &scanner::Token {
        if !self.is_at_end() {
            self.current += 1
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().ty == scanner::TokenType::Eof
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &scanner::Token {
        &self.tokens[self.current - 1]
    }
}
