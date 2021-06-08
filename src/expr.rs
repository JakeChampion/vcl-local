use hyper::{Method};

use derive_builder::Builder;
#[derive(Debug, Clone)]
pub enum Assignment {
    Assign,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    LeftShift,
    RightShift,
    LeftRotate,
    RightRotate,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Call(Box<Expr>, SourceLocation, Vec<Expr>),
    Get(Box<Expr>, Symbol),
    Grouping(Box<Expr>),
    Variable(Symbol),
    Logical(Box<Expr>, LogicalOp, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub struct SourceLocation {
    pub line: usize,
    pub col: i64,
}

#[derive(Debug, Clone)]
pub enum LogicalOp {
    Or,
    And,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Symbol {
    pub name: String,
    pub line: usize,
    pub col: i64,
    pub var_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct SubDecl {
    pub name: Symbol,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Backend {
    pub name: Symbol,
    pub body: BackendBody,
}

#[derive(Default, Debug, Clone, Builder)]
pub struct BackendBody {
    #[builder(setter(strip_option), default)]
    pub dynamic: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub share_key: Option<Expr>,

    // server location
    #[builder(setter(strip_option), default)]
    pub host: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub port: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub ssl: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub ssl_cert_hostname: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub ssl_check_cert: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub ssl_sni_hostname: Option<Expr>,

    // timeouts and limits
    #[builder(setter(strip_option), default)]
    pub between_bytes_timeout: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub connect_timeout: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub first_byte_timeout: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub max_connections: Option<Expr>,

    // host header override
    #[builder(setter(strip_option), default)]
    pub host_header: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub always_use_host_header: Option<Expr>,

    // healthcheck
    #[builder(setter(strip_option), default)]
    pub probe: Option<Healthcheck>,
}

#[derive(Default, Debug, Clone, Builder)]
pub struct Healthcheck {
    #[builder(setter(strip_option), default)]
    pub dummy: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub request: Option<Probe>,
    #[builder(setter(strip_option), default)]
    pub expected_response: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub interval: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub timeout: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub window: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub initial: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub threshold: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Probe {
    pub(crate) method: Method,
    pub(crate) scheme: Scheme,
    pub(crate) path: String,
    pub(crate) headers: Vec<(String, String)>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Scheme {
    Http11,
    Https12,
}

impl From<&str> for Scheme {
    fn from(i: &str) -> Self {
        match i.to_lowercase().as_str() {
            "http/1.1" => Self::Http11,
            "http/1.2" => Self::Https12,
            other => unimplemented!("no other schemes supported - given: {}", other),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Director {
    pub name: Symbol,
    pub body: DirectorBody,
    pub director_type: DirectorType,
}

#[derive(Debug, Clone)]
pub enum DirectorType {
    Random,
    Fallback,
    Hash,
    Client,
    ConsistentHash,
}

#[derive(Default, Debug, Clone, Builder)]
pub struct DirectorBody {
    #[builder(setter(strip_option), default)]
    pub retries: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub quorum: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub key: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub seed: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub vnodes_per_node: Option<Expr>,

    #[builder(setter(strip_option), default)]
    pub backends: Option<Vec<DirectorBackend>>,
}

#[derive(Default, Debug, Clone, Builder)]
pub struct DirectorBackend {
    #[builder(setter(strip_option), default)]
    pub id: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub backend: Option<Expr>,
    #[builder(setter(strip_option), default)]
    pub weight: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Table {
    pub name: Symbol,
    pub body: Vec<TableEntry>,
}

#[derive(Debug, Clone)]
pub struct TableEntry {
    pub key: Expr,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Acl {
    pub name: Symbol,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub status: Option<Expr>,
    pub message: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub body: Vec<ABDIST>,
}

#[derive(Debug, Clone)]
pub enum ABDIST {
    Acl(Acl),
    Backend(Box<Backend>),
    Director(Box<Director>),
    SubDecl(SubDecl),
    Table(Table),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Esi,
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    VarDecl(Symbol),
    Block(Vec<Stmt>),
    Return(SourceLocation, Option<Symbol>),
    Call(Symbol),
    Include(Literal),

    Log(Expr),
    Restart(SourceLocation),
    Error(Error),

    Add(Expr, Expr),
    Set(Expr, Assignment, Expr),
    Unset(Expr),
    Synthetic(Expr),
    SyntheticBase64(Expr),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpTy {
    Bang,
}

#[derive(Debug, Clone, Copy)]
pub struct UnaryOp {
    pub ty: UnaryOpTy,
    pub line: usize,
    pub col: i64,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOpTy {
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Match,
    NotMatch,
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryOp {
    pub ty: BinaryOpTy,
    pub line: usize,
    pub col: i64,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Float(f64),
    Integer(i64),
    String(String),
    True,
    False,
    Duration(f64, DurationUnit),
    AclEntry(String, u8),
    Percent(u64),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Type {
    Acl,
    Backend,
    Bool,
    Float,
    Id,
    Integer,
    Ip,
    Rtime,
    String,
    Time,
    Director,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DurationUnit {
    Milliseconds,
    Seconds,
    Minutes,
    Hours,
    Days,
    Years,
}
