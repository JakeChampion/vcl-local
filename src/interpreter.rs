use hyper::http::Uri;
use hyper::{
    service::{make_service_fn, service_fn},
    Body, Client, Error, Request, Response, Server, Version,
};
use std::u16;
use std::{collections::HashMap, sync::Arc};
use tokio::time::{interval, Duration};
use tokio::{sync::RwLock, time::timeout};

use std::num::Wrapping;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::expr::DurationUnit;
use crate::expr::{self, Program, SubDecl, Symbol};
use std::fmt;

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

#[derive(Clone)]
pub struct Backend {
    pub name: String,
    pub health: Arc<RwLock<bool>>,
}

impl fmt::Debug for Backend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Backend({})", self.name)
    }
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
    Backend(Backend),
    Nil,
    Req(Box<Req>),
    RTime(Duration),
    Symbol(Symbol)
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarnishState {
    Lookup,
    Pass,
    Error,
    Restart,
    Hash,
    Deliver,
    Fetch,
    DeliverStale,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Varnish {
    RecvLookup,
    RecvPass,
    RecvError,
    Restart,
    // HashHash,
    HitDeliver,
    HitPass,
    HitError,
    MissFetch,
    MissDeliverStale,
    MissPass,
    MissError,
    PassPass,
    PassError,
    FetchDeliver,
    FetchDeliverStale,
    FetchPass,
    FetchError,
    ErrorDeliver,
    DeliverDeliver,
    LogDeliver,
}

fn as_callable(_interpreter: &Interpreter, value: &Value) -> Option<Box<dyn Callable>> {
    match value {
        Value::Function(f) => Some(Box::new(f.clone())),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Backend,
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
    Req,
}

#[derive(Clone)]
pub struct Resp {
    // Body bytes send to the client in the response.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-body-bytes-written/
    pub body_bytes_written: usize,
    // Total bytes to send to the client in the response.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-bytes-written/
    pub bytes_written: usize,
    // Whether or not the request completed.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-completed/
    pub completed: bool,
    // Header bytes to send to the client in the response.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-header-bytes-written/
    pub header_bytes_written: usize,
    // Whether request handling went through vcl_error.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-is-locally-generated/
    pub is_locally_generated: bool,
    // HTTP protocol version in use for this request. For example HTTP/1.1.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-proto/
    pub proto: String,
    // The response phrase included with the status code.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-response/
    pub response: String,
    // The HTTP status code stored with the object.
    // https://developer.fastly.com/reference/vcl/variables/client-response/resp-status/
    pub status: usize,
}

// TODO: Resp.tarpit function
// https://developer.fastly.com/reference/vcl/functions/miscellaneous/resp-tarpit/

#[derive(Clone)]
pub struct Req {
    // The request body. Using this variable for binary data will truncate at the first null character. Limited to 8KB in size.
    // Exceeding the limit results in the req.body variable being blank.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-body/
    pub body: String,
    // Total body bytes read from the client generating the request.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-body-bytes-read/
    pub body_bytes_read: Wrapping<i64>,
    // Total bytes read from the client generating the request.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-bytes-read/
    pub bytes_read: Wrapping<i64>,
    // The digest (hash) of the currently served object, derived from information fed into req.hash during the most recent run of vcl_hash. This variable returns the digest in hexadecimal (64 characters).
    // https://developer.fastly.com/reference/vcl/variables/cache-object/req-digest/
    pub digest: String,
    // Apply range handling for responses on pass.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-enable-range-on-pass/
    pub enable_range_on_pass: bool,
    // Assemble the response from individually cacheable block-aligned file segments.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-enable-segmented-caching/
    pub enable_segmented_caching: bool,
    // Whether or not to disable or enable ESI processing during this request. Using set req.esi = false; will disable ESI processing. The default value is true.
    // https://developer.fastly.com/reference/vcl/variables/esi/req-esi/
    pub esi: bool,
    // Level of ESI subrequest. A value of 0 indicates this is the top-level request. A value of 2 would signify an include within an include.
    // https://developer.fastly.com/reference/vcl/variables/esi/req-esi-level/
    pub esi_level: Wrapping<i64>,
    // Hash value for the object. The hash determines what is unique about an object. Anything added to the hash will cause Varnish to cache objects separately based on that information.
    // https://developer.fastly.com/reference/vcl/variables/cache-object/req-hash/
    pub hash: String,
    // Forces the request to miss whether we have a cached version of the object or not. This differs from passing in that Varnish will still request collapse and will avoid the creation of hit_for_pass objects.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-hash-always-miss/
    pub hash_always_miss: bool,
    // When there is more than one simultaneous cache miss for an object, Varnish will normally put all but one of the threads handling those requests to sleep. Thus, only one cache miss hits the origin and the rest wait for that response. req.hash_ignore_busy overrides this behavior and lets all requests through simultaneously.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-hash-ignore-busy/
    pub hash_ignore_busy: bool,
    // Total header bytes read from the client generating the request.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-header-bytes-read/
    pub header_bytes_read: Wrapping<i64>,
    // Whether VCL is being evaluated for a stale while revalidate request to a backend.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-is-background-fetch/
    pub is_background_fetch: bool,
    // Whether the request was received from another machine in the cluster.
    // https://developer.fastly.com/reference/vcl/variables/server/req-is-clustering/
    pub is_clustering: bool,
    // Whether VCL is being evaluated within an ESI fragment.
    // https://developer.fastly.com/reference/vcl/variables/esi/req-is-esi-subreq/
    pub is_esi_subreq: bool,
    // Indicates whether the request was made using IPv6 or not.
    // https://developer.fastly.com/reference/vcl/variables/client-connection/req-is-ipv6/
    pub is_ipv6: bool,
    // Whether the handled request is a purge request.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-is-purge/
    pub is_purge: bool,
    // Indicates whether the request uses SSL or not.
    // https://developer.fastly.com/reference/vcl/variables/client-connection/req-is-ssl/
    pub is_ssl: bool,
    // The maximum stale age that is acceptable for a particular request.
    // https://developer.fastly.com/reference/vcl/variables/server/req-max-stale-if-error/
    pub max_stale_if_error: Duration,
    // Same as req.max_stale_if_error except controls the maximum stale age for stale while revalidate functionality.
    // https://developer.fastly.com/reference/vcl/variables/server/req-max-stale-while-revalidate/
    pub max_stale_while_revalidate: Duration,
    // HTTP method sent by the client, such as "GET" or "POST".
    // Requests using the HTTP PURGE method will appear in VCL as "FASTLYPURGE". All other methods are reported as received, including any unknown or unrecognized methods, provided that the request is syntactically valid HTTP.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-method/
    pub method: String,
    // HTTP protocol version in use for this request. For example HTTP/1.1.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-proto/
    pub proto: String,
    // Indicates whether the request uses SSL or not. Possible values are http and https.
    // https://developer.fastly.com/reference/vcl/variables/client-connection/req-protocol/
    pub protocol: String,
    // Counts the number of times the VCL has been restarted.
    // https://developer.fastly.com/reference/vcl/variables/server/req-restarts/
    pub restarts: i8,
    // In an ESI subrequest, contains the URL of the top-level request that ESI processing was enabled on. If the request is not an ESI, req.topurl will be a not set string value.
    // https://developer.fastly.com/reference/vcl/variables/esi/req-topurl/
    pub topurl: String,
    // The full path, including query parameters.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-url/
    pub url: String,
    // Request ID.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-xid/
    pub xid: String,
    // The backend to use to service the request.
    // https://developer.fastly.com/reference/vcl/variables/backend-connection/req-backend/
    pub backend: Backend,
    // Whether or not this backend, or recursively any of the backends under this director, is considered healthy.
    // https://developer.fastly.com/reference/vcl/variables/backend-connection/req-backend-healthy/
    // pub backend.healthy: Bool,
    // pub backend.is_cluser: Bool,
    // Indicates whether the backend is a customer origin.
    // https://developer.fastly.com/reference/vcl/variables/backend-connection/req-backend-is-origin/
    // pub backend.is_origin: Bool,
    // Indicates whether the backend is a Fastly Shield POP.
    // https://developer.fastly.com/reference/vcl/variables/backend-connection/req-backend-is-shield/
    // pub backend.is_shield: Bool,
    // The name of the backend that was used for this request.
    // https://developer.fastly.com/reference/vcl/variables/backend-response/beresp-backend-name/
    // pub backend.name: String,
}

impl Req {
    // Same as req.body, except the request body is encoded in Base64, which handles null characters and allows representation of binary bodies.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-body-base64/
    fn body_base64(&self) -> String {
        base64::encode(&self.body)
    }

    // A floating point number between 0 and 1 derived from the object digest.
    // https://developer.fastly.com/reference/vcl/variables/cache-object/req-digest-ratio/
    fn digest_ratio(&self) -> f64 {
        //TODO - something with req.digest
        0.1
    }

    // The file name specified in a URL. This will be the last component of the path, from the last / to the end, not including the query string.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-url-basename/
    fn url_basename(&self) -> String {
        let uri = self.url.parse::<Uri>().unwrap();
        let path = uri.path();
        if path == "/" {
            "".to_string()
        } else {
            let mut parts: Vec<&str> = path.split('/').collect();
            parts.pop().unwrap().to_string()
        }
    }

    // The file extension specified in a URL.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-url-ext/
    fn url_ext(&self) -> String {
        let uri = self.url.parse::<Uri>().unwrap();
        let path = uri.path();
        let mut parts: Vec<&str> = path.split('.').collect();
        parts.pop().unwrap().to_string()
    }
    // The full path, without any query parameters.
    // This variable is updated any time req.url is set.
    fn url_path(&self) -> String {
        let uri = self.url.parse::<Uri>().unwrap();
        uri.path().to_string()
    }
    // The query string portion of req.url. This will be from immediately after the ? to the end of the URL.
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-url-qs/
    fn url_qs(&self) -> String {
        let uri = self.url.parse::<Uri>().unwrap();
        match uri.query() {
            Some(qs) => qs.to_string(),
            None => "".to_string(),
        }
    }

    // The directories specified in a URL. This will be from the beginning of the URL up to the last /, not including the query string. The last / will not be part of req.url.dirname unless req.url.dirname is / (the root directory).
    // https://developer.fastly.com/reference/vcl/variables/client-request/req-url-dirname/
    fn url_dirname(&self) -> String {
        let uri = self.url.parse::<Uri>().unwrap();
        let path = uri.path();
        if path == "/" {
            "/".to_string()
        } else {
            let mut parts: Vec<&str> = uri.path().split('/').collect();
            parts.pop().unwrap();
            if parts.len() == 1 {
                "/".to_string()
            } else {
                parts.join("/")
            }
        }
    }

    async fn from_hyper_req(req: Request<Body>, backend: Backend) -> Self {
        let uri = req.uri().to_string();
        let headers = req.headers();
        let mut header_bytes_read = Wrapping(0_i64);
        for (key, value) in headers.iter() {
            header_bytes_read += Wrapping(key.to_string().len() as i64);
            header_bytes_read += Wrapping(value.len() as i64);
        }
        let method = req.method().to_string();
        let proto = match req.version() {
            Version::HTTP_09 => "HTTP/0.9".to_string(),
            Version::HTTP_10 => "HTTP/1.0".to_string(),
            Version::HTTP_11 => "HTTP/1.1".to_string(),
            Version::HTTP_2 => "HTTP/2.0".to_string(),
            Version::HTTP_3 => "HTTP/3.0".to_string(),
            _ => todo!(),
        };
        let body = req.into_body();
        let body_as_bytes = hyper::body::to_bytes(body).await.expect("11111213");
        let body = std::str::from_utf8(&body_as_bytes).unwrap().to_string();
        let body_bytes_read = Wrapping(body_as_bytes.len() as i64);
        let bytes_read = header_bytes_read + body_bytes_read;

        Self {
            backend,
            body,
            body_bytes_read,
            bytes_read,
            enable_range_on_pass: false,
            enable_segmented_caching: false,
            esi: true,
            esi_level: Wrapping(0),
            hash_always_miss: false,
            hash_ignore_busy: false,
            header_bytes_read,
            is_background_fetch: false,
            is_clustering: false,
            is_esi_subreq: false,
            is_ipv6: false,
            is_purge: false,
            is_ssl: false,
            max_stale_if_error: interpret_duration(9_223_372_036.854, &DurationUnit::Seconds),
            max_stale_while_revalidate: interpret_duration(
                9_223_372_036.854,
                &DurationUnit::Seconds,
            ),
            method: method.clone(),
            proto,
            protocol: "http".to_string(),
            restarts: 0,
            topurl: "".to_string(),
            url: uri,
            xid: "1".to_string(),
            digest: "00000000".to_string(),
            hash: "00000000".to_string(),
        }
    }
}

impl fmt::Debug for Req {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Req({})", self.url)
    }
}

pub const fn type_of(val: &Value) -> Type {
    match val {
        Value::Float(_) => Type::Float,
        Value::Integer(_) => Type::Integer,
        Value::String(_) => Type::String,
        Value::Bool(_) => Type::Bool,
        Value::Backend(_) => Type::Backend,
        Value::Function(_) => Type::Function,
        Value::Req(_) => Type::Req,
        Value::Nil => Type::Nil,
        Value::RTime(_) => Type::Rtime,
        Value::Symbol(_) => Type::Id,
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
            Value::Backend(backend) => write!(f, "Backend({})", backend.name),
            Value::Req(req) => write!(f, "{}", req.url),
            Value::Nil => write!(f, "nil"),
            Value::RTime(duration) => write!(f, "{}", duration.as_secs()),
            Value::Symbol(sym) =>  write!(f, "Id({})", sym.name),
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
fn int_to_float(b: i64) -> f64 {
    let bf: Option<f64> = num_traits::cast::FromPrimitive::from_i64(b);
    if let Some(bf) = bf {
        if bf as i64 == b {
            return bf;
        }
    }
    panic!(
        "Integer {} is not exactly representable as a floating-point number",
        b
    );
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
            // TODO: fix this
            expr::Expr::Get(s, _) => self.get_sym(s.as_ref().clone())?,
            expr::Expr::Variable(sym) => sym,
            _ => {
                panic!("nooo")
            }
        };
        Ok(s)
    }

    pub fn assign(&mut self, sym: expr::Expr, val: &Value) -> Result<(), String> {
        let mut rest = None;
        let sym = match &sym {
            expr::Expr::Get(root, inner) => {
                rest = Some(inner);
                self.get_sym(root.as_ref().to_owned())?
            },
            expr::Expr::Variable(_) => self.get_sym(sym.clone())?,
            _ => todo!(),
        };
        if let Some(entry) = self.venv.clone().get(&sym.name) {
            if let Some(prop) = rest {
                if sym.name == "req" {
                    match prop.name.as_str() {
                        "backend" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::Backend(val) => {
                                                req.backend = val.clone();
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a backend to req.backend"),
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "enable_range_on_pass" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::Bool(val) => {
                                                req.enable_range_on_pass = *val;
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a bool to req.enable_range_on_pass"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "enable_segmented_caching" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::Bool(val) => {
                                                req.enable_segmented_caching = *val;
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a bool to req.enable_segmented_caching"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "esi" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::Bool(val) => {
                                                req.esi = *val;
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a bool to req.esi"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "grace" | "max_stale_if_error" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::RTime(val) => {
                                                req.max_stale_if_error = *val;
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a RTime to req.grace or req.max_stale_if_error"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "hash_always_miss" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::Bool(val) => {
                                                req.hash_always_miss = *val;
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a bool to req.hash_always_miss"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "hash_ignore_busy" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::Bool(val) => {
                                                req.hash_ignore_busy = *val;
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a bool to req.hash_ignore_busy"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "max_stale_while_revalidate" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::RTime(val) => {
                                                req.max_stale_while_revalidate = *val;
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a RTime to req.max_stale_while_revalidate"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "method"|"request" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::String(val) => {
                                                req.method = val.clone();
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a String to req.method/req.request"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "proto" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::String(val) => {
                                                req.proto = val.clone();
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a String to req.proto"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        "url" => {
                            let req = entry.1.clone();
                            if let Some(req) = req {
                                match req {
                                    Value::Req(mut req) => {
                                        println!("val: {:?}", val);
                                        match val {
                                            Value::String(val) => {
                                                req.url = val.clone();
                                                self.define(sym.clone(), Type::Req, Some(Value::Req(req)));
                                                return Ok(());
                                            },
                                            _ => todo!("you can only set a String to req.url"),
                                            
                                        }
                                    },
                                    _ => todo!("lol what"),
                                }
                            } else {
                                todo!(":?");
                            }
                        }
                        _ => todo!("what mate?"),
                    }
                } else {
                    todo!("what other variables can have properties?");
                }
            }
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

#[derive(Debug, Clone)]
pub struct Interpreter {
    pub counter: u64,
    pub env: Environment,
    pub globals: Environment,
    pub health: HashMap<String, Arc<RwLock<bool>>>,
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
            health: HashMap::new(),
            retval: None,
            output: Vec::default(),
            interrupted: Arc::new(AtomicBool::new(false)),
            backtrace: vec![(0, "script".to_string())],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Machine {
    pub recv: expr::SubDecl,
    pub hash: expr::SubDecl,
    pub hit: expr::SubDecl,
    pub miss: expr::SubDecl,
    pub pass: expr::SubDecl,
    pub fetch: expr::SubDecl,
    pub error: expr::SubDecl,
    pub deliver: expr::SubDecl,
    pub log: expr::SubDecl,
}

#[derive(Debug, Clone)]
pub struct App {
    pub machine: Machine,
    // backends: Vec<Backend>,
    // directors: Vec<Director>,
    // tables: Vec<Table>,
    // acls: Vec<Acl>,
}

fn combine_subs(subroutines: &[expr::SubDecl], name: &str) -> expr::SubDecl {
    let subs: Vec<expr::SubDecl> = subroutines
        .iter()
        .filter_map(|s| {
            if s.name.name == name {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    let mut stmts = vec![];
    for mut sub in subs {
        stmts.append(&mut sub.body)
    }
    SubDecl {
        name: Symbol {
            name: name.to_string(),
            line: 0,
            col: 0,
            var_type: Some(expr::Type::Sub),
        },
        body: stmts,
    }
}

impl Interpreter {
    pub async fn interpret(
        &mut self,
        program: Program,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        self.interrupted.store(false, Ordering::Release);
        let backends: Vec<Box<expr::Backend>> = program
            .body
            .iter()
            .filter_map(|c| match c {
                expr::ABDIST::Backend(b) => Some(b.clone()),
                _ => None,
            })
            .collect();

        // 1. setup backends + healthchecks
        // 2. setup directors TODO
        for backend in &backends {
            self.interpret_backend(backend);
        }
        // 3. setup acls TODO
        // 4. setup tables
        // 5. setup subroutine state machine
        let subroutines: Vec<expr::SubDecl> = program
            .body
            .iter()
            .filter_map(|c| match c {
                expr::ABDIST::SubDecl(b) => Some(b.clone()),
                _ => None,
            })
            .collect();
        let recv = combine_subs(&subroutines, "vcl_recv");
        let hash = combine_subs(&subroutines, "vcl_hash");
        let hit = combine_subs(&subroutines, "vcl_hit");
        let miss = combine_subs(&subroutines, "vcl_miss");
        let pass = combine_subs(&subroutines, "vcl_pass");
        let fetch = combine_subs(&subroutines, "vcl_fetch");
        let error = combine_subs(&subroutines, "vcl_error");
        let deliver = combine_subs(&subroutines, "vcl_deliver");
        let log = combine_subs(&subroutines, "vcl_log");
        // TODO
        let _custom_subs: Vec<expr::SubDecl> = subroutines
            .iter()
            .filter_map(|s| {
                if s.name.name.starts_with("vcl_") {
                    None
                } else {
                    Some(s.clone())
                }
            })
            .collect();
        let app = App {
            machine: Machine {
                recv,
                hash,
                hit,
                miss,
                pass,
                fetch,
                error,
                deliver,
                log,
            },
        };

        // Construct our SocketAddr to listen on...
        let addr = ([127, 0, 0, 1], 3000).into();
        let default_backend = backends.first().unwrap().name.clone();
        let val = self.lookup(&default_backend).expect("ergh");
        let backend = match val {
            Value::Backend(b) => b.clone(),
            _ => todo!()
        };

        // And a MakeService to handle each connection...
        let make_svc = make_service_fn(move |_| {
            let app = app.clone();
            let s = self.clone();
            let backend = backend.clone();
            // let program = program.clone();
            async move {
                Ok::<_, Error>(service_fn(move |req: Request<Body>| {
                    let app = app.clone();
                    let mut s = s.clone();
                    let backend = backend.clone();
                    // let program = program.clone();
                    async move {
                        let mut vcl_req = Req::from_hyper_req(req, backend).await;
                        s.execute_app(&app, &mut vcl_req);
                        Ok::<_, Error>(Response::new(Body::from("Hello World")))
                    }
                }))
            }
        });

        // Then bind and serve...
        let server = Server::bind(&addr).serve(make_svc);

        println!("listening on: http://{}", server.local_addr());

        // Run forever-ish...
        if let Err(err) = server.await {
            eprintln!("server error: {}", err);
        }

        Ok(())
    }

    pub fn execute_app(&mut self, app: &App, req: &mut Req) -> Resp {
        let mut state = self.execute_recv(app, req);
        while state != Varnish::LogDeliver {
            state = match state {
                Varnish::RecvLookup => {
                    self.execute_hash(app, req);
                    // TODO: implement cache lookup
                    let cached = false;
                    if cached {
                        self.execute_hit(app, req)
                    } else {
                        self.execute_miss(app, req)
                    }
                },
                Varnish::RecvPass => {
                    self.execute_hash(app, req);
                    self.execute_pass(app, req)
                },
                Varnish::RecvError => {
                    self.execute_hash(app, req);
                    self.execute_error(app, req)
                },
                Varnish::Restart => {
                    // increase restart counter - req.restarts
                    // TODO: limit to 3.
                    req.restarts += 1;
                    return self.execute_app(app, req);
                },
                Varnish::HitDeliver | 
                Varnish::FetchDeliver | 
                Varnish::FetchDeliverStale | 
                Varnish::FetchPass | 
                Varnish::ErrorDeliver | 
                Varnish::MissDeliverStale => self.execute_deliver(app, req),
                Varnish::MissPass | Varnish::HitPass => self.execute_pass(app, req),
                Varnish::DeliverDeliver => self.execute_log(app, req),
                // TODO: Start the backend fetch request
                Varnish::MissFetch | Varnish::PassPass => self.execute_fetch(app, req),
                Varnish::FetchError |
                Varnish::PassError |
                Varnish::HitError |
                Varnish::MissError => self.execute_error(app, req),
                Varnish::LogDeliver => break,
            };
        }

        // TODO: get resp from app
        Resp {
            body_bytes_written: 0,
            bytes_written: 0,
            completed: true,
            header_bytes_written: 0,
            is_locally_generated: false,
            proto: "HTTP/1.1".to_string(),
            response: "OK".to_string(),
            status: 200,
        }
    }

    pub fn execute_hit(&self, _app: &App, _req: &Req) -> Varnish {
        Varnish::HitDeliver
    }
    pub fn execute_miss(&self, _app: &App, _req: &Req) -> Varnish {
        Varnish::MissFetch
    }
    pub fn execute_pass(&self, _app: &App, _req: &Req) -> Varnish {
        Varnish::PassPass
    }
    pub fn execute_error(&self, _app: &App, _req: &Req) -> Varnish {
        Varnish::ErrorDeliver
    }
    pub fn execute_log(&self, _app: &App, _req: &Req) -> Varnish {
        Varnish::LogDeliver
    }
    pub fn execute_hash(&self, _app: &App, _req: &Req) {
        
    }
    pub fn execute_deliver(&self, _app: &App, _req: &Req) -> Varnish {
        Varnish::DeliverDeliver
    }
    pub fn execute_fetch(&self, _app: &App, _req: &Req) -> Varnish {
        Varnish::FetchDeliver
    }

    pub fn execute_recv(&mut self, app: &App, req: &Req) -> Varnish {
        let req_sym = Symbol {
            name: "req".to_string(),
            line: 0,
            col: 0,
            var_type: Some(expr::Type::Req),
        };
        self.env = Environment::default();
        self.env
            .define(req_sym, Type::Req, Some(Value::Req(Box::new(req.clone()))));
        for stmt in &app.machine.recv.body {
            let result = self.execute(stmt).expect("sadasdaasda");
            if let Some(result) = result {
                return match result {
                    VarnishState::Lookup => Varnish::RecvLookup,
                    VarnishState::Pass => Varnish::RecvPass,
                    VarnishState::Error => Varnish::RecvError,
                    VarnishState::Restart => Varnish::Restart,
                    _ => todo!("unreachable 1"),
                };
            }
        }
        Varnish::RecvLookup
    }

    pub fn format_backtrace(&self) -> String {
        let lines: Vec<_> = self
            .backtrace
            .iter()
            .map(|(_, funname)| format!("[line ??] in {}", funname))
            .collect();
        format!("Backtrace (most recent call last):\n\n{}", lines.join("\n"))
    }

    fn execute(&mut self, stmt: &expr::Stmt) -> Result<Option<VarnishState>, String> {
        if self.retval.is_some() {
            return Ok(None);
        }

        match stmt {
            expr::Stmt::Expr(e) => match self.interpret_expr(e) {
                Ok(_) => Ok(None),
                Err(err) => Err(err),
            },
            expr::Stmt::If(cond, if_true, maybe_if_false) => {
                if Self::is_truthy(&self.interpret_expr(cond)?) {
                    return self.execute(if_true);
                }
                if let Some(if_false) = maybe_if_false {
                    return self.execute(if_false);
                }
                Ok(None)
            }
            expr::Stmt::Log(e) => match self.interpret_expr(e) {
                Ok(val) => {
                    // TODO: when logging RTime, always convert it to seconds and three decimals places
                    println!("{}", val);
                    self.output.push(format!("{}", val));
                    Ok(None)
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
                Ok(None)
            }
            expr::Stmt::Block(stmts) => {
                for stmt in stmts.iter() {
                    self.execute(stmt)?;
                }

                Ok(None)
            }
            expr::Stmt::Set(identifier, assignment, value) => {
                println!("set statement =  identifier: {:?} -  assignment: {:?} - value: {:?}", identifier, assignment, value);
                self.interpret_set(identifier, assignment, value)?;
                Ok(None)
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
                println!("val_expr: {:?}", val_expr);
                println!("sym: {:?} -- val: {:?}", sym, val);
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
            expr::Expr::Get(lhs, attr) => {
                if let expr::Expr::Variable(lhs) = lhs.as_ref() {
                    let v = self.env.get(&lhs).expect("grr");
                    match v {
                        Value::RTime(_) => todo!(),
                        Value::Integer(_) => todo!(),
                        Value::Float(_) => todo!(),
                        Value::String(_) => todo!(),
                        Value::Bool(_) => todo!(),
                        Value::Function(_) => todo!(),
                        Value::Backend(_) => todo!(),
                        Value::Nil => todo!(),
                        Value::Req(req) => Self::get_req_attr(req, attr),
                        Value::Symbol(_) => todo!(),
                    }
                } else {
                    todo!()
                }
            }
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

    fn get_req_attr(req: &Req, attr: &Symbol) -> Result<Value, String> {
        // TODO: All the other req properties such as
        // backend
        // https://developer.fastly.com/reference/vcl/variables/miscellaneous/req-backend-ip/
        // backend.ip
        // backend.name
        // https://developer.fastly.com/reference/vcl/variables/miscellaneous/req-backend-port/
        // backend.port
        // http.*
        match attr.name.as_str() {
            // The variable req.postbody is an alias for req.body.
            // https://developer.fastly.com/reference/vcl/variables/client-request/req-postbody/
            "body" | "postbody" => Ok(Value::String(req.body.clone())),
            "body.base64" => Ok(Value::String(req.body_base64())),
            "body_bytes_read" => Ok(Value::Integer(req.body_bytes_read)),
            "bytes_read" => Ok(Value::Integer(req.bytes_read)),
            "digest" => Ok(Value::String(req.digest.clone())),
            "digest.ratio" => Ok(Value::Float(req.digest_ratio())),
            "enable_range_on_pass" => Ok(Value::Bool(req.enable_range_on_pass)),
            "enable_segmented_caching" => Ok(Value::Bool(req.enable_segmented_caching)),
            "esi" => Ok(Value::Bool(req.esi)),
            "esi_level" => Ok(Value::Integer(req.esi_level)),
            "hash" => Ok(Value::String(req.hash.clone())),
            "hash_always_miss" => Ok(Value::Bool(req.hash_always_miss)),
            "hash_ignore_busy" => Ok(Value::Bool(req.hash_ignore_busy)),
            "header_bytes_read" => Ok(Value::Integer(req.header_bytes_read)),
            "is_background_fetch" => Ok(Value::Bool(req.is_background_fetch)),
            "is_clustering" => Ok(Value::Bool(req.is_clustering)),
            "is_esi_subreq" => Ok(Value::Bool(req.is_esi_subreq)),
            "is_ipv6" => Ok(Value::Bool(req.is_ipv6)),
            "is_purge" => Ok(Value::Bool(req.is_purge)),
            "is_ssl" => Ok(Value::Bool(req.is_ssl)),
            // https://developer.fastly.com/reference/vcl/variables/server/req-grace/
            "grace" | "max_stale_if_error" => Ok(Value::RTime(req.max_stale_if_error)),
            "max_stale_while_revalidate" => Ok(Value::RTime(req.max_stale_while_revalidate)),
            // Alias of req.method.
            // https://developer.fastly.com/reference/vcl/variables/client-request/req-request/
            "method" | "request" => Ok(Value::String(req.method.clone())),
            "proto" => Ok(Value::String(req.proto.clone())),
            "protocol" => Ok(Value::String(req.protocol.clone())),
            "restarts" => Ok(Value::Integer(Wrapping(i64::from(req.restarts)))),
            // Fastly service ID.
            // https://developer.fastly.com/reference/vcl/variables/miscellaneous/req-service-id/
            "service_id" => Ok(Value::String("7iO2GdsfBkRxWfssgoM1xI".to_string())),
            "topurl" => Ok(Value::String(req.topurl.clone())),
            "url" => Ok(Value::String(req.url.clone())),
            "url.basename" => Ok(Value::String(req.url_basename())),
            "url.dirname" => Ok(Value::String(req.url_dirname())),
            "url.ext" => Ok(Value::String(req.url_ext())),
            "url.path" => Ok(Value::String(req.url_path())),
            "url.qs" => Ok(Value::String(req.url_qs())),
            // https://developer.fastly.com/reference/vcl/variables/miscellaneous/req-vcl/
            "vcl" => Ok(Value::String(
                "7iO2GdsfBkRxWfssgoM1xI.1_0-51269ab2e4ef1da370a2d6e0d35d7b6e".to_string(),
            )),
            // https://developer.fastly.com/reference/vcl/variables/miscellaneous/req-vcl-generation/
            "vcl.generation" => Ok(Value::Integer(Wrapping(2))),
            // https://developer.fastly.com/reference/vcl/variables/miscellaneous/req-vcl-md5/
            // TODO: Maybe actually md5 the vcl?
            "vcl.md5" => Ok(Value::String(
                "51269ab2e4ef1da370a2d6e0d35d7b6e".to_string(),
            )),
            // https://developer.fastly.com/reference/vcl/variables/miscellaneous/req-vcl-version/
            "vcl.version" => Ok(Value::Integer(Wrapping(1))),
            "xid" => Ok(Value::String(req.xid.clone())),
            _ => todo!(),
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
            (Value::Integer(a), expr::Literal::Integer(b)) => {
                Value::Integer(Wrapping((a.0).rem_euclid(*b)))
            }
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
            expr::Literal::Duration(amount, unit) => {
                Value::RTime(interpret_duration(*amount, unit))
            }
            expr::Literal::AclEntry(_, _)
            | expr::Literal::Percent(_) => {
                todo!()
            }
        }
    }

    fn interpret_backend(&mut self, backend: &expr::Backend) {
        let lock: Arc<RwLock<bool>> = Arc::new(RwLock::new(true));
        self.health.insert(backend.clone().name.name, lock.clone());
        self.globals.define(
            backend.clone().name,
            Type::Backend,
            Some(Value::Backend(Backend {
                health: lock.clone(),
                name: backend.clone().name.name,
            })),
        );
        let _handle = tokio::spawn(probe(backend.clone(), lock));
    }
}

fn interpret_duration(amount: f64, unit: &DurationUnit) -> Duration {
    match unit {
        expr::DurationUnit::Milliseconds => Duration::from_millis(amount as u64),
        expr::DurationUnit::Seconds => Duration::from_secs_f64(amount * 1000_f64),
        expr::DurationUnit::Minutes => Duration::from_secs_f64(amount * f64::from(1000 * 60)),
        expr::DurationUnit::Hours => Duration::from_secs_f64(amount * f64::from(1000 * 60 * 60)),
        expr::DurationUnit::Days => {
            Duration::from_secs_f64(amount * f64::from(1000 * 60 * 60 * 24))
        }
        expr::DurationUnit::Years => Duration::from_secs(amount as u64 * 1000 * 60 * 60 * 24 * 365),
    }
}

async fn probe(
    backend: expr::Backend,
    lock: Arc<RwLock<bool>>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let check = backend.body.clone().probe.unwrap();
    if let Some(expr::Expr::Literal(expr::Literal::True)) = check.dummy {
        return Ok(());
    }
    // Run the probe based on it's configured interval
    let interval_amount =
        if let Some(expr::Expr::Literal(expr::Literal::Duration(amount, unit))) = &check.interval {
            interpret_duration(*amount, unit)
        } else {
            todo!()
        };
    let mut interval = interval(interval_amount);
    let mut counter =
        if let Some(expr::Expr::Literal(expr::Literal::Integer(initial))) = &check.initial {
            *initial
        } else {
            todo!()
        };

    let expected_response =
        if let Some(expr::Expr::Literal(expr::Literal::Integer(expected_response))) =
            &check.expected_response
        {
            *expected_response
        } else {
            todo!()
        };

    let window = if let Some(expr::Expr::Literal(expr::Literal::Integer(window))) = &check.window {
        *window
    } else {
        todo!()
    };

    let threshold =
        if let Some(expr::Expr::Literal(expr::Literal::Integer(threshold))) = &check.threshold {
            *threshold
        } else {
            todo!()
        };

    let timeout_amount =
        if let Some(expr::Expr::Literal(expr::Literal::Duration(amount, unit))) = &check.timeout {
            interpret_duration(*amount, unit)
        } else {
            todo!()
        };

    let p = check.request.as_ref().expect("asdasfadfsdf111");
    let host =
        if let Some(expr::Expr::Literal(expr::Literal::String(host))) = backend.body.clone().host {
            host
        } else {
            todo!()
        };
    loop {
        interval.tick().await;

        let mut req = Request::builder()
            .method(p.method.clone())
            .uri(format!("{}{}{}", "http://", host, p.path)); // TODO: Get protocol from the backend
        for header in &p.headers {
            req = req.header(&header.0, &header.1);
        }

        let req = req.body(Body::empty())?;
        let client = Client::new();

        // println!("req: {:?}", req);

        match timeout(timeout_amount, client.request(req)).await {
            Ok(Ok(resp)) => {
                if resp.status() == expected_response as u16 {
                    if counter < window {
                        counter += 1;
                    }
                } else if counter > 0 {
                    counter -= 1;
                }
                println!("Response: {}", resp.status());
            }
            _ => {
                if counter > 0 {
                    counter -= 1;
                }
            }
        }
        if counter >= threshold {
            let mut w = lock.write().await;
            *w = true;
        } else {
            let mut w = lock.write().await;
            *w = false;
        }
        println!("Healthy: {}", lock.read().await);
        println!("counter: {}", counter);
        println!("threshold: {}", threshold);
        println!("window: {}", window);
    }
}
