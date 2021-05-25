# Fastly VCL Interpreters in Rust

A tree-walk interpreter for Fastly VCL

## Install project

- Install rust - `curl https://sh.rustup.rs -sSf | sh`
- Clone project - `git clone https://github.com/JakeChampion/vcl-local.git && cd vcl-local`
- Build project - `cargo build`
- Test project - `cargo test`
- Install project `cargo install --path .`
- Try out new command - `vcl-local examples/polyfill/main.vcl --show-ast`

## Examples

Consider hello.vcl

``` vcl
log "hello world";
```

We can tokenize this with

``` bash
cargo run --release --quiet -- hello.vcl --show-tokens
```

Which gives output

``` bash
tokens: [
    Token { ty: Log, lexeme: "log", literal: None, line: 1, col: 2},
    Token { ty: String, lexeme: ""hello world"", literal: Some(Str("hello world")), line: 1, col: 16},
    Token { ty: Semicolon, lexeme: ";", literal: None, line: 1, col: 17},
    Token { ty: Eof, lexeme: "", literal: None, line: 1, col: 17},
]
```

We can show the AST with

``` bash
cargo run --release --quiet -- hello.vcl --show-ast
```

Which gives

``` bash
AST: [
    Log(
        Literal(
            String(
                "hello world",
            ),
        ),
    ),
]
```

## TODO

### Scanner

- [ ] Add comments to the scan result

### Parser

- [ ] Pragmas
- [ ] Macros
- [ ] ACL
- [x] Includes
- [x] string concat without requiring `+` sign. e.g. - `set req.http.a = "url:"req.url;`
- [ ] heredoc syntax for long strings
- [ ] short strings need percent decoding
- [ ] long strings do not need percent decoding
- [ ] typed tables
- [x] subfield lookup using colon -- `set client.identity = req.http.cookie:user_id;`
- [ ] subfield setting using colon -- `set req.http.Cache-Control:max-age = "3600";`
- [ ] regex capture groups
- [ ] PCRE2 modifiers

### Interpreter

- [ ] The integer variants of the +=, -=, and *= operators wrap around as if they were unsigned 64-bit integers.
- [ ] FLOAT arithmetic has special cases for operands which are NaN: Arithmetic operators evaluate to NaN when either operand is NaN.
- [ ] fastly.error = "EDOM" when Domain error. This occurs for a mathematical function which is not defined for a particular value; formally, that value is not considered part of its input domain. For example, division by zero, or var.x %= 5; where var.x is a floating point infinity.
- [ ] subroutine
- [ ] backend
- [ ] director
- [ ] call
- [ ] state machine
- [ ] stdlib
- [ ] regex capture groups
