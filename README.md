# Fastly VCL Interpreter in Rust

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
    Token { ty: Sub, lexeme: "sub", literal: None, line: 1, col: 2},
    Token { ty: Identifier, lexeme: "vcl_recv", literal: Some(Identifier("vcl_recv")), line: 1, col: 11},
    Token { ty: LeftBrace, lexeme: "{", literal: None, line: 1, col: 13},
    Token { ty: Log, lexeme: "log", literal: None, line: 2, col: 7},
    Token { ty: String, lexeme: ""hello world"", literal: Some(Str("hello world")), line: 2, col: 21},
    Token { ty: Semicolon, lexeme: ";", literal: None, line: 2, col: 22},
    Token { ty: RightBrace, lexeme: "}", literal: None, line: 3, col: 1},
    Token { ty: Eof, lexeme: "", literal: None, line: 3, col: 1},
]

```

We can show the AST with

``` bash
cargo run --release --quiet -- hello.vcl --show-ast
```

Which gives

``` bash
AST: Program {
    body: [
        SubDecl(
            SubDecl {
                name: Symbol {
                    name: "vcl_recv",
                    line: 1,
                    col: 11,
                    var_type: None,
                },
                body: [
                    Log(
                        Literal(
                            String(
                                "hello world",
                            ),
                        ),
                    ),
                ],
            },
        ),
    ],
}
```

## Performance

```
❯ hyperfine "./target/release/vcl-local examples/polyfill/main.vcl --syntax-check" --runs 1000
Benchmark #1: ./target/release/vcl-local examples/polyfill/main.vcl --syntax-check
  Time (mean ± σ):      33.1 ms ±  32.4 ms    [User: 26.0 ms, System: 4.4 ms]
  Range (min … max):    27.0 ms … 1041.8 ms    1000 runs
```

## TODO

### Scanner

- [ ] Add comments to the scan result
- [ ] import

### Parser

- [ ] import
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
- [ ] regex capture groups - <https://developer.fastly.com/reference/vcl/regex/#capture-groups-and-replacement>
- [ ] PCRE2 modifiers - <https://developer.fastly.com/reference/vcl/regex/#pattern-modifiers>
- [ ] error if at end of parsing and at root level there were things other than subroutines, tables and acls
- [ ] error if subroutine starts with vcl_ and is not one of the ones allowed
- [ ] error if subroutine is not a vcl_ prefix and multiple definitions exist

### Interpreter

- [x] The integer variants of the +=, -=, and *= operators wrap around as if they were unsigned 64-bit integers.
- [ ] log
- [ ] FLOAT arithmetic has special cases for operands which are NaN: Arithmetic operators evaluate to NaN when either operand is NaN.
- [ ] fastly.error = "EDOM" when Domain error. This occurs for a mathematical function which is not defined for a particular value; formally, that value is not considered part of its input domain. For example, division by zero, or var.x %= 5; where var.x is a floating point infinity.
- [ ] subroutines
  - [ ] vcl_recv
- [x] backend
- [ ] director
- [ ] call
- [ ] state machine
- [ ] stdlib
- [ ] regex capture groups - <https://developer.fastly.com/reference/vcl/regex/#capture-groups-and-replacement>
- [ ] Predefined variables - <https://developer.fastly.com/reference/vcl/variables/#predefined-variables>
- [ ] Cannot `call` a vcl_ prefixed subroutine
