default: watch-test

test:
	cargo test

test-backtrace:
	RUST_BACKTRACE=1 cargo test

# only run tests matching PATTERN
filter PATTERN:
	cargo test {{PATTERN}}

check:
	cargo check

build:
	cargo build

watch-test:
	cargo watch clear test

watch COMMAND='test':
	cargo watch {{COMMAND}}

fmt: no-changes
	cargo fmt

clippy:
	rustup run nightly cargo clippy -- -D clippy

sloc:
	@cat src/*.rs | sed '/^\s*$/d' | wc -l

# will fail if there are outstanding changes in the repo
no-changes:
	git diff --no-ext-diff --quiet --exit-code

install-rust:
	curl https://sh.rustup.rs -sSf | sh

install-dev-dependencies:
	rustup install stable
	rustup install nightly
	cargo install cargo-check
	cargo install cargo-watch
	cargo install just
	rustup component add clippy-preview

check-examples:
	#!/usr/bin/env bash
	set -eu
	find examples -type f -exec cargo run -- "{}" --syntax-check \;
