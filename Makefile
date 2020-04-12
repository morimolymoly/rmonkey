cov:
	cargo install grcov && export CARGO_INCREMENTAL=0 && export RUSTFLAGS="-Zprofile -Ccodegen-units=1 -Copt-level=0 -Clink-dead-code -Coverflow-checks=off -Zno-landing-pads" && cargo build && cargo test && grcov ./target/debug/ -s . -t html --llvm --branch --ignore-not-existing -o ./target/debug/coverage/ && open target/debug/coverage/index.html
