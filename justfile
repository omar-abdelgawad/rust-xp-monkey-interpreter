# list all commands
default:
  just --list

# run all tests
test:
  cargo test -q

# run interactive shell
run:
  cargo run --release -q

# run a Monkey script file
run-file FILE:
  cargo run --release -q -- {{FILE}}

# run book's benchmark
bench:
  cargo run --release -q --bin benchmark

bench_flame:
  cargo flamegraph --bin benchmark

run-file_flame FILE:
  cargo flamegraph -r -- {{FILE}}

# records an interactive shells
record:
  script -c "just run" run.log

# for testing wasm
[working-directory: 'website']
serve:
  python -m http.server 8000

[working-directory: 'website']
build_and_serve: build_website serve

# running script for building website
build_website:
  ./scripts/build.sh

[doc('dry run of git clean. use clean force to delete')]
clean mode="dry":
    git clean -fxfd -e '*venv' -e '.env' {{ if mode == "force" { "" } else { "--dry-run" } }}
