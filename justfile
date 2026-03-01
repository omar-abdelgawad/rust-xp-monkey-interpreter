# list all commands
default:
  @just --list

# run all tests
test:
  @cargo test -q --lib

# run interactive shell
run:
  @cargo run -q

# run a Monkey script file
run-file FILE:
  @cargo run -q -- {{FILE}}

# run book's benchmark
bench ENGINE="vm":
  cargo run --release -q --bin benchmark -- --engine {{ENGINE}}

# records an interactive shells
record:
  @script -c "just run" run.log

# for testing wasm
[working-directory: 'website']
serve: build_website
  @python -m http.server 8000

# running script for building website
build_website:
  ./scripts/build.sh

git_clean_dry_run:
  git clean -fxfd --dry-run
