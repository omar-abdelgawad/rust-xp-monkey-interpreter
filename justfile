# list all commands
default:
  @just --list

# run all tests
test:
  @cargo test

# run interactive shell
run:
  @cargo run -q

# run a Monkey script file
run-file FILE:
  @cargo run -q -- {{FILE}}

# records an interactive shells
record:
  @script -c "just run" run.log

serve:
  @python -m http.server 8000