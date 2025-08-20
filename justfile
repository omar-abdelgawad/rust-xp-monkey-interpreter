# list all commands
default:
  @just --list

# run all tests
test:
  @cargo test

# run interactive shell
run:
  @cargo run -q

# records an interactive shells
record:
  @script -c "just run" run.log
