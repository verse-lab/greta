# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: all clean

default: test all

# The library can be loaded in utop for interactive testing.
# The flag "--profile release" is passed to avoid warnings-as-errors

all:
	dune exec ./bin/main.exe

test: 
	dune test
	
clean:
	dune clean

utop:
	dune utop . --profile release
