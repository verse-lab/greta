# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: all clean

default: all test

# The library can be loaded in utop for interactive testing.
# The flag "--profile release" is passed to avoid warnings-as-errors

all:
	dune exec ./bin/main.exe 
# dune build --profile release @install 
# @test -L main.native || ln -s _build/install/default/bin/main.native main.native

test: main.native
	./main.native --test
	
clean:
	dune clean

utop:
	dune utop . --profile release
