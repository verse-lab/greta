# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: all clean

# Default intersection mode (default | wo_opt1 | wo_opt2 | wo_opt3 | wo_opt123 )
INTERSECTION_MODE ?= default

default: test all

# The library can be loaded in utop for interactive testing.
# The flag "--profile release" is passed to avoid warnings-as-errors

all:
	INTERSECT_MODE=$(INTERSECT_MODE) dune exec ./bin/main.exe

menhir-only:
	dune exec ./bin/main.exe
	menhir --table --inspection --dump --explain --reference-graph --greta ./lib/parser.mly --base ./_build/default/lib/parser

test: 
	dune test

run-default:
	$(MAKE) all INTERSECT_MODE=default

run-wo-opt1:
	$(MAKE) all INTERSECT_MODE=wo_opt1

run-wo-opt2:
	$(MAKE) all INTERSECT_MODE=wo_opt2

run-wo-opt3:
	$(MAKE) all INTERSECT_MODE=wo_opt3

run-wo-opt123:
	$(MAKE) all INTERSECT_MODE=wo_opt123
	
clean:
	dune clean

utop:
	dune utop . --profile release
