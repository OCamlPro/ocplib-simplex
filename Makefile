
build:
	@dune build @all @install

doc:
	@dune build @doc

all: build doc

clean:
	@dune clean

test: all
	@dune runtest --no-buffer -f

WATCH?= @all
watch:
	@dune build $(WATCH) -w

install: build
	@dune install

uninstall:
	@dune uninstall

fmt:
	dune build @fmt

opam-deps:
	opam install . --deps-only

.PHONY: build doc all clean test watch install uninstall fmt opam-deps
