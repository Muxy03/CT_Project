LIBS := $(shell find lib -name "*.ml")
MAIN := bin/main.ml

.PHONY: all build run test clean watch help

all: build

build:
	dune build

run: build
	dune exec _build/default/bin/main.exe

test: build
	dune test

clean:
	dune clean

watch:
	dune build --watch

fmt:
	ocamlformat --inplace $(LIBS) $(MAIN)

fmt-check:
	ocamlformat --check $(LIBS) $(MAIN)

help:
	@echo "Available targets:"
	@echo "  make build      - build the project"
	@echo "  make run        - build and run bin/main.exe"
	@echo "  make test       - run tests"
	@echo "  make clean      - remove _build/"
	@echo "  make watch      - rebuild automatically on changes"
	@echo "  make fmt        - format all .ml files with ocamlformat"
	@echo "  make fmt-check  - check formatting without modifying files"
	@echo "  make help       - show this message"
