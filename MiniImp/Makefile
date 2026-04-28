LIBS    := $(shell find lib -name "*.ml")
BUILD   := _build/default/bin/main.exe
TESTS   := _build/default/test/test.exe
MAIN    := bin/main.ml
OUTPUT  := generated
FILE    := main.imp

.PHONY: all build run test clean watch help

all: init build

init:
	opam switch
	@echo "Run 'eval \$$(opam env)' in your shell to sync the environment"

build:
	dune build

run: build
	dune exec $(BUILD) -- sources/$(FILE)

compile: run
	opt -passes=mem2reg -S $(OUTPUT)/output.ll -o $(OUTPUT)/optimized.ll;
	llc -filetype=obj $(OUTPUT)/optimized.ll -o $(OUTPUT)/miniimp.o;
	clang wrapper.c $(OUTPUT)/miniimp.o -o $(OUTPUT)/compiled

test: build
	dune exec $(TESTS)
	
clean:
	dune clean;
	rm $(OUTPUT)/*

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
