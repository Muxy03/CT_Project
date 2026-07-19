# CT_Project — Compilation Techniques

Two compilers for the Compilation Techniques course at the University of Pisa:

- **MiniImp** — A minimal imperative language with data-flow analysis, optimizations, and LLVM IR code generation.
- **MiniFun** — A minimal functional language with type checking (simple + Hindley-Milner / Algorithm W) and an environment-based interpreter.

Both subprojects use **hand-written** lexers and parsers (no `ocamllex`/`menhir` generators).

## Project Structure

```
├── MiniFun/
│   ├── lib/
│   │   ├── Ast.ml           — AST definitions
│   │   ├── Lexer.ml         — Hand-written lexer
│   │   ├── Parser.ml        — Hand-written parser (recursive descent)
│   │   ├── RunTime.ml       — Interpreter (environment-based, closures)
│   │   ├── TypeChecker.ml   — Simple annotation-based type checker
│   │   └── AlgoW.ml         — Algorithm W (Hindley-Milner inference)
│   ├── bin/main.ml          — CLI entry point
│   ├── test/test.ml         — Test suite
│   ├── sources/             — Sample .fun programs
│   └── Makefile
├── MiniImp/
│   ├── lib/
│   │   ├── Ast.ml           — AST definitions
│   │   ├── Lexer.ml         — Hand-written lexer
│   │   ├── Parser.ml        — Hand-written parser (recursive descent)
│   │   ├── Runtime.ml       — Interpreter
│   │   ├── Cfg.ml           — Control-flow graph construction
│   │   ├── DataFlow.ml      — Liveness analysis, reaching definitions
│   │   ├── Optimize.ml      — Constant folding, propagation, dead store elim.
│   │   └── Llvm.ml          — LLVM IR code generation
│   ├── bin/main.ml          — CLI entry point
│   ├── test/test.ml         — Test suite
│   ├── sources/             — Sample .imp programs
│   ├── wrapper.c            — C wrapper for compiled LLVM output
│   └── Makefile
└── Dockerfile               — Docker build setup
```

## Quick Start

### Without Docker (native)

**Prerequisites:** OCaml 5.4+, dune 3.21+, LLVM toolchain (optional, for `make compile`).

```bash
# MiniFun — run a program
cd MiniFun && make run FILE=main.fun

# MiniFun — run tests
cd MiniFun && make test

# MiniImp — run a program (interpreter + LLVM IR)
cd MiniImp && make run FILE=factorial.imp

# MiniImp — run tests
cd MiniImp && make test

# MiniImp — compile LLVM IR to native binary
cd MiniImp && make compile FILE=factorial.imp
```

### With Docker

```bash
# Build the image
docker build -t ct_project .

# Run a shell inside the container
docker run -it --rm ct_project

# Inside the container:
#   cd MiniFun && make test
#   cd MiniImp && make run FILE=factorial.imp
```

## Implementation Highlights

### Hand-written Lexer & Parser

Both languages use hand-written scanners and recursive-descent parsers. The lexer processes input character-by-character with manual two-character lookahead for multi-char operators (`:=`, `=>`, `->`, `&&`). The parser encodes operator precedence in the call chain (e.g., `parse_and` → `parse_lt` → `parse_plus_minus` → `parse_star`), avoiding the need for a precedence table or generated LR tables.

### MiniImp Pipeline

```
Source (.imp) → AST → CFG → Data-Flow Analysis → Optimization → LLVM IR
```

- **CFG:** Minimal basic blocks (one statement per node), built inductively.
- **Data-Flow:** Worklist-based liveness (backward) and reaching definitions (forward).
- **Optimizations:** Constant propagation, constant folding, dead store elimination — iterated to fixpoint.
- **LLVM IR:** Memory-based translation (alloca/store/load); LLVM's `mem2reg` pass can promote to SSA.

### MiniFun Pipeline

```
Source (.fun) → AST → Type Checking / Type Inference → Evaluation
```

- **Simple Type Checker:** Enforces explicit type annotations.
- **Algorithm W:** Full Hindley-Milner inference with let-polymorphism, unification, and occurs check.
- **Runtime:** Environment-passing interpreter with closures and recursive closures for `letfun`.
