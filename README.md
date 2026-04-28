```bash
.
├── MiniFun
│   ├── bin
│   │   └── main.ml
│   ├── _build
│   ├── lib
│   │   ├── AlgoW.ml
│   │   ├── Ast.ml
│   │   ├── dune
│   │   ├── lexer.mll
│   │   ├── parser.mly
│   │   ├── RunTime.ml
│   │   └── TypeChecker.ml
│   ├── makefile
│   ├── sources
│   │   └── main.fun
│   └── test
│       └── test.ml
├── MiniImp
│   ├── bin
│   │   └── main.ml
│   ├── _build
│   ├── lib
│   │   ├── Ast.ml
│   │   ├── lexer.mll
│   │   ├── manual
│   │   │   ├── Helpers.ml
│   │   │   ├── Lexer.ml
│   │   │   └── Parser.ml
│   │   ├── parser.mly
│   │   └── Runtime.ml
│   ├── makefile
│   ├── sources
│   │   └── main.imp
│   └── test
│       └── test.ml
└── README.md
```

Implementation's Choice:
- depedencies: melheir ocamllex
MiniImp:
  - hash table for memory (w.h.p constant time operation)
  - "%right ELSE DO" to avoid "shift/reduce conflicts were arbitrarily resolved" 
    - %right ELSE DO before %right SEMI
    - %right SEMI before %nonassoc ELSE DO
MiniFun:
  - RunTime ignore types because typechecking was already done.

TODO:
- MiniFun: Check Correct Associative of '->'
- MiniFun: Check Typecheckers
- MiniFun: Check test 3 of part 4
- MiniFun: Refactoring Claude
- MiniFun: Collect implementation choice
- MiniImp: Collect implementation choice
