```bash
.
├── MiniFun
│   ├── _build
│   ├── bin
│   │   ├── dune
│   │   └── main.ml
│   ├── dune-project
│   ├── lib
│   │   └── dune
│   ├── MiniFun.opam
│   └── test
│       ├── dune
│       └── test_MiniFun.ml
├── MiniImp
│   ├── _build
│   ├── bin
│   │   ├── dune
│   │   └── main.ml
│   ├── dune-project
│   ├── lib
│   │   ├── Ast.ml
│   │   ├── dune
│   │   ├── lexer.mll
│   │   ├── manual
│   │   │   ├── Helpers.ml
│   │   │   ├── Lexer.ml
│   │   │   └── Parser.ml
│   │   ├── parser.mly
│   │   └── Runtime.ml
│   ├── makefile
│   ├── MiniImp.opam
│   ├── sources
│   │   └── main.imp
│   └── test
│       ├── dune
│       └── test_MiniImp.ml
└── README.md
```

Implementation's Choice:
- depedencies: melheir ocamllex
- support input_var:=value to set init value for input variable
- the body of the program is wrapped automatically in CmdParen
- hash table for memory (w.h.p constant time operation)
- "%right ELSE DO" to avoid "shift/reduce conflicts were arbitrarily resolved" 
  - %right ELSE DO before %right SEMI
  - %right SEMI before %nonassoc ELSE DO