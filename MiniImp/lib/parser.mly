%{
open Ast
%}

/* TOKENS */
%token <string> VAR
%token <int> INT

%token DEF MAIN WITH INPUT OUTPUT AS
%token IF THEN ELSE
%token WHILE DO
%token TRUE FALSE AND NOT

%token LPAREN RPAREN
%token ASSIGN     
%token SEMI      
%token LT         
%token PLUS MINUS
%token TIMES
%token EOF

/* Operator Precedences and Associativity
  Ordered from lowest precedence (top) to highest precedence (bottom).
*/
%right SEMI
%nonassoc ELSE DO
%left AND
%nonassoc NOT
%left PLUS MINUS
%left TIMES

/* Entry point of the grammar */
%start <Ast.prog> program

%%

// input_decl:
//   | v=VAR 
//       { InputVar(v, None) }
//   | v=VAR ASSIGN i=INT 
//       { InputVar(v, Some i) }
// ;

// program:
//   | DEF MAIN WITH INPUT input=input_decl OUTPUT output=VAR AS c=cmd EOF
//       { Prog(input, output, c) }
// ;

program:
  | DEF MAIN WITH INPUT input=VAR OUTPUT output=VAR AS c=cmd EOF
      /* We default the int option for InputVar to None since it's not in the grammar */
      { Prog(InputVar(input, None), OutputVar(output), c) }
;

cmd:
  | LPAREN c=cmd RPAREN
      { CmdParen c }
  | v=VAR ASSIGN e=expr
      { Assign(v, e) }
  | c1=cmd SEMI c2=cmd
      { Seq(c1, c2) }
  | IF b=bexpr THEN c1=cmd ELSE c2=cmd
      { If(b, c1, c2) }
  | WHILE b=bexpr DO c=cmd
      { While(b, c) }
;

expr:
  | v=VAR
      { Var v }
  | i=INT
      { Int i }
  | e1=expr PLUS e2=expr
      { Add(e1, e2) }
  | e1=expr MINUS e2=expr
      { Sub(e1, e2) }
  | e1=expr TIMES e2=expr
      { Mul(e1, e2) }
;

bexpr:
  | TRUE
      { True }
  | FALSE
      { False }
  | b1=bexpr AND b2=bexpr
      { And(b1, b2) }
  | NOT b=bexpr
      { Not b }
  | e1=expr LT e2=expr
      { Less(e1, e2) }
;