%{
  open Ast
%}

/* TOKENS */
%token <int> INT
%token <string> VAR

%token TRUE FALSE FUN ARROW LET EQ IN LETFUN IF THEN ELSE
%token PLUS MINUS STAR AND LT NOT LPAREN RPAREN EOF

/* Operator Precedences and Associativity
  Ordered from lowest precedence (top) to highest precedence (bottom).
*/
%nonassoc IN ELSE ARROW
%left AND
%left LT
%left PLUS MINUS
%left STAR
%nonassoc NOT

/* Entry point of the grammar */
%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | FUN; x = VAR; ARROW; e = expr { Fun(x, e) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1, e2, e3) }
  | LET; x = VAR; EQ; e1 = expr; IN; e2 = expr { Let(x, e1, e2) }
  | LETFUN; f = VAR; x = VAR; EQ; e1 = expr; IN; e2 = expr { LetFun(f, x, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop(Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop(Sub, e1, e2) }
  | e1 = expr; STAR; e2 = expr { Binop(Mul, e1, e2) }
  | e1 = expr; AND; e2 = expr { Binop(And, e1, e2) }
  | e1 = expr; LT; e2 = expr { Binop(Lt, e1, e2) }
  | NOT; e = expr { Not(e) }
  | e = app_expr { e }

app_expr:
  | e1 = app_expr; e2 = base_expr { App(e1, e2) }
  | e = base_expr { e }

base_expr:
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | x = VAR { Var x }
  | LPAREN; e = expr; RPAREN { e }