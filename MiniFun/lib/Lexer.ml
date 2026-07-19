(* Hand-written lexer for MiniFun.

   Single-pass character scanner with one-position lookahead for multi-character operators (=>, ->,
   &&). Tokens are accumulated and reversed at the end. *)

(* EXCEPTIONS *)
exception SyntaxError of string

(* TYPES *)
type token =
  | TRUE
  | FALSE
  | FUN
  | ARROW
  | TARROW
  | COLON
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | LETFUN
  | EQ
  | PLUS
  | MINUS
  | STAR
  | AND
  | LT
  | NOT
  | LPAREN
  | RPAREN
  | TINT
  | TBOOL
  | VAR of string
  | INT of int
  | EOF

(* HELPERS *)
let string_of_token t =
  match t with
  | TRUE -> "true"
  | FALSE -> "false"
  | FUN -> "fun"
  | ARROW -> "=>"
  | TARROW -> "->"
  | COLON -> ":"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | LET -> "let"
  | IN -> "in"
  | LETFUN -> "letfun"
  | EQ -> "="
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | AND -> "&&"
  | LT -> "<"
  | NOT -> "~"
  | LPAREN -> "("
  | RPAREN -> ")"
  | TINT -> "int"
  | TBOOL -> "bool"
  | VAR v -> Printf.sprintf "variable '%s'" v
  | INT i -> Printf.sprintf "integer %d" i
  | EOF -> "<eof>"


let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_var_char c = is_letter c || is_digit c

let keyword_or_var w =
  match w with
  | "true" -> TRUE
  | "false" -> FALSE
  | "fun" -> FUN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "let" -> LET
  | "in" -> IN
  | "letfun" -> LETFUN
  | "int" -> TINT
  | "bool" -> TBOOL
  | _ -> VAR w


let tokenize (src : string) : token list =
  let n = String.length src in
  let tokens = ref [] in
  let pos = ref 0 in
  let emit t = tokens := t :: !tokens in
  let next_is c = !pos + 1 < n && src.[!pos + 1] = c in
  while !pos < n do
    let c = src.[!pos] in
    match c with
    | ' ' | '\t' | '\n' | '\r' -> incr pos
    | '(' ->
        emit LPAREN ;
        incr pos
    | ')' ->
        emit RPAREN ;
        incr pos
    | '+' ->
        emit PLUS ;
        incr pos
    | '*' ->
        emit STAR ;
        incr pos
    | '<' ->
        emit LT ;
        incr pos
    | '~' ->
        emit NOT ;
        incr pos
    | ':' ->
        emit COLON ;
        incr pos
    | '=' ->
        if next_is '>' then begin
          emit ARROW ;
          pos := !pos + 2
        end
        else begin
          emit EQ ;
          incr pos
        end
    | '-' ->
        if next_is '>' then begin
          emit TARROW ;
          pos := !pos + 2
        end
        else begin
          emit MINUS ;
          incr pos
        end
    | '&' ->
        if next_is '&' then begin
          emit AND ;
          pos := !pos + 2
        end
        else raise (SyntaxError (Printf.sprintf "Unexpected character: %c" c))
    | _ when is_digit c ->
        let start = !pos in
        while !pos < n && is_digit src.[!pos] do
          incr pos
        done ;
        let s = String.sub src start (!pos - start) in
        emit (INT (int_of_string s))
    | _ when is_letter c ->
        let start = !pos in
        incr pos ;
        while !pos < n && is_var_char src.[!pos] do
          incr pos
        done ;
        let w = String.sub src start (!pos - start) in
        emit (keyword_or_var w)
    | _ -> raise (SyntaxError (Printf.sprintf "Unexpected character: %c" c))
  done ;
  emit EOF ;
  List.rev !tokens
