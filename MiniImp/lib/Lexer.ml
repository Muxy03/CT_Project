(* EXCEPTIONS *)
exception SyntaxError of string

(* TYPES *)
type token =
  | DEF
  | MAIN
  | WITH
  | INPUT
  | OUTPUT
  | AS
  | SKIP
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | TRUE
  | FALSE
  | AND
  | NOT
  | LPAREN
  | RPAREN
  | ASSIGN
  | SEMI
  | LESS
  | PLUS
  | MINUS
  | TIMES
  | VAR of string
  | INT of int
  | EOF

(* HELPERS *)
let string_of_token token =
  match token with
  | DEF -> "def"
  | MAIN -> "main"
  | WITH -> "with"
  | INPUT -> "input"
  | OUTPUT -> "output"
  | AS -> "as"
  | SKIP -> "skip"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | WHILE -> "while"
  | DO -> "do"
  | TRUE -> "true"
  | FALSE -> "false"
  | AND -> "and"
  | NOT -> "not"
  | LPAREN -> "("
  | RPAREN -> ")"
  | ASSIGN -> ":="
  | SEMI -> ";"
  | LESS -> "<"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | VAR v -> "variable '" ^ v ^ "'"
  | INT i -> Printf.sprintf "integer %d" i
  | EOF -> "<eof>"


let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_var_char c = is_letter c || is_digit c

let keyword_or_var w =
  match w with
  | "def" -> DEF
  | "main" -> MAIN
  | "with" -> WITH
  | "input" -> INPUT
  | "output" -> OUTPUT
  | "skip" -> SKIP
  | "as" -> AS
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "while" -> WHILE
  | "do" -> DO
  | "true" -> TRUE
  | "false" -> FALSE
  | "and" -> AND
  | "not" -> NOT
  | _ -> VAR w


(* Single-pass character scanner. Accumulates tokens in reverse (for efficiency) then reverses the
   list at the end. Two-character lookahead is used for := . *)
let tokenize (src : string) : token list =
  let n = String.length src in
  let tokens = ref [] in
  let pos = ref 0 in
  let emit t = tokens := t :: !tokens in
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
    | ';' ->
        emit SEMI ;
        incr pos
    | '<' ->
        emit LESS ;
        incr pos
    | '+' ->
        emit PLUS ;
        incr pos
    | '-' ->
        emit MINUS ;
        incr pos
    | '*' ->
        emit TIMES ;
        incr pos
    (* := is the only two-character token in MiniImp *)
    | ':' ->
        if !pos + 1 < n && src.[!pos + 1] = '=' then begin
          emit ASSIGN ;
          pos := !pos + 2
        end
        else raise (SyntaxError (Printf.sprintf "Unexpected character: %c" c))
    (* Integer literal: consume all consecutive digits *)
    | _ when is_digit c ->
        let start = !pos in
        while !pos < n && is_digit src.[!pos] do
          incr pos
        done ;
        let s = String.sub src start (!pos - start) in
        emit (INT (int_of_string s))
    (* Identifier or keyword: consume alphanumeric sequence, then resolve *)
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
