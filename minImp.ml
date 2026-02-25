(*miniImp*)

(*EXPECTIONS*)
exception LexerError of string

(* TYPES *)
type e =
  | Var of string
  | Int of int
  | Add of e * e (* <e> + <e> *)
  | Sub of e * e (* <e> - <e> *)
  | Mul of e * e (* <e> * <e> *)

type b =
  | True
  | False
  | And of e * e  (* <e> and <e> *)
  | Not of b      (* not <e> *)
  | Less of e * e (* <e> < <e> *)

type cmd =
  | Assign of string * e (* <var> := <e> *)
  | Seq of cmd * cmd     (* <cmd>;<cmd> *)
  | If of b * cmd * cmd  (* if <b> then <cmd> else <cmd> *)
  | While of b * cmd     (* while <b> do <cmd> *)

type prog =
  | Prog of string * string * cmd (* def main with input <var> output <var> as <cmd> *)

(*TOKENS*)

type keyword =
  | TDef
  | TMain
  | TWith
  | TInput
  | TOutput
  | TAs
  | TTrue
  | TFalse
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo

type op =
  | TNot
  | TAnd
  | TLess
  | TPlus
  | TMinus
  | TTimes

type token =
  | TKeyword of keyword
  | TOp of op
  | TSem
  | TLParent
  | TRParent
  | TAssign
  | TVar of string
  | TInt of int
  | TEOF

(* HELPERS *)
let is_digit c = match c with '0' .. '9' -> true | _ -> false
let is_alpha c = match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_alphanum c = is_alpha c || is_digit c

let read src n pred pos =
  let rec aux i = if i < n && pred src.[i] then aux (i + 1) else i in
  let end_pos = aux pos in
  String.sub src pos (end_pos - pos)


let string_of_token t =
  match t with
  | TKeyword k -> (
      match k with
      | TDef -> "def"
      | TMain -> "main"
      | TWith -> "with"
      | TInput -> "input"
      | TOutput -> "output"
      | TAs -> "as"
      | TTrue -> "true"
      | TFalse -> "false"
      | TIf -> "if"
      | TThen -> "then"
      | TElse -> "else"
      | TWhile -> "while"
      | TDo -> "do")
  | TOp o -> (
      match o with
      | TNot -> "not"
      | TAnd -> "and"
      | TLess -> "<"
      | TPlus -> "+"
      | TMinus -> "-"
      | TTimes -> "*")
  | TSem -> ";"
  | TLParent -> "("
  | TRParent -> ")"
  | TAssign -> ":="
  | TVar v -> "variable " ^ v
  | TInt i -> "integer " ^ string_of_int i
  | TEOF -> "end of file"


let tokenize (src : string) : token list =
  let n = String.length src in
  let rec tokenize_rec pos =
    if pos >= n then [ TEOF ] (* base case: end of string *)
    else
      let c = src.[pos] in
      match c with
      | ' ' | '\t' | '\n' | '\r' -> tokenize_rec (pos + 1) (* Skip whitespaces *)
      | ';' -> TSem :: tokenize_rec (pos + 1)
      | '(' -> TLParent :: tokenize_rec (pos + 1)
      | ')' -> TRParent :: tokenize_rec (pos + 1)
      | ':' ->
          if src.[pos + 1] = '=' then TAssign :: tokenize_rec (pos + 2)
          else raise (LexerError "expected '=' after ':'")
      | '<' -> TOp TLess :: tokenize_rec (pos + 1)
      | '+' -> TOp TPlus :: tokenize_rec (pos + 1)
      | '-' -> TOp TMinus :: tokenize_rec (pos + 1)
      | '*' -> TOp TTimes :: tokenize_rec (pos + 1)
      | _ when is_digit c ->
          (* Read Integers *)
          let num_str = read src n is_digit pos in
          TInt (int_of_string num_str) :: tokenize_rec (pos + String.length num_str)
      | _ when is_alpha c ->
          (* Read Keywords, Text Operators or Variables *)
          let word = read src n is_alphanum pos in
          let tok =
            match word with
            | "def" -> TKeyword TDef
            | "main" -> TKeyword TMain
            | "with" -> TKeyword TWith
            | "input" -> TKeyword TInput
            | "output" -> TKeyword TOutput
            | "as" -> TKeyword TAs
            | "true" -> TKeyword TTrue
            | "false" -> TKeyword TFalse
            | "if" -> TKeyword TIf
            | "then" -> TKeyword TThen
            | "else" -> TKeyword TElse
            | "while" -> TKeyword TWhile
            | "do" -> TKeyword TDo
            | "not" -> TOp TNot
            | "and" -> TOp TAnd
            | _ -> TVar word (* If it's not a keyword, it's a variable *)
          in
          tok :: tokenize_rec (pos + String.length word)
      | _ -> raise (LexerError ("unknown symbol: " ^ String.make 1 c))
  in
  tokenize_rec 0


let parse src =
  let tokens = ref (tokenize src) in

  let lookahead () =
    match !tokens with [] -> raise (LexerError "unexpected end of input") | t :: _ -> t
  in
  let consume () =
    match !tokens with
    | [] -> raise (LexerError "unexpected end of input")
    | t :: ts ->
        tokens := ts ; t
  in
  let expect t =
    if lookahead () = t then ignore (consume ())
    else raise (LexerError ("expected token: " ^ string_of_token t))
  in
  let rec parse_prog () =
    expect (TKeyword TDef) ;
    expect (TKeyword TMain) ;
    expect (TKeyword TWith) ;
    expect (TKeyword TInput) ;
    let input_var =
      match consume () with
      | TVar v -> v
      | t -> raise (LexerError ("expected input variable, got: " ^ string_of_token t))
    in
    expect (TKeyword TOutput) ;
    let output_var =
      match consume () with
      | TVar v -> v
      | t -> raise (LexerError ("expected output variable, got: " ^ string_of_token t))
    in
    expect (TKeyword TAs) ;
    let cmd = parse_cmd () in
    Prog (input_var, output_var, cmd)
  and parse_cmd () =
  let base = match lookahead () with
    | TKeyword TIf ->
        ignore (consume ());
        let cond = parse_b () in
        expect (TKeyword TThen);
        let then_cmd = parse_cmd () in
        expect (TKeyword TElse);
        let else_cmd = parse_cmd () in
        If (cond, then_cmd, else_cmd)
    | TKeyword TWhile ->
        ignore (consume ());
        let cond = parse_b () in
        expect (TKeyword TDo);
        let body = parse_cmd () in
        While (cond, body)
    | TLParent ->
        ignore (consume ());
        let cmd = parse_cmd () in
        expect TRParent;
        cmd
    | TVar v ->
        ignore (consume ());
        expect TAssign;
        let expr = parse_e () in
        Assign (v, expr)
    | t -> raise (LexerError ("unexpected token in command: " ^ string_of_token t))
  in
  match lookahead () with
  | TSem ->
      ignore (consume ());
      let rest = parse_cmd () in
      Seq (base, rest)
  | _ -> base
  and parse_e () =
    let rec parse_e_prec min_prec =
      let parse_primary () =
        match lookahead () with
        | TInt i ->
            ignore (consume ()) ;
            Int i
        | TVar v ->
            ignore (consume ()) ;
            Var v
        | TLParent ->
            ignore (consume ()) ;
            let e = parse_e_prec 0 in
            expect TRParent ;
            e
        | t -> raise (LexerError ("unexpected token in expression: " ^ string_of_token t))
      in
      let rec parse_binop lhs prec =
        let op_prec =
          match lookahead () with
          | TOp TPlus -> 1
          | TOp TMinus -> 1
          | TOp TTimes -> 2
          | _ -> -1
        in
        if op_prec >= prec then begin
          let make_node =
            match consume () with
            | TOp TPlus -> fun l r -> Add (l, r)
            | TOp TMinus -> fun l r -> Sub (l, r)
            | TOp TTimes -> fun l r -> Mul (l, r)
            | _ -> assert false
          in
          let rhs = parse_e_prec (op_prec + 1) in
          let new_lhs = make_node lhs rhs in
          parse_binop new_lhs prec
        end
        else lhs
      in
      let lhs = parse_primary () in
      parse_binop lhs min_prec
    in
    parse_e_prec 0
  and parse_b () =
    match lookahead () with
    | TKeyword TTrue ->
        ignore (consume ()) ;
        True
    | TKeyword TFalse ->
        ignore (consume ()) ;
        False
    | TOp TNot ->
        ignore (consume ()) ;
        let b = parse_b () in
        Not b
    | _ -> (
        let e1 = parse_e () in
        match lookahead () with
        | TOp TAnd ->
            ignore (consume ()) ;
            let e2 = parse_e () in
            And (e1, e2)
        | TOp TLess ->
            ignore (consume ()) ;
            let e2 = parse_e () in
            Less (e1, e2)
        | t -> raise (LexerError ("unexpected token in boolean: " ^ string_of_token t)))
  in
  parse_prog ()
