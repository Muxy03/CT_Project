(*EXCEPTIONS*)
exception LexerError of string

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
  | TEOF -> "EOF"


(* LEXER *)
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
      | _ when Helpers.is_digit c ->
          (* Read Integers *)
          let num_str = Helpers.read src n Helpers.is_digit pos in
          TInt (int_of_string num_str) :: tokenize_rec (pos + String.length num_str)
      | _ when Helpers.is_alpha c ->
          (* Read Keywords, Text Operators or Variables *)
          let word = Helpers.read src n Helpers.is_alphanum pos in
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
