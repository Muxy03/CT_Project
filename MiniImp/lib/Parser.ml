(* Hand-written recursive-descent parser for MiniImp.

   Precedence is encoded in the call chain — each function parses one precedence level and delegates
   to the next-higher level for its operands: (lowest) parse_cmd_atom (IF, WHILE, assignment)
   parse_cmd (SEMI, left-assoc) parse_bexpr (AND) parse_bnot (NOT, unary prefix) parse_expr
   (comparison via LESS) parse_add (PLUS / MINUS, left-assoc) parse_mul (TIMES, left-assoc)
   parse_unary (unary minus as syntactic sugar) parse_atom (Var, Int, parenthesized exprs) (highest)

   The IF production handles the dangling-else by greedily consuming the ELSE branch inside
   parse_cmd_atom, matching the standard "match closest else." *)

open Ast
open Lexer

(* EXCEPTIONS *)
exception ParseError of string

let parse (src : string) : Ast.program =
  let toks = ref (Lexer.tokenize src) in
  let peek () =
    match !toks with [] -> raise (ParseError "unexpected end of input") | t :: _ -> t
  in
  let advance () =
    match !toks with
    | [] -> raise (ParseError "unexpected end of input")
    | t :: rest ->
        toks := rest ;
        t
  in
  let expect t =
    let got = advance () in
    if got <> t then
      raise
        (ParseError
           (Printf.sprintf "expected %s, got %s" (Lexer.string_of_token t)
              (Lexer.string_of_token got) ) )
  in
  let expect_var () =
    match advance () with
    | VAR v -> v
    | t -> raise (ParseError ("expected identifier, got " ^ Lexer.string_of_token t))
  in

  (* precedence: PLUS/MINUS (left-assoc), calls parse_mul for operands *)
  let rec parse_expr () = parse_add ()
  and parse_add () =
    let lhs = ref (parse_mul ()) in
    let continue_ = ref true in
    while !continue_ do
      match peek () with
      | PLUS ->
          ignore (advance ()) ;
          let rhs = parse_mul () in
          lhs := Add (!lhs, rhs)
      | MINUS ->
          ignore (advance ()) ;
          let rhs = parse_mul () in
          lhs := Sub (!lhs, rhs)
      | _ -> continue_ := false
    done ;
    !lhs
  (* precedence: TIMES (left-assoc), calls parse_unary for operands *)
  and parse_mul () =
    let lhs = ref (parse_unary ()) in
    let continue_ = ref true in
    while !continue_ do
      match peek () with
      | TIMES ->
          ignore (advance ()) ;
          let rhs = parse_unary () in
          lhs := Mul (!lhs, rhs)
      | _ -> continue_ := false
    done ;
    !lhs
  (* unary minus: syntactic sugar for (0 - e) *)
  and parse_unary () =
    match peek () with
    | MINUS ->
        ignore (advance ()) ;
        let e = parse_unary () in
        Sub (Int 0, e)
    | _ -> parse_atom ()
  and parse_atom () =
    match advance () with
    | VAR v -> Var v
    | INT i -> Int i
    | LPAREN ->
        let e = parse_expr () in
        expect RPAREN ;
        e
    | t -> raise (ParseError ("unexpected token in expression: " ^ Lexer.string_of_token t))
  (* Boolean expressions *)
  and parse_bexpr () = parse_band ()
  and parse_band () =
    let lhs = ref (parse_bnot ()) in
    let continue_ = ref true in
    while !continue_ do
      match peek () with
      | AND ->
          ignore (advance ()) ;
          let rhs = parse_bnot () in
          lhs := And (!lhs, rhs)
      | _ -> continue_ := false
    done ;
    !lhs
  and parse_bnot () =
    match peek () with
    | NOT ->
        ignore (advance ()) ;
        Not (parse_bnot ())
    | TRUE ->
        ignore (advance ()) ;
        True
    | FALSE ->
        ignore (advance ()) ;
        False
    | _ ->
        let e1 = parse_expr () in
        expect LESS ;
        let e2 = parse_expr () in
        Less (e1, e2)
  (* Commands — SEMI is left-associative *)
  and parse_cmd () =
    let lhs = ref (parse_cmd_atom ()) in
    let continue_ = ref true in
    while !continue_ do
      match peek () with
      | SEMI ->
          ignore (advance ()) ;
          let rhs = parse_cmd_atom () in
          lhs := Seq (!lhs, rhs)
      | _ -> continue_ := false
    done ;
    !lhs
  and parse_cmd_atom () =
    match peek () with
    | LPAREN ->
        ignore (advance ()) ;
        let c = parse_cmd () in
        expect RPAREN ;
        CmdParen c
    | SKIP ->
        ignore (advance ()) ;
        Skip
    | IF ->
        ignore (advance ()) ;
        let b = parse_bexpr () in
        expect THEN ;
        let c1 = parse_cmd () in
        expect ELSE ;
        let c2 = parse_cmd () in
        If (b, c1, c2)
    | WHILE ->
        ignore (advance ()) ;
        let b = parse_bexpr () in
        expect DO ;
        let c = parse_cmd () in
        While (b, c)
    | VAR _ ->
        let v = expect_var () in
        expect ASSIGN ;
        let e = parse_expr () in
        Assign (v, e)
    | t -> raise (ParseError ("unexpected token in command: " ^ Lexer.string_of_token t))
  in
  (* Top-level program structure *)
  expect DEF ;
  expect MAIN ;
  expect WITH ;
  expect INPUT ;
  let input = expect_var () in
  expect OUTPUT ;
  let output = expect_var () in
  expect AS ;
  let c = parse_cmd () in
  expect EOF ;
  Program (input, output, c)
