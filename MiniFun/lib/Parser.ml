(*
     %nonassoc IN ELSE   (lowest)
     %right TARROW ARROW
     %left  AND
     %left  LT
     %left  PLUS MINUS
     %left  STAR
     %nonassoc NOT       (highest)
 *)

open Ast
open Lexer

exception ParseError of string

let parse (src : string) : Ast.expr =
  let toks = ref (Lexer.tokenize src) in
  let peek () = match !toks with
    | [] -> raise (ParseError "unexpected end of input")
    | t :: _ -> t
  in
  let advance () = match !toks with
    | [] -> raise (ParseError "unexpected end of input")
    | t :: rest -> toks := rest; t
  in
  let expect t =
    let got = advance () in
      if got <> t then raise (ParseError (Printf.sprintf "expected %s, got %s" (Lexer.string_of_token t) (Lexer.string_of_token got)))
  in
  let expect_var () = match advance () with
    | VAR v -> v
    | t -> raise (ParseError (Printf.sprintf "expected identifier, got %s" (Lexer.string_of_token t)))
  in
  let rec parse_typo () =
    let t = parse_typo_atom () in
    match peek () with
    | TARROW ->
        ignore (advance ());
        let rest = parse_typo () in
          Fun (t, rest)
    | _ -> t

  and parse_typo_atom () =
    match advance () with
    | TINT -> Int
    | TBOOL -> Bool
    | LPAREN ->
        let t = parse_typo () in
          expect RPAREN; t
    | t -> raise (ParseError (Printf.sprintf "unexpected token in type: %s" (Lexer.string_of_token t)))
  in
  let rec parse_operand () = match peek () with
    | FUN | IF | LET | LETFUN -> parse_special_form ()
    | NOT -> ignore (advance ());
        let e = parse_operand () in
          Not e
    | _ -> parse_app ()

  and parse_special_form () = match advance () with
    | FUN ->
        let x = expect_var () in
          let t = match peek () with
            | COLON ->
                ignore (advance ());
                Some (parse_typo ())
            | _ -> None
          in
            expect ARROW;
            let body = parse_expr () in
              Func (x, t, body)
    | IF ->
        let b = parse_expr () in
          expect THEN;
        let e1 = parse_expr () in
          expect ELSE;
        let e2 = parse_expr () in
          If (b, e1, e2)
    | LET ->
        let x = expect_var () in
          expect EQ;
        let e1 = parse_expr () in
          expect IN;
        let e2 = parse_expr () in
          Let (x, e1, e2)
    | LETFUN ->
        let f = expect_var () in
          let x = expect_var () in
            let t = match peek () with
              | COLON -> ignore (advance ()); Some (parse_typo ())
              | _ -> None
            in
              expect EQ;
              let e1 = parse_expr () in
                expect IN;
              let e2 = parse_expr () in
                LetFun (f, x, t, e1, e2)
    | t -> raise (ParseError (Printf.sprintf "unexpected token: %s" (Lexer.string_of_token t)))
  and parse_app () =
    let lhs = ref (parse_base ()) in
      let continue_ = ref true in
        while !continue_ do
          match peek () with
          | INT _ | TRUE | FALSE | VAR _ | LPAREN ->
              let rhs = parse_base () in
                lhs := App (!lhs, rhs)
          | _ -> continue_ := false
        done;
    !lhs

  and parse_base () = match advance () with
    | INT i -> Num i
    | TRUE -> Boolean true
    | FALSE -> Boolean false
    | VAR v -> Var v
    | LPAREN ->
        let e = parse_expr () in
          expect RPAREN; e
    | t -> raise (ParseError (Printf.sprintf "unexpected token in expression: %s" (Lexer.string_of_token t)))

  and parse_star () =
    let lhs = ref (parse_operand ()) in
    let continue_ = ref true in
      while !continue_ do
        match peek () with
        | STAR -> ignore (advance ());
            let rhs = parse_operand () in
              lhs := Binop (Mul, !lhs, rhs)
        | _ -> continue_ := false
      done;
    !lhs

  and parse_plus_minus () =
    let lhs = ref (parse_star ()) in
      let continue_ = ref true in
        while !continue_ do
          match peek () with
          | PLUS -> ignore (advance ());
              let rhs = parse_star () in
                lhs := Binop (Add, !lhs, rhs)
          | MINUS -> ignore (advance ());
              let rhs = parse_star () in
                lhs := Binop (Sub, !lhs, rhs)
          | _ -> continue_ := false
        done;
      !lhs

  and parse_lt () =
    let lhs = ref (parse_plus_minus ()) in
      let continue_ = ref true in
        while !continue_ do
          match peek () with
          | LT -> ignore (advance ());
              let rhs = parse_plus_minus () in
                lhs := Binop (Lt, !lhs, rhs)
          | _ -> continue_ := false
        done;
    !lhs

  and parse_and () =
    let lhs = ref (parse_lt ()) in
    let continue_ = ref true in
    while !continue_ do
      match peek () with
      | AND -> ignore (advance ());
          let rhs = parse_lt () in
            lhs := Binop (And, !lhs, rhs)
      | _ -> continue_ := false
    done;
    !lhs

  and parse_expr () = match peek () with
    | FUN | IF | LET | LETFUN -> parse_special_form ()
    | _ -> parse_and ()
  in
    let e = parse_expr () in
      expect EOF; e
