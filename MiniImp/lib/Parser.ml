(*EXCEPTIONS*)
exception ParserError of string

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
  | And of e * e (* <e> and <e> *)
  | Not of b (* not <e> *)
  | Less of e * e (* <e> < <e> *)

type cmd =
  | Assign of string * e (* <var> := <e> *)
  | Seq of cmd * cmd (* <cmd>;<cmd> *)
  | If of b * cmd * cmd (* if <b> then <cmd> else <cmd> *)
  | While of b * cmd (* while <b> do <cmd> *)

type inputVar = InputVar of string (* renamed from TInput *)
type outputVar = OutputVar of string (* renamed from TOutput *)

type prog =
  | Prog of
      inputVar * outputVar * cmd (* def main with input <var> output <var> as <cmd> *)


let rec string_of_ast p =
  match p with
  | Prog (InputVar i, OutputVar o, cmd) ->
      "Prog (InputVar " ^ i ^ ", OutputVar " ^ o ^ ", " ^ string_of_cmd cmd ^ ")"


and string_of_cmd c =
  match c with
  | Assign (v, e) -> "Assign(" ^ v ^ ", " ^ string_of_e e ^ ")"
  | Seq (c1, c2) -> "Seq(" ^ string_of_cmd c1 ^ ", " ^ string_of_cmd c2 ^ ")"
  | If (b, c1, c2) ->
      "If(" ^ string_of_b b ^ ", " ^ string_of_cmd c1 ^ ", " ^ string_of_cmd c2 ^ ")"
  | While (b, c) -> "While(" ^ string_of_b b ^ ", " ^ string_of_cmd c ^ ")"


and string_of_e e =
  match e with
  | Var v -> v
  | Int i -> string_of_int i
  | Add (e1, e2) -> "(" ^ string_of_e e1 ^ "+" ^ string_of_e e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_e e1 ^ "-" ^ string_of_e e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_e e1 ^ "*" ^ string_of_e e2 ^ ")"


and string_of_b b =
  match b with
  | True -> "true"
  | False -> "false"
  | And (e1, e2) -> "(" ^ string_of_e e1 ^ " and " ^ string_of_e e2 ^ ")"
  | Not b -> "(not " ^ string_of_b b ^ ")"
  | Less (e1, e2) -> "(" ^ string_of_e e1 ^ "<" ^ string_of_e e2 ^ ")"

(* PARSER *)
let parse src =
  let tokens = ref (Lexer.tokenize src) in

  let lookahead () =
    match !tokens with [] -> raise (ParserError "unexpected end of input") | t :: _ -> t
  in
  let consume () =
    match !tokens with
    | [] -> raise (ParserError "unexpected end of input")
    | t :: ts ->
        tokens := ts ;
        t
  in
  let expect t =
    if lookahead () = t then ignore (consume ())
    else raise (ParserError ("expected token: " ^ Lexer.string_of_token t))
  in
  let rec parse_prog () =
    expect (TKeyword TDef) ;
    expect (TKeyword TMain) ;
    expect (TKeyword TWith) ;
    expect (TKeyword TInput) ;
    let input_var =
      match consume () with
      | TVar v -> InputVar v
      | t ->
          raise (ParserError ("expected input variable, got: " ^ Lexer.string_of_token t))
    in
    expect (TKeyword TOutput) ;
    let output_var =
      match consume () with
      | TVar v -> OutputVar v
      | t ->
          raise
            (ParserError ("expected output variable, got: " ^ Lexer.string_of_token t))
    in
    expect (TKeyword TAs) ;
    let cmd = parse_cmd () in
    Prog (input_var, output_var, cmd)
  and parse_cmd () =
    let base =
      match lookahead () with
      | TKeyword TIf ->
          ignore (consume ()) ;
          let cond = parse_b () in
          expect (TKeyword TThen) ;
          let then_cmd = parse_cmd () in
          expect (TKeyword TElse) ;
          let else_cmd = parse_cmd () in
          If (cond, then_cmd, else_cmd)
      | TKeyword TWhile ->
          ignore (consume ()) ;
          let cond = parse_b () in
          expect (TKeyword TDo) ;
          let body = parse_cmd () in
          While (cond, body)
      | TLParent ->
          ignore (consume ()) ;
          let cmd = parse_cmd () in
          expect TRParent ;
          cmd
      | TVar v ->
          ignore (consume ()) ;
          expect TAssign ;
          let expr = parse_e () in
          Assign (v, expr)
      | t ->
          raise (ParserError ("unexpected token in command: " ^ Lexer.string_of_token t))
    in
    match lookahead () with
    | TSem ->
        ignore (consume ()) ;
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
        | t ->
            raise
              (ParserError ("unexpected token in expression: " ^ Lexer.string_of_token t))
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
        | t ->
            raise
              (ParserError ("unexpected token in boolean: " ^ Lexer.string_of_token t)))
  in
  parse_prog ()
