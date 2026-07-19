open MiniFun.Ast
open MiniFun.RunTime

type outcome =
  | Passed
  | Failed of string (* failure reason *)

let results : (string * outcome) list ref = ref []

let print_section title =
  Printf.printf "\n==================== %s ====================\n" title

let record name outcome =
  (match outcome with
   | Passed -> Printf.printf "  [PASS] %s\n" name
   | Failed reason -> Printf.printf "  [FAIL] %s -- %s\n" name reason);
  results := (name, outcome) :: !results

let parse_string (src : string) : MiniFun.Ast.expr =
  try MiniFun.Parser.parse src with
  | MiniFun.Lexer.SyntaxError msg -> failwith (Printf.sprintf"Lexing error: %s" msg)
  | MiniFun.Parser.ParseError msg -> failwith (Printf.sprintf"Parsing error: %s" msg)

let rec string_of_mono m= match m with
  | MiniFun.AlgoW.TInt -> "int"
  | MiniFun.AlgoW.TBool -> "bool"
  | MiniFun.AlgoW.TVar a -> a
  | MiniFun.AlgoW.TFun (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_mono t1) (string_of_mono t2)

(* -------------------------------------------------------------------- *)
(* Fragment 1: parser                                                   *)
(* -------------------------------------------------------------------- *)
let test_parse name code expected_ast =
  let outcome =
    try
      let ast = parse_string code in
        if ast = expected_ast then Passed
        else Failed (Printf.sprintf "AST mismatch: got %s" (string_of_expr ast))
    with e -> Failed (Printexc.to_string e)
  in
    record name outcome

(* -------------------------------------------------------------------- *)
(* Fragment 2: runtime / semantics                                      *)
(* -------------------------------------------------------------------- *)
let test_eval name code expected_value =
  let outcome =
    try
      let ast = parse_string code in
        let v = eval (env_init None) ast in
          if v = expected_value then Passed
          else Failed (Printf.sprintf "expected %s, got %s" (string_of_value expected_value) (string_of_value v))
    with e -> Failed (Printexc.to_string e)
  in
    record name outcome

let test_eval_fail name code =
  let outcome =
    try
      let ast = parse_string code in
        let v = eval (env_init None) ast in
        Failed (Printf.sprintf "expected a runtime error, got %s" (string_of_value v))
    with
    | RunTimeError _ -> Passed
    | e -> Failed (Printf.sprintf "wrong exception: %s" (Printexc.to_string e))
  in
    record name outcome

(* -------------------------------------------------------------------- *)
(* Fragment 3: simple (non-inferring) type system                      *)
(* -------------------------------------------------------------------- *)
let test_simple_type name code expected_type =
  let outcome =
    try
      let ast = parse_string code in
        let t = MiniFun.TypeChecker.typecheck [] ast in
          if t = expected_type then Passed
          else Failed (Printf.sprintf "expected %s, got %s" (string_of_typo expected_type) (string_of_typo t))
    with e -> Failed (Printexc.to_string e)
  in
    record name outcome

let test_simple_fail name code =
  let outcome =
    try
      let ast = parse_string code in
      let t = MiniFun.TypeChecker.typecheck [] ast in
      Failed (Printf.sprintf "expected a type error, got %s" (string_of_typo t))
    with
    | MiniFun.TypeChecker.TypeError _ -> Passed
    | e -> Failed (Printf.sprintf "wrong exception: %s" (Printexc.to_string e))
  in
  record name outcome

(* -------------------------------------------------------------------- *)
(* Fragment 4: Algorithm W (Hindley-Milner type inference)              *)
(* -------------------------------------------------------------------- *)
let test_alg_w name code expected_type_str =
  let outcome =
    try
      let ast = parse_string code in
      let t = MiniFun.AlgoW.typecheck [] ast in
      let t_str = string_of_mono t in
      if t_str = expected_type_str then Passed
      else Failed (Printf.sprintf "expected %s, got %s" expected_type_str t_str)
    with e -> Failed (Printexc.to_string e)
  in
  record name outcome

let test_alg_w_fail name code =
  let outcome =
    try
      let ast = parse_string code in
      let t = MiniFun.AlgoW.typecheck [] ast in
      Failed (Printf.sprintf "expected an inference error, got %s" (string_of_mono t))
    with
    | Failure _ -> Passed
    | e -> Failed (Printf.sprintf "wrong exception: %s" (Printexc.to_string e))
  in
  record name outcome

let run_tests () =
  print_section "FRAGMENT 1: PARSER";
  test_parse "Math precedence"
    "1 + 2 * 3" (Binop (Add, Num 1, Binop (Mul, Num 2, Num 3)));
  test_parse "Logic precedence"
    "~ true && false" (Binop (And, Not (Boolean true), Boolean false));
  test_parse "If expression"
    "if x < 10 then 1 else 0" (If (Binop (Lt, Var "x", Num 10), Num 1, Num 0));
  test_parse "Letfun, unannotated"
    "letfun f x = x in f" (LetFun ("f", "x", None, Var "x", Var "f"));
  test_parse "Letfun, annotated"
    "letfun f x : int -> int = x + 1 in f"
    (LetFun ("f", "x", Some (Fun (Int, Int)), Binop (Add, Var "x", Num 1), Var "f"));

  print_section "FRAGMENT 2: RUNTIME / SEMANTICS";
  test_eval "Basic arithmetic" "10 + 5 * 2" (VInt 20);
  test_eval "Logical operators" "~ (5 < 3) && true" (VBool true);
  test_eval "Lexical scoping"
    "let x = 10 in let f = fun y => x + y in let x = 20 in f 5" (VInt 15);
  test_eval "Recursive factorial"
    "letfun fact x = if x < 2 then 1 else x * fact (x - 1) in fact 5" (VInt 120);
  test_eval_fail "Type mismatch at runtime" "5 + true";
  test_eval_fail "Applying a non-function" "let x = 5 in x 2";
  test_eval_fail "Unbound variable" "x + 1";

  print_section "FRAGMENT 3: SIMPLE TYPE SYSTEM";
  test_simple_type "Basic math" "1 + 2" Int;
  test_simple_type "Logic" "true && ~ false" Bool;
  test_simple_type "Annotated fun" "(fun x : int => x + 1) 5" Int;
  test_simple_type "Annotated letfun"
    "letfun fact x : int -> int = if x < 2 then 1 else x * fact (x - 1) in fact 5" Int;
  test_simple_fail "Missing annotation in fun" "fun x => x + 1";
  test_simple_fail "Missing annotation in letfun" "letfun f x = x in f 1";
  test_simple_fail "Mismatched branch types" "if true then 1 else false";
  test_simple_fail "Applying an int" "1 2";

  print_section "FRAGMENT 4: ALGORITHM W (POLYMORPHISM)";
  test_alg_w "Constant inference" "1 + 2" "int";
  test_alg_w "Identity function" "fun x => x" "('a1 -> 'a1)";
  (* Fresh type-variable names depend on AlgoW's internal counter, so the
     exact numbering below may need adjusting if that counter's starting
     point or increment order changes. *)
  test_alg_w "Higher-order function" "fun f => fun x => f x" "(('a3 -> 'a4) -> ('a3 -> 'a4))";
  test_alg_w "Polymorphic let-generalization"
    "let id = fun x => x in if id true then id 1 else 0" "int";
  test_alg_w "Recursive inference (letfun)"
    "letfun f x = if x < 2 then 1 else x * f (x - 1) in f" "(int -> int)";
  test_alg_w_fail "Occurs check (infinite type)" "fun x => x x";
  test_alg_w_fail "Unification failure" "fun x => x + true";

  print_section "SUMMARY";
  let all = List.rev !results in
  let failed = List.filter (fun (_, outcome) -> outcome <> Passed) all in
  let total = List.length all and failed_count = List.length failed in
  Printf.printf "  %d / %d tests passed\n" (total - failed_count) total;
  if failed <> [] then begin
    Printf.printf "\n  Failed tests:\n";
    List.iter
      (fun (name, outcome) ->
        match outcome with
        | Failed reason -> Printf.printf "    - %s (%s)\n" name reason
        | Passed -> ())
      failed
  end;
  Printf.printf "\n";
  if failed <> [] then exit 1

let () = run_tests ()
