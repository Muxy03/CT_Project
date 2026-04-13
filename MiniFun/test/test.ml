open MiniFun.Ast
open MiniFun.RunTime

(* HELPERS *)
let rec string_of_mono = function
  | MiniFun.AlgoW.TInt -> "int"
  | MiniFun.AlgoW.TBool -> "bool"
  | MiniFun.AlgoW.TVar a -> a
  | MiniFun.AlgoW.TFun (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_mono t1) (string_of_mono t2)

let parse_string code =
  let lexbuf = Lexing.from_string code in
  try
    MiniFun.Parser.prog MiniFun.Lexer.read lexbuf
  with
  | MiniFun.Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.sprintf "Syntax error at line %d, column %d" 
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
      |> failwith

(* TEST RUNNERS *)
let test_parse name code expected_ast =
  Printf.printf "[Fragment 1 - Parse] %s: " name;
  try
    let ast = parse_string code in
    if ast = expected_ast then Printf.printf "✅ OK\n"
    else (
      Printf.printf "❌ FAILED (AST mismatch)\n";
      Printf.printf "   Got: %s\n" (string_of_expr ast)
    )
  with e -> Printf.printf "❌ FAILED (Exception: %s)\n" (Printexc.to_string e)

let test_eval name code expected_value =
  Printf.printf "[Fragment 2 - Eval] %s: " name;
  try
    let ast = parse_string code in
    let env = env_init None in
    let v = eval env ast in
    if v = expected_value then Printf.printf "✅ OK\n"
    else Printf.printf "❌ FAILED (Expected %s, got %s)\n" (string_of_value expected_value) (string_of_value v)
  with e -> Printf.printf "❌ FAILED (Exception: %s)\n" (Printexc.to_string e)

let test_eval_fail name code =
  Printf.printf "[Fragment 2 - Eval Fail] %s: " name;
  try
    let ast = parse_string code in
    let _ = eval (env_init None) ast in
    Printf.printf "❌ FAILED (Expected runtime error)\n"
  with RunTimeError _ -> Printf.printf "✅ OK (Caught error)\n"

let test_simple_type name code expected_type =
  Printf.printf "[Fragment 3 - Type] %s: " name;
  try
    let ast = parse_string code in
    let t = MiniFun.TypeChecker.typecheck [] ast in
    if t = expected_type then Printf.printf "✅ OK\n"
    else Printf.printf "❌ FAILED (Type mismatch)\n"
  with e -> Printf.printf "❌ FAILED (Exception: %s)\n" (Printexc.to_string e)

let test_simple_fail name code =
  Printf.printf "[Fragment 3 - Type Fail] %s: " name;
  try
    let ast = parse_string code in
    let _ = MiniFun.TypeChecker.typecheck [] ast in
    Printf.printf "❌ FAILED (Expected type error)\n"
  with MiniFun.TypeChecker.TypeError _ -> Printf.printf "✅ OK (Caught type error)\n"

let test_alg_w name code expected_type_str =
  Printf.printf "[Fragment 4 - AlgoW] %s: " name;
  try
    let ast = parse_string code in
    let t = MiniFun.AlgoW.typecheck [] ast in
    let t_str = string_of_mono t in
    if t_str = expected_type_str then Printf.printf "✅ OK\n"
    else Printf.printf "❌ FAILED (Expected %s, got %s)\n" expected_type_str t_str
  with e -> Printf.printf "❌ FAILED (Exception: %s)\n" (Printexc.to_string e)

let test_alg_w_fail name code =
  Printf.printf "[Fragment 4 - AlgoW Fail] %s: " name;
  try
    let ast = parse_string code in
    let _ = MiniFun.AlgoW.typecheck [] ast in
    Printf.printf "❌ FAILED (Expected inference error)\n"
  with Failure _ -> Printf.printf "✅ OK (Caught inference error)\n"

(* TEST SUITE *)
let run_tests () =
  Printf.printf "\n==== RUNNING FRAGMENT 1: PARSER ====\n";
  test_parse "Math Precedence" 
    "1 + 2 * 3" (Binop(Add, Num 1, Binop(Mul, Num 2, Num 3)));
  test_parse "Logic Precedence" 
    "~ true && false" (Binop(And, Not(Boolean true), Boolean false));
  test_parse "If expression" 
    "if x < 10 then 1 else 0" (If(Binop(Lt, Var "x", Num 10), Num 1, Num 0));
  test_parse "Letfun unannotated" 
    "letfun f x = x in f" (LetFun("f", "x", None, Var "x", Var "f"));
  test_parse "Letfun annotated" 
    "letfun f x : int -> int = x + 1 in f" (LetFun("f", "x", Some(Fun(Int, Int)), Binop(Add, Var "x", Num 1), Var "f"));

  Printf.printf "\n==== RUNNING FRAGMENT 2: RUNTIME/SEMANTICS ====\n";
  test_eval "Basic Arithmetic" "10 + 5 * 2" (VInt 20);
  test_eval "Logical Operators" "~ (5 < 3) && true" (VBool true);
  test_eval "Lexical Scoping" "let x = 10 in let f = fun y => x + y in let x = 20 in f 5" (VInt 15);
  test_eval "Recursive Factorial" 
    "letfun fact x = if x < 2 then 1 else x * fact (x - 1) in fact 5" (VInt 120);
  test_eval_fail "Type mismatch at runtime" "5 + true";
  test_eval_fail "Applying non-function" "let x = 5 in x 2";
  test_eval_fail "Unbound variable" "x + 1";

  Printf.printf "\n==== RUNNING FRAGMENT 3: SIMPLE TYPE SYSTEM ====\n";
  test_simple_type "Basic Math" "1 + 2" Int;
  test_simple_type "Logic" "true && ~ false" Bool;
  test_simple_type "Annotated Fun" "(fun x : int => x + 1) 5" Int;
  test_simple_type "Annotated Letfun" 
    "letfun fact x : int -> int = if x < 2 then 1 else x * fact (x - 1) in fact 5" Int;
  test_simple_fail "Missing annotation in fun" "fun x => x + 1";
  test_simple_fail "Missing annotation in letfun" "letfun f x = x in f 1";
  test_simple_fail "Wrong branch types" "if true then 1 else false";
  test_simple_fail "Applying Int" "1 2";

  Printf.printf "\n==== RUNNING FRAGMENT 4: ALGORITHM W (POLYMORPHISM) ====\n";
  test_alg_w "Constant Inference" "1 + 2" "int";
  test_alg_w "Identity function" "fun x => x" "('a1 -> 'a1)";
  test_alg_w "Higher order function" "fun f => fun x => f x" "('a1 -> ('a2 -> 'a3))"; (* Le variabili possono variare a seconda del tuo counter *)
  test_alg_w "Polymorphic Let-Generalization" 
    "let id = fun x => x in if id true then id 1 else 0" "int";
  test_alg_w "Recursive Inference (LetFun)" 
    "letfun f x = if x < 2 then 1 else x * f (x - 1) in f" "(int -> int)";
  test_alg_w_fail "Occurs Check (Infinite Type)" "fun x => x x";
  test_alg_w_fail "Unification Failure" "fun x => x + true";

  Printf.printf "\n==== ALL TESTS COMPLETED ====\n\n"

let () = run_tests ()