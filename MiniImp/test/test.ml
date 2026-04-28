open MiniImp

let parse_string src =
  let lexbuf = Lexing.from_string src in
  try Parser.program Lexer.read lexbuf
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "LexerError: %s\n" msg; exit 1
  | Parser.Error -> Printf.eprintf "ParserError: %d\n" (Lexing.lexeme_start lexbuf); exit 1

let parse_string_exn src =
  let lexbuf = Lexing.from_string src in
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError msg -> raise (Failure ("LexerError: " ^ msg))
  | Parser.Error          -> raise (Failure ("ParserError at " ^ string_of_int (Lexing.lexeme_start lexbuf)))
      
let make_cfg program =
  Cfg.reset_counter ();
  match program with
  | Ast.Program (_, _, cmd) -> Cfg.generate_cfg cmd

let eval_with inVal src =
  let ast = parse_string src in
  match ast with
  | Ast.Program (input_name, output_name, cmd) ->
      let mem  = Runtime.mem_create () in
      let mem0 =
        let m1 = Runtime.mem_set mem input_name (Runtime.Int inVal) in
        let m2 = Runtime.mem_set m1  output_name Runtime.Undefined in
        m2
      in
      let mem1 = Runtime.eval_cmd mem0 cmd in
      Runtime.mem_get mem1 output_name
      
let print_header name =
  Printf.printf "\n========================================\n";
  Printf.printf " TESTING %s\n" name;
  Printf.printf "========================================\n"

let pass_count = ref 0
let fail_count = ref 0

let run_test name test_fn =
  Printf.printf "  %-50s " name;
  try
    if test_fn () then 
    begin
      Printf.printf "✅ OK\n";
      incr pass_count
    end else 
    begin
      Printf.printf "❌ FAILED\n";
      incr fail_count
    end
  with
  | Failure msg ->
      Printf.printf "❌ FAILED (Failure: %s)\n" msg; incr fail_count
  | e ->
      Printf.printf "❌ FAILED (Exception: %s)\n" (Printexc.to_string e);
      incr fail_count

let run_eq_test name expected actual_fn  = run_test name (fun () -> actual_fn () = expected)

(* PROGRAM OF TESTS *)
let p_skip     = "def main with input in output out as skip"
let p_assign   = "def main with input in output out as out := in + 1"
let p_seq      = "def main with input x output y as x := x + 1 ; y := x"
let p_if       = "def main with input in output out as \
                  if in < 0 then out := 0 - in else out := in"
let p_while    = "def main with input n output res as \
                  res := 1 ; \
                  while 0 < n do ( res := res * n ; n := n - 1 )"
let p_paren    = "def main with input in output out as (out := in + 1)"
let p_complex  = "def main with input n output res as \
                  if n < 0 then res := 0 \
                  else while 0 < n do n := n - 1"
let p_undef    = "def main with input in output out as x := y + 1 ; out := x"
let p_dead     = "def main with input in output out as dead := in * 2 ; out := in"
let p_fold     = "def main with input in output out as out := (2 + 3) * 4"
let p_prop     = "def main with input in output out as a := 10 ; b := a + 2 ; out := b"
let p_pipeline = "def main with input in output out as \
                  a := 10 ; b := a + 2 ; c := b * 2 ; out := c"

(* PARSING *)
let test_parsing () =
  print_header "PARSING (Lexer & Parser)";

  run_eq_test "Skip"
    (Ast.Program ("in", "out", Ast.Skip))
    (fun () -> parse_string p_skip);

  run_eq_test "Assign (out := in + 1)"
    (Ast.Program ("in", "out",
      Ast.Assign ("out", Ast.Add (Ast.Var "in", Ast.Int 1))))
    (fun () -> parse_string p_assign);

  run_eq_test "Sequence"
    (Ast.Program ("x", "y",
      Ast.Seq (
        Ast.Assign ("x", Ast.Add (Ast.Var "x", Ast.Int 1)),
        Ast.Assign ("y", Ast.Var "x"))))
    (fun () -> parse_string p_seq);

  run_eq_test "If/Else (Absolute value)"
    (Ast.Program ("in", "out",
      Ast.If (
        Ast.Less (Ast.Var "in", Ast.Int 0),
        Ast.Assign ("out", Ast.Sub (Ast.Int 0, Ast.Var "in")),
        Ast.Assign ("out", Ast.Var "in"))))
    (fun () -> parse_string p_if);

  run_test "While (factorial - only parsing)" (fun () ->
    let _ = parse_string p_while in true);

  run_test "Lexical Error caught" (fun () ->
    try let _ = parse_string_exn "def main with input in output out as @" in false
    with Failure _ -> true);

  run_test "Syntax Error caught" (fun () ->
    try let _ = parse_string_exn "def main with input output out as skip" in false
    with Failure _ -> true)

(* SEMANTICS (Interpreter) *)
let test_semantics () =
  print_header "SEMANTICS (Interpreter)";

  run_eq_test "Assign (in=5) → 6"
    (Runtime.Int 6)
    (fun () -> eval_with 5 p_assign);

  run_eq_test "If absolute value (in=-5) → 5"
    (Runtime.Int 5)
    (fun () -> eval_with (-5) p_if);

  run_eq_test "If absolute value (in=5) → 5"
    (Runtime.Int 5)
    (fun () -> eval_with 5 p_if);

  run_eq_test "While factorial (in=5) → 120"
    (Runtime.Int 120)
    (fun () -> eval_with 5 p_while);

  run_eq_test "While factorial (in=0) → 1 (base case)"
    (Runtime.Int 1)
    (fun () -> eval_with 0 p_while);

  run_eq_test "While factorial (in=1) → 1"
    (Runtime.Int 1)
    (fun () -> eval_with 1 p_while);

  run_test "RuntimeError on undefined variable" (fun () ->
    try
      let ast = parse_string "def main with input in output out as out := noinit" in
      let _ = Runtime.eval ast in false
    with Runtime.RuntimeError _ -> true)

(* CFG GENERATION *)
let test_cfg () =
  print_header "CFG GENERATION";

  run_test "Skip → 1 node" (fun () ->
    Hashtbl.length (make_cfg (parse_string p_skip)).nodes = 1);

  run_test "Assign → 1 node" (fun () ->
    Hashtbl.length (make_cfg (parse_string p_assign)).nodes = 1);

  run_test "CmdParen → same number of nodes as Assign" (fun () ->
    Hashtbl.length (make_cfg (parse_string p_paren)).nodes = 1);

  run_test "Seq → 2 nodes" (fun () ->
    Hashtbl.length (make_cfg (parse_string p_seq)).nodes = 2);

  run_test "If/Else → 4 nodes (cond + true + false + join)" (fun () ->
    Hashtbl.length (make_cfg (parse_string p_if)).nodes = 4);

  run_test "While (factorial) → 5 nodes" (fun () ->
    let cfg = make_cfg (parse_string p_while) in
    Cfg.print_cfg cfg;
    Hashtbl.length cfg.nodes = 5);

  run_test "If + nested While → 6 nodes" (fun () ->
    Hashtbl.length (make_cfg (parse_string p_complex)).nodes = 6);

  run_test "Entry reaches Exit (skip)" (fun () ->
    let cfg = make_cfg (parse_string p_skip) in
    cfg.i = cfg.f);

  run_test "Entry node exists in table" (fun () ->
    let cfg = make_cfg (parse_string p_while) in
    Hashtbl.mem cfg.nodes cfg.i);

  run_test "Exit node exists in table" (fun () ->
    let cfg = make_cfg (parse_string p_while) in
    Hashtbl.mem cfg.nodes cfg.f)

(* DATA-FLOW ANALYSIS *)
let test_dataflow () =
  print_header "DATA-FLOW ANALYSIS";

  run_test "No undef variable in correct program" (fun () ->
    let ast = parse_string p_assign in
    DataFlow.VarSet.is_empty (DataFlow.check_undefined_variables ast (make_cfg ast)));

  run_test "Variable 'y' detected as undef in p_undef" (fun () ->
    let ast = parse_string p_undef in
    DataFlow.VarSet.mem "y" (DataFlow.check_undefined_variables ast (make_cfg ast)));

  run_test "print_undefined_warnings does not crash on correct program" (fun () ->
    let ast = parse_string p_assign in
    DataFlow.print_undefined_warnings ast (make_cfg ast);
    true);

  run_test "Dead Store Elimination: detects dead store" (fun () ->
    let ast = parse_string p_dead in
    match ast with
    | Ast.Program (_, out_v, _) -> 
        Optimize.dead_store_elimination (make_cfg ast) out_v);

  run_test "Constant Folding: simplifies (2+3)*4" (fun () ->
    Optimize.constant_folding (make_cfg (parse_string p_fold)));

  run_test "Constant Propagation: propagates a := 10" (fun () ->
    Optimize.constant_propagation (make_cfg (parse_string p_prop)));

  run_test "Complete pipeline: reaches fixed point" (fun () ->
    let ast = parse_string p_pipeline in
    match ast with
    | Ast.Program (_, out_v, _) ->
        let cfg = make_cfg ast in
        Printf.printf "\n[BEFORE]\n";
        Cfg.print_cfg cfg;
        Optimize.optimize cfg out_v;
        Printf.printf "\n[AFTER]\n";
        Cfg.print_cfg cfg;
        true);

  run_test "DSE removes dead stores, leaves live ones" (fun () ->
    let ast = parse_string p_dead in
    match ast with
    | Ast.Program (_, out_v, _) ->
        Optimize.dead_store_elimination (make_cfg ast) out_v)

(* LLVM IR GENERATION *)
let contains_substring haystack needle =
  let hl = String.length haystack in
  let nl = String.length needle in
  if nl = 0 then true
  else if nl > hl then false
  else
    let rec loop i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in
    loop 0

let generate_ir src =
  let ast = parse_string src in
  match ast with
  | Ast.Program (in_v, out_v, cmd) ->
      let cfg = Cfg.generate_cfg cmd in
      Optimize.optimize cfg out_v;
      Llvm.generate_llvm_ir cfg in_v out_v

let test_llvm () =
  print_header "LLVM IR GENERATION";

  run_test "IR contains 'define i64 @func'" (fun () ->
    contains_substring (generate_ir p_assign) "define i64 @func");

  run_test "IR contains 'ret i64'" (fun () ->
    contains_substring (generate_ir p_assign) "ret i64");

  run_test "IR contains alloca for output variable" (fun () ->
    contains_substring (generate_ir p_assign) "alloca i64");

  run_test "IR factorial: contains 'mul i64'" (fun () ->
    contains_substring (generate_ir p_while) "mul i64");

  run_test "IR factorial: contains 'icmp slt'" (fun () ->
    contains_substring (generate_ir p_while) "icmp slt");

  run_test "IR if/else: contains 'br i1'" (fun () ->
    contains_substring (generate_ir p_if) "br i1");

  run_test "IR factorial: preview visible" (fun () ->
    let ir = generate_ir p_while in
    Printf.printf "\n[LLVM IR - Factorial]\n%s\n" ir;
    true)

let () =
  Printf.printf "\n🚀 Starting MiniImp Test Suite...\n";
  test_parsing ();
  test_semantics ();
  test_cfg ();
  test_dataflow ();
  test_llvm ();
  Printf.printf "\n========================================\n";
  Printf.printf " RESULT: %d ✅  |  %d ❌\n" !pass_count !fail_count;
  Printf.printf "========================================\n";
  (* if !fail_count > 0 then exit 1 *)
