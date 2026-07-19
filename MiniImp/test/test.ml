(* MiniImp test suite.

   Exercises the hand-written Manual.Lexer / Manual.Parser together with the
   rest of the pipeline: CFG construction, data-flow analysis, program
   optimizations, the interpreter and LLVM IR generation. *)

open MiniImp

(* -------------------------------------------------------------------- *)
(* Minimal test harness                                                 *)
(* -------------------------------------------------------------------- *)

type outcome =
  | Passed
  | Failed of string (* failure reason *)

let results : (string * outcome) list ref = ref []

let print_section title =
  Printf.printf "\n==================== %s ====================\n" title

(* Runs [check] and records the outcome under [name]. [check] is expected to
   return [true] on success; any exception it raises is caught and reported
   as a failure instead of aborting the whole suite. *)
let test name check =
  let outcome =
    try if check () then Passed else Failed "assertion returned false"
    with
    | Failure msg -> Failed (Printf.sprintf "Failure(%s)" msg)
    | exn -> Failed (Printexc.to_string exn)
  in
  (match outcome with
   | Passed -> Printf.printf "  [PASS] %s\n" name
   | Failed reason -> Printf.printf "  [FAIL] %s -- %s\n" name reason);
  results := (name, outcome) :: !results

(* Convenience wrapper for equality checks: runs [f ()] and compares the
   result to [expected] with structural equality. *)
let test_eq name expected f = test name (fun () -> f () = expected)

(* -------------------------------------------------------------------- *)
(* Parsing / evaluation helpers built on the hand-written parser         *)
(* -------------------------------------------------------------------- *)

let parse (src : string) : Ast.program = Parser.parse src

let cfg_of_source src =
  Cfg.reset_counter ();
  match parse src with
  | Ast.Program (_, _, cmd) -> Cfg.generate_cfg cmd

let node_count src = Hashtbl.length (cfg_of_source src).nodes

(* Runs the interpreter on [src] with [input] bound to the program's input
   variable, and returns the final value of its output variable. *)
let interpret ~input src =
  match parse src with
  | Ast.Program (input_name, output_name, cmd) ->
      let mem = Runtime.mem_create () in
      let mem = Runtime.mem_set mem input_name (Runtime.Int input) in
      let mem = Runtime.mem_set mem output_name Runtime.Undefined in
      let mem = Runtime.eval_cmd mem cmd in
      Runtime.mem_get mem output_name

let contains ~substring haystack =
  let hl = String.length haystack and nl = String.length substring in
  if nl = 0 then true
  else if nl > hl then false
  else
    let rec loop i = i <= hl - nl && (String.sub haystack i nl = substring || loop (i + 1)) in
    loop 0

let generate_ir src =
  match parse src with
  | Ast.Program (in_v, out_v, cmd) ->
      let cfg = Cfg.generate_cfg cmd in
      Optimize.optimize cfg out_v;
      Llvm.generate_llvm_ir cfg in_v out_v

(* -------------------------------------------------------------------- *)
(* Sample programs                                                      *)
(* -------------------------------------------------------------------- *)

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

(* -------------------------------------------------------------------- *)
(* Lexer                                                                 *)
(* -------------------------------------------------------------------- *)

let test_lexer () =
  print_section "LEXER (Manual.Lexer)";
  let open Lexer in

  test_eq "Tokenizes an assignment"
    [ VAR "out"; ASSIGN; VAR "in"; PLUS; INT 1; EOF ]
    (fun () -> tokenize "out := in + 1");

  test_eq "Recognizes all program-header keywords"
    [ DEF; MAIN; WITH; INPUT; VAR "in"; OUTPUT; VAR "out"; AS; SKIP; EOF ]
    (fun () -> tokenize "def main with input in output out as skip");

  test "Identifiers cannot contain underscores" (fun () ->
    try ignore (tokenize "my_var"); false
    with SyntaxError _ -> true);

  test "Unknown characters raise SyntaxError" (fun () ->
    try ignore (tokenize "x := 1 @ 2"); false
    with SyntaxError _ -> true)

(* -------------------------------------------------------------------- *)
(* Parsing                                                               *)
(* -------------------------------------------------------------------- *)

let test_parsing () =
  print_section "PARSING (Manual.Parser)";

  test_eq "Skip"
    (Ast.Program ("in", "out", Ast.Skip))
    (fun () -> parse p_skip);

  test_eq "Assign (out := in + 1)"
    (Ast.Program ("in", "out",
       Ast.Assign ("out", Ast.Add (Ast.Var "in", Ast.Int 1))))
    (fun () -> parse p_assign);

  test_eq "Sequence"
    (Ast.Program ("x", "y",
       Ast.Seq (
         Ast.Assign ("x", Ast.Add (Ast.Var "x", Ast.Int 1)),
         Ast.Assign ("y", Ast.Var "x"))))
    (fun () -> parse p_seq);

  test_eq "If/Else (absolute value)"
    (Ast.Program ("in", "out",
       Ast.If (
         Ast.Less (Ast.Var "in", Ast.Int 0),
         Ast.Assign ("out", Ast.Sub (Ast.Int 0, Ast.Var "in")),
         Ast.Assign ("out", Ast.Var "in"))))
    (fun () -> parse p_if);

  test_eq "Parenthesized command wraps the inner command in CmdParen"
    (Ast.Program ("in", "out",
       Ast.CmdParen (Ast.Assign ("out", Ast.Add (Ast.Var "in", Ast.Int 1)))))
    (fun () -> parse p_paren);

  test "While (factorial) parses without error" (fun () ->
    ignore (parse p_while); true);

  test "A while body absorbs a trailing ';...' chain (SEMI > DO precedence)"
    (fun () ->
      match parse "def main with input n output r as \
                    while n < 1 do ( a := 1 ; b := 2 )" with
      | Ast.Program (_, _, Ast.While (_, Ast.CmdParen (Ast.Seq _))) -> true
      | _ -> false);

  test "Lexical errors surface as Manual.Lexer.SyntaxError" (fun () ->
    try ignore (parse "def main with input in output out as @"); false
    with Lexer.SyntaxError _ -> true);

  test "Syntax errors surface as Manual.Parser.ParseError" (fun () ->
    try ignore (parse "def main with input output out as skip"); false
    with Parser.ParseError _ -> true)

(* -------------------------------------------------------------------- *)
(* Interpreter                                                          *)
(* -------------------------------------------------------------------- *)

let test_semantics () =
  print_section "SEMANTICS (Interpreter)";

  test_eq "Assign (in=5) -> 6" (Runtime.Int 6) (fun () -> interpret ~input:5 p_assign);
  test_eq "Absolute value (in=-5) -> 5" (Runtime.Int 5) (fun () -> interpret ~input:(-5) p_if);
  test_eq "Absolute value (in=5) -> 5" (Runtime.Int 5) (fun () -> interpret ~input:5 p_if);
  test_eq "Factorial (in=5) -> 120" (Runtime.Int 120) (fun () -> interpret ~input:5 p_while);
  test_eq "Factorial (in=0) -> 1 (base case)" (Runtime.Int 1) (fun () -> interpret ~input:0 p_while);
  test_eq "Factorial (in=1) -> 1" (Runtime.Int 1) (fun () -> interpret ~input:1 p_while);

  test "Reading an undefined variable raises Runtime.RuntimeError" (fun () ->
    try
      let ast = parse "def main with input in output out as out := noinit" in
      ignore (Runtime.eval ast); false
    with Runtime.RuntimeError _ -> true)

(* -------------------------------------------------------------------- *)
(* CFG construction                                                     *)
(* -------------------------------------------------------------------- *)

let test_cfg () =
  print_section "CFG GENERATION";

  test_eq "Skip -> 1 node" 1 (fun () -> node_count p_skip);
  test_eq "Assign -> 1 node" 1 (fun () -> node_count p_assign);
  test_eq "CmdParen -> same node count as Assign" 1 (fun () -> node_count p_paren);
  test_eq "Seq -> 2 nodes" 2 (fun () -> node_count p_seq);
  test_eq "If/Else -> 4 nodes (cond + true + false + join)" 4 (fun () -> node_count p_if);

  test "While (factorial) -> 5 nodes" (fun () ->
    let cfg = cfg_of_source p_while in
    Cfg.print_cfg cfg;
    Hashtbl.length cfg.nodes = 5);

  test_eq "If + nested While -> 6 nodes" 6 (fun () -> node_count p_complex);

  test "Entry and exit coincide for a single skip" (fun () ->
    let cfg = cfg_of_source p_skip in
    cfg.i = cfg.f);

  test "Entry node is present in the node table" (fun () ->
    let cfg = cfg_of_source p_while in
    Hashtbl.mem cfg.nodes cfg.i);

  test "Exit node is present in the node table" (fun () ->
    let cfg = cfg_of_source p_while in
    Hashtbl.mem cfg.nodes cfg.f)

(* -------------------------------------------------------------------- *)
(* Data-flow analysis & optimizations                                   *)
(* -------------------------------------------------------------------- *)

let test_dataflow () =
  print_section "DATA-FLOW ANALYSIS & OPTIMIZATIONS";

  test "No undefined variables in a well-formed program" (fun () ->
    let ast = parse p_assign in
    DataFlow.VarSet.is_empty (DataFlow.check_undefined_variables ast (cfg_of_source p_assign)));

  test "Variable 'y' is detected as undefined" (fun () ->
    let ast = parse p_undef in
    DataFlow.VarSet.mem "y" (DataFlow.check_undefined_variables ast (cfg_of_source p_undef)));

  test "print_undefined_warnings does not crash on a correct program" (fun () ->
    let ast = parse p_assign in
    DataFlow.print_undefined_warnings ast (cfg_of_source p_assign);
    true);

  test "Dead store elimination removes an unused assignment" (fun () ->
    match parse p_dead with
    | Ast.Program (_, out_v, _) -> Optimize.dead_store_elimination (cfg_of_source p_dead) out_v);

  test "Constant folding simplifies (2 + 3) * 4" (fun () ->
    Optimize.constant_folding (cfg_of_source p_fold));

  test "Constant propagation propagates a := 10" (fun () ->
    Optimize.constant_propagation (cfg_of_source p_prop));

  test "The full optimization pipeline reaches a fixed point" (fun () ->
    match parse p_pipeline with
    | Ast.Program (_, out_v, _) ->
        let cfg = cfg_of_source p_pipeline in
        Printf.printf "\n[BEFORE]\n";
        Cfg.print_cfg cfg;
        Optimize.optimize cfg out_v;
        Printf.printf "\n[AFTER]\n";
        Cfg.print_cfg cfg;
        true)

(* -------------------------------------------------------------------- *)
(* LLVM IR generation                                                   *)
(* -------------------------------------------------------------------- *)

let test_llvm () =
  print_section "LLVM IR GENERATION";

  test "IR contains 'define i64 @func'" (fun () ->
    contains ~substring:"define i64 @func" (generate_ir p_assign));

  test "IR contains 'ret i64'" (fun () ->
    contains ~substring:"ret i64" (generate_ir p_assign));

  test "IR contains an alloca for the output variable" (fun () ->
    contains ~substring:"alloca i64" (generate_ir p_assign));

  test "IR (factorial) contains 'mul i64'" (fun () ->
    contains ~substring:"mul i64" (generate_ir p_while));

  test "IR (factorial) contains 'icmp slt'" (fun () ->
    contains ~substring:"icmp slt" (generate_ir p_while));

  test "IR (if/else) contains 'br i1'" (fun () ->
    contains ~substring:"br i1" (generate_ir p_if));

  test "IR (factorial) is generated and previewed" (fun () ->
    let ir = generate_ir p_while in
    Printf.printf "\n[LLVM IR - Factorial]\n%s\n" ir;
    true)

(* -------------------------------------------------------------------- *)
(* Entry point                                                          *)
(* -------------------------------------------------------------------- *)

let () =
  Printf.printf "\nStarting MiniImp test suite...\n";
  test_lexer ();
  test_parsing ();
  test_semantics ();
  test_cfg ();
  test_dataflow ();
  test_llvm ();

  let all = List.rev !results in
  let failed = List.filter (fun (_, outcome) -> outcome <> Passed) all in
  let total = List.length all and failed_count = List.length failed in

  print_section "SUMMARY";
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
