open MiniImp

(* ========================================================================= *)
(* UTILITIES DI TESTING                                                      *)
(* ========================================================================= *)

let parse_string code =
  let lexbuf = Lexing.from_string code in
  MiniImp.Parser.program MiniImp.Lexer.read lexbuf

let print_header fragment_name =
  Printf.printf "\n========================================\n";
  Printf.printf " TESTING %s\n" fragment_name;
  Printf.printf "========================================\n"

let run_test name expected actual =
  Printf.printf "Test [%s] ... " name;
  try
    let res = actual () in
    if res = expected then 
      Printf.printf "✅ OK\n"
    else 
      Printf.printf "❌ FAILED\n"
  with 
  | Failure msg -> Printf.printf "❌ FAILED (Failure: %s)\n" msg
  | e -> Printf.printf "❌ FAILED (Exception: %s)\n" (Printexc.to_string e)

let run_test_no_eq name test_fn =
  Printf.printf "Test [%s] ... " name;
  try
    if test_fn () then Printf.printf "✅ OK\n"
    else Printf.printf "❌ FAILED\n"
  with 
  | Failure msg -> Printf.printf "❌ FAILED (Failure: %s)\n" msg
  | e -> Printf.printf "❌ FAILED (Exception: %s)\n" (Printexc.to_string e)

(* ========================================================================= *)
(* PROGRAMMI DI TEST (MiniImp)                                               *)
(* ========================================================================= *)

let p_skip = "def main with input in output out as skip"
let p_assign = "def main with input in output out as out := in + 1"
let p_seq = "def main with input x output y as \n x := x + 1 ; \n y := x"
let p_if = "def main with input in output out as \n if in < 0 then \n out := 0 - in \n else \n out := in"
let p_while = "def main with input n output res as \n res := 1 ; \n while 0 < n do ( \n res := res * n ; \n n := n - 1 \n )"
let p_paren = "def main with input in output out as \n (out := in + 1)"
let p_complex = "def main with input n output res as \n \
                 if n < 0 then \n \
                   res := 0 \n \
                 else \n \
                   while 0 < n do \n \
                     n := n - 1"

(* ========================================================================= *)
(* FRAGMENT: PARSING (Lexer & Parser)                                        *)
(* ========================================================================= *)
let test_parsing () =
  print_header "MINIIMP PARSING";

  run_test "Parse Skip" 
    (Ast.Prog(InputVar("in", None), "out", Skip)) 
    (fun () -> parse_string p_skip);

  run_test "Parse Assign" 
    (Ast.Prog(InputVar("in", None), "out", Assign("out", Add(Var "in", Int 1)))) 
    (fun () -> parse_string p_assign);

  run_test "Parse Sequence" 
    (Ast.Prog(InputVar("x", None), "y", Seq(
       Assign("x", Add(Var "x", Int 1)),
       Assign("y", Var "x")
     ))) 
    (fun () -> parse_string p_seq);

  run_test "Parse If/Else (Absolute Value)" 
    (Ast.Prog(InputVar("in", None), "out", If(
       Less(Var "in", Int 0),
       Assign("out", Sub(Int 0, Var "in")),
       Assign("out", Var "in")
     ))) 
    (fun () -> parse_string p_if);
    
  run_test_no_eq "Parse While (Factorial)"
    (fun () -> 
       let _ = parse_string p_while in 
       true)

(* ========================================================================= *)
(* FRAGMENT: SEMANTICS / EVALUATION (Interpreter)                            *)
(* ========================================================================= *)
let test_semantics () =
  print_header "MINIIMP SEMANTICS (INTERPRETER)";

  (* Helper per iniettare l'input nell'AST parsato *)
  let eval_with_input input_val code =
    let ast = parse_string code in
    let ast_with_input = match ast with
      | Ast.Prog (Ast.InputVar (in_name, _), out_name, cmd) ->
          (* Sostituiamo il None con Some input_val *)
          Ast.Prog (Ast.InputVar (in_name, Some input_val), out_name, cmd)
    in
    Runtime.eval ast_with_input
  in

  run_test "Eval Assign (in=5)" 
    (Runtime.Int 6)  (* Usa Runtime.Int invece di Ast.Int o solo 6 *)
    (fun () -> eval_with_input 5 p_assign);

  run_test "Eval If (in=-5) -> Absolute Value" 
    (Runtime.Int 5) 
    (fun () -> eval_with_input (-5) p_if);

  run_test "Eval While Fact (in=5) -> Factorial" 
    (Runtime.Int 120) 
    (fun () -> eval_with_input 5 p_while)

(* ========================================================================= *)
(* FRAGMENT: CONTROL-FLOW GRAPH (CFG)                                        *)
(* ========================================================================= *)
let test_cfg () =
  print_header "MINIIMP CFG GENERATION";

  (* 1. Test Skip (1 nodo minimo) *)
  run_test_no_eq "Generate CFG for Skip" (fun () ->
    let ast = parse_string p_skip in
    match ast with
    | Ast.Prog (_, _, cmd) -> 
        let cfg = Cfg.generate_cfg cmd in
        Hashtbl.length cfg.nodes = 1
  );

  (* 2. Test Assign (1 nodo minimo) *)
  run_test_no_eq "Generate CFG for Assign" (fun () ->
    let ast = parse_string p_assign in
    match ast with
    | Ast.Prog (_, _, cmd) -> 
        let cfg = Cfg.generate_cfg cmd in
        Hashtbl.length cfg.nodes = 1
  );

  (* 3. Test CmdParen (Le parentesi non devono aggiungere nodi al CFG) *)
  run_test_no_eq "Generate CFG for Parentheses" (fun () ->
    let ast = parse_string p_paren in
    match ast with
    | Ast.Prog (_, _, cmd) -> 
        let cfg = Cfg.generate_cfg cmd in
        Hashtbl.length cfg.nodes = 1 (* Deve essere 1, come l'Assign normale *)
  );

  (* 4. Test Sequence (Somma dei nodi dei due comandi) *)
  run_test_no_eq "Generate CFG for Sequence" (fun () ->
    let ast = parse_string p_seq in
    match ast with
    | Ast.Prog (_, _, cmd) -> 
        let cfg = Cfg.generate_cfg cmd in
        (* x := x + 1 (1 nodo) ; y := x (1 nodo) -> Totale: 2 nodi *)
        Hashtbl.length cfg.nodes = 2 
  );

  (* 5. Test If/Else *)
  run_test_no_eq "Generate CFG for If/Else" (fun () ->
    let ast = parse_string p_if in
    match ast with
    | Ast.Prog (_, _, cmd) -> 
        let cfg = Cfg.generate_cfg cmd in
        (* Condition (1) + True branch (1) + False branch (1) + Join (1) -> Totale: 4 nodi *)
        Hashtbl.length cfg.nodes = 4 
  );

  (* 6. Test While *)
  run_test_no_eq "Generate CFG for Factorial (Seq + While)" (fun () ->
    let ast = parse_string p_while in
    match ast with
    | Ast.Prog (_, _, cmd) -> 
        let cfg = Cfg.generate_cfg cmd in
        Cfg.print_cfg cfg;
        (* res:=1 (1) + [While: Cond(1) + Body(Seq: 2) + Exit(1)] -> Totale: 5 nodi *)
        Hashtbl.length cfg.nodes = 5 
  );

  (* 7. Test Complesso: Nested While inside If *)
  run_test_no_eq "Generate CFG for Complex (If + While)" (fun () ->
    let ast = parse_string p_complex in
    match ast with
    | Ast.Prog (_, _, cmd) -> 
        let cfg = Cfg.generate_cfg cmd in
        (* If Cond (1)
           |- True Branch (Assign: 1)
           |- False Branch (While: Cond(1) + Body(1) + Exit(1) = 3)
           |- Join (1)
           Totale atteso: 1 + 1 + 3 + 1 = 6 nodi
        *)
        Hashtbl.length cfg.nodes = 6
  )

(* ========================================================================= *)
(* MAIN EXECUTOR                                                             *)
(* ========================================================================= *)
let () =
  Printf.printf "\n🚀 Avvio della Test Suite per MiniImp...\n";
  test_parsing ();
  test_semantics ();
  test_cfg ();
  Printf.printf "\n🎉 Tutti i test eseguiti!\n"