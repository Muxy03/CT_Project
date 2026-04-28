(*EXCEPTIONS*)
exception LlvmError of string 

(* GENERATION OF FRESH NAME *)
type counters = {
  mutable reg   : int;
  mutable label : int;
}

let make_counters () = { reg = 0; label = 0 }
let fresh_reg (c : counters) : string = c.reg <- c.reg + 1; "%tmp." ^ string_of_int c.reg
let _fresh_label (c : counters) : string = c.label <- c.label + 1; "bb_" ^ string_of_int c.label

(* HELPERS *)
let rec compile_expr counters e  = match e with
  | Ast.Int i -> (string_of_int i, []) (* constant are inline *)
  | Ast.Var v -> let r = fresh_reg counters in 
  	(r, [ Printf.sprintf "  %s = load i64, ptr %%%s" r v ])
  | Ast.Add (e1, e2) -> compile_binop counters "add" e1 e2
  | Ast.Sub (e1, e2) -> compile_binop counters "sub" e1 e2
  | Ast.Mul (e1, e2) -> compile_binop counters "mul" e1 e2

and compile_binop counters op e1 e2 =
  let (r1, i1) = compile_expr counters e1 in
  let (r2, i2) = compile_expr counters e2 in
  let r        = fresh_reg counters in
  (r, i1 @ i2 @ [ Printf.sprintf "  %s = %s i64 %s, %s" r op r1 r2 ])

let rec compile_bexpr counters b = match b with
  | Ast.True  -> ("true",  [])
  | Ast.False -> ("false", [])
  | Ast.Less (e1, e2) ->
      let (r1, i1) = compile_expr counters e1 in
      let (r2, i2) = compile_expr counters e2 in
      let r        = fresh_reg counters in
      (r, i1 @ i2 @ [ Printf.sprintf "  %s = icmp slt i64 %s, %s" r r1 r2 ])
  | Ast.Not b ->
      let (rb, ib) = compile_bexpr counters b in
      let r        = fresh_reg counters in
      (r, ib @ [ Printf.sprintf "  %s = xor i1 %s, true" r rb ])
  | Ast.And (b1, b2) ->
      let (r1, i1) = compile_bexpr counters b1 in
      let (r2, i2) = compile_bexpr counters b2 in
      let r        = fresh_reg counters in
      (r, i1 @ i2 @ [ Printf.sprintf "  %s = and i1 %s, %s" r r1 r2 ])

let collect_vars (cfg : Cfg.cfg) =
  Hashtbl.fold (fun _ node acc ->
    match node.Cfg.code with
    | Cfg.Stmt (Ast.Assign (v, _)) -> v :: acc
    | _                            -> acc
  ) cfg.nodes []

let compile_node counters (cfg : Cfg.cfg) outVar id node =
  let label = Printf.sprintf "bb_%d:" id in
  let (last_reg, body_instrs) =
    match node.Cfg.code with
    | Cfg.Stmt (Ast.Assign (v, e)) ->
        let (r, ei) = compile_expr counters e in
        let store   = Printf.sprintf "  store i64 %s, ptr %%%s" r v in
        (r, ei @ [store])
    | Cfg.Stmt Ast.Skip -> ("", [])
    | Cfg.Condition b   -> compile_bexpr counters b
    | Cfg.Stmt _ ->
        raise ( LlvmError (Printf.sprintf
          "Llvm.compile_node: nodo %d contiene un comando complesso (Seq/If/While). \
           Il CFG non è stato appiattito correttamente." id))
  in
  let jump_instrs =
    match node.Cfg.next with
    | Cfg.EOF ->
        let ret_reg = fresh_reg counters in
        [ Printf.sprintf "  %s = load i64, ptr %%%s" ret_reg outVar
        ; Printf.sprintf "  ret i64 %s" ret_reg ]
    | Cfg.NextBlock nxt -> [ Printf.sprintf "  br label %%bb_%d" nxt ]
    | Cfg.CondSelect (n_true, n_false) ->
        [ Printf.sprintf "  br i1 %s, label %%bb_%d, label %%bb_%d" last_reg n_true n_false ]
  in
  label :: body_instrs @ jump_instrs

(* ENTRY POINT *)
let generate_llvm_ir (cfg : Cfg.cfg) inVar outVar =
  let c = make_counters () in
  let cfg_vars  = collect_vars cfg in
  let all_vars  = List.sort_uniq String.compare (inVar :: outVar :: cfg_vars) in
  let header = [
    "define i64 @func(i64 %input_val) {"
  ; "entry:"
  ] in
  let allocas = List.map (fun v -> Printf.sprintf "  %%%s = alloca i64" v) all_vars
  in
  let init_input = [ Printf.sprintf "  store i64 %%input_val, ptr %%%s" inVar ]
  in
  let jump_to_start = [ Printf.sprintf "  br label %%bb_%d" cfg.Cfg.i ]
  in
  let sorted_nodes =
	Hashtbl.fold (fun id node acc -> (id, node) :: acc) cfg.nodes []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let body = List.map (fun (id, node) ->
      String.concat "\n" (compile_node c cfg outVar id node)
    ) sorted_nodes
  in
  let footer = [ "}" ] in
  String.concat "\n" (header @ allocas @ init_input @ jump_to_start @ [""] @ body @ footer)
