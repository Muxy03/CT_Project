(* HELPERS *)
let update_node (cfg : Cfg.cfg) id node block changed msg =
  Hashtbl.replace cfg.nodes id { node with Cfg.code = block };
  changed := true;
  Printf.printf "%s\n" msg

(* DEAD STORE ELIMINATION *)
let dead_store_elimination (cfg : Cfg.cfg) outVar =
  let liveness_map = DataFlow.compute_live_variables cfg in
  let changed = ref false in
  Hashtbl.iter (fun id node ->
    match node.Cfg.code with
    | Cfg.Stmt (Ast.Assign (v, _)) ->
        let state = DataFlow.NodeMap.find id liveness_map in
        if not (DataFlow.VarSet.mem v state.DataFlow.liveOut) && v <> outVar then
          update_node cfg id node (Cfg.Stmt Ast.Skip) changed
            (Printf.sprintf "  [Dead Store Elim]    nodo %d: rimosso '%s := ...'" id v)
    | _ -> ()
  ) cfg.nodes;
  !changed

(* CONSTANT FOLDING *)
let rec fold_expr e = match e with
  | Ast.Int _ as leaf -> leaf
  | Ast.Var _ as leaf -> leaf
  | Ast.Add (e1, e2) -> (match fold_expr e1, fold_expr e2 with
      | Ast.Int a, Ast.Int b -> Ast.Int (a + b)
      | f1, f2               -> Ast.Add (f1, f2))
  | Ast.Sub (e1, e2) -> (match fold_expr e1, fold_expr e2 with
      | Ast.Int a, Ast.Int b -> Ast.Int (a - b)
      | f1, f2               -> Ast.Sub (f1, f2))
  | Ast.Mul (e1, e2) -> (match fold_expr e1, fold_expr e2 with
      | Ast.Int a, Ast.Int b -> Ast.Int (a * b)
      | f1, f2               -> Ast.Mul (f1, f2))

let rec fold_bexpr b = match b with
  | Ast.True  as lit -> lit
  | Ast.False as lit -> lit
  | Ast.Not b -> (match fold_bexpr b with
      | Ast.True  -> Ast.False
      | Ast.False -> Ast.True
      | f         -> Ast.Not f)
  | Ast.And (b1, b2) -> (match fold_bexpr b1, fold_bexpr b2 with
      | Ast.True,  Ast.True  -> Ast.True
      | Ast.False, _
      | _,         Ast.False -> Ast.False
      | f1, f2               -> Ast.And (f1, f2))
  | Ast.Less (e1, e2) -> (match fold_expr e1, fold_expr e2 with
      | Ast.Int a, Ast.Int b -> if a < b then Ast.True else Ast.False
      | f1, f2               -> Ast.Less (f1, f2))

(* ENTRY POINT *)
let constant_folding (cfg : Cfg.cfg) =
  let changed = ref false in
  Hashtbl.iter (fun id node ->
    match node.Cfg.code with
    | Cfg.Stmt (Ast.Assign (v, e)) ->
        let fe = fold_expr e in
        if fe <> e then
          update_node cfg id node (Cfg.Stmt (Ast.Assign (v, fe))) changed
            (Printf.sprintf "  [Constant Folding]   nodo %d: semplificato assegnamento '%s'" id v)
    | Cfg.Condition b ->
        let fb = fold_bexpr b in
        if fb <> b then
          update_node cfg id node (Cfg.Condition fb) changed
            (Printf.sprintf "  [Constant Folding]   nodo %d: semplificata condizione" id)
    | _ -> ()
  ) cfg.nodes;
  !changed

(* CONSTANT PROPAGATION *)
let get_constant_value var inDefs (cfg : Cfg.cfg) =
  let relevant =
    DataFlow.DefSet.filter (fun d_id ->
      match (Cfg.find_node cfg.nodes d_id).Cfg.code with
      | Cfg.Stmt (Ast.Assign (v, _)) -> v = var
      | _                            -> false
    ) inDefs
  in
  if DataFlow.DefSet.is_empty relevant then None
  else
    let values =
      DataFlow.DefSet.fold (fun d_id acc ->
        match (Cfg.find_node cfg.nodes d_id).Cfg.code with
        | Cfg.Stmt (Ast.Assign (_, Ast.Int i)) -> Some i :: acc
        | _                                    -> None   :: acc
      ) relevant []
    in
    match values with
    | (Some first) :: rest when List.for_all (( = ) (Some first)) rest -> Some first
    | _ -> None

let rec propagate_in_expr e inDefs (cfg : Cfg.cfg) = match e with
  | Ast.Var v -> (match get_constant_value v inDefs cfg with
      | Some c -> Ast.Int c
      | None   -> Ast.Var v)
  | Ast.Add (e1, e2) -> Ast.Add (propagate_in_expr e1 inDefs cfg, propagate_in_expr e2 inDefs cfg)
  | Ast.Sub (e1, e2) -> Ast.Sub (propagate_in_expr e1 inDefs cfg, propagate_in_expr e2 inDefs cfg)
  | Ast.Mul (e1, e2) -> Ast.Mul (propagate_in_expr e1 inDefs cfg, propagate_in_expr e2 inDefs cfg)
  | Ast.Int _ as lit -> lit

let rec propagate_in_bexpr b inDefs cfg = match b with
  | Ast.True  as lit -> lit
  | Ast.False as lit -> lit
  | Ast.Less (e1, e2) -> Ast.Less (propagate_in_expr e1 inDefs cfg, propagate_in_expr e2 inDefs cfg)
  | Ast.And  (b1, b2) -> Ast.And  (propagate_in_bexpr b1 inDefs cfg, propagate_in_bexpr b2 inDefs cfg)
  | Ast.Not  b        -> Ast.Not  (propagate_in_bexpr b  inDefs cfg)

(* ENTRY POINT *)
let constant_propagation (cfg : Cfg.cfg) =
  let rd_map  = DataFlow.compute_reaching_definitions cfg in
  let changed = ref false in
  Hashtbl.iter (fun id node ->
    let state = DataFlow.NodeMap.find id rd_map in
    match node.Cfg.code with
    | Cfg.Stmt (Ast.Assign (v, e)) ->
        let ne = propagate_in_expr e state.DataFlow.rdIn cfg in
        if ne <> e then
          update_node cfg id node (Cfg.Stmt (Ast.Assign (v, ne))) changed
            (Printf.sprintf "  [Constant Prop]      nodo %d: propagato valore in '%s'" id v)
    | Cfg.Condition b ->
        let nb = propagate_in_bexpr b state.DataFlow.rdIn cfg in
        if nb <> b then
          update_node cfg id node (Cfg.Condition nb) changed
            (Printf.sprintf "  [Constant Prop]      nodo %d: propagato valore in condizione" id)
    | _ -> ()
  ) cfg.nodes;
  !changed

(* OPTIMIZATION PIPELINE *)
let rec optimize_pipeline (cfg : Cfg.cfg) outVar iter =
  Printf.printf "\n--- Iterazione #%d ---\n" iter;
  let c1 = constant_propagation    cfg in
  let c2 = constant_folding        cfg in
  let c3 = dead_store_elimination  cfg outVar in
  if c1 || c2 || c3 then begin
    Printf.printf "  -> Modifiche rilevate, nuovo giro...\n";
    optimize_pipeline cfg outVar (iter + 1)
  end else
    Printf.printf "  -> Punto fisso raggiunto. ✅\n"

let optimize (cfg : Cfg.cfg) outVar =
  Printf.printf "\n🚀 Avvio Pipeline di Ottimizzazione...\n";
  optimize_pipeline cfg outVar 1
