(* MODULES *)
module VarSet = Set.Make(String)
module NodeMap = Map.Make(Int)
module DefSet  = Set.Make(Int)

(* TYPES *)
type liveness_state = {
  liveIn  : VarSet.t;
  liveOut : VarSet.t;
}

type rd_state = {
  rdIn  : DefSet.t;
  rdOut : DefSet.t;
}

(* HELPERS *)
let rec vars_of_expr e = match e with
  | Ast.Var v        -> VarSet.singleton v
  | Ast.Int _        -> VarSet.empty
  | Ast.Add (e1, e2) -> VarSet.union (vars_of_expr e1) (vars_of_expr e2)
  | Ast.Sub (e1, e2) -> VarSet.union (vars_of_expr e1) (vars_of_expr e2)
  | Ast.Mul (e1, e2) -> VarSet.union (vars_of_expr e1) (vars_of_expr e2)

let rec vars_of_bexpr b = match b with
  | Ast.True | Ast.False -> VarSet.empty
  | Ast.Not b            -> vars_of_bexpr b
  | Ast.And (b1, b2)     -> VarSet.union (vars_of_bexpr b1) (vars_of_bexpr b2)
  | Ast.Less (e1, e2)    -> VarSet.union (vars_of_expr e1)  (vars_of_expr e2)

let def_of_node block = match block with
  | Cfg.Stmt (Ast.Assign (v, _)) -> VarSet.singleton v
  | _                            -> VarSet.empty

let use_of_node block = match block with
  | Cfg.Stmt (Ast.Assign (_, e)) -> vars_of_expr e
  | Cfg.Condition b              -> vars_of_bexpr b
  | _                            -> VarSet.empty

(* PREDECESSORS MAP *)
let compute_preds (cfg : Cfg.cfg) =
  let init =
    Hashtbl.fold (fun id _ acc -> NodeMap.add id [] acc) cfg.nodes NodeMap.empty
  in
  Hashtbl.fold (fun id node acc ->
    let add_pred target map =
      let preds = NodeMap.find target map in
      NodeMap.add target (id :: preds) map
    in
    match node.Cfg.next with
    | Cfg.EOF                     -> acc
    | Cfg.NextBlock nxt           -> add_pred nxt acc
    | Cfg.CondSelect (nxt1, nxt2) -> acc |> add_pred nxt1 |> add_pred nxt2
  ) cfg.nodes init

(* LIVENESS ANALYSIS (backward dataflow) *)
let init_liveness_map (cfg : Cfg.cfg) =
  let empty = { liveIn = VarSet.empty; liveOut = VarSet.empty } in
  Hashtbl.fold (fun id _ acc -> NodeMap.add id empty acc) cfg.nodes NodeMap.empty

let compute_live_variables (cfg : Cfg.cfg) =
  let preds_map   = compute_preds cfg in
  let initial_map = init_liveness_map cfg in
  let initial_wl  = Hashtbl.fold (fun id _ acc -> id :: acc) cfg.nodes []
  in
  let rec loop wl map = match wl with
    | [] -> map
    | n :: rest ->
        let node    = Cfg.find_node cfg.nodes n in
        let state_n = NodeMap.find n map in

        let new_liveOut =
          List.fold_left (fun acc succ_id ->
            VarSet.union acc (NodeMap.find succ_id map).liveIn
          ) VarSet.empty (Cfg.successors node)
        in
        let new_liveIn =
          VarSet.union
            (use_of_node node.Cfg.code)
            (VarSet.diff new_liveOut (def_of_node node.Cfg.code))
        in

        if VarSet.equal state_n.liveIn new_liveIn
        && VarSet.equal state_n.liveOut new_liveOut
        then loop rest map
        else
          let new_map = NodeMap.add n { liveIn = new_liveIn; liveOut = new_liveOut } map in
          let preds   = NodeMap.find n preds_map in
          let to_add  = List.filter (fun p -> not (List.mem p rest)) preds in
          loop (to_add @ rest) new_map
  in
  loop initial_wl initial_map

(* REACHING DEFINITIONS (forward dataflow) *)
let find_all_defs_of_var (cfg : Cfg.cfg) (var_name : string) : DefSet.t =
  Hashtbl.fold (fun id node acc ->
    match node.Cfg.code with
    | Cfg.Stmt (Ast.Assign (v, _)) when v = var_name -> DefSet.add id acc
    | _                                              -> acc
  ) cfg.nodes DefSet.empty

let compute_reaching_definitions (cfg : Cfg.cfg) : rd_state NodeMap.t =
  let preds_map   = compute_preds cfg in
  let empty_rd    = { rdIn = DefSet.empty; rdOut = DefSet.empty } in
  let initial_map = Hashtbl.fold (fun id _ acc -> NodeMap.add id empty_rd acc) cfg.nodes NodeMap.empty in
  let initial_wl  = Hashtbl.fold (fun id _ acc -> id :: acc) cfg.nodes []
  in
  let rec loop wl map = match wl with
    | [] -> map
    | n :: rest ->
        let node    = Cfg.find_node cfg.nodes n in
        let state_n = NodeMap.find n map in

        (* IN[n] = union of OUT[pred] *)
        let preds  = NodeMap.find n preds_map in
        let new_in =
          List.fold_left (fun acc p_id ->
            DefSet.union acc (NodeMap.find p_id map).rdOut
          ) DefSet.empty preds
        in

        (* Gen / Kill *)
        let gen, kill =
          match node.Cfg.code with
          | Cfg.Stmt (Ast.Assign (v, _)) ->
              (DefSet.singleton n, find_all_defs_of_var cfg v)
          | _ ->
              (DefSet.empty, DefSet.empty)
        in

        (* OUT[n] = gen ∪ (IN[n] \ kill) *)
        let new_out = DefSet.union gen (DefSet.diff new_in kill) in

        if DefSet.equal state_n.rdIn new_in && DefSet.equal state_n.rdOut new_out
        then loop rest map
        else
          let new_map = NodeMap.add n { rdIn = new_in; rdOut = new_out } map in
          let succs   = Cfg.successors node in
          let to_add  = List.filter (fun s -> not (List.mem s rest)) succs in
          loop (to_add @ rest) new_map
  in
  loop initial_wl initial_map

(* UNDEFINED VARS ANALYSIS *)
let check_undefined_variables program (cfg : Cfg.cfg) =
  let liveness_map = compute_live_variables cfg in
  let entry_live   = (NodeMap.find cfg.i liveness_map).liveIn in
  let input_var    = match program with
    | Ast.Program (name, _, _) -> name
  in
  VarSet.remove input_var entry_live

let print_undefined_warnings program cfg =
  let undef_vars = check_undefined_variables program cfg in
  if VarSet.is_empty undef_vars then
    Printf.printf "DataFlow Analysis: No undefined variables detected.\n"
  else
    VarSet.iter (fun v ->
      Printf.printf "WARNING: Variable '%s' may be used uninitialized!\n" v
    ) undef_vars
