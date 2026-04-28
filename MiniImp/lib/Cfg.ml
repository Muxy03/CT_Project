(* EXECPTIONS *)
exception CfgError of string

(* TYPES *)
type blockCode =
  | Stmt of Ast.cmd
  | Condition of Ast.bexpr

type nodeId = int

type nextNode =
  | EOF
  | NextBlock of nodeId
  | CondSelect of nodeId * nodeId

type node = {
    id   : nodeId
  ; code : blockCode
  ; mutable next : nextNode
}

type cfg = {
    nodes : (nodeId, node) Hashtbl.t
  ; i : nodeId  (* Entry node *)
  ; f : nodeId  (* Exit node  *)
}

(* Counter of nodes *)
let counter = ref 0
let reset_counter () = counter := 0
let fresh_nodeId () = incr counter; !counter

(* HELPERS *)
let successors node =
  match node.next with
  | EOF                ->  []
  | NextBlock next     ->  [next]
  | CondSelect (a, b)  ->  [a; b]

let string_of_blockCode block = match block with
  | Stmt (Ast.Assign (x, e))  -> x ^ " := " ^ Ast.string_of_expr e
  | Stmt Ast.Skip             -> "skip"
  | Stmt _                    -> "<complex stmt>"
  | Condition b               -> Ast.string_of_bexpr b ^ " ?"

let string_of_next next = match next with 
  | EOF               -> "EOF"
  | NextBlock n       -> Printf.sprintf "--> %d" n
  | CondSelect (t, f) -> Printf.sprintf "-True-> %d | -False-> %d" t f

let print_cfg cfg =
  Printf.printf "\n=== CONTROL FLOW GRAPH ===\n";
  Printf.printf "Entry Node : %d\n" cfg.i;
  Printf.printf "Exit Node  : %d\n" cfg.f;
  Printf.printf "------------------------------------------------------\n";
  let nodes = 
    Hashtbl.fold (fun _ node acc -> node :: acc) cfg.nodes []
    |> List.sort (fun n1 n2 -> compare n1.id n2.id)
  in
  List.iter (fun node ->
    Printf.printf "Node %2d | %-30s | %s\n"
      node.id
      (string_of_blockCode node.code)
      (string_of_next node.next)
  ) nodes;
  Printf.printf "==========================\n\n"

(* CFG GENERATION *)
let create_node cfg block =
  let id   = fresh_nodeId () in
  let node = { id; code = block; next = EOF } in
  Hashtbl.add cfg id node;
  node

let find_node cfg id =
  match Hashtbl.find_opt cfg id with
  | Some n -> n
  | None   -> raise (CfgError (Printf.sprintf "Cfg.find_node: nodo %d non trovato nel grafo" id))

let rec build_cfg cfg astStmt =
  match astStmt with
  | Ast.Assign _ as cmd ->
      let n = create_node cfg (Stmt cmd) in (n.id, n.id)
  | Ast.Skip ->
      let n = create_node cfg (Stmt Ast.Skip) in (n.id, n.id)
  | Ast.Seq (c1, c2) ->
      let in1, out1 = build_cfg cfg c1 in
      let in2, out2 = build_cfg cfg c2 in
      (find_node cfg out1).next <- NextBlock in2;
      (in1, out2)
  | Ast.If (b, c1, c2) ->
      let condNode = create_node cfg (Condition b) in
      let joinNode = create_node cfg (Stmt Ast.Skip) in
      let in1, out1 = build_cfg cfg c1 in
      let in2, out2 = build_cfg cfg c2 in
      condNode.next               <- CondSelect (in1, in2);
      (find_node cfg out1).next   <- NextBlock joinNode.id;
      (find_node cfg out2).next   <- NextBlock joinNode.id;
      (condNode.id, joinNode.id)

  | Ast.While (b, c) ->
      let condNode = create_node cfg (Condition b) in
      let exitNode = create_node cfg (Stmt Ast.Skip) in
      let inC, outC = build_cfg cfg c in
      condNode.next             <- CondSelect (inC, exitNode.id);
      (find_node cfg outC).next <- NextBlock condNode.id;
      (condNode.id, exitNode.id)

  | Ast.CmdParen c -> build_cfg cfg c

(* ENTRY POINT *)
let generate_cfg ast =
  let cfg = Hashtbl.create 69 in
  let i,f = build_cfg cfg ast in
  { nodes = cfg; i; f }
