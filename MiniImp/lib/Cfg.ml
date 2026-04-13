type stmt =
  | Skip
  | Assign of string * Ast.e

type block_code =
  | Stmt of Ast.cmd
  | Condition of Ast.b

type nodeId = int

type next_node =
  | EOF
  | NextBlock of nodeId
  | CondSelect of nodeId * nodeId

type node = {
    id : nodeId
  ; code : block_code
  ; mutable next : next_node
}

type cfg = {
    nodes : (nodeId, node) Hashtbl.t
  ; i : nodeId (* Entry node *)
  ; f : nodeId (* Exit node *)
}

let counter = ref 0

let fresh_node () =
  incr counter ;
  !counter


let string_of_block_code = function
  | Stmt (Ast.Assign (x, e)) -> x ^ " := " ^ Ast.string_of_e e
  | Stmt Ast.Skip -> "skip"
  | Stmt _ -> "<complex stmt>" (* Non dovrebbe mai verificarsi nel CFG minimo *)
  | Condition b -> Ast.string_of_b b ^ " ?"

let print_cfg cfg =
  Printf.printf "\n=== CONTROL FLOW GRAPH ===\n";
  Printf.printf "Entry Node : %d\n" cfg.i;
  Printf.printf "Exit Node  : %d\n" cfg.f;
  Printf.printf "------------------------------------------------------\n";
  
  (* Estraiamo i nodi dalla Hashtbl e li ordiniamo per ID *)
  let nodes = Hashtbl.fold (fun _ node acc -> node :: acc) cfg.nodes [] in
  let sorted_nodes = List.sort (fun n1 n2 -> compare n1.id n2.id) nodes in
  
  List.iter (fun node ->
    let code_str = string_of_block_code node.code in
    let next_str = match node.next with
      | EOF -> "EOF"
      | NextBlock n -> Printf.sprintf "--> %d" n
      | CondSelect (t, f) -> Printf.sprintf "-True-> %d | -False-> %d" t f
    in
    Printf.printf "Node %2d | %-20s | %s\n" node.id code_str next_str
  ) sorted_nodes;
  Printf.printf "==========================\n\n"

(* Helper to create a node and add it to a hash table *)
let create_node graph code =
  let id = fresh_node () in
  let node = { id; code; next = EOF } in
  Hashtbl.add graph id node ;
  node


let rec build_graph graph ast_stmt =
  match ast_stmt with
  | (Ast.Assign (_, _) | Ast.Skip) as cmd ->
      let n = create_node graph (Stmt cmd) in
      (n.id, n.id)
  | Ast.Seq (c1, c2) ->
      let in1, out1 = build_graph graph c1 in
      let in2, out2 = build_graph graph c2 in
      let out1_node = Hashtbl.find graph out1 in
      out1_node.next <- NextBlock in2 ;
      (in1, out2)
  | Ast.If (b, c1, c2) ->
      let cond_node = create_node graph (Condition b) in
      let join_node = create_node graph (Stmt Ast.Skip) in

      let in1, out1 = build_graph graph c1 in
      let in2, out2 = build_graph graph c2 in

      cond_node.next <- CondSelect (in1, in2) ;

      (Hashtbl.find graph out1).next <- NextBlock join_node.id ;
      (Hashtbl.find graph out2).next <- NextBlock join_node.id ;

      (cond_node.id, join_node.id)
  | Ast.While (b, c) ->
      let cond_node = create_node graph (Condition b) in
      let exit_node = create_node graph (Stmt Ast.Skip) in

      let in_c, out_c = build_graph graph c in

      cond_node.next <- CondSelect (in_c, exit_node.id) ;
      (Hashtbl.find graph out_c).next <- NextBlock cond_node.id ;

      (cond_node.id, exit_node.id)
  | Ast.CmdParen c -> build_graph graph c


(* Main function to generate the CFG from a program *)
let generate_cfg ast =
  let graph = Hashtbl.create 100 in
  let i, f = build_graph graph ast in
  { nodes = graph; i; f }
