(* EXECPTIONS *)
exception RuntimeError of string

(* TYPES *)
type value =
  | Int       of int
  | Bool      of bool
  | Undefined

type memory = (string, value) Hashtbl.t

(* HELPERS *)
let string_of_value v = match v with
  | Int i     -> string_of_int i
  | Bool b    -> string_of_bool b
  | Undefined -> "undefined"

let mem_get mem var = match Hashtbl.find_opt mem var with
  | Some v -> v
  | None   -> raise (RuntimeError ("Not found variable: " ^ var))

let mem_set mem var value =
  Hashtbl.replace mem var value;
  mem

let mem_create () : memory = Hashtbl.create 69

(* EVALUATION *)
let rec eval_expr mem e = match e with
  | Ast.Var v        -> mem_get mem v
  | Ast.Int i        -> Int i
  | Ast.Add (e1, e2) -> (match eval_expr mem e1, eval_expr mem e2 with
      | Int a, Int b -> Int (a + b)
      | _            -> raise (RuntimeError "Type error in '+' operation"))
  | Ast.Sub (e1, e2) -> (match eval_expr mem e1, eval_expr mem e2 with
      | Int a, Int b -> Int (a - b)
      | _            -> raise (RuntimeError "Type error in '-' operation"))
  | Ast.Mul (e1, e2) -> (match eval_expr mem e1, eval_expr mem e2 with
      | Int a, Int b -> Int (a * b)
      | _            -> raise (RuntimeError "Type error in '*' operation"))

let rec eval_bexpr mem b = match b with
  | Ast.True         -> Bool true
  | Ast.False        -> Bool false
  | Ast.And (b1, b2) -> (match eval_bexpr mem b1, eval_bexpr mem b2 with
      | Bool v1, Bool v2 -> Bool (v1 && v2)
      | _                -> raise (RuntimeError "Type error in 'and' operation"))
  | Ast.Not b        -> (match eval_bexpr mem b with
      | Bool v -> Bool (not v)
      | _      -> raise (RuntimeError "Type error in 'not' operation"))
  | Ast.Less (e1, e2) -> (match eval_expr mem e1, eval_expr mem e2 with
      | Int a, Int b -> Bool (a < b)
      | _            -> raise (RuntimeError "Type error in '<' operation"))

let rec eval_cmd mem c = match c with
  | Ast.Assign (v, e) -> 
  	let value = eval_expr mem e in
  	if value = Undefined then 
    	raise (RuntimeError ("Variable '" ^ v ^ "' assigned to undefined value"))
    else
        mem_set mem v value
  | Ast.Seq (c1, c2) -> eval_cmd (eval_cmd mem c1) c2
  | Ast.If (b, c1, c2) -> (match eval_bexpr mem b with
      | Bool true  -> eval_cmd mem c1
      | Bool false -> eval_cmd mem c2
      | _          -> raise (RuntimeError "Type error in 'if' condition"))
  | Ast.While (b, c) as loop -> (match eval_bexpr mem b with
      | Bool true  -> eval_cmd (eval_cmd mem c) loop
      | Bool false -> mem
      | _          -> raise (RuntimeError "Type error in  'while' condition"))
  | Ast.CmdParen c -> eval_cmd mem c
  | Ast.Skip       -> mem

(* ENTRY POINT *)
let eval program = match program with
  | Ast.Program (input_name, output_name, cmd) ->
      let mem  = mem_create () in
      let mem0 =
        let m1 = mem_set mem input_name  Undefined in
        let m2 = mem_set m1  output_name Undefined in m2
      in
      let mem1 = eval_cmd mem0 cmd in
      mem_get mem1 output_name
