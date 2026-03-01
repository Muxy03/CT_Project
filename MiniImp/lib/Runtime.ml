(* EXCEPTIONS *)
exception RuntimeError of string

(* TYPES *)
type value =
  | Int of int
  | Bool of bool
  | Undefined

type var = Var of string
type memory = (string, value) Hashtbl.t

(* HELPERS *)
let string_of_value = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Undefined -> "undefined"


let mem_get mem var =
  try Hashtbl.find mem var
  with Not_found -> raise (RuntimeError ("not exists variable " ^ var ^ " in memory"))


let mem_set mem var value =
  Hashtbl.replace mem var value ;
  mem


let init_capacity = 256

let mem_init capacity =
  match capacity with Some c -> Hashtbl.create c | None -> Hashtbl.create init_capacity


(* EVALUATION *)
let rec eval_e mem = function
  | Ast.Var v -> mem_get mem v
  | Int i -> Int i
  | Add (e1, e2) -> (
      match (eval_e mem e1, eval_e mem e2) with
      | Int i1, Int i2 -> Int (i1 + i2)
      | _ -> raise (RuntimeError "Type error in addition"))
  | Sub (e1, e2) -> (
      match (eval_e mem e1, eval_e mem e2) with
      | Int i1, Int i2 -> Int (i1 - i2)
      | _ -> raise (RuntimeError "Type error in subtraction"))
  | Mul (e1, e2) -> (
      match (eval_e mem e1, eval_e mem e2) with
      | Int i1, Int i2 -> Int (i1 * i2)
      | _ -> raise (RuntimeError "Type error in multiplication"))


let rec eval_b mem = function
  | Ast.True -> Bool true
  | False -> Bool false
  | And (b1, b2) -> (
      match (eval_b mem b1, eval_b mem b2) with
      | Bool v1, Bool v2 -> Bool (v1 && v2)
      | _ -> raise (RuntimeError "Type error in boolean AND"))
  | Not b -> (
      match eval_b mem b with
      | Bool v -> Bool (not v)
      | _ -> raise (RuntimeError "Type error in boolean NOT"))
  | Less (e1, e2) -> (
      match (eval_e mem e1, eval_e mem e2) with
      | Int i1, Int i2 -> Bool (i1 < i2)
      | _ -> raise (RuntimeError "Type error in less-than comparison"))


let rec eval_cmd mem = function
  | Ast.Assign (v, e) ->
      let value = eval_e mem e in
      if value = Undefined then
        raise (RuntimeError ("Variable " ^ v ^ " is assigned an undefined value"))
      else mem_set mem v value
  | Seq (c1, c2) ->
      let mem1 = eval_cmd mem c1 in
      let mem2 = eval_cmd mem1 c2 in
      mem2
  | If (b, c1, c2) -> (
      match eval_b mem b with
      | Bool true -> eval_cmd mem c1
      | Bool false -> eval_cmd mem c2
      | _ -> raise (RuntimeError "Type error in if condition"))
  | While (b, c) as loop -> (
      match eval_b mem b with
      | Bool true ->
          let mem1 = eval_cmd mem c in
          eval_cmd mem1 loop
      | Bool false -> mem
      | _ -> raise (RuntimeError "Type error in while condition"))
  | CmdParen c -> eval_cmd mem c


let eval prog =
  match prog with
  | Ast.Prog (InputVar (input, input_val), output, cmd) ->
      let memory = mem_init None in
      let mem0 =
        match input_val with
        | Some v -> mem_set (mem_set memory input (Int v)) output Undefined
        | None -> mem_set (mem_set memory input Undefined) output Undefined
      in
      let mem1 = eval_cmd mem0 (CmdParen cmd) in
      mem_get mem1 output
