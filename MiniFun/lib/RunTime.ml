(* EXCEPTIONS *)
exception RunTimeError of string

(* TYPES *)
(* type binop =
  | Add
  | Sub
  | Mul
  | And
  | Lt *)

(* type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Binop of binop * expr * expr
  | Neg of expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | LetFun of string * string * expr * expr *)

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env
  | VRecClosure of string * string * Ast.expr * env

(* Modifica 1: env diventa una Hashtbl invece di una lista *)
and env = (string, value) Hashtbl.t

(* HELPERS *)
let env_get env x =
  try Hashtbl.find env x
  with Not_found -> raise (RunTimeError ("Unbound variable: " ^ x))

let extend_env env x v =
  let new_env = Hashtbl.copy env in
  Hashtbl.add new_env x v;
  new_env

let env_init capacity =
    let init_capacity = 256 in
    match capacity with Some c -> Hashtbl.create c | None -> Hashtbl.create init_capacity

let apply_binop op v1 v2 =
  match (op, v1, v2) with
  | Ast.Add, VInt i1, VInt i2 -> VInt (i1 + i2)
  | Sub, VInt i1, VInt i2 -> VInt (i1 - i2)
  | Mul, VInt i1, VInt i2 -> VInt (i1 * i2)
  | And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Lt, VInt i1, VInt i2 -> VBool (i1 < i2)
  | _ -> raise (RunTimeError "Type error: Invalid operands for binary operator")


(* EVALUATION *)
let rec eval env ast = 
  match ast with
  | Ast.Int i -> VInt i
  | Bool b -> VBool b
  | Var x -> env_get env x
  | Fun (x, t) -> VClosure (x, t, env)
  | App (t1, t2) -> (
      let v1 = eval env t1 in
      let v2 = eval env t2 in
      match v1 with
      | VClosure (x, t, env') -> 
          (* Usiamo extend_env invece dell'operatore :: *)
          eval (extend_env env' x v2) t
      | VRecClosure (f, x, t, env') ->
          let env'' = extend_env env' f v1 in
          let env''' = extend_env env'' x v2 in
          eval env''' t
      | _ -> raise (RunTimeError "Type error: Application of non-function"))
  | Binop (op, t1, t2) ->
      let v1 = eval env t1 in
      let v2 = eval env t2 in
      apply_binop op v1 v2
  | Not t -> (
      match eval env t with
      | VInt i -> VInt (-i)
      | _ -> raise (RunTimeError "Type error: Negation of non-integer"))
  | If (t1, t2, t3) -> (
      match eval env t1 with
      | VBool true -> eval env t2
      | VBool false -> eval env t3
      | _ -> raise (RunTimeError "Type error: If condition must be a boolean"))
  | Let (x, t1, t2) ->
      let v1 = eval env t1 in
      eval (extend_env env x v1) t2
  | LetFun (f, x, t1, t2) ->
      let rec_closure = VRecClosure (f, x, t1, env) in
      eval (extend_env env f rec_closure) t2

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VClosure (x, _, _) -> Printf.sprintf "<closure: %s>" x
  | VRecClosure (f, x, _, _) -> Printf.sprintf "<rec_closure: %s %s>" f x

