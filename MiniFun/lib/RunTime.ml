(* EXCEPTIONS *)
exception RunTimeError of string

(* TYPES *)
type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env
  | VRecClosure of string * string * Ast.expr * env

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
    match capacity with 
    | Some c -> Hashtbl.create c 
    | None -> Hashtbl.create init_capacity

let apply_binop op v1 v2 =
  match (op, v1, v2) with
  | Ast.Add, VInt i1, VInt i2 -> VInt (i1 + i2)
  | Ast.Sub, VInt i1, VInt i2 -> VInt (i1 - i2)
  | Ast.Mul, VInt i1, VInt i2 -> VInt (i1 * i2)
  | Ast.And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Ast.Lt, VInt i1, VInt i2 -> VBool (i1 < i2)
  | _ -> raise (RunTimeError "Type error: Invalid operands for binary operator")

(* EVALUATION *)
let rec eval env ast = 
  match ast with
  | Ast.Num i -> VInt i
  | Ast.Boolean b -> VBool b
  | Ast.Var x -> env_get env x
  | Ast.Func (x, _typ_opt, body) -> VClosure (x, body, env)
  | Ast.App (t1, t2) -> (
      let v1 = eval env t1 in
      let v2 = eval env t2 in
      match v1 with
      | VClosure (x, body, env1) -> 
          eval (extend_env env1 x v2) body
      | VRecClosure (f, x, body, env1) ->
          let env2 = extend_env env1 f v1 in
          let env3 = extend_env env2 x v2 in
          eval env3 body
      | _ -> raise (RunTimeError "Type error: Application of non-function"))
  | Ast.Binop (op, t1, t2) ->
      let v1 = eval env t1 in
      let v2 = eval env t2 in
      apply_binop op v1 v2     
  | Ast.Not t -> (
      (* Not ora applica la negazione logica sui booleani *)
      match eval env t with
      | VBool b -> VBool (not b)
      | _ -> raise (RunTimeError "Type error: Negation of non-boolean"))
  | Ast.If (t1, t2, t3) -> (
      match eval env t1 with
      | VBool true -> eval env t2
      | VBool false -> eval env t3
      | _ -> raise (RunTimeError "Type error: If condition must be a boolean"))
      
  | Ast.Let (x, t1, t2) ->
      let v1 = eval env t1 in
      eval (extend_env env x v1) t2
      
  | Ast.LetFun (f, x, _typ_opt, body, in_expr) ->
      let rec_closure = VRecClosure (f, x, body, env) in
      eval (extend_env env f rec_closure) in_expr

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VClosure (x, _, _) -> Printf.sprintf "<closure: %s>" x
  | VRecClosure (f, x, _, _) -> Printf.sprintf "<rec_closure: %s %s>" f x