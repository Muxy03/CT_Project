open Ast
module SSet = Set.Make (String)

type tvar = string

type mono =
  | TVar of tvar
  | TInt
  | TBool
  | TFun of mono * mono

type poly = Poly of tvar list * mono
type subst = (tvar * mono) list
type env = (string * poly) list

let counter = ref 0

let fresh_tvar () =
  incr counter ;
  TVar ("'a" ^ string_of_int !counter)


let empty_subst : subst = []

let rec apply_mono (s : subst) (t : mono) : mono =
  match t with
  | TInt | TBool -> t
  | TVar a -> (
      match List.assoc_opt a s with
      | Some t' -> apply_mono s t' (* Cerca ricorsivamente nella catena *)
      | None -> t)
  | TFun (t1, t2) -> TFun (apply_mono s t1, apply_mono s t2)


let apply_poly (s : subst) (Poly (vars, t) : poly) : poly =
  let s_clean = List.filter (fun (v, _) -> not (List.mem v vars)) s in
  Poly (vars, apply_mono s_clean t)


let apply_env (s : subst) (env : env) : env =
  List.map (fun (x, p) -> (x, apply_poly s p)) env


let compose_subst s2 s1 = List.map (fun (v, t) -> (v, apply_mono s2 t)) s1 @ s2

let rec fv_mono = function
  | TInt | TBool -> SSet.empty
  | TVar a -> SSet.singleton a
  | TFun (t1, t2) -> SSet.union (fv_mono t1) (fv_mono t2)


let fv_poly (Poly (vars, t)) = SSet.diff (fv_mono t) (SSet.of_list vars)

let fv_env env =
  List.fold_left (fun acc (_, p) -> SSet.union acc (fv_poly p)) SSet.empty env


let inst (Poly (vars, t)) =
  let s = List.map (fun a -> (a, fresh_tvar ())) vars in
  apply_mono s t


let gener env t =
  let new_vars = SSet.diff (fv_mono t) (fv_env env) in
  Poly (SSet.elements new_vars, t)


let occurs a t = SSet.mem a (fv_mono t)

let rec unify t1 t2 =
  match (t1, t2) with
  | TInt, TInt | TBool, TBool -> empty_subst
  | TVar a, TVar b when a = b -> empty_subst
  | TVar a, t | t, TVar a ->
      if occurs a t then failwith "Unification failed: occurs check" else [ (a, t) ]
  | TFun (t1a, t1b), TFun (t2a, t2b) ->
      let s1 = unify t1a t2a in
      let s2 = unify (apply_mono s1 t1b) (apply_mono s1 t2b) in
      compose_subst s2 s1
  | _ -> failwith "Unification failed: incompatible types"


let rec infer env e : subst * mono =
  match e with
  | Num _ -> (empty_subst, TInt)
  | Boolean _ -> (empty_subst, TBool)
  | Var x -> (
      try (empty_subst, inst (List.assoc x env))
      with Not_found -> failwith ("Unbound variable: " ^ x))
  | Func (x, _, body) ->
      let arg_type = fresh_tvar () in
      let s1, ret_type = infer ((x, Poly ([], arg_type)) :: env) body in
      (s1, TFun (apply_mono s1 arg_type, apply_mono s1 ret_type))
  | App (e1, e2) ->
      let s1, t1 = infer env e1 in
      let s2, t2 = infer (apply_env s1 env) e2 in
      let ret_type = fresh_tvar () in
      let s3 = unify (apply_mono s2 t1) (TFun (t2, ret_type)) in
      (compose_subst s3 (compose_subst s2 s1), apply_mono s3 ret_type)
  | Binop (op, e1, e2) ->
      let s1, t1 = infer env e1 in
      let s2, t2 = infer (apply_env s1 env) e2 in
      let t_left, t_right, t_ret =
        match op with
        | Add | Sub | Mul -> (TInt, TInt, TInt)
        | Lt -> (TInt, TInt, TBool)
        | And -> (TBool, TBool, TBool)
      in
      let s3 = unify (apply_mono s2 t1) t_left in
      let s4 = unify (apply_mono s3 t2) t_right in
      (compose_subst s4 (compose_subst s3 (compose_subst s2 s1)), t_ret)
  | Not e1 ->
      let s1, t1 = infer env e1 in
      let s2 = unify t1 TBool in
      (compose_subst s2 s1, TBool)
  | If (cond, e_then, e_else) ->
      let s1, t_cond = infer env cond in
      let s2 = unify t_cond TBool in
      let env2 = apply_env (compose_subst s2 s1) env in
      let s3, t_then = infer env2 e_then in
      let env3 = apply_env s3 env2 in
      let s4, t_else = infer env3 e_else in
      let s5 = unify (apply_mono s4 t_then) t_else in
      ( compose_subst s5 (compose_subst s4 (compose_subst s3 (compose_subst s2 s1)))
      , apply_mono s5 t_else )
  | Let (x, e1, e2) ->
      let s1, t1 = infer env e1 in
      let env1 = apply_env s1 env in
      let poly_t1 = gener env1 t1 in
      let s2, t2 = infer ((x, poly_t1) :: env1) e2 in
      (compose_subst s2 s1, apply_mono s2 t2)
      (* Added apply_mono s2 *)
  | LetFun (f, x, _, e_body, e_in) ->
      let a = fresh_tvar () in
      let b = fresh_tvar () in
      let fun_type = TFun (a, b) in
      let env_body = (f, Poly ([], fun_type)) :: (x, Poly ([], a)) :: env in
      let s1, t_body = infer env_body e_body in
      let s2 = unify (apply_mono s1 b) t_body in
      let s_total1 = compose_subst s2 s1 in
      let env_in = apply_env s_total1 env in
      (* Applichiamo s_total1 al tipo della funzione prima di generalizzare *)
      let inferred_fun_type = apply_mono s_total1 fun_type in
      let poly_f = gener env_in inferred_fun_type in
      let s3, t_in = infer ((f, poly_f) :: env_in) e_in in
      (* Applichiamo s3 (la sostituzione di e_in) al tipo t_in *)
      (compose_subst s3 s_total1, apply_mono s3 t_in)


let typecheck env e =
  (* NON resettare counter := 0 qui se vuoi evitare clash con l'env *)
  let s, t = infer env e in
  apply_mono s t (* FONDAMENTALE: applica la sostituzione finale al tipo *)
