open Ast
module SSet = Set.Make (String)

(* TYPES *)
type mono =
  | TVar of string
  | TInt
  | TBool
  | TFun of mono * mono

type poly = Poly of string list * mono
type subst = (string * mono) list
type env = (string * poly) list

let counter = ref 0

let fresh_tvar () =
  incr counter ;
  TVar (Printf.sprintf "'a%d" !counter)


let empty_subst : subst = []

(* HELPERS — Substitution, unification, generalisation, and instantiation *)

(* Apply a substitution to a monomorphic type, chasing chains of substitutions *)
let rec apply_mono (s : subst) (t : mono) : mono =
  match t with
  | TInt | TBool -> t
  | TVar a -> ( match List.assoc_opt a s with Some t' -> apply_mono s t' | None -> t )
  | TFun (t1, t2) -> TFun (apply_mono s t1, apply_mono s t2)


(* Apply a substitution to a polytype, avoiding capture of quantified variables *)
let apply_poly (s : subst) (Poly (vars, t) : poly) : poly =
  let s_clean = List.filter (fun (v, _) -> not (List.mem v vars)) s in
  Poly (vars, apply_mono s_clean t)


let apply_env (s : subst) (env : env) : env = List.map (fun (x, p) -> (x, apply_poly s p)) env

(* Compose two substitutions: apply s2 then s1 (s1 ∘ s2) *)
let compose_subst s2 s1 = List.map (fun (v, t) -> (v, apply_mono s2 t)) s1 @ s2

(* Free type variables of a monomorphic type *)
let rec fv_mono m =
  match m with
  | TInt | TBool -> SSet.empty
  | TVar a -> SSet.singleton a
  | TFun (t1, t2) -> SSet.union (fv_mono t1) (fv_mono t2)


(* Free type variables of a polytype (excludes quantified vars) *)
let fv_poly (Poly (vars, t)) = SSet.diff (fv_mono t) (SSet.of_list vars)
let fv_env env = List.fold_left (fun acc (_, p) -> SSet.union acc (fv_poly p)) SSet.empty env

(* Instantiate a polytype: replace all quantified vars with fresh type variables *)
let inst (Poly (vars, t)) =
  let s = List.map (fun a -> (a, fresh_tvar ())) vars in
  apply_mono s t


(* Generalise a monomorphic type: quantify over free vars not present in the env *)
let gener env t =
  let new_vars = SSet.diff (fv_mono t) (fv_env env) in
  Poly (SSet.elements new_vars, t)


(* Robinson's unification with occurs check *)
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


(* Algorithm W: type inference — returns (substitution, inferred_type) *)

let rec infer env e : subst * mono =
  match e with
  | Num _ -> (empty_subst, TInt)
  | Boolean _ -> (empty_subst, TBool)
  | Var x -> (
      try (empty_subst, inst (List.assoc x env))
      with Not_found -> failwith (Printf.sprintf "Unbound variable: %s" x) )
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
      (* Let-polymorphism: infer e1, generalise, then bind in environment for e2 *)
      let s1, t1 = infer env e1 in
      let env1 = apply_env s1 env in
      let poly_t1 = gener env1 t1 in
      let s2, t2 = infer ((x, poly_t1) :: env1) e2 in
      (compose_subst s2 s1, apply_mono s2 t2)
  | LetFun (f, x, _, e_body, e_in) ->
      (* Recursive function: assume f : α → β, infer body, unify, then generalise *)
      let a = fresh_tvar () in
      let b = fresh_tvar () in
      let fun_type = TFun (a, b) in
      let env_body = (f, Poly ([], fun_type)) :: (x, Poly ([], a)) :: env in
      let s1, t_body = infer env_body e_body in
      let s2 = unify (apply_mono s1 b) t_body in
      let s_total1 = compose_subst s2 s1 in
      let env_in = apply_env s_total1 env in
      let inferred_fun_type = apply_mono s_total1 fun_type in
      let poly_f = gener env_in inferred_fun_type in
      let s3, t_in = infer ((f, poly_f) :: env_in) e_in in
      (compose_subst s3 s_total1, apply_mono s3 t_in)


let typecheck env e =
  let s, t = infer env e in
  apply_mono s t
