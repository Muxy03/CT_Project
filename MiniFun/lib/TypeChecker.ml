open Ast

exception TypeError of string

(* Helper per controllare i tipi *)
let expect_type expected actual ctx =
  if expected <> actual then
    let msg = Printf.sprintf "Type mismatch in %s:\n Expected: %s\n Got: %s" 
      ctx (string_of_typo expected) (string_of_typo actual) in
    raise (TypeError msg)

(* Sistema di deduzione (Fragment 3) *)
let rec typecheck (env : tenv) (e : expr) : typo =
  match e with
  | Num _ -> Int
  | Boolean _ -> Bool
  
  | Var x -> (
      try List.assoc x env
      with Not_found -> raise (TypeError ("Unbound variable: " ^ x))
    )
    
  | Func (x, Some t_arg, body) ->
      let t_body = typecheck ((x, t_arg) :: env) body in
      Fun (t_arg, t_body)
      
  | Func (_, None, _) -> 
      raise (TypeError "Fragment 3 requires explicit type annotations on Func")
      
  | App (e1, e2) -> (
      let t1 = typecheck env e1 in
      let t2 = typecheck env e2 in
      match t1 with
      | Fun (t_arg, t_ret) ->
          expect_type t_arg t2 "function argument";
          t_ret
      | _ -> raise (TypeError ("Application of non-function: " ^ string_of_typo t1))
    )
    
  | Binop (op, e1, e2) ->
      let t1 = typecheck env e1 in
      let t2 = typecheck env e2 in
      (match op with
      | Add | Sub | Mul ->
          expect_type Int t1 "left operand of arithmetic operator";
          expect_type Int t2 "right operand of arithmetic operator";
          Int
      | Lt ->
          expect_type Int t1 "left operand of '<'";
          expect_type Int t2 "right operand of '<'";
          Bool
      | And ->
          expect_type Bool t1 "left operand of '&&'";
          expect_type Bool t2 "right operand of '&&'";
          Bool)
          
  | Not e1 ->
      let t1 = typecheck env e1 in
      expect_type Bool t1 "logical NOT operator";
      Bool
      
  | If (e1, e2, e3) ->
      let t1 = typecheck env e1 in
      expect_type Bool t1 "if condition";
      let t2 = typecheck env e2 in
      let t3 = typecheck env e3 in
      expect_type t2 t3 "if branches (then and else must match)";
      t2
      
  | Let (x, e1, e2) ->
      let t1 = typecheck env e1 in
      typecheck ((x, t1) :: env) e2
      
  | LetFun (f, x, Some (Fun (t_arg, t_ret) as typ_annot), e1, e2) ->
      let env_body = (x, t_arg) :: (f, typ_annot) :: env in
      let t1 = typecheck env_body e1 in
      expect_type t_ret t1 "letfun body return type";
      typecheck ((f, typ_annot) :: env) e2
      
  | LetFun (_, _, Some _, _, _) ->
      raise (TypeError "letfun annotation must be a function type (e.g., Int -> Int)")
      
  | LetFun (_, _, None, _, _) ->
      raise (TypeError "Fragment 3 requires explicit type annotations on LetFun")