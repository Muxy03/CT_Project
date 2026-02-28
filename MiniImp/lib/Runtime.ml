(* EXCEPTIONS *)
exception RuntimeError of string

(* TYPES *)
type value = | Int of int | Bool of bool  
type memory = (string, value) Hashtbl.t

(* HELPERS *)

(* EVALUATION *)

(* TODO *)