(* EXCEPTIONS *)
exception RuntimeError of string

(* TYPES *)
type value =
  | IntVal  of int
  | BoolVal of bool

type memory = (string, value) Hashtbl.t

(* HELPERS *)

(* EVALUATION *)
let rec eval_e = ()
let rec eval_b = ()
let rec eval_cmd = ()

(* RUNNING A PROGRAM *)