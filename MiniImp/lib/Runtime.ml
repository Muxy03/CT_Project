(* EXCEPTIONS *)
exception RuntimeError of string

(* TYPES *)
type value =
  | IntVal of int
  | BoolVal of bool
  | Undefined

type memory = (string, value) Hashtbl.t

(* HELPERS *)

(* EVALUATION *)
let eval (mem : memory) (prog : Parser.prog) =
  match prog with
  | Parser.Prog (Parser.InputVar (i, v), Parser.OutputVar o, cmd) -> (
      let value = match v with Some n -> IntVal n | None -> Undefined in
      Hashtbl.add mem i value ;
      (* eval_cmd mem cmd ; *)
      match Hashtbl.find_opt mem o with
      | Some v -> v
      | None -> raise (RuntimeError ("output variable not defined: " ^ o)))


let rec eval_e (mem : memory) (e : Parser.e) = ()
let rec eval_b (mem : memory) (b : Parser.b) = ()
let rec eval_cmd (mem : memory) cmd = ()

(* RUNNING A PROGRAM *)
