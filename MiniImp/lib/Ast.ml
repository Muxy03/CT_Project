(* TYPES *)
type expr =
  | Var of string
  | Int of int
  | Add of expr * expr (* <e> + <e> *)
  | Sub of expr * expr (* <e> - <e> *)
  | Mul of expr * expr (* <e> * <e> *)

type bexpr =
  | True
  | False
  | And of bexpr * bexpr (* <b> and <b> *)
  | Not of bexpr (* not <b> *)
  | Less of expr * expr (* <e> < <e> *)

type cmd =
  | Skip
  | CmdParen of cmd (* (<cmd>) *)
  | Assign of string * expr (* <var> := <e> *)
  | Seq of cmd * cmd (* <cmd>;<cmd> *)
  | While of bexpr * cmd (* while <b> do <cmd> *)
  | If of bexpr * cmd * cmd (* if <b> then <cmd> else <cmd> *)

type program =
  | Program of string * string * cmd (* def main with input <var> output <var> as <cmd> *)

(* HELPERS *)
let rec string_of_cmd c =
  match c with
  | Assign (v, e) -> Printf.sprintf "Assign(Var(%s), %s)" v (string_of_expr e)
  | Seq (c1, c2) -> Printf.sprintf "Seq(\n %s, \n %s \n)" (string_of_cmd c1) (string_of_cmd c2)
  | If (b, c1, c2) ->
      Printf.sprintf "If(%s,\n %s, \n %s \n)" (string_of_bexpr b) (string_of_cmd c1)
        (string_of_cmd c2)
  | While (b, c) -> Printf.sprintf "While(%s, \n %s \n)" (string_of_bexpr b) (string_of_cmd c)
  | CmdParen c -> Printf.sprintf "CmdParen(\n %s \n)" (string_of_cmd c)
  | Skip -> "skip"


and string_of_program p =
  match p with
  | Program (input, output, cmd) ->
      Printf.sprintf "Program(\n input %s,\n output %s,\n %s \n)" input output (string_of_cmd cmd)


and string_of_expr e =
  match e with
  | Var v -> Printf.sprintf "Var(%s)" v
  | Int i -> Printf.sprintf "Int(%d)" i
  | Add (e1, e2) -> Printf.sprintf "Add(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Sub (e1, e2) -> Printf.sprintf "Sub(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Mul (e1, e2) -> Printf.sprintf "Mul(%s, %s)" (string_of_expr e1) (string_of_expr e2)


and string_of_bexpr b =
  match b with
  | And (b1, b2) -> Printf.sprintf "And(%s, %s)" (string_of_bexpr b1) (string_of_bexpr b2)
  | Not b -> Printf.sprintf "Not(%s)" (string_of_bexpr b)
  | Less (e1, e2) -> Printf.sprintf "Less(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | True -> "True"
  | False -> "False"
