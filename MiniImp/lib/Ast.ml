(* TYPES *)
type expr =
  | Var of string
  | Int of int
  | Add of expr * expr         (* <e> + <e> *)
  | Sub of expr * expr         (* <e> - <e> *)
  | Mul of expr * expr         (* <e> * <e> *)

type bexpr =
  | True
  | False
  | And of bexpr * bexpr       (* <b> and <b> *)
  | Not of bexpr               (* not <b>     *)
  | Less of expr * expr        (* <e> < <e>   *)

type cmd =
  | Skip
  | CmdParen of cmd          (* (<cmd>)                      *)
  | Assign of string * expr  (* <var> := <e>                 *)
  | Seq of cmd * cmd         (* <cmd>;<cmd>                  *)
  | While of bexpr * cmd     (* while <b> do <cmd>           *)
  | If of bexpr * cmd * cmd  (* if <b> then <cmd> else <cmd> *)

type program =
  | Program of string * string * cmd
  (* def main with input <var> output <var> as <cmd> *)

(* HELPERS *)
let rec string_of_cmd c = match c with
  | Assign (v, e)  -> "Assign(Var(\"" ^ v ^ "\"), " ^ string_of_expr e ^ ")"
  | Seq (c1, c2)   -> "Seq(\n" ^ string_of_cmd c1 ^ ",\n" ^ string_of_cmd  c2 ^ "\n" ^ ")"
  | If (b, c1, c2) -> "If(" ^ string_of_bexpr b ^ ",\n" ^ string_of_cmd c1 ^ ",\n" ^ string_of_cmd c2 ^ "\n" ^ ")"
  | While (b, c)   -> "While(" ^ string_of_bexpr b ^ ",\n" ^ string_of_cmd c ^ "\n" ^ ")"
  | CmdParen c     -> "CmdParen(\n" ^ string_of_cmd  c ^ "\n" ^ ")"
  | Skip           -> "skip"

and string_of_program p = match p with
  | Program (input, output, cmd) -> "Program(\n" ^ "  input " ^ input ^ ",\n" ^ "  output " ^ output ^ ",\n" ^ (string_of_cmd cmd) ^ "  " ^ "\n" ^ ")"


and string_of_expr e = match e with
  | Var v        -> "Var(\"" ^ v ^ "\")"
  | Int i        -> "Int(" ^ string_of_int i ^ ")"
  | Add (e1, e2) -> "Add(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Sub (e1, e2) -> "Sub(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) -> "Mul(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"


and string_of_bexpr b = match b with
  | And (b1, b2)  -> "And(" ^ string_of_bexpr b1 ^ ", " ^ string_of_bexpr b2 ^ ")"
  | Not b         -> "Not(" ^ string_of_bexpr b ^ ")"
  | Less (e1, e2) -> "Less(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | True          -> "True"
  | False         -> "False"
