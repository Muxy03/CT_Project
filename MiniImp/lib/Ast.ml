(* TYPES *)
type e =
  | Var of string
  | Int of int
  | Add of e * e (* <e> + <e> *)
  | Sub of e * e (* <e> - <e> *)
  | Mul of e * e (* <e> * <e> *)

type b =
  | True
  | False
  | And of b * b  (* <b> and <b> *) (*TODO: <e> and <e> ?*)
  | Not of b      (* not <b> *) (*TODO: not <e> ?*)
  | Less of e * e (* <e> < <e> *)

type cmd =
  | CmdParen of cmd      (* (<cmd>) *)
  | Assign of string * e (* <var> := <e> *)
  | Seq of cmd * cmd     (* <cmd>;<cmd> *)
  | If of b * cmd * cmd  (* if <b> then <cmd> else <cmd> *)
  | While of b * cmd     (* while <b> do <cmd> *)

type inputVar = InputVar of string * int option

type prog =
  | Prog of inputVar * string * cmd (* def main with input <var> output <var> as <cmd> *)

(* HELPERS *)
let rec string_of_e = function
  | Var v -> "Var(\"" ^ v ^ "\")"
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Add (e1, e2) -> "Add(" ^ string_of_e e1 ^ ", " ^ string_of_e e2 ^ ")"
  | Sub (e1, e2) -> "Sub(" ^ string_of_e e1 ^ ", " ^ string_of_e e2 ^ ")"
  | Mul (e1, e2) -> "Mul(" ^ string_of_e e1 ^ ", " ^ string_of_e e2 ^ ")"


let rec string_of_b = function
  | True -> "True"
  | False -> "False"
  | And (b1, b2) -> "And(" ^ string_of_b b1 ^ ", " ^ string_of_b b2 ^ ")"
  | Not b -> "Not(" ^ string_of_b b ^ ")"
  | Less (e1, e2) -> "Less(" ^ string_of_e e1 ^ ", " ^ string_of_e e2 ^ ")"


let rec string_of_cmd ind = function
  | Assign (v, e) -> ind ^ "Assign(Var(\"" ^ v ^ "\"), " ^ string_of_e e ^ ")"
  | Seq (c1, c2) ->
      ind ^ "Seq(\n"
      ^ string_of_cmd (ind ^ "  ") c1
      ^ ",\n"
      ^ string_of_cmd (ind ^ "  ") c2
      ^ "\n" ^ ind ^ ")"
  | If (b, c1, c2) ->
      ind ^ "If(" ^ string_of_b b ^ ",\n"
      ^ string_of_cmd (ind ^ "  ") c1
      ^ ",\n"
      ^ string_of_cmd (ind ^ "  ") c2
      ^ "\n" ^ ind ^ ")"
  | While (b, c) ->
      ind ^ "While(" ^ string_of_b b ^ ",\n"
      ^ string_of_cmd (ind ^ "  ") c
      ^ "\n" ^ ind ^ ")"
  | CmdParen c -> ind ^ "CmdParen(\n" ^ string_of_cmd (ind ^ "  ") c ^ "\n" ^ ind ^ ")"


let string_of_prog = function
  | Prog (input, output, cmd) ->
      let input_str =
        match input with
        | InputVar (name, None) -> name
        | InputVar (name, Some default) -> name ^ " := " ^ string_of_int default
      in
      "Prog(\n" ^ "  input " ^ input_str ^ ",\n" ^ "  output " ^ output ^ ",\n"
      ^ string_of_cmd "  " cmd ^ "\n" ^ ")"
