(* TYPES *)
type binop =
  | Add
  | Sub
  | Mul
  | And
  | Lt

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Binop of binop * expr * expr
  | Not of expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | LetFun of string * string * expr * expr

(* HELPERS *)
let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | And -> "&&"
  | Lt -> "<"


let colors =
  [|
     "\027[31m"
   ; (* Red *)
     "\027[33m"
   ; (* Yellow *)
     "\027[32m"
   ; (* Green *)
     "\027[36m"
   ; (* Cyan *)
     "\027[34m"
   ; (* Blue *)
     "\027[35m"
     (* Magenta *)
  |]


let reset = "\027[0m" (* Resets the terminal color back to default *)
let lp d = colors.(d mod Array.length colors) ^ "(" ^ reset
let rp d = colors.(d mod Array.length colors) ^ ")" ^ reset

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | And -> "&&"
  | Lt -> "<"


let string_of_expr e =
  (* 'ind' is the indentation string, 'd' is the current tree depth for colors *)
  let rec aux ind d = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Var x -> x
    | App (e1, e2) ->
        Printf.sprintf "%s%s %s%s" (lp d) (aux ind (d + 1) e1) (aux ind (d + 1) e2) (rp d)
    | Binop (op, e1, e2) ->
        Printf.sprintf "%s%s %s %s%s" (lp d)
          (aux ind (d + 1) e1)
          (string_of_binop op)
          (aux ind (d + 1) e2)
          (rp d)
    | Not e -> Printf.sprintf "%s~ %s%s" (lp d) (aux ind (d + 1) e) (rp d)
    | Fun (x, body) ->
        let next_ind = ind ^ "  " in
        Printf.sprintf "%sfun %s =>\n%s%s%s" (lp d) x next_ind
          (aux next_ind (d + 1) body)
          (rp d)
    | If (e1, e2, e3) ->
        let next_ind = ind ^ "  " in
        Printf.sprintf "%sif %s then\n%s%s\n%selse\n%s%s%s" (lp d)
          (aux ind (d + 1) e1)
          next_ind
          (aux next_ind (d + 1) e2)
          ind next_ind
          (aux next_ind (d + 1) e3)
          (rp d)
    | Let (x, e1, e2) ->
        let next_ind = ind ^ "  " in
        Printf.sprintf "%slet %s =\n%s%s\n%sin\n%s%s%s" (lp d) x next_ind
          (aux next_ind (d + 1) e1)
          ind next_ind
          (aux next_ind (d + 1) e2)
          (rp d)
    | LetFun (f, x, e1, e2) ->
        let next_ind = ind ^ "  " in
        Printf.sprintf "%sletfun %s %s =\n%s%s\n%sin\n%s%s%s" (lp d) f x next_ind
          (aux next_ind (d + 1) e1)
          ind next_ind
          (aux next_ind (d + 1) e2)
          (rp d)
  in
  aux "" 0 e
