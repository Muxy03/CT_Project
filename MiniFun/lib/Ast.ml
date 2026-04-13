(* TYPES *)
type typo = 
| Int
| Bool
| Fun of typo * typo

type tenv = (string * typo) list

type binop =
  | Add
  | Sub
  | Mul
  | And
  | Lt

type expr =
  | Num of int
  | Boolean of bool
  | Var of string
  | Func of string * typo option * expr
  | App of expr * expr
  | Binop of binop * expr * expr
  | Not of expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | LetFun of string * string * typo option * expr * expr

(* HELPERS *)
let string_of_binop op = match op with 
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | And -> "&&"
  | Lt -> "<"

let rec string_of_typo t = match t with
  | Int -> "int"
  | Bool -> "bool"
  | Fun (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_typo t1) (string_of_typo t2)

let rec string_of_expr (e:expr) = match e with 
    | Num i -> string_of_int i
    | Boolean b -> string_of_bool b
    | Var x -> x
    | App (e1, e2) -> Printf.sprintf "(%s) %s" (string_of_expr e1) (string_of_expr e2)
    | Binop (op, e1, e2) -> Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2) 
    | Not e -> Printf.sprintf "Not(%s)" (string_of_expr e)
    | Func (x, t, body) -> (
      match t with
      | Some typ -> Printf.sprintf "fun %s:%s =>\n %s" x (string_of_typo typ) (string_of_expr body)
      | None -> Printf.sprintf "fun %s =>\n %s" x (string_of_expr body)
    )
      | If (e1, e2, e3) -> Printf.sprintf "if %s then \n %s \n else \n %s" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
    | Let (x, e1, e2) -> Printf.sprintf "let %s = %s \n in \n %s" x (string_of_expr e1) (string_of_expr e2)
    | LetFun (f, x, t, e1, e2) -> (
      match t with
      | Some typ ->   Printf.sprintf "letfun %s %s : %s = \n %s \n in \n %s" f x (string_of_typo typ) (string_of_expr e1) (string_of_expr e2)
      | None ->   Printf.sprintf "letfun %s %s = \n %s \n in \n %s" f x (string_of_expr e1) (string_of_expr e2)
    )
    