(* HELPERS *)
let is_digit c = match c with '0' .. '9' -> true | _ -> false
let is_alpha c = match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_alphanum c = is_alpha c || is_digit c

let read src n pred pos =
  let rec aux i = if i < n && pred src.[i] then aux (i + 1) else i in
  let end_pos = aux pos in
  String.sub src pos (end_pos - pos)


let last list = match List.rev list with [] -> None | h :: _ -> Some h

