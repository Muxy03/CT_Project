open MiniImp

(* HELPERS *)
let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.to_string s


let parse_string str =
  let lexbuf = Lexing.from_string str in
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      Printf.eprintf "Lexing error: %s\n" msg ;
      exit 1
  | Parser.Error ->
      Printf.eprintf "Parsing error around character %d\n" (Lexing.lexeme_start lexbuf) ;
      exit 1


let () =
  let sample_code = read_file Sys.argv.(1) in

  Printf.printf "--- Source Code ---\n%s\n\n" sample_code ;

  let ast = parse_string sample_code in

  Printf.printf "--- Abstract Syntax Tree ---\n%s\n" (Ast.string_of_prog ast) ;

  try
    let result = Runtime.eval ast in
    Printf.printf "--- Result ---\n%s\n" (Runtime.string_of_value result)
  with Runtime.RuntimeError msg ->
    Printf.eprintf "--- Runtime Error ---\n%s\n" msg ;
    exit 1
