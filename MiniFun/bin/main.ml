open MiniFun

(* HELPERS *)
let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.to_string s


let parse_string (src : string) : MiniFun.Ast.expr =
  try MiniFun.Parser.parse src with
  | MiniFun.Lexer.SyntaxError msg -> failwith (Printf.sprintf "Lexing error: %s" msg)
  | MiniFun.Parser.ParseError msg -> failwith (Printf.sprintf "Parsing error: %s" msg)


let () =
  let sample_code = read_file Sys.argv.(1) in

  Printf.printf "--- Source Code ---\n%s\n\n" sample_code ;

  let ast = parse_string sample_code in

  Printf.printf "--- Abstract Syntax Tree ---\n%s\n" (Ast.string_of_expr ast) ;

  try
    let result = RunTime.eval (RunTime.env_init None) ast in
    Printf.printf "--- Result ---\n%s\n" (RunTime.string_of_value result)
  with RunTime.RunTimeError msg ->
    Printf.eprintf "--- Runtime Error ---\n%s\n" msg ;
    exit 1
