open MiniImp

(* HELPERS *)
let read_file path =
  try
    let ic = open_in path in
    let n  = in_channel_length ic in
    let buf  = Bytes.create n in
    really_input ic buf 0 n;
    close_in ic;
    Bytes.to_string buf
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

let write_file path content =
  try
    let oc = open_out path in
    output_string oc content;
    output_char oc '\n';
    close_out oc
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

let parse_string src =
  try Parser.parse src
  with
  | Lexer.SyntaxError msg ->
      Printf.eprintf "LexerError: %s\n" msg;
      exit 1
  | Parser.ParseError msg ->
      Printf.eprintf "ParserError: %s\n" msg;
      exit 1

let () =
  if Array.length Sys.argv < 2 then
  begin
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  end;
  let ast = Sys.argv.(1) |> read_file |> parse_string in
  match ast with
  | Ast.Program (input_name, output_name, cmd) ->
      let cfg = Cfg.generate_cfg cmd in
      Optimize.optimize cfg output_name;
      let ir = Llvm.generate_llvm_ir cfg input_name output_name in
      write_file "generated/output.ll" ir;
      Printf.printf "IR code saved in generated/output.ll\n";
      (try
        let result = Runtime.eval ast in
        Printf.printf "--- Result (Interpreter) ---\n %s \n"
          (Runtime.string_of_value result)
       with Runtime.RuntimeError msg ->
        Printf.eprintf "Runtime Error: %s\n" msg)
