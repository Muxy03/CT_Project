let () =
  let src =
    "def main with input x output y as\n\
    \      y := 0;\n\
    \      while x < 10 do\n\
    \        y := y + x;\n\
    \        x := x + 1"
  in
  try
    let ast = MiniImp.Parser.parse src in
    print_endline (MiniImp.Parser.string_of_ast ast) ;
    print_endline "Parsing successful!"
  with
  | MiniImp.Lexer.LexerError msg -> print_endline ("Lexer error: " ^ msg)
  | MiniImp.Parser.ParserError msg -> print_endline ("Parser error: " ^ msg)
