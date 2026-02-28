{
open Parser
exception SyntaxError of string
}

let white = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let var = letter (letter | digit)*
let int_num = digit+

rule read = parse
  | white    { read lexbuf }
  | "def"    { DEF }
  | "main"   { MAIN }
  | "with"   { WITH }
  | "input"  { INPUT }
  | "output" { OUTPUT }
  | "as"     { AS }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "do"     { DO }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "and"    { AND }
  | "not"    { NOT }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | ":="     { ASSIGN }
  | ";"      { SEMI }
  | "<"      { LT }
  | "+"      { PLUS }
  | "-"      { MINUS }
  | "*"      { TIMES }
  | var as v { VAR v }
  | int_num as i { INT (int_of_string i) }
  | eof      { EOF }
  | _        { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }