{
open Parser
exception SyntaxError of string
}

let white = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let var = letter (letter | digit)*
let int_num = '-'? digit+

rule read = parse
  | white    { read lexbuf }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "fun"    { FUN }
  | "=>"     { ARROW }
  | "->"     { TARROW }
  | ":"      { COLON }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "let"    { LET }
  | "in"     { IN }
  | "letfun" { LETFUN }
  | "="      { EQ }
  | "+"      { PLUS }
  | "-"      { MINUS }
  | "*"      { STAR }
  | "&&"     { AND }
  | "<"      { LT }
  | "~"      { NOT }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "int"    { TINT }
  | "bool"   { TBOOL }
  | var as v  { VAR v }
  | int_num as i { INT (int_of_string i) }
  | eof      { EOF }
  | _        { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }