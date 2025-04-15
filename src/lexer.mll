{
open Parser

exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | '('      { LPAR }
  | ')'      { RPAR }
  | "["      { LBRACK }
  | "]"      { RBRACK }
  | '!'      { NOT }
  | '&'      { AND }
  | '|'      { OR }
  | 'X'      { NEXT }
  | 'U'      { UNTIL }
  | 'R'      { RELEASE }
  | "^U"     { XUNTIL }
  | "^R"     { XRELEASE }
  | 'G'	     { BOX }
  | "[]"     { BOX }
  | "^G"     { XBOX }
  | "^[]"    { XBOX }
  | 'F'	     { DIAM }
  | "<>"     { DIAM }
  | "^F"     { XDIAM }
  | "^<>"    { XDIAM }
  | "["	     { LBRACK }
  | "]"	     { RBRACK }
  | "<"	     { LT }
  | ">"	     { GT }
  | "<="     { LE }
  | ">="     { GE }
  | "=>"     { IMP }
  | "->"     { IMP }
  | "<=>"    { EQV }
  | "<->"    { EQV }
  | eof      { EOF }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { EVENT (Lexing.lexeme lexbuf) }
  | '@' (id as i) { LABEL i }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
