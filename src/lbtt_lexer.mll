{
open Lbtt_parser

exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let oper = "<" | "<=" | ">" | ">="
let st = (int ['s' 't']+)

rule read =
  parse
  | white    { SPC }
  | newline  { EOL }
  | st 	 { ST (Lexing.lexeme lexbuf) }
  | 't'  { TRUE }
  | 'f'  { FALSE }
  | '!'  { NOT }
  | '&'  { AND }
  | '|'  { OR }
  | 'i'  { IMP }
  | 'e'  { EQV }
  | '^'  { EXC }
  | "-1"     { END }
  | eof      { EOF }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "\"rst(" (id as x) ")\"" {RESET x}
  | "\"unch(" (id as x) ")\"" {UNCH x}
  | "\"" (id as x) (' '*) (oper as o) (' '*) (int as d) "\"" {CLK (x,o,int_of_string d)}
  | "\"" (id as a) "\""  { EVENT a }
  | _ { raise (SyntaxError ("Unexpected char: " ^ string_of_int (Char.code (Lexing.lexeme_char lexbuf 0)))) }
