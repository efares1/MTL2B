open Lexer
open Lexing
open Mtl

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.form Lexer.read lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    True
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let usage_msg = "cmd [-q]"
let verbose = ref true
let speclist = [("-q", Arg.Clear verbose, "no trace information")]
let input_files = ref []
let anon_fun filename = input_files := filename::!input_files

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let lexbuf = Lexing.from_channel stdin in
  let f = parse_with_error lexbuf in
  let f = Mtl2mtl.add_init f in
  let _ = if !verbose then Format.eprintf "etape0: %a\n" Mtl.pp_mtl f in
  let f = Mtl2mtl.elim_Ule_Rle_Rge f in
  let _ = if !verbose then Format.eprintf "etape1: %a\n" Mtl.pp_mtl f in
  let f = Mtl2ltl.mtl2ltl f in
  let _ = if !verbose then Printf.eprintf "etape2: %a\n" (Ltl.pp_ltl Mtl2ltl.pp_prop) f in
  Printf.printf "%a\n" (Ltl.pp_ltl Mtl2ltl.pp_prop) f
