open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Lbtt_parser.automaton Lbtt_lexer.read lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    (1, "0t", [])
  | Parser.Error | Lbtt_parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let usage_msg = "lbtts2nta [-q]"

let speclist = [
    ("-q", Arg.Clear verbose, "no trace information")
  ]
let input_files = ref []
let anon_fun filename = input_files := filename::!input_files

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let lexbuf = Lexing.from_channel stdin in
  let a = parse_with_error lexbuf in
  if !verbose then Printf.fprintf stdout "etape 0: %a" Lbtt.pp_auto a;
  let a = Lbtt.rm_or a in
  if !verbose then Printf.fprintf stdout "etape 1: %a" Lbtt.pp_auto a;
  let a = Lbtt.add_rst a in
  if !verbose then Printf.fprintf stdout "etape 2: %a" Lbtt.pp_auto a;
  let ta = Lbtt2ta.lbtt2ta a in
  let ta = if !cropt then Ta.rem_unused_resets ta else ta in 
  let ta = if !cropt then Ta.rem_sync_clks ta else ta in 
  let ta = if !prinv then Ta.propagate ta else ta in 
  if (!dta_file <> "") then (
    let out = open_out !dta_file in
    Ta.ppd_auto out ta;
    close_out out
  );
  if (!xta_file <> "") then (
    let out = Format.formatter_of_out_channel (open_out !xta_file) in
    Ta.ppx_auto out ta
  );
  if (!b_file <> "") then (
    let out = open_out !b_file in
    Ta.ppb_auto out ta;
    close_out out
  );
  if (!evb_file <> "") then (
    let out = open_out !evb_file in
    Ta.ppevb_auto out ta;
    close_out out
  )
