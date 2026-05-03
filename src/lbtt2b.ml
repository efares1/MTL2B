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

let usage_msg = "lbtt2b [-q] [-cr] [-dta <out.dot>] [-xta <out.xta>] [-b <outb.mch>] [-evb <out.mch>]"
let verbose = ref true
let cropt = ref false
let dta_file = ref ""
let xta_file = ref ""
let xtaraw_file = ref ""
let name = ref "Undefined"
let b_file = ref ""
let evb_file = ref ""
let alpha = ref ""
let speclist = [
    ("-q", Arg.Clear verbose, "no trace information");
    ("-dta", Arg.Set_string dta_file, "output timed automaton in dot format");
    ("-xta", Arg.Set_string xta_file, "output timed automaton in xta format");
    ("-xtaraw", Arg.Set_string xtaraw_file, "xta format (no headers)");
    ("-name", Arg.Set_string name, "automaton name");
    ("-alpha", Arg.Set_string alpha, "automaton alphabet completion (w1,...,wn)");
    ("-cr", Arg.Set cropt, "clock reduction optimization");
    ("-b", Arg.Set_string b_file, "output timed automaton in B format");
    ("-evb", Arg.Set_string evb_file, "output timed automaton in EVB format")
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
  let alpha = String.split_on_char ',' !alpha in
  let ta = if alpha=[] then Lbtt2ta.lbtt2ta a else Lbtt2ta.lbtt2ta ?alpha:(Some alpha) a in
  let ta = if !cropt then Ta.rem_unused_resets ta else ta in 
  let ta = if !cropt then Ta.rem_sync_clks ta else ta in 
  if (!dta_file <> "") then (
    let out = open_out !dta_file in
    Ta.ppd_auto out ta;
    close_out out
  );
  if (!xta_file <> "") then (
    let out = Format.formatter_of_out_channel (open_out !xta_file) in
    Ta.ppx_auto !name out ta
  );
  if (!xtaraw_file <> "") then (
    let out = Format.formatter_of_out_channel (open_out !xtaraw_file) in
    Ta.ppx_auto ~raw:() !name out ta
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
