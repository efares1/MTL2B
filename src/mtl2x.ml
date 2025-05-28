open Lexer
open Lexing
module List' = List
open Shexp_process
open Shexp_process.Infix

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.forms Lexer.read lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let usage_msg = "mtl2x [-q] [-pr] [-ta] [-b] <input file.mtl>"
let verbose = ref true
let ta_out = ref false
let xta_out = ref false
let b_out = ref false
let p_out = ref false
let close_alpha = ref true
let prinv = ref false

let speclist =
  [("-q", Arg.Clear verbose, "no trace information");
   ("-c", Arg.Clear close_alpha, "don't close alphabet, don't add 'other' event");
   ("-pr", Arg.Set prinv, "invariant propagation");
   ("-ta", Arg.Set ta_out, "output TA in dot/pdf format");
   ("-xta", Arg.Set xta_out, "output TA in XTA format");
   ("-b", Arg.Set b_out, "output TA in B format");
   ("-p", Arg.Set p_out, "output TA product in EVB format")
  ]
let input_files = ref []
let anon_fun filename = input_files := filename::!input_files

let xta_header oc evts procs =
  let ne = List'.length evts in
  let np = List'.length procs in
  Format.fprintf oc "chan ";
  List'.iteri (fun i e ->
      List'.iteri (fun j p ->
          Format.fprintf oc "%s_%s" p e;
          if i<ne-1 || j <np-1 then Format.pp_print_string oc ", "
        ) procs
    ) evts;
  Format.fprintf oc ";\n";
  Format.pp_print_flush oc ()

let xta_sync oc evts procs =
  let ne = List'.length evts and np = List'.length procs in
  Format.fprintf oc "process Synchro() {\n";
  Format.fprintf oc "state\n ";
  List'.iter (fun e ->
      List'.iteri (fun i p ->
          if i<>np-1 then Format.fprintf oc " L%s%s," e p
        ) procs;
      Format.fprintf oc "\n "
    ) evts;
  Format.fprintf oc " L0;\n";
  if List'.length procs > 1 then begin
      Format.fprintf oc "commit\n ";
      List'.iteri (fun j e ->
          List'.iteri (fun i p ->
              if i<>np-1 then (
                Format.fprintf oc " L%s%s" e p;
                if i < np-2 || j<>ne-1 then Format.fprintf oc ","
              )
            ) procs;
          if j=ne-1 then Format.fprintf oc ";\n"
          else Format.fprintf oc "\n "
        ) evts
    end;
  Format.fprintf oc "init L0;\n";
  Format.fprintf oc "trans\n ";
  List'.iteri (fun j e ->
      let s = ref "L0" in
      List'.iteri (fun i p ->
          if i<np-1 then (
            Format.fprintf oc " %s -> L%s%s {sync %s_%s!;}" !s e p p e;
            s := "L"^e^p
          ) else
            Format.fprintf oc " %s -> L0 {sync %s_%s!;}" !s p e;
          if (i=np-1 && j=ne-1) then
            Format.fprintf oc ";\n"
          else
            Format.fprintf oc ",\n "
        )
        procs
    ) evts;
  Format.fprintf oc "}\n";
  List'.iter (fun p -> Format.fprintf oc "i%s = %s();\n" p p) procs;
  Format.fprintf oc "synchro = Synchro();\n";
  Format.fprintf oc "system ";
  List'.iter (fun p -> Format.fprintf oc "i%s, " p) procs;
  Format.fprintf oc "synchro;\n";
  Format.pp_print_flush oc ()

let _ =
  Arg.parse speclist anon_fun usage_msg;
  if (List'.length !input_files <> 1) then (
    Printf.eprintf "Usage: %s\n" usage_msg;
    exit (-1)
  );
  let lexbuf = Lexing.from_channel (open_in (List'.hd !input_files)) in
  let fl = parse_with_error lexbuf in
  let mtl2ltl = "_build/install/default/bin/mtl2ltl.exe" in
  let lbtt2b = "_build/install/default/bin/lbtt2b.exe" in
  let ltl2tgba = "ltl2tgba" in
  if eval (find_executable mtl2ltl) = None then (
    Printf.eprintf "mtl2ltl.exe not found";
    exit (-1)
  );
  if eval (find_executable lbtt2b) = None then (
    Printf.eprintf "lbtt2b.exe not found";
    exit (-1)
  );
  if eval (find_executable ltl2tgba) = None then (
    Printf.eprintf "ltl2tgba not found";
    exit (-1)
  );
  let alpha =
    String.concat ","
      ((if !close_alpha then ["other"] else []) @
         (List'.sort_uniq compare (List'.concat_map (fun (_,m) -> Mtl.get_evts m) fl))) in
  let main =
    mkdir ~p:() "out" >>
    List.iter fl
      ~f:(fun (nm,mtl) ->
        let f = Format.asprintf "%a" Mtl.pp_mtl mtl in
        ((if !verbose then echo f else return ())
         >> (echo f
             |- run mtl2ltl
                  ((if !verbose then [] else ["-q"])
                   @ (if !close_alpha then [] else ["-c"]))
             |- run ltl2tgba ["--lbtt=t"; "-"]
             |- run lbtt2b
                  (["-cr"]
                   @ (if !prinv then ["-pr"] else [])
                   @ (if !ta_out then ["-dta"; "out/"^nm^".dot"] else [])
                   @ (if !xta_out then ["-alpha"; alpha; "-name"; nm; "-xtaraw"; "out/"^nm^".xta"] else [])
                   @ ["-evb"; "out/"^nm^".mch"]
                   @ (if !verbose then [] else ["-q"])
                   @ (if !b_out then ["-b"; "out/"^nm^"-b.mch"] else []))
        ))
        >> if !ta_out then run "dot" ["-T";"pdf"; "out/"^nm^".dot"; "-oout/"^nm^".pdf"] else return ()
      ) in
  eval main;
  if (!p_out) then (
    let out = Format.formatter_of_out_channel (open_out "out/product.mch") in
    Mtl.ppp_mtl out "product" fl
  );
  if (!xta_out) then (
    let prd = "out/product.xta" in
    let sync = "out/sync.xta" in
    let header = "out/header.xta" in
    let procs = List'.map fst fl in
    let alpha = String.split_on_char ',' alpha in
    let _ =
      let out = Format.formatter_of_out_channel (open_out sync) in
      xta_sync out alpha procs in
    let _ =
      let out = Format.formatter_of_out_channel (open_out header) in
      xta_header out alpha procs in
    let prod =
      outputs_to prd (run "cat" (header::(List'.map (fun (nm,_) -> "out/"^nm^".xta") fl) @[sync])) in
    eval prod;
  )
