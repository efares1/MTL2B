(* mtl2tba: from an MTL(0,inf) formula to a timed automaton for UPPAAL.

     mtl2tba [options] 'formula'

   Chain:
     1. parsing and negation normal form          (prototype: lexer, parser, Mtl)
     2. derivation of the ordinary timed operators (extracted from Coq: MUle, ...)
     3. clocked-LTL translation T                  (extracted from Coq)
     4. LTL -> Buchi automaton                     (Spot, ltl2tgba)
     5. relaxation and reset completion            (extracted from Coq: compile_with)
     6. optimization, iterated to a fixpoint       (extracted from Coq: optimize)
     7. export: conjunctive guards and invariants  (extracted from Coq: export)
     8. UPPAAL (.xml) and Graphviz (.dot) output

   Steps 2, 3, 5, 6, 7 are proved correct in Coq (MTL_to_TBA_correct_with,
   optimize_accepts, export_accepts), assuming that Spot is correct. *)

open Optim

let out = ref "out"
let spot = ref "ltl2tgba"
let with_init = ref false
let rounds = ref 50
let verbose = ref false
let dump_tba = ref false
let formula = ref ""
let stats = ref false
let pdf = ref true
let dot_cmd = ref "dot"

let stats_header = String.concat "	"
  [ "formula_clocks"; "spot_states"; "spot_trans"; "spot_s";
    "comp_locs"; "comp_trans"; "comp_clocks";
    "opt_rounds"; "opt_locs"; "opt_trans"; "opt_clocks";
    "exp_locs"; "exp_trans"; "exp_clocks"; "exp_invs"; "exp_diffs";
    "opt_s"; "exp_s"; "total_s" ]

let speclist = [
  ("-o", Arg.Set_string out, "<base> output files <base>.xml and <base>.dot (default: out)");
  ("-init", Arg.Set with_init, " add an initialization event _init_ fixing the time origin");
  ("-n", Arg.Set_int rounds, "<n> maximal number of optimization rounds (default: 50)");
  ("-spot", Arg.Set_string spot, "<cmd> LTL-to-Buchi command (default: ltl2tgba)");
  ("-nopdf", Arg.Clear pdf, " do not run Graphviz to produce <base>.pdf from <base>.dot");
  ("-dot", Arg.Set_string dot_cmd, "<cmd> Graphviz command (default: dot)");
  ("-tba", Arg.Set dump_tba, " also write <base>_tba.dot and <base>_opt.dot");
  ("-v", Arg.Set verbose, " print the intermediate results");
  ("-stats", Arg.Set stats, " print one tab-separated line of measures (see -stats-header)");
  ("-stats-header", Arg.Unit (fun () -> print_endline stats_header; exit 0), " print the header of -stats");
]
let usage = "mtl2tba [options] 'formula'"

let log fmt = Printf.ksprintf (fun s -> if !verbose then prerr_endline s) fmt
let fail fmt = Printf.ksprintf (fun s -> prerr_endline ("mtl2tba: " ^ s); exit 1) fmt

(* ------------------------------------------------------------------ *)
(* 1-2. Formula                                                        *)

let parse_formula s =
  try Parser.form Lexer.read (Lexing.from_string s) with
  | Lexer.SyntaxError m -> fail "syntax error: %s" m
  | Parser.Error -> fail "syntax error in %S" s

let event_tbl : (string, int) Hashtbl.t = Hashtbl.create 16
let event_names : (int, string) Hashtbl.t = Hashtbl.create 16
let event s =
  match Hashtbl.find_opt event_tbl s with
  | Some i -> i
  | None ->
    let i = Hashtbl.length event_tbl in
    Hashtbl.add event_tbl s i; Hashtbl.add event_names i s; i

(* Prototype formulas to the formulas of the Coq development; the ordinary
   timed operators are derived by the extracted definitions. *)
let rec conv (f : Mtl.mtl) : mtl =
  let b2 c p q = (conv p, conv q, c) in
  match f with
  | Mtl.True -> MTrue
  | Mtl.False -> MFalse
  | Mtl.Event s -> MAtom (event s)
  | Mtl.NEvent s -> MNotAtom (event s)
  | Mtl.Next p -> MNext (conv p)
  | Mtl.And (p, q) -> MAnd (conv p, conv q)
  | Mtl.Or (p, q) -> MOr (conv p, conv q)
  | Mtl.Until (c, p, q) ->
    let (p, q, c) = b2 c p q in
    (match c with
     | Mtl.Untimed | Mtl.GE 0 -> MU (p, q)
     | Mtl.LE d -> mUle (float d) p q
     | Mtl.LT d -> mUlt (float d) p q
     | Mtl.GE d -> mUge (float d) p q
     | Mtl.GT d -> mUgt (float d) p q)
  | Mtl.Release (c, p, q) ->
    let (p, q, c) = b2 c p q in
    (match c with
     | Mtl.Untimed | Mtl.GE 0 -> MR (p, q)
     | Mtl.LE d -> mRle (float d) p q
     | Mtl.LT d -> mRlt (float d) p q
     | Mtl.GE d -> mRge (float d) p q
     | Mtl.GT d -> mRgt (float d) p q)
  | Mtl.XUntil (c, p, q) ->
    let (p, q, c) = b2 c p q in
    (match c with
     | Mtl.Untimed | Mtl.GE 0 -> MNext (MU (p, q))
     | Mtl.LE d -> MUhatLe (float d, p, q)
     | Mtl.LT d -> MUhatLt (float d, p, q)
     | Mtl.GE d -> MUhatGe (float d, p, q)
     | Mtl.GT d -> MUhatGt (float d, p, q))
  | Mtl.XRelease (c, p, q) ->
    let (p, q, c) = b2 c p q in
    (match c with
     | Mtl.Untimed | Mtl.GE 0 -> MNext (MR (p, q))
     | Mtl.LE d -> MRhatLe (float d, p, q)
     | Mtl.LT d -> MRhatLt (float d, p, q)
     | Mtl.GE d -> MRhatGe (float d, p, q)
     | Mtl.GT d -> MRhatGt (float d, p, q))

(* [well_formed] of the Coq development *)
let rec well_formed = function
  | MTrue | MFalse | MAtom _ | MNotAtom _ -> true
  | MAnd (p, q) | MOr (p, q) | MU (p, q) | MR (p, q) -> well_formed p && well_formed q
  | MNext p -> well_formed p
  | MUhatLe (d, p, q) | MRhatLe (d, p, q) -> d >= 0. && well_formed p && well_formed q
  | MUhatGe (d, p, q) | MRhatGe (d, p, q) | MUhatLt (d, p, q) | MUhatGt (d, p, q)
  | MRhatLt (d, p, q) | MRhatGt (d, p, q) -> d > 0. && well_formed p && well_formed q

(* ------------------------------------------------------------------ *)
(* 3-4. Clocked LTL and Spot                                           *)

let clocks_of root =
  let rec dedup acc = function
    | [] -> List.rev acc
    | x :: l -> if List.mem x acc then dedup acc l else dedup (x :: acc) l in
  dedup [] (timed_subformulas root)

let clock_name clocks x =
  let rec idx i = function
    | [] -> "x?"
    | y :: l -> if y = x then "x" ^ string_of_int i else idx (i + 1) l in
  idx 0 clocks

let atom_name clocks = function
  | LAct a -> Hashtbl.find event_names a
  | LNAct a -> "!" ^ Hashtbl.find event_names a
  | LCLe (x, d) -> Printf.sprintf "%s <= %s" (clock_name clocks x) (Output.num d)
  | LCLt (x, d) -> Printf.sprintf "%s < %s" (clock_name clocks x) (Output.num d)
  | LCGe (x, d) -> Printf.sprintf "%s >= %s" (clock_name clocks x) (Output.num d)
  | LCGt (x, d) -> Printf.sprintf "%s > %s" (clock_name clocks x) (Output.num d)
  | LRst x -> Printf.sprintf "rst(%s)" (clock_name clocks x)
  | LUnch x -> Printf.sprintf "unch(%s)" (clock_name clocks x)

(* Every atom, including the negated event !e, is a quoted proposition for
   Spot: the back-end treats the atoms as independent propositions, as in
   the axiom of the Coq development. *)
let rec spot_formula nm = function
  | LTrue -> "true"
  | LFalse -> "false"
  | LAtom a -> "\"" ^ nm a ^ "\""
  | LAnd (p, q) -> "(" ^ spot_formula nm p ^ " & " ^ spot_formula nm q ^ ")"
  | LOr (p, q) -> "(" ^ spot_formula nm p ^ " | " ^ spot_formula nm q ^ ")"
  | LNext p -> "X(" ^ spot_formula nm p ^ ")"
  | LUntil (p, q) -> "(" ^ spot_formula nm p ^ " U " ^ spot_formula nm q ^ ")"
  | LRelease (p, q) -> "(" ^ spot_formula nm p ^ " R " ^ spot_formula nm q ^ ")"

let read_file f = In_channel.with_open_bin f In_channel.input_all

let run_spot ltl_text =
  let fin = Filename.temp_file "mtl2tba" ".ltl" in
  let fout = Filename.temp_file "mtl2tba" ".lbtt" in
  Out_channel.with_open_bin fin (fun oc -> output_string oc ltl_text);
  let cmd = Printf.sprintf "%s -B -D --lbtt=t -F %s > %s" !spot
      (Filename.quote fin) (Filename.quote fout) in
  log "command: %s" cmd;
  if Sys.command cmd <> 0 then fail "the command %s failed (is Spot installed?)" !spot;
  let text = read_file fout in
  Sys.remove fin; Sys.remove fout;
  text

(* The Spot automaton as a propositional Buchi automaton of the Coq
   development.  With state-based output (-B), the marked transitions are
   the transitions leaving accepting states; otherwise, states are
   duplicated according to the mark of the entering transition. *)
let to_pbuchi (a : Lbtt_read.automaton) (atom_of : string -> latom) : pBuchi =
  let open Lbtt_read in
  let lits c = List.map (fun (s, b) -> (atom_of s, b)) c in
  let srcs = List.sort_uniq compare (List.map (fun t -> t.src) a.trans) in
  let state_based =
    (not a.trans_based) ||
    List.for_all (fun s ->
        let ts = List.filter (fun t -> t.src = s) a.trans in
        List.for_all (fun t -> t.marked) ts || not (List.exists (fun t -> t.marked) ts)) srcs in
  if state_based then
    let acc = if a.trans_based
      then List.sort_uniq compare (List.filter_map (fun t -> if t.marked then Some t.src else None) a.trans)
      else a.accepting in
    { pb_nstates = a.nstates; pb_init = a.init; pb_accepting = acc;
      pb_trans = List.concat_map (fun t ->
          List.map (fun c -> { pt_src = t.src; pt_label = lits c; pt_tgt = t.dst }) t.cubes)
          a.trans }
  else
    let n = a.nstates in
    { pb_nstates = 2 * n; pb_init = a.init; pb_accepting = List.init n (fun s -> s + n);
      pb_trans = List.concat_map (fun t ->
          let dst = if t.marked then t.dst + n else t.dst in
          List.concat_map (fun src ->
              List.map (fun c -> { pt_src = src; pt_label = lits c; pt_tgt = dst }) t.cubes)
            [ t.src; t.src + n ]) a.trans }

(* ------------------------------------------------------------------ *)
(* Main                                                                *)

let size_tba (a : tBA) =
  let locs = Output.reachable a.tba_init (fun s ->
      List.filter_map (fun t -> if t.bt_source = s then Some t.bt_target else None)
        a.tba_transitions) in
  let trs = List.filter (fun t -> List.mem t.bt_source locs) a.tba_transitions in
  let clks = List.sort_uniq compare
      (List.concat_map (fun t ->
           t.bt_resets @ List.filter_map (Option.map (fun k -> k.guard_clock)) t.bt_guard) trs) in
  (List.length locs, List.length trs, List.length clks)

let () =
  Arg.parse speclist (fun s -> formula := s) usage;
  if !formula = "" then (Arg.usage speclist usage; exit 1);
  (* 1-2 *)
  let t_start = Unix.gettimeofday () in
  let f = parse_formula !formula in
  let f = if !with_init then Mtl2mtl.add_init f else f in
  log "formula: %s" (Format.asprintf "%a" Mtl.pp_mtl f);
  List.iter (fun e -> ignore (event e)) (Mtl.get_evts f);
  let root = conv f in
  if not (well_formed root) then
    fail "the formula is not well formed (bounds [<0] and [>0] are not allowed)";
  let clocks = clocks_of root in
  let nm a = atom_name clocks a in
  log "clocks: %s" (String.concat ", " (List.mapi (fun i _ -> "x" ^ string_of_int i) clocks));
  (* 3 *)
  let ltl = t root in
  let ltl_text = spot_formula nm ltl in
  log "clocked LTL: %s" ltl_text;
  let atom_tbl = Hashtbl.create 16 in
  List.iter (fun a -> Hashtbl.replace atom_tbl (nm a) a) (ltl_atoms root ltl);
  let atom_of s = match Hashtbl.find_opt atom_tbl s with
    | Some a -> a | None -> fail "unknown atom %S in the output of Spot" s in
  (* 4 *)
  let t_spot0 = Unix.gettimeofday () in
  let lbtt = Lbtt_read.parse (run_spot ltl_text) in
  let t_spot = Unix.gettimeofday () -. t_spot0 in
  log "Spot: %d states, %d transitions" lbtt.Lbtt_read.nstates
    (List.length lbtt.Lbtt_read.trans);
  let ba = to_pbuchi lbtt atom_of in
  (* 5 *)
  let tba = compile_with root ba in
  let (s0, t0, c0) = size_tba tba in
  log "after reset completion: %d locations, %d transitions" s0 t0;
  (* 6 *)
  (* rounds until nothing changes, at most !rounds *)
  let rec fix k a =
    if k >= !rounds then (k, a)
    else
      let a' = optimize root 1 a in
      if a' = a then (k, a) else fix (k + 1) a' in
  let t_opt0 = Unix.gettimeofday () in
  let (k, opt) = fix 0 tba in
  let t_opt = Unix.gettimeofday () -. t_opt0 in
  let (s1, t1, c1) = size_tba opt in
  log "after %d optimization rounds: %d locations, %d transitions" k s1 t1;
  (* 7 *)
  let t_exp0 = Unix.gettimeofday () in
  let d = export root opt in
  let t_exp = Unix.gettimeofday () -. t_exp0 in
  (* 8 *)
  let names = {
    Output.clock_name = clock_name clocks;
    event_name = (fun a -> Hashtbl.find event_names a);
    events = List.sort compare (Hashtbl.fold (fun _ i l -> i :: l) event_tbl []);
  } in
  let write file pr = Out_channel.with_open_bin file (fun oc -> pr oc) in
  write (!out ^ ".xml") (fun oc -> Output.uppaal_dta oc names d);
  write (!out ^ ".dot") (fun oc -> Output.dot_dta oc names d);
  (* Graphviz: <base>.dot -> <base>.pdf *)
  let to_pdf base =
    let cmd = Printf.sprintf "%s -Tpdf %s -o %s" !dot_cmd
        (Filename.quote (base ^ ".dot")) (Filename.quote (base ^ ".pdf")) in
    log "command: %s" cmd;
    if Sys.command cmd <> 0 then
      prerr_endline ("mtl2tba: warning: the command " ^ !dot_cmd ^ " failed, no pdf produced") in
  if !dump_tba then begin
    write (!out ^ "_tba.dot") (fun oc -> Output.dot_tba oc names tba);
    write (!out ^ "_opt.dot") (fun oc -> Output.dot_tba oc names opt);
    if !pdf then (to_pdf (!out ^ "_tba"); to_pdf (!out ^ "_opt"))
  end;
  if !pdf then to_pdf !out;
  let locs = Output.dta_locations d in
  let dtrs = List.filter (fun t -> List.mem t.dt_src locs) d.dta_trans in
  let ninv = List.length (List.filter (fun s -> Output.invariant_s names " && " d s <> None) locs) in
  let ndiff = List.fold_left (fun n t ->
      n + List.length (List.filter (function DDiff _ -> true | _ -> false) (List.concat t.dt_guard)))
      0 dtrs in
  let nclk = List.length (Output.used_clocks d locs) in
  if !stats then
    print_endline (String.concat "	" (List.map string_of_int
      [ List.length clocks; lbtt.Lbtt_read.nstates;
        List.fold_left (fun n t -> n + List.length t.Lbtt_read.cubes) 0 lbtt.Lbtt_read.trans ]
      @ [ Printf.sprintf "%.3f" t_spot ]
      @ List.map string_of_int [ s0; t0; c0; k; s1; t1; c1;
                                 List.length locs; List.length dtrs; nclk; ninv; ndiff ]
      @ [ Printf.sprintf "%.3f" t_opt; Printf.sprintf "%.3f" t_exp;
          Printf.sprintf "%.3f" (Unix.gettimeofday () -. t_start) ]))
  else
    Printf.printf "%s: %d clocks, %d locations, %d transitions -> %s.xml, %s.dot%s
"
      !formula nclk (List.length locs) (List.length dtrs) !out !out
      (if !pdf then ", " ^ !out ^ ".pdf" else "")
