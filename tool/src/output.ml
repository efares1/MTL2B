(* Printers for the automata computed by the extracted code:
   UPPAAL (XML, flat-1_2 DTD) and Graphviz (dot). *)

open Optim

type names = {
  clock_name : clock -> string;
  event_name : int -> string;
  events : int list;           (* the events of the formula *)
}

let num (b : float) =
  if Float.is_integer b then Printf.sprintf "%.0f" b else Printf.sprintf "%g" b

let cmp_s = function CLe -> "<=" | CLt -> "<" | CGe -> ">=" | CGt -> ">" | CEq -> "=="

let constraint_s nm k =
  Printf.sprintf "%s %s %s" (nm.clock_name k.guard_clock) (cmp_s k.guard_comparison)
    (num k.guard_bound)

let dconstraint_s nm = function
  | DSingle k -> constraint_s nm k
  | DDiff (x, y, b) ->
    Printf.sprintf "%s - %s <= %s" (nm.clock_name x) (nm.clock_name y) (num b)

let conj_s sep = function
  | [] -> "true"
  | l -> String.concat sep l

(* The events allowed by a list of action literals; [None] is the event
   [other], which stands for every event that does not occur in the
   formula. *)
let allowed nm (lab : alit list) : int option list =
  let ok e = List.for_all (fun (a, b) -> if b then e = Some a else e <> Some a) lab in
  List.filter ok (List.map (fun a -> Some a) nm.events @ [ None ])

let event_s nm = function Some a -> nm.event_name a | None -> "other"

let label_s nm (lab : alit list) =
  match allowed nm lab with
  | l when List.length l = List.length nm.events + 1 -> "any"
  | l -> String.concat "," (List.map (event_s nm) l)

(* Locations reachable from the initial one. *)
let reachable init succs =
  let seen = Hashtbl.create 16 in
  let rec go s =
    if not (Hashtbl.mem seen s) then (Hashtbl.add seen s (); List.iter go (succs s)) in
  go init;
  List.sort compare (Hashtbl.fold (fun s () l -> s :: l) seen [])

let dta_locations (d : dTA) =
  reachable d.dta_init (fun s ->
      List.filter_map (fun t -> if t.dt_src = s then Some t.dt_tgt else None) d.dta_trans)

let invariant_s nm sep (d : dTA) s =
  match d.dta_inv s with
  | Some [ u ] when u <> [] ->
    Some (String.concat sep
            (List.map (fun (x, b) -> Printf.sprintf "%s <= %s" (nm.clock_name x) (num b)) u))
  | _ -> None

let used_clocks (d : dTA) locs =
  let trs = List.filter (fun t -> List.mem t.dt_src locs) d.dta_trans in
  let of_c = function DSingle k -> [ k.guard_clock ] | DDiff (x, y, _) -> [ x; y ] in
  let inv s = match d.dta_inv s with
    | Some us -> List.concat_map (List.map fst) us | None -> [] in
  List.sort_uniq compare
    (List.concat_map (fun t -> t.dt_resets @ List.concat_map of_c (List.concat t.dt_guard)) trs
     @ List.concat_map inv locs)

(* ------------------------------------------------------------------ *)
(* Graphviz                                                            *)

let dot_dta oc nm (d : dTA) =
  let locs = dta_locations d in
  Printf.fprintf oc "digraph TA {\n  rankdir=LR;\n  node [shape=circle];\n";
  Printf.fprintf oc "  init [shape=point];\n  init -> L%d;\n" d.dta_init;
  List.iter (fun s ->
      let shape = if List.mem s d.dta_accepting then "doublecircle" else "circle" in
      let lab = match invariant_s nm " && " d s with
        | Some i -> Printf.sprintf "L%d\\n%s" s i | None -> Printf.sprintf "L%d" s in
      Printf.fprintf oc "  L%d [shape=%s, label=\"%s\"];\n" s shape lab) locs;
  List.iter (fun t ->
      if List.mem t.dt_src locs then
        List.iter (fun c ->
            let g = conj_s " && " (List.map (dconstraint_s nm) c) in
            let r = match t.dt_resets with
              | [] -> "" | z -> "\\n" ^ String.concat ", " (List.map nm.clock_name z) ^ " := 0" in
            Printf.fprintf oc "  L%d -> L%d [label=\"%s%s%s\"];\n" t.dt_src t.dt_tgt
              (if g = "true" then "" else g ^ "\\n") (label_s nm t.dt_label) r)
          t.dt_guard) d.dta_trans;
  Printf.fprintf oc "}\n"

let dot_tba oc nm (a : tBA) =
  let locs = reachable a.tba_init (fun s ->
      List.filter_map (fun t -> if t.bt_source = s then Some t.bt_target else None)
        a.tba_transitions) in
  Printf.fprintf oc "digraph TBA {\n  rankdir=LR;\n  node [shape=circle];\n";
  Printf.fprintf oc "  init [shape=point];\n  init -> L%d;\n" a.tba_init;
  List.iter (fun s ->
      let shape = if List.mem s a.tba_accepting then "doublecircle" else "circle" in
      Printf.fprintf oc "  L%d [shape=%s, label=\"L%d\"];\n" s shape s) locs;
  List.iter (fun t ->
      if List.mem t.bt_source locs then begin
        let g = conj_s " && "
            (List.filter_map (Option.map (constraint_s nm)) t.bt_guard) in
        let r = match t.bt_resets with
          | [] -> "" | z -> "\\n" ^ String.concat ", " (List.map nm.clock_name z) ^ " := 0" in
        Printf.fprintf oc "  L%d -> L%d [label=\"%s%s%s\"];\n" t.bt_source t.bt_target
          (if g = "true" then "" else g ^ "\\n") (label_s nm t.bt_label) r
      end) a.tba_transitions;
  Printf.fprintf oc "}\n"

(* ------------------------------------------------------------------ *)
(* UPPAAL                                                              *)

let xml_escape s =
  let b = Buffer.create (String.length s) in
  String.iter (function
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '&' -> Buffer.add_string b "&amp;"
      | c -> Buffer.add_char b c) s;
  Buffer.contents b

(* A template [Property] with the automaton, receiving the events on
   channels, and a template [Env] that can emit every event at any time.
   Accepting locations are named A<n>, the others L<n>; UPPAAL does not
   check Buchi acceptance. *)
let uppaal_dta oc nm (d : dTA) =
  let locs = dta_locations d in
  let clocks = used_clocks d locs in
  let chans = List.map (fun a -> event_s nm (Some a)) nm.events @ [ "other" ] in
  let p fmt = Printf.fprintf oc fmt in
  p "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
  p "<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' \
     'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>\n";
  p "<nta>\n  <declaration>chan %s;</declaration>\n" (String.concat ", " chans);
  p "  <template>\n    <name>Property</name>\n";
  if clocks <> [] then
    p "    <declaration>clock %s;</declaration>\n"
      (String.concat ", " (List.map nm.clock_name clocks));
  let lname s = Printf.sprintf "%s%d" (if List.mem s d.dta_accepting then "A" else "L") s in
  List.iteri (fun k s ->
      p "    <location id=\"id%d\" x=\"%d\" y=\"%d\">\n      <name>%s</name>\n"
        s (200 * (k mod 5)) (150 * (k / 5)) (lname s);
      (match invariant_s nm " && " d s with
       | Some i -> p "      <label kind=\"invariant\">%s</label>\n" (xml_escape i)
       | None -> ());
      p "    </location>\n") locs;
  p "    <init ref=\"id%d\"/>\n" d.dta_init;
  List.iter (fun t ->
      if List.mem t.dt_src locs then
        List.iter (fun c ->
            List.iter (fun e ->
                p "    <transition>\n      <source ref=\"id%d\"/>\n      <target ref=\"id%d\"/>\n"
                  t.dt_src t.dt_tgt;
                if c <> [] then
                  p "      <label kind=\"guard\">%s</label>\n"
                    (xml_escape (String.concat " && " (List.map (dconstraint_s nm) c)));
                p "      <label kind=\"synchronisation\">%s?</label>\n" (event_s nm e);
                if t.dt_resets <> [] then
                  p "      <label kind=\"assignment\">%s</label>\n"
                    (String.concat ", " (List.map (fun x -> nm.clock_name x ^ " = 0") t.dt_resets));
                p "    </transition>\n")
              (allowed nm t.dt_label))
          t.dt_guard) d.dta_trans;
  p "  </template>\n";
  p "  <template>\n    <name>Env</name>\n    <location id=\"env0\" x=\"0\" y=\"0\"/>\n";
  p "    <init ref=\"env0\"/>\n";
  List.iter (fun c ->
      p "    <transition>\n      <source ref=\"env0\"/>\n      <target ref=\"env0\"/>\n";
      p "      <label kind=\"synchronisation\">%s!</label>\n    </transition>\n" c) chans;
  p "  </template>\n";
  p "  <system>P = Property();\nE = Env();\nsystem P, E;</system>\n</nta>\n"
