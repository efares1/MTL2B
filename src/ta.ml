type op = LE | LT | GE | GT
type ctr =
  True
| False
| Clk of string * op * int
| And of ctr*ctr
| Or of ctr*ctr

let rm_eq = function
    Clk(x,LE,d) -> Clk(x,LT, d+1)
  | Clk(x,GE,d) -> Clk(x,GT, d-1)
  | c -> c

let contradict c1 c2 =
  match rm_eq c1, rm_eq c2 with
    Clk(x,LT,d1), Clk(x',GT,d2) -> x=x' && d1 <= d2+1
  | Clk(x,GT,d1), Clk(x',LT,d2) -> x=x' && d2 <= d1+1
  | False, _ -> true
  | _, False -> true
  | _ -> false

let rec weaker c1 c2 =
  match rm_eq c1, rm_eq c2 with
    Clk(x,LT,d1), Clk(x',LT,d2) -> x=x' && d1 >= d2
  | Clk(x,GT,d1), Clk(x',GT,d2) -> x=x' && d2 >= d1
  | True, _ -> true
  | _, False -> true
  | c1, And(c21, c22) -> weaker c1 c21 || weaker c1 c22
  | And(c11,c12), c2 -> weaker c11 c2 && weaker c12 c2
  | c1,c2 -> c1=c2

let mk_and c1 c2 =
  if c1=c2 then c1
  else if contradict c1 c2 then False
  else if weaker c1 c2 then c2
  else if weaker c2 c1 then c1
  else And(c1,c2)

let rec mk_andl = function
    [] -> True
  | x::l -> mk_and x (mk_andl l)

let mk_or c1 c2 =
  if c1=c2 then c1
  else if weaker c1 c2 then c1
  else if weaker c2 c1 then c2
  else Or(c1,c2)

let rec mk_orl = function
    [] -> False
  | x::l -> mk_or x (mk_orl l)

type transition = {
    src: int;
    dst: int;
    mark: int list;
    event:string;
    guard: ctr;
    reset: string list}

type automaton = {
    nbStates:int;
    init: int list;
    typ:string;
    events: string list;
    clks: string list;
    invs: (int*ctr) list;
    trans: transition list;
  }

(**** merge synchronous clocks ****)

let same_clks_tr c1 c2 tr =
  List.mem c1 tr.reset = List.mem c2 tr.reset

let same_clks c1 c2 a =
  List.for_all (same_clks_tr c1 c2) a.trans

let dup_clks a =
  let rec dup_clks dups c =
    match dups with
      [] -> [[c]]
    | (c'::_) as p::lc when same_clks c c' a -> (c::p)::lc
    | p::lc -> p::dup_clks lc c in
  List.fold_left dup_clks [] a.clks

let rec rename_dups_clk d x =
  match d with
    [] -> x
  | cls::tl -> if List.mem x cls then List.hd cls else rename_dups_clk tl x

let rec rename_dups_ctr dups = function
    True -> True
  | False -> False
  | Clk(x,op,d) -> Clk(rename_dups_clk dups x,op,d)
  | And(c1,c2) -> mk_and (rename_dups_ctr dups c1) (rename_dups_ctr dups c2)
  | Or(c1,c2) -> mk_or (rename_dups_ctr dups c1) (rename_dups_ctr dups c2)

let rename_dups_tr d tr =
  {
    src = tr.src;
    dst = tr.dst;
    mark = tr.mark;
    event = tr.event;
    guard = rename_dups_ctr d tr.guard;
    reset = List.sort_uniq compare (List.map (rename_dups_clk d) tr.reset)
  }

let rem_sync_clks a =
  let d = dup_clks a in
  let to_remove = List.concat_map List.tl d in
  let _ = to_remove in
  {
    nbStates = a.nbStates;
    init = a.init;
    typ = a.typ;
    events = a.events;
    clks = List.filter (fun c -> not (List.mem c to_remove)) a.clks;
    invs = List.map (fun (i,p) -> (i, rename_dups_ctr d p)) a.invs;
    trans = List.filter (fun tr->tr.guard<>False) (List.map (rename_dups_tr d) a.trans)
  }

(***************** unused resets ***************)

module IntSet = Set.Make (struct type t = int let compare = compare end)

let reachable a st =
  let rec reachable st seen =
    let trs = List.filter (fun t -> t.src = st) a.trans in
    let dsts = List.sort_uniq compare (List.map (fun t -> t.dst) trs) in
    let newst = List.filter (fun d -> not (List.mem d seen)) dsts in
    let seen = seen @ newst in
    List.fold_left (fun r d -> reachable d r) seen newst in
  reachable st [st]

let rec get_clocks = function
    True -> []
  | False -> []
  | Clk(x,_,_) -> [x]
  | And(c1,c2) -> List.sort_uniq compare (get_clocks c1 @ get_clocks c2)
  | Or(c1,c2) -> List.sort_uniq compare (get_clocks c1 @ get_clocks c2)

let rem_unused_resets a =
  let ntrs =
    List.map (fun tr ->
        let st = tr.dst in
        let sts = reachable a st in
        let pl =
          (List.map (fun t -> t.guard) (List.filter (fun t -> t.src=st) a.trans)) @
            (List.map snd (List.filter (fun (i,_) -> List.mem i sts) a.invs)) in
        let used = List.sort_uniq compare (List.concat_map get_clocks pl) in
        {tr with reset = List.filter (fun x -> List.mem x used) tr.reset})
      a.trans in
  {a with trans = ntrs}

(***************** B printer ***************)

let ppb_op oc = function
    LE -> Printf.fprintf oc "<="
  | LT -> Printf.fprintf oc "<"
  | GE -> Printf.fprintf oc ">="
  | GT -> Printf.fprintf oc ">"

let rec incr_clks = function
    True -> True
  | False -> False
  | Clk(x,op,d) -> Clk(x,op,d-1)
  | And(c1,c2) -> And(incr_clks c1, incr_clks c2)
  | Or(c1,c2) -> Or(incr_clks c1, incr_clks c2)

let rec ppb_ctr oc = function
    True -> Printf.fprintf oc "true"
  | False -> Printf.fprintf oc "false"
  | Clk(x,op,d) -> Printf.fprintf oc "%s %a %d" x ppb_op op d
  | And(c1,c2) -> Printf.fprintf oc "%a & %a" ppb_ctr c1 ppb_ctr c2
  | Or(c1,c2) -> Printf.fprintf oc "%a or %a" ppb_ctr c1 ppb_ctr c2

let rec ppb_mark = function
    [] -> "no"
  | [x] -> string_of_int x
  | x::l -> string_of_int x ^","^(ppb_mark l)

let ppb_tr oc trl ev =
  let trs = List.filter (fun t -> t.event = ev) trl in
  Printf.fprintf oc "  event_%s =\n" ev;
  let kw = ref "SELECT" in
  List.iter (fun t ->
      Printf.fprintf oc "  %s " !kw;
      kw := "WHEN";
      if t.guard=True then
        Printf.fprintf oc "st=%d" t.src
      else
        Printf.fprintf oc "st=%d & %a" t.src ppb_ctr t.guard;
      Printf.fprintf oc " THEN %% rec: %s\n" (ppb_mark t.mark);
      if (t.src<>t.dst || t.reset<>[]) then (
        let parkw = ref "  " in
        if (t.src<>t.dst) then (
          Printf.fprintf oc "     st := %d\n" t.dst;
          parkw := "||"
        );
        List.iter (fun x ->
            Printf.fprintf oc "  %s %s := 0\n" !parkw x;
            parkw := "||"
          ) t.reset
      ) else
        Printf.fprintf oc "    skip\n"
    ) trs;
  Printf.fprintf oc "  END;\n"


let ppb_auto oc a =
  Printf.fprintf oc "MACHINE auto_%s\n" a.typ;
  Printf.fprintf oc "VARIABLES st, %s\n" (String.concat ", " a.clks);
  Printf.fprintf oc "INVARIANT\n";
  Printf.fprintf oc "  st : 1..%d\n" a.nbStates;
  List.iter (fun x -> Printf.fprintf oc "& %s : NAT\n" x) a.clks;
  List.iter (fun (s,g) ->
      Printf.fprintf oc "& (st=%d => %a)\n" s ppb_ctr g
    ) a.invs;
  Printf.fprintf oc "INITIALISATION\n";
  Printf.fprintf oc "  BEGIN\n";
  if List.length a.init = 1 then
    Printf.fprintf oc "     st := %d\n" (List.hd a.init)
  else
    Printf.fprintf oc "     st :: {%s}\n"
      (String.concat "," (List.map string_of_int a.init));
  List.iter (fun x -> Printf.fprintf oc "  || %s := 0\n" x) a.clks;
  Printf.fprintf oc "  END\n";
  Printf.fprintf oc "OPERATIONS\n";
  List.iter (ppb_tr oc a.trans) a.events;
  Printf.fprintf oc "  tick =\n";
  if a.invs <> [] then (
    Printf.fprintf oc "  WHEN\n";
    let kand = ref " " in
    List.iter (fun (s,g) ->
        Printf.fprintf oc "  %s (st=%d => %a)\n" !kand s ppb_ctr (incr_clks g);
        kand := "&"
      ) a.invs;
    Printf.fprintf oc "  THEN\n";
  ) else
    Printf.fprintf oc "  BEGIN\n";
  let kp = ref "  " in
  List.iter (fun x ->
      Printf.fprintf oc "  %s %s := %s+1\n" !kp x x;
      kp := "||"
    ) a.clks;
  Printf.fprintf oc "  END\n";
  Printf.fprintf oc "END;\n"

(***************** EVB printer ***************)

let ppevb_op oc = function
    LE -> Printf.fprintf oc "≤"
  | LT -> Printf.fprintf oc "<"
  | GE -> Printf.fprintf oc "≥"
  | GT -> Printf.fprintf oc ">"

let rec ppevb_ctr oc = function
    True -> Printf.fprintf oc "⊤"
  | False -> Printf.fprintf oc "⊥"
  | Clk(x,op,d) -> Printf.fprintf oc "%s %a %d" x ppevb_op op d
  | And(c1,c2) -> Printf.fprintf oc "%a ∧ %a" ppevb_ctr c1 ppevb_ctr c2
  | Or(c1,c2) -> Printf.fprintf oc "%a ∨ %a" ppevb_ctr c1 ppevb_ctr c2

let ppevb_tr oc trl ev =
  let trs = List.filter (fun t -> t.event = ev) trl in
  let branch = ref 0 in
  List.iter (fun t ->
      incr branch;
      let ant = (if t.mark=[] then "anticipated " else "") in
      Printf.fprintf oc "  %sevent event_%s%d\n" ant ev !branch;
      Printf.fprintf oc "  when\n";
      Printf.fprintf oc "    @st st = %d\n" t.src;
      if t.guard<>True then
        Printf.fprintf oc "    @g %a\n" ppevb_ctr t.guard;
      if (t.src<>t.dst || t.reset<>[]) then (
        Printf.fprintf oc "  then\n";
        if (t.src<>t.dst) then (
          Printf.fprintf oc "    @nst st ≔ %d\n" t.dst
        );
        List.iter (fun x ->
            Printf.fprintf oc "    @%s %s ≔ 0\n" x x
          ) t.reset
      );
      Printf.fprintf oc "  end\n"
    ) trs

let ppevb_auto oc a =
  Printf.fprintf oc "machine auto_%s\n" a.typ;
  Printf.fprintf oc "variables st %s\n" (String.concat " " a.clks);
  Printf.fprintf oc "invariants\n";
  Printf.fprintf oc "  @st st ∈ 1 ‥ %d\n" a.nbStates;
  List.iter (fun x -> Printf.fprintf oc "  @%s %s ∈ ℕ\n" x x) a.clks;
  List.iter (fun (s,g) ->
      Printf.fprintf oc "  @inv%d (st=%d ⇒ %a)\n" s s ppevb_ctr g
    ) a.invs;
  Printf.fprintf oc "events\n";
  Printf.fprintf oc "  event INITIALISATION\n";
  Printf.fprintf oc "  then\n";
  if List.length a.init = 1 then
    Printf.fprintf oc "    @st st ≔ %d\n" (List.hd a.init)
  else
    Printf.fprintf oc "    @st st :∈ {%s}\n"
      (String.concat "," (List.map string_of_int a.init));
  List.iter (fun x -> Printf.fprintf oc "    @%s %s ≔ 0\n" x x) a.clks;
  Printf.fprintf oc "  end\n";
  List.iter (ppevb_tr oc a.trans) a.events;
  Printf.fprintf oc "  event tick\n";
  if a.invs <> [] then (
    Printf.fprintf oc "  when\n";
    List.iter (fun (s,g) ->
        Printf.fprintf oc "    @ist%d (st=%d ⇒ %a)\n" s s ppevb_ctr (incr_clks g)
      ) a.invs
  );
  Printf.fprintf oc "  then\n";
  List.iter (fun x ->
      Printf.fprintf oc "    @n%s %s ≔ %s+1\n" x x x
    ) a.clks;
  Printf.fprintf oc "  end\n";
  Printf.fprintf oc "end\n"

(*********** dot printer *************)

let pp_op = function
    LE -> "<="
  | LT -> "<"
  | GE -> ">="
  | GT -> ">"

let rec pp_ctr oc = function
    True -> Printf.fprintf oc "true"
  | False -> Printf.fprintf oc "false"
  | Clk(x,op,d) -> Printf.fprintf oc "%s%s%d" x (pp_op op) d
  | And(c1,c2) -> Printf.fprintf oc "%a && %a" pp_ctr c1 pp_ctr c2
  | Or(c1,c2) -> Printf.fprintf oc "%a || %a" pp_ctr c1 pp_ctr c2

let pp_ctr_nt oc g = if g<>True then Printf.fprintf oc "%a\n" pp_ctr g

let ppd_auto_tr oc s tr =
  let color = if tr.mark=[] then "black" else "red" in
  if tr.reset=[] then
    Printf.fprintf oc "  %d -> %d [color=%s,label=\"%a%s\"]\n" s tr.dst color pp_ctr_nt tr.guard tr.event
  else
    Printf.fprintf oc "  %d -> %d [color=%s,label=\"%a%s/rst %s\"]\n" s tr.dst color pp_ctr_nt tr.guard tr.event (String.concat "," tr.reset)

let ppd_auto_st oc st a =
  let inv = try List.assoc st a.invs with Not_found -> True in
  if inv = True then
    Printf.fprintf oc "  %d [label=\"%d\"]\n" st st
  else
    Printf.fprintf oc "  %d [label=\"%d\n[%a]\"]\n" st st pp_ctr inv;
  List.iter (ppd_auto_tr oc st) (List.filter (fun tr->tr.src=st) a.trans)

let ppd_auto oc a =
  Printf.fprintf oc "digraph \"TA\" {\n";
  Printf.fprintf oc "  rankdir=LR\n";
  Printf.fprintf oc "  label=\"%s\"\n" a.typ;
  Printf.fprintf oc "  labelloc=\"t\"\n";
  Printf.fprintf oc "  node [shape=\"circle\"]\n";
  List.iter (fun i ->
      Printf.fprintf oc "  I%d[label=\"\", style=invis, width=0]\n" i;
      Printf.fprintf oc "  I%d -> %d\n" i i
    ) a.init;
  for st = 1 to a.nbStates do
    ppd_auto_st oc st a
  done;
  Printf.fprintf oc "}\n"

(*********** xta printer *************)

let ppx_op = function
    LE -> "<="
  | LT -> "<"
  | GE -> ">="
  | GT -> ">"

let rec ppx_ctr oc = function
    True -> Format.fprintf oc "true"
  | False -> Format.fprintf oc "false"
  | Clk(x,op,d) -> Format.fprintf oc "%s%s%d" x (ppx_op op) d
  | And(c1,c2) -> Format.fprintf oc "%a && %a" ppx_ctr c1 ppx_ctr c2
  | Or(c1,c2) -> Format.fprintf oc "%a || %a" ppx_ctr c1 ppx_ctr c2

let ppx_ctr_nt oc g = if g<>True then Format.fprintf oc "%a\n" ppx_ctr g

let ppx_auto_tr oc prefix last tr =
  Format.fprintf oc "  L%d -> L%d {" tr.src tr.dst;
  if tr.guard <> True then Format.fprintf oc " guard %a;" ppx_ctr tr.guard;
  Format.fprintf oc " sync %s%s?;" prefix tr.event;
  if tr.reset<>[] then (
    let assigns = String.concat "," (List.map (fun x -> x^":=0") tr.reset) in
    Format.fprintf oc " assign %s;" assigns
  );
  Format.fprintf oc (if last then " };\n" else " },\n")

let ppx_auto ?raw name oc a =
  if raw=None && a.events<>[] then
    Format.fprintf oc "chan %s;\n" (String.concat "," a.events);
  Format.fprintf oc "process %s() {\n" name;
  if a.clks<>[] then
    Format.fprintf oc "clock %s;\n" (String.concat "," a.clks);
  Format.fprintf oc "state\n";
  for i = 1 to a.nbStates do
    (match List.assoc_opt i a.invs with
      None | Some True -> Format.fprintf oc "  L%d" i
    | Some inv -> Format.fprintf oc "  L%d { %a }" i ppx_ctr inv);
    if i=a.nbStates then
      Format.fprintf oc ";\n"
    else
      Format.fprintf oc ",\n"
  done;
  Format.fprintf oc "init\n  L%s;\n"
    (String.concat ", L" (List.map string_of_int a.init));
  Format.fprintf oc "trans\n";
  let nbt = List.length a.trans in
  let prefix = if raw<>None && name<>"" then name^"_" else "" in
  List.iteri (fun i -> ppx_auto_tr oc prefix (i=nbt-1)) a.trans;
  Format.fprintf oc "}\n";
  if raw=None then (
    Format.fprintf oc "process Synchro() {\n";
    Format.fprintf oc "state\n  L0;\n";
    Format.fprintf oc "init\n  L0;\n";
    Format.fprintf oc "trans\n";
    let nbc = List.length a.events in
    List.iteri (fun i c ->
        Format.fprintf oc "  L0 -> L0 { sync %s!; }%s\n"
          c (if i < nbc-1 then "," else ";")
      ) a.events;
    Format.fprintf oc "}\n"
  );
  if raw=None then (
    Format.fprintf oc "%s = %s();\n" name name;
    Format.fprintf oc "synchro = Synchro();\n";
    Format.fprintf oc "system %s, synchro;\n" name
  )


(***************** EVB product printer ***************)

let ppp_auto oc nm al =
  let evts = List.sort_uniq compare (List.concat_map (fun (_,a) -> a.events) al) in
  Format.fprintf oc "machine %s\n" nm;
  List.iter (fun (n,_) -> Format.fprintf oc "  includes %s.%s\n" n n) al;
  Format.fprintf oc "events\n";
  Format.fprintf oc "  event INITIALISATION\n";
  List.iter (fun (n,_) -> Format.fprintf oc "    synchronizes %s.INITIALISATION\n" n) al;
  Format.fprintf oc "  end\n";
  List.iter (fun ev ->
      Format.fprintf oc "  event %s\n" ev;
      List.iter (fun s ->
          Format.fprintf oc "    synchronizes %s.%s\n" s ev
        ) (List.map fst (List.filter (fun (_,a) -> List.mem ev a.events) al));
      Format.fprintf oc "  end\n") evts;
  Format.fprintf oc "end\n"
