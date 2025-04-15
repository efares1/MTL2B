type op = LE | LT | GE | GT
let op_of_string = function
    "<=" -> LE
  | "<" -> LT
  | ">=" -> GE
  | ">" -> GT
  | s -> failwith ("op_of_string:"^s)

let mk_opnot = function
    LE -> GT
  | LT -> GE
  | GE -> LT
  | GT -> LE

type ctr =
  True
| False
| Atom of string
| NAtom of string
| Reset of string
| Unch of string
| Clk of string * op * int
| And of ctr*ctr
| Or of ctr*ctr

let rec mk_not = function
  True -> False
| False -> True
| Atom s -> NAtom s
| NAtom s -> Atom s
| Reset x -> Unch x
| Unch x -> Reset x
| Clk(x,o,d)-> Clk(x, mk_opnot o, d)
| And(c1,c2) -> Or(mk_not c1, mk_not c2)
| Or(c1,c2) -> And(mk_not c1, mk_not c2)

let mk_imp c1 c2 = Or(mk_not c1,c2)
let mk_eqv c1 c2 = And(mk_imp c1 c2, mk_imp c2 c1)
let mk_excl c1 c2 = mk_not (mk_eqv c1 c2)

type transition = int * int list * ctr * string list
type state = int * bool * transition list
type automaton = int * string * state list

(** flatten and simplify and-constraints **)
let rec flatten_and = function
    And(c1,c2) -> flatten_and c1 @ flatten_and c2
  | c -> [c]

let rm_eq = function
    Clk(x,LE,d) -> Clk(x,LT, d+1)
  | Clk(x,GE,d) -> Clk(x,GT, d-1)
  | c -> c

let contradict c1 c2 =
  match rm_eq c1, rm_eq c2 with
    Clk(x,LT,d1), Clk(x',GT,d2) -> x=x' && d1 <= d2+1
  | Clk(x,GT,d1), Clk(x',LT,d2) -> x=x' && d2 <= d1+1
  | Atom s, Atom s' -> s <> s'
  | False, _ -> true
  | _, False -> true
  | Reset x, Unch x' -> x=x'
  | Unch x, Reset x' -> x=x'
  | _ -> false

let rec weaker c1 c2 =
  match rm_eq c1, rm_eq c2 with
    Clk(x,LT,d1), Clk(x',LT,d2) -> x=x' && d1 >= d2
  | Clk(x,GT,d1), Clk(x',GT,d2) -> x=x' && d2 >= d1
  | NAtom s, Atom s' -> s <> s'
  | True, _ -> true
  | _, False -> true
  | c1, And(c21, c22) -> weaker c1 c21 || weaker c1 c22
  | And(c11,c12), c2 -> weaker c11 c2 && weaker c12 c2
  | c1,c2 -> c1=c2

(* no And, Or *)
let insert_atom c lc =
  if lc=[False] then lc
  else if (List.exists (contradict c) lc) then [False]
  else if (List.exists (fun c' -> weaker c c') lc) then lc
  else c::List.filter (fun c' -> not (weaker c' c)) lc

let rec simplify_and = function
    [] -> []
  | c::lc -> insert_atom c (simplify_and lc)

let rec mk_land = function
    [] -> True
  | [x] -> x
  | x::l -> And(x,mk_land l)

let mk_and c1 c2 =
  mk_land (simplify_and (flatten_and c1 @ flatten_and c2))

(** replaces or-constraints by multiple transitions **)
let rec rm_or_c = function
  | Or(c1,c2) -> rm_or_c c1 @ rm_or_c c2
  | And(c1,c2) ->
     List.concat_map
       (fun c -> List.map (fun c' -> mk_and c c') (rm_or_c c2))
       (rm_or_c c1)
  | c -> [c]

let rm_or_tr (d,m,c,a) =
  (List.map (fun c -> (d,m,c,a))
     (List.filter (fun c -> c<>False) (rm_or_c c)))
let rm_or_st (s,i,tl) = (s,i, List.flatten (List.map rm_or_tr tl))
let rm_or (n,t,sl) = (n,t,List.map rm_or_st sl)

(** add reset, remove unch **)

let rec rem_rsts = function
    Reset(x) -> [x], True
  | And(Reset(x),c) -> let (r,c') = rem_rsts c in (x::r),c'
  | And(c1,c2) ->
     let (r,c') = rem_rsts c2 in r, (if c'=True then c1 else And(c1,c'))
  | c -> [],c

let rec rem_unch = function
    Unch(x) -> [x], True
  | And(Unch(x),c) -> let (r,c') = rem_unch c in (x::r),c'
  | And(c1,c2) ->
     let (r,c') = rem_unch c2 in r, (if c'=True then c1 else And(c1,c'))
  | c -> [],c

(* manque la liste des horloges xri de l'automate *)
(* ajouter a reset celles qui ne sont pas dans u *)
let rec get_clk_c = function
  | Reset x -> [x]
  | Unch x -> [x]
  | Clk(x,_,_) -> [x]
  | And(c1,c2) -> List.sort_uniq compare ((get_clk_c c1) @ (get_clk_c c2))
  | Or(c1,c2) -> List.sort_uniq compare ((get_clk_c c1) @ (get_clk_c c2))
  | _ -> []

let get_clk_tr (_,_,c,_) = get_clk_c c
let get_clk_st (_,_,tl) = List.flatten (List.map get_clk_tr tl)
let get_clk (_,_,sl) =
  List.sort_uniq compare (List.flatten (List.map get_clk_st sl))

let get_rst a = List.filter (fun x -> String.get x 1 = 'r') (get_clk a)

let add_rst_tr xr (d,m,c,_) =
  let (r,c1) = rem_rsts c in
  let (u,c2) = rem_unch c1 in
  let nr = List.filter (fun c -> not (List.mem c u)) xr in
  (d,m,c2,List.sort_uniq compare (r@nr))
let add_rst_st xr (s,i,tl) = (s,i, List.map (add_rst_tr xr) tl)
let add_rst ((n,t,sl) as a) =
  let xr = get_rst a in
  (n,t,List.map (add_rst_st xr) sl)

(** event-based access **)

let rec get_evts_ctr = function
  | Atom s |  NAtom s -> [s]
  | And(c1,c2) | Or(c1,c2) ->
     List.sort_uniq compare (get_evts_ctr c1 @ get_evts_ctr c2)
  | _ -> []

let get_evts_tr (_,_,c,_) =
  get_evts_ctr c
let get_evts_st (_,_,tl) =
  List.sort_uniq compare (List.concat_map get_evts_tr tl)
let get_evts (_,_,sl) =
  List.sort_uniq compare (List.concat_map get_evts_st sl)

let rec check_ev ev = function
    True -> True
  | False -> raise Not_found
  | Atom s -> if (s=ev) then True else raise Not_found
  | NAtom s -> if (s=ev) then raise Not_found else True
  | And(c1,c2) -> mk_and (check_ev ev c1) (check_ev ev c2)
  | Or(_,_) -> failwith "check_ev"
  | c -> c

let get_trs_tr s ev (d,m,c,a) =
  try let c' = check_ev ev c in [s,d,m,c',a]
  with Not_found -> []
let get_trs_st ev (s,_,tl) =
  List.concat_map (get_trs_tr s ev) tl
let get_trs (_,_,stl) ev =
  List.concat_map (get_trs_st ev) stl

let get_init (_,_,stl) =
  let (init,_,_) = List.find (fun (_,i,_) -> i) stl in
  init

(** synthetize invariant **)

let mk_or c1 c2 =
  if c1=c2 then c1
  else if weaker c1 c2 then c1
  else if weaker c2 c1 then c2
  else Or(c1,c2)

let rec mk_orl = function
    [] -> False
  | x::l -> mk_or x (mk_orl l)

let rec rm_gt = function
  | And(c1,c2) -> mk_and (rm_gt c1) (rm_gt c2)
  | Or(c1,c2) -> mk_or (rm_gt c1) (rm_gt c2)
  | Clk(_,GT, _) -> True
  | Clk(_,GE, _) -> True
  | Atom _ -> True
  | NAtom _ -> True
  | c -> c

let rec rewrite_hyp h c =
  if weaker c h then True else
    match c with
      And(c1,c2) -> mk_and (rewrite_hyp h c1) (rewrite_hyp h c2)
    | Or(c1,c2) -> mk_or (rewrite_hyp h c1) (rewrite_hyp h c2)
    | c -> c

let rec incr_clks = function
  | And(c1,c2) -> And(incr_clks c1, incr_clks c2)
  | Or(c1,c2) -> Or (incr_clks c1, incr_clks c2)
  | Clk(_,GT, _) -> failwith "incr_clks"
  | Clk(_,GE, _) -> failwith "incr_clks"
  | Clk(x,LT, d) -> Clk(x,LT,d-1)
  | Clk(x,LE, d) -> Clk(x,LE,d-1)
  | c -> c

let get_inv_tr (_,_,g,_) = rm_gt g

let get_inv_st (s,_,tl) =
  (s, mk_orl (List.map get_inv_tr tl))

let get_inv (_,_,stl) =
  List.filter (fun (_,g) -> g<>True) (List.map get_inv_st stl)

(** combine transitions **)

let rec combine_sg s g = function
    [] -> [s,g]
  | (s',g')::lt when s = s' ->
     if weaker g g' then (s,g)::lt
     else if weaker g' g then (s,g')::lt
     else (s',g')::combine_sg s g lt
  | sg::lt -> sg::combine_sg s g lt

let rec combine_tr_in ((s,d,m,g,a) as t) = function
    [] -> [(d,a,m),[s,g]]
  | ((d',a',m') as k,lt)::trs when d=d' && a=a' && m=m' ->
     (k, combine_sg s g lt)::trs
  | t'::trs -> t'::combine_tr_in t trs

let rec combine_trs = function
    [] -> []
  | t::trs -> combine_tr_in t (combine_trs trs)

(** pretty print automaton **)

let pp_op oc = function
    LE -> Printf.fprintf oc "<="
  | LT -> Printf.fprintf oc "<"
  | GE -> Printf.fprintf oc ">="
  | GT -> Printf.fprintf oc ">"

let rec pp_ctr oc = function
  True -> Printf.fprintf oc "True"
| False -> Printf.fprintf oc "False"
| Atom s -> Printf.fprintf oc "%s" s
| NAtom s -> Printf.fprintf oc "!%s" s
| Reset x -> Printf.fprintf oc "rst(%s)" x
| Unch x -> Printf.fprintf oc "unch(%s)" x
| Clk(x,o,d)-> Printf.fprintf oc "%s %a %d" x pp_op o d
| And(c1,c2) -> Printf.fprintf oc "(%a & %a)" pp_ctr c1 pp_ctr c2
| Or(c1,c2) -> Printf.fprintf oc "(%a | %a)" pp_ctr c1 pp_ctr c2

let rec pp_rec oc = function
    [] -> Printf.fprintf oc "no"
  | [m] -> Printf.fprintf oc "%d" m
  | m::lm -> Printf.fprintf oc "%d,%a" m pp_rec lm

let rec pp_act oc = function
    [] -> Printf.fprintf oc "none"
  | [x] -> Printf.fprintf oc "%s" x
  | x::lx -> Printf.fprintf oc "%s,%a" x pp_act lx

let pp_trans oc = function
    (i, lm, c, []) ->
     Printf.fprintf oc "  to %d, rec %a, when %a\n" i pp_rec lm pp_ctr c
  | (i, lm, c, a) ->
     Printf.fprintf oc "  to %d, rec %a, when %a do reset %a\n" i pp_rec lm pp_ctr c pp_act a

let pp_state oc (s,i,trl) =
  Printf.fprintf oc "state %d initial: %b\n" s i;
  List.iter (pp_trans oc) trl

let pp_auto oc (i,s,stl) =
  Printf.fprintf oc "automaton with %d states, %s markers\n" i s;
  List.iter (pp_state oc) stl

(** pretty print automaton in dot format **)

let ppd_auto_tr oc s (d,lm,c,a) =
  let color = if lm=[] then "black" else "red" in
  if (a=[]) then
    Printf.fprintf oc "  %d -> %d [color=%s,label=\"%a\"]\n" s d color pp_ctr c
  else
    Printf.fprintf oc "  %d -> %d [color=%s,label=\"%a\nrst %a\"]\n" s d color pp_ctr c pp_act a

let ppd_auto_st oc ((s,_,trl) as st) =
  let inv = snd (get_inv_st st) in
  if inv=True then
    Printf.fprintf oc "  %d [label=\"%d\"]\n" s s
  else
    Printf.fprintf oc "  %d [label=\"%d\n[%a]\"]\n" s s pp_ctr inv;
  List.iter (ppd_auto_tr oc s) trl


let ppd_auto oc ((_,s,stl) as a) =
  Printf.fprintf oc "digraph \"TA\" {\n";
  Printf.fprintf oc "  rankdir=LR\n";
  Printf.fprintf oc "  label=\"%s\"\n" s;
  Printf.fprintf oc "  labelloc=\"t\"\n";
  Printf.fprintf oc "  node [shape=\"circle\"]\n";
  Printf.fprintf oc "  I[label=\"\", style=invis, width=0]\n";
  Printf.fprintf oc "  I -> %d\n" (get_init a);
  List.iter (ppd_auto_st oc) stl;
  Printf.fprintf oc "}\n"

