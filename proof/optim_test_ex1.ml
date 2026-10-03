(* Example 1 of the paper, <>[<=5](p U[<=2] q), automaton of Fig. ex1-with-disj.
   build: ocamlc -o optim_test_ex1.exe optim.ml optim_test_ex1.ml *)
open Optim
let p = 0 and q = 1
let u1 = MUhatLe (2.0, MAtom p, MAtom q)
let u0 = MUhatLe (5.0, MTrue, MOr (MAtom q, u1))
let root = u0
let name c = if c = u0 then "u0" else if c = u1 then "u1" else "?"
let k c cmp b = Some { guard_clock = c; guard_comparison = cmp; guard_bound = b }
let tr s l g z t = { bt_source = s; bt_label = l; bt_guard = g; bt_resets = z; bt_target = t }
let other = [(p, false); (q, false)] and pp = [(p, true)] and qq = [(q, true)]
let a = { tba_nstates = 5; tba_init = 1; tba_accepting = [2];
  tba_transitions = [
    tr 1 other [k u0 CLe 5.0] [u1] 1;   tr 1 pp [k u0 CLe 5.0] [u1] 4;
    tr 1 pp [k u0 CLe 5.0] [u1] 3;      tr 1 qq [k u0 CLe 5.0] [] 2;
    tr 4 other [k u0 CLe 5.0] [u1] 1;   tr 4 pp [k u0 CLe 5.0] [u1] 4;
    tr 4 qq [k u0 CLe 5.0] [] 2;        tr 4 qq [k u1 CLe 2.0] [] 2;
    tr 4 pp [k u0 CLe 5.0] [u1] 3;      tr 4 pp [k u0 CGt 5.0; k u1 CLe 2.0] [] 3;
    tr 3 pp [k u1 CLe 2.0] [] 3;        tr 3 qq [k u1 CLe 2.0] [] 2;
    tr 2 [] [] [] 2 ] }
let cmp_s = function CLe -> "<=" | CLt -> "<" | CGe -> ">=" | CGt -> ">" | CEq -> "="
let num f = Printf.sprintf "%g" f
let lab l = if l = [] then "true" else
  String.concat "&" (List.map (fun (a, b) -> (if b then "" else "!") ^ (if a = p then "p" else "q")) l)
let cc c = name c.guard_clock ^ cmp_s c.guard_comparison ^ num c.guard_bound
let resets z = if z = [] then "" else " / rst(" ^ String.concat "," (List.map name z) ^ ")"
let dc = function DSingle c -> cc c | DDiff (u, v, b) -> name u ^ "-" ^ name v ^ "<=" ^ num b
let () =
  let d = optimize_export root 3 a in
  Printf.printf "initial %d, accepting [%s]\n" d.dta_init
    (String.concat ";" (List.map string_of_int d.dta_accepting));
  List.iter (fun t ->
      let g = String.concat " | " (List.map (fun c ->
          if c = [] then "true" else String.concat " & " (List.map dc c)) t.dt_guard) in
      Printf.printf "  %d -> %d : %s, %s%s\n" t.dt_src t.dt_tgt g (lab t.dt_label) (resets t.dt_resets))
    d.dta_trans;
  for s = 0 to d.dta_nstates - 1 do
    match d.dta_inv s with
    | Some [u] when u <> [] ->
        Printf.printf "  inv(%d): %s\n" s (String.concat " & " (List.map (fun (c, b) -> name c ^ "<=" ^ num b) u))
    | _ -> ()
  done
