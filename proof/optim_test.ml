(* Test of the extracted optimizer and export on Example 3 of the paper,
   (<>[<=5] p) /\ [][<2] !p, after reset completion (Fig. steps45, right).
   build: ocamlc -o optim_test.exe optim.ml optim_test.ml *)
open Optim

let p = 0
let x = MUhatLe (5.0, MTrue, MAtom p)          (* clock x *)
let y = MRhatLt (2.0, MFalse, MNotAtom p)      (* clock y *)
let root = MAnd (x, y)
let name c = if c = x then "x" else if c = y then "y" else "?"

let k c cmp b = Some { guard_clock = c; guard_comparison = cmp; guard_bound = b }
let tr s l g z t = { bt_source = s; bt_label = l; bt_guard = g; bt_resets = z; bt_target = t }
let np = [(p, false)] and pp = [(p, true)]

let a = { tba_nstates = 4; tba_init = 1; tba_accepting = [0];
          tba_transitions = [
            tr 1 np [] [x; y] 3;
            tr 3 np [k x CLe 5.0; k y CLt 2.0] [] 3;
            tr 3 np [k x CLe 5.0; k y CGe 2.0] [] 2;
            tr 3 pp [k x CLe 5.0; k y CGe 2.0] [x] 0;
            tr 2 np [k x CLe 5.0] [] 2;
            tr 2 pp [k x CLe 5.0] [x] 0;
            tr 0 [] [] [x] 0 ] }

let cmp_s = function CLe -> "<=" | CLt -> "<" | CGe -> ">=" | CGt -> ">" | CEq -> "="
let num f = Printf.sprintf "%g" f
let lab l = match l with
  | [] -> "true"
  | _ -> String.concat " & " (List.map (fun (a, b) -> (if b then "" else "!") ^ "p" ^ string_of_int a) l)
let cc c = name c.guard_clock ^ cmp_s c.guard_comparison ^ num c.guard_bound
let resets z = if z = [] then "" else " / rst(" ^ String.concat "," (List.map name z) ^ ")"

let print_tba t =
  Printf.printf "initial %d, accepting [%s]\n" t.tba_init
    (String.concat ";" (List.map string_of_int t.tba_accepting));
  List.iter (fun t ->
      let g = List.filter_map (fun o -> Option.map cc o) t.bt_guard in
      Printf.printf "  %d -> %d : %s, %s%s\n" t.bt_source t.bt_target
        (if g = [] then "true" else String.concat " & " g) (lab t.bt_label) (resets t.bt_resets))
    t.tba_transitions

let dc = function
  | DSingle c -> cc c
  | DDiff (u, v, b) -> name u ^ "-" ^ name v ^ "<=" ^ num b

let print_dta d =
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
        Printf.printf "  inv(%d): %s\n" s
          (String.concat " & " (List.map (fun (c, b) -> name c ^ "<=" ^ num b) u))
    | _ -> ()
  done

let () =
  print_endline "== input"; print_tba a;
  print_endline "== optimize 3"; print_tba (optimize root 3 a);
  print_endline "== optimize_export 3"; print_dta (optimize_export root 3 a)
