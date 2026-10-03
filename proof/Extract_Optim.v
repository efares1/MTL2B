(* Extraction of the optimisation pipeline and of the export to OCaml
   (optim.ml).  Natural numbers are OCaml integers, Booleans and lists are
   OCaml Booleans and lists, real numbers are OCaml floats, and the list
   and arithmetic functions are those of the OCaml standard library. *)
Require Import List Bool Arith Reals Extraction ExtrOcamlBasic ExtrOcamlNatInt.
Require Import MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.
Require Import MTL_to_TBA_Invariants MTL_to_TBA_Optimizations MTL_to_TBA_Export.

(* ---------------------------------------------------------------- *)
(* Booleans, pairs, and subset types                                 *)
Extract Inlined Constant negb => "not".
Extract Inlined Constant Bool.eqb => "(=)".
Extract Inlined Constant Bool.bool_dec => "(=)".
Extract Inlined Constant fst => "fst".
Extract Inlined Constant snd => "snd".
Extract Inductive sig => "" [ "" ].

(* ---------------------------------------------------------------- *)
(* Natural numbers: OCaml integers and operators                     *)
Extract Inductive nat => "int" [ "0" "(fun n -> n + 1)" ]
  "(fun fO fS n -> if n = 0 then fO () else fS (n - 1))".
Extract Inlined Constant Nat.add => "(+)".
Extract Inlined Constant Nat.mul => "( * )".
Extract Inlined Constant Nat.sub => "(fun n m -> max 0 (n - m))".
Extract Inlined Constant Nat.pow =>
  "(fun b e -> let rec p e = if e = 0 then 1 else b * p (e - 1) in p e)".
Extract Inlined Constant Nat.div => "(fun n m -> if m = 0 then 0 else n / m)".
Extract Inlined Constant Nat.modulo => "(fun n m -> if m = 0 then n else n mod m)".
Extract Inlined Constant Nat.max => "max".
Extract Inlined Constant Nat.min => "min".
Extract Inlined Constant Nat.succ => "(fun n -> n + 1)".
Extract Inlined Constant Nat.pred => "(fun n -> max 0 (n - 1))".
Extract Inlined Constant Nat.eqb => "(=)".
Extract Inlined Constant Nat.leb => "(<=)".
Extract Inlined Constant Nat.ltb => "(<)".
Extract Inlined Constant Nat.eq_dec => "(=)".
(* the same functions under their Init.Nat names *)
Extract Inlined Constant Init.Nat.add => "(+)".
Extract Inlined Constant Init.Nat.mul => "( * )".
Extract Inlined Constant Init.Nat.sub => "(fun n m -> max 0 (n - m))".
Extract Inlined Constant Init.Nat.pow =>
  "(fun b e -> let rec p e = if e = 0 then 1 else b * p (e - 1) in p e)".
Extract Inlined Constant Init.Nat.div => "(fun n m -> if m = 0 then 0 else n / m)".
Extract Inlined Constant Init.Nat.modulo => "(fun n m -> if m = 0 then n else n mod m)".
Extract Inlined Constant Init.Nat.max => "max".
Extract Inlined Constant Init.Nat.min => "min".
Extract Inlined Constant Init.Nat.succ => "(fun n -> n + 1)".
Extract Inlined Constant Init.Nat.pred => "(fun n -> max 0 (n - 1))".
Extract Inlined Constant Init.Nat.eqb => "(=)".
Extract Inlined Constant Init.Nat.leb => "(<=)".
Extract Inlined Constant Init.Nat.ltb => "(<)".

(* [dec_b d] is the Boolean of a decision, i.e. the extracted Boolean itself *)
Extract Inlined Constant dec_b => "".


(* Binary integers (they only occur in the literals of real numbers):
   OCaml integers, as in ExtrOcamlZInt *)
Extract Inductive positive => "int"
  [ "(fun p -> 1 + 2 * p)" "(fun p -> 2 * p)" "1" ]
  "(fun f2p1 f2p f1 p -> if p <= 1 then f1 () else if p mod 2 = 0 then f2p (p / 2) else f2p1 (p / 2))".
Extract Inductive Z => "int" [ "0" "" "(~-)" ]
  "(fun f0 fp fn z -> if z = 0 then f0 () else if z > 0 then fp z else fn (- z))".

(* ---------------------------------------------------------------- *)
(* Lists: the OCaml List module                                      *)
Extract Inlined Constant length => "List.length".
Extract Inlined Constant app => "List.append".
Extract Inlined Constant map => "List.map".
Extract Inlined Constant flat_map => "List.concat_map".
Extract Inlined Constant filter => "List.filter".
Extract Inlined Constant existsb => "List.exists".
Extract Inlined Constant forallb => "List.for_all".
Extract Inlined Constant fold_right => "(fun f a l -> List.fold_right f l a)".
Extract Inlined Constant seq => "(fun s n -> List.init n (fun i -> s + i))".
Extract Inlined Constant nth =>
  "(fun n l d -> match List.nth_opt l n with Some x -> x | None -> d)".
Extract Inlined Constant nth_error => "List.nth_opt".
Extract Inlined Constant list_max => "(List.fold_left max 0)".
Extract Inlined Constant In_dec => "(fun eq a l -> List.exists (eq a) l)".
Extract Inlined Constant list_eq_dec => "List.equal".
Extract Inlined Constant list_prod =>
  "(fun l1 l2 -> List.concat_map (fun x -> List.map (fun y -> (x, y)) l2) l1)".

(* ---------------------------------------------------------------- *)
(* Real numbers: OCaml floats and operators                          *)
Extract Inlined Constant R => "float".
Extract Inlined Constant R0 => "0.0".
Extract Inlined Constant R1 => "1.0".
Extract Inlined Constant Rplus => "(+.)".
Extract Inlined Constant Rmult => "( *. )".
Extract Inlined Constant Ropp => "(~-.)".
Extract Inlined Constant Rminus => "(-.)".
Extract Inlined Constant Rinv => "(fun x -> 1.0 /. x)".
Extract Inlined Constant Rmin => "Float.min".
Extract Inlined Constant Rmax => "Float.max".
Extract Inlined Constant IZR => "float_of_int".
Extract Inlined Constant Rle_dec => "(<=)".
Extract Inlined Constant Rlt_dec => "(<)".
Extract Inlined Constant Req_EM_T => "(=)".
Extract Inlined Constant Req_dec_T => "(=)".

(* [optimize_export n A]: n rounds of optimization, then the export. *)
Definition optimize_export {root : mtl} (n : nat) (A : TBA root) : DTA root :=
  export (optimize n A).

Extraction "optim.ml" optimized optimize export optimize_export T ltl_atoms compile_with
  timed_subformulas MUle MUlt MUge MUgt MRle MRlt MRge MRgt.
