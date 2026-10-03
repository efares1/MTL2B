(* Extraction of the optimisation pipeline and of the export to OCaml
   (optim.ml).
   nat -> int, bool -> bool, lists -> lists, R -> float. *)
From Stdlib Require Import Reals Extraction ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlZInt.
Require Import MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.
Require Import MTL_to_TBA_Invariants MTL_to_TBA_Optimizations MTL_to_TBA_Export.

(* Real numbers as OCaml floats. *)
Extract Constant R => "float".
Extract Constant R0 => "0.0".
Extract Constant R1 => "1.0".
Extract Constant Rplus => "(+.)".
Extract Constant Rmult => "( *. )".
Extract Constant Ropp => "(~-.)".
Extract Inlined Constant Rinv => "(fun x -> 1.0 /. x)".
Extract Inlined Constant Rminus => "(-.)".
Extract Inlined Constant Rmin => "Float.min".
Extract Inlined Constant Rmax => "Float.max".
Extract Inlined Constant IZR => "float_of_int".
Extract Inlined Constant Rle_dec => "(<=)".
Extract Inlined Constant Rlt_dec => "(<)".
Extract Inlined Constant Req_EM_T => "(=)".
Extract Inlined Constant Req_dec_T => "(=)".
Extract Constant total_order_T =>
  "fun x y -> if x < y then Some true else if x = y then Some false else None".

(* The internal construction of R (Dedekind cuts) is never executed;
   its entry points are realized as stubs so that the module loads. *)
Extract Constant Rabst => "(fun _ -> failwith ""Rabst: not used"")".
Extract Constant Rrepr => "(fun _ -> failwith ""Rrepr: not used"")".
Extract Constant ClassicalDedekindReals.sig_forall_dec =>
  "(fun _ -> failwith ""sig_forall_dec: not used"")".

(* [optimize_export n A]: n rounds of optimization, then the export. *)
Definition optimize_export {root : mtl} (n : nat) (A : TBA root) : DTA root :=
  export (optimize n A).

Extraction "optim.ml" optimized optimize export optimize_export.
