(*
  MTL_to_TBA_Shared_Clock_Core.v

  Concrete formal interface with syntactic sharing of identical timed subformula clocks

      MTL_{0,infty} --T--> LTL(extended alphabet)
                    --LTL2BA--> Buchi
                    --reinterpret--> Timed Buchi

  IMPORTANT AUDIT PROPERTY
  ------------------------
  This file contains:
      * no Parameter declarations;
      * no Variable/Hypothesis declarations;
      * no Admitted/Abort;
      * exactly ONE explicit project axiom:
            LTL_TO_BUCHI_CORRECT.

  All formerly abstract objects in the previous draft are concrete
  Definitions/Inductives/Records below.

  The proposition [EncodingCorrect] is the remaining mathematical theorem
  that the semantic proof file must establish.  The final composition theorem
  [MTL_to_TBA_correct_from_encoding] is fully proved from that theorem plus
  the single external LTL-to-Buchi axiom.

  Dense time is Coq's real-number type R.
*)

From Coq Require Import Arith Lia List Bool Reals Lra.
Import ListNotations.
Open Scope R_scope.

Set Implicit Arguments.
Unset Strict Implicit.

(* ====================================================================== *)
(* 1. Basic domains                                                       *)
(* ====================================================================== *)

Definition Action := nat.
(* Syntax-tree paths are used only to locate occurrences in structural proofs. *)
Definition Path := list bool.

Definition left_path  (p : Path) : Path := p ++ [false].
Definition right_path (p : Path) : Path := p ++ [true].

(* ====================================================================== *)
(* 2. Dense non-Zeno timed words                                          *)
(* ====================================================================== *)

Record timed_word : Type := {
  tw_action : nat -> Action;
  tw_time : nat -> R;
  tw_time_nonnegative : forall i, 0 <= tw_time i;
  tw_time_strict : forall i, tw_time i < tw_time (S i);
  tw_time_divergent : forall (i : nat) (d : R), 0 <= d ->
      exists j:nat, (i <= j)%nat /\ d <= tw_time j - tw_time i
}.

Definition delta (w : timed_word) (i : nat) : R :=
  tw_time w (S i) - tw_time w i.

Lemma delta_positive :
  forall w i, 0 < delta w i.
Proof.
  intros w i.
  unfold delta.
  pose proof (tw_time_strict w i).
  lra.
Qed.

Lemma time_monotone :
  forall w i j, (i <= j)%nat -> tw_time w i <= tw_time w j.
Proof.
  intros w i j Hij.
  induction Hij.
  - right; reflexivity.
  - eapply Rle_trans.
    + exact IHHij.
    + left. apply tw_time_strict.
Qed.

(* ====================================================================== *)
(* 3. MTL syntax                                                          *)
(* ====================================================================== *)

Inductive mtl : Type :=
| MTrue    : mtl
| MFalse   : mtl
| MAtom    : Action -> mtl
| MNotAtom : Action -> mtl
| MAnd     : mtl -> mtl -> mtl
| MOr      : mtl -> mtl -> mtl
| MNext    : mtl -> mtl
| MU       : mtl -> mtl -> mtl
| MR       : mtl -> mtl -> mtl
| MUhatLe  : R -> mtl -> mtl -> mtl
| MUhatGe  : R -> mtl -> mtl -> mtl
| MRhatLe  : R -> mtl -> mtl -> mtl
| MRhatGe  : R -> mtl -> mtl -> mtl
| MUhatLt  : R -> mtl -> mtl -> mtl
| MUhatGt  : R -> mtl -> mtl -> mtl
| MRhatLt  : R -> mtl -> mtl -> mtl
| MRhatGt  : R -> mtl -> mtl -> mtl.

(* The non-hatted timed operators are derived from the hatted ones.
   Thus the datatype has only the four primitive timed constructors. *)
Definition MUle (d : R) (p q : mtl) : mtl :=
  MOr q (MAnd p (MUhatLe d p q)).

Definition MUge (d : R) (p q : mtl) : mtl :=
  MAnd p (MUhatGe d p q).

Definition MRle (d : R) (p q : mtl) : mtl :=
  MAnd q (MOr p (MRhatLe d p q)).

Definition MRge (d : R) (p q : mtl) : mtl :=
  MOr p (MRhatGe d p q).

(* Strict ordinary timed operators are derived in exactly the same way from
   the strict hatted primitives.  Strict bounds are required to be positive. *)
Definition MUlt (d : R) (p q : mtl) : mtl :=
  MOr q (MAnd p (MUhatLt d p q)).

Definition MUgt (d : R) (p q : mtl) : mtl :=
  MAnd p (MUhatGt d p q).

Definition MRlt (d : R) (p q : mtl) : mtl :=
  MAnd q (MOr p (MRhatLt d p q)).

Definition MRgt (d : R) (p q : mtl) : mtl :=
  MOr p (MRhatGt d p q).

(* Physical/logical clocks are keyed by the timed formula itself.
   Hence two syntactically identical timed subformulas, even at different
   syntax-tree paths, denote exactly the same clock.  Paths remain available
   separately for occurrence lookup and structural induction. *)
Definition Clock := mtl.
Definition clock_of (f : mtl) : Clock := f.

(* Lower bounds are assumed strictly positive; bound 0 is represented by the
   corresponding untimed operator. *)
Fixpoint well_formed (f : mtl) : Prop :=
  match f with
  | MTrue | MFalse | MAtom _ | MNotAtom _ => True
  | MAnd p q | MOr p q | MU p q | MR p q =>
      well_formed p /\ well_formed q
  | MNext p => well_formed p
  | MUhatLe d p q | MRhatLe d p q =>
      0 <= d /\ well_formed p /\ well_formed q
  | MUhatGe d p q | MRhatGe d p q
  | MUhatLt d p q | MUhatGt d p q
  | MRhatLt d p q | MRhatGt d p q =>
      0 < d /\ well_formed p /\ well_formed q
  end.

(* ====================================================================== *)
(* 4. Pointwise MTL semantics                                             *)
(* ====================================================================== *)

Fixpoint msat (w : timed_word) (i : nat) (f : mtl) : Prop :=
  match f with
  | MTrue => True
  | MFalse => False
  | MAtom a => tw_action w i = a
  | MNotAtom a => tw_action w i <> a

  | MAnd p q => msat w i p /\ msat w i q
  | MOr p q  => msat w i p \/ msat w i q
  | MNext p  => msat w (S i) p

  | MU p q =>
      exists j,
        (i <= j)%nat /\
        msat w j q /\
        (forall k, (i <= k < j)%nat -> msat w k p)

  | MR p q =>
      forall j,
        (i <= j)%nat ->
        msat w j q \/
        exists k, (i <= k < j)%nat /\ msat w k p

  | MUhatLe d p q =>
      exists j,
        (i < j)%nat /\
        tw_time w j - tw_time w i <= d /\
        msat w j q /\
        (forall k, (i < k < j)%nat -> msat w k p)

  | MUhatGe d p q =>
      exists j,
        (i < j)%nat /\
        d <= tw_time w j - tw_time w i /\
        msat w j q /\
        (forall k, (i < k < j)%nat -> msat w k p)

  | MRhatLe d p q =>
      forall j,
        (i < j)%nat ->
        tw_time w j - tw_time w i <= d ->
        msat w j q \/
        exists k, (i < k < j)%nat /\ msat w k p

  | MRhatGe d p q =>
      forall j,
        (i < j)%nat ->
        d <= tw_time w j - tw_time w i ->
        msat w j q \/
        exists k, (i < k < j)%nat /\ msat w k p

  | MUhatLt d p q =>
      exists j,
        (i < j)%nat /\
        tw_time w j - tw_time w i < d /\
        msat w j q /\
        (forall k, (i < k < j)%nat -> msat w k p)

  | MUhatGt d p q =>
      exists j,
        (i < j)%nat /\
        d < tw_time w j - tw_time w i /\
        msat w j q /\
        (forall k, (i < k < j)%nat -> msat w k p)

  | MRhatLt d p q =>
      forall j,
        (i < j)%nat ->
        tw_time w j - tw_time w i < d ->
        msat w j q \/
        exists k, (i < k < j)%nat /\ msat w k p

  | MRhatGt d p q =>
      forall j,
        (i < j)%nat ->
        d < tw_time w j - tw_time w i ->
        msat w j q \/
        exists k, (i < k < j)%nat /\ msat w k p
  end.

(* ====================================================================== *)
(* 5. Derived timed operators: semantic correctness                       *)
(* ====================================================================== *)

(* The following four predicates are the intended inclusive-endpoint
   semantics of the non-hatted timed operators.  The datatype itself does
   not contain these constructors: [MUle], [MUge], [MRle], and [MRge] are
   definitions above in terms of the four primitive hatted constructors. *)

Definition MUle_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  exists j,
    (i <= j)%nat /\
    tw_time w j - tw_time w i <= d /\
    msat w j q /\
    (forall k, (i <= k < j)%nat -> msat w k p).

Definition MUge_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  exists j,
    (i <= j)%nat /\
    d <= tw_time w j - tw_time w i /\
    msat w j q /\
    (forall k, (i <= k < j)%nat -> msat w k p).

Definition MRle_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  forall j,
    (i <= j)%nat ->
    tw_time w j - tw_time w i <= d ->
    msat w j q \/
    exists k, (i <= k < j)%nat /\ msat w k p.

Definition MRge_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  forall j,
    (i <= j)%nat ->
    d <= tw_time w j - tw_time w i ->
    msat w j q \/
    exists k, (i <= k < j)%nat /\ msat w k p.

Lemma MUle_derived_semantics :
  forall w i d p q,
    0 <= d ->
    msat w i (MUle d p q) <-> MUle_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MUle, MUle_sem.
  simpl.
  split.
  - intros [Hq | [Hp Hhat]].
    + exists i.
      repeat split.
      * lia.
      * simpl; lra.
      * exact Hq.
      * intros k Hk; lia.
    + destruct Hhat as [j [Hij [Htime [Hqj Hp']]]].
      exists j.
      repeat split.
      * lia.
      * exact Htime.
      * exact Hqj.
      * intros k Hk.
        destruct (Nat.eq_dec i k) as [-> | Hneq].
        -- exact Hp.
        -- apply Hp'.
           lia.
  - intros [j [Hij [Htime [Hqj Hp]]]].
    destruct (Nat.eq_dec i j) as [-> | Hneq].
    + left; exact Hqj.
    + right.
      split.
      * apply Hp; lia.
      * exists j.
        repeat split.
        -- lia.
        -- exact Htime.
        -- exact Hqj.
        -- intros k Hk.
           apply Hp.
           lia.
Qed.

Lemma MUge_derived_semantics :
  forall w i d p q,
    0 < d ->
    msat w i (MUge d p q) <-> MUge_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MUge, MUge_sem.
  simpl.
  split.
  - intros [Hp Hhat].
    destruct Hhat as [j [Hij [Htime [Hqj Hpall]]]].
    exists j.
    repeat split.
    + lia.
    + exact Htime.
    + exact Hqj.
    + intros k Hk.
      destruct (eq_nat_dec k i); subst; auto.
      apply Hpall.
      lia.
  - intros [j [Hij [Htime [Hqj Hpall]]]].
    assert (Hij_strict : (i < j)%nat).
    {
      destruct (Nat.eq_dec i j) as [-> | Hneq].
      - simpl in Htime.
        lra.
      - lia.
    }
    split.
    + apply Hpall.
      lia.
    + exists j.
      repeat split; try assumption.
      intros k Hk.
      apply Hpall.
      lia.
Qed.

Require Import Classical.

Lemma MRle_derived_semantics :
  forall w i d p q,
    0 <= d ->
    msat w i (MRle d p q) <-> MRle_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MRle, MRle_sem.
  simpl.
  split. 
  - intros [Hq [Hp |Hhat]] j Hij Htime.
    + destruct (Nat.eq_dec i j) as [-> | Hneq].
      * left; exact Hq.
      * right; exists i; split; [lia | exact Hp].
    + destruct (Nat.eq_dec i j) as [-> | Hneq]; try tauto.
      assert (Hij_strict : (i < j)%nat) by lia.
      destruct (Hhat j Hij_strict Htime) as [Hqj | [k [Hk Hpk]]].
        -- left; exact Hqj.
        -- right; exists k; split; [lia | exact Hpk].
  - intros Hsem.
    assert (Hqi : msat w i q).
    {
      destruct (Hsem i (le_n i) ltac:(simpl; lra)) as [Hqi | [k [Hk _]]].
      - exact Hqi.
      - lia.
    }
    split.
    + exact Hqi.
    + destruct (classic (msat w i p)) as [Hp | Hn].
      * left; exact Hp.
      * right.
        intros j Hij_strict Htime.
        destruct (Hsem j ltac:(lia) Htime) as [Hqj | [k [Hk Hpk]]].
        -- left; exact Hqj.
        -- destruct (eq_nat_dec k i); subst; try tauto.
          right; exists k; split; [lia | exact Hpk].
Qed.

Lemma MRge_derived_semantics :
  forall w i d p q,
    0 < d ->
    msat w i (MRge d p q) <-> MRge_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MRge, MRge_sem.
  simpl.
  firstorder.
  - destruct (eq_nat_dec i j); subst; try lra.
     right; exists i; split; auto; lia.
  - destruct (eq_nat_dec i j); subst; try lra.
    specialize (H j ltac:(lia)).
    assert (Hij_strict : (S i <= j)%nat) by lia.
    apply time_monotone with (w:=w) in Hij_strict.
    specialize (tw_time_strict w i) as HiSi.
    assert (tw_time w i  < tw_time w j) as Hlt by lra.
    assert (d <= tw_time w j - tw_time w i) as Hle by lra.
    destruct (H Hle); try tauto.
    destruct H2 as [k [H2a H2b]]; right; exists k; split; auto; lia.
  - destruct (eq_nat_dec i j); subst; try lra.
    right; exists i; split; auto; lia.
  - destruct (eq_nat_dec i j); subst; try lra.
    destruct (H j ltac:(lia) ltac:(lra)); try tauto.
    destruct H1 as [k [H1a H1b]].
    right; exists k; split; auto; lia.
  - destruct (classic (msat w i p)) as [Hp | Hn]; try tauto.
    right; intros.
    destruct (H j ltac:(lia) H1); try tauto.
    destruct H2 as [k [H2a H2b]].
    destruct (eq_nat_dec k i); subst; try tauto.
    right; exists k; split; auto; lia.
Qed.


(* Strict-comparator ordinary operators: intended pointwise semantics. *)

Definition MUlt_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  exists j,
    (i <= j)%nat /\
    tw_time w j - tw_time w i < d /\
    msat w j q /\
    (forall k, (i <= k < j)%nat -> msat w k p).

Definition MUgt_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  exists j,
    (i <= j)%nat /\
    d < tw_time w j - tw_time w i /\
    msat w j q /\
    (forall k, (i <= k < j)%nat -> msat w k p).

Definition MRlt_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  forall j,
    (i <= j)%nat ->
    tw_time w j - tw_time w i < d ->
    msat w j q \/
    exists k, (i <= k < j)%nat /\ msat w k p.

Definition MRgt_sem (w : timed_word) (i : nat) (d : R) (p q : mtl) : Prop :=
  forall j,
    (i <= j)%nat ->
    d < tw_time w j - tw_time w i ->
    msat w j q \/
    exists k, (i <= k < j)%nat /\ msat w k p.

Lemma MUlt_derived_semantics :
  forall w i d p q,
    0 < d ->
    msat w i (MUlt d p q) <-> MUlt_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MUlt, MUlt_sem.
  simpl.
  split.
  - intros [Hq | [Hp Hhat]].
    + exists i.
      repeat split.
      * lia.
      * simpl; lra.
      * exact Hq.
      * intros k Hk; lia.
    + destruct Hhat as [j [Hij [Htime [Hqj Hp']]]].
      exists j.
      repeat split.
      * lia.
      * exact Htime.
      * exact Hqj.
      * intros k Hk.
        destruct (Nat.eq_dec i k) as [-> | Hneq].
        -- exact Hp.
        -- apply Hp'. lia.
  - intros [j [Hij [Htime [Hqj Hp]]]].
    destruct (Nat.eq_dec i j) as [-> | Hneq].
    + left; exact Hqj.
    + right.
      split.
      * apply Hp; lia.
      * exists j.
        repeat split; try assumption.
        lia.
        intros k Hk. apply Hp. lia.
Qed.

Lemma MUgt_derived_semantics :
  forall w i d p q,
    0 < d ->
    msat w i (MUgt d p q) <-> MUgt_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MUgt, MUgt_sem.
  simpl.
  split.
  - intros [Hp Hhat].
    destruct Hhat as [j [Hij [Htime [Hqj Hpall]]]].
    exists j.
    repeat split.
    + lia.
    + exact Htime.
    + exact Hqj.
    + intros k Hk.
      destruct (Nat.eq_dec k i) as [-> | Hneq].
      * exact Hp.
      * apply Hpall. lia.
  - intros [j [Hij [Htime [Hqj Hpall]]]].
    assert (Hij_strict : (i < j)%nat).
    {
      destruct (Nat.eq_dec i j) as [-> | Hneq].
      - simpl in Htime. lra.
      - lia.
    }
    split.
    + apply Hpall. lia.
    + exists j.
      repeat split; try assumption.
      intros k Hk. apply Hpall. lia.
Qed.

Require Import Classical.

Lemma MRlt_derived_semantics :
  forall w i d p q,
    0 < d ->
    msat w i (MRlt d p q) <-> MRlt_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MRlt, MRlt_sem.
  simpl.
  split.
  - intros [Hq [Hp | Hhat]] j Hij Htime.
    + destruct (Nat.eq_dec i j) as [-> | Hneq].
      * left; exact Hq.
      * right; exists i; split; [lia | exact Hp].
    + destruct (Nat.eq_dec i j) as [-> | Hneq]; try tauto.
      assert (Hij_strict : (i < j)%nat) by lia.
      destruct (Hhat j Hij_strict Htime) as [Hqj | [k [Hk Hpk]]].
      * left; exact Hqj.
      * right; exists k; split; [lia | exact Hpk].
  - intros Hsem.
    assert (Hqi : msat w i q).
    {
      destruct (Hsem i (le_n i) ltac:(simpl; lra)) as [Hqi | [k [Hk _]]].
      - exact Hqi.
      - lia.
    }
    split.
    + exact Hqi.
    + destruct (classic (msat w i p)) as [Hp | Hnp].
      * left; exact Hp.
      * right.
        intros j Hij_strict Htime.
        destruct (Hsem j ltac:(lia) Htime) as [Hqj | [k [Hk Hpk]]].
        -- left; exact Hqj.
        -- destruct (Nat.eq_dec k i) as [-> | Hneq].
           ++ contradiction.
           ++ right; exists k; split; [lia | exact Hpk].
Qed.

Lemma MRgt_derived_semantics :
  forall w i d p q,
    0 < d ->
    msat w i (MRgt d p q) <-> MRgt_sem w i d p q.
Proof.
  intros w i d p q Hd.
  unfold MRgt, MRgt_sem.
  simpl.
  split.
  - intros [Hp | Hhat] j Hij Htime.
    + destruct (Nat.eq_dec i j) as [-> | Hneq].
      * simpl in Htime. lra.
      * right. exists i. split; [lia | exact Hp].
    + destruct (Nat.eq_dec i j) as [-> | Hneq].
      * simpl in Htime. lra.
      * destruct (Hhat j ltac:(lia) Htime) as [Hqj | [k [Hk Hpk]]].
        -- left; exact Hqj.
        -- right; exists k; split; [lia | exact Hpk].
  - intros Hsem.
    destruct (classic (msat w i p)) as [Hp | Hnp].
    + left; exact Hp.
    + right.
      intros j Hij_strict Htime.
      destruct (Hsem j ltac:(lia) Htime) as [Hqj | [k [Hk Hpk]]].
      * left; exact Hqj.
      * destruct (Nat.eq_dec k i) as [-> | Hneq].
        -- contradiction.
        -- right; exists k; split; [lia | exact Hpk].
Qed.

(* In particular, the derived ordinary constructors have exactly the intended
   pointwise semantics.  The non-strict upper-bound cases admit d = 0; the
   lower-bound and strict-comparator cases are restricted here to d > 0. *)

(* ====================================================================== *)
(* 6. Extended words and clocks                                           *)

(* ====================================================================== *)

Record ext_word : Type := {
  ew_base : timed_word;
  ew_val : nat -> Clock -> R;
  ew_reset : nat -> Clock -> bool
}.

Definition same_base (rho : ext_word) (w : timed_word) : Prop :=
  ew_base rho = w.

Definition clock_consistent (rho : ext_word) : Prop :=
  (forall i x, 0 <= ew_val rho i x) /\
  (forall i x,
      ew_val rho (S i) x =
      if ew_reset rho i x
      then delta (ew_base rho) i
      else ew_val rho i x + delta (ew_base rho) i).

Definition c_le (rho : ext_word) (x : Clock) (d : R) (i : nat) : Prop :=
  ew_val rho i x <= d.

Definition c_lt (rho : ext_word) (x : Clock) (d : R) (i : nat) : Prop :=
  ew_val rho i x < d.

Definition c_ge (rho : ext_word) (x : Clock) (d : R) (i : nat) : Prop :=
  d <= ew_val rho i x.

Definition c_gt (rho : ext_word) (x : Clock) (d : R) (i : nat) : Prop :=
  d < ew_val rho i x.

Definition rst_at (rho : ext_word) (x : Clock) (i : nat) : Prop :=
  ew_reset rho i x = true.

Definition unch_at (rho : ext_word) (x : Clock) (i : nat) : Prop :=
  ew_reset rho i x = false.

(* ====================================================================== *)
(* 6. LTL over the extended alphabet                                      *)
(* ====================================================================== *)

Inductive latom : Type :=
| LAct  : Action -> latom
| LNAct : Action -> latom
| LCLe  : Clock -> R -> latom
| LCLt  : Clock -> R -> latom
| LCGe  : Clock -> R -> latom
| LCGt  : Clock -> R -> latom
| LRst  : Clock -> latom
| LUnch : Clock -> latom.

Inductive ltl : Type :=
| LTrue    : ltl
| LFalse   : ltl
| LAtom    : latom -> ltl
| LAnd     : ltl -> ltl -> ltl
| LOr      : ltl -> ltl -> ltl
| LNext    : ltl -> ltl
| LUntil   : ltl -> ltl -> ltl
| LRelease : ltl -> ltl -> ltl.

Definition LF (p : ltl) : ltl :=
  LUntil LTrue p.

Definition LG (p : ltl) : ltl :=
  LRelease LFalse p.

Definition LW (p q : ltl) : ltl :=
  LOr (LUntil p q) (LG p).

Definition LGF (p : ltl) : ltl :=
  LG (LF p).

Definition atom_sat (rho : ext_word) (i : nat) (a : latom) : Prop :=
  match a with
  | LAct p  => tw_action (ew_base rho) i = p
  | LNAct p => tw_action (ew_base rho) i <> p
  | LCLe x d => c_le rho x d i
  | LCLt x d => c_lt rho x d i
  | LCGe x d => c_ge rho x d i
  | LCGt x d => c_gt rho x d i
  | LRst x => rst_at rho x i
  | LUnch x => unch_at rho x i
  end.

Fixpoint lsat (rho : ext_word) (i : nat) (f : ltl) : Prop :=
  match f with
  | LTrue => True
  | LFalse => False
  | LAtom a => atom_sat rho i a
  | LAnd p q => lsat rho i p /\ lsat rho i q
  | LOr p q => lsat rho i p \/ lsat rho i q
  | LNext p => lsat rho (S i) p

  | LUntil p q =>
      exists j,
        (i <= j)%nat /\
        lsat rho j q /\
        (forall k, (i <= k < j)%nat -> lsat rho k p)

  | LRelease p q =>
      forall j,
        (i <= j)%nat ->
        lsat rho j q \/
        exists k, (i <= k < j)%nat /\ lsat rho k p
  end.

(* ====================================================================== *)
(* 7. Marker-free translation T                                           *)
(* ====================================================================== *)

Fixpoint T_at (path : Path) (f : mtl) : ltl :=
  match f with
  | MTrue => LTrue
  | MFalse => LFalse
  | MAtom a => LAtom (LAct a)
  | MNotAtom a => LAtom (LNAct a)
  | MAnd p q => LAnd (T_at (left_path path) p) (T_at (right_path path) q)
  | MOr p q => LOr (T_at (left_path path) p) (T_at (right_path path) q)
  | MNext p => LNext (T_at (left_path path) p)
  | MU p q => LUntil (T_at (left_path path) p) (T_at (right_path path) q)
  | MR p q => LRelease (T_at (left_path path) p) (T_at (right_path path) q)
  | MUhatLe d p q =>
      let x := clock_of (MUhatLe d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let C := LAtom (LCLe x d) in
      let H := LAtom (LUnch x) in
      LNext (LUntil (LAnd C (LAnd H A))  (LAnd C B))

  | MUhatGe d p q =>
      let x := clock_of (MUhatGe d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let Q := LAnd (LAtom (LCGe x d)) (LUntil A B) in
      let Gamma := LOr (LUntil A Q) (LAnd (LG A) (LGF B)) in
      LAnd (LAtom (LRst x)) (LNext Gamma)

  | MRhatLe d p q =>
      let x := clock_of (MRhatLe d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let D := LOr (LAtom (LCGt x d)) (LAnd A B) in
      LAnd (LAtom (LRst x)) (LNext (LW B D))

  | MRhatGe d p q =>
      let x := clock_of (MRhatGe d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let E := LOr (LRelease A B) (LAnd (LAtom (LCLt x d)) A) in
      let K := LAnd (LAtom (LCLt x d)) (LAtom (LUnch x)) in
      LNext (LW K E)

  | MUhatLt d p q =>
      let x := clock_of (MUhatLt d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let C := LAtom (LCLt x d) in
      let H := LAtom (LUnch x) in
      LNext (LUntil (LAnd C (LAnd H A)) (LAnd C B))

  | MUhatGt d p q =>
      let x := clock_of (MUhatGt d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let Q := LAnd (LAtom (LCGt x d)) (LUntil A B) in
      let Gamma := LOr (LUntil A Q) (LAnd (LG A) (LGF B)) in
      LAnd (LAtom (LRst x)) (LNext Gamma)

  | MRhatLt d p q =>
      let x := clock_of (MRhatLt d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let D := LOr (LAtom (LCGe x d)) (LAnd A B) in
      LAnd (LAtom (LRst x)) (LNext (LW B D))

  | MRhatGt d p q =>
      let x := clock_of (MRhatGt d p q) in
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let E := LOr (LRelease A B) (LAnd (LAtom (LCLe x d)) A) in
      let K := LAnd (LAtom (LCLe x d)) (LAtom (LUnch x)) in
      LNext (LW K E)
  end.

Definition T (f : mtl) : ltl := T_at [] f.

(* ====================================================================== *)
(* 8. Concrete finite-state Buchi automata                                *)
(* ====================================================================== *)

Record letter : Type := {
  letter_action : Action;
  letter_time : R;
  letter_delta : R;
  letter_value : Clock -> R;
  letter_reset : Clock -> bool
}.

Definition letter_of (rho : ext_word) (i : nat) : letter :=
  {| letter_action := tw_action (ew_base rho) i;
     letter_time := tw_time (ew_base rho) i;
     letter_delta := delta (ew_base rho) i;
     letter_value := ew_val rho i;
     letter_reset := ew_reset rho i |}.

Record buchi : Type := {
  ba_nstates : nat;
  ba_init : nat;
  ba_step : nat -> letter -> nat -> Prop;
  ba_accepting : nat -> Prop
}.

Definition BA_accepts (A : buchi) (rho : ext_word) : Prop :=
  exists run : nat -> nat,
    run 0%nat = ba_init A /\
    (forall i,
       (run i < ba_nstates A)%nat /\
       ba_step A (run i) (letter_of rho i) (run (S i))) /\
    (forall n,
       exists j,
         (n <= j)%nat /\ ba_accepting A (run j)).

(* ====================================================================== *)
(* 9. The ONE external axiom: LTL -> Buchi correctness                    *)
(* ====================================================================== *)

Axiom LTL_TO_BUCHI_CORRECT :
  forall f : ltl,
    { A : buchi |
      forall rho : ext_word,
        BA_accepts A rho <-> lsat rho 0 f }.

(* The translator is a DEFINITION extracted from the single axiom. *)
Definition ltl_to_buchi (f : ltl) : buchi :=
  proj1_sig (LTL_TO_BUCHI_CORRECT f).

Theorem ltl_to_buchi_correct :
  forall f rho,
    BA_accepts (ltl_to_buchi f) rho <-> lsat rho 0 f.
Proof.
  intros f rho.
  unfold ltl_to_buchi.
  destruct (LTL_TO_BUCHI_CORRECT f) as [A HA].
  simpl.
  apply HA.
Qed.

(* ====================================================================== *)
(* 10. Concrete TBA reinterpretation                                      *)
(* ====================================================================== *)

Record tba : Type := {
  tba_buchi : buchi
}.

Definition reinterpret_as_TBA (A : buchi) : tba :=
  {| tba_buchi := A |}.

(* A timed run is exactly a Buchi run over a clock-consistent extension of
   the visible timed word.  Thus rst/unch and clock constraints recover their
   operational timed meaning through [clock_consistent] and [atom_sat]. *)
Definition TBA_accepts (A : tba) (w : timed_word) : Prop :=
  exists rho : ext_word,
    same_base rho w /\
    clock_consistent rho /\
    BA_accepts (tba_buchi A) rho.

Theorem reinterpretation_correct :
  forall A w,
    TBA_accepts (reinterpret_as_TBA A) w <->
    exists rho,
      same_base rho w /\
      clock_consistent rho /\
      BA_accepts A rho.
Proof.
  intros.
  reflexivity.
Qed.

(* ====================================================================== *)
(* 11. Exact semantic theorem still to be PROVED, stated separately             *)
(* ====================================================================== *)

Definition EncodingCorrect :=
  forall (f : mtl) (w : timed_word),
    well_formed f ->
    (msat w 0 f <->
     exists rho,
       same_base rho w /\
       clock_consistent rho /\
       lsat rho 0 (T f)).

(* ====================================================================== *)
(* 12. End-to-end composition                                             *)
(* ====================================================================== *)

Definition compile (f : mtl) : tba :=
  reinterpret_as_TBA (ltl_to_buchi (T f)).

Theorem MTL_to_TBA_correct_from_encoding :
  forall (f : mtl) (w : timed_word),
    EncodingCorrect ->
    well_formed f ->
    (msat w 0 f <-> TBA_accepts (compile f) w).
Proof.
  intros f w Henc Hwf.
  specialize (Henc f w Hwf).

  unfold compile, TBA_accepts, reinterpret_as_TBA.
  simpl.

  split.
  - intro Hm.
    apply (proj1 Henc) in Hm.
    destruct Hm as [rho [Hbase [Hclock Hltl]]].
    exists rho.
    repeat split; intros; try (apply Hclock; try assumption); auto.
    apply (proj2 (ltl_to_buchi_correct (T f) rho)).
    exact Hltl.

  - intros [rho [Hbase [Hclock Hba]]].
    apply (proj2 Henc).
    exists rho.
    repeat split; intros; try (apply Hclock; try assumption); auto.
    apply (proj1 (ltl_to_buchi_correct (T f) rho)).
    exact Hba.
Qed.

(* ====================================================================== *)
(* 13. Audit notes                                                        *)
(* ====================================================================== *)

(*
  Useful checks:

      Search "Axiom".
      Print Assumptions ltl_to_buchi_correct.
      Print Assumptions MTL_to_TBA_correct_from_encoding.

    There is exactly one explicit Axiom declaration:
      LTL_TO_BUCHI_CORRECT.
*)
