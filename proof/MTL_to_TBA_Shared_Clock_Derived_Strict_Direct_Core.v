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

  CLOCKS
  ------
  The clocks of an initial formula [root] are its timed subformulas:
      Clock root := { x : mtl | In x (timed_subformulas root) }.
  Extended words, LTL atoms and formulas, the translation [T], and the
  Buchi and timed Buchi automata are all indexed by [root] and use this
  finite type.  [T_uses_every_clock] shows that every clock occurs in the
  translation.
*)

Require Import Arith Lia List Bool Reals Lra.
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
   The datatype therefore contains the eight primitive hatted timed
   constructors: <=, >=, <, and > for Until and Release. *)
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
   definitions above in terms of the four non-strict primitive hatted
   constructors.  The strict ordinary operators are handled analogously by
   their four strict primitive hatted constructors. *)

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
        repeat split; try assumption; try lia.
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

(* ---------------------------------------------------------------------- *)
(* Clocks of a formula                                                    *)
(* ---------------------------------------------------------------------- *)

(* The timed (hatted) subformulas of a formula, including the formula
   itself when it is timed.  Ordinary timed operators are definitions in
   terms of the hatted ones, so they contribute through their unfolding. *)
Fixpoint timed_subformulas (f : mtl) : list mtl :=
  match f with
  | MTrue | MFalse | MAtom _ | MNotAtom _ => []
  | MAnd p q | MOr p q | MU p q | MR p q =>
      timed_subformulas p ++ timed_subformulas q
  | MNext p => timed_subformulas p
  | MUhatLe _ p q | MUhatGe _ p q | MRhatLe _ p q | MRhatGe _ p q
  | MUhatLt _ p q | MUhatGt _ p q | MRhatLt _ p q | MRhatGt _ p q =>
      f :: timed_subformulas p ++ timed_subformulas q
  end.

(* The clocks of a formula [root] are exactly its timed subformulas.
   Clocks are keyed by the timed formula itself: two syntactically
   identical timed subformulas, even at different syntax-tree paths,
   denote the same clock.  The type is finite. *)
Definition Clock (root : mtl) : Type :=
  { x : mtl | In x (timed_subformulas root) }.

Require Import Classical.

(* Decidable equality of formulas; bounds are compared as real numbers. *)
Definition mtl_eq_dec : forall f g : mtl, {f = g} + {f <> g}.
Proof.
  decide equality; first [apply Nat.eq_dec | apply Req_EM_T].
Defined.

Lemma clock_eq :
  forall root (x y : Clock root), proj1_sig x = proj1_sig y -> x = y.
Proof.
  intros root [x Hx] [y Hy] Heq. simpl in Heq. subst y.
  f_equal. apply proof_irrelevance.
Qed.

(* Decidable equality of clocks. *)
Definition clock_eq_dec (root : mtl) (x y : Clock root) : {x = y} + {x <> y}.
Proof.
  destruct (mtl_eq_dec (proj1_sig x) (proj1_sig y)) as [E|N].
  - left. apply clock_eq. exact E.
  - right. intro H. apply N. rewrite H. reflexivity.
Defined.

(* The clock of a timed formula [F] within [root], when [F] is a timed
   subformula of [root]. *)
Definition clock_of (root F : mtl) : option (Clock root) :=
  match In_dec mtl_eq_dec F (timed_subformulas root) with
  | left H => Some (exist _ F H)
  | right _ => None
  end.

Lemma clock_of_mem :
  forall root F (H : In F (timed_subformulas root)),
    clock_of root F = Some (exist _ F H).
Proof.
  intros root F H. unfold clock_of.
  destruct (In_dec mtl_eq_dec F (timed_subformulas root)) as [H'|Hn].
  - f_equal. apply clock_eq. reflexivity.
  - contradiction.
Qed.

Lemma clock_of_proj :
  forall root F x, clock_of root F = Some x -> proj1_sig x = F.
Proof.
  intros root F x. unfold clock_of.
  destruct (In_dec mtl_eq_dec F (timed_subformulas root)).
  - intro Heq. injection Heq as <-. reflexivity.
  - discriminate.
Qed.

Lemma clock_of_none :
  forall root F, clock_of root F = None -> ~ In F (timed_subformulas root).
Proof.
  intros root F. unfold clock_of.
  destruct (In_dec mtl_eq_dec F (timed_subformulas root)).
  - discriminate.
  - intros _. assumption.
Qed.

(* Boolean reflection of a proposition, by classical reasoning.  It is used
   only inside proofs, to build witness reset functions; it is never part of
   the extracted code. *)
Require Import Description.

Definition bool_of_prop (P : Prop) : {b : bool | b = true <-> P}.
Proof.
  apply constructive_definite_description.
  destruct (classic P) as [H|H].
  - exists true. split; [tauto|].
    intros b Hb. destruct b; [reflexivity|]. apply Hb in H. discriminate.
  - exists false. split; [split; [discriminate | contradiction]|].
    intros b Hb. destruct b; [|reflexivity].
    exfalso. apply H. apply Hb. reflexivity.
Defined.

Definition decide_b (P : Prop) : bool := proj1_sig (bool_of_prop P).

Lemma decide_b_spec : forall P, decide_b P = true <-> P.
Proof. intro P. exact (proj2_sig (bool_of_prop P)). Qed.

(* ====================================================================== *)
(* 6. Extended words and clocks                                           *)
(* ====================================================================== *)

(* Everything below is relative to the initial formula [root]: the clocks
   are the elements of [Clock root], i.e. the timed subformulas of [root]. *)
Section Clocks.

Variable root : mtl.

Record ext_word : Type := {
  ew_base : timed_word;
  ew_val : nat -> Clock root -> R;
  ew_reset : nat -> Clock root -> bool
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

Definition c_le (rho : ext_word) (x : Clock root) (d : R) (i : nat) : Prop :=
  ew_val rho i x <= d.

Definition c_lt (rho : ext_word) (x : Clock root) (d : R) (i : nat) : Prop :=
  ew_val rho i x < d.

Definition c_ge (rho : ext_word) (x : Clock root) (d : R) (i : nat) : Prop :=
  d <= ew_val rho i x.

Definition c_gt (rho : ext_word) (x : Clock root) (d : R) (i : nat) : Prop :=
  d < ew_val rho i x.

Definition rst_at (rho : ext_word) (x : Clock root) (i : nat) : Prop :=
  ew_reset rho i x = true.

Definition unch_at (rho : ext_word) (x : Clock root) (i : nat) : Prop :=
  ew_reset rho i x = false.

(* ====================================================================== *)
(* 6. LTL over the extended alphabet                                      *)
(* ====================================================================== *)

Inductive latom : Type :=
| LAct  : Action -> latom
| LNAct : Action -> latom
| LCLe  : Clock root -> R -> latom
| LCLt  : Clock root -> R -> latom
| LCGe  : Clock root -> R -> latom
| LCGt  : Clock root -> R -> latom
| LRst  : Clock root -> latom
| LUnch : Clock root -> latom.

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

(* A timed subformula [F] of [root] uses the clock [clock_of root F].  The
   [None] branch is never taken for subformulas of [root]. *)
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
      match clock_of root (MUhatLe d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let C := LAtom (LCLe x d) in
      let H := LAtom (LUnch x) in
      LNext (LUntil (LAnd C (LAnd H A))  (LAnd C B))
      end

  | MUhatGe d p q =>
      match clock_of root (MUhatGe d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let Q := LAnd (LAtom (LCGe x d)) (LUntil A B) in
      let Gamma := LOr (LUntil A Q) (LAnd (LG A) (LGF B)) in
      LAnd (LAtom (LRst x)) (LNext Gamma)
      end

  | MRhatLe d p q =>
      match clock_of root (MRhatLe d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let D := LOr (LAtom (LCGt x d)) (LAnd A B) in
      LAnd (LAtom (LRst x)) (LNext (LW B D))
      end

  | MRhatGe d p q =>
      match clock_of root (MRhatGe d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let E := LOr (LRelease A B) (LAnd (LAtom (LCLt x d)) A) in
      let K := LAnd (LAtom (LCLt x d)) (LAtom (LUnch x)) in
      LNext (LW K E)
      end

  | MUhatLt d p q =>
      match clock_of root (MUhatLt d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let C := LAtom (LCLt x d) in
      let H := LAtom (LUnch x) in
      LNext (LUntil (LAnd C (LAnd H A)) (LAnd C B))
      end

  | MUhatGt d p q =>
      match clock_of root (MUhatGt d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let Q := LAnd (LAtom (LCGt x d)) (LUntil A B) in
      let Gamma := LOr (LUntil A Q) (LAnd (LG A) (LGF B)) in
      LAnd (LAtom (LRst x)) (LNext Gamma)
      end

  | MRhatLt d p q =>
      match clock_of root (MRhatLt d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let D := LOr (LAtom (LCGe x d)) (LAnd A B) in
      LAnd (LAtom (LRst x)) (LNext (LW B D))
      end

  | MRhatGt d p q =>
      match clock_of root (MRhatGt d p q) with
      | None => LFalse
      | Some x =>
      let A := T_at (left_path path) p in
      let B := T_at (right_path path) q in
      let E := LOr (LRelease A B) (LAnd (LAtom (LCLe x d)) A) in
      let K := LAnd (LAtom (LCLe x d)) (LAtom (LUnch x)) in
      LNext (LW K E)
      end
  end.

(* The clock mentioned by an atom, if any. *)
Definition latom_clock (a : latom) : option (Clock root) :=
  match a with
  | LAct _ | LNAct _ => None
  | LCLe x _ | LCLt x _ | LCGe x _ | LCGt x _ | LRst x | LUnch x => Some x
  end.

Fixpoint ltl_clocks (f : ltl) : list (Clock root) :=
  match f with
  | LTrue | LFalse => []
  | LAtom a =>
      match latom_clock a with
      | Some x => [x]
      | None => []
      end
  | LAnd p q | LOr p q | LUntil p q | LRelease p q =>
      ltl_clocks p ++ ltl_clocks q
  | LNext p => ltl_clocks p
  end.

(* ====================================================================== *)
(* 8. Automata returned by the LTL-to-Buchi back-end                      *)
(* ====================================================================== *)

(* A Buchi letter contains only the visible, untimed action proposition.
   Timing data and clock valuations belong to the timed interpretation, not
   to the input alphabet of the ordinary LTL-to-Buchi automaton. *)
Record letter : Type := {
  letter_action : Action
}.

Definition letter_of (rho : ext_word) (i : nat) : letter :=
  {| letter_action := tw_action (ew_base rho) i |}.

(* Action literals: (a, true) stands for the action a, (a, false) for any
   action other than a.  Transition labels are lists of action literals. *)
Definition alit : Type := (Action * bool)%type.

Definition alit_holds (p : Action) (l : alit) : Prop :=
  if snd l then p = fst l else p <> fst l.

Definition label_holds (ls : list alit) (le : letter) : Prop :=
  Forall (alit_holds (letter_action le)) ls.

(* The LTL-to-Buchi back-end treats the atoms of the extended alphabet as
   independent propositions.  Its input is a propositional word: a truth
   value for every atom at every position. *)
Definition pword : Type := nat -> latom -> Prop.

Fixpoint psat (s : pword) (i : nat) (f : ltl) : Prop :=
  match f with
  | LTrue => True
  | LFalse => False
  | LAtom a => s i a
  | LAnd p q => psat s i p /\ psat s i q
  | LOr p q => psat s i p \/ psat s i q
  | LNext p => psat s (S i) p
  | LUntil p q =>
      exists j,
        (i <= j)%nat /\ psat s j q /\ (forall k, (i <= k < j)%nat -> psat s k p)
  | LRelease p q =>
      forall j,
        (i <= j)%nat ->
        psat s j q \/ exists k, (i <= k < j)%nat /\ psat s k p
  end.

(* The propositional word of an extended word: the truth values of its
   atoms. *)
Definition word_of (rho : ext_word) : pword := fun i a => atom_sat rho i a.

Fixpoint ltl_atoms (f : ltl) : list latom :=
  match f with
  | LTrue | LFalse => []
  | LAtom a => [a]
  | LAnd p q | LOr p q | LUntil p q | LRelease p q => ltl_atoms p ++ ltl_atoms q
  | LNext p => ltl_atoms p
  end.

(* Literals: (a, true) is the atom a, (a, false) its negation.  The
   transitions of the automaton returned by the back-end are labelled by
   cubes, i.e. lists of literals. *)
Definition plit : Type := (latom * bool)%type.

Definition plit_holds (s : pword) (i : nat) (l : plit) : Prop :=
  if snd l then s i (fst l) else ~ s i (fst l).

Record ptransition : Type := {
  pt_src : nat;
  pt_label : list plit;
  pt_tgt : nat
}.

Record PBuchi : Type := {
  pb_nstates : nat;
  pb_init : nat;
  pb_trans : list ptransition;
  pb_accepting : list nat
}.

Definition PBA_accepts (A : PBuchi) (s : pword) : Prop :=
  exists run : nat -> nat,
    run 0%nat = pb_init A /\
    (forall i,
       (run i < pb_nstates A)%nat /\
       exists t,
         In t (pb_trans A) /\
         pt_src t = run i /\
         pt_tgt t = run (S i) /\
         Forall (plit_holds s i) (pt_label t)) /\
    (forall n, exists j, (n <= j)%nat /\ In (run j) (pb_accepting A)).

(* A transition of a timed automaton is read along an extended word only if
   the reset decisions of the word are exactly its reset list. *)
Definition resets_match
    (rho : ext_word) (i : nat) (resets : list (Clock root)) : Prop :=
  forall x, ew_reset rho i x = true <-> In x resets.

(* ====================================================================== *)
(* 9. Timed Buchi automata                                                *)
(* ====================================================================== *)

Inductive clock_comparison : Type :=
| CLe | CLt | CGe | CGt | CEq.

Record clock_constraint : Type := {
  guard_clock : Clock root;
  guard_comparison : clock_comparison;
  guard_bound : R
}.

Definition guard := list (option clock_constraint).

Definition atom_to_guard (a : latom) : option clock_constraint :=
  match a with
  | LCLe x d =>
      Some {| guard_clock := x; guard_comparison := CLe; guard_bound := d |}
  | LCLt x d =>
      Some {| guard_clock := x; guard_comparison := CLt; guard_bound := d |}
  | LCGe x d =>
      Some {| guard_clock := x; guard_comparison := CGe; guard_bound := d |}
  | LCGt x d =>
      Some {| guard_clock := x; guard_comparison := CGt; guard_bound := d |}
  | _ => None
  end.

Definition clock_constraint_holds
    (rho : ext_word) (i : nat) (c : clock_constraint) : Prop :=
  match guard_comparison c with
  | CLe => ew_val rho i (guard_clock c) <= guard_bound c
  | CLt => ew_val rho i (guard_clock c) <  guard_bound c
  | CGe => guard_bound c <= ew_val rho i (guard_clock c)
  | CGt => guard_bound c <  ew_val rho i (guard_clock c)
  | CEq => ew_val rho i (guard_clock c) = guard_bound c
  end.

Definition guard_item_holds
    (rho : ext_word) (i : nat) (c : option clock_constraint) : Prop :=
  match c with
  | Some c' => clock_constraint_holds rho i c'
  | None => True
  end.

Record tba_transition : Type := {
  bt_source : nat;
  bt_label : list alit;
  bt_guard : guard;
  bt_resets : list (Clock root);
  bt_target : nat
}.

(* The Timed Buchi Automaton produced by the construction.  Its clocks are
   the elements of [Clock root]. *)
Record TBA : Type := {
  tba_nstates : nat;
  tba_init : nat;
  tba_transitions : list tba_transition;
  tba_accepting : list nat
}.

Definition tba_transition_enabled
    (rho : ext_word) (i : nat) (t : tba_transition) : Prop :=
  label_holds (bt_label t) (letter_of rho i) /\
  Forall (guard_item_holds rho i) (bt_guard t) /\
  resets_match rho i (bt_resets t).

Definition TBA_ext_accepts (A : TBA) (rho : ext_word) : Prop :=
  exists run : nat -> nat,
    run 0%nat = tba_init A /\
    (forall i,
       (run i < tba_nstates A)%nat /\
       exists t,
         In t (tba_transitions A) /\
         bt_source t = run i /\
         bt_target t = run (S i) /\
         tba_transition_enabled rho i t) /\
    (forall n,
       exists j,
         (n <= j)%nat /\ In (run j) (tba_accepting A)).

Definition TBA_accepts (A : TBA) (w : timed_word) : Prop :=
  exists rho : ext_word,
    same_base rho w /\
    clock_consistent rho /\
    TBA_ext_accepts A rho.

End Clocks.

Arguments LAct {root} _.
Arguments LNAct {root} _.
Arguments LCLe {root} _ _.
Arguments LCLt {root} _ _.
Arguments LCGe {root} _ _.
Arguments LCGt {root} _ _.
Arguments LRst {root} _.
Arguments LUnch {root} _.
Arguments LTrue {root}.
Arguments LFalse {root}.
Arguments LAtom {root} _.
Arguments LAnd {root} _ _.
Arguments LOr {root} _ _.
Arguments LNext {root} _.
Arguments LUntil {root} _ _.
Arguments LRelease {root} _ _.
Arguments LF {root} _.
Arguments LG {root} _.
Arguments LW {root} _ _.
Arguments LGF {root} _.
Arguments T_at {root} _ _.
Arguments atom_to_guard {root} _.


Definition T (f : mtl) : ltl f := T_at (root:=f) [] f.

(* ====================================================================== *)
(* 7b. Every clock of [root] is used by the translation                   *)
(* ====================================================================== *)

(* The clocks occurring in [T f] are elements of [Clock f] by typing, i.e.
   timed subformulas of [f]; conversely every timed subformula of [f]
   occurs as a clock of [T f]. *)
Lemma T_at_uses_clocks :
  forall root f path,
    incl (timed_subformulas f) (timed_subformulas root) ->
    forall x : Clock root,
      In (proj1_sig x) (timed_subformulas f) ->
      In x (ltl_clocks (T_at (root:=root) path f)).
Proof.
  intros root f.
  induction f as [ | | a | a
                 | p IHp q IHq | p IHp q IHq | p IHp
                 | p IHp q IHq | p IHp q IHq
                 | d p IHp q IHq | d p IHp q IHq | d p IHp q IHq
                 | d p IHp q IHq | d p IHp q IHq | d p IHp q IHq
                 | d p IHp q IHq | d p IHp q IHq ];
    intros path Hincl x Hx; simpl in Hx; try contradiction.
  (* binary and unary untimed cases *)
  all: try (simpl; rewrite in_app_iff;
            apply in_app_or in Hx; destruct Hx as [Hx|Hx];
            [ left; apply IHp; [intros y Hy; apply Hincl; apply in_or_app; left; exact Hy | exact Hx]
            | right; apply IHq; [intros y Hy; apply Hincl; apply in_or_app; right; exact Hy | exact Hx]]).
  all: try (simpl; apply IHp; [intros y Hy; apply Hincl; exact Hy | exact Hx]).
  (* timed cases *)
  all:
    match goal with
    | |- In _ (ltl_clocks (T_at ?path ?F)) =>
        assert (HF : In F (timed_subformulas root)) by (apply Hincl; left; reflexivity);
        assert (Hp : incl (timed_subformulas p) (timed_subformulas root))
          by (intros y Hy; apply Hincl; right; apply in_or_app; left; exact Hy);
        assert (Hq : incl (timed_subformulas q) (timed_subformulas root))
          by (intros y Hy; apply Hincl; right; apply in_or_app; right; exact Hy);
        simpl; rewrite (clock_of_mem HF);
        unfold LW, LGF, LG, LF;
        repeat (first [rewrite in_app_iff | progress simpl]);
        destruct Hx as [Hx | Hx];
        [ assert (Hxe : x = exist _ F HF)
            by (apply clock_eq; simpl; symmetry; exact Hx);
          subst x; intuition
        | apply in_app_or in Hx; destruct Hx as [Hx | Hx];
          [ pose proof (IHp (left_path path) Hp x Hx); intuition
          | pose proof (IHq (right_path path) Hq x Hx); intuition ] ]
    end.
Qed.

Theorem T_uses_every_clock :
  forall (f : mtl) (x : Clock f), In x (ltl_clocks (T f)).
Proof.
  intros f x. unfold T.
  apply T_at_uses_clocks.
  - intros y Hy; exact Hy.
  - exact (proj2_sig x).
Qed.

Arguments word_of {root} _ _ _.
Arguments psat {root} _ _ _.
Arguments ltl_atoms {root} _.
Arguments plit_holds {root} _ _ _.

(* ====================================================================== *)
(* 9b. Propositional semantics of LTL over the extended alphabet          *)
(* ====================================================================== *)

Section Completion.

Variable root : mtl.

Lemma lsat_psat :
  forall (rho : ext_word root) (f : ltl root) i,
    lsat rho i f <-> psat (word_of rho) i f.
Proof.
  intros rho f.
  induction f as [| | a | p IHp q IHq | p IHp q IHq | p IHp | p IHp q IHq | p IHp q IHq];
    intro i; simpl.
  - tauto.
  - tauto.
  - unfold word_of. tauto.
  - rewrite IHp, IHq. tauto.
  - rewrite IHp, IHq. tauto.
  - apply IHp.
  - split; intros [j [Hij [Hq Hp]]]; exists j; split; try exact Hij;
      split; try (apply IHq; exact Hq);
      intros k Hk; apply IHp; apply Hp; exact Hk.
  - split; intros H j Hj; destruct (H j Hj) as [Hq | [k [Hk Hp]]];
      try (left; apply IHq; exact Hq);
      right; exists k; split; try exact Hk; apply IHp; exact Hp.
Qed.

(* LTL formulas have no negation: satisfaction is monotone in the atoms. *)
Lemma psat_mono :
  forall (f : ltl root) (s s' : pword root),
    (forall i a, In a (ltl_atoms f) -> s i a -> s' i a) ->
    forall i, psat s i f -> psat s' i f.
Proof.
  intros f s s'.
  induction f as [| | a | p IHp q IHq | p IHp q IHq | p IHp | p IHp q IHq | p IHp q IHq];
    intros Hm i H; simpl in *.
  - exact I.
  - exact H.
  - apply Hm; [left; reflexivity | exact H].
  - assert (Hp : forall i a, In a (ltl_atoms p) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; left; exact Ha).
    assert (Hq : forall i a, In a (ltl_atoms q) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; right; exact Ha).
    destruct H as [H1 H2]. split; [exact (IHp Hp i H1) | exact (IHq Hq i H2)].
  - assert (Hp : forall i a, In a (ltl_atoms p) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; left; exact Ha).
    assert (Hq : forall i a, In a (ltl_atoms q) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; right; exact Ha).
    destruct H as [H1|H2]; [left; exact (IHp Hp i H1) | right; exact (IHq Hq i H2)].
  - exact (IHp Hm (S i) H).
  - assert (Hp : forall i a, In a (ltl_atoms p) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; left; exact Ha).
    assert (Hq : forall i a, In a (ltl_atoms q) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; right; exact Ha).
    destruct H as [j [Hij [Hj Hk]]]. exists j. split; [exact Hij|].
    split; [exact (IHq Hq j Hj)|]. intros k Hk'. exact (IHp Hp k (Hk k Hk')).
  - assert (Hp : forall i a, In a (ltl_atoms p) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; left; exact Ha).
    assert (Hq : forall i a, In a (ltl_atoms q) -> s i a -> s' i a)
      by (intros k a Ha; apply Hm; apply in_or_app; right; exact Ha).
    intros j Hj. destruct (H j Hj) as [Hj' | [k [Hk Hk']]].
    + left. exact (IHq Hq j Hj').
    + right. exists k. split; [exact Hk | exact (IHp Hp k Hk')].
Qed.

(* ====================================================================== *)
(* 9c. Choice of a transition along a run                                 *)
(* ====================================================================== *)

(* The first element of a list that satisfies [P] (classical decision). *)
Fixpoint first_such {A : Type} (P : A -> Prop) (l : list A) : option A :=
  match l with
  | [] => None
  | x :: l' => if decide_b (P x) then Some x else first_such P l'
  end.

Lemma first_such_spec :
  forall (A : Type) (P : A -> Prop) (l : list A),
    (exists x, In x l /\ P x) ->
    exists x, first_such P l = Some x /\ In x l /\ P x.
Proof.
  intros A P l. induction l as [|y l IH]; intros [x [Hx HP]]; [contradiction|].
  simpl. destruct (decide_b (P y)) eqn:E.
  - exists y. split; [reflexivity|]. split; [left; reflexivity|].
    apply decide_b_spec. exact E.
  - destruct Hx as [<-|Hx].
    + exfalso. apply decide_b_spec in HP. congruence.
    + destruct (IH (ex_intro _ x (conj Hx HP))) as [z [Hz [Hin HPz]]].
      exists z. split; [exact Hz|]. split; [right; exact Hin | exact HPz].
Qed.

(* ====================================================================== *)
(* 9d. Decidable equality of atoms                                        *)
(* ====================================================================== *)

Definition latom_eq_dec : forall a b : latom root, {a = b} + {a <> b}.
Proof.
  decide equality;
    first [apply Nat.eq_dec | apply Req_EM_T | apply clock_eq_dec].
Defined.

Definition latom_eqb (a b : latom root) : bool :=
  if latom_eq_dec a b then true else false.

Lemma latom_eqb_true : forall a b, latom_eqb a b = true <-> a = b.
Proof.
  intros a b. unfold latom_eqb.
  destruct (latom_eq_dec a b); split; intro H; congruence.
Qed.

Definition in_atoms (a : latom root) (l : list (latom root)) : bool :=
  existsb (latom_eqb a) l.

Lemma in_atoms_iff : forall a l, in_atoms a l = true <-> In a l.
Proof.
  intros a l. unfold in_atoms. rewrite existsb_exists. split.
  - intros [b [Hb E]]. apply latom_eqb_true in E. subst b. exact Hb.
  - intro H. exists a. split; [exact H|]. apply latom_eqb_true. reflexivity.
Qed.

(* ====================================================================== *)
(* 9e. Relaxation                                                         *)
(* ====================================================================== *)

(* Extended atoms: clock guards and reset/unchanged markers. *)
Definition is_ext (a : latom root) : bool :=
  match a with
  | LAct _ | LNAct _ => false
  | _ => true
  end.

Definition lit_conflict (p q : plit root) : bool :=
  latom_eqb (fst p) (fst q) && xorb (snd p) (snd q).

(* A cube is consistent when it contains no atom together with its
   negation; an inconsistent cube is never satisfied. *)
Definition consistent (l : list (plit root)) : bool :=
  forallb (fun p => negb (existsb (lit_conflict p) l)) l.

Lemma consistent_holds :
  forall (s : pword root) i l, Forall (plit_holds s i) l -> consistent l = true.
Proof.
  intros s i l H. unfold consistent. apply forallb_forall. intros p Hp.
  apply negb_true_iff. destruct (existsb (lit_conflict p) l) eqn:E; [|reflexivity].
  exfalso. apply existsb_exists in E. destruct E as [q [Hq Hc]].
  rewrite Forall_forall in H. pose proof (H p Hp) as H1. pose proof (H q Hq) as H2.
  destruct p as [a b], q as [a' b']. unfold lit_conflict in Hc. simpl in *.
  apply andb_true_iff in Hc. destruct Hc as [Ha Hb].
  apply latom_eqb_true in Ha. subst a'.
  unfold plit_holds in *. simpl in *.
  destruct b, b'; simpl in Hb; try discriminate; tauto.
Qed.

Lemma consistent_no_conflict :
  forall l a, consistent l = true -> In (a, true) l -> In (a, false) l -> False.
Proof.
  intros l a Hc H1 H2. unfold consistent in Hc. rewrite forallb_forall in Hc.
  specialize (Hc _ H1). apply negb_true_iff in Hc.
  assert (E : existsb (lit_conflict (a, true)) l = true).
  { apply existsb_exists. exists (a, false). split; [exact H2|].
    unfold lit_conflict. simpl. rewrite andb_true_iff. split; [|reflexivity].
    apply latom_eqb_true. reflexivity. }
  congruence.
Qed.

(* Relaxation keeps the action literals and the positive literals over the
   extended atoms of the formula [F]; the other literals over extended atoms
   are dropped.  Inconsistent cubes are removed. *)
Definition keep_lit (F : list (latom root)) (p : plit root) : bool :=
  negb (is_ext (fst p)) || (snd p && in_atoms (fst p) F).

Definition relax_trans (F : list (latom root)) (t : ptransition root) : ptransition root :=
  {| pt_src := pt_src t; pt_label := filter (keep_lit F) (pt_label t); pt_tgt := pt_tgt t |}.

Definition relax (F : list (latom root)) (A : PBuchi root) : PBuchi root :=
  {| pb_nstates := pb_nstates A;
     pb_init := pb_init A;
     pb_trans := map (relax_trans F) (filter (fun t => consistent (pt_label t)) (pb_trans A));
     pb_accepting := pb_accepting A |}.

Lemma relax_complete :
  forall F A (s : pword root), PBA_accepts A s -> PBA_accepts (relax F A) s.
Proof.
  intros F A s [run [Hinit [Hsteps Hacc]]].
  exists run. split; [exact Hinit|]. split; [|exact Hacc].
  intro i. destruct (Hsteps i) as [Hb [t [Ht [Hs [Hd Hl]]]]].
  split; [exact Hb|]. exists (relax_trans F t). split.
  - simpl. apply in_map. apply filter_In. split; [exact Ht|].
    exact (consistent_holds Hl).
  - simpl. split; [exact Hs|]. split; [exact Hd|].
    rewrite Forall_forall in *. intros p Hp. apply filter_In in Hp.
    apply Hl. exact (proj1 Hp).
Qed.

(* Relaxation preserves the language: a relaxed run on [s] is a run of the
   original automaton on a word [s'] that differs from [s] only on extended
   atoms, which it makes false (dropped negative literals) or true (dropped
   atoms outside the formula); by monotonicity, [s] satisfies the formula. *)
Lemma relax_sound :
  forall (f : ltl root) A (s : pword root),
    (forall s', PBA_accepts A s' -> psat s' 0 f) ->
    PBA_accepts (relax (ltl_atoms f) A) s -> psat s 0 f.
Proof.
  intros f A s Hax [run [Hinit [Hsteps Hacc]]].
  set (F := ltl_atoms f).
  set (P := fun i (t : ptransition root) =>
              consistent (pt_label t) = true /\ pt_src t = run i /\
              pt_tgt t = run (S i) /\
              Forall (plit_holds s i) (pt_label (relax_trans F t))).
  assert (Hex : forall i, exists t, In t (pb_trans A) /\ P i t).
  { intro i. destruct (Hsteps i) as [_ [t' [Ht' [Hs [Hd Hl]]]]].
    simpl in Ht'. apply in_map_iff in Ht'. destruct Ht' as [t [<- Ht]].
    apply filter_In in Ht. destruct Ht as [Ht Hc].
    exists t. split; [exact Ht|]. split; [exact Hc|]. split; [exact Hs|].
    split; [exact Hd | exact Hl]. }
  set (lab := fun i => match first_such (P i) (pb_trans A) with
                       | Some t => pt_label t | None => [] end).
  set (s' := fun i (a : latom root) =>
               if is_ext a
               then (In (a, true) (lab i) /\ ~ In a F) \/ (s i a /\ ~ In (a, false) (lab i))
               else s i a).
  assert (Hrun : PBA_accepts A s').
  { exists run. split; [exact Hinit|]. split; [|exact Hacc].
    intro i. split; [exact (proj1 (Hsteps i))|].
    destruct (first_such_spec (Hex i)) as [t [Hfs [Ht [Hc [Hs [Hd Hl]]]]]].
    exists t. split; [exact Ht|]. split; [exact Hs|]. split; [exact Hd|].
    assert (Hlab : lab i = pt_label t) by (unfold lab; rewrite Hfs; reflexivity).
    apply Forall_forall. intros [a b] Hab.
    rewrite Forall_forall in Hl. simpl in Hl.
    unfold plit_holds. simpl. unfold s'. rewrite Hlab.
    destruct (is_ext a) eqn:Hext.
    - destruct b.
      + destruct (in_atoms a F) eqn:HF.
        * right. split.
          -- assert (Hk : In (a, true) (filter (keep_lit F) (pt_label t))).
             { apply filter_In. split; [exact Hab|].
               unfold keep_lit. simpl. rewrite Hext, HF. reflexivity. }
             exact (Hl _ Hk).
          -- intro Hf. exact (consistent_no_conflict Hc Hab Hf).
        * left. split; [exact Hab|]. intro Hin.
          apply in_atoms_iff in Hin. congruence.
      + intros [[Ht' _] | [_ Hn]].
        * exact (consistent_no_conflict Hc Ht' Hab).
        * exact (Hn Hab).
    - assert (Hk : In (a, b) (filter (keep_lit F) (pt_label t))).
      { apply filter_In. split; [exact Hab|].
        unfold keep_lit. simpl. rewrite Hext. reflexivity. }
      exact (Hl _ Hk). }
  apply (psat_mono (s := s')); [|exact (Hax s' Hrun)].
  intros i a Ha H. unfold s' in H. destruct (is_ext a).
  - destruct H as [[_ Hn] | [H _]]; [contradiction | exact H].
  - exact H.
Qed.

(* ====================================================================== *)
(* 9f. Preservation and restart clocks                                    *)
(* ====================================================================== *)

(* The clock of a lower-bounded Until or of an upper-bounded Release is a
   restart clock: it is tested only against lower bounds and constrained
   only by reset markers.  The other clocks are preservation clocks: tested
   only against upper bounds and constrained only by unchanged markers. *)
Definition is_restart (x : Clock root) : bool :=
  match proj1_sig x with
  | MUhatGe _ _ _ | MUhatGt _ _ _ | MRhatLe _ _ _ | MRhatLt _ _ _ => true
  | _ => false
  end.

Definition atom_ok (a : latom root) : Prop :=
  match a with
  | LCLe x _ | LCLt x _ | LUnch x => is_restart x = false
  | LCGe x _ | LCGt x _ | LRst x => is_restart x = true
  | _ => True
  end.

Lemma T_at_atoms_ok :
  forall f path a, In a (ltl_atoms (T_at (root:=root) path f)) -> atom_ok a.
Proof.
  intro f.
  induction f as [ | | b | b
                 | p IHp q IHq | p IHp q IHq | p IHp
                 | p IHp q IHq | p IHp q IHq
                 | d p IHp q IHq | d p IHp q IHq | d p IHp q IHq
                 | d p IHp q IHq | d p IHp q IHq | d p IHp q IHq
                 | d p IHp q IHq | d p IHp q IHq ];
    intros path a H; simpl in H.
  all: try contradiction.
  all: try (destruct H as [<- | []]; exact I).
  all: try (apply in_app_or in H; destruct H as [H|H];
            [exact (IHp _ _ H) | exact (IHq _ _ H)]).
  all: try exact (IHp _ _ H).
  all: destruct (clock_of root _) as [x|] eqn:Hc; [|contradiction].
  all: pose proof (clock_of_proj Hc) as Hx.
  all: unfold LW, LGF, LG, LF in H; simpl in H.
  all: repeat (first [rewrite in_app_iff in H | progress simpl in H]).
  all: repeat match goal with
              | H : _ \/ _ |- _ => destruct H as [H|H]
              end.
  all: try contradiction.
  all: try (subst a; unfold atom_ok, is_restart; rewrite Hx; reflexivity).
  all: first [exact (IHp _ _ H) | exact (IHq _ _ H)].
Qed.

(* The clocks of [root], listed. *)
Definition all_clocks : list (Clock root) :=
  flat_map (fun F => match clock_of root F with Some x => [x] | None => [] end)
           (timed_subformulas root).

Lemma all_clocks_complete : forall x, In x all_clocks.
Proof.
  intros [F HF]. unfold all_clocks. apply in_flat_map. exists F.
  split; [exact HF|]. rewrite (clock_of_mem HF). left. reflexivity.
Qed.

(* ====================================================================== *)
(* 9g. Reset completion                                                   *)
(* ====================================================================== *)

Definition has_pos (a : latom root) (l : list (plit root)) : bool :=
  existsb (fun p => latom_eqb (fst p) a && snd p) l.

Lemma has_pos_iff : forall a l, has_pos a l = true <-> In (a, true) l.
Proof.
  intros a l. unfold has_pos. rewrite existsb_exists. split.
  - intros [[b c] [Hin H]]. apply andb_true_iff in H. destruct H as [Hb Hc].
    apply latom_eqb_true in Hb. simpl in *. subst. exact Hin.
  - intro H. exists (a, true). split; [exact H|]. simpl.
    rewrite andb_true_r. apply latom_eqb_true. reflexivity.
Qed.

(* A preservation clock is reset unless the cube contains its unchanged
   marker; a restart clock is reset exactly when the cube contains its reset
   marker. *)
Definition comp_resets (l : list (plit root)) : list (Clock root) :=
  filter (fun x => if is_restart x then has_pos (LRst x) l
                   else negb (has_pos (LUnch x) l)) all_clocks.

Definition act_lit (p : plit root) : list alit :=
  match fst p with
  | LAct q => [(q, snd p)]
  | LNAct q => [(q, negb (snd p))]
  | _ => []
  end.

Definition pos_atoms (l : list (plit root)) : list (latom root) := map fst (filter snd l).

Definition complete_trans (t : ptransition root) : tba_transition root :=
  {| bt_source := pt_src t;
     bt_label := flat_map act_lit (pt_label t);
     bt_guard := map atom_to_guard (pos_atoms (pt_label t));
     bt_resets := comp_resets (pt_label t);
     bt_target := pt_tgt t |}.

Definition complete (A : PBuchi root) : TBA root :=
  {| tba_nstates := pb_nstates A;
     tba_init := pb_init A;
     tba_transitions := map complete_trans (pb_trans A);
     tba_accepting := pb_accepting A |}.

(* Every literal over an extended atom is positive and of the right kind. *)
Definition labels_ok (A : PBuchi root) : Prop :=
  forall t a b, In t (pb_trans A) -> In (a, b) (pt_label t) ->
    is_ext a = true -> b = true /\ atom_ok a.

Lemma relax_labels_ok :
  forall F A, (forall a, In a F -> atom_ok a) -> labels_ok (relax F A).
Proof.
  intros F A HF t a b Ht Hab Hext. simpl in Ht.
  apply in_map_iff in Ht. destruct Ht as [t0 [<- _]].
  simpl in Hab. apply filter_In in Hab. destruct Hab as [_ Hk].
  unfold keep_lit in Hk. simpl in Hk. rewrite Hext in Hk. simpl in Hk.
  apply andb_true_iff in Hk. destruct Hk as [Hb Hin].
  split; [exact Hb|]. apply HF. apply in_atoms_iff. exact Hin.
Qed.

Lemma in_comp_resets :
  forall l x,
    In x (comp_resets l) <->
    (if is_restart x then In (LRst x, true) l else ~ In (LUnch x, true) l).
Proof.
  intros l x. unfold comp_resets. rewrite filter_In.
  split.
  - intros [_ H]. destruct (is_restart x).
    + apply has_pos_iff. exact H.
    + intro Hin. apply has_pos_iff in Hin. rewrite Hin in H. discriminate.
  - intro H. split; [apply all_clocks_complete|]. destruct (is_restart x).
    + apply has_pos_iff. exact H.
    + destruct (has_pos (LUnch x) l) eqn:E; [|reflexivity].
      exfalso. apply H. apply has_pos_iff. exact E.
Qed.

(* Soundness: a run of the completed automaton is a run of the relaxed
   automaton on the propositional word of the same extended word. *)
Lemma complete_trans_sound :
  forall (rho : ext_word root) i t,
    (forall a b, In (a, b) (pt_label t) -> is_ext a = true -> b = true /\ atom_ok a) ->
    tba_transition_enabled rho i (complete_trans t) ->
    Forall (plit_holds (word_of rho) i) (pt_label t).
Proof.
  intros rho i t Hok [Hlab [Hg Hres]].
  unfold complete_trans, label_holds in *. simpl in *.
  rewrite Forall_forall in Hlab, Hg. apply Forall_forall.
  intros [a b] Hin. unfold plit_holds, word_of. simpl.
  destruct a as [q|q|x d|x d|x d|x d|x|x].
  - assert (H : alit_holds (tw_action (ew_base rho) i) (q, b)).
    { apply Hlab. apply in_flat_map. exists (LAct q, b). split; [exact Hin|].
      left. reflexivity. }
    unfold alit_holds in H. simpl in H. destruct b; exact H.
  - assert (H : alit_holds (tw_action (ew_base rho) i) (q, negb b)).
    { apply Hlab. apply in_flat_map. exists (LNAct q, b). split; [exact Hin|].
      left. reflexivity. }
    unfold alit_holds in H. simpl in *. destruct b; simpl in H; [exact H|].
    intro Hn. exact (Hn H).
  - destruct (Hok _ _ Hin eq_refl) as [-> _].
    assert (Hp : In (LCLe x d) (pos_atoms (pt_label t)))
      by (apply in_map_iff; exists (LCLe x d, true); split;
          [reflexivity | apply filter_In; split; [exact Hin | reflexivity]]).
    exact (Hg _ (in_map _ _ _ Hp)).
  - destruct (Hok _ _ Hin eq_refl) as [-> _].
    assert (Hp : In (LCLt x d) (pos_atoms (pt_label t)))
      by (apply in_map_iff; exists (LCLt x d, true); split;
          [reflexivity | apply filter_In; split; [exact Hin | reflexivity]]).
    exact (Hg _ (in_map _ _ _ Hp)).
  - destruct (Hok _ _ Hin eq_refl) as [-> _].
    assert (Hp : In (LCGe x d) (pos_atoms (pt_label t)))
      by (apply in_map_iff; exists (LCGe x d, true); split;
          [reflexivity | apply filter_In; split; [exact Hin | reflexivity]]).
    exact (Hg _ (in_map _ _ _ Hp)).
  - destruct (Hok _ _ Hin eq_refl) as [-> _].
    assert (Hp : In (LCGt x d) (pos_atoms (pt_label t)))
      by (apply in_map_iff; exists (LCGt x d, true); split;
          [reflexivity | apply filter_In; split; [exact Hin | reflexivity]]).
    exact (Hg _ (in_map _ _ _ Hp)).
  - destruct (Hok _ _ Hin eq_refl) as [-> Hk]. simpl in Hk.
    unfold rst_at. apply (proj2 (Hres x)). apply in_comp_resets.
    rewrite Hk. exact Hin.
  - destruct (Hok _ _ Hin eq_refl) as [-> Hk]. simpl in Hk.
    unfold unch_at. apply Bool.not_true_iff_false. intro E.
    apply (proj1 (Hres x)) in E. apply in_comp_resets in E.
    rewrite Hk in E. contradiction.
Qed.

Lemma complete_sound :
  forall A (rho : ext_word root),
    labels_ok A -> TBA_ext_accepts (complete A) rho -> PBA_accepts A (word_of rho).
Proof.
  intros A rho Hok [run [Hinit [Hsteps Hacc]]].
  exists run. split; [exact Hinit|]. split; [|exact Hacc].
  intro i. destruct (Hsteps i) as [Hb [tt [Ht [Hs [Hd Hen]]]]].
  split; [exact Hb|]. simpl in Ht. apply in_map_iff in Ht. destruct Ht as [t [<- Ht]].
  exists t. split; [exact Ht|]. split; [exact Hs|]. split; [exact Hd|].
  apply (complete_trans_sound (rho := rho) (i := i)); [|exact Hen].
  intros a b Hab Hext. exact (Hok t a b Ht Hab Hext).
Qed.

(* Clock values determined by an initial valuation and reset decisions. *)
Fixpoint vals (v0 : Clock root -> R) (r : nat -> Clock root -> bool)
         (w : timed_word) (i : nat) (x : Clock root) : R :=
  match i with
  | O => v0 x
  | S j => if r j x then delta w j else vals v0 r w j x + delta w j
  end.

Lemma vals_nonneg :
  forall v0 r w, (forall x, 0 <= v0 x) -> forall i x, 0 <= vals v0 r w i x.
Proof.
  intros v0 r w H0 i x. induction i as [|j IH]; simpl; [apply H0|].
  pose proof (delta_positive w j). destruct (r j x); lra.
Qed.

(* Completeness (Lemma IV.1 of the paper): along a run of the relaxed
   automaton on the propositional word of a clock-consistent extended word
   [rho], the extended word [rho'] with the same timed word and initial
   values, and with the resets of the completion, is accepted by the
   completed automaton.  Preservation clocks of [rho'] are reset wherever
   those of [rho] are, so their values are smaller and their upper bounds
   still hold; restart clocks of [rho'] are reset only where those of [rho]
   are, so their values are larger and their lower bounds still hold. *)
Lemma complete_complete :
  forall A (rho : ext_word root),
    clock_consistent rho -> labels_ok A -> PBA_accepts A (word_of rho) ->
    exists rho' : ext_word root,
      ew_base rho' = ew_base rho /\ clock_consistent rho' /\
      TBA_ext_accepts (complete A) rho'.
Proof.
  intros A rho [Hnn Hcc] Hok [run [Hinit [Hsteps Hacc]]].
  set (P := fun i (t : ptransition root) =>
              pt_src t = run i /\ pt_tgt t = run (S i) /\
              Forall (plit_holds (word_of rho) i) (pt_label t)).
  assert (Hex : forall i, exists t, In t (pb_trans A) /\ P i t).
  { intro i. destruct (Hsteps i) as [_ [t [Ht H]]]. exists t. split; assumption. }
  set (lab := fun i => match first_such (P i) (pb_trans A) with
                       | Some t => pt_label t | None => [] end).
  assert (Hlab : forall i, exists t, first_such (P i) (pb_trans A) = Some t /\
                   In t (pb_trans A) /\ P i t /\ lab i = pt_label t).
  { intro i. destruct (first_such_spec (Hex i)) as [t [Hf [Ht HP]]].
    exists t. split; [exact Hf|]. split; [exact Ht|]. split; [exact HP|].
    unfold lab. rewrite Hf. reflexivity. }
  set (r := fun i x => if In_dec (@clock_eq_dec root) x (comp_resets (lab i))
                       then true else false).
  set (rho' := {| ew_base := ew_base rho;
                  ew_val := vals (ew_val rho 0) r (ew_base rho);
                  ew_reset := r |}).
  assert (Hr : forall i x, r i x = true <-> In x (comp_resets (lab i))).
  { intros i x. unfold r. destruct (In_dec _ x _); split; intro H;
      try reflexivity; try assumption; try discriminate; contradiction. }
  (* the literals of the chosen cube hold on rho *)
  assert (Hsat : forall i a b, In (a, b) (lab i) -> plit_holds (word_of rho) i (a, b)).
  { intros i a b Hin. destruct (Hlab i) as [t [_ [_ [[_ [_ Hl]] Heq]]]].
    rewrite Heq in Hin. rewrite Forall_forall in Hl. exact (Hl _ Hin). }
  assert (Hok' : forall i a b, In (a, b) (lab i) -> is_ext a = true ->
                   b = true /\ atom_ok a).
  { intros i a b Hin Hext. destruct (Hlab i) as [t [_ [Ht [_ Heq]]]].
    rewrite Heq in Hin. exact (Hok t a b Ht Hin Hext). }
  (* preservation clocks: smaller values *)
  assert (Hpres : forall x, is_restart x = false ->
                    forall i, vals (ew_val rho 0) r (ew_base rho) i x <= ew_val rho i x).
  { intros x Hx i. induction i as [|j IH]; simpl; [lra|].
    rewrite (Hcc j x). pose proof (delta_positive (ew_base rho) j).
    pose proof (Hnn j x).
    destruct (r j x) eqn:Erj.
    - destruct (ew_reset rho j x); lra.
    - destruct (ew_reset rho j x) eqn:E; [|lra].
      exfalso. assert (Hin : In x (comp_resets (lab j))).
      { apply in_comp_resets. rewrite Hx. intro HU.
        pose proof (Hsat _ _ _ HU) as HU'. unfold plit_holds, word_of in HU'.
        simpl in HU'. unfold unch_at in HU'. congruence. }
      apply Hr in Hin. congruence. }
  (* restart clocks: larger values *)
  assert (Hrest : forall x, is_restart x = true ->
                    forall i, ew_val rho i x <= vals (ew_val rho 0) r (ew_base rho) i x).
  { intros x Hx i. induction i as [|j IH]; simpl; [lra|].
    rewrite (Hcc j x). pose proof (delta_positive (ew_base rho) j).
    pose proof (@vals_nonneg (ew_val rho 0) r (ew_base rho) (Hnn 0%nat) j x).
    destruct (r j x) eqn:Erj.
    - apply Hr in Erj. apply in_comp_resets in Erj. rewrite Hx in Erj.
      pose proof (Hsat _ _ _ Erj) as HR. unfold plit_holds, word_of in HR.
      simpl in HR. unfold rst_at in HR. rewrite HR. lra.
    - destruct (ew_reset rho j x); lra. }
  exists rho'. split; [reflexivity|]. split.
  - split.
    + intros i x. apply vals_nonneg. apply Hnn.
    + intros i x. reflexivity.
  - exists run. split; [exact Hinit|]. split; [|exact Hacc].
    intro i. split; [exact (proj1 (Hsteps i))|].
    destruct (Hlab i) as [t [_ [Ht [[Hs [Hd Hl]] Heq]]]].
    exists (complete_trans t). split; [simpl; apply in_map; exact Ht|].
    split; [exact Hs|]. split; [exact Hd|].
    rewrite Forall_forall in Hl.
    split; [|split].
    + (* actions *)
      unfold label_holds. simpl. apply Forall_forall. intros [q c] Hq.
      apply in_flat_map in Hq. destruct Hq as [[a b] [Hab Hq]].
      pose proof (Hl _ Hab) as H. unfold plit_holds, word_of in H. simpl in H.
      unfold act_lit in Hq. simpl in Hq.
      destruct a as [q0|q0|x d|x d|x d|x d|x|x]; simpl in Hq; try contradiction.
      * destruct Hq as [Hq|[]]. injection Hq as <- <-.
        unfold alit_holds. simpl. destruct b; exact H.
      * destruct Hq as [Hq|[]]. injection Hq as <- <-.
        unfold alit_holds. simpl. destruct b; simpl in *; [exact H|].
        destruct (Nat.eq_dec (tw_action (ew_base rho) i) q0) as [E|E];
          [exact E | contradiction].
    + (* guards *)
      apply Forall_forall. intros o Ho. simpl in Ho.
      apply in_map_iff in Ho. destruct Ho as [a [<- Ha]].
      unfold pos_atoms in Ha. apply in_map_iff in Ha.
      destruct Ha as [[a' b] [Ea Ha]]. simpl in Ea. subst a'.
      apply filter_In in Ha. destruct Ha as [Ha Hb]. simpl in Hb. subst b.
      pose proof (Hl _ Ha) as H. unfold plit_holds, word_of in H. simpl in H.
      assert (Hin' : In (a, true) (lab i)) by (rewrite Heq; exact Ha).
      destruct a as [q|q|x d|x d|x d|x d|x|x]; simpl; try exact I;
        pose proof (proj2 (Hok' i _ _ Hin' eq_refl)) as Hk; simpl in Hk;
        simpl in H; unfold c_le, c_lt, c_ge, c_gt in H.
      * pose proof (Hpres x Hk i). unfold clock_constraint_holds, rho'. simpl. lra.
      * pose proof (Hpres x Hk i). unfold clock_constraint_holds, rho'. simpl. lra.
      * pose proof (Hrest x Hk i). unfold clock_constraint_holds, rho'. simpl. lra.
      * pose proof (Hrest x Hk i). unfold clock_constraint_holds, rho'. simpl. lra.
    + (* resets *)
      intro x. simpl. rewrite Hr, Heq. reflexivity.
Qed.

End Completion.

(* ====================================================================== *)
(* 9h. The ONE external axiom: LTL -> Buchi correctness                   *)
(* ====================================================================== *)

(* The external LTL-to-Buchi tool (Spot) treats the atoms of the extended
   alphabet as independent propositions: it returns a Buchi automaton whose
   transitions are labelled by cubes of literals and which accepts exactly
   the propositional words satisfying the formula. *)
Axiom LTL_TO_BUCHI_CORRECT :
  forall (root : mtl) (f : ltl root),
    { A : PBuchi root |
      forall s : pword root, PBA_accepts A s <-> psat s 0 f }.

Definition ltl_to_buchi (root : mtl) (f : ltl root) : PBuchi root :=
  proj1_sig (LTL_TO_BUCHI_CORRECT f).

Theorem ltl_to_buchi_correct :
  forall root (f : ltl root) (s : pword root),
    PBA_accepts (ltl_to_buchi f) s <-> psat s 0 f.
Proof.
  intros root f s. unfold ltl_to_buchi.
  destruct (LTL_TO_BUCHI_CORRECT f) as [A HA]. simpl. apply HA.
Qed.

(* ====================================================================== *)
(* 10. Timed Buchi automaton acceptance                                   *)
(* ====================================================================== *)

Theorem reinterpretation_correct :
  forall root (A : TBA root) w,
    TBA_accepts A w <->
    exists rho : ext_word root,
      same_base rho w /\
      clock_consistent rho /\
      TBA_ext_accepts A rho.
Proof.
  intros.
  reflexivity.
Qed.

(* ====================================================================== *)
(* 11. Exact semantic theorem still to be PROVED, stated separately       *)
(* ====================================================================== *)

Definition EncodingCorrect :=
  forall (f : mtl) (w : timed_word),
    well_formed f ->
    (msat w 0 f <->
     exists rho : ext_word f,
       same_base rho w /\
       clock_consistent rho /\
       lsat rho 0 (T f)).

(* ====================================================================== *)
(* 12. End-to-end composition                                             *)
(* ====================================================================== *)

(* The compiled TBA for a Buchi automaton [A] of [T f] produced by any
   LTL-to-Buchi translator: [A] relaxed and completed.  It has type [TBA f]:
   its clocks are the timed subformulas of [f]. *)
Definition compile_with (f : mtl) (A : PBuchi f) : TBA f :=
  complete (relax (ltl_atoms (T f)) A).

Lemma compile_labels_ok :
  forall f (A : PBuchi f), labels_ok (relax (ltl_atoms (T f)) A).
Proof.
  intros f A. apply relax_labels_ok. intros a Ha. unfold T in Ha.
  exact (T_at_atoms_ok Ha).
Qed.

Theorem compile_with_correct_from_encoding :
  forall (f : mtl) (A : PBuchi f) (w : timed_word),
    EncodingCorrect ->
    (forall s : pword f, PBA_accepts A s <-> psat s 0 (T f)) ->
    well_formed f ->
    (msat w 0 f <-> TBA_accepts (compile_with A) w).
Proof.
  intros f A w Henc HA Hwf.
  specialize (Henc f w Hwf).
  unfold compile_with, TBA_accepts.
  split.
  - intro Hm.
    apply (proj1 Henc) in Hm.
    destruct Hm as [rho [Hbase [Hclock Hltl]]].
    apply lsat_psat in Hltl.
    apply (proj2 (HA (word_of rho))) in Hltl.
    apply (relax_complete (ltl_atoms (T f))) in Hltl.
    destruct (complete_complete Hclock (@compile_labels_ok f A) Hltl)
      as [rho' [Hb' [Hc' Ha']]].
    exists rho'. split; [unfold same_base in *; rewrite Hb'; exact Hbase|].
    split; [exact Hc' | exact Ha'].
  - intros [rho [Hbase [Hclock Htba]]].
    apply (proj2 Henc).
    exists rho. split; [exact Hbase|]. split; [exact Hclock|].
    apply lsat_psat.
    apply (relax_sound (A := A)).
    + intros s' Hs'. apply HA. exact Hs'.
    + exact (complete_sound (@compile_labels_ok f A) Htba).
Qed.

(* The compiled TBA, with the axiomatized back-end. *)
Definition compile (f : mtl) : TBA f := compile_with (ltl_to_buchi (T f)).

Theorem MTL_to_TBA_correct_from_encoding :
  forall (f : mtl) (w : timed_word),
    EncodingCorrect ->
    well_formed f ->
    (msat w 0 f <-> TBA_accepts (compile f) w).
Proof.
  intros f w Henc Hwf. unfold compile.
  apply compile_with_correct_from_encoding; [exact Henc | | exact Hwf].
  intro s. apply ltl_to_buchi_correct.
Qed.

(* ====================================================================== *)
(* 13. Audit notes                                                        *)
(* ====================================================================== *)

(*
  Useful checks:

      Search "Axiom".
      Print Assumptions ltl_to_buchi_correct.
      Print Assumptions MTL_to_TBA_correct_from_encoding.
      Print Assumptions T_uses_every_clock.

  There is exactly one explicit project axiom:
      LTL_TO_BUCHI_CORRECT.
*)
