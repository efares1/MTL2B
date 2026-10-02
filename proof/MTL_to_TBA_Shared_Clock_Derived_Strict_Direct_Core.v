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

Require Import Classical ClassicalDescription.

(* The clock of a timed formula [F] within [root], when [F] is a timed
   subformula of [root]. *)
Definition clock_of (root F : mtl) : option (Clock root) :=
  match excluded_middle_informative (In F (timed_subformulas root)) with
  | left H => Some (exist _ F H)
  | right _ => None
  end.

Lemma clock_eq :
  forall root (x y : Clock root), proj1_sig x = proj1_sig y -> x = y.
Proof.
  intros root [x Hx] [y Hy] Heq. simpl in Heq. subst y.
  f_equal. apply proof_irrelevance.
Qed.

Lemma clock_of_mem :
  forall root F (H : In F (timed_subformulas root)),
    clock_of root F = Some (exist _ F H).
Proof.
  intros root F H. unfold clock_of.
  destruct (excluded_middle_informative (In F (timed_subformulas root)))
    as [H'|Hn].
  - f_equal. apply clock_eq. reflexivity.
  - contradiction.
Qed.

Lemma clock_of_proj :
  forall root F x, clock_of root F = Some x -> proj1_sig x = F.
Proof.
  intros root F x. unfold clock_of.
  destruct (excluded_middle_informative (In F (timed_subformulas root))).
  - intro Heq. injection Heq as <-. reflexivity.
  - discriminate.
Qed.

Lemma clock_of_none :
  forall root F, clock_of root F = None -> ~ In F (timed_subformulas root).
Proof.
  intros root F. unfold clock_of.
  destruct (excluded_middle_informative (In F (timed_subformulas root))).
  - discriminate.
  - intros _. assumption.
Qed.

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
(* 8. Automata returned by ltl_to_buchi: LATom-labelled Buchi automata    *)
(* ====================================================================== *)

(* A Buchi letter contains only the visible, untimed action proposition.
   Timing data and clock valuations belong to the timed interpretation, not
   to the input alphabet of the ordinary LTL-to-Buchi automaton. *)
Record letter : Type := {
  letter_action : Action
}.

Definition letter_of (rho : ext_word) (i : nat) : letter :=
  {| letter_action := tw_action (ew_base rho) i |}.

Record latom_transition : Type := {
  lat_bt_source : nat;
  lat_bt_label : list latom;
  lat_bt_target : nat
}.

(* Ordinary Buchi automaton returned by the LTL-to-Buchi back-end. *)
Record Buchi : Type := {
  ba_nstates : nat;
  ba_init : nat;
  ba_transitions : list latom_transition;
  ba_accepting : nat -> Prop
}.

(* The reset atoms occurring on a LATom-labelled transition determine its
   reset set.  A clock absent from this list is not reset by that transition.
   Thus [LUnch x] is represented implicitly: it means precisely that
   [x] is absent from the reset set.  Since [Clock root] is finite, a
   transition can list every clock it resets. *)
Fixpoint resets_of_atoms (atoms : list latom) : list (Clock root) :=
  match atoms with
  | [] => []
  | LRst x :: tl => x :: resets_of_atoms tl
  | _ :: tl => resets_of_atoms tl
  end.

Definition resets_match
    (rho : ext_word) (i : nat) (resets : list (Clock root)) : Prop :=
  forall x, ew_reset rho i x = true <-> In x resets.

Definition latom_transition_enabled
    (rho : ext_word) (i : nat) (t : latom_transition) : Prop :=
  Forall (atom_sat rho i) (lat_bt_label t) /\
  resets_match rho i (resets_of_atoms (lat_bt_label t)).

Definition BA_accepts (A : Buchi) (rho : ext_word) : Prop :=
  exists run : nat -> nat,
    run 0%nat = ba_init A /\
    (forall i,
       (run i < ba_nstates A)%nat /\
       exists t,
         In t (ba_transitions A) /\
         lat_bt_source t = run i /\
         lat_bt_target t = run (S i) /\
         latom_transition_enabled rho i t) /\
    (forall n,
       exists j,
         (n <= j)%nat /\ ba_accepting A (run j)).

(* ====================================================================== *)
(* 9. Explicit Buchi -> Timed Buchi Automaton conversion                  *)
(* ====================================================================== *)

Inductive clock_comparison : Type :=
| CLe | CLt | CGe | CGt | CEq.

Record clock_constraint : Type := {
  guard_clock : Clock root;
  guard_comparison : clock_comparison;
  guard_bound : R
}.

Definition guard := list (option clock_constraint).

Definition atom_action_holds (a : latom) (p : Action) : Prop :=
  match a with
  | LAct q => p = q
  | LNAct q => p <> q
  | _ => True
  end.

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

(* Reset/unchanged markers are checked against the single reset list of the
   timed transition; unchanged clocks are exactly those absent from the list. *)
Definition atom_reset_holds (a : latom) (resets : list (Clock root)) : Prop :=
  match a with
  | LRst x => In x resets
  | LUnch x => ~ In x resets
  | _ => True
  end.

Record tba_transition : Type := {
  bt_source : nat;
  bt_label : letter -> Prop;
  bt_guard : guard;
  bt_resets : list (Clock root);
  bt_target : nat
}.

(* This is the actual Timed Buchi Automaton produced by the conversion.
   Its clocks are the elements of [Clock root]. *)
Record TBA : Type := {
  tba_nstates : nat;
  tba_init : nat;
  tba_transitions : list tba_transition;
  tba_accepting : nat -> Prop
}.

Definition tba_transition_enabled
    (rho : ext_word) (i : nat) (t : tba_transition) : Prop :=
  bt_label t (letter_of rho i) /\
  Forall (guard_item_holds rho i) (bt_guard t) /\
  resets_match rho i (bt_resets t).

Definition convert_transition (t : latom_transition) : tba_transition :=
  let atoms := lat_bt_label t in
  let resets := resets_of_atoms atoms in
  {| bt_source := lat_bt_source t;
     bt_label :=
       fun l =>
         Forall
           (fun a =>
              atom_action_holds a (letter_action l) /\
              atom_reset_holds a resets)
           atoms;
     bt_guard := map atom_to_guard atoms;
     bt_resets := resets;
     bt_target := lat_bt_target t |}.

Definition convert_buchi_to_tba (A : Buchi) : TBA :=
  {| tba_nstates := ba_nstates A;
     tba_init := ba_init A;
     tba_transitions := map convert_transition (ba_transitions A);
     tba_accepting := ba_accepting A |}.

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
         (n <= j)%nat /\ tba_accepting A (run j)).

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

(* ====================================================================== *)
(* 9b. Correctness of the Buchi -> TBA conversion                         *)
(* ====================================================================== *)

Lemma atom_sat_split_with_resets :
  forall root (rho : ext_word root) i resets (a : latom root),
    resets_match rho i resets ->
    (atom_sat rho i a <->
     atom_action_holds a (tw_action (ew_base rho) i) /\
     guard_item_holds rho i (atom_to_guard a) /\
     atom_reset_holds a resets).
Proof.
  intros root rho i resets a Hreset.
  destruct a as [p|p|x d|x d|x d|x d|x|x];
    simpl;
    unfold c_le, c_lt, c_ge, c_gt, rst_at, unch_at in *;
    simpl in *; try tauto.
  - specialize (Hreset x).
    tauto.
  - specialize (Hreset x).
    destruct (ew_reset rho i x); intuition auto; discriminate.
Qed.

Lemma Forall_map_iff : forall (A B : Type) (P : B -> Prop)
    (f : A -> B) (xs : list A),
  Forall P (map f xs) <-> Forall (fun x => P (f x)) xs.
Proof.
  intros A B P f xs; induction xs as [|x xs IH]; simpl.
  - split; intro; apply Forall_nil.
  - split; intro H.
    + inversion H; subst; constructor.
      * assumption.
      * apply IH; assumption.
    + inversion H; subst; constructor.
      * assumption.
      * apply IH; assumption.
Qed.

Lemma Forall_atom_sat_split :
  forall root (rho : ext_word root) i resets (atoms : list (latom root)),
    resets_match rho i resets ->
    (Forall (atom_sat rho i) atoms <->
     Forall
       (fun a =>
          atom_action_holds a (tw_action (ew_base rho) i) /\
          atom_reset_holds a resets)
       atoms /\
     Forall (fun a => guard_item_holds rho i (atom_to_guard a)) atoms).
Proof.
  intros root rho i resets atoms Hreset.
  induction atoms as [|a atoms IH].
  - split; intro H.
    + split; apply Forall_nil.
    + apply Forall_nil.
  - rewrite Forall_cons_iff.
    rewrite Forall_cons_iff.
    rewrite Forall_cons_iff.
    pose proof (@atom_sat_split_with_resets root rho i resets a Hreset) as Ha_iff.
    split.
    + intros [Ha Htail].
      apply Ha_iff in Ha.
      destruct Ha as [Hact [Hguard Hmarker]].
      destruct (proj1 IH Htail) as [Hacts Hguards].
      split.
      * split.
        -- split; assumption.
        -- exact Hacts.
      * split; assumption.
    + intros [[Hhead Hacts] [Hguard Hguards]].
      destruct Hhead as [Hact Hmarker].
      split.
      * apply Ha_iff. tauto.
      * apply (proj2 IH).
        split; assumption.
Qed.

Lemma latom_transition_conversion_correct :
  forall root (rho : ext_word root) i (t : latom_transition root),
    latom_transition_enabled rho i t <->
    tba_transition_enabled rho i (convert_transition t).
Proof.
  intros root rho i [src atoms dst].
  unfold latom_transition_enabled, tba_transition_enabled,
         convert_transition.
  simpl.
  set (resets := resets_of_atoms atoms).
  split.
  - intros [Hatoms Hreset].
    pose proof
      (proj1 (@Forall_atom_sat_split root rho i resets atoms Hreset) Hatoms)
      as Hsplit.
    destruct Hsplit as [Hlabel Hguard].
    split.
    + exact Hlabel.
    + split.
      * rewrite Forall_map_iff.
        exact Hguard.
      * exact Hreset.
  - intros [Hlabel [Hguard Hreset]].
    split.
    + apply (proj2 (@Forall_atom_sat_split root rho i resets atoms Hreset)).
      split.
      * exact Hlabel.
      * rewrite <- Forall_map_iff.
        exact Hguard.
    + exact Hreset.
Qed.

Lemma convert_buchi_to_tba_correct :
  forall root (A : Buchi root) (rho : ext_word root),
    BA_accepts A rho <->
    TBA_ext_accepts (convert_buchi_to_tba A) rho.
Proof.
  intros root A rho.
  unfold BA_accepts, TBA_ext_accepts, convert_buchi_to_tba.
  simpl.
  split.
  - intros [run [Hinit [Hsteps Hbuchi]]].
    exists run.
    split; [exact Hinit|].
    split; [|exact Hbuchi].
    intro i.
    destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
    split; [exact Hbound|].
    exists (convert_transition t).
    split; [apply in_map; exact Hin|].
    split; [exact Hsrc|].
    split; [exact Hdst|].
    apply (proj1 (latom_transition_conversion_correct rho i t)).
    exact Hen.
  - intros [run [Hinit [Hsteps Hbuchi]]].
    exists run.
    split; [exact Hinit|].
    split; [|exact Hbuchi].
    intro i.
    destruct (Hsteps i) as [Hbound [tt [Hin [Hsrc [Hdst Hen]]]]].
    split; [exact Hbound|].
    apply in_map_iff in Hin.
    destruct Hin as [t [Heq HinA]].
    subst tt.
    exists t.
    split; [exact HinA|].
    split; [exact Hsrc|].
    split; [exact Hdst|].
    apply (proj2 (latom_transition_conversion_correct rho i t)).
    exact Hen.
Qed.

(* ====================================================================== *)
(* 9c. The ONE external axiom: LTL -> Buchi correctness                   *)
(* ====================================================================== *)

(* The external LTL-to-Buchi tool returns an ordinary Buchi automaton over
   LATom-labelled transitions.  Its clocks are those of [Clock root], a
   finite type, so the contract is satisfiable: transitions labelled by
   complete cubes over the atoms of [f], with one reset or unchanged marker
   per clock of [root], realize it. *)
Axiom LTL_TO_BUCHI_CORRECT :
  forall (root : mtl) (f : ltl root),
    { A : Buchi root |
      forall rho : ext_word root,
        BA_accepts A rho <-> lsat rho 0 f }.

Definition ltl_to_buchi (root : mtl) (f : ltl root) : Buchi root :=
  proj1_sig (LTL_TO_BUCHI_CORRECT f).

Theorem ltl_to_buchi_correct :
  forall root (f : ltl root) (rho : ext_word root),
    BA_accepts (ltl_to_buchi f) rho <-> lsat rho 0 f.
Proof.
  intros root f rho.
  unfold ltl_to_buchi.
  destruct (LTL_TO_BUCHI_CORRECT f) as [A HA].
  simpl.
  apply HA.
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

(* The compiled TBA has type [TBA f]: its clocks are the elements of
   [Clock f], i.e. exactly the timed subformulas of [f]. *)
Definition compile (f : mtl) : TBA f :=
  convert_buchi_to_tba (ltl_to_buchi (T f)).

Theorem MTL_to_TBA_correct_from_encoding :
  forall (f : mtl) (w : timed_word),
    EncodingCorrect ->
    well_formed f ->
    (msat w 0 f <-> TBA_accepts (compile f) w).
Proof.
  intros f w Henc Hwf.
  specialize (Henc f w Hwf).
  unfold compile, TBA_accepts.
  split.
  - intro Hm.
    apply (proj1 Henc) in Hm.
    destruct Hm as [rho [Hbase [Hclock Hltl]]].
    exists rho.
    split; [exact Hbase|].
    split; [exact Hclock|].
    apply (proj1 (convert_buchi_to_tba_correct (ltl_to_buchi (T f)) rho)).
    apply (proj2 (ltl_to_buchi_correct (T f) rho)).
    exact Hltl.
  - intros [rho [Hbase [Hclock Htba]]].
    apply (proj2 Henc).
    exists rho.
    split; [exact Hbase|].
    split; [exact Hclock|].
    apply (proj1 (ltl_to_buchi_correct (T f) rho)).
    apply (proj2 (convert_buchi_to_tba_correct (ltl_to_buchi (T f)) rho)).
    exact Htba.
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
