(*
  EncodingCorrect_Shared_Clock_Proof.v

  Full proof script for [EncodingCorrect] over the shared-clock core.

  Identical timed subformulas at different syntax-tree paths use the same
  formula-keyed clock. Paths are retained only for structural occurrence lookup.

  Clocks have type [Clock root], the timed subformulas of the initial
  formula.  The canonical extension [canonical_ext root w : ext_word root]
  evaluates its reset policy at the timed formula [proj1_sig x] naming
  each clock [x].

  IMPORTANT:
  - This file introduces NO new Axiom, Parameter, Hypothesis, Variable,
    Admitted, admit, or Abort.
  - The only project axiom remains [LTL_TO_BUCHI_CORRECT] from the core file.
  - The proof treats <=, >=, <, and > directly over real-valued time.
    Strict guards remain strict throughout; no transformation of a strict
    comparison into a non-strict comparison by modifying the bound is used.
  - The concrete Buchi interface uses proposition letters plus explicit
    lists of clock constraints; timed data are not fields of a letter.
  - The proof strategy follows the canonical extended-word construction:
      upper-bounded Until / lower-bounded Release preserve the oldest
      still-relevant reference; the dual families may restart it.
    The marker-free lower-bounded Until continuation uses
      (A U Q) \/ (G A /\ GF B).

  The final theorem is

      Theorem EncodingCorrect_proved : EncodingCorrect.

  and the final end-to-end theorem is then obtained without passing
  EncodingCorrect as an assumption.
*)

Require Import
  Arith Lia List Bool Reals Lra
  Classical ClassicalDescription.

Require Import MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.

Import ListNotations.
Open Scope R_scope.

Set Implicit Arguments.
Unset Strict Implicit.


Definition Rle_dec' x y : x <= y \/ ~(x <= y) := classic (x <= y).
Definition Rlt_dec' x y : x < y \/ ~(x < y) := classic (x < y).

(* ====================================================================== *)
(* 1. Structural occurrence lookup                                       *)
(* ====================================================================== *)

Fixpoint node_at (f : mtl) (path : Path) : option mtl :=
  match path with
  | [] => Some f
  | b :: tl =>
      match f with
      | MAnd p q | MOr p q | MU p q | MR p q
      | MUhatLe _ p q | MUhatGe _ p q
      | MRhatLe _ p q | MRhatGe _ p q
      | MUhatLt _ p q | MUhatGt _ p q
      | MRhatLt _ p q | MRhatGt _ p q =>
          if b then node_at q tl else node_at p tl
      | MNext p =>
          if b then None else node_at p tl
      | _ => None
      end
  end.

Lemma node_at_app :
  forall f p q,
    node_at f (p ++ q) =
    match node_at f p with
    | Some g => node_at g q
    | None => None
    end.
Proof.
  intros f p.
  revert f.
  induction p as [|b p IH]; intros f q.
  - destruct f; reflexivity.
  - destruct f; simpl; try reflexivity;
      destruct b; simpl; try reflexivity; apply IH.
Qed.

Lemma node_at_nil :
  forall f,
    node_at f [] = Some f.
Proof.
  intro f.
  destruct f; reflexivity.
Qed.

(* Shared clock key of a syntactic occurrence.  Valid occurrences carrying
   syntactically identical formulas map to the same [Clock], independently
   of their syntax-tree paths. *)
Definition clock_at (root : mtl) (path : Path) : option (Clock root) :=
  match node_at root path with
  | Some f => clock_of root f
  | None => None
  end.

Lemma clock_at_node :
  forall root path f,
    node_at root path = Some f ->
    clock_at root path = clock_of root f.
Proof.
  intros root path f H.
  unfold clock_at. rewrite H. reflexivity.
Qed.

Lemma identical_nodes_share_clock :
  forall root p1 p2 f,
    node_at root p1 = Some f ->
    node_at root p2 = Some f ->
    clock_at root p1 = clock_at root p2.
Proof.
  intros root p1 p2 f H1 H2.
  rewrite (clock_at_node H1), (clock_at_node H2).
  reflexivity.
Qed.

(* Example: the same bounded-Until formula occurs once directly and once
   below Next.  The two different paths nevertheless denote one clock. *)
Example duplicated_nested_formula_shares_clock :
  forall (d : R) (a b : Action),
    let F := MUle d (MAtom a) (MAtom b) in
    let root := MAnd F (MNext F) in
    clock_at root [false] = clock_at root [true; false].
Proof.
  intros d a b F root.
  apply identical_nodes_share_clock with (f := F); reflexivity.
Qed.

(* The timed subformulas of a located subformula are timed subformulas
   of the root; in particular a located timed formula is a clock of root. *)
Lemma node_at_timed_incl :
  forall path root f,
    node_at root path = Some f ->
    incl (timed_subformulas f) (timed_subformulas root).
Proof.
  induction path as [|b tl IH]; intros root f Hnode.
  - rewrite node_at_nil in Hnode. injection Hnode as <-.
    intros y Hy; exact Hy.
  - destruct root; simpl in Hnode; try discriminate;
      destruct b; try discriminate;
      intros y Hy; simpl;
      repeat rewrite in_app_iff;
      specialize (IH _ _ Hnode y Hy); tauto.
Qed.

Lemma node_at_left :
  forall root path p q,
    node_at root path = Some (MAnd p q) ->
    node_at root (left_path path) = Some p.
Proof.
  intros root path p q H.
  unfold left_path.
  rewrite node_at_app, H.
  apply node_at_nil.
Qed.

Lemma node_at_right :
  forall root path p q,
    node_at root path = Some (MAnd p q) ->
    node_at root (right_path path) = Some q.
Proof.
  intros root path p q H.
  unfold right_path.
  rewrite node_at_app, H.
  apply node_at_nil.
Qed.

(* The following two generic child lemmas cover every binary constructor. *)
Lemma node_at_left_binary :
  forall root path f p q,
    node_at root path = Some f ->
    (f = MAnd p q \/ f = MOr p q \/ f = MU p q \/ f = MR p q \/
     (exists d, f = MUhatLe d p q) \/
     (exists d, f = MUhatGe d p q) \/
     (exists d, f = MRhatLe d p q) \/
     (exists d, f = MRhatGe d p q) \/
     (exists d, f = MUhatLt d p q) \/
     (exists d, f = MUhatGt d p q) \/
     (exists d, f = MRhatLt d p q) \/
     (exists d, f = MRhatGt d p q)) ->
    node_at root (left_path path) = Some p.
Proof.
  intros root path f p q Hnode Hshape.
  unfold left_path.
  rewrite node_at_app, Hnode.
  destruct Hshape as
      [->|[->|[->|[->|[[d ->]|[[d ->]|[[d ->]|[[d ->]|
       [[d ->]|[[d ->]|[[d ->]|[d ->]]]]]]]]]]]];
    apply node_at_nil.
Qed.

Lemma node_at_right_binary :
  forall root path f p q,
    node_at root path = Some f ->
    (f = MAnd p q \/ f = MOr p q \/ f = MU p q \/ f = MR p q \/
     (exists d, f = MUhatLe d p q) \/
     (exists d, f = MUhatGe d p q) \/
     (exists d, f = MRhatLe d p q) \/
     (exists d, f = MRhatGe d p q) \/
     (exists d, f = MUhatLt d p q) \/
     (exists d, f = MUhatGt d p q) \/
     (exists d, f = MRhatLt d p q) \/
     (exists d, f = MRhatGt d p q)) ->
    node_at root (right_path path) = Some q.
Proof.
  intros root path f p q Hnode Hshape.
  unfold right_path.
  rewrite node_at_app, Hnode.
  destruct Hshape as
      [->|[->|[->|[->|[[d ->]|[[d ->]|[[d ->]|[[d ->]|
       [[d ->]|[[d ->]|[[d ->]|[d ->]]]]]]]]]]]];
    apply node_at_nil.
Qed.

Lemma node_at_next :
  forall root path p,
    node_at root path = Some (MNext p) ->
    node_at root (left_path path) = Some p.
Proof.
  intros root path p Hnode.
  unfold left_path.
  rewrite node_at_app, Hnode.
  apply node_at_nil.
Qed.

(* ====================================================================== *)
(* 2. Residual semantic obligations                                      *)
(* ====================================================================== *)

Definition elapsed (w : timed_word) (i j : nat) : R :=
  tw_time w j - tw_time w i.

Definition ule_residual
    (w : timed_word) (i : nat) (v d : R) (p q : mtl) : Prop :=
  exists j,
    (i <= j)%nat /\
    v + elapsed w i j <= d /\
    msat w j q /\
    (forall k, (i <= k < j)%nat -> msat w k p).

Definition rge_residual
    (w : timed_word) (i : nat) (v d : R) (p q : mtl) : Prop :=
  forall j,
    (i <= j)%nat ->
    d <= v + elapsed w i j ->
    msat w j q \/
    exists k, (i <= k < j)%nat /\ msat w k p.


Definition ult_residual
    (w : timed_word) (i : nat) (v d : R) (p q : mtl) : Prop :=
  exists j,
    (i <= j)%nat /\
    v + elapsed w i j < d /\
    msat w j q /\
    (forall k, (i <= k < j)%nat -> msat w k p).

Definition rgt_residual
    (w : timed_word) (i : nat) (v d : R) (p q : mtl) : Prop :=
  forall j,
    (i <= j)%nat ->
    d < v + elapsed w i j ->
    msat w j q \/
    exists k, (i <= k < j)%nat /\ msat w k p.

Definition decide (P : Prop) : bool := decide_b P.

Lemma decide_true :
  forall P, decide P = true <-> P.
Proof. intro P. exact (decide_b_spec P). Qed.

Lemma decide_false :
  forall P, decide P = false <-> ~ P.
Proof.
  intro P. pose proof (decide_true P) as HP.
  destruct (decide P); split; intro H; try reflexivity; try discriminate.
  - exfalso. apply H. apply HP. reflexivity.
  - intro Hp. apply HP in Hp. discriminate.
Qed.

Lemma negb_decide_false_elim :
  forall P : Prop, negb (decide P) = false -> P.
Proof.
  intros P Hneg.
  destruct (decide P) eqn:Hdec.
  - apply (proj1 (@decide_true P)).
    exact Hdec.
  - simpl in Hneg. discriminate.
Qed.

(* ====================================================================== *)
(* 3. Canonical global reset policy                                      *)
(* ====================================================================== *)

(*
   The reset decision is global: it is defined at EVERY position for EVERY
   syntactic timed occurrence.  Hence the same clock trace is reused when
   the occurrence is activated repeatedly below G/U/R.

   U<= / Uhat<=
       keep the clock exactly while the previous residual remains useful;
       otherwise reset.

   U>= / Uhat>=
       reset whenever the corresponding timed formula is true at this point.

   R<=
       reset when a new nontrivial Release obligation is started.

   Rhat<=
       reset whenever the hatted obligation is true.

   R>= / Rhat>=
       keep the clock while the previous lower-bound Release residual is
       still active and has not been discharged by p.
*)

Definition reset_policy
    (_root : mtl) (w : timed_word)
    (x : mtl) (i : nat) (v : R) : bool :=
  match x with
  | MUhatLe d p q =>
      negb (decide
        (v <= d /\
         ule_residual w i v d p q /\
         ~ msat w i q))

  | MUhatGe d p q =>
      decide (msat w i (MUhatGe d p q))

  | MRhatLe d p q =>
      decide (msat w i (MRhatLe d p q))

  | MRhatGe d p q =>
      negb (decide
        (v < d /\
         rge_residual w i v d p q /\
         ~ msat w i p))

  | MUhatLt d p q =>
      negb (decide
        (v < d /\
         ult_residual w i v d p q /\
         ~ msat w i q))

  | MUhatGt d p q =>
      decide (msat w i (MUhatGt d p q))

  | MRhatLt d p q =>
      decide (msat w i (MRhatLt d p q))

  | MRhatGt d p q =>
      negb (decide
        (v <= d /\
         rgt_residual w i v d p q /\
         ~ msat w i p))

  | _ => true
  end.

Fixpoint canonical_val
    (root : mtl) (w : timed_word) (x : mtl) (n : nat) : R :=
  match n with
  | O => 0
  | S i =>
      let v := canonical_val root w x i in
      if reset_policy root w x i v
      then delta w i
      else v + delta w i
  end.

Definition canonical_reset
    (root : mtl) (w : timed_word) (i : nat) (x : mtl) : bool :=
  reset_policy root w x i (canonical_val root w x i).

(* The canonical extension is an extended word over [Clock root]; the
   policy is evaluated at the timed formula [proj1_sig x] naming clock [x]. *)
Definition canonical_ext (root : mtl) (w : timed_word) : ext_word root :=
  {| ew_base := w;
     ew_val := fun i x => canonical_val root w (proj1_sig x) i;
     ew_reset := fun i x => canonical_reset root w i (proj1_sig x) |}.

Lemma canonical_val_nonnegative :
  forall root w x i,
    0 <= canonical_val root w x i.
Proof.
  intros root w x i.
  induction i as [|i IH].
  - simpl. lra.
  - simpl.
    destruct (reset_policy root w x i (canonical_val root w x i)).
    + left. apply delta_positive.
    + pose proof (delta_positive w i). lra.
Qed.

Lemma canonical_clock_consistent :
  forall root w,
    clock_consistent (canonical_ext root w).
Proof.
  intros root w.
  split.
  - intros i x.
    apply canonical_val_nonnegative.
  - intros i x.
    simpl.
    unfold canonical_reset.
    simpl.
    destruct (reset_policy root w (proj1_sig x) i
                (canonical_val root w (proj1_sig x) i));
      reflexivity.
Qed.

Lemma canonical_same_base :
  forall root w,
    same_base (canonical_ext root w) w.
Proof.
  intros. reflexivity.
Qed.

(* Two distinct occurrence paths carrying exactly the same subformula use
   literally the same canonical clock trace. *)
Lemma identical_nodes_share_canonical_trace :
  forall root w p1 p2 f i (x1 x2 : Clock root),
    node_at root p1 = Some f ->
    node_at root p2 = Some f ->
    clock_at root p1 = Some x1 ->
    clock_at root p2 = Some x2 ->
    x1 = x2 /\
    ew_val (canonical_ext root w) i x1 = ew_val (canonical_ext root w) i x2 /\
    ew_reset (canonical_ext root w) i x1 = ew_reset (canonical_ext root w) i x2.
Proof.
  intros root w p1 p2 f i x1 x2 H1 H2 Hx1 Hx2.
  pose proof (identical_nodes_share_clock H1 H2) as Hclock.
  rewrite Hclock, Hx2 in Hx1.
  injection Hx1 as ->.
  repeat split; reflexivity.
Qed.

Section GenericClocks.
Variable root : mtl.

(* ====================================================================== *)
(* 4. General clock arithmetic                                           *)
(* ====================================================================== *)

Lemma reset_step :
  forall (rho : ext_word root) i x,
    clock_consistent rho ->
    rst_at rho x i ->
    ew_val rho (S i) x = delta (ew_base rho) i.
Proof.
  intros rho i x [_ Hstep] Hr.
  specialize (Hstep i x).
  unfold rst_at in Hr.
  rewrite Hr in Hstep.
  exact Hstep.
Qed.

Lemma unch_step :
  forall (rho : ext_word root) i x,
    clock_consistent rho ->
    unch_at rho x i ->
    ew_val rho (S i) x =
      ew_val rho i x + delta (ew_base rho) i.
Proof.
  intros rho i x [_ Hstep] Hu.
  specialize (Hstep i x).
  unfold unch_at in Hu.
  rewrite Hu in Hstep.
  exact Hstep.
Qed.

Lemma ext_val_nonnegative :
  forall (rho : ext_word root) i x,
    clock_consistent rho ->
    0 <= ew_val rho i x.
Proof.
  intros rho i x [H _].
  apply H.
Qed.

Lemma preserve_accumulate :
  forall (rho : ext_word root) i j x,
    clock_consistent rho ->
    (i <= j)%nat ->
    (forall k:nat, (i <= k < j)%nat -> unch_at rho x k) ->
    ew_val rho j x =
      ew_val rho i x + elapsed (ew_base rho) i j.
Proof.
  intros rho i j x Hcc Hij Hall.
  induction Hij.
  - unfold elapsed. ring.
  - rewrite unch_step; auto.
    rewrite IHHij.
    * unfold elapsed, delta. ring.
    * intros k Hk. apply Hall. lia.
Qed.

Lemma preserve_elapsed_le_value :
  forall (rho : ext_word root) i j x,
    clock_consistent rho ->
    (i <= j)%nat ->
    (forall k:nat, (i <= k < j)%nat -> unch_at rho x k) ->
    elapsed (ew_base rho) i j <= ew_val rho j x.
Proof.
  intros rho i j x Hcc Hij Hall.
  rewrite preserve_accumulate with (i:=i); auto.
  pose proof (ext_val_nonnegative i x Hcc).
  lra.
Qed.

Lemma after_reset_value_le_elapsed :
  forall (rho : ext_word root) i j x,
    clock_consistent rho ->
    rst_at rho x i ->
    (S i <= j)%nat ->
    ew_val rho j x <= elapsed (ew_base rho) i j.
Proof.
  intros rho i j x Hcc Hr Hij.
  remember (j - S i)%nat as n eqn:Hn.
  assert (Hj : j = (S i + n)%nat) by lia.
  subst j.
  clear Hn Hij.
  induction n as [|n IH].
  - replace (S i + 0)%nat with (S i) by lia.
    rewrite (reset_step Hcc Hr).
    unfold elapsed, delta. lra.
  - replace (S i + S n)%nat with (S (S i + n)) by lia.
    destruct Hcc as [Hnn Hstep].
    specialize (Hstep (S i + n)%nat x).
    destruct (ew_reset rho (S i + n)%nat x) eqn:Hr2.
    + rewrite Hstep.
      unfold elapsed, delta.
      assert (Hin : (i <= S i + n)%nat) by lia.
      pose proof (time_monotone (ew_base rho) Hin) as htm.
      lra.
    + rewrite Hstep.
      unfold elapsed, delta in *.
      lra.
Qed.

(* If the first clock step is followed only by unchanged steps, the clock
   value dominates the physical time elapsed from the preceding position. *)
Lemma preserve_from_next_elapsed_le_value :
  forall (rho : ext_word root) i j x,
    clock_consistent rho ->
    (S i <= j)%nat ->
    (forall k : nat, (S i <= k < j)%nat -> unch_at rho x k) ->
    elapsed (ew_base rho) i j <= ew_val rho j x.
Proof.
  intros rho i j x Hcc Hsj Hpres.
  assert (Hfirst :
    elapsed (ew_base rho) i (S i) <= ew_val rho (S i) x).
  { destruct (ew_reset rho i x) eqn:Hr.
    - rewrite (reset_step (rho:=rho) (i:=i) (x:=x) Hcc Hr).
      unfold elapsed, delta.
      right; reflexivity.
    - rewrite (unch_step (rho:=rho) (i:=i) (x:=x) Hcc Hr).
      pose proof
        (ext_val_nonnegative i x Hcc) as Hnonneg.
      unfold elapsed, delta.
      lra. }
  pose proof
    (preserve_accumulate
       (rho:=rho) (i:=S i) (j:=j) (x:=x) Hcc Hsj Hpres) as Hacc.
  rewrite Hacc.
  replace (elapsed (ew_base rho) i j) with
    (elapsed (ew_base rho) i (S i) +
     elapsed (ew_base rho) (S i) j).
  - apply Rplus_le_compat_r. exact Hfirst.
  - unfold elapsed. ring.
Qed.

(* With a reset at [i] and no later reset before [j], the clock at [j]
   is exactly the elapsed physical time since [i]. *)
Lemma reset_preserve_value_eq_elapsed :
  forall (rho : ext_word root) i j x,
    clock_consistent rho ->
    rst_at rho x i ->
    (S i <= j)%nat ->
    (forall k : nat, (S i <= k < j)%nat -> unch_at rho x k) ->
    ew_val rho j x = elapsed (ew_base rho) i j.
Proof.
  intros rho i j x Hcc Hrst Hsj Hpres.
  pose proof
    (preserve_accumulate
       (rho:=rho) (i:=S i) (j:=j) (x:=x) Hcc Hsj Hpres) as Hacc.
  rewrite Hacc.
  rewrite (reset_step (rho:=rho) (i:=i) (x:=x) Hcc Hrst).
  unfold elapsed, delta. ring.
Qed.

Lemma reset_lower_guard_sound :
  forall (rho : ext_word root) i j x d,
    clock_consistent rho ->
    rst_at rho x i ->
    (S i <= j)%nat ->
    c_ge rho x d j ->
    d <= elapsed (ew_base rho) i j.
Proof.
  intros.
  unfold c_ge in H2.
  pose proof (after_reset_value_le_elapsed H H0 H1).
  lra.
Qed.

Lemma reset_strict_lower_guard_sound :
  forall (rho : ext_word root) i j x d,
    clock_consistent rho ->
    rst_at rho x i ->
    (S i <= j)%nat ->
    c_gt rho x d j ->
    d < elapsed (ew_base rho) i j.
Proof.
  intros rho i j x d Hcc Hrst Hsj Hgt.
  unfold c_gt in Hgt.
  pose proof (after_reset_value_le_elapsed Hcc Hrst Hsj) as Hval.
  lra.
Qed.


Lemma preserve_upper_guard_sound :
  forall (rho : ext_word root) i j x d,
    clock_consistent rho ->
    (i <= j)%nat ->
    (forall k:nat, (i <= k < j)%nat -> unch_at rho x k) ->
    c_le rho x d j ->
    elapsed (ew_base rho) i j <= d.
Proof.
  intros.
  unfold c_le in H2.
  pose proof (preserve_elapsed_le_value H H0 H1).
  lra.
Qed.

Lemma preserve_strict_upper_guard_sound :
  forall (rho : ext_word root) i j x d,
    clock_consistent rho ->
    (i <= j)%nat ->
    (forall k:nat, (i <= k < j)%nat -> unch_at rho x k) ->
    c_lt rho x d j ->
    elapsed (ew_base rho) i j < d.
Proof.
  intros.
  unfold c_lt in H2.
  pose proof (preserve_elapsed_le_value H H0 H1).
  lra.
Qed.

(* ====================================================================== *)
(* 5. LTL semantic helper lemmas                                         *)
(* ====================================================================== *)

Lemma LG_semantics :
  forall (rho : ext_word root) i A,
    lsat rho i (LG A) <->
    forall j, (i <= j)%nat -> lsat rho j A.
Proof.
  intros rho i A.
  unfold LG.
  simpl.
  split.
  - intros H j Hij.
    specialize (H j Hij).
    destruct H as [HA | [k [_ HF]]].
    + exact HA.
    + contradiction.
  - intros H j Hij.
    left. apply H. exact Hij.
Qed.

Lemma LF_semantics :
  forall (rho : ext_word root) i A,
    lsat rho i (LF A) <->
    exists j, (i <= j)%nat /\ lsat rho j A.
Proof.
  intros rho i A.
  unfold LF.
  simpl.
  split.
  - intros [j [Hij [HA _]]].
    exists j. auto.
  - intros [j [Hij HA]].
    exists j. repeat split; try assumption.
Qed.

Lemma LGF_semantics :
  forall (rho : ext_word root) i A,
    lsat rho i (LGF A) <->
    forall n, (i <= n)%nat ->
      exists j, (n <= j)%nat /\ lsat rho j A.
Proof.
  intros rho i A.
  unfold LGF.
  rewrite LG_semantics.
  split.
  - intros H n Hin.
    apply LF_semantics.
    apply H. exact Hin.
  - intros H n Hin.
    apply LF_semantics.
    apply H. exact Hin.
Qed.

Lemma LW_semantics :
  forall (rho : ext_word root) i A B,
    lsat rho i (LW A B) <->
    (exists j,
       (i <= j)%nat /\
       lsat rho j B /\
       (forall k, (i <= k < j)%nat -> lsat rho k A))
    \/
    (forall j, (i <= j)%nat -> lsat rho j A).
Proof.
  intros rho i A B.
  unfold LW.
  simpl.
  rewrite <-LG_semantics.
  tauto.
Qed.

Lemma GF_far :
  forall (rho : ext_word root) i d B,
    0 <= d ->
    lsat rho i (LGF B) ->
    exists j,
      (i <= j)%nat /\
      d <= elapsed (ew_base rho) i j /\
      lsat rho j B.
Proof.
  intros rho i d B Hd Hgf.
  rewrite LGF_semantics in Hgf.
  destruct (tw_time_divergent (ew_base rho) i Hd)
    as [n [Hin Hfar]].
  destruct (Hgf n Hin) as [j [Hnj HB]].
  exists j; repeat split; try assumption; try lia.
  unfold elapsed in *.
  assert (i <= j)%nat as hij by lia.
  pose proof (time_monotone (ew_base rho) hij).
  pose proof (time_monotone (ew_base rho) Hnj).
  lra.
Qed.

End GenericClocks.

(* ====================================================================== *)
(* 6. Canonical-policy facts for the eight primitive clock classes      *)
(* ====================================================================== *)

Lemma policy_UhatGe_reset :
  forall root w path d p q i,
    node_at root path = Some (MUhatGe d p q) ->
    msat w i (MUhatGe d p q) ->
    canonical_reset root w i ((MUhatGe d p q)) = true.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  apply decide_true.
  exact H0.
Qed.

Lemma policy_RhatLe_reset :
  forall root w path d p q i,
    node_at root path = Some (MRhatLe d p q) ->
    msat w i (MRhatLe d p q) ->
    canonical_reset root w i ((MRhatLe d p q)) = true.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  apply decide_true.
  exact H0.
Qed.

Lemma policy_UhatLe_preserve :
  forall root w path d p q i,
    node_at root path = Some (MUhatLe d p q) ->
    canonical_val root w ((MUhatLe d p q)) i <= d ->
    ule_residual w i (canonical_val root w ((MUhatLe d p q)) i) d p q ->
    ~ msat w i q ->
    canonical_reset root w i ((MUhatLe d p q)) = false.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  rewrite Bool.negb_false_iff.
  apply decide_true.
  auto.
Qed.

Lemma policy_RhatGe_preserve :
  forall root w path d p q i,
    node_at root path = Some (MRhatGe d p q) ->
    canonical_val root w ((MRhatGe d p q)) i < d ->
    rge_residual w i (canonical_val root w ((MRhatGe d p q)) i) d p q ->
    ~ msat w i p ->
    canonical_reset root w i ((MRhatGe d p q)) = false.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  rewrite Bool.negb_false_iff.
  apply decide_true.
  auto.
Qed.

Lemma policy_UhatGt_reset :
  forall root w path d p q i,
    node_at root path = Some (MUhatGt d p q) ->
    msat w i (MUhatGt d p q) ->
    canonical_reset root w i ((MUhatGt d p q)) = true.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  apply decide_true.
  exact H0.
Qed.

Lemma policy_RhatLt_reset :
  forall root w path d p q i,
    node_at root path = Some (MRhatLt d p q) ->
    msat w i (MRhatLt d p q) ->
    canonical_reset root w i ((MRhatLt d p q)) = true.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  apply decide_true.
  exact H0.
Qed.

Lemma policy_UhatLt_preserve :
  forall root w path d p q i,
    node_at root path = Some (MUhatLt d p q) ->
    canonical_val root w ((MUhatLt d p q)) i < d ->
    ult_residual w i (canonical_val root w ((MUhatLt d p q)) i) d p q ->
    ~ msat w i q ->
    canonical_reset root w i ((MUhatLt d p q)) = false.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  rewrite Bool.negb_false_iff.
  apply decide_true.
  auto.
Qed.

Lemma policy_RhatGt_preserve :
  forall root w path d p q i,
    node_at root path = Some (MRhatGt d p q) ->
    canonical_val root w ((MRhatGt d p q)) i <= d ->
    rgt_residual w i (canonical_val root w ((MRhatGt d p q)) i) d p q ->
    ~ msat w i p ->
    canonical_reset root w i ((MRhatGt d p q)) = false.
Proof.
  intros.
  unfold canonical_reset, reset_policy.
  simpl.
  rewrite Bool.negb_false_iff.
  apply decide_true.
  auto.
Qed.


Section Soundness.
Variable root : mtl.

(* ====================================================================== *)
(* 7. The two dominance lemmas                                            *)
(* ====================================================================== *)

(*
   U<= / R>= use an OLDEST still-live reference.  The following lemma is the
   formal statement used by the completeness proof of U<=.

   If the canonical policy is preserving the clock at i, the residual witness
   belongs to an earlier activation.  Such a witness is also a valid witness
   for every newer U<= activation because:
       elapsed(new,witness) <= elapsed(old,witness),
   and the p-prefix required by the newer activation is a suffix of the
   prefix required by the old activation.
*)
Lemma sound_UhatLe :
  forall (rho : ext_word root) path d p q i,
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MUhatLe d p q)) ->
    msat (ew_base rho) i (MUhatLe d p q).
Proof.
  intros rho path d p q i Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MUhatLe d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  destruct HT as [j [Hsj [[HCj HBj] Hall]]].
  exists j. repeat split.
  - lia.
  - assert (Hpres :
      forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
    { intros k Hk.
      specialize (Hall k Hk).
      exact (proj1 (proj2 Hall)). }
    pose proof
      (preserve_from_next_elapsed_le_value
         (rho:=rho) (i:=i) (j:=j) (x:=x) Hcc Hsj Hpres) as Hel.
    unfold c_le in HCj.
    eapply Rle_trans; [exact Hel | exact HCj].
  - apply IHq. exact HBj.
  - intros k Hk.
    apply IHp.
    assert (Hrange : (S i <= k < j)%nat) by lia.
    specialize (Hall k Hrange).
    exact (proj2 (proj2 Hall)).
Qed.

Lemma sound_UhatGe :
  forall (rho : ext_word root) path d p q i,
    0 < d ->
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MUhatGe d p q)) ->
    msat (ew_base rho) i (MUhatGe d p q).
Proof.
  intros rho path d p q i Hd Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MUhatGe d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  destruct HT as [Hrst Hnext].
  destruct Hnext as [HUQ | [HGA HGFB]].

  - destruct HUQ as [j [Hsj [HQ HallA]]].
    destruct HQ as [Hge HU].
    destruct HU as [m [Hjm [HB Hall2]]].
    exists m. repeat split.
    + lia.
    + assert (Hdij : d <= elapsed (ew_base rho) i j).
      { eapply reset_lower_guard_sound; eauto. }
      pose proof (time_monotone (ew_base rho) Hjm) as Htime_jm.
      assert (Hel_jm :
        elapsed (ew_base rho) i j <= elapsed (ew_base rho) i m).
      { unfold elapsed, Rminus.
        apply Rplus_le_compat_r.
        exact Htime_jm. }
      eapply Rle_trans; [exact Hdij | exact Hel_jm].
    + apply IHq. exact HB.
    + intros k Hk.
      destruct (lt_dec k j) as [Hkj | Hnkj].
      * apply IHp. apply HallA. lia.
      * assert (Hjk : (j <= k)%nat) by lia.
        apply IHp. apply Hall2. lia.

  - (* [simpl] unfolded G A.  Recover its clean semantic form explicitly. *)
    change (lsat rho (S i)
      (LG (T_at (left_path path) p))) in HGA.
    rewrite LG_semantics in HGA.
    assert (Hd0 : 0 <= d) by (apply Rlt_le; exact Hd).
    destruct
      (GF_far Hd0 HGFB) as [j [Hsj [Hfar HB]]].
    exists j. repeat split.
    + lia.
    + eapply Rle_trans; [exact Hfar |].
      unfold elapsed, Rminus.
      apply Rplus_le_compat_l.
      apply Ropp_le_contravar.
      apply Rlt_le.
      apply tw_time_strict.
    + apply IHq. exact HB.
    + intros k Hk.
      apply IHp. apply HGA. lia.
Qed.

Lemma sound_RhatLe :
  forall (rho : ext_word root) path d p q i,
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MRhatLe d p q)) ->
    msat (ew_base rho) i (MRhatLe d p q).
Proof.
  intros rho path d p q i Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MRhatLe d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  destruct HT as [Hrst HW].
  change (lsat rho (S i)
    (LW
      (T_at (right_path path) q)
      (LOr (LAtom (LCGt (x) d))
           (LAnd (T_at (left_path path) p)
                 (T_at (right_path path) q))))) in HW.
  apply (proj1 (@LW_semantics _ rho (S i)
    (T_at (right_path path) q)
    (LOr (LAtom (LCGt (x) d))
         (LAnd (T_at (left_path path) p)
               (T_at (right_path path) q))))) in HW.
  intros j Hij Hbound.
  assert (HSj : (S i <= j)%nat) by lia.
  destruct HW as [HU | HG].
  - destruct HU as [m [HSm [HD Hpre]]].
    destruct (le_lt_dec m j) as [Hmj | Hjm].
    + destruct HD as [Hgt | [HAm HBm]].
      * exfalso.
        unfold c_gt in Hgt.
        pose proof
          (after_reset_value_le_elapsed
             (rho:=rho) (i:=i) (j:=m) (x:=x)
             Hcc Hrst HSm) as Hval_m.
        pose proof (time_monotone (ew_base rho) Hmj) as Htime_mj.
        assert (Hel_mj :
          elapsed (ew_base rho) i m <= elapsed (ew_base rho) i j).
        { unfold elapsed, Rminus.
          apply Rplus_le_compat_r.
          exact Htime_mj. }
        assert (Hval_le_d : ew_val rho m (x) <= d).
        { eapply Rle_trans; [exact Hval_m |].
          eapply Rle_trans; [exact Hel_mj | exact Hbound]. }
        exact (Rlt_not_le _ _ Hgt Hval_le_d).
      * destruct (Nat.eq_dec m j) as [-> | Hneq].
        -- left. apply IHq. exact HBm.
        -- right. exists m. split; [lia|].
           apply IHp. exact HAm.
    + left. apply IHq. apply Hpre. lia.
  - left. apply IHq. apply HG. lia.
Qed.

Lemma sound_RhatGe :
  forall (rho : ext_word root) path d p q i,
    0 < d ->
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MRhatGe d p q)) ->
    msat (ew_base rho) i (MRhatGe d p q).
Proof.
  intros rho path d p q i Hd Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MRhatGe d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  change (lsat rho (S i)
    (LW
      (LAnd (LAtom (LCLt (x) d)) (LAtom (LUnch (x))))
      (LOr
        (LRelease (T_at (left_path path) p)
                  (T_at (right_path path) q))
        (LAnd (LAtom (LCLt (x) d))
              (T_at (left_path path) p))))) in HT.
  apply (proj1 (@LW_semantics _ rho (S i)
    (LAnd (LAtom (LCLt (x) d)) (LAtom (LUnch (x))))
    (LOr
      (LRelease (T_at (left_path path) p)
                (T_at (right_path path) q))
      (LAnd (LAtom (LCLt (x) d))
            (T_at (left_path path) p))))) in HT.
  intros j Hij Hbound.
  assert (HSj : (S i <= j)%nat) by lia.
  destruct HT as [HU | HG].
  - destruct HU as [m [HSm [HE Hpre]]].
    destruct (le_lt_dec m j) as [Hmj | Hjm].
    + destruct HE as [HR | [Hlt HAm]].
      * specialize (HR j Hmj).
        destruct HR as [HB | [k [Hk HA]]].
        -- left. apply IHq. exact HB.
        -- right. exists k. split; [lia|].
           apply IHp. exact HA.
      * destruct (Nat.eq_dec m j) as [-> | Hneq].
        -- exfalso.
           assert (Hpres :
             forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
           { intros k Hk.
             specialize (Hpre k Hk).
             exact (proj2 Hpre). }
           pose proof
             (preserve_from_next_elapsed_le_value
                (rho:=rho) (i:=i) (j:=j) (x:=x)
                Hcc HSj Hpres) as Hel.
           assert (Hdval : d <= ew_val rho j (x)).
           { eapply Rle_trans; [exact Hbound | exact Hel]. }
           exact (Rlt_not_le _ _ Hlt Hdval).
        -- right. exists m. split; [lia|].
           apply IHp. exact HAm.
    + assert (Hjpre : (S i <= j < m)%nat) by lia.
      pose proof (Hpre j Hjpre) as Hprej.
      destruct Hprej as [Hltj _].
      exfalso.
      assert (Hpres :
        forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
      { intros k Hk.
        assert (Hkm : (S i <= k < m)%nat) by lia.
        specialize (Hpre k Hkm).
        exact (proj2 Hpre). }
      pose proof
        (preserve_from_next_elapsed_le_value
           (rho:=rho) (i:=i) (j:=j) (x:=x)
           Hcc HSj Hpres) as Hel.
      unfold c_lt in Hltj.
      assert (Hdval : d <= ew_val rho j (x)).
      { eapply Rle_trans; [exact Hbound | exact Hel]. }
      exact (Rlt_not_le _ _ Hltj Hdval).
  - pose proof (HG j HSj) as HGj.
    destruct HGj as [Hltj Hun].
    exfalso.
    assert (Hpres :
      forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
    { intros k Hk.
      specialize (HG k (proj1 Hk)).
      exact (proj2 HG). }
    pose proof
      (preserve_from_next_elapsed_le_value
         (rho:=rho) (i:=i) (j:=j) (x:=x)
         Hcc HSj Hpres) as Hel.
    unfold c_lt in Hltj.
    assert (Hdval : d <= ew_val rho j (x)).
    { eapply Rle_trans; [exact Hbound | exact Hel]. }
    exact (Rlt_not_le _ _ Hltj Hdval).
Qed.

(* ====================================================================== *)
(* 8. Strict-comparator soundness lemmas                                 *)
(* ====================================================================== *)

Lemma sound_UhatLt :
  forall (rho : ext_word root) path d p q i,
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MUhatLt d p q)) ->
    msat (ew_base rho) i (MUhatLt d p q).
Proof.
  intros rho path d p q i Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MUhatLt d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  destruct HT as [j [Hsj [[HCj HBj] Hall]]].
  exists j. repeat split.
  - lia.
  - assert (Hpres :
      forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
    { intros k Hk.
      specialize (Hall k Hk).
      exact (proj1 (proj2 Hall)). }
    pose proof
      (preserve_from_next_elapsed_le_value
         (rho:=rho) (i:=i) (j:=j) (x:=x) Hcc Hsj Hpres) as Hel.
    unfold c_lt in HCj.
    unfold elapsed in Hel.
    lra.
  - apply IHq. exact HBj.
  - intros k Hk.
    apply IHp.
    assert (Hrange : (S i <= k < j)%nat) by lia.
    specialize (Hall k Hrange).
    exact (proj2 (proj2 Hall)).
Qed.

Lemma sound_UhatGt :
  forall (rho : ext_word root) path d p q i,
    0 < d ->
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MUhatGt d p q)) ->
    msat (ew_base rho) i (MUhatGt d p q).
Proof.
  intros rho path d p q i Hd Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MUhatGt d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  destruct HT as [Hrst Hnext].
  destruct Hnext as [HUQ | [HGA HGFB]].

  - destruct HUQ as [j [Hsj [HQ HallA]]].
    destruct HQ as [Hge HU].
    destruct HU as [m [Hjm [HB Hall2]]].
    exists m. repeat split.
    + lia.
    + assert (Hdij : d < elapsed (ew_base rho) i j).
      { eapply reset_strict_lower_guard_sound; eauto. }
      pose proof (time_monotone (ew_base rho) Hjm) as Htime_jm.
      assert (Hel_jm :
        elapsed (ew_base rho) i j <= elapsed (ew_base rho) i m).
      { unfold elapsed, Rminus.
        apply Rplus_le_compat_r.
        exact Htime_jm. }
      unfold elapsed in Hdij.
      lra.
    + apply IHq. exact HB.
    + intros k Hk.
      destruct (lt_dec k j) as [Hkj | Hnkj].
      * apply IHp. apply HallA. lia.
      * assert (Hjk : (j <= k)%nat) by lia.
        apply IHp. apply Hall2. lia.

  - (* [simpl] unfolded G A.  Recover its clean semantic form explicitly. *)
    change (lsat rho (S i)
      (LG (T_at (left_path path) p))) in HGA.
    rewrite LG_semantics in HGA.
    assert (Hd0 : 0 <= d) by (apply Rlt_le; exact Hd).
    destruct
      (GF_far Hd0 HGFB) as [j [Hsj [Hfar HB]]].
    exists j. repeat split.
    + lia.
    + unfold elapsed in *.
      pose proof (tw_time_strict (ew_base rho) i).
      lra.
    + apply IHq. exact HB.
    + intros k Hk.
      apply IHp. apply HGA. lia.
Qed.

Lemma sound_RhatLt :
  forall (rho : ext_word root) path d p q i,
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MRhatLt d p q)) ->
    msat (ew_base rho) i (MRhatLt d p q).
Proof.
  intros rho path d p q i Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MRhatLt d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  destruct HT as [Hrst HW].
  change (lsat rho (S i)
    (LW
      (T_at (right_path path) q)
      (LOr (LAtom (LCGe (x) d))
           (LAnd (T_at (left_path path) p)
                 (T_at (right_path path) q))))) in HW.
  apply (proj1 (@LW_semantics _ rho (S i)
    (T_at (right_path path) q)
    (LOr (LAtom (LCGe (x) d))
         (LAnd (T_at (left_path path) p)
               (T_at (right_path path) q))))) in HW.
  intros j Hij Hbound.
  assert (HSj : (S i <= j)%nat) by lia.
  destruct HW as [HU | HG].
  - destruct HU as [m [HSm [HD Hpre]]].
    destruct (le_lt_dec m j) as [Hmj | Hjm].
    + destruct HD as [Hge | [HAm HBm]].
      * exfalso.
        unfold c_ge in Hge.
        pose proof
          (after_reset_value_le_elapsed
             (rho:=rho) (i:=i) (j:=m) (x:=x)
             Hcc Hrst HSm) as Hval_m.
        pose proof (time_monotone (ew_base rho) Hmj) as Htime_mj.
        assert (Hel_mj :
          elapsed (ew_base rho) i m <= elapsed (ew_base rho) i j).
        { unfold elapsed, Rminus.
          apply Rplus_le_compat_r.
          exact Htime_mj. }
        assert (Hval_lt_d : ew_val rho m (x) < d).
        { unfold elapsed in Hval_m; lra. }
        exact (Rle_not_lt _ _ Hge Hval_lt_d).
      * destruct (Nat.eq_dec m j) as [-> | Hneq].
        -- left. apply IHq. exact HBm.
        -- right. exists m. split; [lia|].
           apply IHp. exact HAm.
    + left. apply IHq. apply Hpre. lia.
  - left. apply IHq. apply HG. lia.
Qed.

Lemma sound_RhatGt :
  forall (rho : ext_word root) path d p q i,
    0 < d ->
    clock_consistent rho ->
    (forall n,
       lsat rho n (T_at (left_path path) p) ->
       msat (ew_base rho) n p) ->
    (forall n,
       lsat rho n (T_at (right_path path) q) ->
       msat (ew_base rho) n q) ->
    lsat rho i (T_at path (MRhatGt d p q)) ->
    msat (ew_base rho) i (MRhatGt d p q).
Proof.
  intros rho path d p q i Hd Hcc IHp IHq HT.
  simpl in HT.
  destruct (clock_of root (MRhatGt d p q)) as [x|] eqn:Hxof; [|contradiction].
  pose proof (clock_of_proj Hxof) as Hxproj.
  simpl in HT.
  change (lsat rho (S i)
    (LW
      (LAnd (LAtom (LCLe (x) d)) (LAtom (LUnch (x))))
      (LOr
        (LRelease (T_at (left_path path) p)
                  (T_at (right_path path) q))
        (LAnd (LAtom (LCLe (x) d))
              (T_at (left_path path) p))))) in HT.
  apply (proj1 (@LW_semantics _ rho (S i)
    (LAnd (LAtom (LCLe (x) d)) (LAtom (LUnch (x))))
    (LOr
      (LRelease (T_at (left_path path) p)
                (T_at (right_path path) q))
      (LAnd (LAtom (LCLe (x) d))
            (T_at (left_path path) p))))) in HT.
  intros j Hij Hbound.
  assert (HSj : (S i <= j)%nat) by lia.
  destruct HT as [HU | HG].
  - destruct HU as [m [HSm [HE Hpre]]].
    destruct (le_lt_dec m j) as [Hmj | Hjm].
    + destruct HE as [HR | [Hle HAm]].
      * specialize (HR j Hmj).
        destruct HR as [HB | [k [Hk HA]]].
        -- left. apply IHq. exact HB.
        -- right. exists k. split; [lia|].
           apply IHp. exact HA.
      * destruct (Nat.eq_dec m j) as [-> | Hneq].
        -- exfalso.
           assert (Hpres :
             forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
           { intros k Hk.
             specialize (Hpre k Hk).
             exact (proj2 Hpre). }
           pose proof
             (preserve_from_next_elapsed_le_value
                (rho:=rho) (i:=i) (j:=j) (x:=x)
                Hcc HSj Hpres) as Hel.
           apply (Rle_not_lt _ _ Hle).
           apply Rlt_le_trans with (elapsed (ew_base rho) i j); auto.
        -- right. exists m. split; [lia|].
           apply IHp. exact HAm.
    + assert (Hjpre : (S i <= j < m)%nat) by lia.
      pose proof (Hpre j Hjpre) as Hprej.
      destruct Hprej as [Hlej _].
      exfalso.
      assert (Hpres :
        forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
      { intros k Hk.
        assert (Hkm : (S i <= k < m)%nat) by lia.
        specialize (Hpre k Hkm).
        exact (proj2 Hpre). }
      pose proof
        (preserve_from_next_elapsed_le_value
           (rho:=rho) (i:=i) (j:=j) (x:=x)
           Hcc HSj Hpres) as Hel.
      assert (Hval_le_d :
      ew_val rho j (x) <= d).
      {
        simpl in Hlej.
        unfold c_le in Hlej.
        exact Hlej.
      }
      unfold elapsed in Hel, Hbound.
      lra.
  - pose proof (HG j HSj) as HGj.
    destruct HGj as [Hlej Hun].
    exfalso.
    assert (Hpres :
      forall k : nat, (S i <= k < j)%nat -> unch_at rho (x) k).
    { intros k Hk.
      specialize (HG k (proj1 Hk)).
      exact (proj2 HG). }
    pose proof
      (preserve_from_next_elapsed_le_value
         (rho:=rho) (i:=i) (j:=j) (x:=x)
         Hcc HSj Hpres) as Hel.
    unfold c_le in Hlej.
    simpl in Hlej.
    unfold elapsed in Hel.
    unfold c_le in Hlej.
    lra.
Qed.



End Soundness.

(* ====================================================================== *)
(* 9. Canonical COMPLETENESS lemmas                                      *)
(* ====================================================================== *)

(* Small arithmetic and well-ordering helpers used below. *)

Lemma elapsed_nonnegative :
  forall w i j,
    (i <= j)%nat ->
    0 <= elapsed w i j.
Proof.
  intros w i j Hij.
  unfold elapsed.
  pose proof (time_monotone w Hij).
  lra.
Qed.

Lemma first_occurrence_bounded :
  forall (P : nat -> Prop) start stop,
    (start <= stop)%nat ->
    P stop ->
    exists m,
      (start <= m <= stop)%nat /\
      P m /\
      (forall k, (start <= k < m)%nat -> ~ P k).
Proof.
  intros P start stop.
  revert start.
  induction stop using (well_founded_induction lt_wf).
  intros start Hle HP.
  destruct (classic
    (exists k : nat, (start <= k < stop)%nat /\ P k))
    as [Hex | Hnone].
  - destruct Hex as [k [[Hsk Hks] HPk]].
    destruct (H k Hks start Hsk HPk)
      as [m [[Hsm Hmk] [HPm Hmin]]].
    exists m.
    repeat split; try assumption; try lia.
  - exists stop.
    repeat split; try assumption; try lia.
    intros k Hk HPk.
    apply Hnone.
    exists k.
    split; assumption.
Qed.

Lemma policy_UhatLe_preserve_inv :
  forall root w path d p q i,
    node_at root path = Some (MUhatLe d p q) ->
    canonical_reset root w i ((MUhatLe d p q)) = false ->
    canonical_val root w ((MUhatLe d p q)) i <= d /\
    ule_residual w i (canonical_val root w ((MUhatLe d p q)) i) d p q /\
    ~ msat w i q.
Proof.
  intros root w path d p q i Hnode Hreset.
  unfold canonical_reset, reset_policy in Hreset.
  simpl in Hreset.
  exact (negb_decide_false_elim Hreset).
Qed.

Lemma policy_RhatGe_preserve_inv :
  forall root w path d p q i,
    node_at root path = Some (MRhatGe d p q) ->
    canonical_reset root w i ((MRhatGe d p q)) = false ->
    canonical_val root w ((MRhatGe d p q)) i < d /\
    rge_residual w i (canonical_val root w ((MRhatGe d p q)) i) d p q /\
    ~ msat w i p.
Proof.
  intros root w path d p q i Hnode Hreset.
  unfold canonical_reset, reset_policy in Hreset.
  simpl in Hreset.
  exact (negb_decide_false_elim Hreset).
Qed.

Lemma MRhatGe_after_step_residual :
  forall w i d p q,
    0 < d ->
    msat w i (MRhatGe d p q) ->
    rge_residual w (S i) (delta w i) d p q.
Proof.
  intros w i d p q Hd Hmtl.
  unfold rge_residual.
  intros j Hsj Hthreshold.
  simpl in Hmtl.
  assert (Hij : (i < j)%nat) by lia.
  assert (Hphysical : d <= tw_time w j - tw_time w i).
  { unfold elapsed, delta in *; lra. }
  destruct (Hmtl j Hij Hphysical) as [Hq | [k [Hk Hp]]].
  - left. exact Hq.
  - right.
    exists k.
    destruct Hk as [Hik Hkj].
    split; [lia | exact Hp].
Qed.

Lemma well_formed_one_step :
  forall root b child,
    well_formed root ->
    node_at root [b] = Some child ->
    well_formed child.
Proof.
  intros root b child Hwf Hstep.
  destruct root; simpl in Hstep; try discriminate;
    simpl in Hwf;
    destruct b; simpl in Hstep; try discriminate;
    try (rewrite node_at_nil in Hstep);
    inversion Hstep; subst; tauto.
Qed.

Lemma well_formed_node_at :
  forall root path f,
    well_formed root ->
    node_at root path = Some f ->
    well_formed f.
Proof.
  intros root path f Hwf Hnode.
  revert root Hwf Hnode.
  induction path as [|b tl IH]; intros root Hwf Hnode.
  - rewrite node_at_nil in Hnode.
    inversion Hnode; subst.
    exact Hwf.
  - destruct (node_at root [b]) as [m|] eqn:Hfirst.
    + assert (Htail : node_at m tl = Some f).
      { change (node_at root ([b] ++ tl) = Some f) in Hnode.
        rewrite node_at_app, Hfirst in Hnode.
        exact Hnode. }
      assert (Hwm : well_formed m).
      { eapply well_formed_one_step; eauto. }
      eapply IH; eauto.
    + exfalso.
      change (node_at root ([b] ++ tl) = Some f) in Hnode.
      rewrite node_at_app, Hfirst in Hnode.
      discriminate.
Qed.

Definition uhatge_event
    (root : mtl) (w : timed_word) (path : Path)
    (d : R) (p q : mtl) (n : nat) : Prop :=
  d <= canonical_val root w ((MUhatGe d p q)) n /\
  exists m,
    (n <= m)%nat /\
    msat w m q /\
    (forall k, (n <= k < m)%nat -> msat w k p).

Definition rhatle_event
    (root : mtl) (w : timed_word) (path : Path)
    (d : R) (p q : mtl) (n : nat) : Prop :=
  d < canonical_val root w ((MRhatLe d p q)) n \/
  (msat w n p /\ msat w n q).

Definition rhatge_event
    (root : mtl) (w : timed_word) (path : Path)
    (d : R) (p q : mtl) (n : nat) : Prop :=
  msat w n (MR p q) \/
  (canonical_val root w ((MRhatGe d p q)) n < d /\ msat w n p).


Lemma policy_UhatLt_preserve_inv :
  forall root w path d p q i,
    node_at root path = Some (MUhatLt d p q) ->
    canonical_reset root w i ((MUhatLt d p q)) = false ->
    canonical_val root w ((MUhatLt d p q)) i < d /\
    ult_residual w i (canonical_val root w ((MUhatLt d p q)) i) d p q /\
    ~ msat w i q.
Proof.
  intros root w path d p q i Hnode Hreset.
  unfold canonical_reset, reset_policy in Hreset.
  simpl in Hreset.
  exact (negb_decide_false_elim Hreset).
Qed.

Lemma policy_RhatGt_preserve_inv :
  forall root w path d p q i,
    node_at root path = Some (MRhatGt d p q) ->
    canonical_reset root w i ((MRhatGt d p q)) = false ->
    canonical_val root w ((MRhatGt d p q)) i <= d /\
    rgt_residual w i (canonical_val root w ((MRhatGt d p q)) i) d p q /\
    ~ msat w i p.
Proof.
  intros root w path d p q i Hnode Hreset.
  unfold canonical_reset, reset_policy in Hreset.
  simpl in Hreset.
  exact (negb_decide_false_elim Hreset).
Qed.

Lemma MRhatGt_after_step_residual :
  forall w i d p q,
    0 < d ->
    msat w i (MRhatGt d p q) ->
    rgt_residual w (S i) (delta w i) d p q.
Proof.
  intros w i d p q Hd Hmtl.
  unfold rgt_residual.
  intros j Hsj Hthreshold.
  simpl in Hmtl.
  assert (Hij : (i < j)%nat) by lia.
  assert (Hphysical : d < tw_time w j - tw_time w i).
  { unfold elapsed, delta in *; lra. }
  destruct (Hmtl j Hij Hphysical) as [Hq | [k [Hk Hp]]].
  - left. exact Hq.
  - right.
    exists k.
    destruct Hk as [Hik Hkj].
    split; [lia | exact Hp].
Qed.

Definition uhatgt_event
    (root : mtl) (w : timed_word) (path : Path)
    (d : R) (p q : mtl) (n : nat) : Prop :=
  d < canonical_val root w ((MUhatGt d p q)) n /\
  exists m,
    (n <= m)%nat /\
    msat w m q /\
    (forall k, (n <= k < m)%nat -> msat w k p).

Definition rhatlt_event
    (root : mtl) (w : timed_word) (path : Path)
    (d : R) (p q : mtl) (n : nat) : Prop :=
  d <= canonical_val root w ((MRhatLt d p q)) n \/
  (msat w n p /\ msat w n q).

Definition rhatgt_event
    (root : mtl) (w : timed_word) (path : Path)
    (d : R) (p q : mtl) (n : nat) : Prop :=
  msat w n (MR p q) \/
  (canonical_val root w ((MRhatGt d p q)) n <= d /\ msat w n p).

(* ---------------------------------------------------------------------- *)
(* U <=                                                                   *)
(* ---------------------------------------------------------------------- *)

Lemma complete_UhatLe :
  forall root w path d p q,
    node_at root path = Some (MUhatLe d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MUhatLe d p q) ->
      lsat (canonical_ext root w) i (T_at path (MUhatLe d p q)).
Proof.
  intros root w path d p q Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MUhatLe d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  destruct Hmtl as [j [Hij [Hbound [Hq Hp]]]].
  assert (HSj : (S i <= j)%nat) by lia.

  destruct
    (@first_occurrence_bounded
       (fun n => msat w n q) (S i) j HSj Hq)
    as [j0 [[HSj0 Hj0j] [Hq0 Hnoq]]].

  assert (Hbound0 : elapsed w i j0 <= d).
  {
    unfold elapsed in *.
    pose proof (time_monotone w Hj0j).
    lra.
  }
  assert (Hp0 :
    forall k : nat, (S i <= k < j0)%nat -> msat w k p).
  {
    intros k Hk.
    apply Hp.
    lia.
  }

  assert (Hinv :
    forall n : nat,
      (S i <= n <= j0)%nat ->
      canonical_val root w ((MUhatLe d p q)) n + elapsed w n j0 <= d).
  {
    intros n Hrange.
    induction n using (well_founded_induction lt_wf).
    destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
    - subst n.
      change
        ((if canonical_reset root w i ((MUhatLe d p q))
          then delta w i
          else canonical_val root w ((MUhatLe d p q)) i + delta w i)
         + elapsed w (S i) j0 <= d).
      destruct (canonical_reset root w i ((MUhatLe d p q))) eqn:Hr.
      + unfold elapsed, delta in *.
        lra.
      + destruct (policy_UhatLe_preserve_inv Hnode Hr)
          as [_ [Hres Hnqi]].
        destruct Hres as [m [Him [Hresbound [Hqm _]]]].
        assert (HSim : (S i <= m)%nat).
        {
          destruct (Nat.eq_dec m i) as [-> | Hneq].
          - contradiction.
          - lia.
        }
        assert (Hj0m : (j0 <= m)%nat).
        {
          destruct (le_dec j0 m) as [Hle | Hnle].
          - exact Hle.
          - exfalso.
            apply (Hnoq m).
            + lia.
            + exact Hqm.
        }
        pose proof (time_monotone w Hj0m).
        unfold elapsed, delta in *.
        lra.
    - assert (Hpred : (S i <= pred n < n)%nat) by lia.
      assert (Hpred_lt : (pred n < n)%nat) by lia.
      assert (Hpred_range : (S i <= pred n <= j0)%nat) by lia.
      specialize (H (pred n) Hpred_lt Hpred_range).
      assert (Hpred_j0 : (pred n <= j0)%nat) by lia.
      assert (Hel_nonneg : 0 <= elapsed w (pred n) j0).
      { apply elapsed_nonnegative. exact Hpred_j0. }
      assert (Hval_le :
        canonical_val root w ((MUhatLe d p q)) (pred n) <= d) by lra.
      assert (Hres :
        ule_residual w (pred n)
          (canonical_val root w ((MUhatLe d p q)) (pred n)) d p q).
      {
        unfold ule_residual.
        exists j0.
        repeat split.
        + lia.
        + exact H.
        + exact Hq0.
        + intros h Hh.
          apply Hp0.
          lia.
      }
      assert (Hnq : ~ msat w (pred n) q).
      { apply Hnoq. lia. }
      pose proof
        (policy_UhatLe_preserve Hnode Hval_le Hres Hnq) as Hunch.
      unfold canonical_reset in Hunch.
      replace n with (S (pred n)) by lia.
      simpl.
      unfold reset_policy in Hunch; simpl in Hunch.
      rewrite Hunch.
      unfold elapsed, delta in *.
      lra.
  }

  simpl.
  exists j0.
  repeat split.
  - exact HSj0.
  - unfold c_le; simpl.
    assert (Hj0range : (S i <= j0 <= j0)%nat) by lia.
    specialize (Hinv j0 Hj0range).
    unfold elapsed in Hinv.
    lra.
  - apply (proj1 (IHq j0)). exact Hq0.
  - unfold c_le; simpl.
      assert (Hkrange : (S i <= k <= j0)%nat) by lia.
      specialize (Hinv k Hkrange).
      assert (Hkj0 : (k <= j0)%nat) by lia.
      pose proof (@elapsed_nonnegative w k j0 Hkj0) as Hel_nonneg.
      lra.
  -  unfold unch_at, canonical_ext; simpl.
      assert (Hkrange : (S i <= k <= j0)%nat) by lia.
      specialize (Hinv k Hkrange).
      assert (Hel : 0 <= elapsed w k j0).
      { apply elapsed_nonnegative. lia. }
      assert (Hval_le : canonical_val root w ((MUhatLe d p q)) k <= d) by lra.
      assert (Hres :
        ule_residual w k (canonical_val root w ((MUhatLe d p q)) k) d p q).
      {
        unfold ule_residual.
        exists j0.
        repeat split.
        * lia.
        * exact Hinv.
        * exact Hq0.
        * intros h Hh.
          apply Hp0.
          lia.
      }
      apply policy_UhatLe_preserve with (path:=path); simpl; intros; auto; lia.
   -  apply (proj1 (IHp k)).
      apply Hp0. lia.
Qed.

(* ---------------------------------------------------------------------- *)
(* Uhat >=                                                                *)
(* ---------------------------------------------------------------------- *)

Lemma complete_UhatGe :
  forall root w path d p q,
    0 < d ->
    node_at root path = Some (MUhatGe d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MUhatGe d p q) ->
      lsat (canonical_ext root w) i (T_at path (MUhatGe d p q)).
Proof.
  intros root w path d p q Hd Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MUhatGe d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  pose proof Hmtl as HFi.
  destruct Hmtl as [j [Hij [Hfar [Hq Hp]]]].

  simpl.
  split.
  - unfold rst_at, canonical_ext; simpl.
     apply policy_UhatGe_reset with (root:=root) (path:=path); simpl; intros; auto; lia.
  - simpl.

    assert (Hactive :
      forall n : nat,
        (S i <= n)%nat ->
        (forall r : nat,
           (S i <= r < n)%nat ->
           ~ uhatge_event root w path d p q r) ->
        exists s m,
          (i <= s < n)%nat /\
          msat w s (MUhatGe d p q) /\
          (n <= m)%nat /\
          d <= elapsed w s m /\
          msat w m q /\
          (forall k : nat,
             (s < k < m)%nat -> msat w k p) /\
          canonical_val root w ((MUhatGe d p q)) n = elapsed w s n).
    {
      intro n.
      induction n using (well_founded_induction lt_wf).
      intros HSn Hnone.
      destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
      - subst n.
        exists i, j.
        repeat split; try assumption; try lia.
        pose proof (policy_UhatGe_reset Hnode HFi) as Hr.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        unfold elapsed, delta.
        reflexivity.
      - assert (Hpred_range : (S i <= pred n)%nat) by lia.
        assert (Hnone_pred :
          forall r : nat,
            (S i <= r < pred n)%nat ->
            ~ uhatge_event root w path d p q r).
        {
          intros r Hr.
          apply Hnone.
          lia.
        }
        assert (Hpred_lt : (pred n < n)%nat) by lia.
        pose proof
          (H (pred n) Hpred_lt Hpred_range Hnone_pred)
          as Hprev.
        destruct Hprev
          as [s [m [[His Hsn]
               [HFs [Hkm [Hfar_m [Hqm [Hpm Hval]]]]]]]].

        assert (Hkm_strict : (pred n < m)%nat).
        {
          destruct (Nat.eq_dec (pred n) m) as [Heq | Hneq].
          - subst m.
            exfalso.
            apply (Hnone (pred n)); [lia |].
            unfold uhatge_event.
            split.
            + rewrite Hval. exact Hfar_m.
            + exists (pred n).
              repeat split; try lia; try assumption.
          - lia.
        }

        destruct (classic (msat w (pred n) (MUhatGe d p q)))
          as [HFk | HnotFk].
        + pose proof HFk as HFk_full.
          simpl in HFk.
          destruct HFk as [m2 [Hkm2 [Hfar2 [Hq2 Hp2]]]].
          exists (pred n), m2.
          repeat split; try assumption; try lia.
          pose proof (policy_UhatGe_reset Hnode HFk_full) as Hr.
          unfold canonical_reset in Hr.
          replace n with (S (pred n)) by lia.
          simpl.
          unfold reset_policy in Hr; simpl in Hr.
          rewrite Hr.
          unfold elapsed, delta.
          reflexivity.
        + exists s, m.
          repeat split; try assumption; try lia.
          assert (Hr : canonical_reset root w (pred n) ((MUhatGe d p q)) = false).
          {
            unfold canonical_reset, reset_policy; simpl.
            apply (proj2 (decide_false
              (msat w (pred n) (MUhatGe d p q)))).
            exact HnotFk.
          }
          unfold canonical_reset in Hr.
          replace n with (S (pred n)) by lia.
          simpl.
          unfold reset_policy in Hr; simpl in Hr.
          rewrite Hr, Hval.
          unfold elapsed, delta.
          lra.
    }

    destruct (classic
      (exists r : nat,
         (S i <= r)%nat /\
         uhatge_event root w path d p q r))
      as [Hevent | Hnever].

    + destruct Hevent as [r [HSr Her]].
      destruct
        (@first_occurrence_bounded
           (uhatge_event root w path d p q) (S i) r HSr Her)
        as [r0 [[HSr0 Hr0r] [Her0 Hfirst]]].
      left.
      exists r0.
      split; [exact HSr0 |].
      split.
      * unfold uhatge_event in Her0.
        destruct Her0 as [Hge [m [Hrm [Hqm Hpm]]]].
        split.
        -- unfold c_ge; simpl. exact Hge.
        -- exists m.
           repeat split; try assumption.
           ++ apply (proj1 (IHq m)). exact Hqm.
           ++ intros k Hk.
              apply (proj1 (IHp k)).
              apply Hpm. exact Hk.
      * intros k Hk.
        apply (proj1 (IHp k)).
        assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ uhatge_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst.
          lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        destruct (Hactive k HSk Hnone_k)
          as [s [m [[His Hsk]
               [HFs [Hkm [Hfar_m [Hqm [Hpm Hval]]]]]]]].
        assert (Hkm_strict : (k < m)%nat).
        {
          destruct (Nat.eq_dec k m) as [Heq | Hneq].
          - subst m.
            exfalso.
            apply (Hfirst k); [lia |].
            unfold uhatge_event.
            split.
            + rewrite Hval. exact Hfar_m.
            + exists k.
              repeat split; try lia; try assumption.
          - lia.
        }
        apply Hpm.
        lia.

    + right.
      split.
      * change (lsat (canonical_ext root w) (S i)
          (LG (T_at (left_path path) p))).
        apply (proj2 (@LG_semantics _ (canonical_ext root w) (S i)
          (T_at (left_path path) p))).
        intros n HSn.
        apply (proj1 (IHp n)).
        assert (Hnone_n :
          forall t : nat,
            (S i <= t < n)%nat ->
            ~ uhatge_event root w path d p q t).
        {
          intros t Ht Het.
          apply Hnever.
          exists t.
          split; [lia | exact Het].
        }
        destruct (Hactive n HSn Hnone_n)
          as [s [m [[His Hsn]
               [HFs [Hnm [Hfar_m [Hqm [Hpm Hval]]]]]]]].
        assert (Hnm_strict : (n < m)%nat).
        {
          destruct (Nat.eq_dec n m) as [Heq | Hneq].
          - subst m.
            exfalso.
            apply Hnever.
            exists n.
            split; [exact HSn |].
            unfold uhatge_event.
            split.
            + rewrite Hval. exact Hfar_m.
            + exists n.
              repeat split; try lia; try assumption.
          - lia.
        }
        apply Hpm.
        lia.
      * change (lsat (canonical_ext root w) (S i)
          (LGF (T_at (right_path path) q))).
        apply (proj2 (@LGF_semantics _ (canonical_ext root w) (S i)
          (T_at (right_path path) q))).
        intros n HSn.
        assert (Hnone_n :
          forall t : nat,
            (S i <= t < n)%nat ->
            ~ uhatge_event root w path d p q t).
        {
          intros t Ht Het.
          apply Hnever.
          exists t.
          split; [lia | exact Het].
        }
        destruct (Hactive n HSn Hnone_n)
          as [s [m [[His Hsn]
               [HFs [Hnm [Hfar_m [Hqm [Hpm Hval]]]]]]]].
        exists m.
        split; [exact Hnm |].
        apply (proj1 (IHq m)). exact Hqm.
Qed.

(* ---------------------------------------------------------------------- *)
(* Rhat <=                                                                *)
(* ---------------------------------------------------------------------- *)

Lemma complete_RhatLe :
  forall root w path d p q,
    node_at root path = Some (MRhatLe d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MRhatLe d p q) ->
      lsat (canonical_ext root w) i (T_at path (MRhatLe d p q)).
Proof.
  intros root w path d p q Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MRhatLe d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  pose proof Hmtl as HFi.
  simpl.
  split.
  - unfold rst_at, canonical_ext.
    apply policy_RhatLe_reset with (path:=path); simpl; intros; auto; lia.
  - change (lsat (canonical_ext root w) (S i)
      (LW
        (T_at (right_path path) q)
        (LOr (LAtom (LCGt (exist _ (MRhatLe d p q) Hmem) d))
             (LAnd (T_at (left_path path) p)
                   (T_at (right_path path) q))))).
    apply (proj2 (@LW_semantics _ (canonical_ext root w) (S i)
      (T_at (right_path path) q)
      (LOr (LAtom (LCGt (exist _ (MRhatLe d p q) Hmem) d))
           (LAnd (T_at (left_path path) p)
                 (T_at (right_path path) q))))).

    assert (Hactive :
      forall n : nat,
        (S i <= n)%nat ->
        (forall r : nat,
           (S i <= r < n)%nat ->
           ~ rhatle_event root w path d p q r) ->
        exists s,
          (i <= s < n)%nat /\
          msat w s (MRhatLe d p q) /\
          canonical_val root w ((MRhatLe d p q)) n = elapsed w s n /\
          (forall h : nat,
             (s < h < n)%nat -> ~ msat w h p)).
    {
      intro n.
      induction n using (well_founded_induction lt_wf).
      intros HSn Hnone.
      destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
      - subst n.
        exists i.
        repeat split; try assumption; try lia.
        pose proof (policy_RhatLe_reset Hnode HFi) as Hr.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        unfold elapsed, delta.
        reflexivity.
      - assert (Hpred_range : (S i <= pred n)%nat) by lia.
        assert (Hnone_pred :
          forall r : nat,
            (S i <= r < pred n)%nat ->
            ~ rhatle_event root w path d p q r).
        {
          intros r Hr.
          apply Hnone.
          lia.
        }
        assert (Hpred_lt : (pred n < n)%nat) by lia.
        pose proof
          (H (pred n) Hpred_lt Hpred_range Hnone_pred)
          as Hprev.
        destruct Hprev as [s [[His Hsk] [HFs [Hval Hnop]]]].
        assert (Hnoevent_k :
          ~ rhatle_event root w path d p q (pred n)).
        { apply Hnone. lia. }
        assert (Hle : canonical_val root w ((MRhatLe d p q)) (pred n) <= d).
        {
          destruct (Rle_dec' (canonical_val root w ((MRhatLe d p q)) (pred n)) d)
            as [Hle | Hnle].
          - exact Hle.
          - exfalso.
            apply Hnoevent_k.
            unfold rhatle_event.
            left. lra.
        }
        assert (Hqk : msat w (pred n) q).
        {
          pose proof HFs as Hrel.
          simpl in Hrel.
          assert (Hs_pred : (s < pred n)%nat) by lia.
          specialize (Hrel (pred n) Hs_pred).
          assert (Hphys : tw_time w (pred n) - tw_time w s <= d).
          {
            rewrite Hval in Hle.
            unfold elapsed in Hle.
            exact Hle.
          }
          specialize (Hrel Hphys).
          destruct Hrel as [Hqk | [h [Hh Hph]]].
          - exact Hqk.
          - exfalso.
            apply (Hnop h); [exact Hh | exact Hph].
        }
        assert (Hnpk : ~ msat w (pred n) p).
        {
          intro Hpk.
          apply Hnoevent_k.
          unfold rhatle_event.
          right.
          split; assumption.
        }

        destruct (classic (msat w (pred n) (MRhatLe d p q)))
          as [HFk | HnotFk].
        + exists (pred n).
          repeat split; try assumption; try lia.
          pose proof (policy_RhatLe_reset Hnode HFk) as Hr.
          unfold canonical_reset in Hr.
          replace n with (S (pred n)) by lia.
          simpl.
          unfold reset_policy in Hr; simpl in Hr.
          rewrite Hr.
          unfold elapsed, delta.
          ring.
        + exists s.
          repeat split; try assumption; try lia.
          * assert (Hr : canonical_reset root w (pred n) ((MRhatLe d p q)) = false).
            {
              unfold canonical_reset, reset_policy.
              apply (proj2 (decide_false
                (msat w (pred n) (MRhatLe d p q)))).
              exact HnotFk.
            }
            unfold canonical_reset in Hr.
            replace n with (S (pred n)) by lia.
            simpl.
            unfold reset_policy in Hr; simpl in Hr.
            rewrite Hr, Hval.
            unfold elapsed, delta.
            lra.
          * intros h Hh.
            destruct (Nat.eq_dec h (pred n)) as [-> | Hneq].
            -- exact Hnpk.
            -- apply Hnop. lia.
    }

    destruct (classic
      (exists r : nat,
         (S i <= r)%nat /\
         rhatle_event root w path d p q r))
      as [Hevent | Hnever].

    + destruct Hevent as [r [HSr Her]].
      destruct
        (@first_occurrence_bounded
           (rhatle_event root w path d p q) (S i) r HSr Her)
        as [r0 [[HSr0 Hr0r] [Her0 Hfirst]]].
      left.
      exists r0.
      split; [exact HSr0 |].
      split.
      * unfold rhatle_event in Her0.
        destruct Her0 as [Hgt | [Hp_r Hq_r]].
        -- left. unfold c_gt; simpl. exact Hgt.
        -- right. split.
           ++ apply (proj1 (IHp r0)). exact Hp_r.
           ++ apply (proj1 (IHq r0)). exact Hq_r.
      * intros k Hk.
        apply (proj1 (IHq k)).
        assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ rhatle_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst.
          lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        destruct (Hactive k HSk Hnone_k)
          as [s [[His Hsk] [HFs [Hval Hnop]]]].
        assert (Hnoevent_k :
          ~ rhatle_event root w path d p q k).
        { apply Hfirst. lia. }
        assert (Hle : canonical_val root w ((MRhatLe d p q)) k <= d).
        {
          destruct (Rle_dec' (canonical_val root w ((MRhatLe d p q)) k) d)
            as [Hle | Hnle].
          - exact Hle.
          - exfalso.
            apply Hnoevent_k.
            unfold rhatle_event.
            left. lra.
        }
        pose proof HFs as Hrel.
        simpl in Hrel.
        assert (Hsk_le : (s < k)%nat) by lia.
        specialize (Hrel k Hsk_le).
        assert (Hphys : tw_time w k - tw_time w s <= d).
        {
          rewrite Hval in Hle.
          unfold elapsed in Hle.
          exact Hle.
        }
        specialize (Hrel Hphys).
        destruct Hrel as [Hqk | [h [Hh Hph]]].
        -- exact Hqk.
        -- exfalso. apply (Hnop h); assumption.

    + right.
      intros k HSk.
      apply (proj1 (IHq k)).
      assert (Hnone_k :
        forall t : nat,
          (S i <= t < k)%nat ->
          ~ rhatle_event root w path d p q t).
      {
        intros t Ht Het.
        apply Hnever.
        exists t.
        split; [lia | exact Het].
      }
      destruct (Hactive k HSk Hnone_k)
        as [s [[His Hsk] [HFs [Hval Hnop]]]].
      assert (Hnoevent_k :
        ~ rhatle_event root w path d p q k).
      {
        intro Het.
        apply Hnever.
        exists k.
        split; assumption.
      }
      assert (Hle : canonical_val root w ((MRhatLe d p q)) k <= d).
      {
        destruct (Rle_dec' (canonical_val root w ((MRhatLe d p q)) k) d)
          as [Hle | Hnle].
        - exact Hle.
        - exfalso.
          apply Hnoevent_k.
          unfold rhatle_event.
          left. lra.
      }
      pose proof HFs as Hrel.
      simpl in Hrel.
      assert (Hsk_le : (s < k)%nat) by lia.
      specialize (Hrel k Hsk_le).
      assert (Hphys : tw_time w k - tw_time w s <= d).
      {
        rewrite Hval in Hle.
        unfold elapsed in Hle.
        exact Hle.
      }
      specialize (Hrel Hphys).
      destruct Hrel as [Hqk | [h [Hh Hph]]].
      * exact Hqk.
      * exfalso. apply (Hnop h); assumption.
Qed.

(* ---------------------------------------------------------------------- *)
(* Rhat >=                                                                *)
(* ---------------------------------------------------------------------- *)

Lemma rge_residual_shift :
  forall w i v d p q,
    rge_residual w i v d p q ->
    ~ msat w i p ->
    rge_residual w (S i) (v + delta w i) d p q.
Proof.
  intros w i v d p q Hres Hnp.
  unfold rge_residual in *.
  intros j Hsj Hthreshold.
  assert (Hij : (i <= j)%nat) by lia.
  assert (Hthreshold' : d <= v + elapsed w i j).
  { unfold elapsed, delta in *; lra. }
  destruct (Hres j Hij Hthreshold') as [Hq | [k [Hk Hp]]].
  - left. exact Hq.
  - destruct (Nat.eq_dec k i) as [-> | Hneq].
    + contradiction.
    + right.
      exists k.
      split; [lia | exact Hp].
Qed.

Lemma complete_RhatGe :
  forall root w path d p q,
    0 < d ->
    node_at root path = Some (MRhatGe d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MRhatGe d p q) ->
      lsat (canonical_ext root w) i (T_at path (MRhatGe d p q)).
Proof.
  intros root w path d p q Hd Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MRhatGe d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  simpl.
  change (lsat (canonical_ext root w) (S i)
    (LW
      (LAnd (LAtom (LCLt (exist _ (MRhatGe d p q) Hmem) d)) (LAtom (LUnch (exist _ (MRhatGe d p q) Hmem))))
      (LOr
        (LRelease (T_at (left_path path) p)
                  (T_at (right_path path) q))
        (LAnd (LAtom (LCLt (exist _ (MRhatGe d p q) Hmem) d))
              (T_at (left_path path) p))))).
  apply (proj2 (@LW_semantics _ (canonical_ext root w) (S i)
    (LAnd (LAtom (LCLt (exist _ (MRhatGe d p q) Hmem) d)) (LAtom (LUnch (exist _ (MRhatGe d p q) Hmem))))
    (LOr
      (LRelease (T_at (left_path path) p)
                (T_at (right_path path) q))
      (LAnd (LAtom (LCLt (exist _ (MRhatGe d p q) Hmem) d))
            (T_at (left_path path) p))))).

  assert (Hactive :
    forall n : nat,
      (S i <= n)%nat ->
      (forall r : nat,
         (S i <= r < n)%nat ->
         ~ rhatge_event root w path d p q r) ->
      rge_residual w n (canonical_val root w ((MRhatGe d p q)) n) d p q).
  {
    intro n.
    induction n using (well_founded_induction lt_wf).
    intros HSn Hnone.
    destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
    - subst n.
      destruct (canonical_reset root w i ((MRhatGe d p q))) eqn:Hr.
      + pose proof (MRhatGe_after_step_residual Hd Hmtl) as Hshift.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        exact Hshift.
      + destruct (policy_RhatGe_preserve_inv Hnode Hr)
          as [_ [Hres Hnp]].
        pose proof (rge_residual_shift Hres Hnp) as Hshift.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        exact Hshift.
    - assert (Hpred_range : (S i <= pred n)%nat) by lia.
      assert (Hnone_pred :
        forall r : nat,
          (S i <= r < pred n)%nat ->
          ~ rhatge_event root w path d p q r).
      {
        intros r Hr.
        apply Hnone.
        lia.
      }
      assert (Hpred_lt : (pred n < n)%nat) by lia.
      pose proof
        (H (pred n) Hpred_lt Hpred_range Hnone_pred)
        as Hres.
      assert (Hnoevent_k :
        ~ rhatge_event root w path d p q (pred n)).
      { apply Hnone. lia. }
      assert (Hlt : canonical_val root w ((MRhatGe d p q)) (pred n) < d).
      {
        destruct (Rlt_dec' (canonical_val root w ((MRhatGe d p q)) (pred n)) d)
          as [Hlt | Hnlt].
        - exact Hlt.
        - exfalso.
          apply Hnoevent_k.
          unfold rhatge_event.
          left.
          simpl.
          intros j Hkj.
          assert (Hel : 0 <= elapsed w (pred n) j).
          { apply elapsed_nonnegative. exact Hkj. }
          assert (Hthreshold :
            d <= canonical_val root w ((MRhatGe d p q)) (pred n) +
                 elapsed w (pred n) j) by lra.
          exact (Hres j Hkj Hthreshold).
      }
      assert (Hnpk : ~ msat w (pred n) p).
      {
        intro Hpk.
        apply Hnoevent_k.
        unfold rhatge_event.
        right. split; assumption.
      }
      pose proof
        (policy_RhatGe_preserve Hnode Hlt Hres Hnpk) as Hunch.
      pose proof (rge_residual_shift Hres Hnpk) as Hshift.
      unfold canonical_reset in Hunch.
      replace n with (S (pred n)) by lia.
      simpl.
      unfold reset_policy in Hunch; simpl in Hunch.
      rewrite Hunch.
      exact Hshift.
  }

  destruct (classic
    (exists r : nat,
       (S i <= r)%nat /\
       rhatge_event root w path d p q r))
    as [Hevent | Hnever].

  - destruct Hevent as [r [HSr Her]].
    destruct
      (@first_occurrence_bounded
         (rhatge_event root w path d p q) (S i) r HSr Her)
      as [r0 [[HSr0 Hr0r] [Her0 Hfirst]]].
    left.
    exists r0.
    split; [exact HSr0 |].
    split.
    + unfold rhatge_event in Her0.
      destruct Her0 as [HR | [Hlt Hp_r]].
      * left.
        simpl in HR |- *.
        intros j Hrj.
        specialize (HR j Hrj).
        destruct HR as [Hqj | [h [Hh Hph]]].
        -- left. apply (proj1 (IHq j)). exact Hqj.
        -- right. exists h. split; [exact Hh |].
           apply (proj1 (IHp h)). exact Hph.
      * right. split.
        -- unfold c_lt; simpl. exact Hlt.
        -- apply (proj1 (IHp r0)). exact Hp_r.
    + intros k Hk.
      split.
      * assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ rhatge_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst. lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        pose proof (Hactive k HSk Hnone_k) as Hres.
        assert (Hnoevent_k :
          ~ rhatge_event root w path d p q k).
        { apply Hfirst. lia. }
        unfold c_lt; simpl.
        destruct (Rlt_dec' (canonical_val root w ((MRhatGe d p q)) k) d)
          as [Hlt | Hnlt].
        -- exact Hlt.
        -- exfalso.
           apply Hnoevent_k.
           unfold rhatge_event.
           left.
           simpl.
           intros j Hkj.
           assert (Hel : 0 <= elapsed w k j).
           { apply elapsed_nonnegative. exact Hkj. }
           assert (Hthreshold :
             d <= canonical_val root w ((MRhatGe d p q)) k + elapsed w k j)
             by lra.
           exact (Hres j Hkj Hthreshold).
      * unfold unch_at, canonical_ext; simpl.
        assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ rhatge_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst. lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        pose proof (Hactive k HSk Hnone_k) as Hres.
        assert (Hnoevent_k :
          ~ rhatge_event root w path d p q k).
        { apply Hfirst. lia. }
        assert (Hlt : canonical_val root w ((MRhatGe d p q)) k < d).
        {
          destruct (Rlt_dec' (canonical_val root w ((MRhatGe d p q)) k) d)
            as [Hlt | Hnlt].
          - exact Hlt.
          - exfalso.
            apply Hnoevent_k.
            unfold rhatge_event.
            left.
            simpl.
            intros j Hkj.
            assert (Hel : 0 <= elapsed w k j).
            { apply elapsed_nonnegative. exact Hkj. }
            assert (Hthreshold :
              d <= canonical_val root w ((MRhatGe d p q)) k + elapsed w k j)
              by lra.
            exact (Hres j Hkj Hthreshold).
        }
        assert (Hnpk : ~ msat w k p).
        {
          intro Hpk.
          apply Hnoevent_k.
          unfold rhatge_event.
          right. split; assumption.
        }
        apply policy_RhatGe_preserve with (path:=path); intros; auto; lia.

  - right.
    intros k HSk.
    split.
    + assert (Hnone_k :
        forall t : nat,
          (S i <= t < k)%nat ->
          ~ rhatge_event root w path d p q t).
      {
        intros t Ht Het.
        apply Hnever.
        exists t. split; [lia | exact Het].
      }
      pose proof (Hactive k HSk Hnone_k) as Hres.
      assert (Hnoevent_k : ~ rhatge_event root w path d p q k).
      {
        intro Het.
        apply Hnever.
        exists k. split; assumption.
      }
      unfold c_lt; simpl.
      destruct (Rlt_dec' (canonical_val root w ((MRhatGe d p q)) k) d)
        as [Hlt | Hnlt].
      * exact Hlt.
      * exfalso.
        apply Hnoevent_k.
        unfold rhatge_event.
        left.
        simpl.
        intros j Hkj.
        assert (Hel : 0 <= elapsed w k j).
        { apply elapsed_nonnegative. exact Hkj. }
        assert (Hthreshold :
          d <= canonical_val root w ((MRhatGe d p q)) k + elapsed w k j)
          by lra.
        exact (Hres j Hkj Hthreshold).
    + unfold unch_at, canonical_ext; simpl.
      assert (Hnone_k :
        forall t : nat,
          (S i <= t < k)%nat ->
          ~ rhatge_event root w path d p q t).
      {
        intros t Ht Het.
        apply Hnever.
        exists t. split; [lia | exact Het].
      }
      pose proof (Hactive k HSk Hnone_k) as Hres.
      assert (Hnoevent_k : ~ rhatge_event root w path d p q k).
      {
        intro Het.
        apply Hnever.
        exists k. split; assumption.
      }
      assert (Hlt : canonical_val root w ((MRhatGe d p q)) k < d).
      {
        destruct (Rlt_dec' (canonical_val root w ((MRhatGe d p q)) k) d)
          as [Hlt | Hnlt].
        - exact Hlt.
        - exfalso.
          apply Hnoevent_k.
          unfold rhatge_event.
          left.
          simpl.
          intros j Hkj.
          assert (Hel : 0 <= elapsed w k j).
          { apply elapsed_nonnegative. exact Hkj. }
          assert (Hthreshold :
            d <= canonical_val root w ((MRhatGe d p q)) k + elapsed w k j)
            by lra.
          exact (Hres j Hkj Hthreshold).
      }
      assert (Hnpk : ~ msat w k p).
      {
        intro Hpk.
        apply Hnoevent_k.
        unfold rhatge_event.
        right. split; assumption.
      }
      apply policy_RhatGe_preserve with (path:=path); intros; auto; lia.
Qed.

(* ====================================================================== *)
(* 10. Strict-comparator COMPLETENESS lemmas                             *)
(* ====================================================================== *)

Lemma complete_UhatLt :
  forall root w path d p q,
    node_at root path = Some (MUhatLt d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MUhatLt d p q) ->
      lsat (canonical_ext root w) i (T_at path (MUhatLt d p q)).
Proof.
  intros root w path d p q Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MUhatLt d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  destruct Hmtl as [j [Hij [Hbound [Hq Hp]]]].
  assert (HSj : (S i <= j)%nat) by lia.

  destruct
    (@first_occurrence_bounded
       (fun n => msat w n q) (S i) j HSj Hq)
    as [j0 [[HSj0 Hj0j] [Hq0 Hnoq]]].

  assert (Hbound0 : elapsed w i j0 < d).
  {
    unfold elapsed in *.
    pose proof (time_monotone w Hj0j).
    lra.
  }
  assert (Hp0 :
    forall k : nat, (S i <= k < j0)%nat -> msat w k p).
  {
    intros k Hk.
    apply Hp.
    lia.
  }

  assert (Hinv :
    forall n : nat,
      (S i <= n <= j0)%nat ->
      canonical_val root w ((MUhatLt d p q)) n + elapsed w n j0 < d).
  {
    intros n Hrange.
    induction n using (well_founded_induction lt_wf).
    destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
    - subst n.
      change
        ((if canonical_reset root w i ((MUhatLt d p q))
          then delta w i
          else canonical_val root w ((MUhatLt d p q)) i + delta w i)
         + elapsed w (S i) j0 < d).
      destruct (canonical_reset root w i ((MUhatLt d p q))) eqn:Hr.
      + unfold elapsed, delta in *.
        lra.
      + destruct (policy_UhatLt_preserve_inv Hnode Hr)
          as [_ [Hres Hnqi]].
        destruct Hres as [m [Him [Hresbound [Hqm _]]]].
        assert (HSim : (S i <= m)%nat).
        {
          destruct (Nat.eq_dec m i) as [-> | Hneq].
          - contradiction.
          - lia.
        }
        assert (Hj0m : (j0 <= m)%nat).
        {
          destruct (le_dec j0 m) as [Hle | Hnle].
          - exact Hle.
          - exfalso.
            apply (Hnoq m).
            + lia.
            + exact Hqm.
        }
        pose proof (time_monotone w Hj0m).
        unfold elapsed, delta in *.
        lra.
    - assert (Hpred : (S i <= pred n < n)%nat) by lia.
      assert (Hpred_lt : (pred n < n)%nat) by lia.
      assert (Hpred_range : (S i <= pred n <= j0)%nat) by lia.
      specialize (H (pred n) Hpred_lt Hpred_range).
      assert (Hpred_j0 : (pred n <= j0)%nat) by lia.
      assert (Hel_nonneg : 0 <= elapsed w (pred n) j0).
      { apply elapsed_nonnegative. exact Hpred_j0. }
      assert (Hval_le :
        canonical_val root w ((MUhatLt d p q)) (pred n) < d) by lra.
      assert (Hres :
        ult_residual w (pred n)
          (canonical_val root w ((MUhatLt d p q)) (pred n)) d p q).
      {
        unfold ult_residual.
        exists j0.
        repeat split.
        + lia.
        + exact H.
        + exact Hq0.
        + intros h Hh.
          apply Hp0.
          lia.
      }
      assert (Hnq : ~ msat w (pred n) q).
      { apply Hnoq. lia. }
      pose proof
        (policy_UhatLt_preserve Hnode Hval_le Hres Hnq) as Hunch.
      unfold canonical_reset in Hunch.
      replace n with (S (pred n)) by lia.
      simpl.
      unfold reset_policy in Hunch; simpl in Hunch.
      rewrite Hunch.
      unfold elapsed, delta in *.
      lra.
  }

  simpl.
  exists j0.
  repeat split.
  - exact HSj0.
  - unfold c_lt; simpl.
    assert (Hj0range : (S i <= j0 <= j0)%nat) by lia.
    specialize (Hinv j0 Hj0range).
    unfold elapsed in Hinv.
    lra.
  - apply (proj1 (IHq j0)). exact Hq0.
  - unfold c_lt; simpl.
      assert (Hkrange : (S i <= k <= j0)%nat) by lia.
      specialize (Hinv k Hkrange).
      assert (Hkj0 : (k <= j0)%nat) by lia.
      pose proof (@elapsed_nonnegative w k j0 Hkj0) as Hel_nonneg.
      lra.
  -  unfold unch_at, canonical_ext; simpl.
      assert (Hkrange : (S i <= k <= j0)%nat) by lia.
      specialize (Hinv k Hkrange).
      assert (Hel : 0 <= elapsed w k j0).
      { apply elapsed_nonnegative. lia. }
      assert (Hval_le : canonical_val root w ((MUhatLt d p q)) k < d) by lra.
      assert (Hres :
        ult_residual w k (canonical_val root w ((MUhatLt d p q)) k) d p q).
      {
        unfold ult_residual.
        exists j0.
        repeat split.
        * lia.
        * exact Hinv.
        * exact Hq0.
        * intros h Hh.
          apply Hp0.
          lia.
      }
      apply policy_UhatLt_preserve with (path:=path); simpl; intros; auto; lia.
   -  apply (proj1 (IHp k)).
      apply Hp0. lia.
Qed.

Lemma complete_UhatGt :
  forall root w path d p q,
    0 < d ->
    node_at root path = Some (MUhatGt d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MUhatGt d p q) ->
      lsat (canonical_ext root w) i (T_at path (MUhatGt d p q)).
Proof.
  intros root w path d p q Hd Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MUhatGt d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  pose proof Hmtl as HFi.
  destruct Hmtl as [j [Hij [Hfar [Hq Hp]]]].

  simpl.
  split.
  - unfold rst_at, canonical_ext; simpl.
     apply policy_UhatGt_reset with (root:=root) (path:=path); simpl; intros; auto; lia.
  - simpl.

    assert (Hactive :
      forall n : nat,
        (S i <= n)%nat ->
        (forall r : nat,
           (S i <= r < n)%nat ->
           ~ uhatgt_event root w path d p q r) ->
        exists s m,
          (i <= s < n)%nat /\
          msat w s (MUhatGt d p q) /\
          (n <= m)%nat /\
          d < elapsed w s m /\
          msat w m q /\
          (forall k : nat,
             (s < k < m)%nat -> msat w k p) /\
          canonical_val root w ((MUhatGt d p q)) n = elapsed w s n).
    {
      intro n.
      induction n using (well_founded_induction lt_wf).
      intros HSn Hnone.
      destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
      - subst n.
        exists i, j.
        repeat split; try assumption; try lia.
        pose proof (policy_UhatGt_reset Hnode HFi) as Hr.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        unfold elapsed, delta.
        reflexivity.
      - assert (Hpred_range : (S i <= pred n)%nat) by lia.
        assert (Hnone_pred :
          forall r : nat,
            (S i <= r < pred n)%nat ->
            ~ uhatgt_event root w path d p q r).
        {
          intros r Hr.
          apply Hnone.
          lia.
        }
        assert (Hpred_lt : (pred n < n)%nat) by lia.
        pose proof
          (H (pred n) Hpred_lt Hpred_range Hnone_pred)
          as Hprev.
        destruct Hprev
          as [s [m [[His Hsn]
               [HFs [Hkm [Hfar_m [Hqm [Hpm Hval]]]]]]]].

        assert (Hkm_strict : (pred n < m)%nat).
        {
          destruct (Nat.eq_dec (pred n) m) as [Heq | Hneq].
          - subst m.
            exfalso.
            apply (Hnone (pred n)); [lia |].
            unfold uhatgt_event.
            split.
            + rewrite Hval. exact Hfar_m.
            + exists (pred n).
              repeat split; try lia; try assumption.
          - lia.
        }

        destruct (classic (msat w (pred n) (MUhatGt d p q)))
          as [HFk | HnotFk].
        + pose proof HFk as HFk_full.
          simpl in HFk.
          destruct HFk as [m2 [Hkm2 [Hfar2 [Hq2 Hp2]]]].
          exists (pred n), m2.
          repeat split; try assumption; try lia.
          pose proof (policy_UhatGt_reset Hnode HFk_full) as Hr.
          unfold canonical_reset in Hr.
          replace n with (S (pred n)) by lia.
          simpl.
          unfold reset_policy in Hr; simpl in Hr.
          rewrite Hr.
          unfold elapsed, delta.
          reflexivity.
        + exists s, m.
          repeat split; try assumption; try lia.
          assert (Hr : canonical_reset root w (pred n) ((MUhatGt d p q)) = false).
          {
            unfold canonical_reset, reset_policy; simpl.
            apply (proj2 (decide_false
              (msat w (pred n) (MUhatGt d p q)))).
            exact HnotFk.
          }
          unfold canonical_reset in Hr.
          replace n with (S (pred n)) by lia.
          simpl.
          unfold reset_policy in Hr; simpl in Hr.
          rewrite Hr, Hval.
          unfold elapsed, delta.
          lra.
    }

    destruct (classic
      (exists r : nat,
         (S i <= r)%nat /\
         uhatgt_event root w path d p q r))
      as [Hevent | Hnever].

    + destruct Hevent as [r [HSr Her]].
      destruct
        (@first_occurrence_bounded
           (uhatgt_event root w path d p q) (S i) r HSr Her)
        as [r0 [[HSr0 Hr0r] [Her0 Hfirst]]].
      left.
      exists r0.
      split; [exact HSr0 |].
      split.
      * unfold uhatgt_event in Her0.
        destruct Her0 as [Hge [m [Hrm [Hqm Hpm]]]].
        split.
        -- unfold c_gt; simpl. exact Hge.
        -- exists m.
           repeat split; try assumption.
           ++ apply (proj1 (IHq m)). exact Hqm.
           ++ intros k Hk.
              apply (proj1 (IHp k)).
              apply Hpm. exact Hk.
      * intros k Hk.
        apply (proj1 (IHp k)).
        assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ uhatgt_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst.
          lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        destruct (Hactive k HSk Hnone_k)
          as [s [m [[His Hsk]
               [HFs [Hkm [Hfar_m [Hqm [Hpm Hval]]]]]]]].
        assert (Hkm_strict : (k < m)%nat).
        {
          destruct (Nat.eq_dec k m) as [Heq | Hneq].
          - subst m.
            exfalso.
            apply (Hfirst k); [lia |].
            unfold uhatgt_event.
            split.
            + rewrite Hval. exact Hfar_m.
            + exists k.
              repeat split; try lia; try assumption.
          - lia.
        }
        apply Hpm.
        lia.

    + right.
      split.
      * change (lsat (canonical_ext root w) (S i)
          (LG (T_at (left_path path) p))).
        apply (proj2 (@LG_semantics _ (canonical_ext root w) (S i)
          (T_at (left_path path) p))).
        intros n HSn.
        apply (proj1 (IHp n)).
        assert (Hnone_n :
          forall t : nat,
            (S i <= t < n)%nat ->
            ~ uhatgt_event root w path d p q t).
        {
          intros t Ht Het.
          apply Hnever.
          exists t.
          split; [lia | exact Het].
        }
        destruct (Hactive n HSn Hnone_n)
          as [s [m [[His Hsn]
               [HFs [Hnm [Hfar_m [Hqm [Hpm Hval]]]]]]]].
        assert (Hnm_strict : (n < m)%nat).
        {
          destruct (Nat.eq_dec n m) as [Heq | Hneq].
          - subst m.
            exfalso.
            apply Hnever.
            exists n.
            split; [exact HSn |].
            unfold uhatgt_event.
            split.
            + rewrite Hval. exact Hfar_m.
            + exists n.
              repeat split; try lia; try assumption.
          - lia.
        }
        apply Hpm.
        lia.
      * change (lsat (canonical_ext root w) (S i)
          (LGF (T_at (right_path path) q))).
        apply (proj2 (@LGF_semantics _ (canonical_ext root w) (S i)
          (T_at (right_path path) q))).
        intros n HSn.
        assert (Hnone_n :
          forall t : nat,
            (S i <= t < n)%nat ->
            ~ uhatgt_event root w path d p q t).
        {
          intros t Ht Het.
          apply Hnever.
          exists t.
          split; [lia | exact Het].
        }
        destruct (Hactive n HSn Hnone_n)
          as [s [m [[His Hsn]
               [HFs [Hnm [Hfar_m [Hqm [Hpm Hval]]]]]]]].
        exists m.
        split; [exact Hnm |].
        apply (proj1 (IHq m)). exact Hqm.
Qed.

Lemma complete_RhatLt :
  forall root w path d p q,
    node_at root path = Some (MRhatLt d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MRhatLt d p q) ->
      lsat (canonical_ext root w) i (T_at path (MRhatLt d p q)).
Proof.
  intros root w path d p q Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MRhatLt d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  pose proof Hmtl as HFi.
  simpl.
  split.
  - unfold rst_at, canonical_ext.
    apply policy_RhatLt_reset with (path:=path); simpl; intros; auto; lia.
  - change (lsat (canonical_ext root w) (S i)
      (LW
        (T_at (right_path path) q)
        (LOr (LAtom (LCGe (exist _ (MRhatLt d p q) Hmem) d))
             (LAnd (T_at (left_path path) p)
                   (T_at (right_path path) q))))).
    apply (proj2 (@LW_semantics _ (canonical_ext root w) (S i)
      (T_at (right_path path) q)
      (LOr (LAtom (LCGe (exist _ (MRhatLt d p q) Hmem) d))
           (LAnd (T_at (left_path path) p)
                 (T_at (right_path path) q))))).

    assert (Hactive :
      forall n : nat,
        (S i <= n)%nat ->
        (forall r : nat,
           (S i <= r < n)%nat ->
           ~ rhatlt_event root w path d p q r) ->
        exists s,
          (i <= s < n)%nat /\
          msat w s (MRhatLt d p q) /\
          canonical_val root w ((MRhatLt d p q)) n = elapsed w s n /\
          (forall h : nat,
             (s < h < n)%nat -> ~ msat w h p)).
    {
      intro n.
      induction n using (well_founded_induction lt_wf).
      intros HSn Hnone.
      destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
      - subst n.
        exists i.
        repeat split; try assumption; try lia.
        pose proof (policy_RhatLt_reset Hnode HFi) as Hr.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        unfold elapsed, delta.
        reflexivity.
      - assert (Hpred_range : (S i <= pred n)%nat) by lia.
        assert (Hnone_pred :
          forall r : nat,
            (S i <= r < pred n)%nat ->
            ~ rhatlt_event root w path d p q r).
        {
          intros r Hr.
          apply Hnone.
          lia.
        }
        assert (Hpred_lt : (pred n < n)%nat) by lia.
        pose proof
          (H (pred n) Hpred_lt Hpred_range Hnone_pred)
          as Hprev.
        destruct Hprev as [s [[His Hsk] [HFs [Hval Hnop]]]].
        assert (Hnoevent_k :
          ~ rhatlt_event root w path d p q (pred n)).
        { apply Hnone. lia. }
        assert (Hlt : canonical_val root w ((MRhatLt d p q)) (pred n) < d).
        {
          destruct (Rlt_dec' (canonical_val root w ((MRhatLt d p q)) (pred n)) d)
            as [Hlt | Hnlt].
          - exact Hlt.
          - exfalso.
            apply Hnoevent_k.
            unfold rhatlt_event.
            left. lra.
        }
        assert (Hqk : msat w (pred n) q).
        {
          pose proof HFs as Hrel.
          simpl in Hrel.
          assert (Hs_pred : (s < pred n)%nat) by lia.
          specialize (Hrel (pred n) Hs_pred).
          assert (Hphys : tw_time w (pred n) - tw_time w s < d).
          {
            rewrite Hval in Hlt.
            unfold elapsed in Hlt.
            exact Hlt.
          }
          specialize (Hrel Hphys).
          destruct Hrel as [Hqk | [h [Hh Hph]]].
          - exact Hqk.
          - exfalso.
            apply (Hnop h); [exact Hh | exact Hph].
        }
        assert (Hnpk : ~ msat w (pred n) p).
        {
          intro Hpk.
          apply Hnoevent_k.
          unfold rhatlt_event.
          right.
          split; assumption.
        }

        destruct (classic (msat w (pred n) (MRhatLt d p q)))
          as [HFk | HnotFk].
        + exists (pred n).
          repeat split; try assumption; try lia.
          pose proof (policy_RhatLt_reset Hnode HFk) as Hr.
          unfold canonical_reset in Hr.
          replace n with (S (pred n)) by lia.
          simpl.
          unfold reset_policy in Hr; simpl in Hr.
          rewrite Hr.
          unfold elapsed, delta.
          ring.
        + exists s.
          repeat split; try assumption; try lia.
          * assert (Hr : canonical_reset root w (pred n) ((MRhatLt d p q)) = false).
            {
              unfold canonical_reset, reset_policy.
              apply (proj2 (decide_false
                (msat w (pred n) (MRhatLt d p q)))).
              exact HnotFk.
            }
            unfold canonical_reset in Hr.
            replace n with (S (pred n)) by lia.
            simpl.
            unfold reset_policy in Hr; simpl in Hr.
            rewrite Hr, Hval.
            unfold elapsed, delta.
            lra.
          * intros h Hh.
            destruct (Nat.eq_dec h (pred n)) as [-> | Hneq].
            -- exact Hnpk.
            -- apply Hnop. lia.
    }

    destruct (classic
      (exists r : nat,
         (S i <= r)%nat /\
         rhatlt_event root w path d p q r))
      as [Hevent | Hnever].

    + destruct Hevent as [r [HSr Her]].
      destruct
        (@first_occurrence_bounded
           (rhatlt_event root w path d p q) (S i) r HSr Her)
        as [r0 [[HSr0 Hr0r] [Her0 Hfirst]]].
      left.
      exists r0.
      split; [exact HSr0 |].
      split.
      * unfold rhatlt_event in Her0.
        destruct Her0 as [Hgt | [Hp_r Hq_r]].
        -- left. unfold c_ge; simpl. exact Hgt.
        -- right. split.
           ++ apply (proj1 (IHp r0)). exact Hp_r.
           ++ apply (proj1 (IHq r0)). exact Hq_r.
      * intros k Hk.
        apply (proj1 (IHq k)).
        assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ rhatlt_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst.
          lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        destruct (Hactive k HSk Hnone_k)
          as [s [[His Hsk] [HFs [Hval Hnop]]]].
        assert (Hnoevent_k :
          ~ rhatlt_event root w path d p q k).
        { apply Hfirst. lia. }
        assert (Hlt : canonical_val root w ((MRhatLt d p q)) k < d).
        {
          destruct (Rlt_dec' (canonical_val root w ((MRhatLt d p q)) k) d)
            as [Hlt | Hnlt].
          - exact Hlt.
          - exfalso.
            apply Hnoevent_k.
            unfold rhatlt_event.
            left. lra.
        }
        pose proof HFs as Hrel.
        simpl in Hrel.
        assert (Hsk_le : (s < k)%nat) by lia.
        specialize (Hrel k Hsk_le).
        assert (Hphys : tw_time w k - tw_time w s < d).
        {
          rewrite Hval in Hlt.
          unfold elapsed in Hlt.
          exact Hlt.
        }
        specialize (Hrel Hphys).
        destruct Hrel as [Hqk | [h [Hh Hph]]].
        -- exact Hqk.
        -- exfalso. apply (Hnop h); assumption.

    + right.
      intros k HSk.
      apply (proj1 (IHq k)).
      assert (Hnone_k :
        forall t : nat,
          (S i <= t < k)%nat ->
          ~ rhatlt_event root w path d p q t).
      {
        intros t Ht Het.
        apply Hnever.
        exists t.
        split; [lia | exact Het].
      }
      destruct (Hactive k HSk Hnone_k)
        as [s [[His Hsk] [HFs [Hval Hnop]]]].
      assert (Hnoevent_k :
        ~ rhatlt_event root w path d p q k).
      {
        intro Het.
        apply Hnever.
        exists k.
        split; assumption.
      }
      assert (Hlt : canonical_val root w ((MRhatLt d p q)) k < d).
      {
        destruct (Rlt_dec' (canonical_val root w ((MRhatLt d p q)) k) d)
          as [Hlt | Hnlt].
        - exact Hlt.
        - exfalso.
          apply Hnoevent_k.
          unfold rhatlt_event.
          left. lra.
      }
      pose proof HFs as Hrel.
      simpl in Hrel.
      assert (Hsk_le : (s < k)%nat) by lia.
      specialize (Hrel k Hsk_le).
      assert (Hphys : tw_time w k - tw_time w s < d).
      {
        rewrite Hval in Hlt.
        unfold elapsed in Hlt.
        exact Hlt.
      }
      specialize (Hrel Hphys).
      destruct Hrel as [Hqk | [h [Hh Hph]]].
      * exact Hqk.
      * exfalso. apply (Hnop h); assumption.
Qed.

Lemma rgt_residual_shift :
  forall w i v d p q,
    rgt_residual w i v d p q ->
    ~ msat w i p ->
    rgt_residual w (S i) (v + delta w i) d p q.
Proof.
  intros w i v d p q Hres Hnp.
  unfold rgt_residual in *.
  intros j Hsj Hthreshold.
  assert (Hij : (i <= j)%nat) by lia.
  assert (Hthreshold' : d < v + elapsed w i j).
  { unfold elapsed, delta in *; lra. }
  destruct (Hres j Hij Hthreshold') as [Hq | [k [Hk Hp]]].
  - left. exact Hq.
  - destruct (Nat.eq_dec k i) as [-> | Hneq].
    + contradiction.
    + right.
      exists k.
      split; [lia | exact Hp].
Qed.

Lemma complete_RhatGt :
  forall root w path d p q,
    0 < d ->
    node_at root path = Some (MRhatGt d p q) ->
    (forall n,
       msat w n p <->
       lsat (canonical_ext root w) n (T_at (left_path path) p)) ->
    (forall n,
       msat w n q <->
       lsat (canonical_ext root w) n (T_at (right_path path) q)) ->
    forall i,
      msat w i (MRhatGt d p q) ->
      lsat (canonical_ext root w) i (T_at path (MRhatGt d p q)).
Proof.
  intros root w path d p q Hd Hnode IHp IHq i Hmtl.
  assert (Hmem : In (MRhatGt d p q) (timed_subformulas root))
    by (apply (node_at_timed_incl Hnode); simpl; left; reflexivity).
  cbn [T_at]; rewrite (clock_of_mem Hmem).
  simpl.
  change (lsat (canonical_ext root w) (S i)
    (LW
      (LAnd (LAtom (LCLe (exist _ (MRhatGt d p q) Hmem) d)) (LAtom (LUnch (exist _ (MRhatGt d p q) Hmem))))
      (LOr
        (LRelease (T_at (left_path path) p)
                  (T_at (right_path path) q))
        (LAnd (LAtom (LCLe (exist _ (MRhatGt d p q) Hmem) d))
              (T_at (left_path path) p))))).
  apply (proj2 (@LW_semantics _ (canonical_ext root w) (S i)
    (LAnd (LAtom (LCLe (exist _ (MRhatGt d p q) Hmem) d)) (LAtom (LUnch (exist _ (MRhatGt d p q) Hmem))))
    (LOr
      (LRelease (T_at (left_path path) p)
                (T_at (right_path path) q))
      (LAnd (LAtom (LCLe (exist _ (MRhatGt d p q) Hmem) d))
            (T_at (left_path path) p))))).

  assert (Hactive :
    forall n : nat,
      (S i <= n)%nat ->
      (forall r : nat,
         (S i <= r < n)%nat ->
         ~ rhatgt_event root w path d p q r) ->
      rgt_residual w n (canonical_val root w ((MRhatGt d p q)) n) d p q).
  {
    intro n.
    induction n using (well_founded_induction lt_wf).
    intros HSn Hnone.
    destruct (Nat.eq_dec n (S i)) as [Hbase | Hstep].
    - subst n.
      destruct (canonical_reset root w i ((MRhatGt d p q))) eqn:Hr.
      + pose proof (MRhatGt_after_step_residual Hd Hmtl) as Hshift.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        exact Hshift.
      + destruct (policy_RhatGt_preserve_inv Hnode Hr)
          as [_ [Hres Hnp]].
        pose proof (rgt_residual_shift Hres Hnp) as Hshift.
        unfold canonical_reset in Hr.
        simpl.
        unfold reset_policy in Hr; simpl in Hr.
        rewrite Hr.
        exact Hshift.
    - assert (Hpred_range : (S i <= pred n)%nat) by lia.
      assert (Hnone_pred :
        forall r : nat,
          (S i <= r < pred n)%nat ->
          ~ rhatgt_event root w path d p q r).
      {
        intros r Hr.
        apply Hnone.
        lia.
      }
      assert (Hpred_lt : (pred n < n)%nat) by lia.
      pose proof
        (H (pred n) Hpred_lt Hpred_range Hnone_pred)
        as Hres.
      assert (Hnoevent_k :
        ~ rhatgt_event root w path d p q (pred n)).
      { apply Hnone. lia. }
      assert (Hlt : canonical_val root w ((MRhatGt d p q)) (pred n) <= d).
      {
        destruct (Rle_dec' (canonical_val root w ((MRhatGt d p q)) (pred n)) d)
          as [Hlt | Hnlt].
        - exact Hlt.
        - exfalso.
          apply Hnoevent_k.
          unfold rhatgt_event.
          left.
          simpl.
          intros j Hkj.
          assert (Hel : 0 <= elapsed w (pred n) j).
          { apply elapsed_nonnegative. exact Hkj. }
          assert (Hthreshold :
            d < canonical_val root w ((MRhatGt d p q)) (pred n) +
                 elapsed w (pred n) j) by lra.
          exact (Hres j Hkj Hthreshold).
      }
      assert (Hnpk : ~ msat w (pred n) p).
      {
        intro Hpk.
        apply Hnoevent_k.
        unfold rhatgt_event.
        right. split; assumption.
      }
      pose proof
        (policy_RhatGt_preserve Hnode Hlt Hres Hnpk) as Hunch.
      pose proof (rgt_residual_shift Hres Hnpk) as Hshift.
      unfold canonical_reset in Hunch.
      replace n with (S (pred n)) by lia.
      simpl.
      unfold reset_policy in Hunch; simpl in Hunch.
      rewrite Hunch.
      exact Hshift.
  }

  destruct (classic
    (exists r : nat,
       (S i <= r)%nat /\
       rhatgt_event root w path d p q r))
    as [Hevent | Hnever].

  - destruct Hevent as [r [HSr Her]].
    destruct
      (@first_occurrence_bounded
         (rhatgt_event root w path d p q) (S i) r HSr Her)
      as [r0 [[HSr0 Hr0r] [Her0 Hfirst]]].
    left.
    exists r0.
    split; [exact HSr0 |].
    split.
    + unfold rhatgt_event in Her0.
      destruct Her0 as [HR | [Hlt Hp_r]].
      * left.
        simpl in HR |- *.
        intros j Hrj.
        specialize (HR j Hrj).
        destruct HR as [Hqj | [h [Hh Hph]]].
        -- left. apply (proj1 (IHq j)). exact Hqj.
        -- right. exists h. split; [exact Hh |].
           apply (proj1 (IHp h)). exact Hph.
      * right. split.
        -- unfold c_le; simpl. exact Hlt.
        -- apply (proj1 (IHp r0)). exact Hp_r.
    + intros k Hk.
      split.
      * assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ rhatgt_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst. lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        pose proof (Hactive k HSk Hnone_k) as Hres.
        assert (Hnoevent_k :
          ~ rhatgt_event root w path d p q k).
        { apply Hfirst. lia. }
        unfold c_le; simpl.
        destruct (Rle_dec' (canonical_val root w ((MRhatGt d p q)) k) d)
          as [Hlt | Hnlt].
        -- exact Hlt.
        -- exfalso.
           apply Hnoevent_k.
           unfold rhatgt_event.
           left.
           simpl.
           intros j Hkj.
           assert (Hel : 0 <= elapsed w k j).
           { apply elapsed_nonnegative. exact Hkj. }
           assert (Hthreshold :
             d < canonical_val root w ((MRhatGt d p q)) k + elapsed w k j)
             by lra.
           exact (Hres j Hkj Hthreshold).
      * unfold unch_at, canonical_ext; simpl.
        assert (Hnone_k :
          forall t : nat,
            (S i <= t < k)%nat ->
            ~ rhatgt_event root w path d p q t).
        {
          intros t Ht.
          apply Hfirst. lia.
        }
        assert (HSk : (S i <= k)%nat) by lia.
        pose proof (Hactive k HSk Hnone_k) as Hres.
        assert (Hnoevent_k :
          ~ rhatgt_event root w path d p q k).
        { apply Hfirst. lia. }
        assert (Hlt : canonical_val root w ((MRhatGt d p q)) k <= d).
        {
          destruct (Rle_dec' (canonical_val root w ((MRhatGt d p q)) k) d)
            as [Hlt | Hnlt].
          - exact Hlt.
          - exfalso.
            apply Hnoevent_k.
            unfold rhatgt_event.
            left.
            simpl.
            intros j Hkj.
            assert (Hel : 0 <= elapsed w k j).
            { apply elapsed_nonnegative. exact Hkj. }
            assert (Hthreshold :
              d < canonical_val root w ((MRhatGt d p q)) k + elapsed w k j)
              by lra.
            exact (Hres j Hkj Hthreshold).
        }
        assert (Hnpk : ~ msat w k p).
        {
          intro Hpk.
          apply Hnoevent_k.
          unfold rhatgt_event.
          right. split; assumption.
        }
        apply policy_RhatGt_preserve with (path:=path); intros; auto; lia.

  - right.
    intros k HSk.
    split.
    + assert (Hnone_k :
        forall t : nat,
          (S i <= t < k)%nat ->
          ~ rhatgt_event root w path d p q t).
      {
        intros t Ht Het.
        apply Hnever.
        exists t. split; [lia | exact Het].
      }
      pose proof (Hactive k HSk Hnone_k) as Hres.
      assert (Hnoevent_k : ~ rhatgt_event root w path d p q k).
      {
        intro Het.
        apply Hnever.
        exists k. split; assumption.
      }
      unfold c_le; simpl.
      destruct (Rle_dec' (canonical_val root w ((MRhatGt d p q)) k) d)
        as [Hlt | Hnlt].
      * exact Hlt.
      * exfalso.
        apply Hnoevent_k.
        unfold rhatgt_event.
        left.
        simpl.
        intros j Hkj.
        assert (Hel : 0 <= elapsed w k j).
        { apply elapsed_nonnegative. exact Hkj. }
        assert (Hthreshold :
          d < canonical_val root w ((MRhatGt d p q)) k + elapsed w k j)
          by lra.
        exact (Hres j Hkj Hthreshold).
    + unfold unch_at, canonical_ext; simpl.
      assert (Hnone_k :
        forall t : nat,
          (S i <= t < k)%nat ->
          ~ rhatgt_event root w path d p q t).
      {
        intros t Ht Het.
        apply Hnever.
        exists t. split; [lia | exact Het].
      }
      pose proof (Hactive k HSk Hnone_k) as Hres.
      assert (Hnoevent_k : ~ rhatgt_event root w path d p q k).
      {
        intro Het.
        apply Hnever.
        exists k. split; assumption.
      }
      assert (Hlt : canonical_val root w ((MRhatGt d p q)) k <= d).
      {
        destruct (Rle_dec' (canonical_val root w ((MRhatGt d p q)) k) d)
          as [Hlt | Hnlt].
        - exact Hlt.
        - exfalso.
          apply Hnoevent_k.
          unfold rhatgt_event.
          left.
          simpl.
          intros j Hkj.
          assert (Hel : 0 <= elapsed w k j).
          { apply elapsed_nonnegative. exact Hkj. }
          assert (Hthreshold :
            d < canonical_val root w ((MRhatGt d p q)) k + elapsed w k j)
            by lra.
          exact (Hres j Hkj Hthreshold).
      }
      assert (Hnpk : ~ msat w k p).
      {
        intro Hpk.
        apply Hnoevent_k.
        unfold rhatgt_event.
        right. split; assumption.
      }
      apply policy_RhatGt_preserve with (path:=path); intros; auto; lia.
Qed.



(* ====================================================================== *)
(* 10. Canonical encoding correctness at every occurrence                 *)
(* ====================================================================== *)

Theorem canonical_encoding_all :
  forall root w,
    well_formed root ->
    forall path f,
      node_at root path = Some f ->
      forall i,
        msat w i f <->
        lsat (canonical_ext root w) i (T_at path f).
Proof.
  intros root w Hwf.
  (* Structural recursion is on the formula [f], not on the proof [Hnode].
     Recursive calls are made on immediate subformulas (f1/f2/etc.), whereas
     the corresponding node_at proofs HL/HR are derived facts rather than
     structural subterms of Hnode. *)
  fix IH 2.
  intros path f Hnode i.
  destruct f; simpl in *.

  - tauto.
  - tauto.
  - tauto.
  - tauto.

  - (* And *)
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto. }
    specialize (IH (left_path path) f1 HL i) as IHl.
    specialize (IH (right_path path) f2 HR i) as IHr.
    tauto.

  - (* Or *)
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto. }
    specialize (IH (left_path path) f1 HL i) as IHl.
    specialize (IH (right_path path) f2 HR i) as IHr.
    tauto.

  - (* Next *)
    assert (HC :
      node_at root (left_path path) = Some f).
    { eapply node_at_next; eauto. }
    apply IH with (path:=left_path path) (f:=f) (i:=S i) in HC.
    exact HC.

  - (* ordinary Until *)
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto. }
    split.
    + intros [j [Hij [Hq Hp]]].
      exists j. repeat split; try assumption.
      * apply (proj1 (IH (right_path path) f2 HR j)).
        exact Hq.
      * intros k Hk.
        apply (proj1 (IH (left_path path) f1 HL k)).
        apply Hp. exact Hk.
    + intros [j [Hij [Hq Hp]]].
      exists j. repeat split; try assumption.
      * apply (proj2 (IH (right_path path) f2 HR j)).
        exact Hq.
      * intros k Hk.
        apply (proj2 (IH (left_path path) f1 HL k)).
        apply Hp. exact Hk.

  - (* ordinary Release *)
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto. }
    split.
    + intros H j Hij.
      specialize (H j Hij).
      destruct H as [Hq | [k [Hk Hp]]].
      * left.
        apply (proj1 (IH (right_path path) f2 HR j)).
        exact Hq.
      * right. exists k. split; [exact Hk|].
        apply (proj1 (IH (left_path path) f1 HL k)).
        exact Hp.
    + intros H j Hij.
      specialize (H j Hij).
      destruct H as [Hq | [k [Hk Hp]]].
      * left.
        apply (proj2 (IH (right_path path) f2 HR j)).
        exact Hq.
      * right. exists k. split; [exact Hk|].
        apply (proj2 (IH (left_path path) f1 HL k)).
        exact Hp.

  - (* Uhat <= *)
    assert (Hlocal : well_formed (MUhatLe r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 4 right. left; exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 4 right. left. exists r. reflexivity. }
    split.
    + apply complete_UhatLe with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_UhatLe with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try exact HT.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

  - (* Uhat >= *)
    assert (Hlocal : well_formed (MUhatGe r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 5 right. left. exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 5 right. left. exists r. reflexivity. }
    split.
    + apply complete_UhatGe with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_UhatGe with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try eassumption.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

  - (* Rhat <= *)
    assert (Hlocal : well_formed (MRhatLe r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 6 right. left. exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 6 right. left. exists r. reflexivity. }
    split.
    + apply complete_RhatLe with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_RhatLe with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try exact HT.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

  - (* Rhat >= *)
    assert (Hlocal : well_formed (MRhatGe r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 7 right. left. exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 7 right. left. exists r. reflexivity. }
    split.
    + apply complete_RhatGe with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_RhatGe with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try eassumption.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

  - (* Uhat < *)
    assert (Hlocal : well_formed (MUhatLt r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 8 right. left. exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 8 right. left. exists r. reflexivity. }
    split.
    + apply complete_UhatLt with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_UhatLt with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try exact HT.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

  - (* Uhat > *)
    assert (Hlocal : well_formed (MUhatGt r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 9 right. left. exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 9 right. left. exists r. reflexivity. }
    split.
    + apply complete_UhatGt with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_UhatGt with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try eassumption.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

  - (* Rhat < *)
    assert (Hlocal : well_formed (MRhatLt r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 10 right. left. exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 10 right. left. exists r. reflexivity. }
    split.
    + apply complete_RhatLt with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_RhatLt with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try exact HT.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

  - (* Rhat > *)
    assert (Hlocal : well_formed (MRhatGt r f1 f2)).
    { eapply well_formed_node_at; eauto. }
    simpl in Hlocal.
    destruct Hlocal as [Hd [Hw1 Hw2]].
    assert (HL :
      node_at root (left_path path) = Some f1).
    { eapply node_at_left_binary; eauto.
      do 11 right. exists r. reflexivity. }
    assert (HR :
      node_at root (right_path path) = Some f2).
    { eapply node_at_right_binary; eauto.
      do 11 right. exists r. reflexivity. }
    split.
    + apply complete_RhatGt with
        (root:=root) (w:=w) (path:=path)
        (d:=r) (p:=f1) (q:=f2);
        try assumption.
      * intro n. apply IH; assumption.
      * intro n. apply IH; assumption.
    + intro HT.
      eapply sound_RhatGt with
        (rho:=canonical_ext root w)
        (path:=path) (d:=r) (p:=f1) (q:=f2);
        try eassumption.
      * apply canonical_clock_consistent.
      * intros n Hn.
        apply (proj2 (IH (left_path path) f1 HL n)).
        exact Hn.
      * intros n Hn.
        apply (proj2 (IH (right_path path) f2 HR n)).
        exact Hn.

Qed.

(* ====================================================================== *)
(* 11. EncodingCorrect                                                   *)
(* ====================================================================== *)

Theorem EncodingCorrect_proved :
  EncodingCorrect.
Proof.
  unfold EncodingCorrect.
  intros f w Hwf.
  split.

  - intro Hmtl.
    exists (canonical_ext f w).
    split.
    + apply canonical_same_base.
    + split.
      * apply canonical_clock_consistent.
      * unfold T.
         specialize (node_at_nil f) as Hn.
         apply  (canonical_encoding_all w Hwf Hn 0).
        exact Hmtl.

  - intros [rho [Hbase [Hclock Hltl]]].
    assert (Hsound :
      forall f path i,
        well_formed f ->
        lsat rho i (T_at path f) ->
        msat (ew_base rho) i f).
    {
      fix SIH 1.
      intros g path i Hwg Hg.
      destruct g; simpl in *.
      - exact I.
      - contradiction.
      - exact Hg.
      - exact Hg.
      - destruct Hwg as [Hw1 Hw2].
        destruct Hg as [H1 H2].
        split.
        + eapply SIH; eauto.
        + eapply SIH; eauto.
      - destruct Hwg as [Hw1 Hw2].
        destruct Hg as [H1 | H2].
        + left. eapply SIH; eauto.
        + right. eapply SIH; eauto.
      - eapply SIH; eauto.
      - destruct Hwg as [Hw1 Hw2].
        destruct Hg as [j [Hij [Hq Hp]]].
        exists j. repeat split; try assumption.
        + eapply SIH; eauto.
        + intros k Hk. eapply SIH; eauto.
      - destruct Hwg as [Hw1 Hw2].
        intros j Hij.
        specialize (Hg j Hij).
        destruct Hg as [Hq | [k [Hk Hp]]].
        + left. eapply SIH; eauto.
        + right. exists k. split; [exact Hk |].
          eapply SIH; eauto.
      - (* Uhat <= *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_UhatLe with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.
      - (* Uhat >= *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_UhatGe with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.
      - (* Rhat <= *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_RhatLe with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.
      - (* Rhat >= *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_RhatGe with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.

      - (* Uhat < *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_UhatLt with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.
      - (* Uhat > *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_UhatGt with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.
      - (* Rhat < *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_RhatLt with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.
      - (* Rhat > *)
        destruct Hwg as [Hd [Hw1 Hw2]].
        eapply sound_RhatGt with
          (rho:=rho) (path:=path)
          (d:=r) (p:=g1) (q:=g2);
          try eassumption.
        + intros n Hn. eapply SIH; eauto.
        + intros n Hn. eapply SIH; eauto.
    }
    unfold T in Hltl.
    rewrite <- Hbase.
    apply (Hsound f []); auto.
Qed.

(* ====================================================================== *)
(* 12. End-to-end theorem with EncodingCorrect no longer assumed         *)
(* ====================================================================== *)

Theorem MTL_to_TBA_correct :
  forall (f : mtl) (w : timed_word),
    well_formed f ->
    (msat w 0 f <-> TBA_accepts (compile f) w).
Proof.
  intros f w Hwf.
  eapply MTL_to_TBA_correct_from_encoding.
  - exact EncodingCorrect_proved.
  - exact Hwf.
Qed.
Print Assumptions MTL_to_TBA_correct.

(* The same result for the Buchi automaton returned by any correct
   LTL-to-Buchi translator, e.g. Spot called by the tool: no project axiom
   is used, the correctness of the translator is a hypothesis. *)
Theorem MTL_to_TBA_correct_with :
  forall (f : mtl) (A : PBuchi f) (w : timed_word),
    (forall s : pword f, PBA_accepts A s <-> psat s 0 (T f)) ->
    well_formed f ->
    (msat w 0 f <-> TBA_accepts (compile_with A) w).
Proof.
  intros f A w HA Hwf.
  apply compile_with_correct_from_encoding;
    [exact EncodingCorrect_proved | exact HA | exact Hwf].
Qed.
Print Assumptions MTL_to_TBA_correct_with.
(*
  Intended audit after compilation:

      Print Assumptions EncodingCorrect_proved.
      Print Assumptions MTL_to_TBA_correct.

  EncodingCorrect_proved should have no project-specific assumptions.
  MTL_to_TBA_correct should depend only on LTL_TO_BUCHI_CORRECT
  (plus any standard logical principles imported from Coq.Classical).
*)
