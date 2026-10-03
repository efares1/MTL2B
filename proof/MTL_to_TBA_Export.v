(*
  MTL_to_TBA_Export.v

  From the optimized timed Buchi automaton to the exported automaton:

  1. The exported model: guards in disjunctive normal form over clock
     constraints and clock-difference constraints (x - y <= c); location
     invariants that are disjunctions of conjunctions of upper bounds.  Its
     semantics is given on the same trace model as before.

  2. Transitions with the same source, target, event, and resets are merged:
     their guards are joined by disjunction.  The synthesized invariant of a
     location then becomes disjunctive: one disjunct per conjunct of an
     outgoing guard (its upper bounds).

  3. Elimination of disjunctive invariants, following the paper: each
     location l with invariant I_1 \/ ... \/ I_p is split into copies l_k
     with the conjunctive invariant I_k.  Every transition entering l is
     replicated toward every copy l_k and its guard is strengthened with
     G_k[Z := 0], where G_k requires I_k and that no other copy admits a
     strictly longer delay; outgoing transitions are replicated from every
     copy.  A dedicated copy without invariant is used for the initial
     location.

  4. Disjunctive guards are split into one transition per disjunct.

  The final automaton has conjunctive guards and conjunctive upper-bound
  invariants, and accepts exactly the models of the formula.  No new
  axiom: the only project axiom remains LTL_TO_BUCHI_CORRECT.
*)

From Stdlib Require Import Arith Lia List Bool Reals Lra.
From Stdlib Require Import Classical ClassicalDescription.
Require Import MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.
Require Import EncodingCorrect_Shared_Clock_Derived_Strict_Direct_Proof.
Require Import MTL_to_TBA_Invariants.
Require Import MTL_to_TBA_Optimizations.
Import ListNotations.
Open Scope R_scope.

Set Implicit Arguments.
Unset Strict Implicit.

Section Export.

Variable root : mtl.

(* ====================================================================== *)
(* 1. The exported model                                                  *)
(* ====================================================================== *)

Definition valuation := Clock root -> R.

(* A clock constraint, or a clock-difference constraint x - y <= c. *)
Inductive dconstraint : Type :=
| DSingle : clock_constraint root -> dconstraint
| DDiff : Clock root -> Clock root -> R -> dconstraint.

Definition single_holds (v : valuation) (k : clock_constraint root) : Prop :=
  match guard_comparison k with
  | CLe => v (guard_clock k) <= guard_bound k
  | CLt => v (guard_clock k) < guard_bound k
  | CGe => guard_bound k <= v (guard_clock k)
  | CGt => guard_bound k < v (guard_clock k)
  | CEq => v (guard_clock k) = guard_bound k
  end.

Definition dc_holds (v : valuation) (c : dconstraint) : Prop :=
  match c with
  | DSingle k => single_holds v k
  | DDiff x y b => v x - v y <= b
  end.

Definition dconj := list dconstraint.

Definition conj_holds (v : valuation) (c : dconj) : Prop := Forall (dc_holds v) c.

(* Guards in disjunctive normal form. *)
Definition dguard := list dconj.

Definition dguard_holds (v : valuation) (g : dguard) : Prop :=
  exists c, In c g /\ conj_holds v c.

(* Conjunction of upper bounds x <= b. *)
Definition uinv := list (Clock root * R).

Definition uinv_holds (v : valuation) (U : uinv) : Prop :=
  Forall (fun p => v (fst p) <= snd p) U.

Record dtrans : Type := {
  dt_src : nat;
  dt_label : list alit;
  dt_guard : dguard;
  dt_resets : list (Clock root);
  dt_tgt : nat
}.

(* [dta_inv l = None]: no invariant; [Some Us]: the disjunction of [Us]. *)
Record DTA : Type := {
  dta_nstates : nat;
  dta_init : nat;
  dta_trans : list dtrans;
  dta_accepting : list nat;
  dta_inv : nat -> option (list uinv)
}.

Definition at_event (rho : ext_word root) (i : nat) : valuation :=
  fun x => ew_val rho i x.

Definition dtrans_enabled (rho : ext_word root) (i : nat) (t : dtrans) : Prop :=
  label_holds (dt_label t) (letter_of rho i) /\
  dguard_holds (at_event rho i) (dt_guard t) /\
  resets_match rho i (dt_resets t).

Definition dinv_holds (I : option (list uinv)) (v : valuation) : Prop :=
  match I with
  | None => True
  | Some Us => exists U, In U Us /\ uinv_holds v U
  end.

(* The invariant holds during the whole stay that ends at event i. *)
Definition dinv_during (rho : ext_word root) (i : nat) (I : option (list uinv))
    : Prop :=
  forall dl, 0 <= dl <= stay rho i ->
    dinv_holds I (fun x => ew_val rho i x - dl).

Definition DTA_ext_accepts (D : DTA) (rho : ext_word root) : Prop :=
  exists run : nat -> nat,
    run 0%nat = dta_init D /\
    (forall i,
       (run i < dta_nstates D)%nat /\
       dinv_during rho i (dta_inv D (run i)) /\
       exists t,
         In t (dta_trans D) /\
         dt_src t = run i /\
         dt_tgt t = run (S i) /\
         dtrans_enabled rho i t) /\
    (forall n,
       exists j,
         (n <= j)%nat /\ In (run j) (dta_accepting D)).

Definition DTA_accepts (D : DTA) (w : timed_word) : Prop :=
  exists rho : ext_word root,
    same_base rho w /\
    clock_consistent rho /\
    DTA_ext_accepts D rho.

Lemma dta_accepts_of_ext :
  forall (D E : DTA),
    (forall rho, DTA_ext_accepts E rho -> DTA_ext_accepts D rho) ->
    (forall rho, clock_consistent rho ->
                 DTA_ext_accepts D rho -> DTA_ext_accepts E rho) ->
    forall w, DTA_accepts D w <-> DTA_accepts E w.
Proof.
  intros D E Hs Hc w. unfold DTA_accepts. split.
  - intros [rho [Hb [Hcc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hcc|]. exact (Hc rho Hcc Ha).
  - intros [rho [Hb [Hcc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hcc|]. exact (Hs rho Ha).
Qed.

(* ====================================================================== *)
(* 2. Embedding of the timed Buchi automaton                              *)
(* ====================================================================== *)

Fixpoint singles (g : guard root) : dconj :=
  match g with
  | [] => []
  | Some k :: g' => DSingle k :: singles g'
  | None :: g' => singles g'
  end.

Lemma singles_holds :
  forall (rho : ext_word root) i g,
    Forall (guard_item_holds rho i) g <-> conj_holds (at_event rho i) (singles g).
Proof.
  intros rho i g. unfold conj_holds.
  induction g as [|[k|] g IH]; simpl.
  - split; intro; constructor.
  - rewrite !Forall_cons_iff, IH.
    unfold clock_constraint_holds, single_holds, at_event. simpl. tauto.
  - rewrite Forall_cons_iff, IH. simpl. tauto.
Qed.

Definition of_tba_trans (t : tba_transition root) : dtrans :=
  {| dt_src := bt_source t;
     dt_label := bt_label t;
     dt_guard := [singles (bt_guard t)];
     dt_resets := bt_resets t;
     dt_tgt := bt_target t |}.

Definition of_tba (A : TBA root) : DTA :=
  {| dta_nstates := tba_nstates A;
     dta_init := tba_init A;
     dta_trans := map of_tba_trans (tba_transitions A);
     dta_accepting := tba_accepting A;
     dta_inv := fun _ => None |}.

Lemma of_tba_enabled :
  forall (rho : ext_word root) i t,
    tba_transition_enabled rho i t <-> dtrans_enabled rho i (of_tba_trans t).
Proof.
  intros rho i t. unfold tba_transition_enabled, dtrans_enabled. simpl.
  unfold dguard_holds. split.
  - intros [Hl [Hg Hr]]. split; [exact Hl|]. split; [|exact Hr].
    exists (singles (bt_guard t)). split; [left; reflexivity|].
    apply singles_holds. exact Hg.
  - intros [Hl [[c [[<-|[]] Hc]] Hr]]. split; [exact Hl|]. split; [|exact Hr].
    apply singles_holds. exact Hc.
Qed.

Theorem of_tba_accepts :
  forall A w, TBA_accepts A w <-> DTA_accepts (of_tba A) w.
Proof.
  intros A w. unfold TBA_accepts, DTA_accepts. split.
  - intros [rho [Hb [Hcc [run [Hinit [Hsteps Hbuchi]]]]]].
    exists rho. split; [exact Hb|]. split; [exact Hcc|].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i. destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
    split; [exact Hbound|]. split; [intros dl _; exact I|].
    exists (of_tba_trans t). split; [apply in_map; exact Hin|].
    split; [exact Hsrc|]. split; [exact Hdst|].
    apply of_tba_enabled. exact Hen.
  - intros [rho [Hb [Hcc [run [Hinit [Hsteps Hbuchi]]]]]].
    exists rho. split; [exact Hb|]. split; [exact Hcc|].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i. destruct (Hsteps i) as [Hbound [_ [tt [Hin [Hsrc [Hdst Hen]]]]]].
    split; [exact Hbound|].
    simpl in Hin. apply in_map_iff in Hin. destruct Hin as [t [<- Hin]].
    exists t. split; [exact Hin|]. split; [exact Hsrc|]. split; [exact Hdst|].
    apply of_tba_enabled. exact Hen.
Qed.

(* ====================================================================== *)
(* 3. Merging transitions with the same source, target, event, resets     *)
(* ====================================================================== *)

Definition same_key (t u : dtrans) : Prop :=
  dt_src t = dt_src u /\ dt_tgt t = dt_tgt u /\
  dt_label t = dt_label u /\ dt_resets t = dt_resets u.

Lemma same_key_refl : forall t, same_key t t.
Proof. intro t. repeat split. Qed.

Lemma same_key_sym : forall t u, same_key t u -> same_key u t.
Proof. intros t u [H1 [H2 [H3 H4]]]. repeat split; congruence. Qed.

Lemma same_key_trans :
  forall t u v, same_key t u -> same_key u v -> same_key t v.
Proof.
  intros t u v [H1 [H2 [H3 H4]]] [G1 [G2 [G3 G4]]]. repeat split; congruence.
Qed.

Definition alit_eq_dec : forall a b : alit, {a = b} + {a <> b}.
Proof. decide equality; [apply Bool.bool_dec | apply Nat.eq_dec]. Defined.

Definition same_key_dec (t u : dtrans) : {same_key t u} + {~ same_key t u}.
Proof.
  unfold same_key.
  destruct (Nat.eq_dec (dt_src t) (dt_src u)) as [E1|N];
    [|right; intros [H _]; exact (N H)].
  destruct (Nat.eq_dec (dt_tgt t) (dt_tgt u)) as [E2|N];
    [|right; intros [_ [H _]]; exact (N H)].
  destruct (list_eq_dec alit_eq_dec (dt_label t) (dt_label u)) as [E3|N];
    [|right; intros [_ [_ [H _]]]; exact (N H)].
  destruct (list_eq_dec (@clock_eq_dec root) (dt_resets t) (dt_resets u)) as [E4|N];
    [|right; intros [_ [_ [_ H]]]; exact (N H)].
  left. repeat split; assumption.
Defined.

Definition add_guard (t : dtrans) (g : dguard) : dtrans :=
  {| dt_src := dt_src t; dt_label := dt_label t;
     dt_guard := dt_guard t ++ g;
     dt_resets := dt_resets t; dt_tgt := dt_tgt t |}.

Lemma add_guard_key : forall t g, same_key (add_guard t g) t.
Proof. intros. repeat split. Qed.

(* Insertion of a transition: its guard is added to a transition with the
   same key, if any. *)
Fixpoint insert_trans (t : dtrans) (acc : list dtrans) : list dtrans :=
  match acc with
  | [] => [t]
  | u :: acc' =>
      if same_key_dec u t
      then add_guard u (dt_guard t) :: acc'
      else u :: insert_trans t acc'
  end.

Definition group (ts : list dtrans) : list dtrans := fold_right insert_trans [] ts.

(* Every conjunct of an inserted transition is kept, with the same key. *)
Lemma insert_keeps :
  forall t acc,
    (exists u, In u (insert_trans t acc) /\ same_key u t /\
               incl (dt_guard t) (dt_guard u)) /\
    (forall v, In v acc -> exists u, In u (insert_trans t acc) /\
               same_key u v /\ incl (dt_guard v) (dt_guard u)).
Proof.
  intros t acc. induction acc as [|u acc IH]; simpl.
  - split.
    + exists t. split; [left; reflexivity|]. split; [apply same_key_refl|].
      intros c Hc; exact Hc.
    + intros v [].
  - destruct (same_key_dec u t) as [Hk|Hk].
    + split.
      * exists (add_guard u (dt_guard t)). split; [left; reflexivity|].
        split; [exact (same_key_trans (add_guard_key u (dt_guard t)) Hk)|].
        intros c Hc. simpl. apply in_or_app. right. exact Hc.
      * intros v [->|Hv].
        -- exists (add_guard v (dt_guard t)). split; [left; reflexivity|].
           split; [apply add_guard_key|].
           intros c Hc. simpl. apply in_or_app. left. exact Hc.
        -- exists v. split; [right; exact Hv|]. split; [apply same_key_refl|].
           intros c Hc; exact Hc.
    + destruct IH as [[w [Hw [Hwk Hwg]]] IHv].
      split.
      * exists w. split; [right; exact Hw|]. split; assumption.
      * intros v [->|Hv].
        -- exists v. split; [left; reflexivity|]. split; [apply same_key_refl|].
           intros c Hc; exact Hc.
        -- destruct (IHv v Hv) as [w' [Hw' [Hk' Hg']]].
           exists w'. split; [right; exact Hw'|]. split; assumption.
Qed.

(* Every conjunct of a grouped transition comes from an original one with
   the same key. *)
Lemma insert_origin :
  forall t acc u c,
    In u (insert_trans t acc) -> In c (dt_guard u) ->
    (exists v, In v acc /\ same_key u v /\ In c (dt_guard v)) \/
    (same_key u t /\ In c (dt_guard t)).
Proof.
  intros t acc. induction acc as [|w acc IH]; intros u c Hu Hc; simpl in Hu.
  - destruct Hu as [Heq|[]]. subst u. right. split; [apply same_key_refl | exact Hc].
  - destruct (same_key_dec w t) as [Hk|Hk].
    + destruct Hu as [Heq|Hu].
      * subst u. simpl in Hc. apply in_app_or in Hc. destruct Hc as [Hc|Hc].
        -- left. exists w. split; [left; reflexivity|].
           split; [apply add_guard_key | exact Hc].
        -- right. split; [exact (same_key_trans (add_guard_key w (dt_guard t)) Hk)
                         | exact Hc].
      * left. exists u. split; [right; exact Hu|].
        split; [apply same_key_refl | exact Hc].
    + destruct Hu as [Heq|Hu].
      * subst u. left. exists w. split; [left; reflexivity|].
        split; [apply same_key_refl | exact Hc].
      * destruct (IH u c Hu Hc) as [[v [Hv [Hk' Hc']]]|Hr].
        -- left. exists v. split; [right; exact Hv|]. split; assumption.
        -- right. exact Hr.
Qed.

Lemma group_keeps :
  forall ts t, In t ts ->
    exists u, In u (group ts) /\ same_key u t /\ incl (dt_guard t) (dt_guard u).
Proof.
  induction ts as [|t0 ts IH]; intros t Ht; [contradiction|].
  simpl. destruct (insert_keeps t0 (group ts)) as [Hnew Hold].
  destruct Ht as [<-|Ht].
  - exact Hnew.
  - destruct (IH t Ht) as [u [Hu [Hk Hg]]].
    destruct (Hold u Hu) as [u' [Hu' [Hk' Hg']]].
    exists u'. split; [exact Hu'|]. split; [exact (same_key_trans Hk' Hk)|].
    intros c Hc. apply Hg'. apply Hg. exact Hc.
Qed.

Lemma group_origin :
  forall ts u c, In u (group ts) -> In c (dt_guard u) ->
    exists t, In t ts /\ same_key u t /\ In c (dt_guard t).
Proof.
  induction ts as [|t0 ts IH]; intros u c Hu Hc; simpl in Hu.
  - contradiction.
  - destruct (insert_origin Hu Hc) as [[v [Hv [Hk Hc']]]|[Hk Hc']].
    + destruct (IH v c Hv Hc') as [t [Ht [Hk2 Hc2]]].
      exists t. split; [right; exact Ht|].
      split; [exact (same_key_trans Hk Hk2) | exact Hc2].
    + exists t0. split; [left; reflexivity|]. split; assumption.
Qed.

Definition merge_transitions (D : DTA) : DTA :=
  {| dta_nstates := dta_nstates D;
     dta_init := dta_init D;
     dta_trans := group (dta_trans D);
     dta_accepting := dta_accepting D;
     dta_inv := dta_inv D |}.

Lemma same_key_enabled :
  forall (rho : ext_word root) i t u c,
    same_key u t -> In c (dt_guard u) -> In c (dt_guard t) ->
    dtrans_enabled rho i u -> conj_holds (at_event rho i) c ->
    dtrans_enabled rho i t.
Proof.
  intros rho i t u c [_ [_ [Hl Hr]]] _ Hct [Hlab [_ Hres]] Hc.
  split; [rewrite <- Hl; exact Hlab|].
  split; [exists c; split; assumption | rewrite <- Hr; exact Hres].
Qed.

Theorem merge_transitions_accepts :
  forall D w, DTA_accepts D w <-> DTA_accepts (merge_transitions D) w.
Proof.
  intro D. apply dta_accepts_of_ext.
  - intros rho [run [Hinit [Hsteps Hbuchi]]].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i. destruct (Hsteps i) as [Hbound [Hinv [u [Hu [Hsrc [Hdst Hen]]]]]].
    split; [exact Hbound|]. split; [exact Hinv|].
    pose proof Hen as [_ [[c [Hc Hch]] _]].
    destruct (group_origin Hu Hc) as [t [Ht [Hk Hct]]].
    exists t. split; [exact Ht|].
    destruct Hk as [Hs [Hd [Hl Hr]]] eqn:Hk'.
    split; [congruence|]. split; [congruence|].
    exact (same_key_enabled Hk Hc Hct Hen Hch).
  - intros rho _ [run [Hinit [Hsteps Hbuchi]]].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i. destruct (Hsteps i) as [Hbound [Hinv [t [Ht [Hsrc [Hdst Hen]]]]]].
    split; [exact Hbound|]. split; [exact Hinv|].
    destruct (group_keeps Ht) as [u [Hu [Hk Hg]]].
    exists u. split; [exact Hu|].
    destruct Hk as [Hs [Hd [Hl Hr]]].
    split; [congruence|]. split; [congruence|].
    destruct Hen as [Hlab [[c [Hc Hch]] Hres]].
    split; [rewrite Hl; exact Hlab|].
    split; [exists c; split; [apply Hg; exact Hc | exact Hch] | rewrite Hr; exact Hres].
Qed.


(* ====================================================================== *)
(* 4. Disjunctive invariants                                              *)
(* ====================================================================== *)

(* Upper bounds of a conjunct. *)
Fixpoint ub_conj (c : dconj) : uinv :=
  match c with
  | [] => []
  | DSingle k :: c' =>
      (if is_upper (guard_comparison k)
       then [(guard_clock k, guard_bound k)] else []) ++ ub_conj c'
  | DDiff _ _ _ :: c' => ub_conj c'
  end.

Lemma ub_conj_sound :
  forall v c, conj_holds v c -> uinv_holds v (ub_conj c).
Proof.
  intros v c. unfold conj_holds, uinv_holds.
  induction c as [|[k|x y b] c IH]; intro H; simpl; [constructor| |].
  - inversion H as [|a l Ha Hl]; subst.
    apply Forall_app. split; [|exact (IH Hl)].
    destruct (is_upper (guard_comparison k)) eqn:Hu; [|constructor].
    constructor; [|constructor]. simpl.
    unfold dc_holds, single_holds in Ha.
    destruct (guard_comparison k); simpl in Hu; try discriminate; lra.
  - inversion H; subst. exact (IH ltac:(assumption)).
Qed.

Definition outgoing_d (D : DTA) (l : nat) : list dtrans :=
  filter (fun t => Nat.eqb (dt_src t) l) (dta_trans D).

(* Disjunctive invariant of [l]: one disjunct per conjunct of an outgoing
   guard. *)
Definition disj_inv (D : DTA) (l : nat) : list uinv :=
  flat_map (fun t => map ub_conj (dt_guard t)) (outgoing_d D l).

Lemma disj_inv_in :
  forall D t c,
    In t (dta_trans D) -> In c (dt_guard t) ->
    In (ub_conj c) (disj_inv D (dt_src t)).
Proof.
  intros D t c Ht Hc. unfold disj_inv. apply in_flat_map.
  exists t. split.
  - unfold outgoing_d. apply filter_In. split; [exact Ht | apply Nat.eqb_refl].
  - apply in_map. exact Hc.
Qed.

(* ====================================================================== *)
(* 5. Admissible delays and the copy with the longest one                 *)
(* ====================================================================== *)

(* Longest delay admitted by [U] from [v]: [None] stands for +infinity. *)
Fixpoint dmin (U : uinv) (v : valuation) : option R :=
  match U with
  | [] => None
  | (x, b) :: U' =>
      match dmin U' v with
      | None => Some (b - v x)
      | Some m => Some (Rmin (b - v x) m)
      end
  end.

(* Order on delays, +infinity being the greatest. *)
Definition Dle (a b : option R) : Prop :=
  match b with
  | None => True
  | Some bk => match a with None => False | Some al => al <= bk end
  end.

Lemma Dle_refl : forall a, Dle a a.
Proof. intros [a|]; simpl; [lra | exact I]. Qed.

Lemma Dle_trans : forall a b c, Dle a b -> Dle b c -> Dle a c.
Proof.
  intros [a|] [b|] [c|]; simpl; intros; try lra; tauto.
Qed.

Lemma Dle_total : forall a b, ~ Dle a b -> Dle b a.
Proof.
  intros [a|] [b|]; simpl; intro H; try tauto; lra.
Qed.

Definition Dle_dec (a b : option R) : {Dle a b} + {~ Dle a b}.
Proof.
  destruct b as [b|]; [|left; exact I].
  destruct a as [a|]; [|right; simpl; auto].
  simpl. apply Rle_dec.
Defined.

Lemma dmin_le :
  forall U v m, dmin U v = Some m ->
    forall p, In p U -> m <= snd p - v (fst p).
Proof.
  induction U as [|[x b] U IH]; intros v m Hm p Hp; simpl in *; [discriminate|].
  destruct (dmin U v) as [m'|] eqn:E.
  - injection Hm as <-. destruct Hp as [<-|Hp]; simpl.
    + apply Rmin_l.
    + pose proof (IH v m' E p Hp). pose proof (Rmin_r (b - v x) m'). lra.
  - injection Hm as <-. destruct Hp as [<-|Hp]; simpl; [lra|].
    destruct U as [|q U']; [contradiction|].
    simpl in E. destruct q as [y c]. destruct (dmin U' v); discriminate.
Qed.

Lemma dmin_attained :
  forall U v m, dmin U v = Some m ->
    exists p, In p U /\ m = snd p - v (fst p).
Proof.
  induction U as [|[x b] U IH]; intros v m Hm; simpl in *; [discriminate|].
  destruct (dmin U v) as [m'|] eqn:E.
  - injection Hm as <-. unfold Rmin. destruct (Rle_dec (b - v x) m').
    + exists (x, b). split; [left; reflexivity | reflexivity].
    + destruct (IH v m' E) as [p [Hp Hpm]].
      exists p. split; [right; exact Hp | exact Hpm].
  - injection Hm as <-. exists (x, b). split; [left; reflexivity | reflexivity].
Qed.

Lemma dmin_none : forall U v, dmin U v = None -> U = [].
Proof.
  intros [|[x b] U] v H; [reflexivity|]. simpl in H.
  destruct (dmin U v); discriminate.
Qed.

(* [U] holds after a delay [d] from [v] iff [d] is at most the delay
   admitted by [U]. *)
Lemma uinv_shift :
  forall U v d,
    uinv_holds (fun x => v x + d) U <-> Dle (Some d) (dmin U v).
Proof.
  intros U v d. unfold uinv_holds.
  destruct (dmin U v) as [m|] eqn:Hm.
  - simpl. split.
    + intro H. destruct (dmin_attained Hm) as [p [Hp ->]].
      rewrite Forall_forall in H. pose proof (H p Hp). simpl in *. lra.
    + intro H. rewrite Forall_forall. intros p Hp.
      pose proof (dmin_le Hm Hp). simpl. lra.
  - apply dmin_none in Hm. subst U. simpl. split; intro; [exact I | constructor].
Qed.

(* Index of a copy admitting the longest delay. *)
Fixpoint best (ds : list (option R)) : nat :=
  match ds with
  | [] => 0
  | d :: ds' =>
      match ds' with
      | [] => 0
      | _ => let k := best ds' in
             if Dle_dec d (nth k ds' None) then S k else 0
      end
  end.

Lemma best_spec :
  forall ds, ds <> [] ->
    (best ds < length ds)%nat /\
    forall l, (l < length ds)%nat -> Dle (nth l ds None) (nth (best ds) ds None).
Proof.
  induction ds as [|d ds IH]; intro Hne; [contradiction|].
  destruct ds as [|d' ds''].
  - simpl. split; [lia|]. intros l Hl. assert (l = 0%nat) by lia. subst.
    apply Dle_refl.
  - set (ds := d' :: ds'') in *.
    destruct (IH ltac:(discriminate)) as [Hk Hall].
    change (best (d :: ds)) with
      (if Dle_dec d (nth (best ds) ds None) then S (best ds) else 0%nat).
    destruct (Dle_dec d (nth (best ds) ds None)) as [Hle|Hnle].
    + split; [change (length (d :: ds)) with (S (length ds)); lia|].
      intros [|l] Hl; [exact Hle|].
      change (Dle (nth l ds None) (nth (best ds) ds None)).
      apply Hall. change (length (d :: ds)) with (S (length ds)) in Hl. lia.
    + split; [change (length (d :: ds)) with (S (length ds)); lia|].
      intros [|l] Hl; [apply Dle_refl|].
      change (Dle (nth l ds None) d).
      apply Dle_trans with (nth (best ds) ds None).
      * apply Hall. change (length (d :: ds)) with (S (length ds)) in Hl. lia.
      * apply Dle_total. exact Hnle.
Qed.

(* ====================================================================== *)
(* 6. Entry condition G_k                                                 *)
(* ====================================================================== *)

Definition upper_singles (U : uinv) : dconj :=
  map (fun p => DSingle {| guard_clock := fst p; guard_comparison := CLe;
                           guard_bound := snd p |}) U.

Lemma upper_singles_holds :
  forall v U, uinv_holds v U -> conj_holds v (upper_singles U).
Proof.
  intros v U H. unfold uinv_holds, conj_holds, upper_singles in *.
  rewrite Forall_map. eapply Forall_impl; [|exact H].
  intros p Hp. exact Hp.
Qed.

(* Copy [Ul] does not admit a strictly longer delay than copy [Uk]:
   some bound y of Ul is reached no later than every bound x of Uk, i.e.
   x - y <= b^k_x - b^l_y. *)
Definition notlonger (Ul Uk : uinv) : dguard :=
  match Uk with
  | [] => [[]]
  | _ => map (fun q => map (fun p => DDiff (fst p) (fst q) (snd p - snd q)) Uk) Ul
  end.

Lemma notlonger_complete :
  forall v Ul Uk, Dle (dmin Ul v) (dmin Uk v) -> dguard_holds v (notlonger Ul Uk).
Proof.
  intros v Ul Uk H.
  destruct Uk as [|pk Uk'].
  - exists []. split; [left; reflexivity | constructor].
  - set (Uk := pk :: Uk') in *.
    change (notlonger Ul Uk) with
      (map (fun q => map (fun p => DDiff (fst p) (fst q) (snd p - snd q)) Uk) Ul).
    destruct (dmin Uk v) as [mk|] eqn:Hk.
    2: { apply dmin_none in Hk. discriminate. }
    destruct (dmin Ul v) as [ml|] eqn:Hl; simpl in H; [|contradiction].
    destruct (dmin_attained Hl) as [q [Hq Hqm]].
    exists (map (fun p => DDiff (fst p) (fst q) (snd p - snd q)) Uk).
    split.
    + exact (in_map (fun q => map (fun p => DDiff (fst p) (fst q) (snd p - snd q)) Uk)
                    Ul q Hq).
    + unfold conj_holds. rewrite Forall_map. rewrite Forall_forall.
      intros p Hp. simpl. pose proof (dmin_le Hk Hp). lra.
Qed.

(* Conjunction of two guards in disjunctive normal form. *)
Definition dnf_and (g1 g2 : dguard) : dguard :=
  flat_map (fun c1 => map (fun c2 => c1 ++ c2) g2) g1.

Lemma dnf_and_holds :
  forall v g1 g2,
    dguard_holds v (dnf_and g1 g2) <-> dguard_holds v g1 /\ dguard_holds v g2.
Proof.
  intros v g1 g2. unfold dguard_holds, dnf_and, conj_holds. split.
  - intros [c [Hc Hh]]. apply in_flat_map in Hc.
    destruct Hc as [c1 [Hc1 Hc]]. apply in_map_iff in Hc.
    destruct Hc as [c2 [<- Hc2]]. apply Forall_app in Hh.
    split; [exists c1 | exists c2]; split; tauto.
  - intros [[c1 [Hc1 H1]] [c2 [Hc2 H2]]].
    exists (c1 ++ c2). split.
    + apply in_flat_map. exists c1. split; [exact Hc1|].
      apply in_map. exact Hc2.
    + apply Forall_app. split; assumption.
Qed.

Fixpoint notlonger_all (Us : list uinv) (Uk : uinv) : dguard :=
  match Us with
  | [] => [[]]
  | Ul :: Us' => dnf_and (notlonger Ul Uk) (notlonger_all Us' Uk)
  end.

Lemma notlonger_all_complete :
  forall v Us Uk,
    (forall Ul, In Ul Us -> Dle (dmin Ul v) (dmin Uk v)) ->
    dguard_holds v (notlonger_all Us Uk).
Proof.
  intros v Us Uk. induction Us as [|Ul Us IH]; intro H; simpl.
  - exists []. split; [left; reflexivity | constructor].
  - apply dnf_and_holds. split.
    + apply notlonger_complete. apply H. left. reflexivity.
    + apply IH. intros U HU. apply H. right. exact HU.
Qed.

(* G_k: the invariant of copy k holds, and no copy admits a strictly longer
   delay. *)
Definition entry_cond (Us : list uinv) (k : nat) : dguard :=
  dnf_and [upper_singles (nth k Us [])] (notlonger_all Us (nth k Us [])).

Lemma entry_cond_complete :
  forall v Us k,
    (forall Ul, In Ul Us -> Dle (dmin Ul v) (dmin (nth k Us []) v)) ->
    uinv_holds v (nth k Us []) ->
    dguard_holds v (entry_cond Us k).
Proof.
  intros v Us k Hmax Hk. unfold entry_cond. apply dnf_and_holds. split.
  - exists (upper_singles (nth k Us [])). split; [left; reflexivity|].
    apply upper_singles_holds. exact Hk.
  - apply notlonger_all_complete. exact Hmax.
Qed.

(* ====================================================================== *)
(* 7. Substitution [Z := 0]                                               *)
(* ====================================================================== *)

(* Clock values right after a transition resetting [Z]. *)
Definition post (u : valuation) (Z : list (Clock root)) : valuation :=
  fun x => if In_dec (@clock_eq_dec root) x Z then 0 else u x.

(* Whether a constraint holds when its clock is 0 (real comparisons). *)
Definition const_true (k : clock_constraint root) : bool :=
  match guard_comparison k with
  | CLe => if Rle_dec 0 (guard_bound k) then true else false
  | CLt => if Rlt_dec 0 (guard_bound k) then true else false
  | CGe => if Rle_dec (guard_bound k) 0 then true else false
  | CGt => if Rlt_dec (guard_bound k) 0 then true else false
  | CEq => if Req_EM_T 0 (guard_bound k) then true else false
  end.

Lemma const_true_spec :
  forall k, single_holds (fun _ => 0) k -> const_true k = true.
Proof.
  intros k H. unfold const_true, single_holds in *.
  destruct (guard_comparison k).
  - destruct (Rle_dec 0 (guard_bound k)); [reflexivity | contradiction].
  - destruct (Rlt_dec 0 (guard_bound k)); [reflexivity | contradiction].
  - destruct (Rle_dec (guard_bound k) 0); [reflexivity | contradiction].
  - destruct (Rlt_dec (guard_bound k) 0); [reflexivity | contradiction].
  - destruct (Req_EM_T 0 (guard_bound k)); [reflexivity | contradiction].
Qed.

(* A constraint on the values after the resets, expressed on the values
   before them: [None] if it is false, [Some []] if it is true. *)
Definition subst_c (Z : list (Clock root)) (c : dconstraint) : option dconj :=
  match c with
  | DSingle k =>
      if In_dec (@clock_eq_dec root) (guard_clock k) Z
      then (if const_true k then Some [] else None)
      else Some [c]
  | DDiff x y b =>
      if In_dec (@clock_eq_dec root) x Z then
        if In_dec (@clock_eq_dec root) y Z then
          (if Rle_dec 0 b then Some [] else None)
        else Some [DSingle {| guard_clock := y; guard_comparison := CGe;
                              guard_bound := - b |}]
      else
        if In_dec (@clock_eq_dec root) y Z then
          Some [DSingle {| guard_clock := x; guard_comparison := CLe;
                           guard_bound := b |}]
        else Some [c]
  end.

Fixpoint subst_conj (Z : list (Clock root)) (c : dconj) : option dconj :=
  match c with
  | [] => Some []
  | a :: c' =>
      match subst_c Z a, subst_conj Z c' with
      | Some p, Some q => Some (p ++ q)
      | _, _ => None
      end
  end.

Definition subst_guard (Z : list (Clock root)) (g : dguard) : dguard :=
  flat_map (fun c => match subst_conj Z c with Some c' => [c'] | None => [] end) g.

Lemma single_holds_at :
  forall v v' k, v (guard_clock k) = v' (guard_clock k) ->
    single_holds v k -> single_holds v' k.
Proof.
  intros v v' k Heq H. unfold single_holds in *. rewrite <- Heq.
  destruct (guard_comparison k); exact H.
Qed.

Lemma subst_c_complete :
  forall u Z a,
    dc_holds (post u Z) a ->
    exists p, subst_c Z a = Some p /\ conj_holds u p.
Proof.
  intros u Z [k|x y b] H; simpl in *; unfold conj_holds.
  - destruct (In_dec (@clock_eq_dec root) (guard_clock k) Z) as [HZ|HZ].
    + exists []. split; [|constructor].
      assert (Hc : const_true k = true).
      { apply const_true_spec. apply (single_holds_at (v := post u Z)); [|exact H].
        unfold post. destruct (In_dec (@clock_eq_dec root) (guard_clock k) Z);
          [reflexivity | contradiction]. }
      destruct (In_dec (@clock_eq_dec root) (guard_clock k) Z) as [_|Hn];
        [|contradiction].
      rewrite Hc. reflexivity.
    + exists [DSingle k]. split; [reflexivity|].
      constructor; [|constructor]. simpl.
      apply (single_holds_at (v := post u Z)); [|exact H].
      unfold post. destruct (In_dec (@clock_eq_dec root) (guard_clock k) Z);
        [contradiction | reflexivity].
  - unfold post in H.
    destruct (In_dec (@clock_eq_dec root) x Z) as [Hx|Hx];
      destruct (In_dec (@clock_eq_dec root) y Z) as [Hy|Hy].
    + destruct (Rle_dec 0 b) as [_|Hn]; [|exfalso; lra].
      exists []. split; [reflexivity | constructor].
    + eexists. split; [reflexivity|].
      constructor; [|constructor]. simpl. unfold single_holds. simpl. lra.
    + eexists. split; [reflexivity|].
      constructor; [|constructor]. simpl. unfold single_holds. simpl. lra.
    + eexists. split; [reflexivity|].
      constructor; [|constructor]. simpl. lra.
Qed.

Lemma subst_conj_complete :
  forall u Z c,
    conj_holds (post u Z) c ->
    exists p, subst_conj Z c = Some p /\ conj_holds u p.
Proof.
  intros u Z c. unfold conj_holds.
  induction c as [|a c IH]; intro H; simpl.
  - exists []. split; [reflexivity | constructor].
  - inversion H as [|a' c' Ha Hc]; subst.
    destruct (subst_c_complete Ha) as [p [Hp Hph]].
    destruct (IH Hc) as [q [Hq Hqh]].
    rewrite Hp, Hq. exists (p ++ q). split; [reflexivity|].
    apply Forall_app. split; assumption.
Qed.

Lemma subst_guard_complete :
  forall u Z g,
    dguard_holds (post u Z) g -> dguard_holds u (subst_guard Z g).
Proof.
  intros u Z g [c [Hc Hh]].
  destruct (subst_conj_complete Hh) as [p [Hp Hph]].
  exists p. split; [|exact Hph].
  unfold subst_guard. apply in_flat_map. exists c. split; [exact Hc|].
  rewrite Hp. left. reflexivity.
Qed.

(* Along a clock-consistent extended word, the values after the resets of
   event i are those of event i+1 minus the delay. *)
Lemma post_reset :
  forall (rho : ext_word root) i Z,
    clock_consistent rho ->
    resets_match rho i Z ->
    forall x, ew_val rho (S i) x =
              post (at_event rho i) Z x + delta (ew_base rho) i.
Proof.
  intros rho i Z [_ Hstep] Hres x.
  rewrite (Hstep i x). unfold post, at_event.
  destruct (In_dec (@clock_eq_dec root) x Z) as [HZ|HZ].
  - rewrite (proj2 (Hres x) HZ). lra.
  - destruct (ew_reset rho i x) eqn:E; [|reflexivity].
    exfalso. apply HZ. apply (proj1 (Hres x)). exact E.
Qed.


Lemma dc_holds_ext :
  forall v v' c, (forall x, v x = v' x) -> dc_holds v c -> dc_holds v' c.
Proof.
  intros v v' [k|x y b] Hext H; simpl in *.
  - unfold single_holds in *. rewrite <- Hext.
    destruct (guard_comparison k); exact H.
  - rewrite <- !Hext. exact H.
Qed.

Lemma dguard_holds_ext :
  forall v v' g, (forall x, v x = v' x) -> dguard_holds v g -> dguard_holds v' g.
Proof.
  intros v v' g Hext [c [Hc Hh]]. exists c. split; [exact Hc|].
  unfold conj_holds in *. eapply Forall_impl; [|exact Hh].
  intros a Ha. exact (dc_holds_ext Hext Ha).
Qed.

Lemma uinv_holds_ext :
  forall v v' U, (forall x, v x = v' x) -> uinv_holds v U -> uinv_holds v' U.
Proof.
  intros v v' U Hext H. unfold uinv_holds in *.
  eapply Forall_impl; [|exact H]. intros p Hp. rewrite <- Hext. exact Hp.
Qed.

Lemma Dle_some_mono :
  forall a a' b, a' <= a -> Dle (Some a) b -> Dle (Some a') b.
Proof. intros a a' [b|] Ha H; simpl in *; [lra | exact I]. Qed.

(* ====================================================================== *)
(* 8. Splitting the locations with disjunctive invariants                 *)
(* ====================================================================== *)

(* Number of regular copies: the largest number of disjuncts.  Copy
   [kmax D] is the dedicated initial copy, without invariant. *)
Definition kmax (D : DTA) : nat :=
  list_max (map (fun l => length (disj_inv D l))
                (seq 0 (dta_nstates D) ++ map (fun t => dt_tgt t) (dta_trans D))).

Definition kp (D : DTA) : nat := S (kmax D).

Definition enc (D : DTA) (l k : nat) : nat := (l * kp D + k)%nat.

Lemma kp_nz : forall D, kp D <> 0%nat.
Proof. intro D. unfold kp. lia. Qed.

Lemma enc_div : forall D l k, (k < kp D)%nat -> (enc D l k / kp D)%nat = l.
Proof.
  intros D l k Hk. unfold enc. symmetry.
  apply (Nat.div_unique (l * kp D + k) (kp D) l k); [assumption | lia].
Qed.

Lemma enc_mod : forall D l k, (k < kp D)%nat -> (enc D l k mod kp D)%nat = k.
Proof.
  intros D l k Hk. unfold enc. symmetry.
  apply (Nat.mod_unique (l * kp D + k) (kp D) l k); [assumption | lia].
Qed.

Lemma enc_lt :
  forall D l k n, (l < n)%nat -> (k < kp D)%nat -> (enc D l k < n * kp D)%nat.
Proof. intros. unfold enc. nia. Qed.

Lemma length_le_kmax :
  forall D l,
    In l (seq 0 (dta_nstates D) ++ map (fun t => dt_tgt t) (dta_trans D)) ->
    (length (disj_inv D l) <= kmax D)%nat.
Proof.
  intros D l Hl. unfold kmax.
  assert (Hall := proj1 (list_max_le
            (map (fun l => length (disj_inv D l))
                 (seq 0 (dta_nstates D) ++ map (fun t => dt_tgt t) (dta_trans D)))
            (list_max (map (fun l => length (disj_inv D l))
                 (seq 0 (dta_nstates D) ++ map (fun t => dt_tgt t) (dta_trans D)))))
            (Nat.le_refl _)).
  rewrite Forall_forall in Hall. apply Hall.
  apply (in_map (fun l => length (disj_inv D l))). exact Hl.
Qed.

Lemma length_le_kmax_tgt :
  forall D t, In t (dta_trans D) -> (length (disj_inv D (dt_tgt t)) <= kmax D)%nat.
Proof.
  intros D t Ht. apply length_le_kmax. apply in_or_app. right.
  apply (in_map (fun t => dt_tgt t)). exact Ht.
Qed.

(* Every transition l --g,Z--> l' is replicated from every copy of l
   toward every regular copy k of l', with the guard g /\ G_k[Z := 0]. *)
Definition split_trans (D : DTA) (t : dtrans) : list dtrans :=
  flat_map (fun j =>
    map (fun k =>
           {| dt_src := enc D (dt_src t) j;
              dt_label := dt_label t;
              dt_guard := dnf_and (dt_guard t)
                            (subst_guard (dt_resets t)
                               (entry_cond (disj_inv D (dt_tgt t)) k));
              dt_resets := dt_resets t;
              dt_tgt := enc D (dt_tgt t) k |})
        (seq 0 (length (disj_inv D (dt_tgt t)))))
    (seq 0 (kp D)).

(* Copy k of l carries the k-th disjunct of the invariant of l. *)
Definition split_inv (D : DTA) (s : nat) : option (list uinv) :=
  let k := (s mod kp D)%nat in
  if Nat.eqb k (kmax D) then None
  else match nth_error (disj_inv D (s / kp D)%nat) k with
       | Some U => Some [U]
       | None => None
       end.

Definition split (D : DTA) : DTA :=
  {| dta_nstates := (dta_nstates D * kp D)%nat;
     dta_init := enc D (dta_init D) (kmax D);
     dta_trans := flat_map (split_trans D) (dta_trans D);
     dta_accepting := flat_map (fun l => map (enc D l) (seq 0 (kp D))) (dta_accepting D);
     dta_inv := split_inv D |}.

Lemma split_trans_in :
  forall D t t',
    In t' (split_trans D t) ->
    exists j k, (j < kp D)%nat /\ (k < length (disj_inv D (dt_tgt t)))%nat /\
      dt_src t' = enc D (dt_src t) j /\ dt_tgt t' = enc D (dt_tgt t) k /\
      dt_label t' = dt_label t /\ dt_resets t' = dt_resets t /\
      dt_guard t' = dnf_and (dt_guard t)
                      (subst_guard (dt_resets t) (entry_cond (disj_inv D (dt_tgt t)) k)).
Proof.
  intros D t t' Hin. unfold split_trans in Hin.
  apply in_flat_map in Hin. destruct Hin as [j [Hj Hin]].
  apply in_map_iff in Hin. destruct Hin as [k [<- Hk]].
  apply in_seq in Hj. apply in_seq in Hk.
  exists j, k. repeat split; simpl; lia || reflexivity.
Qed.

Lemma split_sound :
  forall D (rho : ext_word root),
    (forall l, dta_inv D l = None) ->
    DTA_ext_accepts (split D) rho -> DTA_ext_accepts D rho.
Proof.
  intros D rho Hnone [run [Hinit [Hsteps Hbuchi]]].
  exists (fun i => (run i / kp D)%nat).
  split.
  - simpl. rewrite Hinit. simpl. apply enc_div. unfold kp. lia.
  - split.
    + intro i.
      destruct (Hsteps i) as [Hbound [_ [t' [Hin [Hsrc [Hdst Hen]]]]]].
      simpl in Hbound, Hin.
      split.
      * apply Nat.div_lt_upper_bound; [apply kp_nz | lia].
      * split; [rewrite Hnone; intros dl _; exact I|].
        apply in_flat_map in Hin. destruct Hin as [t [Ht Hin]].
        destruct (split_trans_in Hin)
          as [j [k [Hj [Hk [Hs [Hd [Hl [Hr Hg]]]]]]]].
        pose proof (length_le_kmax_tgt Ht) as Hlen.
        exists t. split; [exact Ht|].
        split; [rewrite <- Hsrc, Hs; symmetry; apply enc_div; exact Hj|].
        split; [rewrite <- Hdst, Hd; symmetry; apply enc_div; unfold kp; lia|].
        destruct Hen as [Hlab [Hguard Hres]].
        rewrite Hg in Hguard. apply dnf_and_holds in Hguard.
        split; [rewrite <- Hl; exact Hlab|].
        split; [exact (proj1 Hguard) | rewrite <- Hr; exact Hres].
    + intro n. destruct (Hbuchi n) as [j [Hj Hacc]].
      exists j. split; [exact Hj|].
      change (In (run j) (flat_map (fun l => map (enc D l) (seq 0 (kp D)))
                                   (dta_accepting D))) in Hacc.
      apply in_flat_map in Hacc. destruct Hacc as [l [Hl Hin]].
      apply in_map_iff in Hin. destruct Hin as [k [Hk Hks]]. apply in_seq in Hks.
      rewrite <- Hk, (@enc_div D l k ltac:(lia)). exact Hl.
Qed.

Lemma split_complete :
  forall D (rho : ext_word root),
    clock_consistent rho ->
    DTA_ext_accepts D rho -> DTA_ext_accepts (split D) rho.
Proof.
  intros D rho Hcc [run [Hinit [Hsteps Hbuchi]]].
  (* values right after the resets of event j *)
  set (v := fun (j : nat) (x : Clock root) =>
              ew_val rho (S j) x - delta (ew_base rho) j).
  set (Us := fun l => disj_inv D l).
  set (ds := fun j => map (fun U => dmin U (v j)) (Us (run (S j)))).
  set (copy := fun i => match i with
                        | O => kmax D
                        | S j => best (ds j)
                        end).
  (* the invariant of every visited location has a disjunct *)
  assert (Hne : forall i, Us (run i) <> []).
  { intro i. destruct (Hsteps i) as [_ [_ [t [Ht [Hs [_ [_ [[c [Hc _]] _]]]]]]]].
    pose proof (disj_inv_in Ht Hc) as H. unfold Us. rewrite <- Hs.
    intro E. rewrite E in H. exact H. }
  assert (Hds_ne : forall j, ds j <> []).
  { intros j E. unfold ds in E. apply map_eq_nil in E. exact (Hne (S j) E). }
  assert (Hds_nth : forall j l, nth l (ds j) None = dmin (nth l (Us (run (S j))) []) (v j)).
  { intros j l. unfold ds.
    change None with ((fun U => dmin U (v j)) []).
    apply map_nth. }
  assert (Hlen_ds : forall j, length (ds j) = length (Us (run (S j)))).
  { intro j. unfold ds. apply length_map. }
  (* the chosen copy is valid *)
  assert (Hcopy_lt : forall j, (best (ds j) < length (Us (run (S j))))%nat).
  { intro j. rewrite <- Hlen_ds. exact (proj1 (best_spec (Hds_ne j))). }
  assert (Hlen_le : forall j, (length (Us (run (S j))) <= kmax D)%nat).
  { intro j. destruct (Hsteps j) as [_ [_ [t [Ht [_ [Hd _]]]]]].
    unfold Us. rewrite <- Hd. exact (length_le_kmax_tgt Ht). }
  (* key property of the chosen copy *)
  assert (Hkey : forall j,
            let Uk := nth (best (ds j)) (Us (run (S j))) [] in
            (forall Ul, In Ul (Us (run (S j))) -> Dle (dmin Ul (v j)) (dmin Uk (v j))) /\
            Dle (Some (delta (ew_base rho) j)) (dmin Uk (v j))).
  { intro j. simpl.
    destruct (best_spec (Hds_ne j)) as [_ Hmax].
    assert (Hmax' : forall Ul, In Ul (Us (run (S j))) ->
              Dle (dmin Ul (v j)) (dmin (nth (best (ds j)) (Us (run (S j))) []) (v j))).
    { intros Ul HUl. destruct (In_nth _ _ [] HUl) as [l [Hl Hnth]].
      rewrite <- Hnth, <- !Hds_nth. apply Hmax. rewrite Hlen_ds. exact Hl. }
    split; [exact Hmax'|].
    destruct (Hsteps (S j)) as [_ [_ [t [Ht [Hs [_ [_ [[c [Hc Hch]] _]]]]]]]].
    pose proof (disj_inv_in Ht Hc) as HU. rewrite Hs in HU.
    pose proof (ub_conj_sound Hch) as Hub.
    assert (Hshift : uinv_holds (fun x => v j x + delta (ew_base rho) j) (ub_conj c)).
    { apply (uinv_holds_ext (v := at_event rho (S j))); [|exact Hub].
      intro x. unfold v, at_event. ring. }
    apply uinv_shift in Hshift.
    apply Dle_trans with (dmin (ub_conj c) (v j)); [exact Hshift|].
    apply Hmax'. exact HU. }
  exists (fun i => enc D (run i) (copy i)).
  split; [simpl; rewrite Hinit; reflexivity|].
  split.
  - intro i.
    destruct (Hsteps i) as [Hbound [_ [t [Ht [Hsrc [Hdst Hen]]]]]].
    assert (Hcopy_i : (copy i < kp D)%nat).
    { destruct i as [|j]; simpl; unfold kp; [lia|].
      pose proof (Hcopy_lt j). pose proof (Hlen_le j). lia. }
    split; [simpl; apply enc_lt; assumption|].
    split.
    + (* invariant of the copy *)
      simpl. unfold split_inv. rewrite enc_mod, enc_div by exact Hcopy_i.
      destruct i as [|j].
      * simpl. rewrite Nat.eqb_refl. intros dl _. exact I.
      * simpl.
        pose proof (Hcopy_lt j) as Hlt. pose proof (Hlen_le j) as Hle.
        assert (Hneq : Nat.eqb (best (ds j)) (kmax D) = false)
          by (apply Nat.eqb_neq; lia).
        rewrite Hneq.
        rewrite (nth_error_nth' (disj_inv D (run (S j))) [] Hlt).
        intros dl Hdl. exists (nth (best (ds j)) (Us (run (S j))) []).
        split; [left; reflexivity|].
        destruct (Hkey j) as [_ Hdel]. simpl in Hdl.
        assert (Hsh : uinv_holds (fun x => v j x + (delta (ew_base rho) j - dl))
                        (nth (best (ds j)) (Us (run (S j))) [])).
        { apply uinv_shift. apply Dle_some_mono with (delta (ew_base rho) j);
            [lra | exact Hdel]. }
        apply (uinv_holds_ext (v := fun x => v j x + (delta (ew_base rho) j - dl)));
          [|exact Hsh].
        intro x. unfold v. ring.
    + (* the replicated transition toward the chosen copy *)
      set (k := copy (S i)).
      assert (Hk : (k < length (disj_inv D (dt_tgt t)))%nat).
      { unfold k. simpl. rewrite Hdst. exact (Hcopy_lt i). }
      exists {| dt_src := enc D (dt_src t) (copy i);
                dt_label := dt_label t;
                dt_guard := dnf_and (dt_guard t)
                              (subst_guard (dt_resets t)
                                 (entry_cond (disj_inv D (dt_tgt t)) k));
                dt_resets := dt_resets t;
                dt_tgt := enc D (dt_tgt t) k |}.
      split.
      * simpl. apply in_flat_map. exists t. split; [exact Ht|].
        unfold split_trans. apply in_flat_map. exists (copy i).
        split; [apply in_seq; lia|].
        apply (in_map (fun k0 =>
           {| dt_src := enc D (dt_src t) (copy i);
              dt_label := dt_label t;
              dt_guard := dnf_and (dt_guard t)
                            (subst_guard (dt_resets t)
                               (entry_cond (disj_inv D (dt_tgt t)) k0));
              dt_resets := dt_resets t;
              dt_tgt := enc D (dt_tgt t) k0 |})).
        apply in_seq. lia.
      * simpl. split; [rewrite Hsrc; reflexivity|].
        split; [rewrite Hdst; reflexivity|].
        destruct Hen as [Hlab [Hguard Hres]].
        split; [exact Hlab|]. split; [|exact Hres].
        apply dnf_and_holds. split; [exact Hguard|].
        apply subst_guard_complete.
        apply (dguard_holds_ext (v := v i)).
        { intro x. unfold v. rewrite (post_reset Hcc Hres x). ring. }
        rewrite Hdst. unfold k. simpl.
        destruct (Hkey i) as [Hmax Hdel].
        apply entry_cond_complete; [exact Hmax|].
        apply (uinv_holds_ext (v := fun x => v i x + 0)); [intro x; ring|].
        apply uinv_shift. apply Dle_some_mono with (delta (ew_base rho) i);
          [left; apply delta_positive | exact Hdel].
  - intro n. destruct (Hbuchi n) as [j [Hj Hacc]].
    exists j. split; [exact Hj|].
    assert (Hcj : (copy j < kp D)%nat).
    { destruct j as [|j']; simpl; unfold kp; [lia|].
      pose proof (Hcopy_lt j'). pose proof (Hlen_le j'). lia. }
    change (In (enc D (run j) (copy j))
              (flat_map (fun l => map (enc D l) (seq 0 (kp D))) (dta_accepting D))).
    apply in_flat_map. exists (run j). split; [exact Hacc|].
    apply (in_map (enc D (run j))). apply in_seq. lia.
Qed.

Theorem split_accepts :
  forall D w,
    (forall l, dta_inv D l = None) ->
    (DTA_accepts D w <-> DTA_accepts (split D) w).
Proof.
  intros D w Hnone. apply dta_accepts_of_ext.
  - intros rho H. exact (split_sound Hnone H).
  - intros rho Hcc H. exact (split_complete Hcc H).
Qed.

(* The invariants of the split automaton are conjunctive. *)
Theorem split_inv_conjunctive :
  forall D s, dta_inv (split D) s = None \/ exists U, dta_inv (split D) s = Some [U].
Proof.
  intros D s. simpl. unfold split_inv.
  destruct (Nat.eqb (s mod kp D) (kmax D)); [left; reflexivity|].
  destruct (nth_error (disj_inv D (s / kp D)) (s mod kp D)) as [U|];
    [right; exists U; reflexivity | left; reflexivity].
Qed.

(* ====================================================================== *)
(* 9. Disjunctive guards split into one transition per disjunct           *)
(* ====================================================================== *)

Definition explode (t : dtrans) : list dtrans :=
  map (fun c => {| dt_src := dt_src t; dt_label := dt_label t; dt_guard := [c];
                   dt_resets := dt_resets t; dt_tgt := dt_tgt t |})
      (dt_guard t).

Definition explode_all (D : DTA) : DTA :=
  {| dta_nstates := dta_nstates D;
     dta_init := dta_init D;
     dta_trans := flat_map explode (dta_trans D);
     dta_accepting := dta_accepting D;
     dta_inv := dta_inv D |}.

Theorem explode_accepts :
  forall D w, DTA_accepts D w <-> DTA_accepts (explode_all D) w.
Proof.
  intro D. apply dta_accepts_of_ext.
  - intros rho [run [Hinit [Hsteps Hbuchi]]].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i. destruct (Hsteps i) as [Hbound [Hinv [t' [Hin [Hsrc [Hdst Hen]]]]]].
    split; [exact Hbound|]. split; [exact Hinv|].
    simpl in Hin. apply in_flat_map in Hin. destruct Hin as [t [Ht Hin]].
    unfold explode in Hin. apply in_map_iff in Hin. destruct Hin as [c [<- Hc]].
    exists t. split; [exact Ht|]. split; [exact Hsrc|]. split; [exact Hdst|].
    destruct Hen as [Hlab [[c' [[<-|[]] Hch]] Hres]].
    split; [exact Hlab|]. split; [exists c; split; assumption | exact Hres].
  - intros rho _ [run [Hinit [Hsteps Hbuchi]]].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i. destruct (Hsteps i) as [Hbound [Hinv [t [Ht [Hsrc [Hdst Hen]]]]]].
    split; [exact Hbound|]. split; [exact Hinv|].
    destruct Hen as [Hlab [[c [Hc Hch]] Hres]].
    exists {| dt_src := dt_src t; dt_label := dt_label t; dt_guard := [c];
              dt_resets := dt_resets t; dt_tgt := dt_tgt t |}.
    split.
    + simpl. apply in_flat_map. exists t. split; [exact Ht|].
      unfold explode.
      apply (in_map (fun c => {| dt_src := dt_src t; dt_label := dt_label t;
                                 dt_guard := [c]; dt_resets := dt_resets t;
                                 dt_tgt := dt_tgt t |})).
      exact Hc.
    + simpl. split; [exact Hsrc|]. split; [exact Hdst|].
      split; [exact Hlab|].
      split; [exists c; split; [left; reflexivity | exact Hch] | exact Hres].
Qed.

Theorem explode_conjunctive :
  forall D t, In t (dta_trans (explode_all D)) -> exists c, dt_guard t = [c].
Proof.
  intros D t Hin. simpl in Hin. apply in_flat_map in Hin.
  destruct Hin as [t0 [_ Hin]]. unfold explode in Hin.
  apply in_map_iff in Hin. destruct Hin as [c [<- _]]. exists c. reflexivity.
Qed.

(* ====================================================================== *)
(* 10. The exported automaton                                             *)
(* ====================================================================== *)

Definition export (A : TBA root) : DTA :=
  explode_all (split (merge_transitions (of_tba A))).

Theorem export_accepts :
  forall A w, TBA_accepts A w <-> DTA_accepts (export A) w.
Proof.
  intros A w. unfold export.
  rewrite (of_tba_accepts A w).
  rewrite (merge_transitions_accepts (of_tba A) w).
  rewrite (@split_accepts (merge_transitions (of_tba A)) w);
    [| intro l; reflexivity].
  apply explode_accepts.
Qed.

(* Guards of the exported automaton are conjunctions (one disjunct). *)
Theorem export_guards_conjunctive :
  forall A t, In t (dta_trans (export A)) -> exists c, dt_guard t = [c].
Proof.
  intros A t Hin. exact (explode_conjunctive Hin).
Qed.

(* Invariants of the exported automaton are conjunctions of upper bounds. *)
Theorem export_invariants_conjunctive :
  forall A s,
    dta_inv (export A) s = None \/ exists U, dta_inv (export A) s = Some [U].
Proof.
  intros A s. exact (split_inv_conjunctive (merge_transitions (of_tba A)) s).
Qed.

End Export.

(* ====================================================================== *)
(* 11. End-to-end correctness of the exported automaton                   *)
(* ====================================================================== *)

Definition compile_export (n : nat) (f : mtl) : DTA f :=
  export (compile_optimized n f).

Theorem MTL_to_exported_correct :
  forall (n : nat) (f : mtl) (w : timed_word),
    well_formed f ->
    (msat w 0 f <-> DTA_accepts (compile_export n f) w).
Proof.
  intros n f w Hwf.
  rewrite (MTL_to_optimized_TBA_correct n w Hwf).
  apply export_accepts.
Qed.

Print Assumptions MTL_to_exported_correct.
