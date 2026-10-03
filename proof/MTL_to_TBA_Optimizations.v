(*
  MTL_to_TBA_Optimizations.v

  Semantics-preserving optimizations of the timed Buchi automaton:

  1. Forward propagation of lower bounds.  For a location l and a clock x,
     the entry bound of x is the minimum, over the transitions entering l,
     of the lower bound that the transition guarantees on x when l is
     entered (0 if the transition resets x, the lower bound of its guard on
     x otherwise; the initial location also counts the initial entry, with
     bound 0).  If some entering transition gives no lower bound on x, x
     has no entry bound.  The entry bound is added to the invariant of l
     (as a lower-bound invariant) and to the guards of the transitions
     leaving l.

  2. Simplification:
       - removal of the transitions whose guard contains contradictory
         arithmetic constraints;
       - removal of the locations, other than the initial one, without
         entering transition (their outgoing transitions are removed; the
         location becomes isolated).

  3. Removal of useless resets: the reset of a clock x is removed from the
     transitions entering a location where x is dead (no transition testing
     x can be taken any more); the two properties used by the proof are
     checked explicitly.

  4. Merging of synchronously reset clocks.

  5. Normalization of guards: removal of empty items, of trivially true
     lower bounds, and of constraints implied by another constraint of the
     same guard.

  Each step preserves the language of timed words; the steps are iterated
  [n] times, for every [n], together with the backward propagation of
  MTL_to_TBA_Invariants.v.

  No new axiom: the only project axiom remains LTL_TO_BUCHI_CORRECT.
*)

From Stdlib Require Import Arith Lia List Bool Reals Lra.
From Stdlib Require Import Classical ClassicalDescription.
Require Import MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.
Require Import EncodingCorrect_Shared_Clock_Derived_Strict_Direct_Proof.
Require Import MTL_to_TBA_Invariants.
Import ListNotations.
Open Scope R_scope.

Set Implicit Arguments.
Unset Strict Implicit.

Section Optimizations.

Variable root : mtl.

(* ====================================================================== *)
(* 0. Generic facts                                                       *)
(* ====================================================================== *)

(* The same automaton with another list of transitions. *)
Definition with_transitions (A : TBA root) (ts : list (tba_transition root))
    : TBA root :=
  {| tba_nstates := tba_nstates A;
     tba_init := tba_init A;
     tba_transitions := ts;
     tba_accepting := tba_accepting A |}.

(* If every transition of [B] is a transition of [A] with a stronger
   enabling condition, every run of [B] is a run of [A]. *)
Lemma with_transitions_sound :
  forall (A : TBA root) ts (rho : ext_word root),
    (forall t', In t' ts -> exists t,
        In t (tba_transitions A) /\
        bt_source t = bt_source t' /\ bt_target t = bt_target t' /\
        forall i, tba_transition_enabled rho i t' ->
                  tba_transition_enabled rho i t) ->
    TBA_ext_accepts (with_transitions A ts) rho -> TBA_ext_accepts A rho.
Proof.
  intros A ts rho Himpl [run [Hinit [Hsteps Hbuchi]]].
  exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
  intro i.
  destruct (Hsteps i) as [Hbound [t' [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  destruct (Himpl t' Hin) as [t [HinA [Hs [Hd Himp]]]].
  exists t. split; [exact HinA|].
  split; [congruence|]. split; [congruence|].
  exact (Himp i Hen).
Qed.

(* Conversely, a run of [A] is a run of [B] if every transition it uses
   has a counterpart in [B] that is enabled at the same position. *)
Lemma with_transitions_complete :
  forall (A : TBA root) ts (rho : ext_word root) (run : nat -> nat),
    run 0%nat = tba_init A ->
    (forall n, exists j, (n <= j)%nat /\ In (run j) (tba_accepting A)) ->
    (forall i,
       (run i < tba_nstates A)%nat /\
       exists t', In t' ts /\
         bt_source t' = run i /\ bt_target t' = run (S i) /\
         tba_transition_enabled rho i t') ->
    TBA_ext_accepts (with_transitions A ts) rho.
Proof.
  intros A ts rho run Hinit Hbuchi Hsteps.
  exists run. split; [exact Hinit|]. split; [exact Hsteps | exact Hbuchi].
Qed.

Lemma stay_nonneg :
  forall (rho : ext_word root) i, 0 <= stay rho i.
Proof.
  intros rho [|j]; simpl; [lra|]. left. apply delta_positive.
Qed.

(* ====================================================================== *)
(* 1. Lower bound put by a guard on a clock                               *)
(* ====================================================================== *)

Definition is_lower (k : clock_comparison) : bool :=
  match k with
  | CGe | CGt | CEq => true
  | CLe | CLt => false
  end.

Definition opt_max (a b : option R) : option R :=
  match a, b with
  | None, _ => b
  | _, None => a
  | Some u, Some v => Some (Rmax u v)
  end.

Definition lb_item (x : Clock root) (o : option (clock_constraint root))
    : option R :=
  match o with
  | Some k =>
      if is_lower (guard_comparison k) && clock_eqb (guard_clock k) x
      then Some (guard_bound k)
      else None
  | None => None
  end.

(* The lower bound that a guard puts on clock [x]: the greatest of the lower
   bounds of its items on [x]; [None] if no item bounds [x] from below. *)
Fixpoint lb_guard (g : guard root) (x : Clock root) : option R :=
  match g with
  | [] => None
  | o :: g' => opt_max (lb_item x o) (lb_guard g' x)
  end.

Lemma lb_item_sound :
  forall (rho : ext_word root) i x o b,
    guard_item_holds rho i o ->
    lb_item x o = Some b ->
    b <= ew_val rho i x.
Proof.
  intros rho i x [k|] b Hhold Hlb; simpl in *; [|discriminate].
  destruct (is_lower (guard_comparison k)) eqn:Hlo;
    destruct (clock_eqb (guard_clock k) x) eqn:Heq;
    simpl in Hlb; try discriminate.
  injection Hlb as <-.
  apply clock_eqb_true in Heq. subst x.
  unfold clock_constraint_holds in Hhold.
  destruct (guard_comparison k); simpl in Hlo; try discriminate; lra.
Qed.

Lemma lb_guard_sound :
  forall (rho : ext_word root) i g x b,
    Forall (guard_item_holds rho i) g ->
    lb_guard g x = Some b ->
    b <= ew_val rho i x.
Proof.
  intros rho i g x.
  induction g as [|o g IH]; intros b Hall Hlb; simpl in Hlb; [discriminate|].
  inversion Hall as [|o' g' Ho Hg]; subst.
  destruct (lb_item x o) as [u|] eqn:Hi;
    destruct (lb_guard g x) as [v|] eqn:Hg';
    simpl in Hlb; try discriminate.
  - injection Hlb as <-.
    pose proof (lb_item_sound Ho Hi).
    pose proof (IH v Hg eq_refl).
    unfold Rmax; destruct (Rle_dec u v); lra.
  - injection Hlb as <-. exact (lb_item_sound Ho Hi).
  - apply IH; [exact Hg | exact Hlb].
Qed.

(* Minimum of a list of bounds; [None] if the list is empty or contains an
   unbounded element. *)
Fixpoint min_all (l : list (option R)) : option R :=
  match l with
  | [] => None
  | [a] => a
  | a :: l' =>
      match a, min_all l' with
      | Some u, Some v => Some (Rmin u v)
      | _, _ => None
      end
  end.

Lemma min_all_le :
  forall l m a,
    min_all l = Some m ->
    In a l ->
    exists b, a = Some b /\ m <= b.
Proof.
  induction l as [|a0 l IH]; intros m a Hmin Hin; [contradiction|].
  destruct l as [|a1 l'].
  - simpl in Hmin. destruct Hin as [<- | []].
    exists m. split; [exact Hmin | lra].
  - change (match a0, min_all (a1 :: l') with
            | Some u, Some v => Some (Rmin u v)
            | _, _ => None end = Some m) in Hmin.
    destruct a0 as [u|]; [|discriminate].
    destruct (min_all (a1 :: l')) as [v|] eqn:Hv; [|discriminate].
    injection Hmin as <-.
    destruct Hin as [<- | Hin].
    + exists u. split; [reflexivity | apply Rmin_l].
    + destruct (IH v a eq_refl Hin) as [b [-> Hb]].
      exists b. split; [reflexivity|].
      pose proof (Rmin_r u v). lra.
Qed.

(* ====================================================================== *)
(* 2. Entry lower bounds (forward propagation)                            *)
(* ====================================================================== *)

Definition incoming (A : TBA root) (l : nat) : list (tba_transition root) :=
  filter (fun t => Nat.eqb (bt_target t) l) (tba_transitions A).

(* Lower bound guaranteed on [x] when a transition is taken: 0 if it
   resets [x], the lower bound of its guard otherwise. *)
Definition entry_bound (t : tba_transition root) (x : Clock root) : option R :=
  if In_dec (@clock_eq_dec root) x (bt_resets t) then Some 0
  else lb_guard (bt_guard t) x.

(* Entry bound of [x] in location [l]: the minimum over the entering
   transitions (and over the initial entry, with bound 0, if [l] is
   initial). *)
Definition entry_lb (A : TBA root) (l : nat) (x : Clock root) : option R :=
  min_all ((if Nat.eqb l (tba_init A) then [Some 0] else []) ++
           map (fun t => entry_bound t x) (incoming A l)).

(* On every run over a clock-consistent extended word, the entry bound of
   the current location holds during the whole stay that ends at event i. *)
Lemma entry_lb_sound :
  forall (A : TBA root) (rho : ext_word root) (run : nat -> nat),
    clock_consistent rho ->
    run 0%nat = tba_init A ->
    (forall i, exists t,
        In t (tba_transitions A) /\
        bt_source t = run i /\ bt_target t = run (S i) /\
        tba_transition_enabled rho i t) ->
    forall i x m,
      entry_lb A (run i) x = Some m ->
      forall dl, 0 <= dl <= stay rho i -> m <= ew_val rho i x - dl.
Proof.
  intros A rho run Hcc Hinit Hsteps i x m Hm dl Hdl.
  destruct Hcc as [Hnn Hstep].
  destruct i as [|j].
  - simpl in Hdl.
    assert (Hin : In (Some 0) ((if Nat.eqb (run 0%nat) (tba_init A)
                                then [Some 0] else []) ++
                               map (fun t => entry_bound t x)
                                   (incoming A (run 0%nat)))).
    { rewrite Hinit, Nat.eqb_refl. left. reflexivity. }
    destruct (min_all_le Hm Hin) as [b [Hb Hmb]].
    injection Hb as <-.
    pose proof (Hnn 0%nat x). lra.
  - destruct (Hsteps j) as [t [Hin [_ [Hdst [_ [Hguard Hres]]]]]].
    assert (Hinc : In t (incoming A (run (S j)))).
    { unfold incoming. apply filter_In. split; [exact Hin|].
      rewrite Hdst. apply Nat.eqb_refl. }
    assert (Hin2 : In (entry_bound t x)
                      ((if Nat.eqb (run (S j)) (tba_init A)
                        then [Some 0] else []) ++
                       map (fun t => entry_bound t x) (incoming A (run (S j))))).
    { apply in_or_app. right.
      apply (in_map (fun t0 => entry_bound t0 x)). exact Hinc. }
    destruct (min_all_le Hm Hin2) as [b [Hb Hmb]].
    simpl in Hdl.
    specialize (Hstep j x).
    unfold entry_bound in Hb.
    destruct (In_dec (@clock_eq_dec root) x (bt_resets t)) as [HZ|HZ].
    + injection Hb as <-.
      assert (Hr : ew_reset rho j x = true) by (apply (proj2 (Hres x)); exact HZ).
      rewrite Hr in Hstep. lra.
    + assert (Hr : ew_reset rho j x = false).
      { destruct (ew_reset rho j x) eqn:E; [|reflexivity].
        exfalso. apply HZ. apply (proj1 (Hres x)). exact E. }
      rewrite Hr in Hstep.
      pose proof (lb_guard_sound Hguard (eq_sym (eq_sym Hb))) as Hlb.
      lra.
Qed.


(* ---------------------------------------------------------------------- *)
(* Forward propagation step: the entry bounds of the source location are  *)
(* added to the guards of the transitions leaving it.                     *)
(* ---------------------------------------------------------------------- *)

Definition lower_clocks (o : option (clock_constraint root)) : list (Clock root) :=
  match o with
  | Some k => if is_lower (guard_comparison k) then [guard_clock k] else []
  | None => []
  end.

(* Candidate clocks of the entry bounds of [l]. *)
Definition entry_clocks (A : TBA root) (l : nat) : list (Clock root) :=
  flat_map (fun t => bt_resets t ++ flat_map lower_clocks (bt_guard t))
           (incoming A l).

Definition entry_guard (A : TBA root) (l : nat) : guard root :=
  map (fun x =>
         match entry_lb A l x with
         | Some m => Some {| guard_clock := x; guard_comparison := CGe;
                             guard_bound := m |}
         | None => None
         end)
      (entry_clocks A l).

Definition forward_transition (A : TBA root) (t : tba_transition root)
    : tba_transition root :=
  {| bt_source := bt_source t;
     bt_label := bt_label t;
     bt_guard := bt_guard t ++ entry_guard A (bt_source t);
     bt_resets := bt_resets t;
     bt_target := bt_target t |}.

Definition forward (A : TBA root) : TBA root :=
  with_transitions A (map (forward_transition A) (tba_transitions A)).

Lemma forward_sound :
  forall A (rho : ext_word root),
    TBA_ext_accepts (forward A) rho -> TBA_ext_accepts A rho.
Proof.
  intros A rho.
  apply with_transitions_sound.
  intros t' Hin. apply in_map_iff in Hin. destruct Hin as [t [<- Hin]].
  exists t. split; [exact Hin|]. split; [reflexivity|]. split; [reflexivity|].
  intros i [Hlab [Hguard Hres]]. simpl in *.
  split; [exact Hlab|]. split; [|exact Hres].
  apply Forall_app in Hguard. exact (proj1 Hguard).
Qed.

Lemma forward_complete :
  forall A (rho : ext_word root),
    clock_consistent rho ->
    TBA_ext_accepts A rho -> TBA_ext_accepts (forward A) rho.
Proof.
  intros A rho Hcc [run [Hinit [Hsteps Hbuchi]]].
  assert (Hsteps' : forall i, exists t,
             In t (tba_transitions A) /\ bt_source t = run i /\
             bt_target t = run (S i) /\ tba_transition_enabled rho i t).
  { intro i. destruct (Hsteps i) as [_ H]. exact H. }
  apply with_transitions_complete with (run := run); try assumption.
  intro i.
  destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  exists (forward_transition A t).
  split; [apply in_map; exact Hin|].
  split; [exact Hsrc|]. split; [exact Hdst|].
  destruct Hen as [Hlab [Hguard Hres]].
  split; [exact Hlab|]. split; [|exact Hres].
  simpl. apply Forall_app. split; [exact Hguard|].
  apply Forall_forall. intros o Ho.
  unfold entry_guard in Ho. apply in_map_iff in Ho.
  destruct Ho as [x [<- _]].
  destruct (entry_lb A (bt_source t) x) as [m|] eqn:Hm; [|exact I].
  simpl. unfold clock_constraint_holds. simpl.
  rewrite Hsrc in Hm.
  pose proof (entry_lb_sound Hcc Hinit Hsteps' Hm (dl := 0)) as H.
  assert (H0 : 0 <= 0 <= stay rho i) by (split; [lra | apply stay_nonneg]).
  specialize (H H0). lra.
Qed.

(* ====================================================================== *)
(* 3. Two-sided invariants                                                *)
(* ====================================================================== *)

(* Timed Buchi automaton whose location invariants consist of upper bounds
   (synthesized by backward propagation) and of lower bounds (synthesized
   by forward propagation). *)
Record TBAIL : Type := {
  tbail_base : TBA root;
  tbail_up : nat -> invariant root;
  tbail_low : nat -> invariant root
}.

(* The lower-bound invariant holds during the whole stay ending at event i. *)
Definition low_during (rho : ext_word root) (i : nat) (L : invariant root)
    : Prop :=
  forall dl, 0 <= dl <= stay rho i ->
    forall x m, L x = Some m -> m <= ew_val rho i x - dl.

Definition TBAIL_ext_accepts (B : TBAIL) (rho : ext_word root) : Prop :=
  exists run : nat -> nat,
    run 0%nat = tba_init (tbail_base B) /\
    (forall i,
       (run i < tba_nstates (tbail_base B))%nat /\
       inv_during rho i (tbail_up B (run i)) /\
       low_during rho i (tbail_low B (run i)) /\
       exists t,
         In t (tba_transitions (tbail_base B)) /\
         bt_source t = run i /\
         bt_target t = run (S i) /\
         tba_transition_enabled rho i t) /\
    (forall n,
       exists j,
         (n <= j)%nat /\ In (run j) (tba_accepting (tbail_base B))).

Definition TBAIL_accepts (B : TBAIL) (w : timed_word) : Prop :=
  exists rho : ext_word root,
    same_base rho w /\
    clock_consistent rho /\
    TBAIL_ext_accepts B rho.

Definition add_two_sided_invariants (A : TBA root) : TBAIL :=
  {| tbail_base := A; tbail_up := synth_inv A; tbail_low := entry_lb A |}.

Theorem add_two_sided_invariants_accepts :
  forall (A : TBA root) w,
    TBA_accepts A w <-> TBAIL_accepts (add_two_sided_invariants A) w.
Proof.
  intros A w. split.
  - intros [rho [Hb [Hc [run [Hinit [Hsteps Hbuchi]]]]]].
    exists rho. split; [exact Hb|]. split; [exact Hc|].
    assert (Hsteps' : forall i, exists t,
               In t (tba_transitions A) /\ bt_source t = run i /\
               bt_target t = run (S i) /\ tba_transition_enabled rho i t).
    { intro i. destruct (Hsteps i) as [_ H]. exact H. }
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i.
    destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
    split; [exact Hbound|]. split.
    + apply inv_during_iff_at_event. simpl. rewrite <- Hsrc.
      exact (enabled_transition_satisfies_invariant Hin Hen).
    + split.
      * intros dl Hdl x m Hm. simpl in Hm.
        exact (entry_lb_sound Hc Hinit Hsteps' Hm Hdl).
      * exists t. split; [exact Hin|]. split; [exact Hsrc|].
        split; [exact Hdst | exact Hen].
  - intros [rho [Hb [Hc [run [Hinit [Hsteps Hbuchi]]]]]].
    exists rho. split; [exact Hb|]. split; [exact Hc|].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i.
    destruct (Hsteps i) as [Hbound [_ [_ Htrans]]].
    split; [exact Hbound | exact Htrans].
Qed.

(* ====================================================================== *)
(* 4. Simplification                                                      *)
(* ====================================================================== *)

(* Lower and upper bounds of a guard item, with their strictness. *)
Definition lower_info (o : option (clock_constraint root))
    : option (Clock root * R * bool) :=
  match o with
  | Some k =>
      match guard_comparison k with
      | CGe | CEq => Some (guard_clock k, guard_bound k, false)
      | CGt => Some (guard_clock k, guard_bound k, true)
      | _ => None
      end
  | None => None
  end.

Definition upper_info (o : option (clock_constraint root))
    : option (Clock root * R * bool) :=
  match o with
  | Some k =>
      match guard_comparison k with
      | CLe | CEq => Some (guard_clock k, guard_bound k, false)
      | CLt => Some (guard_clock k, guard_bound k, true)
      | _ => None
      end
  | None => None
  end.

(* l <(=) x <(=) u with u < l, or u = l and one comparison strict. *)
Definition contradictory_bounds (l : R) (sl : bool) (u : R) (su : bool) : bool :=
  if Rlt_dec u l then true
  else if Req_EM_T u l then sl || su else false.

Definition contra_pair (a b : option (clock_constraint root)) : bool :=
  match lower_info a, upper_info b with
  | Some (x, l, sl), Some (y, u, su) =>
      clock_eqb x y && contradictory_bounds l sl u su
  | _, _ => false
  end.

(* An upper bound that contradicts the nonnegativity of clocks. *)
Definition negative_upper (o : option (clock_constraint root)) : bool :=
  match upper_info o with
  | Some (_, u, su) => contradictory_bounds 0 false u su
  | None => false
  end.

Definition guard_contradictory (g : guard root) : bool :=
  existsb negative_upper g || existsb (fun a => existsb (contra_pair a) g) g.

Lemma lower_info_holds :
  forall (rho : ext_word root) i o x l sl,
    guard_item_holds rho i o ->
    lower_info o = Some (x, l, sl) ->
    if sl then l < ew_val rho i x else l <= ew_val rho i x.
Proof.
  intros rho i [k|] x l sl Hh Hinfo; simpl in *; [|discriminate].
  unfold clock_constraint_holds in Hh.
  destruct (guard_comparison k); try discriminate;
    injection Hinfo as <- <- <-; lra.
Qed.

Lemma upper_info_holds :
  forall (rho : ext_word root) i o x u su,
    guard_item_holds rho i o ->
    upper_info o = Some (x, u, su) ->
    if su then ew_val rho i x < u else ew_val rho i x <= u.
Proof.
  intros rho i [k|] x u su Hh Hinfo; simpl in *; [|discriminate].
  unfold clock_constraint_holds in Hh.
  destruct (guard_comparison k); try discriminate;
    injection Hinfo as <- <- <-; lra.
Qed.

Lemma contradictory_bounds_sound :
  forall l sl u su v,
    contradictory_bounds l sl u su = true ->
    (if sl then l < v else l <= v) ->
    (if su then v < u else v <= u) ->
    False.
Proof.
  intros l sl u su v Hc Hl Hu.
  unfold contradictory_bounds in Hc.
  destruct (Rlt_dec u l) as [Hlt|Hnlt].
  - destruct sl, su; lra.
  - destruct (Req_EM_T u l) as [Heq|Hneq]; [|discriminate].
    destruct sl, su; simpl in Hc; try discriminate; lra.
Qed.

Lemma guard_contradictory_sound :
  forall (rho : ext_word root) i g,
    clock_consistent rho ->
    guard_contradictory g = true ->
    ~ Forall (guard_item_holds rho i) g.
Proof.
  intros rho i g [Hnn _] Hc Hall.
  rewrite Forall_forall in Hall.
  unfold guard_contradictory in Hc.
  apply orb_true_iff in Hc. destruct Hc as [Hneg | Hpair].
  - apply existsb_exists in Hneg. destruct Hneg as [o [Ho Hn]].
    unfold negative_upper in Hn.
    destruct (upper_info o) as [[[x u] su]|] eqn:Hu; [|discriminate].
    pose proof (upper_info_holds (Hall o Ho) Hu) as Hv.
    apply (contradictory_bounds_sound (v := ew_val rho i x) Hn);
      [ exact (Hnn i x) | exact Hv ].
  - apply existsb_exists in Hpair. destruct Hpair as [a [Ha Hp]].
    apply existsb_exists in Hp. destruct Hp as [b' [Hb Hp]].
    unfold contra_pair in Hp.
    destruct (lower_info a) as [[[x l] sl]|] eqn:Hl; [|discriminate].
    destruct (upper_info b') as [[[y u] su]|] eqn:Hu; [|discriminate].
    apply andb_true_iff in Hp. destruct Hp as [Hxy Hp].
    apply clock_eqb_true in Hxy. subst y.
    exact (contradictory_bounds_sound Hp
             (lower_info_holds (Hall a Ha) Hl)
             (upper_info_holds (Hall b' Hb) Hu)).
Qed.

(* Removal of the transitions with contradictory guards. *)
Definition remove_contradictory (A : TBA root) : TBA root :=
  with_transitions A
    (filter (fun t => negb (guard_contradictory (bt_guard t)))
            (tba_transitions A)).

Lemma filter_sound :
  forall A (p : tba_transition root -> bool) (rho : ext_word root),
    TBA_ext_accepts (with_transitions A (filter p (tba_transitions A))) rho ->
    TBA_ext_accepts A rho.
Proof.
  intros A p rho.
  apply with_transitions_sound.
  intros t Hin. apply filter_In in Hin.
  exists t. split; [exact (proj1 Hin)|]. auto.
Qed.

Lemma remove_contradictory_complete :
  forall A (rho : ext_word root),
    clock_consistent rho ->
    TBA_ext_accepts A rho -> TBA_ext_accepts (remove_contradictory A) rho.
Proof.
  intros A rho Hcc [run [Hinit [Hsteps Hbuchi]]].
  apply with_transitions_complete with (run := run); try assumption.
  intro i.
  destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  exists t. split; [|split; [exact Hsrc|]; split; [exact Hdst | exact Hen]].
  apply filter_In. split; [exact Hin|].
  destruct (guard_contradictory (bt_guard t)) eqn:Hc; [|reflexivity].
  exfalso. destruct Hen as [_ [Hguard _]].
  exact (guard_contradictory_sound Hcc Hc Hguard).
Qed.

(* Removal of the locations, other than the initial one, without entering
   transition: their outgoing transitions are removed. *)
Definition has_incoming (A : TBA root) (l : nat) : bool :=
  existsb (fun t => Nat.eqb (bt_target t) l) (tba_transitions A).

Definition remove_unreachable (A : TBA root) : TBA root :=
  with_transitions A
    (filter (fun t => Nat.eqb (bt_source t) (tba_init A) ||
                      has_incoming A (bt_source t))
            (tba_transitions A)).

Lemma remove_unreachable_complete :
  forall A (rho : ext_word root),
    TBA_ext_accepts A rho -> TBA_ext_accepts (remove_unreachable A) rho.
Proof.
  intros A rho [run [Hinit [Hsteps Hbuchi]]].
  apply with_transitions_complete with (run := run); try assumption.
  intro i.
  destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  exists t. split; [|split; [exact Hsrc|]; split; [exact Hdst | exact Hen]].
  apply filter_In. split; [exact Hin|].
  apply orb_true_iff.
  destruct i as [|j].
  - left. rewrite Hsrc, Hinit. apply Nat.eqb_refl.
  - right. unfold has_incoming. apply existsb_exists.
    destruct (Hsteps j) as [_ [t' [Hin' [_ [Hdst' _]]]]].
    exists t'. split; [exact Hin'|].
    rewrite Hdst', Hsrc. apply Nat.eqb_refl.
Qed.

(* ====================================================================== *)
(* 5. Language preservation at the level of timed words                   *)
(* ====================================================================== *)

Lemma accepts_of_ext :
  forall (A B : TBA root),
    (forall rho, TBA_ext_accepts B rho -> TBA_ext_accepts A rho) ->
    (forall rho, clock_consistent rho ->
                 TBA_ext_accepts A rho -> TBA_ext_accepts B rho) ->
    forall w, TBA_accepts A w <-> TBA_accepts B w.
Proof.
  intros A B Hs Hc w. unfold TBA_accepts. split.
  - intros [rho [Hb [Hcc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hcc|]. exact (Hc rho Hcc Ha).
  - intros [rho [Hb [Hcc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hcc|]. exact (Hs rho Ha).
Qed.

Theorem forward_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (forward A) w.
Proof.
  intros A. apply accepts_of_ext.
  - apply forward_sound.
  - apply forward_complete.
Qed.

Theorem remove_contradictory_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (remove_contradictory A) w.
Proof.
  intros A. apply accepts_of_ext.
  - intro rho. apply filter_sound.
  - apply remove_contradictory_complete.
Qed.

Theorem remove_unreachable_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (remove_unreachable A) w.
Proof.
  intros A. apply accepts_of_ext.
  - intro rho. apply filter_sound.
  - intros rho _. apply remove_unreachable_complete.
Qed.

(* ====================================================================== *)
(* 6. Merging of synchronously reset clocks                               *)
(* ====================================================================== *)

Lemma clock_eqb_refl : forall x : Clock root, clock_eqb x x = true.
Proof.
  intro x. unfold clock_eqb.
  destruct (@clock_eq_dec root x x); [reflexivity|].
  exfalso. auto.
Qed.

Lemma clock_eqb_false :
  forall x y : Clock root, clock_eqb x y = false -> x <> y.
Proof.
  intros x y H ->. rewrite clock_eqb_refl in H. discriminate.
Qed.

(* Guards: every constraint on [x] becomes the same constraint on [y]. *)
Definition rename_item (x y : Clock root) (o : option (clock_constraint root))
    : option (clock_constraint root) :=
  match o with
  | Some k =>
      if clock_eqb (guard_clock k) x
      then Some {| guard_clock := y; guard_comparison := guard_comparison k;
                   guard_bound := guard_bound k |}
      else o
  | None => None
  end.

Definition merge_transition (x y : Clock root) (t : tba_transition root)
    : tba_transition root :=
  {| bt_source := bt_source t;
     bt_label := bt_label t;
     bt_guard := map (rename_item x y) (bt_guard t);
     bt_resets := bt_resets t;
     bt_target := bt_target t |}.

(* Clock [x] is replaced by clock [y] in all guards. *)
Definition merge_clock (x y : Clock root) (A : TBA root) : TBA root :=
  with_transitions A (map (merge_transition x y) (tba_transitions A)).

Definition mentions (x : Clock root) (o : option (clock_constraint root)) : bool :=
  match o with
  | Some k => clock_eqb (guard_clock k) x
  | None => false
  end.

(* [x] and [y] are reset synchronously, by every transition that leaves the
   initial location, and the initial location has no entering transition
   and no guard on [x]: after the first event they always hold the same
   value. *)
Definition mergeable (A : TBA root) (x y : Clock root) : Prop :=
  x <> y /\
  (forall t, In t (tba_transitions A) ->
             (In x (bt_resets t) <-> In y (bt_resets t))) /\
  (forall t, In t (tba_transitions A) -> bt_target t <> tba_init A) /\
  (forall t, In t (tba_transitions A) -> bt_source t = tba_init A ->
             In x (bt_resets t) /\ existsb (mentions x) (bt_guard t) = false).

(* The extended word in which clock [x] is a copy of clock [y]. *)
Definition copy_clock (x y : Clock root) (rho : ext_word root) : ext_word root :=
  {| ew_base := ew_base rho;
     ew_val := fun i c => if clock_eqb c x then ew_val rho i y else ew_val rho i c;
     ew_reset := fun i c => if clock_eqb c x then ew_reset rho i y
                            else ew_reset rho i c |}.

Lemma copy_clock_consistent :
  forall x y rho, clock_consistent rho -> clock_consistent (copy_clock x y rho).
Proof.
  intros x y rho [Hnn Hstep]. split.
  - intros i c. simpl. destruct (clock_eqb c x); apply Hnn.
  - intros i c. simpl. destruct (clock_eqb c x); apply Hstep.
Qed.

Lemma rename_item_copy :
  forall x y (rho : ext_word root) i o,
    guard_item_holds rho i (rename_item x y o) ->
    guard_item_holds (copy_clock x y rho) i o.
Proof.
  intros x y rho i [k|] H; [|exact I].
  simpl in *. unfold clock_constraint_holds in *. simpl in *.
  destruct (clock_eqb (guard_clock k) x) eqn:E; simpl in H; exact H.
Qed.

Lemma merge_sound :
  forall A x y (rho : ext_word root),
    mergeable A x y ->
    TBA_ext_accepts (merge_clock x y A) rho ->
    TBA_ext_accepts A (copy_clock x y rho).
Proof.
  intros A x y rho [_ [Hsync _]] [run [Hinit [Hsteps Hbuchi]]].
  exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
  intro i.
  destruct (Hsteps i) as [Hbound [tt [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  simpl in Hin. apply in_map_iff in Hin. destruct Hin as [t [<- Hin]].
  exists t. split; [exact Hin|]. split; [exact Hsrc|]. split; [exact Hdst|].
  destruct Hen as [Hlab [Hguard Hres]]. simpl in *.
  split; [exact Hlab|]. split.
  - rewrite Forall_forall in *. intros o Ho.
    apply rename_item_copy. apply Hguard. apply in_map. exact Ho.
  - intros c. simpl.
    destruct (clock_eqb c x) eqn:E.
    + apply clock_eqb_true in E. subst c.
      rewrite (Hres y). symmetry. apply (Hsync t Hin).
    + apply Hres.
Qed.

(* On a run, the merged clocks hold the same value from event 1 on. *)
Lemma mergeable_equal_values :
  forall A x y (rho : ext_word root) (run : nat -> nat),
    mergeable A x y ->
    clock_consistent rho ->
    run 0%nat = tba_init A ->
    (forall i, exists t,
        In t (tba_transitions A) /\
        bt_source t = run i /\ bt_target t = run (S i) /\
        tba_transition_enabled rho i t) ->
    forall i, ew_val rho (S i) x = ew_val rho (S i) y.
Proof.
  intros A x y rho run [_ [Hsync [_ Hinit_out]]] [_ Hstep] Hinit Hsteps i.
  induction i as [|i IH].
  - destruct (Hsteps 0%nat) as [t [Hin [Hsrc [_ [_ [_ Hres]]]]]].
    rewrite Hinit in Hsrc.
    destruct (Hinit_out t Hin Hsrc) as [HxZ _].
    assert (HyZ : In y (bt_resets t)) by (apply (Hsync t Hin); exact HxZ).
    rewrite (Hstep 0%nat x), (Hstep 0%nat y).
    rewrite (proj2 (Hres x) HxZ), (proj2 (Hres y) HyZ). reflexivity.
  - destruct (Hsteps (S i)) as [t [Hin [_ [_ [_ [_ Hres]]]]]].
    rewrite (Hstep (S i) x), (Hstep (S i) y), IH.
    destruct (ew_reset rho (S i) x) eqn:Ex;
      destruct (ew_reset rho (S i) y) eqn:Ey; try reflexivity.
    + exfalso. apply (proj1 (Hres x)) in Ex.
      apply (Hsync t Hin) in Ex. apply (proj2 (Hres y)) in Ex. congruence.
    + exfalso. apply (proj1 (Hres y)) in Ey.
      apply (Hsync t Hin) in Ey. apply (proj2 (Hres x)) in Ey. congruence.
Qed.

Lemma rename_item_holds :
  forall x y (rho : ext_word root) i o,
    (mentions x o = true -> ew_val rho i x = ew_val rho i y) ->
    guard_item_holds rho i o ->
    guard_item_holds rho i (rename_item x y o).
Proof.
  intros x y rho i [k|] Heq H; [|exact I].
  simpl in *. destruct (clock_eqb (guard_clock k) x) eqn:E; [|exact H].
  pose proof (Heq eq_refl) as Hxy.
  apply clock_eqb_true in E.
  unfold clock_constraint_holds in *. simpl in *.
  rewrite E, Hxy in H. exact H.
Qed.

Lemma merge_complete :
  forall A x y (rho : ext_word root),
    mergeable A x y ->
    clock_consistent rho ->
    TBA_ext_accepts A rho -> TBA_ext_accepts (merge_clock x y A) rho.
Proof.
  intros A x y rho Hm Hcc [run [Hinit [Hsteps Hbuchi]]].
  assert (Hsteps' : forall i, exists t,
             In t (tba_transitions A) /\ bt_source t = run i /\
             bt_target t = run (S i) /\ tba_transition_enabled rho i t).
  { intro i. destruct (Hsteps i) as [_ H]. exact H. }
  pose proof (mergeable_equal_values Hm Hcc Hinit Hsteps') as Hequal.
  destruct Hm as [_ [_ [_ Hinit_out]]].
  apply with_transitions_complete with (run := run); try assumption.
  intro i.
  destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  exists (merge_transition x y t).
  split; [apply in_map; exact Hin|].
  split; [exact Hsrc|]. split; [exact Hdst|].
  destruct Hen as [Hlab [Hguard Hres]].
  split; [exact Hlab|]. split; [|exact Hres].
  simpl. rewrite Forall_forall in *. intros o' Ho'.
  apply in_map_iff in Ho'. destruct Ho' as [o [<- Ho]].
  apply rename_item_holds; [|exact (Hguard o Ho)].
  intro Hmention.
  destruct i as [|j].
  - exfalso.
    rewrite Hinit in Hsrc.
    destruct (Hinit_out t Hin Hsrc) as [_ Hno].
    assert (Hyes : existsb (mentions x) (bt_guard t) = true)
      by (apply existsb_exists; exists o; split; assumption).
    congruence.
  - apply Hequal.
Qed.

Theorem merge_accepts :
  forall A x y w,
    mergeable A x y ->
    (TBA_accepts A w <-> TBA_accepts (merge_clock x y A) w).
Proof.
  intros A x y w Hm. unfold TBA_accepts. split.
  - intros [rho [Hb [Hcc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hcc|].
    exact (merge_complete Hm Hcc Ha).
  - intros [rho [Hb [Hcc Ha]]]. exists (copy_clock x y rho).
    split; [exact Hb|]. split; [exact (copy_clock_consistent x y Hcc)|].
    exact (merge_sound Hm Ha).
Qed.

(* Membership of a clock in a list, as a boolean. *)
Definition clock_in (x : Clock root) (l : list (Clock root)) : bool :=
  if In_dec (@clock_eq_dec root) x l then true else false.

Lemma clock_in_iff : forall x l, clock_in x l = true <-> In x l.
Proof.
  intros x l. unfold clock_in.
  destruct (In_dec (@clock_eq_dec root) x l); split; intro H;
    try reflexivity; try assumption; try discriminate; contradiction.
Qed.

(* Boolean check of the merging condition. *)
Definition mergeable_b (A : TBA root) (x y : Clock root) : bool :=
  (if @clock_eq_dec root x y then false else true) &&
  forallb (fun t => Bool.eqb (clock_in x (bt_resets t)) (clock_in y (bt_resets t)))
          (tba_transitions A) &&
  forallb (fun t => negb (Nat.eqb (bt_target t) (tba_init A))) (tba_transitions A) &&
  forallb (fun t => negb (Nat.eqb (bt_source t) (tba_init A)) ||
                    (clock_in x (bt_resets t) &&
                     negb (existsb (mentions x) (bt_guard t))))
          (tba_transitions A).

Lemma mergeable_b_sound :
  forall A x y, mergeable_b A x y = true -> mergeable A x y.
Proof.
  intros A x y H. unfold mergeable_b in H.
  apply andb_true_iff in H. destruct H as [H H4].
  apply andb_true_iff in H. destruct H as [H H3].
  apply andb_true_iff in H. destruct H as [H1 H2].
  rewrite forallb_forall in H2, H3, H4.
  split; [|split; [|split]].
  - destruct (@clock_eq_dec root x y); [discriminate | assumption].
  - intros t Ht. specialize (H2 t Ht). apply Bool.eqb_prop in H2.
    rewrite <- !clock_in_iff, H2. tauto.
  - intros t Ht Heq. specialize (H3 t Ht). rewrite Heq, Nat.eqb_refl in H3.
    discriminate.
  - intros t Ht Hs. specialize (H4 t Ht). rewrite Hs, Nat.eqb_refl in H4.
    simpl in H4. apply andb_true_iff in H4. destruct H4 as [Hx Hg].
    split; [apply clock_in_iff; exact Hx|].
    apply negb_true_iff. exact Hg.
Qed.

(* Merge [x] into [y] when the merging condition holds. *)
Definition try_merge (x y : Clock root) (A : TBA root) : TBA root :=
  if mergeable_b A x y then merge_clock x y A else A.

Theorem try_merge_accepts :
  forall x y A w, TBA_accepts A w <-> TBA_accepts (try_merge x y A) w.
Proof.
  intros x y A w. unfold try_merge.
  destruct (mergeable_b A x y) eqn:E.
  - apply merge_accepts. apply mergeable_b_sound. exact E.
  - reflexivity.
Qed.

(* All pairs of reset clocks are tried, in order. *)
Fixpoint merge_pairs (ps : list (Clock root * Clock root)) (A : TBA root)
    : TBA root :=
  match ps with
  | [] => A
  | (x, y) :: ps' => merge_pairs ps' (try_merge x y A)
  end.

Definition reset_clocks (A : TBA root) : list (Clock root) :=
  flat_map (fun t => bt_resets t) (tba_transitions A).

Definition merge_all (A : TBA root) : TBA root :=
  merge_pairs (list_prod (reset_clocks A) (reset_clocks A)) A.

Lemma merge_pairs_accepts :
  forall ps A w, TBA_accepts A w <-> TBA_accepts (merge_pairs ps A) w.
Proof.
  induction ps as [|[x y] ps IH]; intros A w; simpl; [reflexivity|].
  rewrite (try_merge_accepts x y A w). apply IH.
Qed.

Theorem merge_all_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (merge_all A) w.
Proof.
  intros A w. apply merge_pairs_accepts.
Qed.

(* ====================================================================== *)
(* 6b. Removal of useless resets                                          *)
(* ====================================================================== *)

(* A transition tests clock [x] when its guard mentions [x]. *)
Definition tests (x : Clock root) (t : tba_transition root) : bool :=
  existsb (mentions x) (bt_guard t).

(* Locations from which a transition testing [x] may still be taken:
   backward closure of the sources of the transitions testing [x]. *)
Definition live_step (A : TBA root) (x : Clock root) (L : list nat) : list nat :=
  L ++ map (fun t => bt_source t)
           (filter (fun t => tests x t ||
                             existsb (Nat.eqb (bt_target t)) L)
                   (tba_transitions A)).

Fixpoint live_iter (n : nat) (A : TBA root) (x : Clock root) (L : list nat)
    : list nat :=
  match n with
  | O => L
  | S n' => live_iter n' A x (live_step A x L)
  end.

Definition live (A : TBA root) (x : Clock root) : list nat :=
  live_iter (S (length (tba_transitions A))) A x [].

(* [x] is dead in [l]: no transition testing [x] can be taken from [l]. *)
Definition dead (A : TBA root) (x : Clock root) (l : nat) : bool :=
  negb (existsb (Nat.eqb l) (live A x)).

(* Check of the two properties used by the proof: the dead locations are
   closed under successors, and no transition leaving them tests [x]. *)
Definition dead_ok (A : TBA root) (x : Clock root) : bool :=
  forallb (fun t => negb (dead A x (bt_source t)) ||
                    (dead A x (bt_target t) && negb (tests x t)))
          (tba_transitions A).

Definition drop_reset (x : Clock root) (t : tba_transition root)
    : tba_transition root :=
  {| bt_source := bt_source t;
     bt_label := bt_label t;
     bt_guard := bt_guard t;
     bt_resets := filter (fun c => negb (clock_eqb c x)) (bt_resets t);
     bt_target := bt_target t |}.

Definition prune (A : TBA root) (x : Clock root) (t : tba_transition root)
    : tba_transition root :=
  if dead A x (bt_target t) then drop_reset x t else t.

(* The reset of [x] is removed from every transition entering a location
   where [x] is dead. *)
Definition remove_dead_resets_clock (x : Clock root) (A : TBA root) : TBA root :=
  if dead_ok A x
  then with_transitions A (map (prune A x) (tba_transitions A))
  else A.

Lemma in_drop :
  forall (x c : Clock root) (Z : list (Clock root)),
    In c (filter (fun c => negb (clock_eqb c x)) Z) <-> In c Z /\ c <> x.
Proof.
  intros x c Z. rewrite filter_In. split.
  - intros [Hin Hneq]. split; [exact Hin|].
    intros ->. rewrite clock_eqb_refl in Hneq. discriminate.
  - intros [Hin Hneq]. split; [exact Hin|].
    destruct (clock_eqb c x) eqn:E; [|reflexivity].
    apply clock_eqb_true in E. contradiction.
Qed.

(* Extended word whose clock [x] is reset according to [r] and otherwise
   evolves by the clock recurrence; the other clocks are those of [rho]. *)
Fixpoint xval (rho : ext_word root) (x : Clock root) (r : nat -> bool) (i : nat)
    : R :=
  match i with
  | O => ew_val rho 0 x
  | S j => if r j then delta (ew_base rho) j
           else xval rho x r j + delta (ew_base rho) j
  end.

Definition reset_x (x : Clock root) (r : nat -> bool) (rho : ext_word root)
    : ext_word root :=
  {| ew_base := ew_base rho;
     ew_val := fun i c => if clock_eqb c x then xval rho x r i
                          else ew_val rho i c;
     ew_reset := fun i c => if clock_eqb c x then r i else ew_reset rho i c |}.

Lemma xval_nonneg :
  forall rho x r i,
    clock_consistent rho -> 0 <= xval rho x r i.
Proof.
  intros rho x r i [Hnn _].
  induction i as [|j IH]; simpl; [apply Hnn|].
  pose proof (delta_positive (ew_base rho) j).
  destruct (r j); lra.
Qed.

Lemma reset_x_consistent :
  forall x r rho, clock_consistent rho -> clock_consistent (reset_x x r rho).
Proof.
  intros x r rho Hcc.
  pose proof Hcc as [Hnn Hstep]. split.
  - intros i c. simpl. destruct (clock_eqb c x).
    + apply xval_nonneg. exact Hcc.
    + apply Hnn.
  - intros i c. simpl. destruct (clock_eqb c x); [reflexivity | apply Hstep].
Qed.

(* If [r] agrees with the resets of [rho] on [x] before [i], the value of
   [x] at [i] is unchanged. *)
Lemma xval_agree :
  forall rho x r i,
    clock_consistent rho ->
    (forall j, (j < i)%nat -> r j = ew_reset rho j x) ->
    xval rho x r i = ew_val rho i x.
Proof.
  intros rho x r i [_ Hstep] Hagree.
  induction i as [|j IH]; simpl; [reflexivity|].
  rewrite (Hagree j ltac:(lia)), (Hstep j x).
  rewrite IH; [reflexivity|]. intros k Hk. apply Hagree. lia.
Qed.

(* Along a run, dead locations are never left. *)
Lemma dead_forward :
  forall A x (run : nat -> nat) (rho : ext_word root),
    dead_ok A x = true ->
    (forall i, exists t,
        In t (tba_transitions A) /\
        bt_source t = run i /\ bt_target t = run (S i)) ->
    forall i j, (i <= j)%nat -> dead A x (run i) = true -> dead A x (run j) = true.
Proof.
  intros A x run rho Hok Hsteps i j Hij Hd.
  induction Hij as [|j Hij IH]; [exact Hd|].
  destruct (Hsteps j) as [t [Hin [Hsrc Hdst]]].
  unfold dead_ok in Hok. rewrite forallb_forall in Hok.
  specialize (Hok t Hin). rewrite Hsrc, IH in Hok. simpl in Hok.
  apply andb_true_iff in Hok. rewrite <- Hdst. exact (proj1 Hok).
Qed.

Lemma dead_no_test :
  forall A x t,
    dead_ok A x = true -> In t (tba_transitions A) ->
    dead A x (bt_source t) = true -> tests x t = false.
Proof.
  intros A x t Hok Hin Hd.
  unfold dead_ok in Hok. rewrite forallb_forall in Hok.
  specialize (Hok t Hin). rewrite Hd in Hok. simpl in Hok.
  apply andb_true_iff in Hok. destruct Hok as [_ H].
  destruct (tests x t); [discriminate | reflexivity].
Qed.

(* Guard items do not depend on the values of clocks they do not mention. *)
Lemma guard_holds_change_x :
  forall (rho rho' : ext_word root) i x g,
    (forall c, c <> x -> ew_val rho' i c = ew_val rho i c) ->
    (existsb (mentions x) g = true -> ew_val rho' i x = ew_val rho i x) ->
    Forall (guard_item_holds rho i) g ->
    Forall (guard_item_holds rho' i) g.
Proof.
  intros rho rho' i x g Hother Hx Hall.
  rewrite Forall_forall in *. intros o Ho.
  specialize (Hall o Ho).
  destruct o as [k|]; [|exact I].
  simpl in *. unfold clock_constraint_holds in *.
  destruct (clock_eqb (guard_clock k) x) eqn:E.
  - apply clock_eqb_true in E.
    assert (Hm : existsb (mentions x) g = true).
    { apply existsb_exists. exists (Some k). split; [exact Ho|].
      simpl. rewrite E. apply clock_eqb_refl. }
    rewrite E in *. rewrite (Hx Hm). exact Hall.
  - apply clock_eqb_false in E.
    rewrite (Hother _ E). exact Hall.
Qed.

(* Before a live location of a run, all entered locations are live. *)
Lemma live_before :
  forall A x (run : nat -> nat) (rho : ext_word root),
    dead_ok A x = true ->
    (forall i, exists t,
        In t (tba_transitions A) /\
        bt_source t = run i /\ bt_target t = run (S i)) ->
    forall i, dead A x (run i) = false ->
    forall j, (j < i)%nat -> dead A x (run (S j)) = false.
Proof.
  intros A x run rho Hok Hsteps i Hi j Hj.
  destruct (dead A x (run (S j))) eqn:E; [|reflexivity].
  rewrite (@dead_forward A x run rho Hok Hsteps (S j) i ltac:(lia) E) in Hi.
  discriminate.
Qed.

Lemma prune_target :
  forall A x t, bt_target (prune A x t) = bt_target t.
Proof. intros. unfold prune. destruct (dead A x (bt_target t)); reflexivity. Qed.

Lemma prune_source :
  forall A x t, bt_source (prune A x t) = bt_source t.
Proof. intros. unfold prune. destruct (dead A x (bt_target t)); reflexivity. Qed.

Lemma prune_guard :
  forall A x t, bt_guard (prune A x t) = bt_guard t.
Proof. intros. unfold prune. destruct (dead A x (bt_target t)); reflexivity. Qed.

Lemma prune_label :
  forall A x t, bt_label (prune A x t) = bt_label t.
Proof. intros. unfold prune. destruct (dead A x (bt_target t)); reflexivity. Qed.

(* Resets of a pruned transition: those of the original one, except [x]
   when its target is dead. *)
Lemma prune_resets :
  forall A x t c,
    In c (bt_resets (prune A x t)) <->
    In c (bt_resets t) /\ (dead A x (bt_target t) = true -> c <> x).
Proof.
  intros A x t c. unfold prune.
  destruct (dead A x (bt_target t)) eqn:E; simpl.
  - rewrite in_drop. split; intros [H1 H2]; split; auto.
  - split; [intro H; split; [exact H | discriminate] | tauto].
Qed.

Theorem remove_dead_resets_clock_accepts :
  forall x A w,
    TBA_accepts A w <-> TBA_accepts (remove_dead_resets_clock x A) w.
Proof.
  intros x A w. unfold remove_dead_resets_clock.
  destruct (dead_ok A x) eqn:Hok; [|reflexivity].
  unfold TBA_accepts. split.
  - (* A -> pruned A *)
    intros [rho [Hb [Hcc [run [Hinit [Hsteps Hbuchi]]]]]].
    assert (Hpath : forall i, exists t,
               In t (tba_transitions A) /\ bt_source t = run i /\
               bt_target t = run (S i)).
    { intro i. destruct (Hsteps i) as [_ [t [Hin [Hs [Hd _]]]]].
      exists t. auto. }
    set (r := fun i => if dead A x (run (S i)) then false else ew_reset rho i x).
    exists (reset_x x r rho).
    split; [exact Hb|]. split; [exact (reset_x_consistent x r Hcc)|].
    apply with_transitions_complete with (run := run); try assumption.
    intro i.
    destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst [Hlab [Hguard Hres]]]]]]].
    split; [exact Hbound|].
    exists (prune A x t).
    split; [apply in_map; exact Hin|].
    rewrite prune_source, prune_target.
    split; [exact Hsrc|]. split; [exact Hdst|].
    split; [rewrite prune_label; exact Hlab|].
    split.
    + rewrite prune_guard.
      apply (guard_holds_change_x (rho := rho) (x := x)); [| |exact Hguard].
      * intros c Hc. simpl.
        destruct (clock_eqb c x) eqn:E; [apply clock_eqb_true in E; contradiction|].
        reflexivity.
      * intro Htest. simpl. rewrite clock_eqb_refl.
        apply xval_agree; [exact Hcc|].
        intros j Hj. unfold r.
        assert (Hlive : dead A x (run i) = false).
        { destruct (dead A x (run i)) eqn:E; [|reflexivity].
          rewrite <- Hsrc in E.
          pose proof (dead_no_test Hok Hin E) as Hnt. unfold tests in Hnt.
          rewrite Hnt in Htest. discriminate. }
        rewrite (@live_before A x run rho Hok Hpath i Hlive j Hj). reflexivity.
    + intro c. simpl. rewrite prune_resets.
      destruct (clock_eqb c x) eqn:E.
      * apply clock_eqb_true in E. subst c. unfold r. rewrite <- Hdst.
        destruct (dead A x (bt_target t)) eqn:Ed.
        -- split; [discriminate|]. intros [_ H]. exfalso. exact (H eq_refl eq_refl).
        -- rewrite (Hres x). split; [intro H; split; [exact H | discriminate] | tauto].
      * apply clock_eqb_false in E.
        rewrite (Hres c). split; [intro H; split; [exact H | intros _ Heq; exact (E Heq)] | tauto].
  - (* pruned A -> A *)
    intros [rho [Hb [Hcc [run [Hinit [Hsteps Hbuchi]]]]]].
    simpl in Hinit, Hbuchi.
    assert (Hpath : forall i, exists t,
               In t (tba_transitions A) /\ bt_source t = run i /\
               bt_target t = run (S i)).
    { intro i. destruct (Hsteps i) as [_ [tt [Hin [Hsrc [Hdst _]]]]].
      simpl in Hin. apply in_map_iff in Hin. destruct Hin as [t [<- Hin]].
      exists t. rewrite prune_source, prune_target in *. auto. }
    set (P := fun i => exists t, In t (tba_transitions A) /\
                bt_source t = run i /\ bt_target t = run (S i) /\
                tba_transition_enabled rho i (prune A x t) /\
                In x (bt_resets t)).
    set (r := fun i => if dead A x (run (S i))
                       then decide_b (P i)
                       else ew_reset rho i x).
    exists (reset_x x r rho).
    split; [exact Hb|]. split; [exact (reset_x_consistent x r Hcc)|].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i.
    destruct (Hsteps i) as [Hbound [tt [Hin [Hsrc [Hdst Hen]]]]].
    simpl in Hin. apply in_map_iff in Hin. destruct Hin as [t0 [<- Hin0]].
    rewrite prune_source in Hsrc. rewrite prune_target in Hdst.
    split; [exact Hbound|].
    (* the value of x is unchanged where a transition testing x is taken *)
    assert (Hval : forall t, In t (tba_transitions A) -> bt_source t = run i ->
                   tests x t = true -> xval rho x r i = ew_val rho i x).
    { intros t Ht Hs Htest. apply xval_agree; [exact Hcc|].
      intros j Hj.
      assert (Hlive : dead A x (run i) = false).
      { destruct (dead A x (run i)) eqn:E; [|reflexivity].
        rewrite <- Hs in E. rewrite (dead_no_test Hok Ht E) in Htest.
        discriminate. }
      unfold r. rewrite (@live_before A x run rho Hok Hpath i Hlive j Hj).
      reflexivity. }
    assert (Hguard_of : forall t, In t (tba_transitions A) -> bt_source t = run i ->
              Forall (guard_item_holds rho i) (bt_guard t) ->
              Forall (guard_item_holds (reset_x x r rho) i) (bt_guard t)).
    { intros t Ht Hs Hg.
      apply (guard_holds_change_x (rho := rho) (x := x)); [| |exact Hg].
      - intros c Hc. simpl.
        destruct (clock_eqb c x) eqn:E;
          [apply clock_eqb_true in E; contradiction | reflexivity].
      - intro Htest. simpl. rewrite clock_eqb_refl.
        exact (Hval t Ht Hs Htest). }
    destruct (dead A x (run (S i))) eqn:Hd.
    + destruct (classic (P i)) as [HP|HnP].
      * assert (HPi := HP). destruct HP as [t [Ht [Hs [Htg [[Hlab [Hguard Hres]] HxZ]]]]].
        exists t. split; [exact Ht|]. split; [exact Hs|]. split; [exact Htg|].
        rewrite prune_label in Hlab. rewrite prune_guard in Hguard.
        split; [exact Hlab|]. split; [exact (Hguard_of t Ht Hs Hguard)|].
        intro c. simpl. destruct (clock_eqb c x) eqn:E.
        -- apply clock_eqb_true in E. subst c. unfold r. rewrite Hd.
           rewrite (proj2 (decide_b_spec (P i)) HPi).
           split; [intros _; exact HxZ | reflexivity].
        -- apply clock_eqb_false in E.
           rewrite (Hres c), prune_resets.
           split; [intros [H _]; exact H | intro H; split; [exact H | intros _; exact E]].
      * destruct Hen as [Hlab [Hguard Hres]].
        exists t0. split; [exact Hin0|]. split; [exact Hsrc|]. split; [exact Hdst|].
        rewrite prune_label in Hlab. rewrite prune_guard in Hguard.
        split; [exact Hlab|]. split; [exact (Hguard_of t0 Hin0 Hsrc Hguard)|].
        intro c. simpl. destruct (clock_eqb c x) eqn:E.
        -- apply clock_eqb_true in E. subst c. unfold r. rewrite Hd.
           destruct (decide_b (P i)) eqn:Ed; [exfalso; exact (HnP (proj1 (decide_b_spec (P i)) Ed))|].
           split; [discriminate|]. intro HxZ. exfalso. apply HnP.
           exists t0. split; [exact Hin0|]. split; [exact Hsrc|].
           split; [exact Hdst|]. split; [|exact HxZ].
           split; [rewrite prune_label; exact Hlab|].
           split; [rewrite prune_guard; exact Hguard | exact Hres].
        -- apply clock_eqb_false in E.
           rewrite (Hres c), prune_resets.
           split; [intros [H _]; exact H | intro H; split; [exact H | intros _; exact E]].
    + destruct Hen as [Hlab [Hguard Hres]].
      exists t0. split; [exact Hin0|]. split; [exact Hsrc|]. split; [exact Hdst|].
      rewrite prune_label in Hlab. rewrite prune_guard in Hguard.
      split; [exact Hlab|]. split; [exact (Hguard_of t0 Hin0 Hsrc Hguard)|].
      intro c. simpl.
      assert (Hnd : dead A x (bt_target t0) = false) by (rewrite Hdst; exact Hd).
      destruct (clock_eqb c x) eqn:E.
      -- apply clock_eqb_true in E. subst c. unfold r. rewrite Hd.
         rewrite (Hres x), prune_resets, Hnd.
         split; [intros [H _]; exact H | intro H; split; [exact H | discriminate]].
      -- rewrite (Hres c), prune_resets, Hnd.
         split; [intros [H _]; exact H | intro H; split; [exact H | discriminate]].
Qed.

(* All reset clocks are processed in turn. *)
Fixpoint remove_dead_resets_list (xs : list (Clock root)) (A : TBA root)
    : TBA root :=
  match xs with
  | [] => A
  | x :: xs' => remove_dead_resets_list xs' (remove_dead_resets_clock x A)
  end.

Definition remove_dead_resets (A : TBA root) : TBA root :=
  remove_dead_resets_list (reset_clocks A) A.

Theorem remove_dead_resets_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (remove_dead_resets A) w.
Proof.
  intros A w. unfold remove_dead_resets.
  generalize (reset_clocks A) as xs. intro xs.
  revert A. induction xs as [|x xs IH]; intro A; simpl; [reflexivity|].
  rewrite (remove_dead_resets_clock_accepts x A w). apply IH.
Qed.



(* ====================================================================== *)
(* 6c. Normalization of guards                                            *)
(* ====================================================================== *)

(* The passes above only add constraints to guards, which accumulate
   duplicates and implied constraints.  Normalization drops the empty items,
   the lower bounds that every clock value satisfies (x >= m with m <= 0,
   x > m with m < 0), and every constraint implied by another constraint of
   the same guard on the same clock. *)

Definition dec_b {P Q : Prop} (d : {P} + {Q}) : bool := if d then true else false.

Lemma dec_b_true : forall (P Q : Prop) (d : {P} + {Q}), dec_b d = true -> P.
Proof. intros P Q [p|q] H; [exact p | discriminate]. Qed.

(* Satisfaction of a clock constraint by a clock valuation. *)
Definition cc_holds (v : Clock root -> R) (k : clock_constraint root) : Prop :=
  match guard_comparison k with
  | CLe => v (guard_clock k) <= guard_bound k
  | CLt => v (guard_clock k) < guard_bound k
  | CGe => guard_bound k <= v (guard_clock k)
  | CGt => guard_bound k < v (guard_clock k)
  | CEq => v (guard_clock k) = guard_bound k
  end.

Lemma cc_holds_at :
  forall (rho : ext_word root) i k,
    clock_constraint_holds rho i k <-> cc_holds (fun x => ew_val rho i x) k.
Proof. intros rho i k. unfold clock_constraint_holds, cc_holds. tauto. Qed.

(* [implies_c a b = true]: the constraint [a] implies the constraint [b]. *)
Definition implies_c (a b : clock_constraint root) : bool :=
  let u := guard_bound a in
  let w := guard_bound b in
  clock_eqb (guard_clock a) (guard_clock b) &&
  match guard_comparison b with
  | CLe => match guard_comparison a with
           | CLe | CLt | CEq => dec_b (Rle_dec u w)
           | _ => false
           end
  | CLt => match guard_comparison a with
           | CLe | CEq => dec_b (Rlt_dec u w)
           | CLt => dec_b (Rle_dec u w)
           | _ => false
           end
  | CGe => match guard_comparison a with
           | CGe | CGt | CEq => dec_b (Rle_dec w u)
           | _ => false
           end
  | CGt => match guard_comparison a with
           | CGe | CEq => dec_b (Rlt_dec w u)
           | CGt => dec_b (Rle_dec w u)
           | _ => false
           end
  | CEq => match guard_comparison a with
           | CEq => dec_b (Req_EM_T u w)
           | _ => false
           end
  end.

Lemma implies_c_sound :
  forall v a b, implies_c a b = true -> cc_holds v a -> cc_holds v b.
Proof.
  intros v [xa ca ua] [xb cb ub] Himp Ha.
  unfold implies_c, cc_holds in *. simpl in *.
  apply andb_true_iff in Himp. destruct Himp as [Hx Hc].
  apply clock_eqb_true in Hx. subst xb.
  destruct cb, ca; try discriminate; apply dec_b_true in Hc; lra.
Qed.

(* Lower bounds that every (nonnegative) clock value satisfies. *)
Definition trivial_c (k : clock_constraint root) : bool :=
  match guard_comparison k with
  | CGe => dec_b (Rle_dec (guard_bound k) 0)
  | CGt => dec_b (Rlt_dec (guard_bound k) 0)
  | _ => false
  end.

Lemma trivial_c_sound :
  forall v k, (forall x, 0 <= v x) -> trivial_c k = true -> cc_holds v k.
Proof.
  intros v [x c b] Hnn H. unfold trivial_c, cc_holds in *. simpl in *.
  specialize (Hnn x).
  destruct c; try discriminate; apply dec_b_true in H; lra.
Qed.

Definition insert_c (c : clock_constraint root) (acc : list (clock_constraint root))
    : list (clock_constraint root) :=
  if trivial_c c then acc
  else if existsb (fun a => implies_c a c) acc then acc
  else c :: filter (fun a => negb (implies_c c a)) acc.

Fixpoint present (g : guard root) : list (clock_constraint root) :=
  match g with
  | [] => []
  | Some k :: g' => k :: present g'
  | None :: g' => present g'
  end.

Definition norm_guard (g : guard root) : guard root :=
  map Some (fold_right insert_c [] (present g)).

Lemma insert_c_holds :
  forall v c acc,
    (forall x, 0 <= v x) ->
    (Forall (cc_holds v) (insert_c c acc) <->
     cc_holds v c /\ Forall (cc_holds v) acc).
Proof.
  intros v c acc Hnn. unfold insert_c.
  destruct (trivial_c c) eqn:Ht.
  - pose proof (trivial_c_sound Hnn Ht). tauto.
  - destruct (existsb (fun a => implies_c a c) acc) eqn:He.
    + apply existsb_exists in He. destruct He as [a [Ha Hac]].
      split; [|tauto]. intro H. split; [|exact H].
      rewrite Forall_forall in H. exact (implies_c_sound Hac (H a Ha)).
    + rewrite Forall_cons_iff. split.
      * intros [Hc Hf]. split; [exact Hc|].
        rewrite Forall_forall in *. intros a Ha.
        destruct (implies_c c a) eqn:E.
        -- exact (implies_c_sound E Hc).
        -- apply Hf. apply filter_In. split; [exact Ha|]. rewrite E. reflexivity.
      * intros [Hc Hf]. split; [exact Hc|].
        rewrite Forall_forall in *. intros a Ha.
        apply filter_In in Ha. apply Hf. tauto.
Qed.

Lemma fold_insert_holds :
  forall v l,
    (forall x, 0 <= v x) ->
    (Forall (cc_holds v) (fold_right insert_c [] l) <-> Forall (cc_holds v) l).
Proof.
  intros v l Hnn. induction l as [|c l IH]; simpl; [tauto|].
  rewrite (insert_c_holds c _ Hnn), IH, Forall_cons_iff. tauto.
Qed.

Lemma present_holds :
  forall (rho : ext_word root) i g,
    Forall (guard_item_holds rho i) g <->
    Forall (cc_holds (fun x => ew_val rho i x)) (present g).
Proof.
  intros rho i g. induction g as [|[k|] g IH]; simpl.
  - split; intros _; constructor.
  - rewrite !Forall_cons_iff, IH. simpl. rewrite cc_holds_at. tauto.
  - rewrite Forall_cons_iff, IH. simpl. tauto.
Qed.

Lemma norm_guard_holds :
  forall (rho : ext_word root) i g,
    (forall x, 0 <= ew_val rho i x) ->
    (Forall (guard_item_holds rho i) (norm_guard g) <->
     Forall (guard_item_holds rho i) g).
Proof.
  intros rho i g Hnn. unfold norm_guard.
  transitivity (Forall (cc_holds (fun x => ew_val rho i x))
                       (fold_right insert_c [] (present g))).
  - rewrite Forall_map.
    split; intro H; eapply Forall_impl; try exact H;
      intros k Hk; simpl in *; apply cc_holds_at; exact Hk.
  - rewrite (@fold_insert_holds (fun x => ew_val rho i x) (present g) Hnn).
    symmetry. apply present_holds.
Qed.

Definition normalize_transition (t : tba_transition root) : tba_transition root :=
  {| bt_source := bt_source t;
     bt_label := bt_label t;
     bt_guard := norm_guard (bt_guard t);
     bt_resets := bt_resets t;
     bt_target := bt_target t |}.

Definition normalize (A : TBA root) : TBA root :=
  with_transitions A (map normalize_transition (tba_transitions A)).

Lemma normalize_enabled :
  forall (rho : ext_word root) i t,
    clock_consistent rho ->
    (tba_transition_enabled rho i (normalize_transition t) <->
     tba_transition_enabled rho i t).
Proof.
  intros rho i t [Hnn _].
  unfold tba_transition_enabled, normalize_transition. simpl.
  rewrite (norm_guard_holds (bt_guard t) (Hnn i)). tauto.
Qed.

Theorem normalize_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (normalize A) w.
Proof.
  intros A w. unfold TBA_accepts. split.
  - intros [rho [Hb [Hcc Ha]]]. exists rho. split; [exact Hb|]. split; [exact Hcc|].
    destruct Ha as [run [Hinit [Hsteps Hbuchi]]].
    apply with_transitions_complete with (run := run); try assumption.
    intro i. destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
    split; [exact Hbound|].
    exists (normalize_transition t). split; [apply in_map; exact Hin|].
    split; [exact Hsrc|]. split; [exact Hdst|].
    apply (normalize_enabled i t Hcc). exact Hen.
  - intros [rho [Hb [Hcc Ha]]]. exists rho. split; [exact Hb|]. split; [exact Hcc|].
    revert Ha. apply with_transitions_sound.
    intros t' Hin. apply in_map_iff in Hin. destruct Hin as [t [<- Hin]].
    exists t. split; [exact Hin|]. split; [reflexivity|]. split; [reflexivity|].
    intros i Hen. apply (normalize_enabled i t Hcc). exact Hen.
Qed.

(* ====================================================================== *)
(* 7. The optimization pipeline, iterated                                 *)
(* ====================================================================== *)

(* One round: backward propagation, forward propagation, simplification,
   normalization of the guards (which removes the trivial lower bounds
   x >= 0 added by the forward step, so that they do not keep clocks live),
   removal of useless resets, merging of synchronously reset clocks, and a
   final normalization. *)
Definition optimize_step (A : TBA root) : TBA root :=
  normalize
    (merge_all
       (remove_dead_resets
          (normalize
             (remove_unreachable (remove_contradictory (forward (propagate A))))))).

Fixpoint optimize (n : nat) (A : TBA root) : TBA root :=
  match n with
  | O => A
  | S n' => optimize n' (optimize_step A)
  end.

Theorem optimize_step_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (optimize_step A) w.
Proof.
  intros A w. unfold optimize_step.
  rewrite (propagate_accepts A w).
  rewrite (forward_accepts (propagate A) w).
  rewrite (remove_contradictory_accepts (forward (propagate A)) w).
  rewrite (remove_unreachable_accepts
             (remove_contradictory (forward (propagate A))) w).
  rewrite (normalize_accepts
             (remove_unreachable (remove_contradictory (forward (propagate A)))) w).
  rewrite (remove_dead_resets_accepts
             (normalize
                (remove_unreachable (remove_contradictory (forward (propagate A))))) w).
  rewrite merge_all_accepts.
  apply normalize_accepts.
Qed.

Theorem optimize_accepts :
  forall n A w, TBA_accepts A w <-> TBA_accepts (optimize n A) w.
Proof.
  induction n as [|n IH]; intros A w; simpl; [reflexivity|].
  rewrite (optimize_step_accepts A w). apply IH.
Qed.

(* The optimized automaton with its two-sided invariants.  The lower-bound
   invariants are not accepted by UPPAAL; they serve to identify infeasible
   transitions (through [forward] and [remove_contradictory]) and are not
   part of the exported automaton (MTL_to_TBA_Export.v). *)
Definition optimized (n : nat) (A : TBA root) : TBAIL :=
  add_two_sided_invariants (optimize n A).

Theorem optimized_accepts :
  forall n A w, TBA_accepts A w <-> TBAIL_accepts (optimized n A) w.
Proof.
  intros n A w.
  rewrite (optimize_accepts n A w).
  apply add_two_sided_invariants_accepts.
Qed.

End Optimizations.

(* ====================================================================== *)
(* 8. End-to-end correctness of the optimized automaton                   *)
(* ====================================================================== *)

Definition compile_optimized (n : nat) (f : mtl) : TBA f :=
  optimize n (compile f).

Theorem MTL_to_optimized_TBA_correct :
  forall (n : nat) (f : mtl) (w : timed_word),
    well_formed f ->
    (msat w 0 f <-> TBA_accepts (compile_optimized n f) w).
Proof.
  intros n f w Hwf.
  rewrite (MTL_to_TBA_correct w Hwf).
  apply optimize_accepts.
Qed.

Print Assumptions MTL_to_optimized_TBA_correct.
