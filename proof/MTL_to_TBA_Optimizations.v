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

Require Import Arith Lia List Bool Reals Lra.
Require Import Classical ClassicalDescription.
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
  nodup (@clock_eq_dec root)
    (flat_map (fun t => bt_resets t ++ flat_map lower_clocks (bt_guard t))
              (incoming A l)).

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
     bt_guard := conj_guard (bt_guard t) (entry_guard A (bt_source t));
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
  apply conj_guard_holds in Hguard. exact (proj1 Hguard).
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
  simpl. apply conj_guard_holds. split; [exact Hguard|].
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

(* Removal of the transitions whose action label is unsatisfiable: a label
   requiring two different actions, or an action and its negation, is never
   satisfied by the single action of a position.  Such labels arise because
   the LTL-to-Buchi back-end treats the atoms a and !a as independent
   propositions. *)
Definition label_sat (l : list alit) : bool :=
  match find (fun p : alit => snd p) l with
  | Some (a, _) =>
      forallb (fun p : alit => if snd p then Nat.eqb (fst p) a
                               else negb (Nat.eqb (fst p) a)) l
  | None => true
  end.

Lemma label_sat_holds :
  forall l le, label_holds l le -> label_sat l = true.
Proof.
  intros l le H. unfold label_holds in H. rewrite Forall_forall in H.
  unfold label_sat. destruct (find (fun p : alit => snd p) l) as [[a b]|] eqn:E;
    [|reflexivity].
  apply find_some in E. destruct E as [Hin Hb]. simpl in Hb. subst b.
  pose proof (H _ Hin) as Ha. unfold alit_holds in Ha. simpl in Ha.
  apply forallb_forall. intros [c b] Hc. pose proof (H _ Hc) as Hp.
  unfold alit_holds in Hp. simpl in *. rewrite Ha in Hp.
  destruct b.
  - subst c. apply Nat.eqb_refl.
  - apply negb_true_iff. apply Nat.eqb_neq. intro E. apply Hp. symmetry. exact E.
Qed.

Definition remove_unsat (A : TBA root) : TBA root :=
  with_transitions A (filter (fun t => label_sat (bt_label t)) (tba_transitions A)).

Lemma remove_unsat_complete :
  forall A (rho : ext_word root),
    TBA_ext_accepts A rho -> TBA_ext_accepts (remove_unsat A) rho.
Proof.
  intros A rho [run [Hinit [Hsteps Hbuchi]]].
  apply with_transitions_complete with (run := run); try assumption.
  intro i.
  destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  exists t. split; [|split; [exact Hsrc|]; split; [exact Hdst | exact Hen]].
  apply filter_In. split; [exact Hin|].
  destruct Hen as [Hlab _]. exact (label_sat_holds Hlab).
Qed.

Theorem remove_unsat_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (remove_unsat A) w.
Proof.
  intros A. apply accepts_of_ext.
  - intro rho. apply filter_sound.
  - intros rho _. apply remove_unsat_complete.
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
  nodup (@clock_eq_dec root) (flat_map (fun t => bt_resets t) (tba_transitions A)).

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
  nodup Nat.eq_dec
    (L ++ map (fun t => bt_source t)
              (filter (fun t => tests x t ||
                                existsb (Nat.eqb (bt_target t)) L)
                      (tba_transitions A))).

(* The iteration stops as soon as the list does not grow.  (The correctness
   of the pass does not depend on how [live] is computed: the properties it
   needs are checked by [dead_ok].) *)
Fixpoint live_iter (n : nat) (A : TBA root) (x : Clock root) (L : list nat)
    : list nat :=
  match n with
  | O => L
  | S n' =>
      let L' := live_step A x L in
      if Nat.eqb (length L') (length L) then L' else live_iter n' A x L'
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
(* The same pass, computing the live locations once. *)
Definition remove_dead_resets_clock_fast (x : Clock root) (A : TBA root) : TBA root :=
  let L := live A x in
  let dead_in := fun l => negb (existsb (Nat.eqb l) L) in
  if forallb (fun t => negb (dead_in (bt_source t)) ||
                       (dead_in (bt_target t) && negb (tests x t)))
             (tba_transitions A)
  then with_transitions A
         (map (fun t => if dead_in (bt_target t) then drop_reset x t else t)
              (tba_transitions A))
  else A.

Lemma remove_dead_resets_clock_fast_eq :
  forall x A, remove_dead_resets_clock_fast x A = remove_dead_resets_clock x A.
Proof. intros x A. reflexivity. Qed.

Fixpoint remove_dead_resets_list (xs : list (Clock root)) (A : TBA root)
    : TBA root :=
  match xs with
  | [] => A
  | x :: xs' => remove_dead_resets_list xs' (remove_dead_resets_clock_fast x A)
  end.

Definition remove_dead_resets (A : TBA root) : TBA root :=
  remove_dead_resets_list (reset_clocks A) A.

Theorem remove_dead_resets_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (remove_dead_resets A) w.
Proof.
  intros A w. unfold remove_dead_resets.
  generalize (reset_clocks A) as xs. intro xs.
  revert A. induction xs as [|x xs IH]; intro A; simpl; [reflexivity|].
  rewrite remove_dead_resets_clock_fast_eq.
  rewrite (remove_dead_resets_clock_accepts x A w). apply IH.
Qed.



(* ====================================================================== *)
(* 6c. Normalization of guards                                            *)
(* ====================================================================== *)

(* Normalization tightens the guards (see [tighten]) and also drops the
   lower bounds that every clock value satisfies (x >= m with m <= 0,
   x > m with m < 0); it is needed after the merging of clocks, which
   renames constraints. *)

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
  if trivial_c c then acc else insert_t c acc.

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
  - apply insert_t_holds.
Qed.

Lemma fold_insert_holds :
  forall v l,
    (forall x, 0 <= v x) ->
    (Forall (cc_holds v) (fold_right insert_c [] l) <-> Forall (cc_holds v) l).
Proof.
  intros v l Hnn. induction l as [|c l IH]; simpl; [tauto|].
  rewrite (insert_c_holds c _ Hnn), IH, Forall_cons_iff. tauto.
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
(* 6d. Merging of equivalent states and of transitions                    *)
(* ====================================================================== *)

(* ---------------------------------------------------------------------- *)
(* Decidable equality of the components of transitions                    *)

Definition alit_dec : forall a b : alit, {a = b} + {a <> b}.
Proof. decide equality; [apply Bool.bool_dec | apply Nat.eq_dec]. Defined.

Definition cmp_dec : forall a b : clock_comparison, {a = b} + {a <> b}.
Proof. decide equality. Defined.

Definition cc_dec : forall a b : clock_constraint root, {a = b} + {a <> b}.
Proof.
  decide equality; first [apply Req_EM_T | apply cmp_dec | apply (@clock_eq_dec root)].
Defined.

Definition item_dec : forall a b : option (clock_constraint root), {a = b} + {a <> b}.
Proof. decide equality. apply cc_dec. Defined.

Definition tr_dec : forall a b : tba_transition root, {a = b} + {a <> b}.
Proof.
  decide equality;
    first [apply Nat.eq_dec | apply (list_eq_dec alit_dec)
          | apply (list_eq_dec item_dec) | apply (list_eq_dec (@clock_eq_dec root))].
Defined.

Definition beq {A : Type} (d : forall a b : A, {a = b} + {a <> b}) (a b : A) : bool :=
  if d a b then true else false.

Lemma beq_true :
  forall (A : Type) (d : forall a b : A, {a = b} + {a <> b}) a b,
    beq d a b = true <-> a = b.
Proof. intros A d a b. unfold beq. destruct (d a b); split; intro H; congruence. Qed.

Definition set_incl {A : Type} (d : forall a b : A, {a = b} + {a <> b}) (l1 l2 : list A)
    : bool :=
  forallb (fun a => existsb (beq d a) l2) l1.

Lemma set_incl_true :
  forall (A : Type) (d : forall a b : A, {a = b} + {a <> b}) l1 l2,
    set_incl d l1 l2 = true -> incl l1 l2.
Proof.
  intros A d l1 l2 H a Ha. unfold set_incl in H. rewrite forallb_forall in H.
  pose proof (H a Ha) as Hx. apply existsb_exists in Hx. destruct Hx as [b [Hb E]].
  apply beq_true in E. subst b. exact Hb.
Qed.

Definition set_eqb {A : Type} (d : forall a b : A, {a = b} + {a <> b}) (l1 l2 : list A)
    : bool :=
  set_incl d l1 l2 && set_incl d l2 l1.

Lemma label_incl :
  forall (l1 l2 : list alit) le, incl l2 l1 -> label_holds l1 le -> label_holds l2 le.
Proof.
  intros l1 l2 le Hi H. unfold label_holds in *. rewrite Forall_forall in *.
  intros a Ha. apply H. apply Hi. exact Ha.
Qed.

Lemma guard_incl :
  forall (rho : ext_word root) i (g1 g2 : guard root),
    incl g2 g1 -> Forall (guard_item_holds rho i) g1 -> Forall (guard_item_holds rho i) g2.
Proof.
  intros rho i g1 g2 Hi H. rewrite Forall_forall in *.
  intros a Ha. apply H. apply Hi. exact Ha.
Qed.

(* Enabledness depends only on the label, the guard, and the resets. *)
Lemma enabled_ext :
  forall (rho : ext_word root) i (t t' : tba_transition root),
    bt_label t = bt_label t' -> bt_guard t = bt_guard t' -> bt_resets t = bt_resets t' ->
    tba_transition_enabled rho i t -> tba_transition_enabled rho i t'.
Proof.
  intros rho i t t' El Eg Er [H1 [H2 H3]]. unfold tba_transition_enabled.
  rewrite <- El, <- Eg, <- Er. split; [exact H1|]. split; assumption.
Qed.

(* ---------------------------------------------------------------------- *)
(* Merging of equivalent states                                           *)

(* [q] is merged into [p]: transitions entering [q] are redirected to [p],
   and the transitions leaving [q] are removed.  This is allowed when [p] and
   [q] have the same acceptance status and the same outgoing transitions,
   targets being compared after the redirection. *)
Definition red (p q s : nat) : nat := if Nat.eqb s q then p else s.

Lemma red_q : forall p q, red p q q = p.
Proof. intros p q. unfold red. rewrite Nat.eqb_refl. reflexivity. Qed.

Lemma red_other : forall p q s, s <> q -> red p q s = s.
Proof. intros p q s H. unfold red. apply Nat.eqb_neq in H. rewrite H. reflexivity. Qed.

Lemma red_neq : forall p q s, p <> q -> red p q s <> q.
Proof.
  intros p q s Hpq. unfold red. destruct (Nat.eqb s q) eqn:E; [exact Hpq|].
  apply Nat.eqb_neq. exact E.
Qed.

Definition redirect (p q : nat) (t : tba_transition root) : tba_transition root :=
  {| bt_source := bt_source t; bt_label := bt_label t; bt_guard := bt_guard t;
     bt_resets := bt_resets t; bt_target := red p q (bt_target t) |}.

(* Labels, guards, and resets are compared as sets. *)
Definition sim (p q : nat) (t t' : tba_transition root) : bool :=
  set_eqb alit_dec (bt_label t) (bt_label t') &&
  set_eqb item_dec (bt_guard t) (bt_guard t') &&
  set_eqb (@clock_eq_dec root) (bt_resets t) (bt_resets t') &&
  Nat.eqb (red p q (bt_target t)) (red p q (bt_target t')).

(* A transition with fewer constraints and the same resets is enabled
   whenever the first one is. *)
Lemma enabled_set :
  forall (rho : ext_word root) i (t t' : tba_transition root),
    incl (bt_label t') (bt_label t) -> incl (bt_guard t') (bt_guard t) ->
    (forall x, In x (bt_resets t) <-> In x (bt_resets t')) ->
    tba_transition_enabled rho i t -> tba_transition_enabled rho i t'.
Proof.
  intros rho i t t' Hl Hg Hr [H1 [H2 H3]]. unfold tba_transition_enabled.
  split; [exact (label_incl Hl H1)|]. split; [exact (guard_incl Hg H2)|].
  intro x. rewrite (H3 x). apply Hr.
Qed.

Lemma sim_spec :
  forall p q t t', sim p q t t' = true ->
    (forall (rho : ext_word root) i,
       tba_transition_enabled rho i t -> tba_transition_enabled rho i t') /\
    (forall (rho : ext_word root) i,
       tba_transition_enabled rho i t' -> tba_transition_enabled rho i t) /\
    red p q (bt_target t) = red p q (bt_target t').
Proof.
  intros p q t t' H. unfold sim, set_eqb in H.
  repeat rewrite andb_true_iff in H.
  destruct H as [[[[L1 L2] [G1 G2]] [R1 R2]] H4].
  apply set_incl_true in L1. apply set_incl_true in L2.
  apply set_incl_true in G1. apply set_incl_true in G2.
  apply set_incl_true in R1. apply set_incl_true in R2.
  apply Nat.eqb_eq in H4.
  split; [|split; [|exact H4]]; intros rho i; apply enabled_set; auto;
    intro x; split; intro Hx; auto.
Qed.

(* Every transition leaving [s] has a similar transition leaving [s']. *)
Definition covers (A : TBA root) (p q s s' : nat) : bool :=
  forallb (fun t => negb (Nat.eqb (bt_source t) s) ||
                    existsb (fun t' => Nat.eqb (bt_source t') s' && sim p q t t')
                            (tba_transitions A))
          (tba_transitions A).

Lemma covers_spec :
  forall A p q s s', covers A p q s s' = true ->
    forall t, In t (tba_transitions A) -> bt_source t = s ->
      exists t', In t' (tba_transitions A) /\ bt_source t' = s' /\ sim p q t t' = true.
Proof.
  intros A p q s s' H t Ht Hs. unfold covers in H. rewrite forallb_forall in H.
  specialize (H t Ht). rewrite Hs, Nat.eqb_refl in H. simpl in H.
  apply existsb_exists in H. destruct H as [t' [Ht' E]].
  apply andb_true_iff in E. destruct E as [E1 E2]. apply Nat.eqb_eq in E1.
  exists t'. tauto.
Qed.

Definition in_nat (n : nat) (l : list nat) : bool := existsb (Nat.eqb n) l.

Lemma in_nat_iff : forall n l, in_nat n l = true <-> In n l.
Proof.
  intros n l. unfold in_nat. rewrite existsb_exists. split.
  - intros [m [Hm E]]. apply Nat.eqb_eq in E. subst m. exact Hm.
  - intro H. exists n. split; [exact H | apply Nat.eqb_refl].
Qed.

Definition states_mergeable (A : TBA root) (p q : nat) : bool :=
  negb (Nat.eqb p q) && Nat.ltb p (tba_nstates A) && Nat.ltb q (tba_nstates A) &&
  Bool.eqb (in_nat p (tba_accepting A)) (in_nat q (tba_accepting A)) &&
  covers A p q q p && covers A p q p q.

Definition merge_states (p q : nat) (A : TBA root) : TBA root :=
  {| tba_nstates := tba_nstates A;
     tba_init := red p q (tba_init A);
     tba_transitions := map (redirect p q)
                          (filter (fun t => negb (Nat.eqb (bt_source t) q))
                                  (tba_transitions A));
     tba_accepting := tba_accepting A |}.

(* A run defined step by step from a choice of the next state. *)
Fixpoint build_run (step : nat -> nat -> option nat) (s0 : nat) (i : nat) : nat :=
  match i with
  | O => s0
  | S j => match step j (build_run step s0 j) with Some s => s | None => s0 end
  end.

Lemma merge_states_ext :
  forall A p q, states_mergeable A p q = true ->
    forall rho : ext_word root,
      TBA_ext_accepts A rho <-> TBA_ext_accepts (merge_states p q A) rho.
Proof.
  intros A p q Hm rho. unfold states_mergeable in Hm.
  repeat rewrite andb_true_iff in Hm.
  destruct Hm as [[[[[Hpq Hp] Hq] Hacc] Hqp] Hpq'].
  apply negb_true_iff, Nat.eqb_neq in Hpq.
  apply Nat.ltb_lt in Hp. apply Nat.ltb_lt in Hq.
  apply Bool.eqb_prop in Hacc.
  assert (Hacc' : In p (tba_accepting A) <-> In q (tba_accepting A)).
  { rewrite <- !in_nat_iff, Hacc. tauto. }
  assert (Hbound : forall s, (s < tba_nstates A)%nat -> (red p q s < tba_nstates A)%nat).
  { intros s Hs. unfold red. destruct (Nat.eqb s q); assumption. }
  split.
  - (* A -> merged: replace q by p along the run *)
    intros [run [Hinit [Hsteps Hbuchi]]].
    exists (fun i => red p q (run i)). simpl.
    split; [rewrite Hinit; reflexivity|]. split.
    + intro i. destruct (Hsteps i) as [Hb [t [Ht [Hs [Hd Hen]]]]].
      split; [exact (Hbound _ Hb)|].
      destruct (Nat.eq_dec (run i) q) as [Eq|Nq].
      * destruct (covers_spec Hqp Ht (eq_trans Hs Eq)) as [t' [Ht' [Hs' Hsim]]].
        destruct (sim_spec Hsim) as [Hen1 [_ Et]].
        exists (redirect p q t'). split.
        -- apply in_map. apply filter_In. split; [exact Ht'|].
           rewrite Hs'. apply negb_true_iff, Nat.eqb_neq. exact Hpq.
        -- simpl. split; [rewrite Eq, red_q; exact Hs'|].
           split; [rewrite <- Et, Hd; reflexivity|].
           exact (enabled_ext (t := t') (t' := redirect p q t') eq_refl eq_refl eq_refl
                                (Hen1 rho i Hen)).
      * exists (redirect p q t). split.
        -- apply in_map. apply filter_In. split; [exact Ht|].
           rewrite Hs. apply negb_true_iff, Nat.eqb_neq. exact Nq.
        -- simpl. split; [rewrite Hs, red_other; [reflexivity | exact Nq]|].
           split; [rewrite Hd; reflexivity|].
           exact (enabled_ext (t' := redirect p q t) eq_refl eq_refl eq_refl Hen).
    + intro n. destruct (Hbuchi n) as [j [Hj Ha]]. exists j. split; [exact Hj|].
      unfold red. destruct (Nat.eqb (run j) q) eqn:E; [|exact Ha].
      apply Nat.eqb_eq in E. rewrite E in Ha. apply Hacc'. exact Ha.
  - (* merged -> A: follow the run, choosing in q the twin of a transition of p *)
    intros [run' [Hinit' [Hsteps' Hbuchi']]].
    simpl in Hinit', Hsteps', Hbuchi'.
    (* a step of the merged run comes from a transition of A not leaving q *)
    assert (Hstep : forall i, exists t, In t (tba_transitions A) /\ bt_source t <> q /\
                      bt_source t = run' i /\ red p q (bt_target t) = run' (S i) /\
                      tba_transition_enabled rho i t).
    { intro i. destruct (Hsteps' i) as [_ [tt [Htt [Hs [Hd Hen]]]]].
      apply in_map_iff in Htt. destruct Htt as [t [<- Ht]].
      apply filter_In in Ht. destruct Ht as [Ht Hnq].
      apply negb_true_iff, Nat.eqb_neq in Hnq.
      exists t. split; [exact Ht|]. split; [exact Hnq|]. simpl in *.
      split; [exact Hs|]. split; [exact Hd|].
      exact (enabled_ext (t := redirect p q t) eq_refl eq_refl eq_refl Hen). }
    set (P := fun i s (t : tba_transition root) =>
                bt_source t = s /\ red p q (bt_target t) = run' (S i) /\
                tba_transition_enabled rho i t).
    set (step := fun i s => match first_such (P i s) (tba_transitions A) with
                            | Some t => Some (bt_target t) | None => None end).
    set (run := build_run step (tba_init A)).
    (* the chosen transition exists at every step *)
    assert (Hex : forall i, red p q (run i) = run' i ->
                    exists t, In t (tba_transitions A) /\ P i (run i) t).
    { intros i Hr. destruct (Hstep i) as [t [Ht [Hnq [Hs [Hd Hen]]]]].
      destruct (Nat.eq_dec (run i) q) as [Eq|Nq].
      - assert (Hsp : bt_source t = p) by (rewrite Hs, <- Hr, Eq; apply red_q).
        destruct (covers_spec Hpq' Ht Hsp) as [t' [Ht' [Hs' Hsim]]].
        destruct (sim_spec Hsim) as [Hen1 [_ Et]].
        exists t'. split; [exact Ht'|]. split; [rewrite Eq; exact Hs'|].
        split; [rewrite <- Et; exact Hd|].
        exact (Hen1 rho i Hen).
      - exists t. split; [exact Ht|].
        split; [rewrite Hs, <- Hr; apply red_other; exact Nq|].
        split; [exact Hd | exact Hen]. }
    assert (Hrun : forall i, red p q (run i) = run' i).
    { induction i as [|i IH].
      - simpl. rewrite Hinit'. reflexivity.
      - destruct (first_such_spec (Hex i IH)) as [t [Hf [_ [_ [Hd _]]]]].
        change (run (S i)) with
          (match step i (run i) with Some s => s | None => tba_init A end).
        unfold step. rewrite Hf. exact Hd. }
    assert (Hrun_q : forall i, run i <> q -> run i = run' i).
    { intros i Hn. rewrite <- (Hrun i). symmetry. apply red_other. exact Hn. }
    exists run. split; [reflexivity|]. split.
    + intro i. split.
      * destruct (Nat.eq_dec (run i) q) as [Eq|Nq]; [rewrite Eq; exact Hq|].
        rewrite (Hrun_q i Nq). exact (proj1 (Hsteps' i)).
      * destruct (first_such_spec (Hex i (Hrun i))) as [t [Hf [Ht [Hs [_ Hen]]]]].
        exists t. split; [exact Ht|]. split; [exact Hs|].
        split; [|exact Hen].
        change (run (S i)) with
          (match step i (run i) with Some s => s | None => tba_init A end).
        unfold step. rewrite Hf. reflexivity.
    + intro n. destruct (Hbuchi' n) as [j [Hj Ha]]. exists j. split; [exact Hj|].
      destruct (Nat.eq_dec (run j) q) as [Eq|Nq].
      * rewrite Eq. apply Hacc'. rewrite <- (Hrun j), Eq, red_q in Ha. exact Ha.
      * rewrite (Hrun_q j Nq). exact Ha.
Qed.

Definition try_merge_states (p q : nat) (A : TBA root) : TBA root :=
  if states_mergeable A p q then merge_states p q A else A.

Definition state_pairs (n : nat) : list (nat * nat) :=
  filter (fun pq => Nat.ltb (fst pq) (snd pq)) (list_prod (seq 0 n) (seq 0 n)).

Definition merge_states_all (A : TBA root) : TBA root :=
  fold_left (fun B pq => try_merge_states (fst pq) (snd pq) B)
            (state_pairs (tba_nstates A)) A.

Lemma try_merge_states_ext :
  forall p q A (rho : ext_word root),
    TBA_ext_accepts A rho <-> TBA_ext_accepts (try_merge_states p q A) rho.
Proof.
  intros p q A rho. unfold try_merge_states.
  destruct (states_mergeable A p q) eqn:E; [apply merge_states_ext; exact E | reflexivity].
Qed.

Theorem merge_states_all_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (merge_states_all A) w.
Proof.
  intros A w. unfold merge_states_all.
  generalize (state_pairs (tba_nstates A)) as ps. intro ps.
  revert A. induction ps as [|[p q] ps IH]; intro A; simpl; [reflexivity|].
  rewrite <- IH. unfold TBA_accepts. split.
  - intros [rho [Hb [Hc Ha]]]. exists rho. split; [exact Hb|]. split; [exact Hc|].
    apply try_merge_states_ext. exact Ha.
  - intros [rho [Hb [Hc Ha]]]. exists rho. split; [exact Hb|]. split; [exact Hc|].
    apply (try_merge_states_ext p q). exact Ha.
Qed.

(* ---------------------------------------------------------------------- *)
(* Merging of transitions with the same source and target                  *)

(* [rel old new]: every enabled transition of [old] has an enabled
   counterpart in [new] with the same source and target, and conversely. *)
Definition rel (old new : list (tba_transition root)) : Prop :=
  (forall t, In t old -> forall (rho : ext_word root) i, tba_transition_enabled rho i t ->
     exists u, In u new /\ bt_source u = bt_source t /\ bt_target u = bt_target t /\
               tba_transition_enabled rho i u) /\
  (forall u, In u new -> forall (rho : ext_word root) i, tba_transition_enabled rho i u ->
     exists t, In t old /\ bt_source t = bt_source u /\ bt_target t = bt_target u /\
               tba_transition_enabled rho i t).

(* The sound direction needs the counterpart at the position where the
   transition is taken, so it is proved on runs directly. *)
Lemma rel_ext :
  forall A ts, rel (tba_transitions A) ts ->
    forall rho : ext_word root,
      TBA_ext_accepts A rho <-> TBA_ext_accepts (with_transitions A ts) rho.
Proof.
  intros A ts [Hf Hb] rho. split.
  - intros [run [Hinit [Hsteps Hbuchi]]].
    apply with_transitions_complete with (run := run); try assumption.
    intro i. destruct (Hsteps i) as [Hbd [t [Ht [Hs [Hd Hen]]]]].
    split; [exact Hbd|].
    destruct (Hf t Ht rho i Hen) as [u [Hu [Hus [Hut Hue]]]].
    exists u. split; [exact Hu|]. split; [congruence|]. split; [congruence | exact Hue].
  - intros [run [Hinit [Hsteps Hbuchi]]].
    exists run. simpl in *. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i. destruct (Hsteps i) as [Hbd [u [Hu [Hs [Hd Hen]]]]].
    split; [exact Hbd|].
    destruct (Hb u Hu rho i Hen) as [t [Ht [Hts [Htt Hte]]]].
    exists t. split; [exact Ht|]. split; [congruence|]. split; [congruence | exact Hte].
Qed.

Lemma rel_trans : forall l1 l2 l3, rel l1 l2 -> rel l2 l3 -> rel l1 l3.
Proof.
  intros l1 l2 l3 [F12 B12] [F23 B23]. split.
  - intros t Ht rho i Hen. destruct (F12 t Ht rho i Hen) as [u [Hu [Hs [Hd He]]]].
    destruct (F23 u Hu rho i He) as [v [Hv [Hs' [Hd' He']]]].
    exists v. split; [exact Hv|]. split; [congruence|]. split; [congruence | exact He'].
  - intros v Hv rho i Hen. destruct (B23 v Hv rho i Hen) as [u [Hu [Hs [Hd He]]]].
    destruct (B12 u Hu rho i He) as [t [Ht [Hs' [Hd' He']]]].
    exists t. split; [exact Ht|]. split; [congruence|]. split; [congruence | exact He'].
Qed.

(* Same source, target, and resets. *)
Definition same_ends (t u : tba_transition root) : bool :=
  Nat.eqb (bt_source t) (bt_source u) && Nat.eqb (bt_target t) (bt_target u) &&
  beq (list_eq_dec (@clock_eq_dec root)) (bt_resets t) (bt_resets u).

Lemma same_ends_spec :
  forall t u, same_ends t u = true ->
    bt_source t = bt_source u /\ bt_target t = bt_target u /\ bt_resets t = bt_resets u.
Proof.
  intros t u H. unfold same_ends in H. repeat rewrite andb_true_iff in H.
  destruct H as [[H1 H2] H3]. apply Nat.eqb_eq in H1. apply Nat.eqb_eq in H2.
  apply beq_true in H3. tauto.
Qed.

(* Every constraint of [g'] is implied by a constraint of [g]. *)
Definition guard_implied (g g' : guard root) : bool :=
  forallb (fun o => match o with
                    | None => true
                    | Some k' => existsb (fun o2 => match o2 with
                                                    | Some k => implies_c k k'
                                                    | None => false end) g
                    end) g'.

Lemma guard_implied_holds :
  forall (rho : ext_word root) i g g',
    guard_implied g g' = true ->
    Forall (guard_item_holds rho i) g -> Forall (guard_item_holds rho i) g'.
Proof.
  intros rho i g g' H Hg. unfold guard_implied in H. rewrite forallb_forall in H.
  rewrite Forall_forall in *. intros [k'|] Ho; [|exact I].
  specialize (H _ Ho). simpl in H. apply existsb_exists in H.
  destruct H as [[k|] [Hk E]]; [|discriminate].
  pose proof (Hg _ Hk) as Hh. simpl in *. apply cc_holds_at in Hh. apply cc_holds_at.
  exact (implies_c_sound E Hh).
Qed.

(* [u] makes [t] redundant: same ends, weaker label and weaker guard. *)
Definition subsumes (u t : tba_transition root) : bool :=
  same_ends u t && set_incl alit_dec (bt_label u) (bt_label t) &&
  guard_implied (bt_guard t) (bt_guard u).

Lemma subsumes_spec :
  forall u t, subsumes u t = true ->
    bt_source u = bt_source t /\ bt_target u = bt_target t /\
    forall (rho : ext_word root) i,
      tba_transition_enabled rho i t -> tba_transition_enabled rho i u.
Proof.
  intros u t H. unfold subsumes in H. repeat rewrite andb_true_iff in H.
  destruct H as [[He Hl] Hg]. destruct (same_ends_spec He) as [Hs [Hd Hr]].
  split; [exact Hs|]. split; [exact Hd|].
  intros rho i [H1 [H2 H3]]. split; [|split].
  - exact (label_incl (set_incl_true Hl) H1).
  - exact (guard_implied_holds Hg H2).
  - unfold resets_match in *. rewrite Hr. exact H3.
Qed.

Definition insert_sub (t : tba_transition root) (acc : list (tba_transition root))
    : list (tba_transition root) :=
  if existsb (fun a => subsumes a t) acc then acc
  else t :: filter (fun a => negb (subsumes t a)) acc.

Lemma insert_sub_rel :
  forall t l acc, rel l acc -> rel (t :: l) (insert_sub t acc).
Proof.
  intros t l acc [F B]. unfold insert_sub.
  destruct (existsb (fun a => subsumes a t) acc) eqn:E.
  - apply existsb_exists in E. destruct E as [a [Ha Hs]].
    destruct (subsumes_spec Hs) as [Hsa [Hda Hen]]. split.
    + intros t' [->|Ht'] rho i He.
      * exists a. split; [exact Ha|]. split; [exact Hsa|]. split; [exact Hda|].
        exact (Hen rho i He).
      * exact (F t' Ht' rho i He).
    + intros u Hu rho i He. destruct (B u Hu rho i He) as [t' [Ht' H]].
      exists t'. split; [right; exact Ht' | exact H].
  - split.
    + intros t' [->|Ht'] rho i He.
      * exists t'. split; [left; reflexivity|]. tauto.
      * destruct (F t' Ht' rho i He) as [u [Hu [Hs [Hd Hue]]]].
        destruct (subsumes t u) eqn:Etu.
        -- destruct (subsumes_spec Etu) as [Hs' [Hd' Hen]].
           exists t. split; [left; reflexivity|]. split; [congruence|].
           split; [congruence | exact (Hen rho i Hue)].
        -- exists u. split; [right; apply filter_In; rewrite Etu; tauto|]. tauto.
    + intros u [->|Hu] rho i He.
      * exists u. split; [left; reflexivity|]. tauto.
      * apply filter_In in Hu. destruct (B u (proj1 Hu) rho i He) as [t' [Ht' H]].
        exists t'. split; [right; exact Ht' | exact H].
Qed.

(* Resolution: two labels (or guards) that differ only by a literal and its
   negation are replaced by their common part. *)
Definition remove_item {A : Type} (d : forall a b : A, {a = b} + {a <> b}) (x : A)
    (l : list A) : list A :=
  filter (fun y => negb (beq d x y)) l.

Lemma remove_item_incl :
  forall (A : Type) d (x : A) l, incl (remove_item d x l) l.
Proof. intros A d x l y Hy. apply filter_In in Hy. tauto. Qed.

Lemma remove_item_cover :
  forall (A : Type) d (x : A) l y, In y l -> y = x \/ In y (remove_item d x l).
Proof.
  intros A d x l y Hy. destruct (d x y) as [<-|N]; [left; reflexivity|].
  right. apply filter_In. split; [exact Hy|]. unfold beq.
  destruct (d x y); [contradiction | reflexivity].
Qed.

Definition neg_alit (l : alit) : alit := (fst l, negb (snd l)).

Definition resolve_lab (l1 l2 : list alit) : option (list alit) :=
  match find (fun x => existsb (beq alit_dec (neg_alit x)) l2 &&
                       set_eqb alit_dec (remove_item alit_dec x l1)
                               (remove_item alit_dec (neg_alit x) l2)) l1 with
  | Some x => Some (remove_item alit_dec x l1)
  | None => None
  end.

Lemma alit_cases : forall (e : Action) (x : alit), alit_holds e x \/ alit_holds e (neg_alit x).
Proof.
  intros e [a b]. unfold alit_holds, neg_alit. simpl.
  destruct b; simpl; destruct (Nat.eq_dec e a); tauto.
Qed.

Lemma resolve_lab_spec :
  forall l1 l2 m, resolve_lab l1 l2 = Some m ->
    (forall le, label_holds l1 le -> label_holds m le) /\
    (forall le, label_holds l2 le -> label_holds m le) /\
    (forall le, label_holds m le -> label_holds l1 le \/ label_holds l2 le).
Proof.
  intros l1 l2 m H. unfold resolve_lab in H.
  destruct (find _ l1) as [x|] eqn:E; [|discriminate]. injection H as <-.
  apply find_some in E. destruct E as [Hx E].
  apply andb_true_iff in E. destruct E as [Hn Hq].
  apply existsb_exists in Hn. destruct Hn as [y [Hny Ey]]. apply beq_true in Ey. subst y.
  unfold set_eqb in Hq. apply andb_true_iff in Hq. destruct Hq as [Hq1 Hq2].
  apply set_incl_true in Hq1. apply set_incl_true in Hq2.
  split; [|split].
  - intros le H. exact (label_incl (@remove_item_incl _ alit_dec x l1) H).
  - intros le H. apply (label_incl Hq1).
    exact (label_incl (@remove_item_incl _ alit_dec (neg_alit x) l2) H).
  - intros le H. unfold label_holds in *. rewrite Forall_forall in H.
    destruct (alit_cases (letter_action le) x) as [Hxh|Hxh].
    + left. apply Forall_forall. intros y Hy.
      destruct (@remove_item_cover _ alit_dec x _ _ Hy) as [->|Hy']; [exact Hxh | exact (H y Hy')].
    + right. apply Forall_forall. intros y Hy.
      destruct (@remove_item_cover _ alit_dec (neg_alit x) _ _ Hy) as [->|Hy']; [exact Hxh|].
      apply H. apply Hq2. exact Hy'.
Qed.

(* The complement of a non-strict/strict bound. *)
Definition compl (k : clock_constraint root) : option (clock_constraint root) :=
  let mk c := Some {| guard_clock := guard_clock k; guard_comparison := c;
                      guard_bound := guard_bound k |} in
  match guard_comparison k with
  | CLe => mk CGt
  | CLt => mk CGe
  | CGe => mk CLt
  | CGt => mk CLe
  | CEq => None
  end.

Lemma compl_cases :
  forall (rho : ext_word root) i k k', compl k = Some k' ->
    clock_constraint_holds rho i k \/ clock_constraint_holds rho i k'.
Proof.
  intros rho i [x c d] k' H. unfold compl in H. simpl in H.
  destruct c; try discriminate; injection H as <-;
    unfold clock_constraint_holds; simpl;
    destruct (Rle_lt_dec (ew_val rho i x) d); lra.
Qed.

Definition resolve_grd (g1 g2 : guard root) : option (guard root) :=
  match find (fun o => match o with
                       | Some k =>
                           match compl k with
                           | Some k' => existsb (beq item_dec (Some k')) g2 &&
                                        set_eqb item_dec (remove_item item_dec o g1)
                                                (remove_item item_dec (Some k') g2)
                           | None => false
                           end
                       | None => false
                       end) g1 with
  | Some o => Some (remove_item item_dec o g1)
  | None => None
  end.

Lemma resolve_grd_spec :
  forall g1 g2 m, resolve_grd g1 g2 = Some m ->
    forall (rho : ext_word root) i,
    (Forall (guard_item_holds rho i) g1 -> Forall (guard_item_holds rho i) m) /\
    (Forall (guard_item_holds rho i) g2 -> Forall (guard_item_holds rho i) m) /\
    (Forall (guard_item_holds rho i) m ->
       Forall (guard_item_holds rho i) g1 \/ Forall (guard_item_holds rho i) g2).
Proof.
  intros g1 g2 m H rho i. unfold resolve_grd in H.
  destruct (find _ g1) as [o|] eqn:E; [|discriminate]. injection H as <-.
  apply find_some in E. destruct E as [Ho E].
  destruct o as [k|]; [|discriminate].
  destruct (compl k) as [k'|] eqn:Ec; [|discriminate].
  apply andb_true_iff in E. destruct E as [Hn Hq].
  apply existsb_exists in Hn. destruct Hn as [y [Hny Ey]]. apply beq_true in Ey. subst y.
  unfold set_eqb in Hq. apply andb_true_iff in Hq. destruct Hq as [Hq1 Hq2].
  apply set_incl_true in Hq1. apply set_incl_true in Hq2.
  split; [|split].
  - intro H. exact (guard_incl (@remove_item_incl _ item_dec (Some k) g1) H).
  - intro H. apply (guard_incl Hq1).
    exact (guard_incl (@remove_item_incl _ item_dec (Some k') g2) H).
  - intro H. rewrite Forall_forall in H.
    destruct (compl_cases rho i Ec) as [Hk|Hk].
    + left. apply Forall_forall. intros y Hy.
      destruct (@remove_item_cover _ item_dec (Some k) _ _ Hy) as [->|Hy']; [exact Hk | exact (H y Hy')].
    + right. apply Forall_forall. intros y Hy.
      destruct (@remove_item_cover _ item_dec (Some k') _ _ Hy) as [->|Hy']; [exact Hk|].
      apply H. apply Hq2. exact Hy'.
Qed.

Definition with_label (t : tba_transition root) (l : list alit) : tba_transition root :=
  {| bt_source := bt_source t; bt_label := l; bt_guard := bt_guard t;
     bt_resets := bt_resets t; bt_target := bt_target t |}.

Definition with_guard (t : tba_transition root) (g : guard root) : tba_transition root :=
  {| bt_source := bt_source t; bt_label := bt_label t; bt_guard := g;
     bt_resets := bt_resets t; bt_target := bt_target t |}.

(* Two transitions with the same ends, equal guards (as sets) and resolvable
   labels, or equal labels (as sets) and resolvable guards. *)
Definition resolve (u t : tba_transition root) : option (tba_transition root) :=
  if same_ends u t then
    if set_eqb item_dec (bt_guard u) (bt_guard t) then
      match resolve_lab (bt_label u) (bt_label t) with
      | Some m => Some (with_label u m) | None => None end
    else if set_eqb alit_dec (bt_label u) (bt_label t) then
      match resolve_grd (bt_guard u) (bt_guard t) with
      | Some g => Some (with_guard u g) | None => None end
    else None
  else None.

Lemma resolve_spec :
  forall u t m, resolve u t = Some m ->
    bt_source m = bt_source u /\ bt_target m = bt_target u /\
    bt_source t = bt_source u /\ bt_target t = bt_target u /\
    forall (rho : ext_word root) i,
      (tba_transition_enabled rho i u -> tba_transition_enabled rho i m) /\
      (tba_transition_enabled rho i t -> tba_transition_enabled rho i m) /\
      (tba_transition_enabled rho i m ->
         tba_transition_enabled rho i u \/ tba_transition_enabled rho i t).
Proof.
  intros u t m H. unfold resolve in H.
  destruct (same_ends u t) eqn:Ee; [|discriminate].
  destruct (same_ends_spec Ee) as [Hs [Hd Hr]].
  destruct (set_eqb item_dec (bt_guard u) (bt_guard t)) eqn:Eg.
  - destruct (resolve_lab (bt_label u) (bt_label t)) as [l|] eqn:El; [|discriminate].
    injection H as <-. simpl.
    destruct (resolve_lab_spec El) as [L1 [L2 L3]].
    unfold set_eqb in Eg. apply andb_true_iff in Eg. destruct Eg as [G1 G2].
    apply set_incl_true in G1. apply set_incl_true in G2.
    split; [reflexivity|]. split; [reflexivity|]. split; [congruence|]. split; [congruence|].
    intros rho i. unfold tba_transition_enabled. simpl. split; [|split].
    + intros [H1 [H2 H3]]. split; [exact (L1 _ H1)|]. split; assumption.
    + intros [H1 [H2 H3]]. split; [exact (L2 _ H1)|].
      split; [exact (guard_incl G1 H2)|]. unfold resets_match in *. rewrite Hr. exact H3.
    + intros [H1 [H2 H3]]. destruct (L3 _ H1) as [H1'|H1'].
      * left. split; [exact H1'|]. split; assumption.
      * right. split; [exact H1'|]. split; [exact (guard_incl G2 H2)|].
        unfold resets_match in *. rewrite <- Hr. exact H3.
  - destruct (set_eqb alit_dec (bt_label u) (bt_label t)) eqn:Elb; [|discriminate].
    destruct (resolve_grd (bt_guard u) (bt_guard t)) as [g|] eqn:Eg'; [|discriminate].
    injection H as <-. simpl.
    unfold set_eqb in Elb. apply andb_true_iff in Elb. destruct Elb as [B1 B2].
    apply set_incl_true in B1. apply set_incl_true in B2.
    split; [reflexivity|]. split; [reflexivity|]. split; [congruence|]. split; [congruence|].
    intros rho i. destruct (resolve_grd_spec Eg' rho i) as [R1 [R2 R3]].
    unfold tba_transition_enabled. simpl. split; [|split].
    + intros [H1 [H2 H3]]. split; [exact H1|]. split; [exact (R1 H2) | exact H3].
    + intros [H1 [H2 H3]]. split; [exact (label_incl B1 H1)|].
      split; [exact (R2 H2)|]. unfold resets_match in *. rewrite Hr. exact H3.
    + intros [H1 [H2 H3]]. destruct (R3 H2) as [H2'|H2'].
      * left. split; [exact H1|]. split; assumption.
      * right. split; [exact (label_incl B2 H1)|]. split; [exact H2'|].
        unfold resets_match in *. rewrite <- Hr. exact H3.
Qed.

Definition insert_res (t : tba_transition root) (acc : list (tba_transition root))
    : list (tba_transition root) :=
  match find (fun a => match resolve a t with Some _ => true | None => false end) acc with
  | Some a =>
      match resolve a t with
      | Some m => m :: remove_item tr_dec a acc
      | None => t :: acc
      end
  | None => t :: acc
  end.

Lemma insert_res_rel :
  forall t l acc, rel l acc -> rel (t :: l) (insert_res t acc).
Proof.
  intros t l acc [F B].
  assert (Hadd : rel (t :: l) (t :: acc)).
  { split.
    - intros t' [->|Ht'] rho i He.
      + exists t'. split; [left; reflexivity|]. tauto.
      + destruct (F t' Ht' rho i He) as [u [Hu H]]. exists u. split; [right; exact Hu | exact H].
    - intros u [->|Hu] rho i He.
      + exists u. split; [left; reflexivity|]. tauto.
      + destruct (B u Hu rho i He) as [t' [Ht' H]]. exists t'. split; [right; exact Ht' | exact H]. }
  unfold insert_res.
  destruct (find _ acc) as [a|] eqn:Ef; [|exact Hadd].
  destruct (resolve a t) as [m|] eqn:Er; [|exact Hadd].
  apply find_some in Ef. destruct Ef as [Ha _].
  destruct (resolve_spec Er) as [Hms [Hmd [Hts [Htd Hen]]]].
  split.
  - intros t' [->|Ht'] rho i He.
    + exists m. split; [left; reflexivity|]. split; [congruence|]. split; [congruence|].
      exact (proj1 (proj2 (Hen rho i)) He).
    + destruct (F t' Ht' rho i He) as [u [Hu [Hus [Hud Hue]]]].
      destruct (@remove_item_cover _ tr_dec a _ _ Hu) as [->|Hu'].
      * exists m. split; [left; reflexivity|]. split; [congruence|]. split; [congruence|].
        exact (proj1 (Hen rho i) Hue).
      * exists u. split; [right; exact Hu'|]. tauto.
  - intros u [->|Hu] rho i He.
    + destruct (proj2 (proj2 (Hen rho i)) He) as [Ha'|Ht'].
      * destruct (B a Ha rho i Ha') as [t' [Ht'' [Hs [Hd Hte]]]].
        exists t'. split; [right; exact Ht''|]. split; [congruence|]. split; [congruence | exact Hte].
      * exists t. split; [left; reflexivity|]. split; [congruence|]. split; [congruence | exact Ht'].
    + apply remove_item_incl in Hu. destruct (B u Hu rho i He) as [t' [Ht' H]].
      exists t'. split; [right; exact Ht' | exact H].
Qed.

Lemma fold_rel :
  forall (ins : tba_transition root -> list (tba_transition root) -> list (tba_transition root)),
    (forall t l acc, rel l acc -> rel (t :: l) (ins t acc)) ->
    forall l, rel l (fold_right ins [] l).
Proof.
  intros ins Hins l. induction l as [|t l IH]; simpl.
  - split; intros x Hx; contradiction.
  - apply Hins. exact IH.
Qed.

Definition merge_trans (A : TBA root) : TBA root :=
  with_transitions A
    (fold_right insert_res [] (fold_right insert_sub [] (tba_transitions A))).

Theorem merge_trans_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (merge_trans A) w.
Proof.
  intros A w.
  assert (Hrel : rel (tba_transitions A)
                     (fold_right insert_res [] (fold_right insert_sub [] (tba_transitions A)))).
  { apply rel_trans with (fold_right insert_sub [] (tba_transitions A)).
    - apply fold_rel. exact insert_sub_rel.
    - apply fold_rel. exact insert_res_rel. }
  unfold merge_trans, TBA_accepts. split.
  - intros [rho [Hb [Hc Ha]]]. exists rho. split; [exact Hb|]. split; [exact Hc|].
    apply (rel_ext Hrel). exact Ha.
  - intros [rho [Hb [Hc Ha]]]. exists rho. split; [exact Hb|]. split; [exact Hc|].
    apply (rel_ext Hrel). exact Ha.
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
  merge_states_all (merge_trans (normalize
    (merge_all
       (remove_dead_resets
          (normalize
             (remove_unreachable
                (remove_unsat (remove_contradictory (forward (propagate A)))))))))).

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
  rewrite (remove_unsat_accepts (remove_contradictory (forward (propagate A))) w).
  rewrite (remove_unreachable_accepts
             (remove_unsat (remove_contradictory (forward (propagate A)))) w).
  rewrite (normalize_accepts
             (remove_unreachable
                (remove_unsat (remove_contradictory (forward (propagate A))))) w).
  rewrite (remove_dead_resets_accepts
             (normalize
                (remove_unreachable
                   (remove_unsat (remove_contradictory (forward (propagate A)))))) w).
  rewrite merge_all_accepts.
  rewrite normalize_accepts.
  rewrite merge_trans_accepts.
  apply merge_states_all_accepts.
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
