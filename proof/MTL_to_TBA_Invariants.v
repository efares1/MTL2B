(*
  MTL_to_TBA_Invariants.v

  Timed Buchi automata with location invariants, invariant synthesis, and
  preservation of the semantics.

  * A location invariant contains only upper-bound constraints on clocks:
    it maps each clock either to [None] (the clock is unconstrained) or to
    [Some M] (the constraint x <= M).

  * Synthesis: for a location l and a clock x, the bound of x is the maximum,
    over the transitions leaving l, of the upper bound that the guard of the
    transition puts on x.  If some outgoing guard does not bound x (or l has
    no outgoing transition), x does not appear in the invariant of l.

  * Semantics: timed Buchi automata with invariants are interpreted on the
    same trace model (extended words over [Clock root]).  Between the events
    i-1 and i the automaton stays in location [run i]; its invariant must
    hold during the whole stay, i.e. at the clock values
    [ew_val rho i x - delta] for every delay [delta] of the stay.

  * Preservation: adding the synthesized invariants does not change the
    accepted extended words, hence the end-to-end correctness theorem holds
    for the automaton with invariants.

  * Iterated backward propagation: one step strengthens every transition
    l --g,Z--> l' into g /\ Inv(l')[Z := 0], where Inv(l') is the
    synthesized invariant of the target; the invariants are then
    synthesized again from the strengthened guards.  Each step preserves
    the language of timed words, hence so do n steps for every n, in
    particular the number of steps after which a fixpoint is reached.

  No new axiom: the only project axiom remains LTL_TO_BUCHI_CORRECT.
*)

Require Import Arith Lia List Bool Reals Lra.
Require Import Classical ClassicalDescription.
Require Import MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.
Require Import EncodingCorrect_Shared_Clock_Derived_Strict_Direct_Proof.
Import ListNotations.
Open Scope R_scope.

Set Implicit Arguments.
Unset Strict Implicit.

Section Invariants.

Variable root : mtl.

(* ====================================================================== *)
(* 1. Upper bound put by a guard on a clock                               *)
(* ====================================================================== *)

(* Comparisons that give an upper bound on the clock. *)
Definition is_upper (k : clock_comparison) : bool :=
  match k with
  | CLe | CLt | CEq => true
  | CGe | CGt => false
  end.

(* Equality test on clocks (classical, as clocks contain real bounds). *)
Definition clock_eqb (x y : Clock root) : bool :=
  if @clock_eq_dec root x y then true else false.

Lemma clock_eqb_true :
  forall x y, clock_eqb x y = true -> x = y.
Proof.
  intros x y. unfold clock_eqb.
  destruct (@clock_eq_dec root x y); [auto | discriminate].
Qed.

Definition opt_min (a b : option R) : option R :=
  match a, b with
  | None, _ => b
  | _, None => a
  | Some u, Some v => Some (Rmin u v)
  end.

(* The upper bound that one guard item puts on clock [x], if any. *)
Definition ub_item (x : Clock root) (o : option (clock_constraint root))
    : option R :=
  match o with
  | Some k =>
      if is_upper (guard_comparison k) && clock_eqb (guard_clock k) x
      then Some (guard_bound k)
      else None
  | None => None
  end.

(* The upper bound that a guard puts on clock [x]: the least of the upper
   bounds of its items on [x]; [None] if no item bounds [x]. *)
Fixpoint ub_guard (g : guard root) (x : Clock root) : option R :=
  match g with
  | [] => None
  | o :: g' => opt_min (ub_item x o) (ub_guard g' x)
  end.

Lemma ub_item_sound :
  forall (rho : ext_word root) i x o b,
    guard_item_holds rho i o ->
    ub_item x o = Some b ->
    ew_val rho i x <= b.
Proof.
  intros rho i x [k|] b Hhold Hub; simpl in *; [|discriminate].
  destruct (is_upper (guard_comparison k)) eqn:Hup;
    destruct (clock_eqb (guard_clock k) x) eqn:Heq;
    simpl in Hub; try discriminate.
  injection Hub as <-.
  apply clock_eqb_true in Heq. subst x.
  unfold clock_constraint_holds in Hhold.
  destruct (guard_comparison k); simpl in Hup; try discriminate; lra.
Qed.

Lemma ub_guard_sound :
  forall (rho : ext_word root) i g x b,
    Forall (guard_item_holds rho i) g ->
    ub_guard g x = Some b ->
    ew_val rho i x <= b.
Proof.
  intros rho i g x.
  induction g as [|o g IH]; intros b Hall Hub; simpl in Hub; [discriminate|].
  inversion Hall as [|o' g' Ho Hg]; subst.
  destruct (ub_item x o) as [u|] eqn:Hi;
    destruct (ub_guard g x) as [v|] eqn:Hg';
    simpl in Hub; try discriminate.
  - injection Hub as <-.
    pose proof (ub_item_sound Ho Hi).
    pose proof (IH v Hg eq_refl).
    unfold Rmin; destruct (Rle_dec u v); lra.
  - injection Hub as <-. exact (ub_item_sound Ho Hi).
  - apply IH; [exact Hg | exact Hub].
Qed.

(* ====================================================================== *)
(* 2. Invariant synthesis                                                 *)
(* ====================================================================== *)

(* Maximum of a non-empty list of bounds; [None] if the list is empty or
   if one of its elements is [None] (an unbounded clock). *)
Fixpoint max_all (l : list (option R)) : option R :=
  match l with
  | [] => None
  | [a] => a
  | a :: l' =>
      match a, max_all l' with
      | Some u, Some v => Some (Rmax u v)
      | _, _ => None
      end
  end.

Lemma max_all_ge :
  forall l M a,
    max_all l = Some M ->
    In a l ->
    exists b, a = Some b /\ b <= M.
Proof.
  induction l as [|a0 l IH]; intros M a Hmax Hin; [contradiction|].
  destruct l as [|a1 l'].
  - simpl in Hmax. destruct Hin as [<- | []].
    exists M. split; [exact Hmax | lra].
  - change (match a0, max_all (a1 :: l') with
            | Some u, Some v => Some (Rmax u v)
            | _, _ => None end = Some M) in Hmax.
    destruct a0 as [u|]; [|discriminate].
    destruct (max_all (a1 :: l')) as [v|] eqn:Hv; [|discriminate].
    injection Hmax as <-.
    destruct Hin as [<- | Hin].
    + exists u. split; [reflexivity | apply Rmax_l].
    + destruct (IH v a eq_refl Hin) as [b [-> Hb]].
      exists b. split; [reflexivity|].
      pose proof (Rmax_r u v). lra.
Qed.

(* Transitions leaving location [l]. *)
Definition outgoing (A : TBA root) (l : nat) : list (tba_transition root) :=
  filter (fun t => Nat.eqb (bt_source t) l) (tba_transitions A).

(* Synthesized invariant of location [l]: for each clock, the maximum of the
   upper bounds of the outgoing guards; [None] when some outgoing guard does
   not bound the clock. *)
Definition synth_inv (A : TBA root) (l : nat) (x : Clock root) : option R :=
  max_all (map (fun t => ub_guard (bt_guard t) x) (outgoing A l)).

(* ====================================================================== *)
(* 3. Timed Buchi automata with invariants and their semantics           *)
(* ====================================================================== *)

(* An invariant contains only upper-bound constraints on clocks. *)
Definition invariant := Clock root -> option R.

Record TBAI : Type := {
  tbai_base : TBA root;
  tbai_inv : nat -> invariant
}.

Definition inv_holds (I : invariant) (v : Clock root -> R) : Prop :=
  forall x M, I x = Some M -> v x <= M.

(* Duration of the stay that ends at event [i]: the location [run i] is
   entered at event [i-1]; nothing is observed before event 0. *)
Definition stay (rho : ext_word root) (i : nat) : R :=
  match i with
  | O => 0
  | S j => delta (ew_base rho) j
  end.

(* The invariant holds during the whole stay that ends at event [i]. *)
Definition inv_during (rho : ext_word root) (i : nat) (I : invariant) : Prop :=
  forall dl, 0 <= dl <= stay rho i ->
    inv_holds I (fun x => ew_val rho i x - dl).

(* Clocks grow during a stay, so an upper-bound invariant holds during the
   whole stay iff it holds when the event occurs. *)
Lemma inv_during_iff_at_event :
  forall rho i I,
    inv_during rho i I <-> inv_holds I (fun x => ew_val rho i x).
Proof.
  intros rho i I. split.
  - intros H x M HI.
    assert (H0 : 0 <= 0 <= stay rho i).
    { split; [lra|]. destruct i; simpl; [lra|].
      left. apply delta_positive. }
    pose proof (H 0 H0 x M HI). simpl in *. lra.
  - intros H dl Hdl x M HI.
    pose proof (H x M HI). simpl in *. lra.
Qed.

Definition TBAI_ext_accepts (B : TBAI) (rho : ext_word root) : Prop :=
  exists run : nat -> nat,
    run 0%nat = tba_init (tbai_base B) /\
    (forall i,
       (run i < tba_nstates (tbai_base B))%nat /\
       inv_during rho i (tbai_inv B (run i)) /\
       exists t,
         In t (tba_transitions (tbai_base B)) /\
         bt_source t = run i /\
         bt_target t = run (S i) /\
         tba_transition_enabled rho i t) /\
    (forall n,
       exists j,
         (n <= j)%nat /\ In (run j) (tba_accepting (tbai_base B))).

Definition TBAI_accepts (B : TBAI) (w : timed_word) : Prop :=
  exists rho : ext_word root,
    same_base rho w /\
    clock_consistent rho /\
    TBAI_ext_accepts B rho.

(* The TBA extended with its synthesized invariants. *)
Definition add_invariants (A : TBA root) : TBAI :=
  {| tbai_base := A; tbai_inv := synth_inv A |}.

(* ====================================================================== *)
(* 4. Preservation of the semantics                                       *)
(* ====================================================================== *)

(* A transition enabled at event [i] makes the synthesized invariant of its
   source location hold at that event. *)
Lemma enabled_transition_satisfies_invariant :
  forall (A : TBA root) (rho : ext_word root) i t,
    In t (tba_transitions A) ->
    tba_transition_enabled rho i t ->
    inv_holds (synth_inv A (bt_source t)) (fun x => ew_val rho i x).
Proof.
  intros A rho i t Hin [_ [Hguard _]] x M HM.
  unfold synth_inv in HM.
  assert (Hout : In t (outgoing A (bt_source t))).
  { unfold outgoing. apply filter_In. split; [exact Hin|].
    apply Nat.eqb_refl. }
  destruct (max_all_ge HM (in_map (fun t => ub_guard (bt_guard t) x) _ _ Hout))
    as [b [Hb HbM]].
  pose proof (ub_guard_sound Hguard Hb).
  simpl. lra.
Qed.

Theorem add_invariants_correct :
  forall (A : TBA root) (rho : ext_word root),
    TBA_ext_accepts A rho <-> TBAI_ext_accepts (add_invariants A) rho.
Proof.
  intros A rho. unfold TBA_ext_accepts, TBAI_ext_accepts. simpl.
  split.
  - intros [run [Hinit [Hsteps Hbuchi]]].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i.
    destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
    split; [exact Hbound|]. split.
    + apply inv_during_iff_at_event.
      rewrite <- Hsrc.
      exact (enabled_transition_satisfies_invariant Hin Hen).
    + exists t. split; [exact Hin|]. split; [exact Hsrc|].
      split; [exact Hdst|exact Hen].
  - intros [run [Hinit [Hsteps Hbuchi]]].
    exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
    intro i.
    destruct (Hsteps i) as [Hbound [_ Htrans]].
    split; [exact Hbound | exact Htrans].
Qed.

Theorem add_invariants_accepts :
  forall (A : TBA root) w,
    TBA_accepts A w <-> TBAI_accepts (add_invariants A) w.
Proof.
  intros A w. unfold TBA_accepts, TBAI_accepts.
  split.
  - intros [rho [Hb [Hc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hc|].
    exact (proj1 (add_invariants_correct A rho) Ha).
  - intros [rho [Hb [Hc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hc|].
    exact (proj2 (add_invariants_correct A rho) Ha).
Qed.


(* ====================================================================== *)
(* 5. Iterated backward propagation                                       *)
(* ====================================================================== *)

(* Clocks bounded by an upper-bound item of a guard. *)
Definition upper_clocks (o : option (clock_constraint root)) : list (Clock root) :=
  match o with
  | Some k => if is_upper (guard_comparison k) then [guard_clock k] else []
  | None => []
  end.

(* Candidate clocks of the invariant of location [l]: those bounded by some
   outgoing guard (a clock outside this list cannot be in the invariant). *)
Definition inv_clocks (A : TBA root) (l : nat) : list (Clock root) :=
  nodup (@clock_eq_dec root) (flat_map (fun t => flat_map upper_clocks (bt_guard t)) (outgoing A l)).

(* The invariant of [l] after the resets [Z], as guard items:
   x <= M for a clock not in Z, and 0 <= M for a clock in Z, which is
   dropped when true and encoded by the unsatisfiable x < 0 when false. *)
Definition inv_guard_item (A : TBA root) (l : nat) (Z : list (Clock root))
    (x : Clock root) : option (clock_constraint root) :=
  match synth_inv A l x with
  | Some M =>
      if In_dec (@clock_eq_dec root) x Z then
        if Rle_dec 0 M then None
        else Some {| guard_clock := x; guard_comparison := CLt;
                     guard_bound := 0 |}
      else Some {| guard_clock := x; guard_comparison := CLe;
                   guard_bound := M |}
  | None => None
  end.

Definition inv_guard (A : TBA root) (l : nat) (Z : list (Clock root))
    : guard root :=
  map (inv_guard_item A l Z) (inv_clocks A l).

(* ====================================================================== *)
(* 3b. Tightening of guards                                               *)
(* ====================================================================== *)

(* Every time a guard is strengthened, the constraints implied by another
   constraint of the guard on the same clock are dropped, so that a guard
   keeps at most one lower bound and one upper bound per clock (an equality
   counting as both). *)

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

(* Adds [c] to a list of constraints unless it is implied by one of them,
   and removes the constraints that [c] implies. *)
Definition insert_t (c : clock_constraint root) (acc : list (clock_constraint root))
    : list (clock_constraint root) :=
  if existsb (fun a => implies_c a c) acc then acc
  else c :: filter (fun a => negb (implies_c c a)) acc.

Lemma insert_t_holds :
  forall v c acc,
    Forall (cc_holds v) (insert_t c acc) <-> cc_holds v c /\ Forall (cc_holds v) acc.
Proof.
  intros v c acc. unfold insert_t.
  destruct (existsb (fun a => implies_c a c) acc) eqn:He.
  - apply existsb_exists in He. destruct He as [a [Ha Hac]].
    split; [|tauto]. intro H. split; [|exact H].
    rewrite Forall_forall in H. exact (implies_c_sound Hac (H a Ha)).
  - rewrite Forall_cons_iff. split.
    + intros [Hc Hf]. split; [exact Hc|].
      rewrite Forall_forall in *. intros a Ha.
      destruct (implies_c c a) eqn:E.
      * exact (implies_c_sound E Hc).
      * apply Hf. apply filter_In. split; [exact Ha|]. rewrite E. reflexivity.
    + intros [Hc Hf]. split; [exact Hc|].
      rewrite Forall_forall in *. intros a Ha.
      apply filter_In in Ha. apply Hf. tauto.
Qed.

Fixpoint present (g : guard root) : list (clock_constraint root) :=
  match g with
  | [] => []
  | Some k :: g' => k :: present g'
  | None :: g' => present g'
  end.

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

Lemma guard_of_list_holds :
  forall (rho : ext_word root) i l,
    Forall (guard_item_holds rho i) (map Some l) <->
    Forall (cc_holds (fun x => ew_val rho i x)) l.
Proof.
  intros rho i l. rewrite Forall_map.
  split; intro H; eapply Forall_impl; try exact H;
    intros k Hk; simpl in *; apply cc_holds_at; exact Hk.
Qed.

Definition tighten (g : guard root) : guard root :=
  map Some (fold_right insert_t [] (present g)).

Lemma tighten_holds :
  forall (rho : ext_word root) i g,
    Forall (guard_item_holds rho i) (tighten g) <->
    Forall (guard_item_holds rho i) g.
Proof.
  intros rho i g. unfold tighten.
  rewrite guard_of_list_holds, (present_holds rho i g).
  induction (present g) as [|c l IH]; simpl; [tauto|].
  rewrite insert_t_holds, IH, Forall_cons_iff. tauto.
Qed.

(* Concatenation of two guards, tightened. *)
Definition conj_guard (g h : guard root) : guard root := tighten (g ++ h).

Lemma conj_guard_holds :
  forall (rho : ext_word root) i g h,
    Forall (guard_item_holds rho i) (conj_guard g h) <->
    Forall (guard_item_holds rho i) g /\ Forall (guard_item_holds rho i) h.
Proof.
  intros rho i g h. unfold conj_guard. rewrite tighten_holds, Forall_app. tauto.
Qed.

(* g := g /\ Inv(l')[Z := 0] for a transition l --g,Z--> l'. *)
Definition strengthen_transition (A : TBA root) (t : tba_transition root)
    : tba_transition root :=
  {| bt_source := bt_source t;
     bt_label := bt_label t;
     bt_guard := conj_guard (bt_guard t) (inv_guard A (bt_target t) (bt_resets t));
     bt_resets := bt_resets t;
     bt_target := bt_target t |}.

(* One propagation step: every transition is strengthened with the
   synthesized invariant of its target. *)
Definition propagate (A : TBA root) : TBA root :=
  {| tba_nstates := tba_nstates A;
     tba_init := tba_init A;
     tba_transitions := map (strengthen_transition A) (tba_transitions A);
     tba_accepting := tba_accepting A |}.

(* [n] iterations of backward propagation. *)
Fixpoint propagate_n (n : nat) (A : TBA root) : TBA root :=
  match n with
  | O => A
  | S n' => propagate_n n' (propagate A)
  end.

(* A strengthened transition is enabled only if the original one is. *)
Lemma strengthen_enabled_sound :
  forall A (rho : ext_word root) i t,
    tba_transition_enabled rho i (strengthen_transition A t) ->
    tba_transition_enabled rho i t.
Proof.
  intros A rho i t [Hlab [Hguard Hres]].
  simpl in *.
  split; [exact Hlab|]. split; [|exact Hres].
  apply conj_guard_holds in Hguard. exact (proj1 Hguard).
Qed.

Lemma propagate_sound :
  forall A (rho : ext_word root),
    TBA_ext_accepts (propagate A) rho -> TBA_ext_accepts A rho.
Proof.
  intros A rho [run [Hinit [Hsteps Hbuchi]]].
  exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
  intro i.
  destruct (Hsteps i) as [Hbound [tt [Hin [Hsrc [Hdst Hen]]]]].
  split; [exact Hbound|].
  simpl in Hin. apply in_map_iff in Hin.
  destruct Hin as [t [<- Hin]].
  exists t. split; [exact Hin|].
  split; [exact Hsrc|]. split; [exact Hdst|].
  exact (strengthen_enabled_sound Hen).
Qed.

(* Completeness: along an accepting run over a clock-consistent extended
   word, the invariant of the next location, evaluated after the resets,
   holds when the transition is taken. *)
Lemma inv_guard_holds :
  forall A (rho : ext_word root) i t t',
    clock_consistent rho ->
    tba_transition_enabled rho i t ->
    In t' (tba_transitions A) ->
    bt_source t' = bt_target t ->
    tba_transition_enabled rho (S i) t' ->
    Forall (guard_item_holds rho i) (inv_guard A (bt_target t) (bt_resets t)).
Proof.
  intros A rho i t t' Hcc [_ [_ Hres]] Hin' Hsrc' Hen'.
  pose proof (enabled_transition_satisfies_invariant Hin' Hen') as Hinv.
  rewrite Hsrc' in Hinv.
  apply Forall_forall. intros o Ho.
  unfold inv_guard in Ho. apply in_map_iff in Ho.
  destruct Ho as [x [<- _]].
  unfold inv_guard_item.
  destruct (synth_inv A (bt_target t) x) as [M|] eqn:HM; [|exact I].
  pose proof (Hinv x M HM) as Hnext. simpl in Hnext.
  destruct Hcc as [_ Hstep].
  specialize (Hstep i x).
  pose proof (delta_positive (ew_base rho) i) as Hdpos.
  destruct (In_dec (@clock_eq_dec root) x (bt_resets t)) as [HZ|HZ].
  - assert (Hr : ew_reset rho i x = true) by (apply (proj2 (Hres x)); exact HZ).
    rewrite Hr in Hstep.
    destruct (Rle_dec 0 M) as [_|Hn]; [exact I|].
    exfalso. apply Hn. lra.
  - assert (Hr : ew_reset rho i x = false).
    { destruct (ew_reset rho i x) eqn:E; [|reflexivity].
      exfalso. apply HZ. apply (proj1 (Hres x)). exact E. }
    rewrite Hr in Hstep.
    simpl. unfold clock_constraint_holds. simpl. lra.
Qed.

Lemma propagate_complete :
  forall A (rho : ext_word root),
    clock_consistent rho ->
    TBA_ext_accepts A rho -> TBA_ext_accepts (propagate A) rho.
Proof.
  intros A rho Hcc [run [Hinit [Hsteps Hbuchi]]].
  exists run. split; [exact Hinit|]. split; [|exact Hbuchi].
  intro i.
  destruct (Hsteps i) as [Hbound [t [Hin [Hsrc [Hdst Hen]]]]].
  destruct (Hsteps (S i)) as [_ [t' [Hin' [Hsrc' [_ Hen']]]]].
  split; [exact Hbound|].
  exists (strengthen_transition A t).
  split; [simpl; apply in_map; exact Hin|].
  split; [exact Hsrc|]. split; [exact Hdst|].
  destruct Hen as [Hlab [Hguard Hres]].
  split; [exact Hlab|]. split; [|exact Hres].
  simpl. apply conj_guard_holds. split; [exact Hguard|].
  apply (@inv_guard_holds A rho i t t' Hcc); try assumption.
  - split; [exact Hlab|]. split; assumption.
  - rewrite Hsrc', Hdst. reflexivity.
Qed.

Theorem propagate_accepts :
  forall A w, TBA_accepts A w <-> TBA_accepts (propagate A) w.
Proof.
  intros A w. unfold TBA_accepts. split.
  - intros [rho [Hb [Hc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hc|].
    exact (propagate_complete Hc Ha).
  - intros [rho [Hb [Hc Ha]]]. exists rho.
    split; [exact Hb|]. split; [exact Hc|].
    exact (propagate_sound Ha).
Qed.

Theorem propagate_n_accepts :
  forall n A w, TBA_accepts A w <-> TBA_accepts (propagate_n n A) w.
Proof.
  induction n as [|n IH]; intros A w; simpl.
  - reflexivity.
  - rewrite (propagate_accepts A w). apply IH.
Qed.

(* Invariants synthesized after [n] propagation steps. *)
Definition propagated_invariants (n : nat) (A : TBA root) : TBAI :=
  add_invariants (propagate_n n A).

Theorem propagated_invariants_accepts :
  forall n A w,
    TBA_accepts A w <-> TBAI_accepts (propagated_invariants n A) w.
Proof.
  intros n A w.
  rewrite (propagate_n_accepts n A w).
  apply add_invariants_accepts.
Qed.

End Invariants.

(* ====================================================================== *)
(* 6. End-to-end correctness with invariants                              *)
(* ====================================================================== *)

Definition compile_with_invariants (f : mtl) : TBAI f :=
  add_invariants (compile f).

Theorem MTL_to_TBAI_correct :
  forall (f : mtl) (w : timed_word),
    well_formed f ->
    (msat w 0 f <-> TBAI_accepts (compile_with_invariants f) w).
Proof.
  intros f w Hwf.
  rewrite (MTL_to_TBA_correct w Hwf).
  apply add_invariants_accepts.
Qed.

Definition compile_with_propagated_invariants (n : nat) (f : mtl) : TBAI f :=
  propagated_invariants n (compile f).

Theorem MTL_to_TBAI_propagated_correct :
  forall (n : nat) (f : mtl) (w : timed_word),
    well_formed f ->
    (msat w 0 f <->
     TBAI_accepts (compile_with_propagated_invariants n f) w).
Proof.
  intros n f w Hwf.
  rewrite (MTL_to_TBA_correct w Hwf).
  apply propagated_invariants_accepts.
Qed.

Print Assumptions MTL_to_TBAI_correct.
Print Assumptions MTL_to_TBAI_propagated_correct.
