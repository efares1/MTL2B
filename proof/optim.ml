type action = int

type mtl =
| MTrue
| MFalse
| MAtom of action
| MNotAtom of action
| MAnd of mtl * mtl
| MOr of mtl * mtl
| MNext of mtl
| MU of mtl * mtl
| MR of mtl * mtl
| MUhatLe of float * mtl * mtl
| MUhatGe of float * mtl * mtl
| MRhatLe of float * mtl * mtl
| MRhatGe of float * mtl * mtl
| MUhatLt of float * mtl * mtl
| MUhatGt of float * mtl * mtl
| MRhatLt of float * mtl * mtl
| MRhatGt of float * mtl * mtl

type clock = mtl

(** val mtl_eq_dec : mtl -> mtl -> bool **)

let rec mtl_eq_dec m x =
  match m with
  | MTrue -> (match x with
              | MTrue -> true
              | _ -> false)
  | MFalse -> (match x with
               | MFalse -> true
               | _ -> false)
  | MAtom a -> (match x with
                | MAtom a0 -> (=) a a0
                | _ -> false)
  | MNotAtom a -> (match x with
                   | MNotAtom a0 -> (=) a a0
                   | _ -> false)
  | MAnd (m0, m1) ->
    (match x with
     | MAnd (m2, m3) -> if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
     | _ -> false)
  | MOr (m0, m1) ->
    (match x with
     | MOr (m2, m3) -> if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
     | _ -> false)
  | MNext m0 -> (match x with
                 | MNext m1 -> mtl_eq_dec m0 m1
                 | _ -> false)
  | MU (m0, m1) ->
    (match x with
     | MU (m2, m3) -> if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
     | _ -> false)
  | MR (m0, m1) ->
    (match x with
     | MR (m2, m3) -> if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
     | _ -> false)
  | MUhatLe (r, m0, m1) ->
    (match x with
     | MUhatLe (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)
  | MUhatGe (r, m0, m1) ->
    (match x with
     | MUhatGe (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)
  | MRhatLe (r, m0, m1) ->
    (match x with
     | MRhatLe (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)
  | MRhatGe (r, m0, m1) ->
    (match x with
     | MRhatGe (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)
  | MUhatLt (r, m0, m1) ->
    (match x with
     | MUhatLt (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)
  | MUhatGt (r, m0, m1) ->
    (match x with
     | MUhatGt (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)
  | MRhatLt (r, m0, m1) ->
    (match x with
     | MRhatLt (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)
  | MRhatGt (r, m0, m1) ->
    (match x with
     | MRhatGt (r0, m2, m3) ->
       if (=) r r0
       then if mtl_eq_dec m0 m2 then mtl_eq_dec m1 m3 else false
       else false
     | _ -> false)

(** val clock_eq_dec : mtl -> clock -> clock -> bool **)

let clock_eq_dec _ x y =
  mtl_eq_dec (let a = x in a) (let a = y in a)

type alit = action * bool

type clock_comparison =
| CLe
| CLt
| CGe
| CGt
| CEq

type clock_constraint = { guard_clock : clock;
                          guard_comparison : clock_comparison;
                          guard_bound : float }

type guard = clock_constraint option list

type tba_transition = { bt_source : int; bt_label : alit list;
                        bt_guard : guard; bt_resets : clock list;
                        bt_target : int }

type tBA = { tba_nstates : int; tba_init : int;
             tba_transitions : tba_transition list; tba_accepting : int list }

(** val is_upper : clock_comparison -> bool **)

let is_upper = function
| CGe -> false
| CGt -> false
| _ -> true

(** val clock_eqb : mtl -> clock -> clock -> bool **)

let clock_eqb root x y =
  if clock_eq_dec root x y then true else false

(** val opt_min : float option -> float option -> float option **)

let opt_min a b =
  match a with
  | Some u -> (match b with
               | Some v -> Some (Float.min u v)
               | None -> a)
  | None -> b

(** val ub_item : mtl -> clock -> clock_constraint option -> float option **)

let ub_item root x = function
| Some k ->
  if (&&) (is_upper k.guard_comparison) (clock_eqb root k.guard_clock x)
  then Some k.guard_bound
  else None
| None -> None

(** val ub_guard : mtl -> guard -> clock -> float option **)

let rec ub_guard root g x =
  match g with
  | [] -> None
  | o :: g' -> opt_min (ub_item root x o) (ub_guard root g' x)

(** val max_all : float option list -> float option **)

let rec max_all = function
| [] -> None
| a :: l' ->
  (match l' with
   | [] -> a
   | _ :: _ ->
     (match a with
      | Some u ->
        (match max_all l' with
         | Some v -> Some (Float.max u v)
         | None -> None)
      | None -> None))

(** val outgoing : mtl -> tBA -> int -> tba_transition list **)

let outgoing _ a l =
  List.filter (fun t -> (=) t.bt_source l) a.tba_transitions

(** val synth_inv : mtl -> tBA -> int -> clock -> float option **)

let synth_inv root a l x =
  max_all (List.map (fun t -> ub_guard root t.bt_guard x) (outgoing root a l))

type invariant = clock -> float option

(** val upper_clocks : mtl -> clock_constraint option -> clock list **)

let upper_clocks _ = function
| Some k -> if is_upper k.guard_comparison then k.guard_clock :: [] else []
| None -> []

(** val inv_clocks : mtl -> tBA -> int -> clock list **)

let inv_clocks root a l =
  List.concat_map (fun t -> List.concat_map (upper_clocks root) t.bt_guard)
    (outgoing root a l)

(** val inv_guard_item :
    mtl -> tBA -> int -> clock list -> clock -> clock_constraint option **)

let inv_guard_item root a l z0 x =
  match synth_inv root a l x with
  | Some m ->
    if (fun eq a l -> List.exists (eq a) l) (clock_eq_dec root) x z0
    then if (<=) (float_of_int 0) m
         then None
         else Some { guard_clock = x; guard_comparison = CLt; guard_bound =
                (float_of_int 0) }
    else Some { guard_clock = x; guard_comparison = CLe; guard_bound = m }
  | None -> None

(** val inv_guard : mtl -> tBA -> int -> clock list -> guard **)

let inv_guard root a l z0 =
  List.map (inv_guard_item root a l z0) (inv_clocks root a l)

(** val implies_c : mtl -> clock_constraint -> clock_constraint -> bool **)

let implies_c root a b =
  let u = a.guard_bound in
  let w = b.guard_bound in
  (&&) (clock_eqb root a.guard_clock b.guard_clock)
    (match b.guard_comparison with
     | CLe ->
       (match a.guard_comparison with
        | CGe -> false
        | CGt -> false
        | _ ->  ((<=) u w))
     | CLt ->
       (match a.guard_comparison with
        | CLe ->  ((<) u w)
        | CLt ->  ((<=) u w)
        | CEq ->  ((<) u w)
        | _ -> false)
     | CGe ->
       (match a.guard_comparison with
        | CLe -> false
        | CLt -> false
        | _ ->  ((<=) w u))
     | CGt ->
       (match a.guard_comparison with
        | CLe -> false
        | CLt -> false
        | CGt ->  ((<=) w u)
        | _ ->  ((<) w u))
     | CEq -> (match a.guard_comparison with
               | CEq ->  ((=) u w)
               | _ -> false))

(** val insert_t :
    mtl -> clock_constraint -> clock_constraint list -> clock_constraint list **)

let insert_t root c acc =
  if List.exists (fun a -> implies_c root a c) acc
  then acc
  else c :: (List.filter (fun a -> not (implies_c root c a)) acc)

(** val present : mtl -> guard -> clock_constraint list **)

let rec present root = function
| [] -> []
| o :: g' ->
  (match o with
   | Some k -> k :: (present root g')
   | None -> present root g')

(** val tighten : mtl -> guard -> guard **)

let tighten root g =
  List.map (fun x -> Some x)
    ((fun f a l -> List.fold_right f l a) (insert_t root) [] (present root g))

(** val conj_guard : mtl -> guard -> guard -> guard **)

let conj_guard root g h =
  tighten root (List.append g h)

(** val strengthen_transition :
    mtl -> tBA -> tba_transition -> tba_transition **)

let strengthen_transition root a t =
  { bt_source = t.bt_source; bt_label = t.bt_label; bt_guard =
    (conj_guard root t.bt_guard (inv_guard root a t.bt_target t.bt_resets));
    bt_resets = t.bt_resets; bt_target = t.bt_target }

(** val propagate : mtl -> tBA -> tBA **)

let propagate root a =
  { tba_nstates = a.tba_nstates; tba_init = a.tba_init; tba_transitions =
    (List.map (strengthen_transition root a) a.tba_transitions);
    tba_accepting = a.tba_accepting }

(** val with_transitions : mtl -> tBA -> tba_transition list -> tBA **)

let with_transitions _ a ts =
  { tba_nstates = a.tba_nstates; tba_init = a.tba_init; tba_transitions = ts;
    tba_accepting = a.tba_accepting }

(** val is_lower : clock_comparison -> bool **)

let is_lower = function
| CLe -> false
| CLt -> false
| _ -> true

(** val opt_max : float option -> float option -> float option **)

let opt_max a b =
  match a with
  | Some u -> (match b with
               | Some v -> Some (Float.max u v)
               | None -> a)
  | None -> b

(** val lb_item : mtl -> clock -> clock_constraint option -> float option **)

let lb_item root x = function
| Some k ->
  if (&&) (is_lower k.guard_comparison) (clock_eqb root k.guard_clock x)
  then Some k.guard_bound
  else None
| None -> None

(** val lb_guard : mtl -> guard -> clock -> float option **)

let rec lb_guard root g x =
  match g with
  | [] -> None
  | o :: g' -> opt_max (lb_item root x o) (lb_guard root g' x)

(** val min_all : float option list -> float option **)

let rec min_all = function
| [] -> None
| a :: l' ->
  (match l' with
   | [] -> a
   | _ :: _ ->
     (match a with
      | Some u ->
        (match min_all l' with
         | Some v -> Some (Float.min u v)
         | None -> None)
      | None -> None))

(** val incoming : mtl -> tBA -> int -> tba_transition list **)

let incoming _ a l =
  List.filter (fun t -> (=) t.bt_target l) a.tba_transitions

(** val entry_bound : mtl -> tba_transition -> clock -> float option **)

let entry_bound root t x =
  if (fun eq a l -> List.exists (eq a) l) (clock_eq_dec root) x t.bt_resets
  then Some (float_of_int 0)
  else lb_guard root t.bt_guard x

(** val entry_lb : mtl -> tBA -> int -> clock -> float option **)

let entry_lb root a l x =
  min_all
    (List.append
      (if (=) l a.tba_init then (Some (float_of_int 0)) :: [] else [])
      (List.map (fun t -> entry_bound root t x) (incoming root a l)))

(** val lower_clocks : mtl -> clock_constraint option -> clock list **)

let lower_clocks _ = function
| Some k -> if is_lower k.guard_comparison then k.guard_clock :: [] else []
| None -> []

(** val entry_clocks : mtl -> tBA -> int -> clock list **)

let entry_clocks root a l =
  List.concat_map (fun t ->
    List.append t.bt_resets (List.concat_map (lower_clocks root) t.bt_guard))
    (incoming root a l)

(** val entry_guard : mtl -> tBA -> int -> guard **)

let entry_guard root a l =
  List.map (fun x ->
    match entry_lb root a l x with
    | Some m ->
      Some { guard_clock = x; guard_comparison = CGe; guard_bound = m }
    | None -> None) (entry_clocks root a l)

(** val forward_transition :
    mtl -> tBA -> tba_transition -> tba_transition **)

let forward_transition root a t =
  { bt_source = t.bt_source; bt_label = t.bt_label; bt_guard =
    (conj_guard root t.bt_guard (entry_guard root a t.bt_source));
    bt_resets = t.bt_resets; bt_target = t.bt_target }

(** val forward : mtl -> tBA -> tBA **)

let forward root a =
  with_transitions root a
    (List.map (forward_transition root a) a.tba_transitions)

type tBAIL = { tbail_base : tBA; tbail_up : (int -> invariant);
               tbail_low : (int -> invariant) }

(** val add_two_sided_invariants : mtl -> tBA -> tBAIL **)

let add_two_sided_invariants root a =
  { tbail_base = a; tbail_up = (synth_inv root a); tbail_low =
    (entry_lb root a) }

(** val lower_info :
    mtl -> clock_constraint option -> ((clock * float) * bool) option **)

let lower_info _ = function
| Some k ->
  (match k.guard_comparison with
   | CLe -> None
   | CLt -> None
   | CGt -> Some ((k.guard_clock, k.guard_bound), true)
   | _ -> Some ((k.guard_clock, k.guard_bound), false))
| None -> None

(** val upper_info :
    mtl -> clock_constraint option -> ((clock * float) * bool) option **)

let upper_info _ = function
| Some k ->
  (match k.guard_comparison with
   | CLe -> Some ((k.guard_clock, k.guard_bound), false)
   | CLt -> Some ((k.guard_clock, k.guard_bound), true)
   | CEq -> Some ((k.guard_clock, k.guard_bound), false)
   | _ -> None)
| None -> None

(** val contradictory_bounds : float -> bool -> float -> bool -> bool **)

let contradictory_bounds l sl u su =
  if (<) u l then true else if (=) u l then (||) sl su else false

(** val contra_pair :
    mtl -> clock_constraint option -> clock_constraint option -> bool **)

let contra_pair root a b =
  match lower_info root a with
  | Some p ->
    let (p0, sl) = p in
    let (x, l) = p0 in
    (match upper_info root b with
     | Some p1 ->
       let (p2, su) = p1 in
       let (y, u) = p2 in
       (&&) (clock_eqb root x y) (contradictory_bounds l sl u su)
     | None -> false)
  | None -> false

(** val negative_upper : mtl -> clock_constraint option -> bool **)

let negative_upper root o =
  match upper_info root o with
  | Some p ->
    let (p0, su) = p in
    let (_, u) = p0 in contradictory_bounds (float_of_int 0) false u su
  | None -> false

(** val guard_contradictory : mtl -> guard -> bool **)

let guard_contradictory root g =
  (||) (List.exists (negative_upper root) g)
    (List.exists (fun a -> List.exists (contra_pair root a) g) g)

(** val remove_contradictory : mtl -> tBA -> tBA **)

let remove_contradictory root a =
  with_transitions root a
    (List.filter (fun t -> not (guard_contradictory root t.bt_guard))
      a.tba_transitions)

(** val has_incoming : mtl -> tBA -> int -> bool **)

let has_incoming _ a l =
  List.exists (fun t -> (=) t.bt_target l) a.tba_transitions

(** val remove_unreachable : mtl -> tBA -> tBA **)

let remove_unreachable root a =
  with_transitions root a
    (List.filter (fun t ->
      (||) ((=) t.bt_source a.tba_init) (has_incoming root a t.bt_source))
      a.tba_transitions)

(** val rename_item :
    mtl -> clock -> clock -> clock_constraint option -> clock_constraint
    option **)

let rename_item root x y o = match o with
| Some k ->
  if clock_eqb root k.guard_clock x
  then Some { guard_clock = y; guard_comparison = k.guard_comparison;
         guard_bound = k.guard_bound }
  else o
| None -> None

(** val merge_transition :
    mtl -> clock -> clock -> tba_transition -> tba_transition **)

let merge_transition root x y t =
  { bt_source = t.bt_source; bt_label = t.bt_label; bt_guard =
    (List.map (rename_item root x y) t.bt_guard); bt_resets = t.bt_resets;
    bt_target = t.bt_target }

(** val merge_clock : mtl -> clock -> clock -> tBA -> tBA **)

let merge_clock root x y a =
  with_transitions root a
    (List.map (merge_transition root x y) a.tba_transitions)

(** val mentions : mtl -> clock -> clock_constraint option -> bool **)

let mentions root x = function
| Some k -> clock_eqb root k.guard_clock x
| None -> false

(** val clock_in : mtl -> clock -> clock list -> bool **)

let clock_in root x l =
  if (fun eq a l -> List.exists (eq a) l) (clock_eq_dec root) x l
  then true
  else false

(** val mergeable_b : mtl -> tBA -> clock -> clock -> bool **)

let mergeable_b root a x y =
  (&&)
    ((&&)
      ((&&) (if clock_eq_dec root x y then false else true)
        (List.for_all (fun t ->
          (=) (clock_in root x t.bt_resets) (clock_in root y t.bt_resets))
          a.tba_transitions))
      (List.for_all (fun t -> not ((=) t.bt_target a.tba_init))
        a.tba_transitions))
    (List.for_all (fun t ->
      (||) (not ((=) t.bt_source a.tba_init))
        ((&&) (clock_in root x t.bt_resets)
          (not (List.exists (mentions root x) t.bt_guard))))
      a.tba_transitions)

(** val try_merge : mtl -> clock -> clock -> tBA -> tBA **)

let try_merge root x y a =
  if mergeable_b root a x y then merge_clock root x y a else a

(** val merge_pairs : mtl -> (clock * clock) list -> tBA -> tBA **)

let rec merge_pairs root ps a =
  match ps with
  | [] -> a
  | p :: ps' -> let (x, y) = p in merge_pairs root ps' (try_merge root x y a)

(** val reset_clocks : mtl -> tBA -> clock list **)

let reset_clocks _ a =
  List.concat_map (fun t -> t.bt_resets) a.tba_transitions

(** val merge_all : mtl -> tBA -> tBA **)

let merge_all root a =
  merge_pairs root
    ((fun l1 l2 -> List.concat_map (fun x -> List.map (fun y -> (x, y)) l2) l1)
      (reset_clocks root a) (reset_clocks root a))
    a

(** val tests : mtl -> clock -> tba_transition -> bool **)

let tests root x t =
  List.exists (mentions root x) t.bt_guard

(** val live_step : mtl -> tBA -> clock -> int list -> int list **)

let live_step root a x l =
  List.append l
    (List.map (fun t -> t.bt_source)
      (List.filter (fun t ->
        (||) (tests root x t) (List.exists ((=) t.bt_target) l))
        a.tba_transitions))

(** val live_iter : mtl -> int -> tBA -> clock -> int list -> int list **)

let rec live_iter root n a x l =
  (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
    (fun _ -> l)
    (fun n' -> live_iter root n' a x (live_step root a x l))
    n

(** val live : mtl -> tBA -> clock -> int list **)

let live root a x =
  live_iter root ((fun n -> n + 1) (List.length a.tba_transitions)) a x []

(** val dead : mtl -> tBA -> clock -> int -> bool **)

let dead root a x l =
  not (List.exists ((=) l) (live root a x))

(** val dead_ok : mtl -> tBA -> clock -> bool **)

let dead_ok root a x =
  List.for_all (fun t ->
    (||) (not (dead root a x t.bt_source))
      ((&&) (dead root a x t.bt_target) (not (tests root x t))))
    a.tba_transitions

(** val drop_reset : mtl -> clock -> tba_transition -> tba_transition **)

let drop_reset root x t =
  { bt_source = t.bt_source; bt_label = t.bt_label; bt_guard = t.bt_guard;
    bt_resets =
    (List.filter (fun c -> not (clock_eqb root c x)) t.bt_resets);
    bt_target = t.bt_target }

(** val prune : mtl -> tBA -> clock -> tba_transition -> tba_transition **)

let prune root a x t =
  if dead root a x t.bt_target then drop_reset root x t else t

(** val remove_dead_resets_clock : mtl -> clock -> tBA -> tBA **)

let remove_dead_resets_clock root x a =
  if dead_ok root a x
  then with_transitions root a (List.map (prune root a x) a.tba_transitions)
  else a

(** val remove_dead_resets_list : mtl -> clock list -> tBA -> tBA **)

let rec remove_dead_resets_list root xs a =
  match xs with
  | [] -> a
  | x :: xs' ->
    remove_dead_resets_list root xs' (remove_dead_resets_clock root x a)

(** val remove_dead_resets : mtl -> tBA -> tBA **)

let remove_dead_resets root a =
  remove_dead_resets_list root (reset_clocks root a) a

(** val trivial_c : mtl -> clock_constraint -> bool **)

let trivial_c _ k =
  match k.guard_comparison with
  | CGe ->  ((<=) k.guard_bound (float_of_int 0))
  | CGt ->  ((<) k.guard_bound (float_of_int 0))
  | _ -> false

(** val insert_c :
    mtl -> clock_constraint -> clock_constraint list -> clock_constraint list **)

let insert_c root c acc =
  if trivial_c root c then acc else insert_t root c acc

(** val norm_guard : mtl -> guard -> guard **)

let norm_guard root g =
  List.map (fun x -> Some x)
    ((fun f a l -> List.fold_right f l a) (insert_c root) [] (present root g))

(** val normalize_transition : mtl -> tba_transition -> tba_transition **)

let normalize_transition root t =
  { bt_source = t.bt_source; bt_label = t.bt_label; bt_guard =
    (norm_guard root t.bt_guard); bt_resets = t.bt_resets; bt_target =
    t.bt_target }

(** val normalize : mtl -> tBA -> tBA **)

let normalize root a =
  with_transitions root a
    (List.map (normalize_transition root) a.tba_transitions)

(** val optimize_step : mtl -> tBA -> tBA **)

let optimize_step root a =
  normalize root
    (merge_all root
      (remove_dead_resets root
        (normalize root
          (remove_unreachable root
            (remove_contradictory root (forward root (propagate root a)))))))

(** val optimize : mtl -> int -> tBA -> tBA **)

let rec optimize root n a =
  (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
    (fun _ -> a)
    (fun n' -> optimize root n' (optimize_step root a))
    n

(** val optimized : mtl -> int -> tBA -> tBAIL **)

let optimized root n a =
  add_two_sided_invariants root (optimize root n a)

type dconstraint =
| DSingle of clock_constraint
| DDiff of clock * clock * float

type dconj = dconstraint list

type dguard = dconj list

type uinv = (clock * float) list

type dtrans = { dt_src : int; dt_label : alit list; dt_guard : dguard;
                dt_resets : clock list; dt_tgt : int }

type dTA = { dta_nstates : int; dta_init : int; dta_trans : dtrans list;
             dta_accepting : int list; dta_inv : (int -> uinv list option) }

(** val singles : mtl -> guard -> dconj **)

let rec singles root = function
| [] -> []
| o :: g' ->
  (match o with
   | Some k -> (DSingle k) :: (singles root g')
   | None -> singles root g')

(** val of_tba_trans : mtl -> tba_transition -> dtrans **)

let of_tba_trans root t =
  { dt_src = t.bt_source; dt_label = t.bt_label; dt_guard =
    ((singles root t.bt_guard) :: []); dt_resets = t.bt_resets; dt_tgt =
    t.bt_target }

(** val of_tba : mtl -> tBA -> dTA **)

let of_tba root a =
  { dta_nstates = a.tba_nstates; dta_init = a.tba_init; dta_trans =
    (List.map (of_tba_trans root) a.tba_transitions); dta_accepting =
    a.tba_accepting; dta_inv = (fun _ -> None) }

(** val alit_eq_dec : alit -> alit -> bool **)

let alit_eq_dec a b =
  let (a0, b0) = a in
  let (a1, b1) = b in if (=) a0 a1 then (=) b0 b1 else false

(** val same_key_dec : mtl -> dtrans -> dtrans -> bool **)

let same_key_dec root t u =
  let s = (=) t.dt_src u.dt_src in
  if s
  then let s0 = (=) t.dt_tgt u.dt_tgt in
       if s0
       then let s1 = List.equal alit_eq_dec t.dt_label u.dt_label in
            if s1
            then List.equal (clock_eq_dec root) t.dt_resets u.dt_resets
            else false
       else false
  else false

(** val add_guard : mtl -> dtrans -> dguard -> dtrans **)

let add_guard _ t g =
  { dt_src = t.dt_src; dt_label = t.dt_label; dt_guard =
    (List.append t.dt_guard g); dt_resets = t.dt_resets; dt_tgt = t.dt_tgt }

(** val insert_trans : mtl -> dtrans -> dtrans list -> dtrans list **)

let rec insert_trans root t = function
| [] -> t :: []
| u :: acc' ->
  if same_key_dec root u t
  then (add_guard root u t.dt_guard) :: acc'
  else u :: (insert_trans root t acc')

(** val group : mtl -> dtrans list -> dtrans list **)

let group root ts =
  (fun f a l -> List.fold_right f l a) (insert_trans root) [] ts

(** val merge_transitions : mtl -> dTA -> dTA **)

let merge_transitions root d =
  { dta_nstates = d.dta_nstates; dta_init = d.dta_init; dta_trans =
    (group root d.dta_trans); dta_accepting = d.dta_accepting; dta_inv =
    d.dta_inv }

(** val ub_conj : mtl -> dconj -> uinv **)

let rec ub_conj root = function
| [] -> []
| d :: c' ->
  (match d with
   | DSingle k ->
     List.append
       (if is_upper k.guard_comparison
        then (k.guard_clock, k.guard_bound) :: []
        else [])
       (ub_conj root c')
   | DDiff (_, _, _) -> ub_conj root c')

(** val outgoing_d : mtl -> dTA -> int -> dtrans list **)

let outgoing_d _ d l =
  List.filter (fun t -> (=) t.dt_src l) d.dta_trans

(** val uimplies : mtl -> uinv -> uinv -> bool **)

let uimplies root u u' =
  List.for_all (fun p ->
    List.exists (fun q0 ->
      (&&) (clock_eqb root (fst q0) (fst p)) ( ((<=) (snd q0) (snd p)))) u)
    u'

(** val insert_u : mtl -> uinv -> uinv list -> uinv list **)

let insert_u root u acc =
  if List.exists (fun a -> uimplies root u a) acc
  then acc
  else u :: (List.filter (fun a -> not (uimplies root a u)) acc)

(** val dedupe_u : mtl -> uinv list -> uinv list **)

let dedupe_u root l =
  (fun f a l -> List.fold_right f l a) (insert_u root) [] l

(** val disj_inv : mtl -> dTA -> int -> uinv list **)

let disj_inv root d l =
  dedupe_u root
    (List.concat_map (fun t -> List.map (ub_conj root) t.dt_guard)
      (outgoing_d root d l))

(** val upper_singles : mtl -> uinv -> dconj **)

let upper_singles _ u =
  List.map (fun p -> DSingle { guard_clock = (fst p); guard_comparison = CLe;
    guard_bound = (snd p) }) u

(** val notlonger : mtl -> uinv -> uinv -> dguard **)

let notlonger _ ul uk = match uk with
| [] -> [] :: []
| _ :: _ ->
  List.map (fun q0 ->
    List.map (fun p -> DDiff ((fst p), (fst q0), ((-.) (snd p) (snd q0)))) uk)
    ul

(** val dnf_and : mtl -> dguard -> dguard -> dguard **)

let dnf_and _ g1 g2 =
  List.concat_map (fun c1 -> List.map (fun c2 -> List.append c1 c2) g2) g1

(** val notlonger_all : mtl -> uinv list -> uinv -> dguard **)

let rec notlonger_all root us uk =
  match us with
  | [] -> [] :: []
  | ul :: us' ->
    dnf_and root (notlonger root ul uk) (notlonger_all root us' uk)

(** val entry_cond : mtl -> uinv list -> int -> dguard **)

let entry_cond root us k =
  dnf_and root
    ((upper_singles root
       ((fun n l d -> match List.nth_opt l n with Some x -> x | None -> d) k
         us [])) :: [])
    (notlonger_all root us
      ((fun n l d -> match List.nth_opt l n with Some x -> x | None -> d) k
        us []))

(** val const_true : mtl -> clock_constraint -> bool **)

let const_true _ k =
  match k.guard_comparison with
  | CLe -> if (<=) (float_of_int 0) k.guard_bound then true else false
  | CLt -> if (<) (float_of_int 0) k.guard_bound then true else false
  | CGe -> if (<=) k.guard_bound (float_of_int 0) then true else false
  | CGt -> if (<) k.guard_bound (float_of_int 0) then true else false
  | CEq -> if (=) (float_of_int 0) k.guard_bound then true else false

(** val subst_c : mtl -> clock list -> dconstraint -> dconj option **)

let subst_c root z0 c = match c with
| DSingle k ->
  if (fun eq a l -> List.exists (eq a) l) (clock_eq_dec root) k.guard_clock z0
  then if const_true root k then Some [] else None
  else Some (c :: [])
| DDiff (x, y, b) ->
  if (fun eq a l -> List.exists (eq a) l) (clock_eq_dec root) x z0
  then if (fun eq a l -> List.exists (eq a) l) (clock_eq_dec root) y z0
       then if (<=) (float_of_int 0) b then Some [] else None
       else Some ((DSingle { guard_clock = y; guard_comparison = CGe;
              guard_bound = ((~-.) b) }) :: [])
  else if (fun eq a l -> List.exists (eq a) l) (clock_eq_dec root) y z0
       then Some ((DSingle { guard_clock = x; guard_comparison = CLe;
              guard_bound = b }) :: [])
       else Some (c :: [])

(** val subst_conj : mtl -> clock list -> dconj -> dconj option **)

let rec subst_conj root z0 = function
| [] -> Some []
| a :: c' ->
  (match subst_c root z0 a with
   | Some p ->
     (match subst_conj root z0 c' with
      | Some q0 -> Some (List.append p q0)
      | None -> None)
   | None -> None)

(** val subst_guard : mtl -> clock list -> dguard -> dguard **)

let subst_guard root z0 g =
  List.concat_map (fun c ->
    match subst_conj root z0 c with
    | Some c' -> c' :: []
    | None -> []) g

(** val kmax : mtl -> dTA -> int **)

let kmax root d =
  (List.fold_left max 0)
    (List.map (fun l -> List.length (disj_inv root d l))
      (List.append
        ((fun s n -> List.init n (fun i -> s + i)) 0 d.dta_nstates)
        (List.map (fun t -> t.dt_tgt) d.dta_trans)))

(** val kp : mtl -> dTA -> int **)

let kp root d =
  (fun n -> n + 1) (kmax root d)

(** val enc : mtl -> dTA -> int -> int -> int **)

let enc root d l k =
  (+) (( * ) l (kp root d)) k

(** val split_trans : mtl -> dTA -> dtrans -> dtrans list **)

let split_trans root d t =
  List.concat_map (fun j ->
    List.map (fun k -> { dt_src = (enc root d t.dt_src j); dt_label =
      t.dt_label; dt_guard =
      (dnf_and root t.dt_guard
        (subst_guard root t.dt_resets
          (entry_cond root (disj_inv root d t.dt_tgt) k)));
      dt_resets = t.dt_resets; dt_tgt = (enc root d t.dt_tgt k) })
      ((fun s n -> List.init n (fun i -> s + i)) 0
        (List.length (disj_inv root d t.dt_tgt))))
    ((fun s n -> List.init n (fun i -> s + i)) 0 (kp root d))

(** val split_inv : mtl -> dTA -> int -> uinv list option **)

let split_inv root d s =
  let k = (fun n m -> if m = 0 then n else n mod m) s (kp root d) in
  if (=) k (kmax root d)
  then None
  else (match List.nth_opt
                (disj_inv root d
                  ((fun n m -> if m = 0 then 0 else n / m) s (kp root d)))
                k with
        | Some u -> Some (u :: [])
        | None -> None)

(** val split : mtl -> dTA -> dTA **)

let split root d =
  { dta_nstates = (( * ) d.dta_nstates (kp root d)); dta_init =
    (enc root d d.dta_init (kmax root d)); dta_trans =
    (List.concat_map (split_trans root d) d.dta_trans); dta_accepting =
    (List.concat_map (fun l ->
      List.map (enc root d l)
        ((fun s n -> List.init n (fun i -> s + i)) 0 (kp root d)))
      d.dta_accepting);
    dta_inv = (split_inv root d) }

(** val explode : mtl -> dtrans -> dtrans list **)

let explode _ t =
  List.map (fun c -> { dt_src = t.dt_src; dt_label = t.dt_label; dt_guard =
    (c :: []); dt_resets = t.dt_resets; dt_tgt = t.dt_tgt }) t.dt_guard

(** val explode_all : mtl -> dTA -> dTA **)

let explode_all root d =
  { dta_nstates = d.dta_nstates; dta_init = d.dta_init; dta_trans =
    (List.concat_map (explode root) d.dta_trans); dta_accepting =
    d.dta_accepting; dta_inv = d.dta_inv }

(** val implies_d : mtl -> dconstraint -> dconstraint -> bool **)

let implies_d root a b =
  match a with
  | DSingle k1 ->
    (match b with
     | DSingle k2 -> implies_c root k1 k2
     | DDiff (_, _, _) -> false)
  | DDiff (x, y, u) ->
    (match b with
     | DSingle _ -> false
     | DDiff (x', y', u') ->
       (&&) ((&&) (clock_eqb root x x') (clock_eqb root y y')) ( ((<=) u u')))

(** val trivial_d : mtl -> dconstraint -> bool **)

let trivial_d root = function
| DSingle _ -> false
| DDiff (x, y, b) -> (&&) (clock_eqb root x y) ( ((<=) (float_of_int 0) b))

(** val self_contra : mtl -> dconstraint -> bool **)

let self_contra root = function
| DSingle _ -> false
| DDiff (x, y, b) -> (&&) (clock_eqb root x y) ( ((<) b (float_of_int 0)))

(** val insert_d : mtl -> dconstraint -> dconj -> dconj **)

let insert_d root c acc =
  if trivial_d root c
  then acc
  else if List.exists (fun a -> implies_d root a c) acc
       then acc
       else c :: (List.filter (fun a -> not (implies_d root c a)) acc)

(** val dnorm_conj : mtl -> dconj -> dconj **)

let dnorm_conj root c =
  (fun f a l -> List.fold_right f l a) (insert_d root) [] c

(** val conj_implies : mtl -> dconj -> dconj -> bool **)

let conj_implies root c a =
  List.for_all (fun b -> List.exists (fun x -> implies_d root x b) c) a

(** val insert_g : mtl -> dconj -> dguard -> dguard **)

let insert_g root c acc =
  if List.exists (self_contra root) c
  then acc
  else if List.exists (fun a -> conj_implies root c a) acc
       then acc
       else c :: (List.filter (fun a -> not (conj_implies root a c)) acc)

(** val dnorm_guard : mtl -> dguard -> dguard **)

let dnorm_guard root g =
  (fun f a l -> List.fold_right f l a) (insert_g root) []
    (List.map (dnorm_conj root) g)

(** val normalize_dtrans : mtl -> dtrans -> dtrans **)

let normalize_dtrans root t =
  { dt_src = t.dt_src; dt_label = t.dt_label; dt_guard =
    (dnorm_guard root t.dt_guard); dt_resets = t.dt_resets; dt_tgt =
    t.dt_tgt }

(** val normalize_d : mtl -> dTA -> dTA **)

let normalize_d root d =
  { dta_nstates = d.dta_nstates; dta_init = d.dta_init; dta_trans =
    (List.map (normalize_dtrans root) d.dta_trans); dta_accepting =
    d.dta_accepting; dta_inv = d.dta_inv }

(** val dhas_incoming : mtl -> dTA -> int -> bool **)

let dhas_incoming _ d l =
  List.exists (fun t -> (=) t.dt_tgt l) d.dta_trans

(** val dprune : mtl -> dTA -> dTA **)

let dprune root d =
  { dta_nstates = d.dta_nstates; dta_init = d.dta_init; dta_trans =
    (List.filter (fun t ->
      (||) ((=) t.dt_src d.dta_init) (dhas_incoming root d t.dt_src))
      d.dta_trans);
    dta_accepting = d.dta_accepting; dta_inv = d.dta_inv }

(** val dprune_n : mtl -> int -> dTA -> dTA **)

let rec dprune_n root n d =
  (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
    (fun _ -> d)
    (fun n' ->
    if (=) (List.length (dprune root d).dta_trans) (List.length d.dta_trans)
    then d
    else dprune_n root n' (dprune root d))
    n

(** val dprune_all : mtl -> dTA -> dTA **)

let dprune_all root d =
  dprune_n root (List.length d.dta_trans) d

(** val export : mtl -> tBA -> dTA **)

let export root a =
  dprune_all root
    (explode_all root
      (normalize_d root
        (split root
          (normalize_d root (merge_transitions root (of_tba root a))))))

(** val optimize_export : mtl -> int -> tBA -> dTA **)

let optimize_export root n a =
  export root (optimize root n a)
