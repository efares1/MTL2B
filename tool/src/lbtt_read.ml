(* Reader for the LBTT automata produced by Spot (ltl2tgba --lbtt).

   Unlike the parser of the prototype, it keeps the literals of Spot
   exactly as they are: a negated atom stays a negative literal, so that the
   automaton given to the verified reset completion is the automaton of
   Spot.  Guards are turned into disjunctive normal form; each cube becomes
   one transition. *)

type form =
  | T
  | F
  | Atom of string
  | Not of form
  | And of form * form
  | Or of form * form

type transition = {
  src : int;
  dst : int;
  marked : bool;                     (* carries an acceptance mark *)
  cubes : (string * bool) list list; (* disjunction of cubes of literals *)
}

type automaton = {
  nstates : int;
  init : int;
  trans : transition list;
  accepting : int list;              (* state-based acceptance, if any *)
  trans_based : bool;
}

(* ------------------------------------------------------------------ *)
(* Tokens                                                              *)

type token = Int of int | Str of string | Sym of string

let tokenize (s : string) : token list =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' -> go (i + 1) acc
      | '"' ->
        let j = String.index_from s (i + 1) '"' in
        go (j + 1) (Str (String.sub s (i + 1) (j - i - 1)) :: acc)
      | _ ->
        let j = ref i in
        while !j < n && not (List.mem s.[!j] [' '; '\t'; '\n'; '\r'; '"']) do incr j done;
        let w = String.sub s i (!j - i) in
        let tok = match int_of_string_opt w with Some k -> Int k | None -> Sym w in
        go !j (tok :: acc)
  in
  go 0 []

(* ------------------------------------------------------------------ *)
(* Guards (prefix notation)                                            *)

let rec parse_form = function
  | Sym "t" :: r -> (T, r)
  | Sym "f" :: r -> (F, r)
  | Str a :: r -> (Atom a, r)
  | Sym a :: r when not (List.mem a [ "!"; "&"; "|"; "i"; "e"; "^" ]) ->
    (* Spot writes the atoms that are plain identifiers without quotes *)
    (Atom a, r)
  | Sym "!" :: r -> let (p, r) = parse_form r in (Not p, r)
  | Sym ("&" | "|" | "i" | "e" | "^" as op) :: r ->
    let (p, r) = parse_form r in
    let (q, r) = parse_form r in
    let f = match op with
      | "&" -> And (p, q)
      | "|" -> Or (p, q)
      | "i" -> Or (Not p, q)
      | "e" -> Or (And (p, q), And (Not p, Not q))
      | _ -> Or (And (p, Not q), And (Not p, q)) in
    (f, r)
  | _ -> failwith "LBTT: malformed guard"

(* Disjunctive normal form, literals as (atom, polarity). *)
let rec dnf pos = function
  | T -> if pos then [ [] ] else []
  | F -> if pos then [] else [ [] ]
  | Atom a -> [ [ (a, pos) ] ]
  | Not p -> dnf (not pos) p
  | And (p, q) when pos -> conj (dnf true p) (dnf true q)
  | Or (p, q) when pos -> dnf true p @ dnf true q
  | And (p, q) -> dnf false p @ dnf false q
  | Or (p, q) -> conj (dnf false p) (dnf false q)
and conj g1 g2 = List.concat_map (fun c1 -> List.map (fun c2 -> c1 @ c2) g2) g1

(* ------------------------------------------------------------------ *)
(* Automaton                                                           *)

let parse (text : string) : automaton =
  let header = match tokenize text with
    | Int n :: Sym kind :: rest -> Some (n, String.contains kind 't', rest)
    | Int n :: Int _ :: rest -> Some (n, false, rest)
    | _ -> None in
  match header with
  | Some (nstates, trans_based, rest) ->
    let init = ref (-1) and acc = ref [] and trans = ref [] in
    let rec states k toks =
      if k = 0 then ()
      else
        match toks with
        | Int s :: Int i :: r ->
          if i = 1 then init := s;
          (* state-based acceptance sets *)
          let rec accs r = match r with
            | Int (-1) :: r -> r
            | Int _ :: r -> acc := s :: !acc; accs r
            | _ -> r in
          let r = if trans_based then r else accs r in
          let rec trs r = match r with
            | Int (-1) :: r -> r
            | Int d :: r ->
              let rec marks m r = match r with
                | Int (-1) :: r -> (m, r)
                | Int _ :: r -> marks true r
                | _ -> failwith "LBTT: malformed transition" in
              let (m, r) = marks false r in
              let (g, r) = parse_form r in
              trans := { src = s; dst = d; marked = m; cubes = dnf true g } :: !trans;
              trs r
            | _ -> failwith "LBTT: malformed transition list" in
          states (k - 1) (trs r)
        | _ -> failwith "LBTT: malformed state" in
    states nstates rest;
    { nstates; init = !init; trans = List.rev !trans;
      accepting = List.sort_uniq compare !acc; trans_based }
  | _ -> failwith "LBTT: malformed header"
