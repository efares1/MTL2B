open Mtl

let rec apply f = function
| Or(p1,p2) -> f (Or (apply f p1, apply f p2))
| And(p1,p2) -> f (And (apply f p1, apply f p2))
| Until(c,p1,p2) -> f (Until(c,apply f p1, apply f p2))
| XUntil(c,p1,p2) -> f (XUntil(c, apply f p1, apply f p2))
| Release(c,p1,p2) -> f (Release (c, apply f p1, apply f p2))
| XRelease(c,p1,p2) -> f (XRelease (c, apply f p1, apply f p2))
| Next p -> f (Next (apply f p))
| p -> f p

let elim_Ule_Rle_Rge =
  let elim_Ule_Rle_Rge  = function
      Until((LE _|LT _) as c, p1, p2) -> mk_or p2 (And(p1, XUntil(c, p1, p2)))
    | Release((LE _|LT _) as c, p1, p2) -> mk_and p2 (Or(p1, XRelease(c, p1,p2)))
    | Until(GE 0, p1, p2) -> Until(Untimed, p1, p2)
    | XUntil(GE 0, p1, p2) -> XUntil(Untimed, p1, p2)
    | Release(GE 0, p1, p2) -> Release(Untimed, p1, p2)
    | XRelease(GE 0, p1, p2) -> XRelease(Untimed, p1, p2)
    | Release((GE _|GT _) as c, p1, p2) -> mk_or p1 (XRelease(c, p1,p2))
    | p -> p in
  apply elim_Ule_Rle_Rge

let rec add_next = function
  | True -> True
  | False -> False
  | Or (p1,p2) -> Or(add_next p1, add_next p2)
  | And (p1,p2) -> And(add_next p1, add_next p2)
  | Until(c,p1,p2) -> XUntil(c,p1,p2)
  | Release(c,p1,p2) -> XRelease(c,p1,p2)
  | XUntil(c,p1,p2) -> XUntil(c,Next (Or(p1,p2)), p2)
  | XRelease(c,p1,p2) -> XRelease(c,Next (And(p1,p2)), p2)
  | p -> Next(p)

let add_init p = And(Event "_init_", add_next p)

let close_alpha alpha p =
  let evts = get_evts p in
  let ors = List.fold_left (fun r e -> mk_or (Event e) r)
              (Event alpha) evts
  in
  And (p, mtl_box Untimed ors)
