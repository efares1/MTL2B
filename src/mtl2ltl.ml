(*
https://ceur-ws.org/Vol-1032/paper-41.pdf
https://spinroot.com/spin/symposia/ws17/SPIN_2017_paper_23.pdf
 *)
type prop =
  Atom of string
| Reset of string
| Unch of string
| Ctr of string * Mtl.ctr (* x ~ d *)

let pp_prop oc = function
    Atom s -> Printf.fprintf oc "%s" s
  | Reset s -> Printf.fprintf oc "\"rst(%s)\"" s
  | Unch s -> Printf.fprintf oc "\"unch(%s)\"" s
  | Ctr (x,LE d) -> Printf.fprintf oc "\"%s <= %d\"" x d
  | Ctr (x,LT d) -> Printf.fprintf oc "\"%s < %d\"" x d
  | Ctr (x,GE d) -> Printf.fprintf oc "\"%s >= %d\"" x d
  | Ctr (x,GT d) -> Printf.fprintf oc "\"%s > %d\"" x d
  | Ctr (_,Untimed) -> Printf.fprintf oc "true"

let map = Hashtbl.create 100
let cpt = ref 0

let getClk ru p =
  try
    "x"^ru^(string_of_int (Hashtbl.find map p))
  with
    Not_found -> (Hashtbl.add map p !cpt; incr cpt; "x"^ru^(string_of_int (!cpt-1)))

let rec mtl2ltl = function
  | Mtl.True -> Ltl.True
  | Mtl.False -> Ltl.False
  | Mtl.Event s -> Ltl.Prop (Atom s)
  | Mtl.NEvent s -> Ltl.NProp (Atom s)
  | Mtl.And(p1,p2) -> Ltl.mk_and (mtl2ltl p1) (mtl2ltl p2)
  | Mtl.Or(p1,p2) -> Ltl.mk_or (mtl2ltl p1) (mtl2ltl p2)
  | Mtl.Next p -> Ltl.Next (mtl2ltl p)
  | Mtl.Until(Untimed,p1,p2) -> Ltl.Until(mtl2ltl p1, mtl2ltl p2)
  | Mtl.Release(Untimed,p1,p2) -> Ltl.Release(mtl2ltl p1, mtl2ltl p2)
  | Mtl.Until((LE _ | LT _) as c,p1,p2) ->
     let p = Mtl.mk_or p2 (Mtl.mk_and p1 (Mtl.XUntil (c, p1, p2))) in 
     mtl2ltl p
  | Mtl.Until((GE _ | GT _) as c,p1,p2) ->
     let p = Mtl.mk_and p1 (Mtl.XUntil (c,p1,p2)) in
     mtl2ltl p
  | Mtl.Release((LE _ | LT _) as c,p1,p2) ->
     let p = Mtl.mk_and p2 (Mtl.mk_or p1 (Mtl.XRelease (c,p1,p2))) in
     mtl2ltl p
  | Mtl.Release((GE _ | GT _) as c,p1,p2) ->
     let p = Mtl.mk_or p1 (Mtl.XRelease (c,p1,p2)) in
     mtl2ltl p
  | Mtl.XUntil((LE _ | LT _) as c,p1,p2) as p ->
     let x = getClk "u" p in
     let q1 = mtl2ltl p1 and q2 = mtl2ltl p2 in
     Ltl.Next (Ltl.Until
                 (Ltl.mk_and
                    (Ltl.Prop (Ctr(x,c)))
                    (Ltl.mk_and q1 (Ltl.Prop (Unch x))),
                  (Ltl.mk_and
                     (Ltl.Prop (Ctr(x,c)))
                     q2)))
  | Mtl.XUntil((GE _ | GT _) as c,p1,p2) as p ->
     let x = getClk "r" p in
     let q1 = mtl2ltl p1 and q2 = mtl2ltl p2 in
     Ltl.mk_and (Ltl.Prop (Reset x))
       (Ltl.Next
          (Ltl.mk_or
             (Ltl.Until(q1, Ltl.mk_and (Ltl.Prop (Ctr(x,c)))
                              (Ltl.Until (q1,q2))))
             (Ltl.mk_and (Ltl.mk_G q1) (Ltl.mk_GF q2))))
  | Mtl.XUntil(Untimed,p1,p2) -> Ltl.Next (Ltl.Until(mtl2ltl p1, mtl2ltl p2))
  | Mtl.XRelease((LE _ | LT _) as c,p1,p2) as p ->
     let x = getClk "r" p in
     let q1 = mtl2ltl p1 and q2 = mtl2ltl p2 in
     Ltl.mk_and (Ltl.Prop (Reset x))
       (Ltl.Next
          (Ltl.mk_W q2
             (Ltl.mk_or
                (Ltl.Prop (Ctr(x, Mtl.ctr_not c)))
                (Ltl.mk_and q1 q2))))
  | Mtl.XRelease((GE _ | GT _) as c,p1,p2) as p ->
     let x = getClk "u" p in
     let q1 = mtl2ltl p1 and q2 = mtl2ltl p2 in
     Ltl.Next
       (Ltl.mk_W
          (Ltl.mk_and
             (Ltl.Prop (Ctr(x, Mtl.ctr_not c)))
             (Ltl.Prop (Unch x)))
          (Ltl.mk_or
             (Ltl.Release(q1,q2))
             (Ltl.mk_and
                (Ltl.Prop (Ctr(x, Mtl.ctr_not c)))
                q1)))
  | Mtl.XRelease(Untimed,p1,p2) -> Ltl.Next(Ltl.Release(mtl2ltl p1, mtl2ltl p2))


