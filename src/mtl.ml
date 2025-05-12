type ctr =
  LE of int
| LT of int
| GE of int
| GT of int
| Untimed

let ctr_not = function
    LE d -> GT d
  | LT d -> GE d
  | GE d -> LT d
  | GT d -> LE d
  | Untimed -> Untimed

type mtl =
  Event of string
| NEvent of string
| True
| False
| Next of mtl
| Or of mtl * mtl
| And of mtl * mtl
| Until of ctr * mtl * mtl
| XUntil of ctr * mtl * mtl
| Release of ctr * mtl * mtl
| XRelease of ctr * mtl * mtl

let rec get_evts = function
    Event s | NEvent s -> [s]
  | Next m -> get_evts m
  | Or(m1,m2) | And(m1,m2) | Until(_,m1,m2) | XUntil(_,m1,m2)
    | Release(_,m1,m2) | XRelease(_,m1,m2)
    -> List.sort_uniq compare (get_evts m1 @ get_evts m2)
  | _ -> []

let mk_and p1 p2 =
  if p1 = p2 then p1
  else
    match p1,p2 with
      True, _ -> p2
    | _, True -> p1
    | False, _ -> p1
    | _, False -> p2
    | _,_ -> And(p1,p2)

let mk_or p1 p2 =
  if p1 = p2 then p1
  else
    match p1,p2 with
      True, _ -> p1
    | _, True -> p2
    | False, _ -> p2
    | _, False -> p1
    | _,_ -> Or(p1,p2)

let rec mtl_not = function
  Event s -> NEvent s
| NEvent s -> Event s
| True -> False
| False -> True
| Next p -> Next (mtl_not p)
| Or(p1,p2) -> And (mtl_not p1, mtl_not p2)
| And(p1,p2) -> Or (mtl_not p1, mtl_not p2)
| Until(c,p1,p2) -> Release(c,mtl_not p1, mtl_not p2)
| XUntil(c,p1,p2) -> XRelease(c,mtl_not p1, mtl_not p2)
| Release(c,p1,p2) -> Until(c,mtl_not p1, mtl_not p2)
| XRelease(c,p1,p2) -> XUntil(c,mtl_not p1, mtl_not p2)

let mtl_box c p = Release(c,False,p)
let mtl_xbox c p = XRelease(c,False,p)
let mtl_diam c p = Until(c,True,p)
let mtl_xdiam c p = XUntil(c,True,p)

let pp_ctr oc = function
    LE d -> Format.fprintf oc "[<= %d]" d
  | LT d -> Format.fprintf oc "[< %d]" d
  | GT d -> Format.fprintf oc "[> %d]" d
  | GE d -> Format.fprintf oc "[>= %d]" d
  | Untimed -> ()

let rec pp_mtl oc = function
  | True -> Format.fprintf oc "true"
  | False -> Format.fprintf oc "false"
  | Event s -> Format.fprintf oc "%s" s
  | NEvent s -> Format.fprintf oc "!%s" s
  | Next p -> Format.fprintf oc "X (%a)" pp_mtl p
  | And(p1,p2) -> Format.fprintf oc "(%a & %a)" pp_mtl p1 pp_mtl p2
  | Or(p1,p2) -> Format.fprintf oc "(%a | %a)" pp_mtl p1 pp_mtl p2
  | Until(c,p1,p2) -> Format.fprintf oc "(%a U%a %a)" pp_mtl p1 pp_ctr c pp_mtl p2
  | XUntil(c,p1,p2) -> Format.fprintf oc "(%a ^U%a %a)" pp_mtl p1 pp_ctr c pp_mtl p2
  | Release(c,p1,p2) -> Format.fprintf oc "(%a R%a %a)" pp_mtl p1 pp_ctr c pp_mtl p2
  | XRelease(c,p1,p2) -> Format.fprintf oc "(%a ^R%a %a)" pp_mtl p1 pp_ctr c pp_mtl p2

(***************** EVB product printer ***************)

let ppp_mtl oc nm fl =
  let evts = List.sort_uniq compare (List.concat_map (fun (_,m)->get_evts m) fl) in
  Format.fprintf oc "machine %s\n" nm;
  List.iter (fun (n,_) -> Format.fprintf oc "  includes %s.%s\n" n n) fl;
  Format.fprintf oc "events\n";
  Format.fprintf oc "  event INITIALISATION\n";
  List.iter (fun (n,_) -> Format.fprintf oc "    synchronizes %s.INITIALISATION\n" n) fl;
  Format.fprintf oc "  end\n";
  List.iter (fun ev ->
      Format.fprintf oc "  event %s\n" ev;
      List.iter (fun s ->
          Format.fprintf oc "    synchronizes %s.%s\n" s ev
        ) (List.map fst (List.filter (fun (_,m) -> List.mem ev (get_evts m)) fl));
      Format.fprintf oc "  end\n") evts;
  Format.fprintf oc "end\n"
