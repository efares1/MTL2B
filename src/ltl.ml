type 'a ltl =
  Prop of 'a
| NProp of 'a
| True
| False
| Or of 'a ltl * 'a ltl
| And of 'a ltl * 'a ltl
| Next of 'a ltl
| Until of 'a ltl * 'a ltl
| Release of 'a ltl * 'a ltl

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

let rec pp_ltl pp_p oc = function
  | True -> Printf.fprintf oc "true"
  | False -> Printf.fprintf oc "false"
  | Prop p -> Printf.fprintf oc "%a" pp_p p
  | NProp p -> Printf.fprintf oc "!%a" pp_p p
  | And(p1,p2) -> Printf.fprintf oc "(%a & %a)" (pp_ltl pp_p) p1 (pp_ltl pp_p) p2
  | Or(p1,p2) -> Printf.fprintf oc "(%a | %a)" (pp_ltl pp_p) p1 (pp_ltl pp_p) p2
  | Next p -> Printf.fprintf oc "X(%a)" (pp_ltl pp_p) p
  | Until(p1,p2) -> Printf.fprintf oc "(%a U %a)" (pp_ltl pp_p) p1 (pp_ltl pp_p) p2
  | Release(p1,p2) -> Printf.fprintf oc "(%a R %a)" (pp_ltl pp_p) p1 (pp_ltl pp_p) p2

