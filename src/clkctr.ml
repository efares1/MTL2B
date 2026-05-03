type op = LE | LT | GE | GT
let op_of_string = function
    "<=" -> LE
  | "<" -> LT
  | ">=" -> GE
  | ">" -> GT
  | s -> failwith ("op_of_string:"^s)

let mk_opnot = function
    LE -> GT
  | LT -> GE
  | GE -> LT
  | GT -> LE

type clk = string * op * int

let clk_contradict (x,o,d1) (x',o',d2) =
  x=x' && match o,o' with
            LT,GT -> d1 < d2
          | LT,GE -> d1 <= d2
          | LE,GT -> d1 <= d2
          | LE,GE -> d1 < d2

          | GT,LT -> d1 > d2
          | GT,LE -> d1 >= d2
          | GE,LT -> d1 >= d2
          | GE,LE ->  d1 > d2

          | _ -> false

let clk_weaker (x,o,d1) (x',o',d2) =
  x=x' && match o,o' with
            LT,LT -> d1 >= d2
          | LT,LE -> d1 > d2
          | LE,LT -> d1 >= d2
          | LE,LE -> d1 >= d2

          | GT,GT -> d2 >= d1
          | GT,GE -> d2 > d1
          | GE,GT -> d2 >= d1
          | GE,GE -> d2 >= d1
          | _ -> false
