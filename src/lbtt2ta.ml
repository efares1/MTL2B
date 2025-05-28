let lbtt2ta_op = function
    Lbtt.LE -> Ta.LE
  | Lbtt.LT -> Ta.LT
  | Lbtt.GE -> Ta.GE
  | Lbtt.GT -> Ta.GT

let rec lbtt2ta_ctr = function
    Lbtt.True -> Ta.True
  | Lbtt.False -> Ta.False
  | Lbtt.Clk(x,op,d) -> Ta.Clk(x,lbtt2ta_op op,d)
  | Lbtt.And(c1,c2) -> Ta.mk_and (lbtt2ta_ctr c1) (lbtt2ta_ctr c2)
  | Lbtt.Or(c1,c2) -> Ta.mk_or (lbtt2ta_ctr c1) (lbtt2ta_ctr c2)
  | _ -> failwith "lbtt2ta_ctr"


let lbtt2ta_evt a init invs ev =
  let swap st = if st=0 then init else if st=init then 0 else st in
  let trs = Lbtt.get_trs a ev in
  let acts = Lbtt.combine_trs trs in
  List.map
    (fun ((d,a,m),l) ->
      List.map (fun (s,g) ->
          let inv = try List.assoc s invs with _ -> Lbtt.True in
          let g' = lbtt2ta_ctr (Lbtt.rewrite_hyp inv g) in
          Ta.{src=swap s;dst=swap d;mark=m;event=ev;guard=g';reset=a}
        ) l
    ) acts

let lbtt2ta ?(alpha=["others"]) ((nst,typ,_) as a) : Ta.automaton =
  let clks = Lbtt.get_clk a in
  let invs = Lbtt.get_inv a in
  let init = Lbtt.get_init a in
  let events = List.sort_uniq compare (alpha@(List.filter ((<>) "_init_") (Lbtt.get_evts a))) in
  let start =
    List.map (fun (_,d,_,_,_) -> d)
       (List.filter (fun (s,_,_,_,_) -> s=init)
         (Lbtt.get_trs a "_init_")) in
  let trs = List.concat_map (lbtt2ta_evt a init invs) events in
  let ta = Ta.{nbStates=nst-1;
            typ=typ;
            init=start;
            clks=clks;
            events=events;
            invs=List.map (fun (s,c) -> (s,lbtt2ta_ctr c)) invs;
            trans=List.concat trs} in
  ta
