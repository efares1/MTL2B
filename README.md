# mtl2b

```
cd existing_repo
git remote add origin https://gitlab.irit.fr/methodesformelles/mtl2b/mtl2b.git
git branch -M main
git push -uf origin main

```

## Compile
```
  dune build
```

## Execute
with options
  -q: no trace
  -b: generates B machine
  -evb: generates Event-B machine
  -ta: generates pdf automaton
  -xta: generates Uppaal automaton
  -f: MTL formula

```
  _build/install/default/bin/mtl2x.exe -q -b -evb -ta -xta -f '[][<=2]([][<=3]p)'
```

## mtl2tba: verified chain (tool/, proof/, evaluation/)

`tool/` contains `mtl2tba`, which translates an MTL(0,inf) formula into a
timed automaton for UPPAAL (`.xml`) and its drawing (`.dot`, `.pdf`):

```
  cd tool && dune build
  _build/default/src/mtl2tba.exe -o sporadic '[](e -> ^[][<=2] !e)'
```

Every step except parsing, the call to Spot, and printing is OCaml code
extracted from the Coq development of `proof/` (`make` in `proof/` compiles
the proofs and extracts `optim.ml`; `make tool` rebuilds the tool).
`evaluation/` contains the benchmark formulas, the scripts, and the results
(comparison with CASAAL).  Requirements: OCaml with dune and menhir, Spot
(`ltl2tgba`), Graphviz (`dot`); Rocq 9 to recompile the proofs.
