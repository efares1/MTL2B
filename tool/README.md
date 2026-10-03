# mtl2tba

From an MTL(0,inf) formula to a timed automaton for UPPAAL (XML) and its
drawing (Graphviz dot).

    mtl2tba [options] 'formula'

      -o <base>    output files <base>.xml, <base>.dot, <base>.pdf (default: out)
      -init        add an initialization event _init_ fixing the time origin
      -n <n>       maximal number of optimization rounds (default: 50)
      -spot <cmd>  LTL-to-Buchi command (default: ltl2tgba)
      -nopdf       do not run Graphviz to produce <base>.pdf
      -dot <cmd>   Graphviz command (default: dot)
      -tba         also write <base>_tba.dot (after reset completion) and
                   <base>_opt.dot (after optimization)
      -v           print the intermediate results

Example:

    mtl2tba -o sporadic '[](e -> ^[][<=2] !e)'
    # writes sporadic.xml (UPPAAL), sporadic.dot and sporadic.pdf (Graphviz)

## Chain

| Step | Code |
|---|---|
| parsing, negation normal form, `-init` | prototype (`mtl.ml`, `lexer.mll`, `parser.mly`, `mtl2mtl.ml`) |
| derivation of the ordinary timed operators | extracted from Coq (`MUle`, ..., `MRgt`) |
| clocked-LTL translation `T` | extracted from Coq |
| LTL to Buchi automaton | Spot, `ltl2tgba -B -D --lbtt=t` |
| reading of the Spot automaton (literals kept) | `lbtt_read.ml` |
| relaxation and reset completion | extracted from Coq (`compile_with`) |
| optimization, iterated to a fixpoint | extracted from Coq (`optimize`) |
| export: conjunctive guards and invariants | extracted from Coq (`export`) |
| UPPAAL and dot output | `output.ml` |
| PDF drawing | Graphviz (`dot -Tpdf`) |

The extracted steps are proved in Coq: for every Buchi automaton `A` that
accepts exactly the propositional models of `T f`,
`MTL_to_TBA_correct_with` states that `compile_with A` accepts exactly the
models of `f`, and `optimize_accepts` and `export_accepts` that the later
steps preserve the language.  Every atom, including a negated event `!e`, is
passed to Spot as an independent quoted proposition, as in the hypothesis of
that theorem.

In the UPPAAL model, the template `Property` receives the events on
channels (`other` stands for every event that does not occur in the
formula) and the template `Env` can emit every event; accepting locations
are named `A<n>`.  UPPAAL does not check Buchi acceptance.

## Build

From the parent folder: `make tool` (requires dune and menhir).
`examples/` contains the output for Examples 1 and 3 of the paper and for
the sporadicity requirement.
