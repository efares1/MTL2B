# MTL(0,inf) to timed Büchi automata: Coq development

Mechanized correctness of the translation of MTL(0,inf), with hatted
operators, into timed Büchi automata, of the relaxation and reset completion
of the Büchi automaton returned by the LTL-to-Büchi translator, and of the
post-processing toward UPPAAL; extraction of the code of the tool `mtl2tba`.

| File | Content |
|---|---|
| `MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.v` | syntax, semantics, clocked-LTL translation, Büchi and timed automata, relaxation, reset completion, LTL-to-Büchi axiom |
| `EncodingCorrect_Shared_Clock_Derived_Strict_Direct_Proof.v` | correctness of the encoding, end-to-end theorems `MTL_to_TBA_correct` and `MTL_to_TBA_correct_with` |
| `MTL_to_TBA_Invariants.v` | location invariants, invariant synthesis, backward propagation, tightening of guards |
| `MTL_to_TBA_Optimizations.v` | forward propagation, simplification, dead resets, merging of clocks, transitions, and states, normalization, iterated pipeline |
| `MTL_to_TBA_Export.v` | transition merging, elimination of disjunctive invariants and guards, `MTL_to_exported_correct` |
| `Extract_Optim.v` | extraction to `../tool/src/optim.ml` |
| `tools/prune_extraction.py` | removes the unused code of the real-number library from the extracted file |

## Build

    make          # compiles the Coq files (Rocq 9) and extracts ../tool/src/optim.ml
    make tool     # builds the tool ../tool/_build/default/src/mtl2tba.exe
    make zip      # builds mtl2tba.zip

The only project axiom is `LTL_TO_BUCHI_CORRECT`, the correctness of the
LTL-to-Büchi translator; `MTL_to_TBA_correct_with` takes it as a hypothesis
and uses no project axiom.  `Print Assumptions` at the end of the proof,
optimization, and export files lists the axioms of the standard library of
real numbers.
