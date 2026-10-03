# MTL(0,inf) to timed Büchi automata: Coq development

Mechanized correctness of the translation of MTL(0,inf), with hatted
operators, into timed Büchi automata, of its post-processing toward UPPAAL,
and the OCaml code extracted from it.

| File | Content |
|---|---|
| `MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core.v` | syntax, semantics, clocked-LTL translation, Büchi and timed automata, LTL-to-Büchi axiom |
| `EncodingCorrect_Shared_Clock_Derived_Strict_Direct_Proof.v` | correctness of the encoding, end-to-end theorem `MTL_to_TBA_correct` |
| `MTL_to_TBA_Invariants.v` | location invariants, invariant synthesis, backward propagation, tightening of guards |
| `MTL_to_TBA_Optimizations.v` | forward propagation, simplification, dead resets, clock merging, normalization, iterated pipeline |
| `MTL_to_TBA_Export.v` | transition merging, elimination of disjunctive invariants and guards, `MTL_to_exported_correct` |
| `Extract_Optim.v` | extraction of `optimized`, `optimize`, `export`, `optimize_export` to `optim.ml` |
| `tools/prune_extraction.py` | removes the unused code of the real-number library from `optim.ml` |
| `optim_test.ml`, `optim_test_ex1.ml` | Examples 3 and 1 of the paper run through the extracted code |

## Build

    make          # compiles the Coq files (Rocq 9) and extracts optim.ml
    make test     # compiles and runs the OCaml tests
    make zip      # builds mtl2tba.zip

The only project axiom is `LTL_TO_BUCHI_CORRECT`; `Print Assumptions` at the
end of the proof, optimization, and export files lists it together with the
axioms of the standard library of real numbers.
