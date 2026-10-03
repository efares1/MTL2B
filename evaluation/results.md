# Evaluation results (2026-10-03)

Tool: `mtl2tba` (tool/), Spot 2.16 (`ltl2tgba -B -D --lbtt=t`), WSL Ubuntu.
CASAAL: `casaal.exe` (Windows). Data: `results.tsv`; LaTeX table:
`results_table.tex`; automata: `out/`.

## Main observations

- All 23 formulas are translated; the whole chain takes less than 0.06 s
  except R3 (0.22 s), R4 (2.6 s), and N4 (3.3 s), where the export
  (elimination of disjunctive invariants) dominates.
- Clocks: the number of clocks of the optimized automaton never exceeds the
  number of distinct timed subformulas and equals the number of clocks of
  CASAAL, except where it is smaller:
  - F2 and F3b: 0 clocks.  With one event per position, `[](p -> [][<=3] q)`
    forces `p` never to occur (the window contains the current position), so
    no clock is needed; CASAAL, which reads propositions, keeps 1 clock for F2.
  - F11 (Example 3): 1 clock instead of 2 (verified merging of synchronously
    reset clocks).
- Optimization: up to 83% fewer transitions (R4: 1625 to 276), 67% (R3),
  63% (N4: 501 to 184), 61% (F13), 47% (N3), 43% (F12).  The merging of
  transitions (subsumption, resolution of complementary literals) accounts
  for part of it (N4: 378 to 184 transitions, 1594 to 1035 after export;
  F5: 35 to 25).
- Merging of states with the same Buchi marking and the same outgoing
  transitions finds nothing to merge on these formulas: Spot already
  reduces its automata, and the remaining twin states differ in their Buchi
  marking (copies created by the state-based Buchi construction, e.g. L1 and
  L2 for F5).
- Size compared with CASAAL: CASAAL automata are smaller, notably for nested
  formulas (N4: 6 states/20 transitions vs 17 locations/184 transitions).
  Caveats: our transitions are cubes (one per conjunction of literals) whereas
  CASAAL edges carry Boolean formulas; Spot sees `!e`, `rst(x)`, `unch(x)` as
  independent propositions (needed by the correctness theorem) and is asked
  for a state-based Buchi automaton; CASAAL reads propositions, our tool
  events (one per position), so the languages differ.
- Export for UPPAAL: difference constraints appear when disjunctive invariants
  are eliminated (R3: 78, R4: 606, N4: 672), and the number of transitions
  grows (N4: 184 to 1035).
- CASAAL cannot express the hatted formulas F3 and F13; the encodings marked
  with a dagger are not equivalent.
