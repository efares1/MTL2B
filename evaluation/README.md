# Evaluation

- `formulas.tsv`: the 23 benchmark formulas, in the syntax of `mtl2tba` and
  in the syntax of CASAAL (`~` marks a CASAAL encoding that is not
  equivalent: CASAAL has no hatted operators).
- `run_tool_eval.sh`: runs `mtl2tba -stats` on every formula (Linux/WSL,
  requires Spot, dune, menhir); writes `ours_results.tsv` and the automata
  in `out/`.
- `run_casaal.py`: runs CASAAL (Windows executable) on every formula; writes
  `casaal_results.tsv`.
- `make_tables.py`: joins both into `results.tsv` and the LaTeX table
  `results_table.tex`.
- `results.md`: summary of the observations.
- `setup_wsl.sh`: one-time installation of Spot, OCaml, dune, and menhir in
  WSL Ubuntu.

## Steps

1. Windows PowerShell as administrator: `wsl --install -d Ubuntu`, reboot,
   open Ubuntu once and create the Linux user.
2. In Ubuntu, from this folder: `sudo bash setup_wsl.sh`
3. In Ubuntu: `bash run_tool_eval.sh`
4. On Windows: `python run_casaal.py <casaal folder>`
5. `python make_tables.py`
