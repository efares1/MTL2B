#!/bin/bash
# Evaluation of mtl2tba on the formulas of formulas.tsv.
# Run in WSL Ubuntu (Spot, dune, menhir installed):
#     bash run_tool_eval.sh
# Writes ours_results.tsv and the automata in out/.
HERE="$(cd "$(dirname "$0")" && pwd)"
TOOL="$HERE/../tool"
B=~/mtl2tba_eval
rm -rf "$B" && cp -r "$TOOL" "$B" && (cd "$B" && rm -rf _build && dune build 2>&1 | head -20)
EXE="$B/_build/default/src/mtl2tba.exe"
LIMIT=${LIMIT:-300}
mkdir -p "$HERE/out"
OUT="$HERE/ours_results.tsv"
printf 'id\tstatus\t%s\n' "$("$EXE" -stats-header)" > "$OUT"
grep -v '^#' "$HERE/formulas.tsv" | while IFS=$'\t' read -r id desc ours cas; do
  [ -z "$id" ] && continue
  line=$(cd "$HERE/out" && timeout "$LIMIT" "$EXE" -stats -nopdf -o "$id" "$ours" 2>"$HERE/out/$id.err")
  code=$?
  if [ $code -eq 0 ]; then
    printf '%s\tok\t%s\n' "$id" "$line" >> "$OUT"
  elif [ $code -eq 124 ]; then
    printf '%s\ttimeout\n' "$id" >> "$OUT"
  else
    printf '%s\terror\n' "$id" >> "$OUT"
  fi
  echo "$id: $code"
done
column -t -s $'\t' "$OUT"
