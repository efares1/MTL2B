echo $* | _build/default/src/main.exe -q | ltl2tgba --lbtt=t -d - | dot -T pdf > out.pdf

