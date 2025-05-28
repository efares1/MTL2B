echo $* | _build/install/default/bin/mtl2ltl.exe -q | ltl2tgba --lbtt=t - | _build/install/default/bin/lbtt2b.exe -cr -pr -dta out.dot -xta out.xta -evb out.mch -b outb.mch
dot -Tpdf out.dot > out.pdf

# auto de Buchi --> -b
