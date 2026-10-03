# Coq development of the MTL(0,inf) -> timed Buchi automata translation,
# its verified post-processing, the extracted OCaml code, and the tool
# mtl2tba built on it.
#
#   make          compile the Coq files and extract optim.ml
#   make tool     build the tool $(TOOLDIR)/_build/default/src/mtl2tba.exe
#   make test     compile and run the OCaml tests on the extracted code
#   make zip      build mtl2tba.zip with all the files
#   make clean    remove the generated files
#
# Requires Rocq/Coq 9 (coqc), OCaml with dune and menhir, Python 3
# (dead-code removal of the extracted file and zip archive), Spot (ltl2tgba)
# to run the tool, and Graphviz (dot) for its pdf output.

COQC    ?= coqc
OCAMLC  ?= ocamlc
DUNE    ?= dune
PYTHON  ?= python3
# folder of the tool mtl2tba (../tool in the GitHub repository)
TOOLDIR ?= $(if $(wildcard tool/src),tool,../tool)
COQFLAGS = -w -deprecated-missing-stdlib \
           -w -extraction-opaque-accessed \
           -w -extraction-axiom-to-realize \
           -w -extraction-default-directory

CORE  = MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core
PROOF = EncodingCorrect_Shared_Clock_Derived_Strict_Direct_Proof
VFILES = $(CORE).v $(PROOF).v MTL_to_TBA_Invariants.v \
         MTL_to_TBA_Optimizations.v MTL_to_TBA_Export.v Extract_Optim.v
TESTS = optim_test.ml optim_test_ex1.ml
ROOTS = optimized optimize export optimize_export t ltl_atoms compile_with \
        timed_subformulas mUle mUlt mUge mUgt mRle mRlt mRge mRgt
TOOLFILES = $(addprefix $(TOOLDIR)/, dune-project src/dune src/mtl.ml src/lexer.mll \
            src/parser.mly src/mtl2mtl.ml src/lbtt_read.ml src/output.ml \
            src/mtl2tba.ml README.md)
ZIP   = mtl2tba.zip
ZIPPY = import sys, zipfile; \
        z = zipfile.ZipFile(sys.argv[1], 'w', zipfile.ZIP_DEFLATED); \
        [z.write(f, f.replace('../', '')) for f in sys.argv[2:]]; z.close()

.PHONY: all tool test zip clean

all: optim.ml

%.vo: %.v
	$(COQC) $(COQFLAGS) $<

$(PROOF).vo: $(CORE).vo
MTL_to_TBA_Invariants.vo: $(PROOF).vo
MTL_to_TBA_Optimizations.vo: MTL_to_TBA_Invariants.vo
MTL_to_TBA_Export.vo: MTL_to_TBA_Optimizations.vo

# Extraction, then removal of the unused real-number library code.
optim.ml: Extract_Optim.v MTL_to_TBA_Export.vo tools/prune_extraction.py
	$(COQC) $(COQFLAGS) Extract_Optim.v
	rm -f optim.mli
	$(PYTHON) tools/prune_extraction.py optim.ml $(ROOTS)

tool: optim.ml
	cp optim.ml $(TOOLDIR)/src/optim.ml
	cd $(TOOLDIR) && $(DUNE) build

test: optim.ml $(TESTS)
	$(OCAMLC) -w -a -o optim_test.exe optim.ml optim_test.ml
	$(OCAMLC) -w -a -o optim_test_ex1.exe optim.ml optim_test_ex1.ml
	./optim_test.exe
	./optim_test_ex1.exe

zip: optim.ml
	rm -f $(ZIP)
	$(PYTHON) -c "$(ZIPPY)" $(ZIP) $(VFILES) optim.ml $(TESTS) \
	  tools/prune_extraction.py Makefile README_proof.md $(TOOLFILES)

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux optim.mli *.cmi *.cmo *.exe $(ZIP)
	rm -rf $(TOOLDIR)/_build $(TOOLDIR)/src/optim.ml
