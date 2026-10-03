# Coq development of the MTL(0,inf) -> timed Buchi automata translation,
# its verified post-processing, and the extracted OCaml optimizer.
#
#   make          compile the Coq files and extract optim.ml
#   make test     compile and run the OCaml tests on the extracted code
#   make zip      build mtl2tba.zip with all the files
#   make clean    remove the generated files
#
# Requires Rocq/Coq 9 (coqc), OCaml (ocamlc), and Python 3 (dead-code
# removal of the extracted file and zip archive).

COQC    ?= coqc
OCAMLC  ?= ocamlc
PYTHON  ?= python3
COQFLAGS = -w -deprecated-missing-stdlib \
           -w -extraction-opaque-accessed \
           -w -extraction-axiom-to-realize \
           -w -extraction-default-directory

CORE  = MTL_to_TBA_Shared_Clock_Derived_Strict_Direct_Core
PROOF = EncodingCorrect_Shared_Clock_Derived_Strict_Direct_Proof
VFILES = $(CORE).v $(PROOF).v MTL_to_TBA_Invariants.v \
         MTL_to_TBA_Optimizations.v MTL_to_TBA_Export.v Extract_Optim.v
TESTS = optim_test.ml optim_test_ex1.ml
ROOTS = optimized optimize export optimize_export
ZIP   = mtl2tba.zip

.PHONY: all test zip clean

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

test: optim.ml $(TESTS)
	$(OCAMLC) -w -a -o optim_test.exe optim.ml optim_test.ml
	$(OCAMLC) -w -a -o optim_test_ex1.exe optim.ml optim_test_ex1.ml
	./optim_test.exe
	./optim_test_ex1.exe

zip: optim.ml
	rm -f $(ZIP)
	$(PYTHON) -c "import sys, zipfile; z = zipfile.ZipFile(sys.argv[1], 'w', zipfile.ZIP_DEFLATED); [z.write(f) for f in sys.argv[2:]]; z.close()" \
	  $(ZIP) $(VFILES) optim.ml $(TESTS) tools/prune_extraction.py Makefile README_proof.md

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux optim.mli *.cmi *.cmo *.exe $(ZIP)
