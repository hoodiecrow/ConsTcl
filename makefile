RM         = rm -f
MV	   = mv
CP	   = cp
Py3        = python3
GITHUB     = /mnt/c/Users/plewe/Documents/GitHub
ANSTO_PATH = ../anstox/
ANSTOTEX   = $(ANSTO_PATH)anstotex.py
ANSTOMD    = $(ANSTO_PATH)anstomd.py
ANSTOCODE  = $(ANSTO_PATH)anstocode.py
ANSTOTEST  = $(ANSTO_PATH)anstotest.py
ANSTOHTML  = $(ANSTO_PATH)anstohtml.py


# to remove CR in files
# tr --delete '\r' <<path> >foo && mv foo <path>


.PHONY: all clean installTests

all: book.tex book.md constcl.tcl constcl.test schemebase.scm installTests

source_files = src/initial.ans src/input.ans src/eval.ans src/macros.ans src/rld.ans src/output.ans src/idcheck.ans src/environment.ans src/repl.ans src/equipred.ans src/numbers.ans src/booleans.ans src/characters.ans src/control.ans src/io.ans src/pairslists.ans src/strings.ans src/symbols.ans src/vectors.ans src/setup.ans

book.tex: src/top.tex body.tex src/bottom.tex
	cat $^ >$@
	$(RM) body.tex

body.tex: $(source_files) src/schemebase.ans
	$(Py3) $(ANSTOTEX) $^ >$@

book.md: src/top.md constcl.md
	cat $^ >$@
	$(RM) constcl.md

constcl.md: $(source_files) src/schemebase.ans
	$(Py3) $(ANSTOMD) $^ >$@

constcl.tcl: $(source_files)
	$(Py3) $(ANSTOCODE) $^ >$@

constcl.test: $(source_files)
	$(Py3) $(ANSTOTEST) $^ >$@

define build_test
@echo "\npackage require tcltest\nsource constcl.tcl\n" > tests/$(notdir $(basename $(1))).test
@tr --delete '\r' <$(1) |sed -n '/^TT/,/^TT/p' |sed '/^TT/d' >> tests/$(notdir $(basename $(1))).test
@echo "\ntcltest::cleanupTests\nreturn\n" >> tests/$(notdir $(basename $(1))).test
@sed 's/\r//g' -i $(shell pwd)/tests/$(notdir $(basename $(1))).test
@echo "#\\---"
endef

installTests:
	$(foreach src,$(source_files),$(call build_test,$(src)))

schemebase.scm: src/schemebase.ans
	$(Py3) $(ANSTOCODE) $< >$@

clean:
	$(RM) *~
	$(RM) body.tex constcl.md


