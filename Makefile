
PROGRAM   = proto
O         = o
HDRS      = 
OBJS      = proto.$(O)

CC        = clang
FEATURES  =
CFLAGS    = -g -W -Wall -Wextra -pedantic
LDFLAGS   =
LIBS      = 
RM        = rm -f
LD        = $(CC)
LDOUT     = -o 
EXE       = 
T90       = ../anstox/tclsh90.exe
P3        = python3
ANSTOTEX  = ../anstox/anstotex.py
ANSTOMD   = ../anstox/anstomd.py
ANSTOCODE = ../anstox/anstocode.py
ANSTOTEST = ../anstox/anstotest.py
ANSTOSCM  = ../anstox/anstoscm.awk

.PHONY: all
all: book.tex README.md constcl.tcl constcl.test schemebase.scm #wiki/type.md wiki/read.md $(PROGRAM)

source_files = initial.ans input.ans eval.ans macros.ans rld.ans output.ans idcheck.ans environment.ans repl.ans equipred.ans numbers.ans booleans.ans characters.ans control.ans io.ans pairslists.ans strings.ans symbols.ans vectors.ans setup.ans

constcl.pdf: README.md
	pandoc -f gfm -t html5 --pdf-engine-opt=--enable-local-file-access --metadata pagetitle="ConsTcl" --css github.css README.md -o constcl.pdf

book.tex: top.tex body.tex bottom.tex
	cat $^ >$@

body.tex: $(source_files) schemebase.ans
	$(P3) $(ANSTOTEX) $^ >$@

#body.tex: $(source_files) schemebase.ans
#	gawk -f $(ANSTOTEX) dict.txt $^ >$@

#constcl.tex: book.md
#	gawk -f latex.awk $< >$@

#book.md: booktop.md constcl.md lutables.md schemebase.md
#	gawk -f prototype.awk dict.txt $^ >$@

README.md: top.md constcl.md
	cat $^ >$@

#README.md: top.md constcl.md schemebase.md
#	gawk -f prototype.awk dict.txt $^ >$@

constcl.md: $(source_files) schemebase.ans
	$(P3) $(ANSTOMD) $^ >$@

#constcl.md: $(source_files) schemebase.ans
#	gawk -f $(ANSTOMD) dict.txt $^ >$@

#constcl.md: $(tcl_source_files)
#	cat $^ |sed -e s/^CB/\`\`\`/g -e /^MD/d -e /^TT/,/^TT/d >$@

constcl.tcl: $(source_files)
	$(P3) $(ANSTOCODE) $^ >$@

#constcl.tcl: $(source_files)
#	gawk -f $(ANSTOCODE) $^ >$@

#constcl.tcl: $(tcl_source_files)
#	cat $^ |sed -e /CB/d -e /^MD/,/^MD/d -e /^PR/,/^PR/d -e /^TT/,/^TT/d -e s/\\r//g >$@

constcl.test: $(source_files)
	$(P3) $(ANSTOTEST) $^ >$@

#constcl.test: $(source_files)
#	gawk -f $(ANSTOTEST) $^ >$@

#constcl.test: $(tcl_source_files)
#	echo 'package require tcltest' >$@
#	echo 'source constcl.tcl\n' >>$@
#	cat $^ |sed -n '/TT/,// { //n ; p }' >>$@
#	echo '\n::tcltest::cleanupTests' >>$@

lutables.md: lutables.tcl
	cat $^ |sed -e s/^CB/\`\`\`/g -e /^MD/d -e /^TT/,/^TT/d >$@

schemebase.scm: schemebase.ans
	gawk -f $(ANSTOSCM) $< >$@

#schemebase.md: schemebase.scm
#	gawk -f scmtomd.awk $< >$@

.PHONY: clean
clean:
	rm -f thtcl-level-l.md thtcl-level-2.md
	$(RM) $(OBJS)
	$(RM) core $(PROGRAM) $(PROGRAM).exe
	$(RM) *~

.SUFFIXES: .obj .exe

.c.obj:
	$(CC) $(CFLAGS) -c $*.c

.c.o:
	$(CC) $(CFLAGS) $(FEATURES) -c $*.c

.scm.md:
	gawk -f scmtomd.awk $*.scm >$*.md

$(PROGRAM): $(HDRS) $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(LIBS) $(LDOUT)$@$(EXE)


