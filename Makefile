
PROGRAM  = proto
O        = o
HDRS     = 
OBJS     = proto.$(O)

CC       = clang
FEATURES =
CFLAGS   = -g -W -Wall -Wextra -pedantic
LDFLAGS  =
LIBS     = 
RM       = rm -f
LD       = $(CC)
LDOUT    = -o 
EXE      = 

.PHONY: all
all: book.md book.tex README.md constcl.tcl constcl.test wiki/type.md wiki/read.md $(PROGRAM)

source_files = intro.src initial.src input.src eval.src macros.src rld.src output.src equipred.src numbers.src

tcl_source_files = type.tcl read.tcl eval.tcl macros.tcl rld.tcl write.tcl equipred.tcl numbers.tcl booleans.tcl characters.tcl control.tcl io.tcl pairslists.tcl strings.tcl symbols.tcl vectors.tcl idcheck.tcl s9fes.tcl cons.tcl repl.tcl environment.class global_env.tcl

constcl.pdf: README.md
	pandoc -f gfm -t html5 --pdf-engine-opt=--enable-local-file-access --metadata pagetitle="ConsTcl" --css github.css README.md -o constcl.pdf

book.tex: top.tex body.tex bottom.tex
	cat $^ >$@

body.tex: $(source_files)
	awk -f srctotex.awk dict.txt $^ >$@

constcl.tex: book.md
	awk -f latex.awk $< >$@

book.md: booktop.md constcl.md lutables.md schemebase.md
	awk -f prototype.awk dict.txt $^ >$@

README.md: top.md constcl.md schemebase.md
	awk -f prototype.awk dict.txt $^ >$@

constcl.md: $(tcl_source_files)
	cat $^ |sed -e s/^CB/\`\`\`/g -e /^MD/d -e /^TT/,/^TT/d >$@

constcl.tcl: $(tcl_source_files)
	cat $^ |sed -e /CB/d -e /^MD/,/^MD/d -e /^PR/,/^PR/d -e /^TT/,/^TT/d -e s/\\r//g >$@

constcl.test: $(tcl_source_files)
	echo 'package require tcltest' >$@
	echo 'source constcl.tcl\n' >>$@
	cat $^ |sed -n '/TT/,// { //n ; p }' >>$@
	echo '\n::tcltest::cleanupTests' >>$@

lutables.md: lutables.tcl
	cat $^ |sed -e s/^CB/\`\`\`/g -e /^MD/d -e /^TT/,/^TT/d >$@

schemebase.md: schemebase.scm
	awk -f minimal.awk $< >$@

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
	awk -f minimal.awk $*.scm >$*.md

$(PROGRAM): $(HDRS) $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(LIBS) $(LDOUT)$@$(EXE)


