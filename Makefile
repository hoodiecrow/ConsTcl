
.PHONY: all
all: README.md constcl.tcl constcl.test

source_files = type.tcl read.tcl idcheck.tcl write.tcl equipred.tcl numbers.tcl booleans.tcl pairslists.tcl symbols.tcl characters.tcl strings.tcl vectors.tcl control.tcl eval.tcl io.tcl cons.tcl environment.class global_env.tcl
README.md: top.md constcl.md
	cat $^ |sed -e s/\\r//g >$@
constcl.md: $(source_files)
	cat $^ |sed -e s/^CB/\`\`\`/g -e /MD/d -e /TT/,/TT/d >$@

constcl.tcl: $(source_files)
	cat $^ |sed -e /CB/d -e /MD/,/MD/d -e /TT/,/TT/d >$@

constcl.test: $(source_files)
	echo 'package require tcltest' >$@
	echo 'source constcl.tcl\n' >>$@
	cat $^ |sed -n '/TT/,// { //n ; p }' >>$@
	echo '\n::tcltest::cleanupTests' >>$@

.PHONY: clean
clean:
	rm -f thtcl-level-l.md thtcl-level-2.md


