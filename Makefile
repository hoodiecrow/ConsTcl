
.PHONY: all
all: README.md constcl.tcl constcl.test

source_files = type.tcl read.tcl eval.tcl write.tcl environment.class equipred.tcl numbers.tcl booleans.tcl characters.tcl control.tcl io.tcl pairslists.tcl symbols.tcl strings.tcl vectors.tcl idcheck.tcl cons.tcl global_env.tcl
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


