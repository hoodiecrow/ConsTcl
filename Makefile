
.PHONY: all
all: README.md constcl.tcl constcl.test


README.md: top.md constcl.md
	cat $^ |sed -e s/\\r//g >$@

constcl.md: type.tcl cons.tcl read.tcl idcheck.tcl write.tcl
	cat $^ |sed -e s/^CB/\`\`\`/g -e /MD/d -e /TT/,/TT/d >$@

constcl.tcl: type.tcl cons.tcl read.tcl idcheck.tcl write.tcl
	cat $^ |sed -e /CB/d -e /MD/,/MD/d -e /TT/,/TT/d >$@

constcl.test: type.tcl cons.tcl read.tcl idcheck.tcl write.tcl
	echo 'package require tcltest' >$@
	echo 'source constcl.tcl\n' >>$@
	cat $^ |sed -n '/TT/,// { //n ; p }' >>$@
	echo '\n::tcltest::cleanupTests' >>$@

.PHONY: clean
clean:
	rm -f thtcl-level-l.md thtcl-level-2.md


