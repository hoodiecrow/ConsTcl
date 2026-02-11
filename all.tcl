package require Tcl
package require tcltest
namespace import -force ::tcltest::*

set dir [file dirname [file normalize [info script]]]

configure -testdir [file join $dir]
configure -tmpdir [file join $dir tmp]
matchFiles ../constcl.test
set OUTFILE [file join $dir tmp outfile.txt]
set ERRFILE [file join $dir tmp errfile.txt]
exec echo -n "" > $OUTFILE
exec echo -n "" > $ERRFILE
configure -outfile $OUTFILE
configure -errfile $ERRFILE

configure -verbose {body start error line}

configure {*}$argv
runAllTests
