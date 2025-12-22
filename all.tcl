package require Tcl
package require tcltest
namespace import -force ::tcltest::*

set dir [file dirname [file normalize [info script]]]

matchFiles ../constcl.test
configure -testdir [file join $dir tests]
configure -tmpdir [file join $dir tmp]
set OUTFILE [file join $dir tmp outfile.txt]
set ERRFILE [file join $dir tmp errfile.txt]
exec echo -n "" > $OUTFILE
exec echo -n "" > $ERRFILE
configure -outfile $OUTFILE
configure -errfile $ERRFILE

configure -verbose {body start error line}

configure {*}$argv
runAllTests
