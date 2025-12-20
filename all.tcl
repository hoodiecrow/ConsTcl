package require Tcl
package require tcltest

set dir [file dirname [file normalize [info script]]]

::tcltest::matchFiles ../constcl.test
::tcltest::configure -testdir [file join $dir tests]
::tcltest::configure -tmpdir [file join $dir tmp]
set OUTFILE [file join $dir tmp outfile.txt]
set ERRFILE [file join $dir tmp errfile.txt]
exec echo -n "" > $OUTFILE
exec echo -n "" > $ERRFILE
::tcltest::configure -outfile $OUTFILE
::tcltest::configure -errfile $ERRFILE

::tcltest::configure -verbose {body start error line}

::tcltest::configure {*}$argv
::tcltest::runAllTests
