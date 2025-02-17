

MD(
## The REPL

The REPL (read-eval-print loop) is a
loop that repeatedly **reads** a Scheme source string from the user through the
command `::constcl::input` (breaking the loop if given an empty line) and
`::constcl::parse`, **evaluates** it using `::constcl::eval`, and **prints** using
`::constcl::write`.
MD)

MD(
__input__

`input` is modelled after the Python 3 function. It displays a prompt and reads a string.
MD)

CB
proc ::constcl::input {prompt} {
  puts -nonewline $prompt
  flush stdout
  set buf [gets stdin]
  set openpars [regexp -all -inline {\(} $buf]
  set clsepars [regexp -all -inline {\)} $buf]
  set openbrak [regexp -all -inline {\[} $buf]
  set clsebrak [regexp -all -inline {\]} $buf]
  while {[llength $openpars] > [llength $clsepars] ||
         [llength $openbrak] > [llength $clsebrak]} {
    ::append buf [gets stdin]
    set openpars [regexp -all -inline {\(} $buf]
    set clsepars [regexp -all -inline {\)} $buf]
    set openbrak [regexp -all -inline {\[} $buf]
    set clsebrak [regexp -all -inline {\]} $buf]
  }
  return $buf
}
CB

MD(
__repl__

`repl` puts the 'loop' in the read-eval-print loop. It repeats prompting for a
string until given a blank input. Given non-blank input, it parses and evaluates
the string, printing the resulting value.
MD)

CB
proc ::repl {{prompt "ConsTcl> "}} {
  set str [::constcl::input $prompt]
  while {$str ne ""} {
    pep $str
    set str [::constcl::input $prompt]
  }
}
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
