

MD(
## The REPL

The REPL ([read-eval-print loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop))
is a loop that repeatedly _reads_ a Scheme source string from the user through the command
`::constcl::input` (breaking the loop if given an empty line) and `::constcl::parse`, _evaluates_ it using
`::constcl::eval`, and _prints_ using `::constcl::write`.
MD)

MD(
**input**

`input` is modelled after the Python 3 function. It displays a prompt and reads a string.
MD)

CB
proc ::constcl::input {prompt} {
    puts -nonewline $prompt
    flush stdout
    gets stdin
}
CB

MD(
**repl**

`repl` puts the loop in the read-eval-print loop. It repeats prompting for a string until given
a blank input. Given non-blank input, it parses and evaluates the string, printing the resulting value.
MD)

CB
proc ::constcl::repl {{prompt "ConsTcl> "}} {
    set str [input $prompt]
    while {$str ne ""} {
        write [eval [parse $str]]
        set str [input $prompt]
    }
}
CB

# vim: ft=tcl tw=80
