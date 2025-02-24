# ConsTcl

## Introduction

### To run the software

First things first. To run, source the file __constcl.tcl__ (with
__schemebase.lsp__ in the directory) in a Tcl console (I use __tkcon__) and use
the command __repl__ for a primitive command dialog.  Source
__all.tcl__ to run the test suite (you need __constcl.test__ for that). The
files can be found on [GitHub/ConsTcl](https://github.com/hoodiecrow/ConsTcl).

### Background

It all started with Peter Norvig's Lisp emulator
[Lispy](https://norvig.com/lispy.html). In January 2025 I was inspired to port
it to Tcl. The result was [Thtcl](https://github.com/hoodiecrow/thtcl). It had
the same features and limitations as Lispy, plus a couple that were due to
shortcomings of Tcl, and I came out of the experience feeling a bit
dissatisfied. In the latter part of January I embarked on a new project,
ConsTcl, a true Lisp interpreter. In Tcl.

#### About ConsTcl

Compared to Lispy/Thtcl, ConsTcl has, (quote from Lispy), "comments, quote and
quasiquote notation, # literals, the derived expression types (such as cond,
derived from if, or let, derived from lambda), and dotted list notation."
Again compared to Lispy/Thtcl, ConsTcl has the data types, quote, "strings,
characters, booleans, ports, vectors." And pairs and procedures too. The
number of missing primitive procedures is in the tens, not the 100s. 

The completeness comes with a price: due to the sheer number of calls for each
action, ConsTcl is is fairly slow. On my cheap computer, the following code
(which calculates the factorial of 100) takes 0.03 seconds to run. That is ten
times slower than Lispy assuming that Norvig's computer is as slow as mine,
which is unlikely. So it's probably a factor of less than ten.

```
time {pe "(fact 100)"} 10
```

ConsTcl is of course still limited. It doesn't come close to having call/cc or
tail recursion. It doesn't have exact/inexact numbers, or most of the numerical
tower. There is no memory management. Error reporting is spotty, and there is no
error recovery.

