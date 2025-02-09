# ConsTcl

To run, source the file __constcl.tcl__ (with __schemebase.lsp__ in the directory)
in a Tcl console (I use __tkcon__) and use the command `::constcl::repl` for a
primitive command dialog.  Source __all.tcl__ to run the test suite.

ConsTcl is a second try at a Lisp interpreter written in Tcl (the first one was 
[Thtcl](https://github.com/hoodiecrow/thtcl)), this time with a real Lisp-like 
type system. It steps over and back over the border between Tcl and Lisp a lot
of times while working, and as a result is fairly slow.

#### Benchmark

On my cheap computer, the following code takes 0.024 seconds to run.

```
namespace eval ::constcl {
    eval [parse "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))"]
    time {eval [parse "(fact 100)"]} 10
}
```


Speed aside, it is an amusing piece of machinery. The types are implemented as TclOO
classes, and evaluation is to a large extent applying Lisp methods to Tcl data.

It is limited. Quite a few standard procedures are missing. It doesn't come
near to having call/cc or tail recursion. It doesn't have exact/inexact
numbers, or most of the numerical tower. Error reporting is spotty, and there
is no error recovery.

