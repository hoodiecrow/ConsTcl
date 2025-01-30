# ConsTcl
A second try at a Lisp interpreter written in Tcl (the first one was [Thtcl](https://github.com/hoodiecrow/thtcl)),
this time with a real Lisp-like type system. It steps over and back over the border
between Tcl and Lisp a lot of times while working, and as a result is fairly slow.

MD(
#### Benchmark

On my cheap computer, the following code takes 0.024 seconds to run.

```
namespace eval ::constcl {
    eval [parse "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"]
    time {eval [parse "(fact 100)"]} 10
}
```
MD)


Speed aside, it is an amusing piece of machinery. The types are implemented as TclOO
classes, and evaluation is to a large extent applying Lisp methods to Tcl data.

It is limited: as of 2025-01-30, it still doesn't handle input (but has an interactive
REPL). Quite a few standard procedures are missing. It doesn't come near to having
call/cc or tail recursion. It doesn't have ports or exact/inexact numbers, or most of
the numerical tower. Error reporting is spotty, and there is no error recovery.

