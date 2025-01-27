
MD(
#### Benchmark

On my cheap computer, the following code takes 0.025 seconds to run.

```
namespace eval ::constcl {
    eval [parse "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"]
    time {eval [parse "(fact 100)"]} 10
}
```

Let's start off with a procedure to resolve calls to standard procedures `caar` - `cddddr`
(I'm not going to write them all!).
MD)

CB
namespace eval ::constcl {
    namespace unknown resolve

    proc resolve {cmd args} {
        if {no} {
        } else {
            return -code error "no such command: '$cmd'"
        }
    }
}
CB

MD(
Next, some procedures that make my life as developer somewhat easier, but
don't really matter to the interpreter (except the first one, `reg`, which
registers built-in procedures in the definitions register).
MD)

CB
# utility functions
proc reg {sym impl} {
    dict set ::defreg $sym $impl
}

proc ::pep {str} {
    ::constcl::write [::constcl::eval [::constcl::parse $str]]
}

proc ::pp {str} {
    ::constcl::write [::constcl::parse $str]
}

proc ::pxp {str} {
    set p [::constcl::parse $str]
    set op [::constcl::car $p]
    set args [::constcl::cdr $p]
    ::constcl::expand-macro op args ::global_env
    set p [::constcl::cons $op $args]
    ::constcl::write $p
}

proc mksymlist {tcllist} {
    return [::constcl::list {*}[lmap s $tcllist {::constcl::MkSymbol $s}]]
}
CB

MD(
This one is a little bit of both, a utility function that is also among the
builtins in the library. It started out as a one-liner by Donal K. Fellows,
but has grown a bit since then to suit my needs.
MD)

CB
reg in-range ::constcl::in-range

#started out as DKF's code
proc ::constcl::tcl-in-range {args} {
    set start 0
    set step 1
    switch [llength $args] {
        1 { lassign $args e ; set end [$e value]}
        2 { lassign $args s e ; set start [$s value] ; set end [$e value]}
        3 { lassign $args s e t ; set start [$s value] ; set end [$e value] ; set step [$t value]}
    }
    set res $start
    while {$step > 0 && $end > [incr start $step] || $step < 0 && $end < [incr start $step]} {
        lappend res $start
    }
    return [lmap r $res {MkNumber $r}]
}

proc ::constcl::in-range {args} {
    set start 0
    set step 1
    switch [llength $args] {
        1 { lassign $args e ; set end [$e value]}
        2 { lassign $args s e ; set start [$s value] ; set end [$e value]}
        3 { lassign $args s e t ; set start [$s value] ; set end [$e value] ; set step [$t value]}
    }
    set res $start
    while {$step > 0 && $end > [incr start $step] || $step < 0 && $end < [incr start $step]} {
        lappend res $start
    }
    return [list {*}[lmap r $res {MkNumber $r}]]
}
CB

MD(
The `NIL` class has one object: the empty list called `#NIL`. It is also base class for many other
type classes.
MD)

CB
catch { ::constcl::NIL destroy }

oo::class create ::constcl::NIL {
    constructor {} {}
    method bvalue {} {return #t}
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method numval {} {error "Not a number"}
    method write {} {puts -nonewline "()"}
    method show {} {format "()"}
}
CB

MD(
The `null?` standard predicate recognizes the empty list. Predicates
in ConsTcl return #t or #f for true or false, so some care is necessary
when calling them from Tcl code.
MD)

CB
reg null? ::constcl::null?

proc ::constcl::null? {obj} {
    if {$obj eq "#NIL"} {
        return #t
    } else {
        return #f
    }
}
CB

MD(
The `None` class serves but one purpose: to avoid printing a result after `define`.
MD)

CB
catch { ::constcl::None destroy}

oo::class create ::constcl::None {}
CB

MD(
The `Dot` class is a helper class for the parser.
MD)

CB
catch { ::constcl::Dot destroy }

oo::class create ::constcl::Dot {
    method mkconstant {} {}
}
CB

