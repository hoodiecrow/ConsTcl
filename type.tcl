
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

MD(
## Initial declarations
MD)

MD(
First, I need to create the namespace that will be used for most identifiers:
MD)

CB
namespace eval ::constcl {}
CB

MD(
Next, some procedures that make my life as developer somewhat easier, but
don't really matter to the interpreter (except the first one, `reg`, which
registers built-in procedures in the definitions register). The other ones
will show up a lot in the test cases.
MD)

CB
# utility functions
proc ::reg {key args} {
    if {[llength $args] == 0} {
        set val ::constcl::$key
    } else {
        set val [lindex $args 0]
    }
    dict set ::constcl::defreg $key $val
}

proc ::pep {str} {
    ::constcl::write [::constcl::eval [::constcl::parse $str]]
}

proc ::pp {str} {
    ::constcl::write [::constcl::parse $str]
}

proc ::pxp {str} {
    set val [::constcl::parse $str]
    set op [::constcl::car $val]
    set args [::constcl::cdr $val]
    ::constcl::expand-macro op args ::constcl::global_env
    set val [::constcl::cons $op $args]
    ::constcl::write $val
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
    method bvalue {} {return #NIL}
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

proc ::constcl::dot? {obj} {
    if {[info object isa typeof $obj Dot]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Dot]} {
        return #t
    } else {
        return #f
    }
}

CB

