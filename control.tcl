
MD(
### Control

This section concerns itself with procedures and the application of the same.

A `Procedure` object is basically a
[closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)),
storing the procedure's parameter list, the body, and the environment that is current
when the object is created (when the procedure is defined).

When a `Procedure` object is called, the body is evaluated in a new environment
where the parameters are given values from the argument list and the outer link
goes to the closure environment.

MD)

CB
catch { ::constcl::Procedure destroy }

oo::class create ::constcl::Procedure {
    superclass ::constcl::NIL
    variable parms body env
    constructor {p b e} {
        set parms $p         ;# a Lisp list|improper list|symbol denoting parameter names
        set body $b          ;# a Lisp list of expressions under 'begin, or a single expression
        set env $e           ;# the closed over environment
    }
    method value {} {}
    method write {} { puts -nonewline [self] }
    method show {} { return [self] }
    method call {args} {
        ::constcl::eval $body [::constcl::Environment new $parms $args $env]
    }

}

interp alias {} ::constcl::MkProcedure {} ::constcl::Procedure new

reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {obj} {
    if {[info object isa typeof $obj ::constcl::Procedure]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Procedure]} {
        return #t
    } elseif {[::string match "::constcl::*" $obj]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test control-1.0 {try procedure?)} -body {
    pep {(procedure? car)}
    pep {(procedure? 'car)}
    pep {(procedure? (lambda (x) (* x x)))}
} -output "#t\n#f\n#t\n"

TT)

MD(
`apply` applies a procedure to a Lisp list of Lisp arguments.
MD)

CB
reg apply ::constcl::apply

proc ::constcl::apply {proc vals} {
    if {[procedure? $proc] eq "#t"} {
        invoke $proc $vals
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}
CB

TT(

::tcltest::test control-1.1 {try apply)} -body {
    pep {(apply + (list 3 4))}
    pep {(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))}
    pep {((compose sqrt *) 12 75)}
} -output "7\n30.0\n"

TT)

MD(
`map` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The Lisp list of the results of the invocations is 
returned.
MD)

CB
reg map ::constcl::map

proc ::constcl::map {proc args} {
    if {[procedure? $proc] eq "#t"} {
        set arglists $args
        for {set i 0} {$i < [llength $arglists]} {incr i} {
            lset arglists $i [splitlist [lindex $arglists $i]]
        }
        set res {}
        for {set item 0} {$item < [llength [lindex $arglists 0]]} {incr item} {
            set arguments {}
            for {set arg 0} {$arg < [llength $arglists]} {incr arg} {
                lappend arguments [lindex $arglists $arg $item]
            }
            lappend res [invoke $proc [list {*}$arguments]]
        }
        return [list {*}$res]
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}
CB

TT(

::tcltest::test control-1.2 {try map)} -body {
    pep {(map cadr '((a b) (d e) (g h)))}
    pep {(map (lambda (n) (expt n n)) '(1 2 3 4 5))}
    pep {(map + '(1 2 3) '(4 5 6))}
    pep {(let ((count 0))
  (map (lambda (ignored)
         (set! count (+ count 1))
         count)
       '(a b)))}
} -output "(b e h)\n(1.0 4.0 27.0 256.0 3125.0)\n(5 7 9)\n(1 2)\n"

TT)

MD(
`for-each` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The empty list is returned.
MD)

CB
reg for-each ::constcl::for-each

proc ::constcl::for-each {proc args} {
    if {[procedure? $proc] eq "#t"} {
        set arglists $args
        for {set i 0} {$i < [llength $arglists]} {incr i} {
            lset arglists $i [splitlist [lindex $arglists $i]]
        }
        for {set item 0} {$item < [llength [lindex $arglists 0]]} {incr item} {
            set arguments {}
            for {set arg 0} {$arg < [llength $arglists]} {incr arg} {
                lappend arguments [lindex $arglists $arg $item]
            }
            invoke $proc [list {*}$arguments]
        }
        return [list]
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}
CB

TT(

::tcltest::test control-1.3 {try for-each)} -body {
    pep {(for-each display '(1 2 3))}
} -output "123()\n"

TT)

CB
proc ::constcl::force {promise} {
    # TODO
}
CB

CB
proc ::constcl::call-with-current-continuation {proc} {
    # TODO
}
CB

CB
proc ::constcl::values {args} {
    # TODO
}
CB

CB
proc ::constcl::call-with-values {producer consumer} {
    # TODO
}
CB

CB
proc ::constcl::dynamic-wind {before thunk after} {
    # TODO
}
CB

