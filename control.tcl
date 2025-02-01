
MD(
### Control

This section concerns itself with procedures and the application of the same.

A `Procedure` object is a
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
CB

PR(
procedure? (public);val val -> bool
PR)

CB
reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {val} {
    if {[info object isa typeof $val ::constcl::Procedure]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Procedure]} {
        return #t
    } elseif {[::string match "::constcl::*" $val]} {
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

PR(
apply (public);pr proc vals lvals -> invoke
PR)

CB
reg apply ::constcl::apply

proc ::constcl::apply {pr vals} {
    if {[procedure? $pr] ne "#f"} {
        invoke $pr $vals
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

PR(
map (public);pr proc args lists -> lvals
PR)

CB
reg map ::constcl::map

proc ::constcl::map {pr args} {
    if {[procedure? $pr] ne "#f"} {
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
            lappend res [invoke $pr [list {*}$arguments]]
        }
        return [list {*}$res]
    } else {
        error "PROCEDURE expected\n(apply [$pr show] ...)"
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

PR(
for-each (public);pr proc args lists -> nil
PR)

CB
reg for-each ::constcl::for-each

proc ::constcl::for-each {proc args} {
    if {[procedure? $proc] ne "#f"} {
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

