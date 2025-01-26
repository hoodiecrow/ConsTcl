
MD(
### Control

This section concerns itself with procedures and the application of the same.
MD)

CB
catch { ::constcl::Procedure destroy }

oo::class create ::constcl::Procedure {
    superclass ::constcl::NIL
    variable parms body env
    constructor {p b e} {
        set parms $p         ;# a Lisp list|improper list|symbol denoting parameter names
        set body $b          ;# a Lisp list of expressions under 'begin
        set env $e           ;# an environment
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
`apply` applies a procedure to a Tcl list of Lisp arguments.
MD)

CB
reg apply ::constcl::apply

proc ::constcl::apply {proc args} {
    if {[procedure? $proc] eq "#t"} {
        invoke $proc $args 
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

CB
reg map ::constcl::map

proc ::constcl::map {proc args} {
    if {[procedure? $proc] eq "#t"} {
        if {[list? [lindex $args end]] eq "#t"} {
            $proc call ;# TODO
        } else {
            error "LIST expected\n(apply [$proc show] ...)"
        }
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}
CB

CB
reg for-each ::constcl::for-each

proc ::constcl::for-each {proc args} {
    if {[::constcl::procedure? $proc] eq "#t"} {
        if {[::constcl::list? [lindex $args end]] eq "#t"} {
            $proc call ;# TODO
        } else {
            error "LIST expected\n(apply [$proc show] ...)"
        }
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}
CB

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

