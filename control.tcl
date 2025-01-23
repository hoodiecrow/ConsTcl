
MD(
## Control
MD)

CB
catch { Procedure destroy }

oo::class create Procedure {
    superclass NIL
    variable parms body env
    constructor {p b e} {
        set parms $p         ;# a Tcl list of parameter names
        set body $b          ;# a Lisp llist of expressions under 'begin
        set env $e           ;# an environment
    }
    method value {} {
        set value
    }
    method write {} { puts -nonewline Procedure[self] }
    method call {args} {
        if {[llength $parms] != [llength $args]} {
            error "Wrong number of arguments passed to procedure"
        }
        ::constcl::eval $body [Environment new $parms $args $env]
    }

}
CB

CB
proc MkProcedure {parms body env} {
    Procedure create Mem[incr ::M] $parms $body $env
}
CB

CB
reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {obj} {
    if {[info object isa typeof $obj Procedure]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Procedure]} {
        return #t
    } elseif {[::string match "::constcl::*" $obj] && ![::string match "::constcl::Mem*" $obj]} {
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

CB
reg apply ::constcl::apply

proc ::constcl::apply {proc args} {
    if {[procedure? $proc] eq "#t"} {
        if {[list? [lindex $args end]] eq "#t"} {
           invoke $proc $args 
        } else {
            error "LIST expected\n(apply [$proc write] ...)"
        }
    } else {
        error "PROCEDURE expected\n(apply [$proc write] ...)"
    }
}
CB

TT(

::tcltest::test control-1.1 {try apply)} -body {
    pep {(apply + (list 3 4))}
    pep {(define compose
  (lambda (f g)
    (lambda (args)
      (f (apply g args)))))}
    pep {((compose sqrt *) (list 12 75))}
} -output "7\n()\n30.0\n"

TT)

CB
reg map ::constcl::map

proc ::constcl::map {proc args} {
    if {[::constcl::procedure? $proc] eq "#t"} {
        if {[::constcl::list? [lindex $args end]] eq "#t"} {
            $proc call # TODO
        } else {
            error "LIST expected\n(apply [$proc write] ...)"
        }
    } else {
        error "PROCEDURE expected\n(apply [$proc write] ...)"
    }
}
CB

CB
reg for-each ::constcl::for-each

proc ::constcl::for-each {proc args} {
    if {[::constcl::procedure? $proc] eq "#t"} {
        if {[::constcl::list? [lindex $args end]] eq "#t"} {
            $proc call # TODO
        } else {
            error "LIST expected\n(apply [$proc write] ...)"
        }
    } else {
        error "PROCEDURE expected\n(apply [$proc write] ...)"
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

