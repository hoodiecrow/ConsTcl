
MD(
### Vectors
MD)

CB
oo::class create ::constcl::Vector {
    superclass ::constcl::NIL
    variable value constant
    constructor {v} {
        set value $v
        set constant 0
    }
    method length {} {llength $value}
    method ref {i} {lindex $value $i}
    method value {} {set value}
    method set! {i obj} {
        if {[my constant]} {
            error "vector is constant"
        } else {
            set value [::lreplace [my value] $i $i $obj]
        }
        return [self]
    }
    method fill! {c} {
        if {[my constant]} {
            error "vector is constant"
        } else {
            set value [::lrepeat [::llength [my value]] $c]
        }
        return [self]
    }
    method mkconstant {} {set constant 1}
    method constant {} {set constant}
    method write {} {puts -nonewline [my show]}
    method show {} {format "#(%s)" [join [lmap val [my value] {$val show}] " "]}
}

proc ::constcl::MkVector {v} {
    foreach instance [info class instances ::constcl::Vector] {
        if {$instance eq $v} {
            return $instance
        }
    }
    return [::constcl::Vector new $v]
}
CB

CB
reg vector? ::constcl::vector?

proc ::constcl::vector? {obj} {
    if {[info object isa typeof $obj ::constcl::Vector]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Vector]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test vectors-1.0 {try vector? (and make-vector, vector)} -body {
    pep {(vector? '#(0 (2 2 2 2) "Anna"))}
    pep {(vector? (make-vector 3 #\X))}
    pep {(vector? (vector 'a 'b 'c))}
} -output "#t\n#t\n#t\n"

TT)

CB
reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {args} {
    if {[llength $args] == 1} {
        lassign $args k
        set fill #NIL
    } else {
        lassign $args k fill
    }
    MkVector [lrepeat [$k value] $fill]
}
CB

CB
reg vector ::constcl::vector

proc ::constcl::vector {args} {
    MkVector $args
}
CB

TT(

::tcltest::test vectors-1.1 {try vector} -body {
    pep {(vector 'a 'b 'c)}
    pep {(vector 0 '(2 2 2 2) "Anna")}
} -output "#(a b c)\n#(0 (2 2 2 2) \"Anna\")\n"

TT)

CB
reg vector-length ::constcl::vector-length

proc ::constcl::vector-length {vec} {
    if {[::constcl::vector? $vec] eq "#t"} {
        return [MkNumber [$vec length]]
    } else {
        error "VECTOR expected\n(vector-length [$vec show])"
    }
}
CB

TT(

::tcltest::test vectors-1.2 {try vector-length} -body {
    pep {(vector-length (vector 'a 'b 'c))}
} -output "3\n"

TT)

CB
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$vec ref [$k value]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec show] [$k show])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec show] [$k show])"
    }
}
CB

TT(

::tcltest::test vectors-1.3 {try vector-ref} -body {
    pep {(vector-ref (vector 'a 'b 'c) 1)}
} -output "b\n"

TT)


CB
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k obj} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$vec set! [$k value] $obj]
        } else {
            error "NUMBER expected\n(vector-set! [$vec show] [$k show] [$obj show])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec show] [$k show] [$obj show])"
    }
}
CB

TT(

::tcltest::test vectors-1.4 {try vector-set!} -body {
    pep {(define x (lambda () (vector 0 '(2 2 2 2) "Anna")))}
    pep {(vector-set! (x) 1 '(foo bar))}
} -output "#(0 (foo bar) \"Anna\")\n"

TT)

CB
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
    list {*}[$vec value]
}
CB

TT(

::tcltest::test vectors-1.5 {try vector->list} -body {
    pep {(vector->list (vector 'a 'b 'c))}
} -output "(a b c)\n"

TT)

CB
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
    vector {*}[splitlist $list]
}
CB

TT(

::tcltest::test vectors-1.6 {try list->vector} -body {
    pep {(list->vector '(a b c))}
} -output "#(a b c)\n"

TT)

CB
reg vector-fill! ::constcl::vector-fill!

proc ::constcl::vector-fill! {vec fill} {
    if {[::constcl::vector? $vec] eq "#t"} {
        $vec fill! $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec show] [$fill show])"
    }
}
CB

TT(

::tcltest::test vectors-1.7 {try vector-fill!} -body {
    pep {(vector-fill! (vector 'a 'b 'c) 'x)}
} -output "#(x x x)\n"

TT)

