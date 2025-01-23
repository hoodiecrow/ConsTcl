
MD(
## Vectors
MD)

CB
oo::class create Vector {
    superclass NIL
    variable value constant
    constructor {v} {
        set value $v
        set constant 0
    }
    method length {} {llength $value}
    method ref {i} {lindex $value $i}
    method value {} {lmap val $value {$val value}}
    method mkconstant {} {set constant 1}
    method constant {} {set constant}
    method write {} {puts -nonewline [my show]}
    method show {} {return #([lmap val $value {$val show}])}
}

proc ::constcl::MkVector {v} {
    foreach instance [info class instances Vector] {
        if {$instance eq $v} {
            return $instance
        }
    }
    return [Vector create Mem[incr ::M] $v]
}
CB

CB
reg vector? ::constcl::vector?

proc ::constcl::vector? {obj} {
    if {[info object isa typeof $obj Vector]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Vector]} {
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
        return [MkNumber [$str length]]]
    } else {
        error "VECTOR expected\n(vector-length [$vec show])"
    }
}
CB

CB
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str ref $k]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec show] [$k show])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec show] [$k show])"
    }
}
CB


CB
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k obj} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str set! $k $obj]]
        } else {
            error "NUMBER expected\n(vector-set! [$vec show] [$k show] [$obj show])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec show] [$k show] [$obj show])"
    }
}
CB

CB
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
    # TODO
}
CB

CB
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
    # TODO
}
CB

CB
reg vector-fill ::constcl::vector-fill

proc ::constcl::vector-fill {vec fill} {
    if {[::constcl::vector? $vec] eq "#t"} {
        $vec fill $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec show] [$fill show])"
    }
}
CB

