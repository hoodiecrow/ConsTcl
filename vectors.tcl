
MD(
### Vectors

Vectors are heterogenous structures whose elements are indexed by integers. They are implemented
as Tcl lists of Lisp values.

The number of elements that a vector contains (the _length_) is set when the vector is created.
Elements can be indexed by integers from zero to length minus one.
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
            if {$i < 0 || $i >= [my length]} {
                error "index out of range\n$i"
            } else {
                set value [::lreplace [my value] $i $i $obj]
            }
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

MD(
`make-vector` creates a vector with a given length and optionally a fill value.
MD)

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

MD(
Given a number of Lisp values, `vector` creates a vector containing them.
MD)

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

MD(
`vector-length` returns the length of a vector.
MD)

CB
reg vector-length ::constcl::vector-length

proc ::constcl::vector-length {vec} {
    if {[vector? $vec] eq "#t"} {
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

MD(
`vector-ref` _vector_ _k_ returns the element of _vector_ at index _k_.
MD)

CB
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[vector? $vec] eq "#t"} {
        if {[number? $k] eq "#t"} {
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

MD(
`vector-set!` _vector_ _k_ _obj_, for a non-constant vector, sets the element at
index _k_ to _obj_.
MD)

CB
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k obj} {
    if {[vector? $vec] eq "#t"} {
        if {[number? $k] eq "#t"} {
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

MD(
`vector->list` converts a vector value to a Lisp list.
MD)

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

MD(
`list->vector` converts a Lisp list value to a vector.
MD)

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

MD(
`vector-fill!` fills a non-constant vector with a given value.
MD)

CB
reg vector-fill! ::constcl::vector-fill!

proc ::constcl::vector-fill! {vec fill} {
    if {[vector? $vec] eq "#t"} {
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

