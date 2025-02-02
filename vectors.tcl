
MD(
### Vectors

Vectors are heterogenous structures of fixed length whose elements are indexed by integers. 
They are implemented as Tcl lists of Lisp values.

The number of elements that a vector contains (the _length_) is set when the vector is created.
Elements can be indexed by integers from zero to length minus one.
MD)

CB
oo::class create ::constcl::Vector {
    superclass ::constcl::NIL
    variable vsaddr length constant
    constructor {v} {
        set vsaddr $::constcl::vectorAssign
        set length [llength $v]
        incr ::constcl::vectorAssign $length
        set idx $vsaddr
        foreach elt $v {
            lset ::constcl::vectorSpace $idx $elt
            incr idx
        }
        set constant 0
    }
    method length {} {set length}
    method ref {i} {lindex $::constcl::vectorSpace [expr {$i + $vsaddr}]}
    method value {} {lrange $::constcl::vectorSpace $vsaddr [expr {$length + $vsaddr - 1}]}
    method set! {i obj} {
        if {[my constant]} {
            error "vector is constant"
        } else {
            if {$i < 0 || $i >= [my length]} {
                error "index out of range\n$i"
            } else {
                lset ::constcl::vectorSpace [expr {$i + $vsaddr}] $obj
            }
        }
        return [self]
    }
    method fill! {c} {
        if {[my constant]} {
            error "vector is constant"
        } else {
            for {set idx $vsaddr} {$idx < [expr {$length + $vsaddr}]} {incr idx} {
                lset ::constcl::vectorSpace $idx $c
            }
        }
        return [self]
    }
    method mkconstant {} {set constant 1}
    method constant {} {set constant}
    method write {} {puts -nonewline [my show]}
    method show {} {format "#(%s)" [join [lmap val [my value] {$val show}] " "]}
}

interp alias {} ::constcl::MkVector {} ::constcl::Vector new
CB

MD(
**vector?**
MD)

PR(
vector? (public);val val -> bool
PR)

CB
reg vector? ::constcl::vector?

proc ::constcl::vector? {val} {
    if {[info object isa typeof $val ::constcl::Vector]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Vector]} {
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
**make-vector**

`make-vector` creates a vector with a given length and optionally a fill value.
If a fill value isn't given, the empty list will be used.
MD)

PR(
make-vector? (public);k num ?fill? val -> vec
PR)

CB
reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {k args} {
    if {[llength $args] == 0} {
        set fill #NIL
    } else {
        lassign $args fill
    }
    MkVector [lrepeat [$k numval] $fill]
}
CB

MD(
**vector**

Given a number of Lisp values, `vector` creates a vector containing them.
MD)

PR(
vector (public);args vals -> vec
PR)

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
**vector-length**

`vector-length` returns the length of a vector.
MD)

PR(
vector-length (public);vec vec -> num
PR)

CB
reg vector-length ::constcl::vector-length

proc ::constcl::vector-length {vec} {
    if {[vector? $vec] ne "#f"} {
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
**vector-ref**

`vector-ref` returns the element of _vec_ at index _k_.
MD)

PR(
vector-ref (public);vec vec k num -> val
PR)

CB
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[vector? $vec] ne "#f"} {
        if {[number? $k] ne "#f"} {
            return [$vec ref [$k numval]]
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
`vector-set!`, for a non-constant vector, sets the element at index _k_ to _val_.
MD)

PR(
vector-set! (public);vec vec k num val val -> vec
PR)

CB
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k val} {
    if {[vector? $vec] ne "#f"} {
        if {[number? $k] ne "#f"} {
            return [$vec set! [$k numval] $val]
        } else {
            error "NUMBER expected\n(vector-set! [$vec show] [$k show] [$val show])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec show] [$k show] [$val show])"
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
**vector->list**

`vector->list` converts a vector value to a Lisp list.
MD)

PR(
vector->list (public);vec vec -> lvals
PR)

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
**list->vector**

`list->vector` converts a Lisp list value to a vector.
MD)

PR(
list->vector (public);list lvals -> vec
PR)

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
**vector-fill!**

`vector-fill!` fills a non-constant vector with a given value.
MD)

PR(
vector-fill! (public);vec vec fill val -> vec
PR)

CB
reg vector-fill! ::constcl::vector-fill!

proc ::constcl::vector-fill! {vec fill} {
    if {[vector? $vec] ne "#f"} {
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

# vim: ft=tcl tw=80
