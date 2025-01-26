
MD(
### Pairs and lists

List processing is another of Lisp's great strengths.
MD)

CB
catch { ::constcl::Pair destroy }

oo::class create ::constcl::Pair {
    variable car cdr constant
    constructor {a d} {
        set car $a
        set cdr $d
        set constant 0
    }
    method bvalue {} {return #t}
    method name {} {} ;# for eval
    method numval {} {throw "Not a number"}
    method value {} {my show}
    method car {} { set car }
    method cdr {} { set cdr }
    method set-car! {val} {
        if {$constant} {
            error "Can't modify a constant pair"
        } else {
            set car $val
        }
    }
    method set-cdr! {val} {
        if {$constant} {
            error "Can't modify a constant pair"
        } else {
            set cdr $val
        }
    }
    method mkconstant {} {set constant 1}
    method constant {} {return $constant}
    method write {} {
        puts -nonewline "("
        ::constcl::write-pair [self]
        puts -nonewline ")"
    }
    method show {} {format "(%s)" [::constcl::show-pair [self]]}
}


interp alias {} ::constcl::MkPair {} ::constcl::Pair new

reg pair? ::constcl::pair?

proc ::constcl::pair? {obj} {
    if {[info object isa typeof $obj ::constcl::Pair]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Pair]} {
        return #t
    } else {
        return #f
    }
}
CB

MD(
Helper procedure to make a string representation of a list.
MD)

CB
proc ::constcl::show-pair {obj} {
    # take an object and print the car and the cdr of the stored value
    set str {}
    set a [car $obj]
    set d [cdr $obj]
    # print car
    ::append str [$a show]
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        ::append str " "
        ::append str [show-pair $d]
    } elseif {$d eq "#NIL"} {
        # cdr is nil
        return $str
    } else {
        # it is an atom
        ::append str " . "
        ::append str [$d show]
    }
    return $str
}
CB

TT(

::tcltest::test pairslists-1.0 {playing with lists} -body {
    pep {(define x (list 'a 'b 'c))}
    pep {(define y x)}
    pep {y}
    pep {(list? y)}
} -output "(a b c)\n#t\n"

::tcltest::test pairslists-1.2 {playing with lists} -body {
    pep {(set-cdr! x 4)}
    pep {x}
} -output "4\n(a . 4)\n"

::tcltest::test pairslists-1.3 {playing with lists} -body {
    pep {(eqv? x y)}
    pep {y}
} -output "#t\n(a . 4)\n"

::tcltest::test pairslists-1.4 {playing with lists} -body {
    pep {(eqv? x y)}
    pep {y}
    pep {(list? y)}
} -output "#t\n(a . 4)\n#f\n"

::tcltest::test pairslists-1.5 {try pair?} -body {
    pep {(pair? '(a . b))}
    pep {(pair? '(a b c))}
    pep {(pair? '())}
} -output "#t\n#t\n#f\n"

TT)

MD(
`cons` adds a pair to a list.
MD)

CB
reg cons ::constcl::cons

proc ::constcl::cons {car cdr} {
    MkPair $car $cdr
}
CB

TT(

::tcltest::test pairslists-1.6 {try cons} -body {
    pep {(cons 'a '())}
    pep {(cons '(a) '(b c d))}
    pep {(cons "a" '(b c))}
    pep {(cons 'a 3)}
    pep {(cons '(a b) 'c)}
} -output "(a)\n((a) b c d)\n(\"a\" b c)\n(a . 3)\n((a b) . c)\n"

TT)

MD(
`car` gets the contents of the first cell in a pair.
MD)

CB
reg car ::constcl::car

proc ::constcl::car {obj} {
    $obj car
}
CB

TT(

::tcltest::test pairslists-1.7 {try car} -body {
    pep {(car '(a b c))}
    pep {(car '((a) b c d))}
    pep {(car '(1 . 2))}
} -output "a\n(a)\n1\n"

::tcltest::test pairslists-1.8 {try car} -body {
    pep {(car '())}
} -returnCodes error -result "PAIR expected"

TT)

MD(
`cdr` gets the contents of the second cell in a pair.
MD)

CB
reg cdr ::constcl::cdr

proc ::constcl::cdr {obj} {
    $obj cdr
}
CB

TT(

::tcltest::test pairslists-1.9 {try cdr} -body {
    pep {(cdr '((a) b c d))}
    pep {(cdr '(1 . 2))}
} -output "(b c d)\n2\n"

::tcltest::test pairslists-1.10 {try cdr} -body {
    pep {(cdr '())}
} -returnCodes error -result "PAIR expected"

TT)

MD(
`set-car!` sets the contents of the first cell in a pair.
MD)

CB
reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {obj val} {
    $obj set-car! $val
}
CB

TT(

::tcltest::test pairslists-1.11 {try set-car!} -body {
    pep {(define f (lambda () (list 'not-a-constant-list)))}
    pep {(define g (lambda () '(constant-list)))}
    pep {(set-car! (f) 3)}
} -output "3\n"

::tcltest::test pairslists-1.12 {try set-car!} -body {
    pep {(set-car! (g) 3)}
} -returnCodes error -result "Can't modify a constant pair"

TT)

MD(
`set-cdr!` sets the contents of the second cell in a pair.
MD)

CB
reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {obj val} {
    $obj set-cdr! $val
}
CB

TT(

::tcltest::test pairslists-1.13 {try set-cdr!} -body {
    pep {(define f (lambda () (list 'not-a-constant-list)))}
    pep {(define g (lambda () '(constant-list)))}
    pep {(set-cdr! (f) 3)}
} -output "3\n"

::tcltest::test pairslists-1.14 {try set-cdr!} -body {
    pep {(set-cdr! (g) 3)}
} -returnCodes error -result "Can't modify a constant pair"

TT)

MD(
The `list?` predicate tests if a pair is part of a proper list, one that
ends with NIL.
MD)

CB
reg list? ::constcl::list?

proc ::constcl::list? {obj} {
    if {$obj eq "#NIL"} {
        return #t
    } elseif {[pair? $obj] eq "#t"} {
        if {[cdr $obj] eq "#NIL"} {
            return #t
        } else {
            return [list? [cdr $obj]]
        }
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test pairslists-1.15 {try list?} -body {
    pep {(list? '(a b c))}
    pep {(list? '())}
    pep {(list? '(a . b))}
} -output "#t\n#t\n#f\n"

::tcltest::test pairslists-1.16 {try list?} -constraints knownBug -body {
    pep {(let ((x (list 'a)))
          (set-cdr! x x)
          (list? x))}
} -output "#f"

TT)

MD(
`list` constructs a Lisp list from a Tcl list of items.
MD)

CB
reg list ::constcl::list

proc ::constcl::list {args} {
    if {[llength $args] == 0} {
        return #NIL
    } else {
        set prev #NIL
        foreach obj [lreverse $args] {
            set prev [cons $obj $prev]
        }
        return $prev
    }
}
CB

TT(

::tcltest::test pairslists-1.17 {try list} -body {
    pep {(list 'a (+ 3 4) 'c)}
    pep {(list)}
} -output "(a 7 c)\n()\n"

TT)

MD(
`length` reports the length of a Lisp list of items.
MD)

CB
proc ::constcl::length-helper {obj} {
    if {$obj eq "#NIL"} {
        return 0
    } else {
        return [expr {1 + [length-helper [cdr $obj]]}]
    }
}

reg length ::constcl::length

proc ::constcl::length {obj} {
    if {[list? $obj] eq "#t"} {
        MkNumber [length-helper $obj]
    } else {
        error "LIST expected\n(list lst)"
    }
}
CB

TT(

::tcltest::test pairslists-1.18 {try length} -body {
    pep {(length '(a b c))}
    pep {(length '(a (b) (c d e)))}
    pep {(length '())}
} -output "3\n3\n0\n"

TT)

MD(
`append` joins lists together.
MD)

CB
proc ::constcl::copy-list {obj next} {
    # TODO only fresh conses in the direct chain to NIL
    if {[null? $obj] eq "#t"} {
        set next
    } elseif {[null? [cdr $obj]] eq "#t"} {
        cons [car $obj] $next
    } else {
        cons [car $obj] [copy-list [cdr $obj] $next]
    }
}

reg append ::constcl::append

proc ::constcl::append {args} {
    set prev [lindex $args end]
    foreach r [lreverse [lrange $args 0 end-1]] {
        set prev [copy-list $r $prev]
    }
    set prev
}
CB

TT(

::tcltest::test pairslists-1.19 {try append} -body {
    pep {(append '(x) '(y))}
    pep {(append '(a) '(b c d))}
    pep {(append '(a (b)) '((c)))}
    pep {(append '(a b) '(c . d))}
    pep {(append '() 'a)}
} -output "(x y)\n(a b c d)\n(a (b) (c))\n(a b c . d)\na\n"

TT)

MD(
`reverse` produces a reversed copy of a Lisp list.
MD)

CB
reg reverse ::constcl::reverse

proc ::constcl::reverse {obj} {
    list {*}[lreverse [splitlist $obj]]
}
CB

TT(

::tcltest::test pairslists-1.20 {try reverse} -body {
    pep {(reverse '(a b c))}
    pep {(reverse '(a (b c) d (e (f))))}
} -output "(c b a)\n((e (f)) d (b c) a)\n"

TT)

MD(
Given a list index, `list-tail` yields the sublist starting from that index.
MD)

CB
reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {obj k} {
    if {[zero? $k] eq "#t"} {
        return $obj
    } else {
        list-tail [cdr $obj] [- $k #1]
    }
}
CB

TT(

::tcltest::test pairslists-1.21 {try list-tail} -body {
    pep {(list-tail '(a b c d) 2)}
} -output "(c d)\n"

TT)

MD(
`list-ref` yields the list item at a given index.
MD)

CB
reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {obj k} {
    car [list-tail $obj $k]
}
CB

TT(

::tcltest::test pairslists-1.22 {try list-ref} -body {
    pep {(list-ref '(a b c d) 2)}
} -output "c\n"

TT)

MD(
`memq`, `memv`, and `member` return the sublist starting with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison.
MD)

CB
reg memq ::constcl::memq

proc ::constcl::memq {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[eq? $obj1 [car $obj2]] eq "#t"} {
                return $obj2
            } else {
                return [memq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 show] [$obj2 show])"
    }
}
CB

TT(

::tcltest::test pairslists-1.23 {try memq, memv} -body {
    pep {(memq 'a '(a b c))}
    pep {(memq 'b '(a b c))}
    pep {(memq 'a '(b c d))}
    pep {(memq (list 'a) '(b (a) c))}
    pep {(memq 101 '(100 101 102))}
    pep {(memv 101 '(100 101 102))}
} -output "(a b c)\n(b c)\n#f\n#f\n(101 102)\n(101 102)\n"

::tcltest::test pairslists-1.24 {try member} -body {
    pep {(member (list 'a) '(b (a) c))}
} -output "((a) c)\n"

TT)

CB
reg memv ::constcl::memv

proc ::constcl::memv {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[eqv? $obj1 [car $obj2]] eq "#t"} {
                return $obj2
            } else {
                return [memv $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memv [$obj1 show] [$obj2 show])"
    }
}
CB

CB
reg member ::constcl::member

proc ::constcl::member {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[equal? $obj1 [car $obj2]] eq "#t"} {
                return $obj2
            } else {
                return [member $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(member [$obj1 show] [$obj2 show])"
    }
}
CB

MD(
`assq`, `assv`, and `assoc` return the associative item marked with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison.
MD)

CB
reg assq ::constcl::assq

proc ::constcl::assq {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[pair? [car $obj2]] eq "#t" && [eq? $obj1 [caar $obj2]] eq "#t"} {
                return [car $obj2]
            } else {
                return [assq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(assq [$obj1 show] [$obj2 show])"
    }
}
CB


CB
reg assv ::constcl::assv

proc ::constcl::assv {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[pair? [car $obj2]] eq "#t" && [eqv? $obj1 [caar $obj2]] eq "#t"} {
                return [car $obj2]
            } else {
                return [assq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(assv [$obj1 show] [$obj2 show])"
    }
}
CB

CB
reg assoc ::constcl::assoc

proc ::constcl::assoc {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[pair? [car $obj2]] eq "#t" && [equal? $obj1 [caar $obj2]] eq "#t"} {
                return [car $obj2]
            } else {
                return [assq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(assoc [$obj1 show] [$obj2 show])"
    }
}
CB

TT(
::tcltest::test pairslists-1.25 {try member} -body {
    pep {(define e '((a 1) (b 2) (c 3)))}
    pep {(assq 'a e)}
    pep {(assq 'b e)}
    pep {(assq 'd e)}
    pep {(assq (list 'a) '(((a)) ((b)) ((c))))}
    pep {(assoc (list 'a) '(((a)) ((b)) ((c))))}
    pep {(assq 5 '((2 3) (5 7) (11 13)))}
    pep {(assv 5 '((2 3) (5 7) (11 13)))}
} -output "(a 1)\n(b 2)\n#f\n#f\n((a))\n(5 7)\n(5 7)\n"
TT)

