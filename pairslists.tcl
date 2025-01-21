
MD(
## Pairs and lists
MD)

CB
oo::class create Cons {
    variable car cdr
    constructor {a d} {
        set truth Mem1
        set car $a
        set cdr $d
    }
    method truth {} {return #t}
    method numval {} {throw "Not a number"}
    method car {} { set car }
    method cdr {} { set cdr }
    method set-car! {val} { set car $val }
    method set-cdr! {val} { set cdr $val }
    method write {} {
        puts -nonewline "("
        ::constcl::write-pair [self]
        puts -nonewline ")"
    }
    method show {} {format "%s . %s" [my car] [my cdr]}
}

proc ::constcl::MkCons {a d} {
    return [Cons create Mem[incr ::M] $a $d]
}
CB

CB
reg pair? ::constcl::pair?

proc ::constcl::pair? {obj} {
    if {[info object isa typeof $obj Cons]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Cons]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg cons ::constcl::cons

proc ::constcl::cons {car cdr} {
    MkCons $car $cdr
}
CB

CB
reg car ::constcl::car

proc ::constcl::car {obj} {
    $obj car
}
CB

CB
reg cdr ::constcl::cdr

proc ::constcl::cdr {obj} {
    $obj cdr
}
CB

CB
reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {obj val} {
    $obj set-car! $val
}
CB

CB
reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {obj val} {
    $obj set-cdr! $val
}
CB

CB
reg list ::constcl::list

proc ::constcl::list {args} {
    if {[llength $args] == 0} {
        return #NIL
    } else {
        set prev #NIL
        foreach obj [lreverse $args] {
            set prev [::constcl::cons $obj $prev]
        }
        return $prev
    }
}
CB

CB
reg list? ::constcl::list?

proc ::constcl::list? {obj} {
    # TODO need to work on this a bit more
    if {[info object isa typeof $obj Cons] || $obj eq "Mem0"} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg length ::constcl::length

proc ::constcl::length {obj} {
    if {$obj eq "#NIL"} {
        return #0
    } elseif {[info object isa typeof $obj Cons]} {
        if {[info object isa typeof [cdr $obj] Cons]} {
            return [[::constcl::length [cdr $obj]] 1+]
        } else {
            error "Ill-formed procedure call"
        }
    } else {
        error "LIST expected\n(length [$obj show])"
    }
}
CB

CB
reg append ::constcl::append

proc ::constcl::append {args} {
    # TODO
}
CB

CB
proc ::constcl::reverse {obj} {
    # TODO
}
CB

CB
reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {obj k} {
    if {[::constcl::zero? $k]} {
        return $obj
    } else {
        ::constcl::list-tail [::constcl::cdr $obj] [::constcl::- $k #1]
    }
}
CB

CB
reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {obj k} {
    ::constcl::car [::constcl::list-tail $obj $k]
}
CB

CB
reg memq ::constcl::memq

proc ::constcl::memq {obj1 obj2} {
    if {[::constcl::list? $obj2] eq "#t"} {
        if {[::constcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::constcl::pair? $obj2] eq "#t"} {
            if {[::constcl::eq? $obj1 [::constcl::car $obj2]]} {
                return $obj2
            } else {
                return [::constcl::memq $obj1 [::constcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 show] [$obj2 show])"
    }
}
CB

CB
reg eq? ::constcl::eq?

proc ::constcl::eq? {obj1 obj2} {
    # TODO
    if {$obj1 eq $obj2} {
        return #t
    } elseif {[::constcl::number? $obj1] && [::constcl::number? $obj] && [$obj = [$obj value]]} {
        return #t
    } elseif {[::constcl::char? $obj1] && [::constcl::char? $obj] && [$obj1 = [$obj2 value]]} {
        return #t
    } elseif {[::constcl::string? $obj1] && [::constcl::string? $obj2] && [$obj index] eq [$obj2 index]]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg memv ::constcl::memv

proc ::constcl::memv {obj1 obj2} {
    if {[::constcl::list? $obj2] eq "#t"} {
        if {[::constcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::constcl::pair? $obj2] eq "#t"} {
            if {[::constcl::eqv? $obj1 [::constcl::car $obj2]]} {
                return $obj2
            } else {
                return [::constcl::memv $obj1 [::constcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memv [$obj1 show] [$obj2 show])"
    }
}
CB

CB
reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {obj1 obj2} {
    if {[::constcl::eq? $obj1 $obj2]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg member ::constcl::member

proc ::constcl::member {obj1 obj2} {
    if {[::constcl::list? $obj2] eq "#t"} {
        if {[::constcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::constcl::pair? $obj2] eq "#t"} {
            if {[::constcl::equal? $obj1 [::constcl::car $obj2]]} {
                return $obj2
            } else {
                return [::constcl::member $obj1 [::constcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(member [$obj1 show] [$obj2 show])"
    }
}
CB

CB
reg equal? ::constcl::equal?

proc ::constcl::equal? {obj1 obj2} {
    if {[::constcl::eqv? $obj1 $obj2]} {
        return #t
    } else {
        if {[$obj1 show] eq [$obj2 show]} {
            return #t
        } else {
            return #f
        }
        # TODO
    }
}
CB

CB
proc ::constcl::assq {obj1 obj2} {
    if {[::constcl::list? $obj2] eq "#t"} {
        if {[::constcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::constcl::pair? $obj2] eq "#t"} {
            #TODO replace with a-list handling code
            if {[::constcl::eq? $obj1 [::constcl::car $obj2]]} {
                return $obj2
            } else {
                return [::constcl::memq $obj1 [::constcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 show] [$obj2 show])"
    }
}
CB


CB
proc ::constcl::assv {obj1 obj2} {
    if {[::constcl::list? $obj2] eq "#t"} {
        if {[::constcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::constcl::pair? $obj2] eq "#t"} {
            #TODO replace with a-list handling code
            if {[::constcl::eqv? $obj1 [::constcl::car $obj2]]} {
                return $obj2
            } else {
                return [::constcl::memq $obj1 [::constcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 show] [$obj2 show])"
    }
}
CB

CB
proc ::constcl::assoc {obj1 obj2} {
    if {[::constcl::list? $obj2] eq "#t"} {
        if {[::constcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::constcl::pair? $obj2] eq "#t"} {
            #TODO replace with a-list handling code
            if {[::constcl::equal? $obj1 [::constcl::car $obj2]]} {
                return $obj2
            } else {
                return [::constcl::memq $obj1 [::constcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 show] [$obj2 show])"
    }
}
CB

CB
reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {obj} {
    if {[::constcl::symbol? $obj] eq "#t"} {
        return [$obj name]
    } else {
        error "SYMBOL expected\n(symbol->string [$obj show])"
    }
}
CB

CB
reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkSymbol [$str value]]
    } else {
        error "STRING expected\n(string->symbol [$obj show])"
    }
}
CB

