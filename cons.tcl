
unset -nocomplain M
set M 0 # memory cell number

unset -nocomplain S
set S 0 # string store number

unset -nocomplain StrSto
set StrSto [list]

NIL create Mem0
interp alias {} #NIL {} Mem0

interp alias {} #t {} Mem1
Boolean create Mem[incr ::M] #t

interp alias {} #f {} Mem2
Boolean create Mem[incr ::M] #f

Number create Mem[incr ::M] -1
interp alias {} #-1 {} Mem$::M

Number create Mem[incr ::M] 0
interp alias {} #0 {} Mem$::M

Number create Mem[incr ::M] 1
interp alias {} #1 {} Mem$::M


EndOfFile create Mem[incr ::M]
interp alias {} #EOF {} Mem$::M


proc ::constcl::= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(= num...)"
    }
    ::tcl::mathop::== {*}$vals
}

proc ::constcl::< {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(< num...)"
    }
    ::tcl::mathop::< {*}$vals
}

proc ::constcl::> {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(> num...)"
    }
    ::tcl::mathop::> {*}$vals
}

proc ::constcl::<= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(<= num...)"
    }
    ::tcl::mathop::<= {*}$vals
}

proc ::constcl::>= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(>= num...)"
    }
    ::tcl::mathop::>= {*}$vals
}

proc ::constcl::zero? {obj} {
    if {[::constcl::number? $obj] eq #t} {
        if {[$obj value] == 0} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(zero? [$obj write])"
    }
}

proc ::constcl::positive? {obj} {
    if {[::constcl::number? $obj] eq #t} {
        if {[$obj positive]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(positive? [$obj write])"
    }
}

proc ::constcl::negative? {obj} {
    if {[::constcl::number? $obj] eq #t} {
        if {[$obj negative]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(negative? [$obj write])"
    }
}

proc ::constcl::even? {obj} {
    if {[::constcl::number? $obj] eq #t} {
        if {[$obj even]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(even? [$obj write])"
    }
}

proc ::constcl::odd? {obj} {
    if {[::constcl::odd? $obj] eq #t} {
        if {[$obj even]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(odd? [$obj write])"
    }
}

proc ::constcl::max {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(max num...)"
    }
    ::tcl::mathop::max {*}$vals
}

proc ::constcl::min {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(min num...)"
    }
    ::tcl::mathop::min {*}$vals
}

proc ::constcl::+ {args} {
    if {[llength $args] == 0} {
        return #0
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            return $obj
        } else {
            error "NUMBER expected\n(+ [$obj write])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            set num $obj
        } else {
            error "NUMBER expected\n(+ [$obj write])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::constcl::number? $obj eq "#t"]} {
                $num incr [$obj value]
            } else {
                error "NUMBER expected\n(- [$obj write])"
            }
        }
        return $num
    }
}

proc ::constcl::* {args} {
    if {[llength $args] == 0} {
        return #1
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            return $obj
        } else {
            error "NUMBER expected\n(+ [$obj write])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            set num $obj
        } else {
            error "NUMBER expected\n(+ [$obj write])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::constcl::number? $obj eq "#t"]} {
                $num mult [$obj value]
            } else {
                error "NUMBER expected\n(- [$obj write])"
            }
        }
        return $num
    }
}

proc ::constcl::- {args} {
    if {[llength $args] == 0} {
        error "expected arguments"
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            return [Number create Mem[incr ::M] -[$obj value]]
        } else {
            error "NUMBER expected\n(- [$obj write])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            set num $obj
        } else {
            error "NUMBER expected\n(- [$obj write])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::constcl::number? $obj eq "#t"]} {
                $num decr [$obj value]
            } else {
                error "NUMBER expected\n(- [$obj write])"
            }
        }
        return $num
    }
}

proc ::constcl::/ {args} {
    if {[llength $args] == 0} {
        error "expected arguments"
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            return [Number create Mem[incr ::M] [expr {1 / [$obj value]]
        } else {
            error "NUMBER expected\n(- [$obj write])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj eq "#t"]} {
            set num $obj
        } else {
            error "NUMBER expected\n(- [$obj write])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::constcl::number? $obj eq "#t"]} {
                $num decr [$obj value]
            } else {
                error "NUMBER expected\n(- [$obj write])"
            }
        }
        return $num
    }
}

proc ::constcl::abs {x} {
    if {[::constcl::number? $x] eq #t} {
        if {[$x negative]} {
            return [Number create Mem[incr ::M] [expr {[$x value] * -1}]]
        } else {
            return $x
        }
    } else {
        error "NUMBER expected\n(abs [$x write])"
    }
}

proc ::constcl::quotient {n1 n2} {
    # TODO
}

proc ::constcl::remainder {n1 n2} {
    # TODO
}

proc ::constcl::modulo {n1 n2} {
    # TODO
}

proc ::constcl::gcd {args} {
    # TODO
}

proc ::constcl::lcm {args} {
    # TODO
}

proc ::constcl::numerator {q} {
    # TODO
}

proc ::constcl::denominator {q} {
    # TODO
}

proc ::constcl::floor {x} {
    if {[::constcl::number? $x] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::floor [$x value]]
    } else {
        error "NUMBER expected\n(floor [$x write])"
    }
}

proc ::constcl::ceiling {x} {
    if {[::constcl::number? $x] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::ceil [$x value]]
    } else {
        error "NUMBER expected\n(ceiling [$x write])"
    }
}

proc ::constcl::truncate {x} {
    if {[::constcl::number? $x] eq #t} {
        # TODO
    } else {
        error "NUMBER expected\n(truncate [$x write])"
    }
}

proc ::constcl::round {x} {
    if {[::constcl::number? $x] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::round [$x value]]
    } else {
        error "NUMBER expected\n(round [$x write])"
    }
}

proc ::constcl::rationalize {x y} {
    # TODO
}

proc ::constcl::exp {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::exp [$z value]]
    } else {
        error "NUMBER expected\n(exp [$z write])"
    }
}

proc ::constcl::log {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::log [$z value]]
    } else {
        error "NUMBER expected\n(log [$z write])"
    }
}

proc ::constcl::sin {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::sin [$z value]]
    } else {
        error "NUMBER expected\n(sin [$z write])"
    }
}

proc ::constcl::cos {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::cos [$z value]]
    } else {
        error "NUMBER expected\n(cos [$z write])"
    }
}

proc ::constcl::tan {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::tan [$z value]]
    } else {
        error "NUMBER expected\n(tan [$z write])"
    }
}

proc ::constcl::asin {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::asin [$z value]]
    } else {
        error "NUMBER expected\n(asin [$z write])"
    }
}

proc ::constcl::acos {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::acos [$z value]]
    } else {
        error "NUMBER expected\n(acos [$z write])"
    }
}

proc ::constcl::atan {args} {
    if {[llength $args] == 1} {
        if {[::constcl::number? $z] eq #t} {
            Number create Mem[incr ::M] [::tcl::mathfunc::atan [$z value]]
        } else {
            error "NUMBER expected\n(atan [$z write])"
        }
    } else {
        lassign $args y x
        if {[::constcl::number? $y] eq #t && [::constcl::number $x]} {
            Number create Mem[incr ::M] [::tcl::mathfunc::atan2 [$y value] [$x value]]
        } else {
            error "NUMBER expected\n(atan [$y write] [$x write])"
        }
    }
}

proc ::constcl::sqrt {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::sqrt [$z value]]
    } else {
        error "NUMBER expected\n(sqrt [$z write])"
    }
}

proc ::constcl::expt {z1 z2} {
    if {[::constcl::number? $z1] eq #t && [::constcl::number $z2]} {
        Number create Mem[incr ::M] [::tcl::mathfunc::pow [$z1 value] [$z2 value]]
    } else {
        error "NUMBER expected\n(expt [$z1 write] [$z2 write])"
    }
}

proc ::constcl::make-rectangular {x1 x2} {
    # TODO
}

proc ::constcl::make-polar {x3 x4} {
    # TODO
}

proc ::constcl::real-part {z} {
    # TODO
}

proc ::constcl::imag-part {z} {
    # TODO
}

proc ::constcl::magnitude {z} {
    # TODO
}

proc ::constcl::angle {z} {
    # TODO
}

proc ::constcl::exact->inexact {z} {
    # TODO
}

proc ::constcl::inexact->exact {z} {
    # TODO
}

proc ::constcl::number->string {args} {
    # TODO
}

proc ::constcl::string->number {args} {
    # TODO
}

proc ::constcl::cons {car cdr} {
    Cons create Mem[incr ::M] $car $cdr
}

proc ::constcl::car {obj} {
    $obj car
}

proc ::constcl::cdr {obj} {
    $obj cdr
}

proc ::constcl::set-car! {obj val} {
    $obj set-car! $val
}

proc ::constcl::set-cdr! {obj val} {
    $obj set-cdr! $val
}

proc ::constcl::list {args} {
    set prev #NIL
    foreach obj [lreverse $args] {
        set prev [::constcl::cons $obj $prev]
    }
    return $prev
}

proc ::constcl::list? {obj} {
    # TODO need to work on this a bit more
    if {[info object isa typeof $obj Cons] || $obj eq "Mem0"} {
        return #t
    } else {
        return #f
    }
}

proc ::constcl::not {obj} {
    if {[$obj truth] eq "#f"} {
        return #t
    } else {
        return #f
    }
}

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
        error "LIST expected\n(length [$obj write])"
    }
}

proc ::constcl::append {args} {
    # TODO
}

proc ::constcl::reverse {obj} {
    # TODO
}

proc ::constcl::list-tail {obj k} {
    if {[::constcl::zero? $k]} {
        return $obj
    } else {
        ::constcl::list-tail [::constcl::cdr $obj] [::constcl::- $k #1]
    }
}

proc ::constcl::list-ref {obj k} {
    ::constcl::car [::constcl::list-tail $obj $k]
}

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
        error "LIST expected\n(memq [$obj1 write] [$obj2 write])"
    }
}

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
        error "LIST expected\n(memv [$obj1 write] [$obj2 write])"
    }
}

proc ::constcl::eqv? {obj1 obj2} {
    if {[::constcl::eq? $obj1 $obj2]} {
        return #t
    } else {
        return #f
    }
}

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
        error "LIST expected\n(member [$obj1 write] [$obj2 write])"
    }
}

proc ::constcl::equal? {obj1 obj2} {
    if {[::constcl::eqv? $obj1 $obj2]} {
        return #t
    } else {
        if {[$obj1 write] eq [$obj2 write]} {
            return #t
        } else {
            return #f
        }
        # TODO
    }
}

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
        error "LIST expected\n(memq [$obj1 write] [$obj2 write])"
    }
}


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
        error "LIST expected\n(memq [$obj1 write] [$obj2 write])"
    }
}

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
        error "LIST expected\n(memq [$obj1 write] [$obj2 write])"
    }
}

proc ::constcl::symbol->string {obj} {
    if {[::constcl::symbol? $obj] eq "#t"} {
        return [$obj name]
    } else {
        error "SYMBOL expected\n(symbol->string [$obj write])"
    }
}

proc ::constcl::string->symbol {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [Symbol create Mem[incr ::M] [$str value]]
    } else {
        error "STRING expected\n(string->symbol [$obj write])"
    }
}

proc ::constcl::char=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] eq [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char<? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] < [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char>? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] > [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] <= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] >= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char-ci=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] eq [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char-ci<? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] < [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char-ci>? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] > [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char-ci<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] <= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char-ci>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] >= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 write] [$c2 write])"
    }
}

proc ::constcl::char-alphabetic? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char alphabetic?]
    } else {
        error "CHAR expected\n(char-alphabetic? [$char write])"
    }
}

proc ::constcl::char-numeric? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char numeric?]
    } else {
        error "CHAR expected\n(char-numeric? [$char write])"
    }
}

proc ::constcl::char-whitespace? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char whitespace?]
    } else {
        error "CHAR expected\n(char-whitespace? [$char write])"
    }
}

proc ::constcl::char-upper-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char write])"
    }
}

proc ::constcl::char-lower-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char write])"
    }
}

proc ::constcl::char->integer {char} {
    # TODO
}

proc ::constcl::integer->char {n} {
    # TODO
}

proc ::constcl::char-upcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [Char create Mem[incr ::M] [string toupper [$char char]]]
    } else {
        error "CHAR expected\n(char-upcase [$char write])"
    }
}


proc ::constcl::char-downcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [Char create Mem[incr ::M] [string tolower [$char char]]]
    } else {
        error "CHAR expected\n(char-downcase [$char write])"
    }
}

proc ::constcl::make-string {args} {
    # TODO
}

proc ::constcl::string {args} {
    set str {}
    foreach char $args {
        if {[::constcl::char? $char] eq "#t"} {
            append str [$char char]
        } else {
            error "CHAR expected\n(string [$char write])"
        }
    }
    return [String create Mem[incr ::M] $str]
}

proc ::constcl::string-length {str} {
    if {[::constcl::str? $String] eq "#t"} {
        return [Number create Mem[incr ::M] [$str length]]
    } else {
        error "STRING expected\n(string-length [$str write])"
    }
}

proc ::constcl::string-ref {str k} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-ref [$str write] [$k write])"
        }
        return [$str ref $i]
    } else {
        error "STRING expected\n(string-ref [$str write] [$k write])"
    }
}

proc ::constcl::string-set! {str k char} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-set! [$str write] [$k write] [$char write])"
        }
        if {[::constcl::char? $char] eq "#t"} {
            return [$str set! $i [$char char]]
        } else {
            error "CHAR expected\n(string-set! [$str write] [$k write] [$char write])"
        }
    } else {
        error "STRING expected\n(string-set! [$str write] [$k write] [$char write])"
    }
}

proc ::constcl::string=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] eq [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string=? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string-ci=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] eq [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci=? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string<? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] < [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string-ci<? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] < [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string>? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] > [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string-ci>? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] > [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] <= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<=? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string-ci<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] <= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<=? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] >= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>=? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::string-ci>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] >= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>=? [$s1 write] [$s2 write])"
    }
}

proc ::constcl::substring {str start end} {
    if {[::constcl::string? $str] eq "t"} {
        if {[::constcl::number? $start] eq "t" && [::constcl::number? $end] eq "t"} {
            return {String create Mem[incr ::M] [$str substring [$start value] [$end value]]
        } else {
            error "NUMBER expected\n(substring [$str write] [$start write] [$end write])"
        }
    } else {
        error "STRING expected\n(substring [$str write] [$start write] [$end write])"
    }
}

proc ::constcl::string-append {args} {
    # TODO
}

proc ::constcl::string->list {str} {
    # TODO
}

proc ::constcl::list->string {list} {
    # TODO
}

proc ::constcl::string-copy {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [String create Mem[incr ::M] [$str value]]
    } else {
        error "STRING expected\n(string-copy [$str write])"
    }
}

proc ::constcl::string-fill! {str char} {
    if {[::constcl::string? $str] eq "#t"} {
        return [String create Mem[incr ::M] [$str fill [$char value]]]
    } else {
        error "STRING expected\n(string-fill [$str write] [$char write])"
    }
}

proc ::constcl::make-vector {args} {
    # TODO
}

proc ::constcl::vector {args} {
    # TODO
}

proc ::constcl::vector-length {vec} {
    if {[::constcl::vector? $vec] eq "#t"} {
        return [Number create Mem[incr ::M] [$str length]]]
    } else {
        error "VECTOR expected\n(vector-length [$vec write])"
    }
}

proc ::constcl::vector-ref {vec k} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str ref $k]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec write] [$k write])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec write] [$k write])"
    }
}


proc ::constcl::vector-set! {vec k obj} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str set! $k $obj]]
        } else {
            error "NUMBER expected\n(vector-set! [$vec write] [$k write] [$obj write])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec write] [$k write] [$obj write])"
    }
}

proc ::constcl::vector->list {vec} {
    # TODO
}

proc ::constcl::list->vector {list} {
    # TODO
}

proc ::constcl::vector-fill {vec fill} {
    if {[::constcl::vector? $vec] eq "#t"} {
        $vec fill $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec write] [$fill write])"
    }
}

proc ::constcl::apply {proc args} {
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

proc ::constcl::force {promise} {
    # TODO
}

proc ::constcl::call-with-current-continuation {proc} {
    # TODO
}

proc ::constcl::values {args} {
    # TODO
}

proc ::constcl::call-with-values {producer consumer} {
    # TODO
}

proc ::constcl::dynamic-wind {before thunk after} {
    # TODO
}

proc ::constcl::eval {expression environment_specifier} {
    # TODO
}

proc ::constcl::scheme-report-environment {version} {
    # TODO
}

proc ::constcl::null-environment {version} {
    # TODO
}

proc ::constcl::interaction-environment {} {
    # TODO
}

proc ::constcl::call-with-input-file string {proc} {
    # TODO
}

proc ::constcl::call-with-output-file string {proc} {
    # TODO
}

proc ::constcl::input-port? {obj} {
    # TODO
}

proc ::constcl::output-port? {obj} {
    # TODO
}

proc ::constcl::current-input-port {} {
    # TODO
}

proc ::constcl::current-output-port {} {
    # TODO
}

proc ::constcl::with-input-from-file {string thunk} {
    # TODO
}


proc ::constcl::with-output-to-file {string thunk} {
    # TODO
}

proc ::constcl::open-input-file {filename} {
    # TODO
}

proc ::constcl::open-output-file {filename} {
    # TODO
}

proc ::constcl::close-input-port {port} {
    # TODO
}

proc ::constcl::close-output-port {port} {
    # TODO
}

proc ::constcl::read {args} {
    # TODO
}

proc ::constcl::read-char {args} {
    # TODO
}

proc ::constcl::peek-char {args} {
    # TODO
}

proc ::constcl::char-ready? {args} {
    # TODO
}

proc ::constcl::write {obj args} {
    # TODO write [$obj write]
}

proc ::constcl::display {obj args} {
    # TODO write [$obj display]
}

proc ::constcl::newline {args} {
    # TODO write newline
}

proc ::constcl::write-char {args} {
    # TODO
}

proc ::constcl::load {filename} {
    # TODO
}

proc ::constcl::transcript-on {filename} {
    # TODO
}

proc ::constcl::transcript-off {} {
    # TODO
}

