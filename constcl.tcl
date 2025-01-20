
oo::class create NIL {
    constructor {} {}
    method truth {} {return #t}
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method numval {} {throw "Not a number"}
    method write {} {return "()"}
}

namespace eval ::constcl {}

proc ::constcl::null? {obj} {
    if {[info object isa typeof $obj NIL]} {
        return #t
    } else {
        return #f
    }
}

oo::class create EndOfFile {}

proc ::eof-object? {obj} {
    if {[info object isa typeof $obj EndOfFile]} {
        return #t
    } else {
        return #f
    }
}

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
    method write {} { return "([$car write] . [$cdr write])" }
}

proc ::constcl::pair? {obj} {
    if {[info object isa typeof $obj Cons]} {
        return #t
    } else {
        return #f
    }
}

oo::class create Boolean {
    superclass NIL
    variable truth
    constructor {v} {
        set truth $v
    }
    method truth {} {
        set truth
    }
    method write {} {return $truth}
}

proc ::constcl::boolean? {obj} {
    if {[info object isa typeof $obj Boolean]} {
        return #t
    } else {
        return #f
    }
}

oo::class create Number {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method = {num} {expr {$value == $num}}
    method positive {} {expr {$value > 0}}
    method negative {} {expr {$value < 0}}
    method even {} {expr {$value % 2 == 0}}
    method odd {} {expr {$value % 2 == 1}}
    method 1+ {} {incr value}
    method incr {val} {incr value $val}
    method mult {val} {set value [expr {$value * $val}]}
    method decr {val} {incr value -$val}
    method div {val} {set value [expr {$value / $val}]}
    method value {} { set value }
    method numval {} {set value}
    method write {} {return $value}
}

proc ::constcl::number? {obj} {
    if {[info object isa typeof $obj Number]} {
        return #t
    } else {
        return #f
    }
}

oo::class create Symbol {
    superclass NIL
    variable name
    constructor {n} {
        # TODO idcheck this
        set name $n
    }
    method name {} {set name}
    method = {symname} {expr {$name eq $symname}}
    method write {} {return $name}
}

proc ::constcl::symbol? {obj} {
    if {[info object isa typeof $obj Symbol]} {
        return #t
    } else {
        return #f
    }
}

oo::class create String {
    superclass NIL
    variable s
    constructor {v} {
        set s $::S
        lset ::StrSto $s $v
        incr ::S
    }
    method index {} {set s}
    method = {str} {string equal [lindex $::StrSto $s] $str}
    method length {} {string length [lindex $::StrSto $s]}
    method ref {i} {string index [lindex $::StrSto $s] $i}
    method value {} {return [lindex $::StrSto $s]}
    method write {} {return "\"[lindex $::StrSto $s]\""}
}

proc ::constcl::string? {obj} {
    if {[info object isa typeof $obj String]} {
        return #t
    } else {
        return #f
    }
}

oo::class create Char {
    superclass NIL
    variable value
    constructor {v} {
        # TODO check for #\ and set character names to lowercase
        set value $v
    }
    method char {} {
        if {[regexp {^#\\[A-Za-z]$} [my value]]} {
            return [string index [my value] 2]
        } elseif {[regexp {^#\\([[:graph:]]+)$} [my value] -> char_name]} {
            # TODO
            switch $char_name {
                space {return " "}
                newline {return "\n"}
            }
        }
    }
    method alphabetic? {} {
        if {[string is alpha [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method numeric? {} {
        if {[string is digit [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method whitespace? {} {
        if {[string is space [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method upper-case? {} {
        if {[string is upper [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case? {} {
        if {[string is lower [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method value {} {return $value}
    method write {} {return "\"$value\""}
}

proc ::constcl::char? {obj} {
    if {[info object isa typeof $obj Char]} {
        return #t
    } else {
        return #f
    }
}

oo::class create Vector {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method length {} {string length $value}
    method ref {i} {string index $value $i}
    method value {} {return $value}
    method write {} {return #($value)}
}

proc ::constcl::vector? {obj} {
    if {[info object isa typeof $obj Vector]} {
        return #t
    } else {
        return #f
    }
}

oo::class create Procedure {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method value {} {
        set value
    }
    method write {} {return $value}
    method call {vals} {
        # TODO
    }
}

proc ::constcl::procedure? {obj} {
    if {[info object isa typeof $obj Procedure]} {
        return #t
    } else {
        return #f
    }
}


unset -nocomplain M
# memory cell number
set M 0

unset -nocomplain S
# string store number
set S 0

unset -nocomplain StrSto
set StrSto [list]

interp alias {} #NIL {} [NIL create Mem0]

interp alias {} #t {} [Boolean create Mem[incr ::M] #t]

interp alias {} #f {} [Boolean create Mem[incr ::M] #f]

interp alias {} #-1 {} [Number create Mem[incr ::M] -1]

interp alias {} #0 {} [Number create Mem[incr ::M] 0]

Number create Mem[incr ::M] 1
interp alias {} #1 {} Mem$::M

Symbol create Mem[incr ::M] quote
interp alias {} #Q {} Mem$::M

Symbol create Mem[incr ::M] +
interp alias {} #+ {} Mem$::M

Symbol create Mem[incr ::M] -
interp alias {} #- {} Mem$::M

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
            return [Number create Mem[incr ::M] [expr {1 / [$obj value]}]
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
            return [String create Mem[incr ::M] [$str substring [$start value] [$end value]]]
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

proc ::constcl::call-with-input-file {string proc} {
    # TODO
}

proc ::constcl::call-with-output-file {string proc} {
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

if no {
    # defined in read.tcl
proc ::constcl::read {args} {
    # TODO
}
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

if no {
proc ::constcl::write {obj args} {
    # TODO write [$obj write]
}
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



set inputstr {}

proc ::constcl::advance {args} {
    if {[llength $args] == 1} {
        set ::inputstr [::string range $::inputstr 1+$args end]
    } else {
        set ::inputstr [::string range $::inputstr 1 end]
    }
}

proc ::constcl::first {} {
    ::string index $::inputstr 0
}

proc ::constcl::second {} {
    ::string index $::inputstr 1
}

proc ::constcl::read {args} {
    if {$::inputstr eq {}} {set ::inputstr [gets stdin]}
    skip-whitespace
    switch -regexp [first] {
        {\(} {
            advance
            skip-whitespace
            set p [read-pair ")"]
            skip-whitespace
            if {[first] != ")"} {
                error "Missing right parenthesis."
            }
            return $p
        }
        {\[} {
            advance
            skip-whitespace
            set p [read-pair "\]"]
            skip-whitespace
            if {[first] != "\]"} {
                error "Missing right bracket."
            }
            return $p
        }
        {\+} - {\-} {
            # TODO check if + / - is a symbol
            if {![::string is digit [second]]} {
                if {[first] eq "+"} {
                    return #+
                } else {
                    return #-
                }
            } else {
                return [::constcl::read-number]
            }
        }
        {\d} {
            return [::constcl::read-number]
        }
        {#} {
            advance
            switch [first] {
                ( {
                    return [::constcl::read-vector]
                }
                t {
                    advance
                    return #t
                }
                f { 
                    advance
                    return #f
                }
                "\\" {
                    return [::constcl::read-char]
                }
                default {
                    error "Illegal #-literal"
                }
            }
        }
        {"} {
            return [::constcl::read-string]
        }
        default {
            return [::constcl::read-identifier]
        }

    }
}


proc ::constcl::read-number {} {
    while {$::inputstr ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append num [first]
        advance
    }
    if {[::string length $num] && [::string is double $num]} {
        return [Number create Mem[incr ::M] $num]
    } else {
        error "Invalid numeric constant $num"
    }
}


proc ::constcl::character-check {name} {
    regexp {^#\\([[:graph:]]|space|newline)$} $name
}

proc ::constcl::read-char {} {
    set name "#"
    while {$::inputstr ne {} && ![::string is space [first]]} {
        ::append name [first]
        advance
    }
    if {[::constcl::character-check $name]} {
        return [Char create Mem[incr ::M] $name]
    } else {
        error "Invalid character constant $name"
    }
}


proc ::constcl::read-string {} {
    set str {}
    advance
    while {[first] ne {"}} {
        set c [first]
        if {$c eq "\\"} {
            ::append str $c
            advance
            ::append str [first]
        } else {
            ::append str $c
        }
        advance
    }
    return [String create Mem[incr ::M] $str]
}


proc ::constcl::read-identifier {} {
    set name {}
    while {$::inputstr ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append name [first]
        advance
    }
    # idcheck throws error if invalid identifier
    return [Symbol create Mem[incr ::M] [::constcl::idcheck $name]]
}


proc ::constcl::skip-whitespace {} {
    # move the source pointer past whitespace and comments
    # adapted from Robert Nystrom, Crafting Interpreters
    while true {
        set c [first]
        switch $c {
            " " -
            "\r" -
            "\t" -
            "\n" {
                advance
            }
            ";" {
                # a comment goes on until the end of the line
                while {[first] != "\n" && $::inputstr ne {}} {
                    advance
                }
            }
            default {
                return
            }
        }
    }
}

proc ::constcl::find-char {c} {
    # take a character, peek beyond whitespace to find it
    set cp 0
    while {[::string is space [lindex $::inputstr $cp]]} {
        incr cp
    }
    return [expr {[lindex $::inputstr $cp] eq $c}]
}

proc ::constcl::read-pair {c} {
    # take a character, read a car and a cdr value, pass the char to findC
    skip-whitespace
    set a [read]
    if {[::string equal [::string range $::inputstr 0 3] " . "]} {
        advance 3
        skip-whitespace
        set d [read]
    } elseif {[find-char $c]} {
        skip-whitespace
        set d #NIL
    } else {
        skip-whitespace
        set d [read-pair $c]
    }
    skip-whitespace
    return [Cons create Mem[incr ::M] $a $d]
}

proc ::constcl::read-v {} {
    # take an input token, return a value (integer or cons cell)
    skip-whitespace
    if {[string is alpha [first]]} {
        # return readOpcode();
    } elseif {[first] eq "-" || [string is digit [first]]} {
        # return readNumber();
    } elseif {[first] eq "("} {
        advance
        skip-whitespace
        set p [read-pair ")"]
        skip-whitespace
        if {[first] != ")"} {
            error "Missing right parenthesis."
        }
        return $p
    } elseif {[first] eq "\["} {
        # same as above, but with [] instead of ()
        advance
        skip-whitespace
        set p [read-pair "\]"]
        skip-whitespace
        if {[first] != "\]"} {
            error "Missing right parenthesis."
        }
        return $p
    }
    return 0
}




proc ::constcl::idcheckinit {init} {
    if {[::string is alpha $init] || $init in {! $ % & * / : < = > ? ^ _ ~}} {
        return true
    } else {
        return false
    }
}

proc ::constcl::idchecksubs {subs} {
    foreach c [split $subs {}] {
        if {!([::string is alnum $c] || $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
            return false
        }
    }
    return true
}

proc ::constcl::idcheck {sym} {
    if {(![idcheckinit [::string index $sym 0]] ||
        ![idchecksubs [::string range $sym 1 end]]) && $sym ni {+ - ...}} {
        error "Identifier expected"
    } else {
        if {$sym in {else => define unquote unquote-splicing quote lambda if set! begin
            cond and or case let let* letrec do delay quasiquote}} {
            error "Macro name can't be used as a variable: $sym"
        }
    }
    set sym
}


proc ::constcl::write-pair {obj} {
    # take an object and print the car and the cdr of the stored value
    set a [car $obj]
    set d [cdr $obj]
    # print car
    write $a
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d;
    } elseif {$d eq "#NIL"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write $d
    }
}

proc ::constcl::write {obj args} {
    # take an object and print the value
    if {[number? $obj] eq "#t"} {
        puts -nonewline [$obj value]
    } elseif {[boolean? [interp alias {} $obj]] eq "#t"} {
        if {$obj eq "#t"} {
            puts -nonewline "#t"
        } else {
            puts -nonewline "#f"
        }
    } elseif {[symbol? $obj] eq "#t"} {
        puts -nonewline [$obj name]
    } elseif {[pair? $obj] eq "#t"} {
        # it is a cons pair
        puts -nonewline "("
        write-pair $obj
        puts -nonewline ")"
    }
}



