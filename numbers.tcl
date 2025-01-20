
MD(
## Numbers
MD)

CB
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
    method write {} { puts -nonewline [my value] }
}
CB

CB
proc ::constcl::number? {obj} {
    if {[info object isa typeof $obj Number]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Number]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
proc ::constcl::= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(= num...)"
    }
    ::tcl::mathop::== {*}$vals
}
CB

CB
proc ::constcl::< {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(< num...)"
    }
    ::tcl::mathop::< {*}$vals
}
CB

CB
proc ::constcl::> {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(> num...)"
    }
    ::tcl::mathop::> {*}$vals
}
CB

CB
proc ::constcl::<= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(<= num...)"
    }
    ::tcl::mathop::<= {*}$vals
}
CB

CB
proc ::constcl::>= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(>= num...)"
    }
    ::tcl::mathop::>= {*}$vals
}
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
proc ::constcl::max {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(max num...)"
    }
    ::tcl::mathop::max {*}$vals
}
CB

CB
proc ::constcl::min {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(min num...)"
    }
    ::tcl::mathop::min {*}$vals
}
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
proc ::constcl::quotient {n1 n2} {
    # TODO
}
CB

CB
proc ::constcl::remainder {n1 n2} {
    # TODO
}
CB

CB
proc ::constcl::modulo {n1 n2} {
    # TODO
}
CB

CB
proc ::constcl::gcd {args} {
    # TODO
}
CB

CB
proc ::constcl::lcm {args} {
    # TODO
}
CB

CB
proc ::constcl::numerator {q} {
    # TODO
}
CB

CB
proc ::constcl::denominator {q} {
    # TODO
}
CB

CB
proc ::constcl::floor {x} {
    if {[::constcl::number? $x] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::floor [$x value]]
    } else {
        error "NUMBER expected\n(floor [$x write])"
    }
}
CB

CB
proc ::constcl::ceiling {x} {
    if {[::constcl::number? $x] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::ceil [$x value]]
    } else {
        error "NUMBER expected\n(ceiling [$x write])"
    }
}
CB

CB
proc ::constcl::truncate {x} {
    if {[::constcl::number? $x] eq #t} {
        # TODO
    } else {
        error "NUMBER expected\n(truncate [$x write])"
    }
}
CB

CB
proc ::constcl::round {x} {
    if {[::constcl::number? $x] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::round [$x value]]
    } else {
        error "NUMBER expected\n(round [$x write])"
    }
}
CB

CB
proc ::constcl::rationalize {x y} {
    # TODO
}
CB

CB
proc ::constcl::exp {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::exp [$z value]]
    } else {
        error "NUMBER expected\n(exp [$z write])"
    }
}
CB

CB
proc ::constcl::log {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::log [$z value]]
    } else {
        error "NUMBER expected\n(log [$z write])"
    }
}
CB

CB
proc ::constcl::sin {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::sin [$z value]]
    } else {
        error "NUMBER expected\n(sin [$z write])"
    }
}
CB

CB
proc ::constcl::cos {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::cos [$z value]]
    } else {
        error "NUMBER expected\n(cos [$z write])"
    }
}
CB

CB
proc ::constcl::tan {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::tan [$z value]]
    } else {
        error "NUMBER expected\n(tan [$z write])"
    }
}
CB

CB
proc ::constcl::asin {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::asin [$z value]]
    } else {
        error "NUMBER expected\n(asin [$z write])"
    }
}
CB

CB
proc ::constcl::acos {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::acos [$z value]]
    } else {
        error "NUMBER expected\n(acos [$z write])"
    }
}
CB

CB
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
CB

CB
proc ::constcl::sqrt {z} {
    if {[::constcl::number? $z] eq #t} {
        Number create Mem[incr ::M] [::tcl::mathfunc::sqrt [$z value]]
    } else {
        error "NUMBER expected\n(sqrt [$z write])"
    }
}
CB

CB
proc ::constcl::expt {z1 z2} {
    if {[::constcl::number? $z1] eq #t && [::constcl::number $z2]} {
        Number create Mem[incr ::M] [::tcl::mathfunc::pow [$z1 value] [$z2 value]]
    } else {
        error "NUMBER expected\n(expt [$z1 write] [$z2 write])"
    }
}
CB

CB
proc ::constcl::make-rectangular {x1 x2} {
    # TODO
}
CB

CB
proc ::constcl::make-polar {x3 x4} {
    # TODO
}
CB

CB
proc ::constcl::real-part {z} {
    # TODO
}
CB

CB
proc ::constcl::imag-part {z} {
    # TODO
}
CB

CB
proc ::constcl::magnitude {z} {
    # TODO
}
CB

CB
proc ::constcl::angle {z} {
    # TODO
}
CB

CB
proc ::constcl::exact->inexact {z} {
    # TODO
}
CB

CB
proc ::constcl::inexact->exact {z} {
    # TODO
}
CB

CB
proc ::constcl::number->string {args} {
    # TODO
}
CB

CB
proc ::constcl::string->number {args} {
    # TODO
}
CB

