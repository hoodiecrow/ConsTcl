
MD(
### Numbers

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
    method mkconstant {} {}
    method constant {} {return 1}
    method write {} { puts -nonewline [my value] }
    method show {} { set value }
}

interp alias {} ::constcl::MkNumber {} Number new
CB

CB
reg number? ::constcl::number?

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

TT(

::tcltest::test number-1.0 {try number?} -body {
    namespace eval ::constcl {
        set ::inputstr "(number? 99.99)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg = ::constcl::=

proc ::constcl::= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(= num...)"
    }
    if {[::tcl::mathop::== {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.1 {try =} -body {
    namespace eval ::constcl {
        set ::inputstr "(= 9 9 9 9)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg < ::constcl::<

proc ::constcl::< {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(< num...)"
    }
    if {[::tcl::mathop::< {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.2 {try <} -body {
    namespace eval ::constcl {
        set ::inputstr "(< 1 2 4 7)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg > ::constcl::>

proc ::constcl::> {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(> num...)"
    }
    if {[::tcl::mathop::> {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.3 {try >} -body {
    namespace eval ::constcl {
        set ::inputstr "(> 7 4 2 1)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg <= ::constcl::<=

proc ::constcl::<= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(<= num...)"
    }
    if {[::tcl::mathop::<= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.4 {try <=} -body {
    namespace eval ::constcl {
        set ::inputstr "(<= 1 4 4 7)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg >= ::constcl::>=

proc ::constcl::>= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(>= num...)"
    }
    if {[::tcl::mathop::>= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.5 {try >=} -body {
    namespace eval ::constcl {
        set ::inputstr "(>= 7 4 4 1)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg zero? ::constcl::zero?

proc ::constcl::zero? {obj} {
    if {[number? $obj] eq "#t"} {
        if {[$obj value] == 0} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(zero? [$obj show])"
    }
}
CB

TT(

::tcltest::test number-1.6 {try zero?} -body {
    namespace eval ::constcl {
        set ::inputstr "(zero? 77)"
        write [eval [read]]
    }
} -output "#f\n"

TT)

CB
reg positive? ::constcl::positive?

proc ::constcl::positive? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj positive]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(positive? [$obj show])"
    }
}
CB

TT(

::tcltest::test number-1.7 {try positive?} -body {
    namespace eval ::constcl {
        set ::inputstr "(positive? 77)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg negative? ::constcl::negative?

proc ::constcl::negative? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj negative]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(negative? [$obj show])"
    }
}
CB

TT(

::tcltest::test number-1.8 {try negative?} -body {
    namespace eval ::constcl {
        set ::inputstr "(negative? 77)"
        write [eval [read]]
    }
} -output "#f\n"

TT)

CB
reg even? ::constcl::even?

proc ::constcl::even? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj even]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(even? [$obj show])"
    }
}
CB

TT(

::tcltest::test number-1.9 {try even?} -body {
    namespace eval ::constcl {
        set ::inputstr "(even? 77)"
        write [eval [read]]
    }
} -output "#f\n"

TT)

CB
reg odd? ::constcl::odd?

proc ::constcl::odd? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj odd]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(odd? [$obj show])"
    }
}
CB

TT(

::tcltest::test number-1.10 {try odd?} -body {
    namespace eval ::constcl {
        set ::inputstr "(odd? 77)"
        write [eval [read]]
    }
} -output "#t\n"

TT)

CB
reg max ::constcl::max

proc ::constcl::max {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(max num...)"
    }
    MkNumber [::tcl::mathfunc::max {*}$vals]
}
CB

TT(

::tcltest::test number-1.11 {try max} -body {
    namespace eval ::constcl {
        set ::inputstr "(max 7 1 10 3)"
        write [eval [read]]
    }
} -output "10\n"

TT)

CB
reg min ::constcl::min

proc ::constcl::min {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(min num...)"
    }
    MkNumber [::tcl::mathfunc::min {*}$vals]
}
CB

TT(

::tcltest::test number-1.12 {try min} -body {
    namespace eval ::constcl {
        set ::inputstr "(min 7 1 10 3)"
        write [eval [read]]
    }
} -output "1\n"

TT)

CB
reg + ::constcl::+

proc ::constcl::+ {args} {
    if {[llength $args] == 0} {
        return #0
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj] eq "#t"} {
            return $obj
        } else {
            error "NUMBER expected\n(+ [$obj show])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj] eq "#t"} {
            set num [MkNumber [$obj value]]
        } else {
            error "NUMBER expected\n(+ [$obj show])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::constcl::number? $obj] eq "#t"} {
                $num incr [$obj value]
            } else {
                error "NUMBER expected\n(+ [$obj show])"
            }
        }
        return $num
    }
}
CB

TT(

::tcltest::test number-1.12 {try +} -body {
    namespace eval ::constcl {
        set ::inputstr "(+ 7 1 10 3)"
        write [eval [read]]
    }
} -output "21\n"

TT)

CB
reg * ::constcl::*

proc ::constcl::* {args} {
    if {[llength $args] == 0} {
        return #1
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[number? $obj] eq "#t"} {
            return $obj
        } else {
            error "NUMBER expected\n(* [$obj show])"
        }
    } else {
        set obj [lindex $args 0]
        if {[number? $obj] eq "#t"} {
            set num [MkNumber [$obj value]]
        } else {
            error "NUMBER expected\n(* [$obj show])"
        }
        foreach obj [lrange $args 1 end] {
            if {[number? $obj] eq "#t"} {
                $num mult [$obj value]
            } else {
                error "NUMBER expected\n(* [$obj show])"
            }
        }
        return $num
    }
}
CB

TT(

::tcltest::test number-1.13 {try *} -body {
    namespace eval ::constcl {
        set ::inputstr "(* 7 1 10 3)"
        write [eval [read]]
    }
} -output "210\n"

TT)

CB
reg - ::constcl::-

proc ::constcl::- {args} {
    if {[llength $args] == 0} {
        error "expected arguments"
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj] eq "#t"} {
            return [MkNumber -[$obj value]]
        } else {
            error "NUMBER expected\n(- [$obj show])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj] eq "#t"} {
            set num [MkNumber [$obj value]]
        } else {
            error "NUMBER expected\n(- [$obj show])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::constcl::number? $obj] eq "#t"} {
                $num decr [$obj value]
            } else {
                error "NUMBER expected\n(- [$obj show])"
            }
        }
        return $num
    }
}
CB

TT(

::tcltest::test number-1.14 {try -} -body {
    namespace eval ::constcl {
        set ::inputstr "(- 7 1 10 3)"
        write [eval [read]]
    }
} -output "-7\n"

TT)

CB
reg / ::constcl::/

proc ::constcl::/ {args} {
    if {[llength $args] == 0} {
        error "expected arguments"
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj] eq "#t"} {
            return [MkNumber [expr {1 / [$obj value]}]]
        } else {
            error "NUMBER expected\n(/ [$obj show])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj] eq "#t"} {
            set num [MkNumber [$obj value]]
        } else {
            error "NUMBER expected\n(/ [$obj show])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::constcl::number? $obj] eq "#t"} {
                $num div [$obj value]
            } else {
                error "NUMBER expected\n(/ [$obj show])"
            }
        }
        return $num
    }
}
CB

TT(

::tcltest::test number-1.15 {try /} -body {
    namespace eval ::constcl {
        set ::inputstr "(/ 60 1 10 3)"
        write [eval [read]]
    }
} -output "2\n"

TT)

CB
reg abs ::constcl::abs

proc ::constcl::abs {x} {
    if {[::constcl::number? $x] eq "#t"} {
        if {[$x negative]} {
            return [MkNumber [expr {[$x value] * -1}]]
        } else {
            return $x
        }
    } else {
        error "NUMBER expected\n(abs [$x show])"
    }
}
CB

TT(

::tcltest::test number-1.16 {try abs} -body {
    pep "(abs -99)"
} -output "99\n"

TT)

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
reg floor ::constcl::floor

proc ::constcl::floor {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::floor [$x value]]
    } else {
        error "NUMBER expected\n(floor [$x show])"
    }
}
CB

TT(

::tcltest::test number-1.17 {try floor} -body {
    pep "(floor 99.9)"
} -output "99.0\n"

TT)

CB
reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::ceil [$x value]]
    } else {
        error "NUMBER expected\n(ceiling [$x show])"
    }
}
CB

TT(

::tcltest::test number-1.18 {try ceiling} -body {
    pep "(ceiling 99.9)"
} -output "100.0\n"

TT)

CB
reg truncate ::constcl::truncate

proc ::constcl::truncate {x} {
    if {[::constcl::number? $x] eq "#t"} {
        if {[$x negative]} {
            MkNumber [::tcl::mathfunc::ceil [$x value]]
        } else {
            MkNumber [::tcl::mathfunc::floor [$x value]]
        }
    } else {
        error "NUMBER expected\n(truncate [$x show])"
    }
}
CB

TT(

::tcltest::test number-1.19 {try truncate} -body {
    pep "(truncate 99.9)"
    pep "(truncate -99.9)"
} -output "99.0\n-99.0\n"

TT)

CB
reg round ::constcl::round

proc ::constcl::round {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::round [$x value]]
    } else {
        error "NUMBER expected\n(round [$x show])"
    }
}
CB

TT(

::tcltest::test number-1.20 {try round} -body {
    pep "(round 99.9)"
    pep "(round 99.3)"
} -output "100\n99\n"

::tcltest::test number-1.21 {try various} -body {
    pep "(floor 3.5)"
    pep "(ceiling 3.5)"
    pep "(truncate 3.5)"
    pep "(round 3.5)"
} -output "3.0\n4.0\n3.0\n4\n"

TT)

CB
proc ::constcl::rationalize {x y} {
    # TODO
}
CB

CB
reg exp ::constcl::exp

proc ::constcl::exp {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::exp [$z value]]
    } else {
        error "NUMBER expected\n(exp [$z show])"
    }
}
CB

TT(

::tcltest::test number-1.22 {try exp} -body {
    pep "(exp 3)"
} -output "20.085536923187668\n"

TT)

CB
reg log ::constcl::log

proc ::constcl::log {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::log [$z value]]
    } else {
        error "NUMBER expected\n(log [$z show])"
    }
}
CB

TT(

::tcltest::test number-1.23 {try log} -body {
    pep "(log 3)"
} -output "1.0986122886681098\n"

TT)

CB
reg sin ::constcl::sin

proc ::constcl::sin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sin [$z value]]
    } else {
        error "NUMBER expected\n(sin [$z show])"
    }
}
CB

CB
reg cos ::constcl::cos

proc ::constcl::cos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::cos [$z value]]
    } else {
        error "NUMBER expected\n(cos [$z show])"
    }
}
CB

CB
reg tan ::constcl::tan

proc ::constcl::tan {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::tan [$z value]]
    } else {
        error "NUMBER expected\n(tan [$z show])"
    }
}
CB

TT(

::tcltest::test number-1.24 {try trig} -body {
    pep "(sin (/ pi 3))"
    pep "(cos (/ pi 3))"
    pep "(tan (/ pi 3))"
} -output "0.8660254037844386\n0.5000000000000001\n1.7320508075688767\n"

TT)

CB
reg asin ::constcl::asin

proc ::constcl::asin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::asin [$z value]]
    } else {
        error "NUMBER expected\n(asin [$z show])"
    }
}
CB

CB
reg acos ::constcl::acos

proc ::constcl::acos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::acos [$z value]]
    } else {
        error "NUMBER expected\n(acos [$z show])"
    }
}
CB

CB
reg atan ::constcl::atan

proc ::constcl::atan {args} {
    if {[llength $args] == 1} {
        set z [lindex $args 0]
        if {[::constcl::number? $z] eq "#t"} {
            MkNumber [::tcl::mathfunc::atan [$z value]]
        } else {
            error "NUMBER expected\n(atan [$z show])"
        }
    } else {
        lassign $args y x
        if {[::constcl::number? $y] eq "#t" && [::constcl::number? $x] eq "#t"} {
            MkNumber [::tcl::mathfunc::atan2 [$y value] [$x value]]
        } else {
            error "NUMBER expected\n(atan [$y show] [$x show])"
        }
    }
}
CB

TT(

::tcltest::test number-1.25 {try trig} -body {
    pep "(asin 0.3)"
    pep "(acos 0.3)"
    pep "(atan 0.3)"
} -output "0.3046926540153975\n1.2661036727794992\n0.2914567944778671\n"

TT)

CB
reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sqrt [$z value]]
    } else {
        error "NUMBER expected\n(sqrt [$z show])"
    }
}
CB

TT(

::tcltest::test number-1.25 {try sqrt} -body {
    pep "(sqrt 16)"
} -output "4.0\n"

TT)

CB
reg expt ::constcl::expt

proc ::constcl::expt {z1 z2} {
    if {[::constcl::number? $z1] eq "#t" && [::constcl::number? $z2] eq "#t"} {
        MkNumber [::tcl::mathfunc::pow [$z1 value] [$z2 value]]
    } else {
        error "NUMBER expected\n(expt [$z1 show] [$z2 show])"
    }
}
CB

TT(

::tcltest::test number-1.25 {try expt} -body {
    pep "(expt 4 2)"
} -output "16.0\n"

TT)

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
reg number->string ::constcl::number->string

proc ::constcl::number->string {args} {
    if {[llength $args] == 1} {
        set num [lindex $args 0]
        if {[number? $num] eq "#t"} {
            return [MkString [$num value]]
        } else {
            error "NUMBER expected\n(string->number [$num show])"
        }
    } else {
        lassign $args num radix
        if {[number? $num] eq "#t"} {
            if {[$radix value] == 10} {
                return [MkString [$num value]]
            } elseif {[$radix value] in {2 8 16}} {
                return [MkString [base [$radix value] [$num value]]]
            } else {
                error "radix not in 2, 8, 10, 16"
            }
        } else {
            error "NUMBER expected\n(string->number [$num show])"
        }
    }
}

# due to Richard Suchenwirth, <URL: https://wiki.tcl-lang.org/page/Based+numbers>
proc base {base number} {
    set negative [regexp ^-(.+) $number -> number]
    set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
    set res {}
    while {$number} {
        set digit [expr {$number % $base}]
        set res [lindex $digits $digit]$res
        set number [expr {$number / $base}]
    }
    if $negative {set res -$res}
    set res
}
CB

TT(

::tcltest::test number-1.26 {try number->string} -body {
    pep "(number->string 23)"
    pep "(number->string 23 2)"
    pep "(number->string 23 8)"
    pep "(number->string 23 16)"
} -output "\"23\"\n\"10111\"\n\"27\"\n\"17\"\n"

TT)

CB
reg string->number ::constcl::string->number

proc ::constcl::string->number {args} {
    if {[llength $args] == 1} {
        set str [lindex $args 0]
        if {[string? $str] eq "#t"} {
            return [MkNumber [$str value]]
        } else {
            error "STRING expected\n(string->number [$str show])"
        }
    } else {
        lassign $args str radix
        if {[string? $str] eq "#t"} {
            if {[$radix value] == 10} {
                return [MkNumber [$str value]]
            } elseif {[$radix value] in {2 8 16}} {
                return [MkNumber [frombase [$radix value] [$str value]]]
            } else {
                error "radix not in 2, 8, 10, 16"
            }
        } else {
            error "STRING expected\n(string->number [$str show])"
        }
    }
}

# due to Richard Suchenwirth, <URL: https://wiki.tcl-lang.org/page/Based+numbers>
proc frombase {base number} {
    set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
    set negative [regexp ^-(.+) $number -> number]
    set res 0
    foreach digit [split $number {}] {
        set decimalvalue [lsearch $digits $digit]
        if {$decimalvalue < 0 || $decimalvalue >= $base} {
            error "bad digit $decimalvalue for base $base"
        }
        set res [expr {$res * $base + $decimalvalue}]
    }
    if $negative {set res -$res}
    set res
}
CB

TT(

::tcltest::test number-1.27 {try string->number} -body {
    pep {(string->number "23")}
    pep {(string->number "10111" 2)}
    pep {(string->number "27" 8)}
    pep {(string->number "17" 16)}
} -output "23\n23\n23\n23\n"

TT)

