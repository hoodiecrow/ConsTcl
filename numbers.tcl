
MD(
### Numbers

I have only implemented a bare-bones version of Scheme's numerical
library. The following is a reasonably complete framework for operations
on integers and floating-point numbers. No rationals, no complex numbers,
no gcd or lcm.
MD)

CB
oo::class create ::constcl::Number {
    superclass ::constcl::NIL
    variable value
    constructor {v} {
        ::if {[::string is double -strict $v]} {
            set value $v
        } else {
            ::error "NUMBER expected\n$v"
        }
    }
    method zero? {} {::if {$value == 0} then {return #t} else {return #f}}
    method positive? {} {::if {$value > 0} then {return #t} else {return #f}}
    method negative? {} {::if {$value < 0} then {return #t} else {return #f}}
    method even? {} {::if {$value % 2 == 0} then {return #t} else {return #f}}
    method odd? {} {::if {$value % 2 == 1} then {return #t} else {return #f}}
    method value {} { set value }
    method numval {} {set value}
    method mkconstant {} {}
    method constant {} {return 1}
    method write {} { puts -nonewline [my value] }
    method show {} { set value }
}

interp alias {} ::constcl::MkNumber {} ::constcl::Number new

CB

MD(
**number?**

`number?` recognizes a number by object type, not by content.
MD)

PR(
number? (public);val val -> bool
PR)

CB
reg number? ::constcl::number?

proc ::constcl::number? {val} {
    ::if {[info object isa typeof $val ::constcl::Number]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Number]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.0 {try number?} -body {
    pep "(number? 99.99)"
} -output "#t\n"

::tcltest::test number-1.1 {try number?} -body {
    ::constcl::MkNumber foo
} -returnCodes error -result "NUMBER expected\nfoo"

TT)

MD(
**=**

**<**

**>**

**<=**

**>=**

The predicates `=`, `<`, `>`, `<=`, and `>=` are implemented.

MD)

PR(
=, <, >, <=, >= (public);args nums -> bool
PR)

CB
reg = ::constcl::=

proc ::constcl::= {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(= num ...)"
    }
    ::if {[::tcl::mathop::== {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.2 {try =} -body {
        pep "(= 9 9 9 9)"
} -output "#t\n"

TT)

CB
reg < ::constcl::<

proc ::constcl::< {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(< num ...)"
    }
    ::if {[::tcl::mathop::< {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.3 {try <} -body {
        pep "(< 1 2 4 7)"
} -output "#t\n"

TT)

CB
reg > ::constcl::>

proc ::constcl::> {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(> num ...)"
    }
    ::if {[::tcl::mathop::> {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.4 {try >} -body {
        pep "(> 7 4 2 1)"
} -output "#t\n"

TT)

CB
reg <= ::constcl::<=

proc ::constcl::<= {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(<= num ...)"
    }
    ::if {[::tcl::mathop::<= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.5 {try <=} -body {
        pep "(<= 1 4 4 7)"
} -output "#t\n"

TT)

CB
reg >= ::constcl::>=

proc ::constcl::>= {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(>= num ...)"
    }
    ::if {[::tcl::mathop::>= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test number-1.6 {try >=} -body {
        pep "(>= 7 4 4 1)"
} -output "#t\n"

TT)

MD(
**zero?**

The `zero?` predicate tests if a given number is equal to zero.
MD)

PR(
zero? (public);num num -> bool
PR)

CB
reg zero? ::constcl::zero?

proc ::constcl::zero? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num zero?]
}
CB

TT(

::tcltest::test number-1.7 {try zero?} -body {
        pep "(zero? 77)"
} -output "#f\n"

TT)

MD(
**positive?**

**negative?**

**even?**

**odd?**

The `positive?`/`negative?`/`even?`/`odd?` predicates test a number
for those traits.
MD)

PR(
positive?, negative?, even?, odd? (public);num num -> bool
PR)

CB
reg positive? ::constcl::positive?

proc ::constcl::positive? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num positive?]
}
CB

TT(

::tcltest::test number-1.8 {try positive?} -body {
        pep "(positive? 77)"
} -output "#t\n"

TT)

CB
reg negative? ::constcl::negative?

proc ::constcl::negative? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num negative?]
}
CB

TT(

::tcltest::test number-1.9 {try negative?} -body {
        pep "(negative? 77)"
} -output "#f\n"

TT)

CB
reg even? ::constcl::even?

proc ::constcl::even? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num even?]
}
CB

TT(

::tcltest::test number-1.10 {try even?} -body {
        pep "(even? 77)"
} -output "#f\n"

TT)

CB
reg odd? ::constcl::odd?

proc ::constcl::odd? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num odd?]
}
CB

TT(

::tcltest::test number-1.11 {try odd?} -body {
        pep "(odd? 77)"
} -output "#t\n"

TT)

MD(
**max**

**min**

The `max` function selects the largest number, and the `min` function
selects the smallest number.
MD)

PR(
max, min (public);num num args nums -> num
PR)

MD(
Example:

```
(max 7 1 10 3)   ⇒  10
(min 7 1 10 3)   ⇒  1
```
MD)

CB
reg max ::constcl::max

proc ::constcl::max {num args} {
    try {
        set vals [lmap arg [::list $num {*}$args] {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(max num...)"
    }
    MkNumber [::tcl::mathfunc::max {*}$vals]
}
CB

TT(

::tcltest::test number-1.12 {try max} -body {
    pep "(max 7 1 10 3)"
} -output "10\n"

TT)

CB
reg min ::constcl::min

proc ::constcl::min {num args} {
    try {
        set vals [lmap arg [::list $num {*}$args] {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(min num...)"
    }
    MkNumber [::tcl::mathfunc::min {*}$vals]
}
CB

TT(

::tcltest::test number-1.13 {try min} -body {
        pep "(min 7 1 10 3)"
} -output "1\n"

TT)

MD(
**+**

__*__

**-**

**/**

The operators `+`, `*`, `-`, and `/` stand for the respective
mathematical operations. They take a number of operands, but
at least one for `-` and `/`.
MD)

PR(
+, * (public);args nums -> num
PR)

PR(
-, / (public);num num args nums -> num
PR)

MD(
Example:

```
(list [+ 2 2] [* 2 2] [- 10 6] [/ 20 5])   ⇒  (4 4 4 4)
(+ 21 7 3)                                 ⇒  31
(* 21 7 3)                                 ⇒  441
(- 21 7 3)                                 ⇒  11
(/ 21 7 3)                                 ⇒  1
(- 5)                                      ⇒  -5
(/ 5)                                      ⇒  0.2
```
MD)

CB
reg + ::constcl::+

proc ::constcl::+ {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(+ num ...)"
    }
    MkNumber [::tcl::mathop::+ {*}$vals]
}
CB

TT(

::tcltest::test number-1.14 {try +} -body {
    pep "(+)"
    pep "(+ 5)"
    pep "(+ 7 1 10 3)"
} -output "0\n5\n21\n"

TT)

CB
reg * ::constcl::*

proc ::constcl::* {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(* num ...)"
    }
    MkNumber [::tcl::mathop::* {*}$vals]
}
CB

TT(

::tcltest::test number-1.15 {try *} -body {
    pep "(*)"
    pep "(* 5)"
    pep "(* 7 1 10 3)"
} -output "1\n5\n210\n"

TT)

CB
reg - ::constcl::-

proc ::constcl::- {num args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(- num ...)"
    }
    MkNumber [::tcl::mathop::- [$num numval] {*}$vals]
}
CB

TT(

::tcltest::test number-1.16 {try -} -body {
    pep "(-)"
} -returnCodes error -result {wrong # args: should be "::constcl::- num ?arg ...?"}

::tcltest::test number-1.16 {try -} -body {
    pep "(- 5)"
    pep "(- 7 1 10 3)"
} -output "-5\n-7\n"

TT)

CB
reg / ::constcl::/

proc ::constcl::/ {num args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(/ num ...)"
    }
    MkNumber [::tcl::mathop::/ [$num numval] {*}$vals]
}
CB

TT(

::tcltest::test number-1.17 {try /} -body {
    pep "(/)"
} -returnCodes error -result {wrong # args: should be "::constcl::/ num ?arg ...?"}

::tcltest::test number-1.17 {try /} -body {
    pep "(/ 5)"
    pep "(/ 21 7 3)"
} -output "0.2\n1\n"

TT)

MD(
**abs**

The `abs` function yields the absolute value of a number.
MD)

PR(
abs (public);num num -> num
PR)

CB
reg abs ::constcl::abs

proc ::constcl::abs {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    ::if {[$num negative?] ne "#f"} {
        return [MkNumber [expr {[$num numval] * -1}]]
    } else {
        return $num
    }
}
CB

TT(

::tcltest::test number-1.18 {try abs} -body {
    pep "(abs -99)"
} -output "99\n"

::tcltest::test number-check-1.0 {try check} -body {
    pep "(abs \"foo\")"
} -returnCodes error -result "NUMBER expected\n(abs \"foo\")"
TT)

MD(
**quotient**

`quotient` calculates the quotient between two numbers.
MD)

PR(
quotient (public);num1 num num2 num -> num
PR)

MD(
Example:

```
(quotient 7 3)   ⇒  2.0
```
MD)

CB
reg quotient

proc ::constcl::quotient {num1 num2} {
    set q [::tcl::mathop::/ [$num1 numval] [$num2 numval]]
    ::if {$q > 0} {
        return [MkNumber [::tcl::mathfunc::floor $q]]
    } elseif {$q < 0} {
        return [MkNumber [::tcl::mathfunc::ceil $q]]
    } else {
        return #0
    }
}
CB

MD(
**remainder**

`remainder` is a variant of the modulus function. (I'm a programmer, not
a mathematician!)
MD)

PR(
remainder (public);num1 num num2 num -> num
PR)

MD(
Example:

```
(remainder 7 3)   ⇒  1
```
MD)

CB
reg remainder

proc ::constcl::remainder {num1 num2} {
    set n [::tcl::mathop::% [[abs $num1] numval] [[abs $num2] numval]]
    ::if {[$num1 negative?] ne "#f"} {
        set n -$n
    }
    return [MkNumber $n]
}
CB

MD(
**modulo**
MD)

PR(
modulo (public);num1 num num2 num -> num
PR)

MD(
Example:

```
(modulo 7 3)   ⇒  1
```
MD)

CB
reg modulo

proc ::constcl::modulo {num1 num2} {
    return [MkNumber [::tcl::mathop::% [$num1 numval] [$num2 numval]]]
}
CB

TT(

::tcltest::test number-1.19 {try quotient, remainder, modulo} -body {
    pep "(quotient 13 4)"
    pep "(modulo 13 4)"
    pep "(remainder 13 4)"
    pep "(modulo -13 4)"
    pep "(remainder -13 4)"
    pep "(modulo 13 -4)"
    pep "(remainder 13 -4)"
    pep "(modulo -13 -4)"
    pep "(remainder -13 -4)"
} -output "3.0\n1\n1\n3\n-1\n-3\n1\n-1\n-1\n"

TT)

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

MD(
**floor**

**ceiling**

**truncate**

**round**

`floor`, `ceiling`, `truncate`, and `round` are different methods for
converting a real number to an integer.
MD)

PR(
floor, ceiling, truncate, round (public);num num -> num
PR)

MD(
Example:

```
(floor 7.5)      ⇒  7.0
(ceiling 7.5)    ⇒  8.0
(truncate 7.5)   ⇒  7.0
(round 7.5)      ⇒  8
```
MD)

CB
reg floor ::constcl::floor

proc ::constcl::floor {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::floor [$num numval]]
}
CB

TT(

::tcltest::test number-1.20 {try floor} -body {
    pep "(floor 99.9)"
} -output "99.0\n"

TT)

CB
reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::ceil [$num numval]]
}
CB

TT(

::tcltest::test number-1.21 {try ceiling} -body {
    pep "(ceiling 99.9)"
} -output "100.0\n"

TT)

CB
reg truncate ::constcl::truncate

proc ::constcl::truncate {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    ::if {[$num negative?] ne "#f"} {
        MkNumber [::tcl::mathfunc::ceil [$num numval]]
    } else {
        MkNumber [::tcl::mathfunc::floor [$num numval]]
    }
}
CB

TT(

::tcltest::test number-1.22 {try truncate} -body {
    pep "(truncate 99.9)"
    pep "(truncate -99.9)"
} -output "99.0\n-99.0\n"

TT)

CB
reg round ::constcl::round

proc ::constcl::round {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::round [$num numval]]
}
CB

TT(

::tcltest::test number-1.23 {try round} -body {
    pep "(round 99.9)"
    pep "(round 99.3)"
} -output "100\n99\n"

::tcltest::test number-1.24 {try various} -body {
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

MD(
**exp**

**log**

**sin**

**cos**

**tan**

**asin**

**acos**

**atan**

The mathematical functions _e<sup>x</sup>_, natural logarithm,
sine, cosine, tangent, arcsine, arccosine, and arctangent are
calculated by `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`,
and `atan`, respectively.
MD)

PR(
exp, log, sin, cos, tan, asin, acos, atan (public);num num -> num
PR)

PR(
(binary) atan (public);num1 num num2 num -> num
PR)

MD(
Example:

```
(let ((x (log 2))) (= 2 (exp x)))                         ⇒  #t
(let ((a (/ pi 3))) (let ((s (sin a))) (= a (asin s))))   ⇒  #t
```
MD)

CB
reg exp ::constcl::exp

proc ::constcl::exp {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::exp [$num numval]]
}
CB

TT(

::tcltest::test number-1.25 {try exp} -body {
    pep "(exp 3)"
} -output "20.085536923187668\n"

TT)

CB
reg log ::constcl::log

proc ::constcl::log {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::log [$num numval]]
}
CB

TT(

::tcltest::test number-1.26 {try log} -body {
    pep "(log 3)"
} -output "1.0986122886681098\n"

TT)

CB
reg sin ::constcl::sin

proc ::constcl::sin {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::sin [$num numval]]
}
CB

CB
reg cos ::constcl::cos

proc ::constcl::cos {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::cos [$num numval]]
}
CB

CB
reg tan ::constcl::tan

proc ::constcl::tan {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::tan [$num numval]]
}
CB

TT(

::tcltest::test number-1.27 {try trig} -body {
    pep "(sin (/ pi 3))"
    pep "(cos (/ pi 3))"
    pep "(tan (/ pi 3))"
} -output "0.8660254037844386\n0.5000000000000001\n1.7320508075688767\n"

::tcltest::test number-check-1.0 {try triggering tan} -body {
    pep "(tan #\\A)"
} -returnCodes error -result "NUMBER expected\n(tan #\\A)"
TT)

CB
reg asin ::constcl::asin

proc ::constcl::asin {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::asin [$num numval]]
}
CB

CB
reg acos ::constcl::acos

proc ::constcl::acos {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::acos [$num numval]]
}
CB

CB
reg atan ::constcl::atan

proc ::constcl::atan {args} {
    ::if {[llength $args] == 1} {
        set num [lindex $args 0]
        check {number? $num} {NUMBER expected\n([pn] [$num show])}
        MkNumber [::tcl::mathfunc::atan [$num numval]]
    } else {
        lassign $args num1 num2
        check {number? $num1} {NUMBER expected\n([pn] [$num1 show])}
        check {number? $num2} {NUMBER expected\n([pn] [$num2 show])}
        MkNumber [::tcl::mathfunc::atan2 [$num1 numval] [$num2 numval]]
    }
}
CB

TT(

::tcltest::test number-1.28 {try trig} -body {
    pep "(asin 0.3)"
    pep "(acos 0.3)"
    pep "(atan 0.3)"
} -output "0.3046926540153975\n1.2661036727794992\n0.2914567944778671\n"

TT)

MD(
**sqrt**

`sqrt` calculates the square root.
MD)

PR(
sqrt (public);num num -> num
PR)

CB
reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::sqrt [$num numval]]
}
CB

TT(

::tcltest::test number-1.29 {try sqrt} -body {
    pep "(sqrt 16)"
} -output "4.0\n"

TT)

MD(
**expt**

`expt` calculates the _x_ to the power of _y_, or _x<sup>y</sup>_.
MD)

PR(
expt (public);num1 num num2 num -> num
PR)

CB
reg expt ::constcl::expt

proc ::constcl::expt {num1 num2} {
    check {number? $num1} {NUMBER expected\n([pn] [$num1 show] [$num2 show])}
    check {number? $num2} {NUMBER expected\n([pn] [$num1 show] [$num2 show])}
    MkNumber [::tcl::mathfunc::pow [$num1 numval] [$num2 numval]]
}
CB

TT(

::tcltest::test number-1.30 {try expt} -body {
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

MD(
**number->string**

The procedures `number->string` and `string->number` convert between
number and string with optional radix conversion.
MD)

PR(
number->string (public);num num ?radix? num -> str
PR)

MD(
Example:

```
(number->string 23)      ⇒  "23"
(number->string 23 2)    ⇒  "10111"
(number->string 23 8)    ⇒  "27"
(number->string 23 16)   ⇒  "17"
```
MD)

CB
reg number->string ::constcl::number->string

proc ::constcl::number->string {num args} {
    ::if {[llength $args] == 0} {
        check {number? $num} {NUMBER expected\n([pn] [$num show])}
        return [MkString [$num numval]]
    } else {
        lassign $args radix
        check {number? $num} {NUMBER expected\n([pn] [$num show])}
        check {number? $radix} {NUMBER expected\n([pn] [$num show] [$radix show])}
        check {memv $radix [list [MkNumber 2] [MkNumber 8] [MkNumber 10] [MkNumber 16]]} {Radix not in 2, 8, 10, 16\n([pn] [$num show] [$radix show])}
        ::if {[$radix numval] == 10} {
            return [MkString [$num numval]]
        } else {
            return [MkString [base [$radix numval] [$num numval]]]
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
    ::if $negative {set res -$res}
    set res
}
CB

TT(

::tcltest::test number-1.31 {try number->string} -body {
    pep "(number->string 23)"
    pep "(number->string 23 2)"
    pep "(number->string 23 8)"
    pep "(number->string 23 16)"
} -output "\"23\"\n\"10111\"\n\"27\"\n\"17\"\n"

::tcltest::test number-1.32 {try number->string} -body {
    pep "(number->string 23 13)"
} -returnCodes error -result "Radix not in 2, 8, 10, 16\n(number->string 23 13)"

TT)

MD(
**string->number**

As with `number->string`, above.
MD)

PR(
string->number (public);str str ?radix? num -> num
PR)

MD(
Example:

```
(string->number "23")        ⇒  23
(string->number "10111" 2)   ⇒  23
(string->number "27" 8)      ⇒  23
(string->number "17" 16)     ⇒  23
```
MD)

CB
reg string->number ::constcl::string->number

proc ::constcl::string->number {str args} {
    ::if {[llength $args] == 0} {
        check {string? $str} {STRING expected\n([pn] [$str show])}
        return [MkNumber [$str value]]
    } else {
        lassign $args radix
        check {string? $str} {STRING expected\n([pn] [$str show])}
        check {memv $radix [list [MkNumber 2] [MkNumber 8] [MkNumber 10] [MkNumber 16]]} {Radix not in 2, 8, 10, 16\n([pn] [$str show] [$radix show])}
        ::if {[$radix numval] == 10} {
            return [MkNumber [$str value]]
        } else {
            return [MkNumber [frombase [$radix numval] [$str value]]]
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
        ::if {$decimalvalue < 0 || $decimalvalue >= $base} {
            ::error "bad digit $decimalvalue for base $base"
        }
        set res [expr {$res * $base + $decimalvalue}]
    }
    ::if $negative {set res -$res}
    set res
}
CB

TT(

::tcltest::test number-1.33 {try string->number} -body {
    pep {(string->number "23")}
    pep {(string->number "10111" 2)}
    pep {(string->number "27" 8)}
    pep {(string->number "17" 16)}
} -output "23\n23\n23\n23\n"

::tcltest::test number-1.34 {try string->number} -body {
    pep {(string->number "23" 13)}
} -returnCodes error -result "Radix not in 2, 8, 10, 16\n(string->number \"23\" 13)"

TT)

# vim: ft=tcl tw=80
