
MD(
### Numbers

I have only implemented a bare-bones version of Scheme's numerical
library. The following is a reasonably complete framework for operations
on integers and floating-point numbers. No rationals, no complex numbers,
no gcd or lcm.

__Number__ class
MD)

CB
oo::class create ::constcl::Number {
  superclass ::constcl::NIL
  variable value
  constructor {v} {
    if {[::string is double -strict $v]} {
      set value $v
    } else {
      ::error "NUMBER expected\n$v"
    }
  }
  method zero? {} {
    if {$value == 0} {return #t} {return #f}
  }
  method positive? {} {
    if {$value > 0} {return #t} {return #f}
  }
  method negative? {} {
    if {$value < 0} {return #t} {return #f}
  }
  method even? {} {
    if {$value % 2 == 0} {return #t} {return #f}
  }
  method odd? {} {
    if {$value % 2 == 1} {return #t} {return #f}
  }
  method value {} {
    set value
  }
  method numval {} {
    set value
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method write {handle} {
    puts -nonewline $handle [my value]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set value
  }
}

interp alias {} ::constcl::MkNumber \
  {} ::constcl::Number new
interp alias {} N {} ::constcl::Number new
CB

MD(
__number?__

`number?` recognizes a number by object type, not by content.
MD)

PR(
number? (public);val val -> bool
PR)

CB
reg number?

proc ::constcl::number? {val} {
  return [typeof? $val Number]
}
CB

TT(

::tcltest::test numbers-1.0 {try number?} -body {
    pew "(number? 99.99)"
} -output "#t\n"

::tcltest::test numbers-1.1 {try number?} -body {
    N foo
} -returnCodes error -result "NUMBER expected\nfoo"

::tcltest::test numbers-1.2 {try number?} -body {
    N 4294967295
} -match glob -result "::oo::Obj*"

TT)

MD(
__=__

__<__

__>__

__<=__

__>=__

The predicates `=`, `<`, `>`, `<=`, and `>=` are implemented.

MD)

PR(
=, <, >, <=, >= (public);args nums -> bool
PR)

CB
reg =

proc ::constcl::= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(= num ...)"
  }
  if {[::tcl::mathop::== {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test numbers-2.0 {try =} -body {
        pew "(= 9 9 9 9)"
} -output "#t\n"

::tcltest::test numbers-2.1 {try =} -body {
        pew "(= 9 9 9 9.0)"
} -output "#t\n"

TT)

CB
reg <

proc ::constcl::< {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(< num ...)"
  }
  if {[::tcl::mathop::< {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test numbers-3.0 {try <} -body {
        pew "(< 1 2 4 7)"
} -output "#t\n"

TT)

CB
reg >

proc ::constcl::> {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(> num ...)"
  }
  if {[::tcl::mathop::> {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test numbers-4.0 {try >} -body {
        pew "(> 7 4 2 1)"
} -output "#t\n"

TT)

CB
reg <=

proc ::constcl::<= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(<= num ...)"
  }
  if {[::tcl::mathop::<= {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test numbers-5.0 {try <=} -body {
        pew "(<= 1 4 4 7)"
} -output "#t\n"

TT)

CB
reg >=

proc ::constcl::>= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(>= num ...)"
  }
  if {[::tcl::mathop::>= {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test numbers-6.0 {try >=} -body {
        pew "(>= 7 4 4 1)"
} -output "#t\n"

TT)

MD(
__zero?__

The `zero?` predicate tests if a given number is equal to zero.
MD)

PR(
zero? (public);num num -> bool
PR)

CB
reg zero?

proc ::constcl::zero? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num zero?]
}
CB

TT(

::tcltest::test numbers-7.0 {try zero?} -body {
        pew "(zero? 77)"
} -output "#f\n"

::tcltest::test numbers-7.1 {check zero?} -body {
        pew "(zero? \"foo\")"
} -returnCodes error -result "NUMBER expected\n(zero? \"foo\")"

TT)

MD(
__positive?__

__negative?__

__even?__

__odd?__

The `positive?`/`negative?`/`even?`/`odd?` predicates test a number
for those traits.
MD)

PR(
positive?, negative?, even?, odd? (public);num num -> bool
PR)

CB
reg positive?

proc ::constcl::positive? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num positive?]
}
CB

TT(

::tcltest::test numbers-8.0 {try positive?} -body {
        pew "(positive? 77)"
} -output "#t\n"

TT)

CB
reg negative?

proc ::constcl::negative? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num negative?]
}
CB

TT(

::tcltest::test numbers-9.0 {try negative?} -body {
        pew "(negative? 77)"
} -output "#f\n"

TT)

CB
reg even?

proc ::constcl::even? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num even?]
}
CB

TT(

::tcltest::test numbers-10.0 {try even?} -body {
        pew "(even? 77)"
} -output "#f\n"

TT)

CB
reg odd?

proc ::constcl::odd? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num odd?]
}
CB

TT(

::tcltest::test numbers-11.0 {try odd?} -body {
        pew "(odd? 77)"
} -output "#t\n"

TT)

MD(
__max__

__min__

The `max` function selects the largest number, and the `min` function
selects the smallest number.
MD)

PR(
max, min (public);num num args nums -> num
PR)

MD(
Example:

```
(max 7 1 10 3)   =>  10
(min 7 1 10 3)   =>  1
```
MD)

CB
reg max

proc ::constcl::max {num args} {
  lappend args $num
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(max num...)"
  }
  MkNumber [::tcl::mathfunc::max {*}$vals]
}
CB

TT(

::tcltest::test numbers-12.0 {try max} -body {
    pew "(max 7 1 10 3)"
} -output "10\n"

TT)

CB
reg min

proc ::constcl::min {num args} {
  lappend args $num
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(min num...)"
  }
  MkNumber [::tcl::mathfunc::min {*}$vals]
}
CB

TT(

::tcltest::test numbers-13.0 {try min} -body {
        pew "(min 7 1 10 3)"
} -output "1\n"

TT)

MD(
__+__

__*__

__-__

__/__

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
(list (+ 2 2) (* 2 2)
  (- 10 6) (/ 20 5))   =>  (4 4 4 4)
(+ 21 7 3)             =>  31
(* 21 7 3)             =>  441
(- 21 7 3)             =>  11
(/ 21 7 3)             =>  1
(- 5)                  =>  -5
(/ 5)                  =>  0.2
```
MD)

CB
reg +

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

::tcltest::test numbers-14.0 {try +} -body {
    pew "(+)"
    pew "(+ 5)"
    pew "(+ 7 1 10 3)"
} -output "0\n5\n21\n"

TT)

CB
reg *

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

::tcltest::test numbers-15.0 {try *} -body {
    pew "(*)"
    pew "(* 5)"
    pew "(* 7 1 10 3)"
} -output "1\n5\n210\n"

TT)

CB
reg -

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

::tcltest::test numbers-16.0 {try -} -body {
    pew "(-)"
} -returnCodes error -result {wrong # args: should be "::constcl::- num ?arg ...?"}

::tcltest::test numbers-16.1 {try -} -body {
    pew "(- 5)"
    pew "(- 7 1 10 3)"
} -output "-5\n-7\n"

TT)

CB
reg /

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

::tcltest::test numbers-17.0 {try /} -body {
    pew "(/)"
} -returnCodes error -result {wrong # args: should be "::constcl::/ num ?arg ...?"}

::tcltest::test numbers-17.1 {try /} -body {
    pew "(/ 5)"
    pew "(/ 21 7 3)"
} -output "0.2\n1\n"

TT)

MD(
__abs__

The `abs` function yields the absolute value of a number.
MD)

PR(
abs (public);num num -> num
PR)

CB
reg abs

proc ::constcl::abs {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    return [MkNumber [expr {[$num numval] * -1}]]
  } else {
    return $num
  }
}
CB

TT(

::tcltest::test numbers-18.0 {try abs} -body {
    pew "(abs -99)"
} -output "99\n"

::tcltest::test numbers-18.1 {try check} -body {
    pew "(abs \"foo\")"
} -returnCodes error -result "NUMBER expected\n(abs \"foo\")"
TT)

MD(
__quotient__

`quotient` calculates the quotient between two numbers.
MD)

PR(
quotient (public);num1 num num2 num -> num
PR)

MD(
Example:

```
(quotient 7 3)   =>  2.0
```
MD)

CB
reg quotient

proc ::constcl::quotient {num1 num2} {
  set q [::tcl::mathop::/ [$num1 numval] \
    [$num2 numval]]
  if {$q > 0} {
    return [MkNumber [::tcl::mathfunc::floor $q]]
  } elseif {$q < 0} {
    return [MkNumber [::tcl::mathfunc::ceil $q]]
  } else {
    return #0
  }
}
CB

MD(
__remainder__

`remainder` is a variant of the modulus function. (I'm a programmer, not
a mathematician!)
MD)

PR(
remainder (public);num1 num num2 num -> num
PR)

MD(
Example:

```
(remainder 7 3)   =>  1
```
MD)

CB
reg remainder

proc ::constcl::remainder {num1 num2} {
  set n [::tcl::mathop::% [[abs $num1] numval] \
    [[abs $num2] numval]]
  if {[$num1 negative?] ne "#f"} {
    set n -$n
  }
  return [MkNumber $n]
}
CB

MD(
__modulo__
MD)

PR(
modulo (public);num1 num num2 num -> num
PR)

MD(
Example:

```
(modulo 7 3)   =>  1
```
MD)

CB
reg modulo

proc ::constcl::modulo {num1 num2} {
  return [MkNumber [::tcl::mathop::% [$num1 numval] \
    [$num2 numval]]]
}
CB

TT(

::tcltest::test numbers-19.0 {try quotient, remainder, modulo} -body {
    pew "(quotient 13 4)"
    pew "(modulo 13 4)"
    pew "(remainder 13 4)"
    pew "(modulo -13 4)"
    pew "(remainder -13 4)"
    pew "(modulo 13 -4)"
    pew "(remainder 13 -4)"
    pew "(modulo -13 -4)"
    pew "(remainder -13 -4)"
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
__floor__

__ceiling__

__truncate__

__round__

`floor`, `ceiling`, `truncate`, and `round` are different methods for
converting a real number to an integer.
MD)

PR(
floor, ceiling, truncate, round (public);num num -> num
PR)

MD(
Example:

```
(floor 7.5)      =>  7.0
(ceiling 7.5)    =>  8.0
(truncate 7.5)   =>  7.0
(round 7.5)      =>  8
```
MD)

CB
reg floor

proc ::constcl::floor {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::floor [$num numval]]
}
CB

TT(

::tcltest::test numbers-20.0 {try floor} -body {
    pew "(floor 99.9)"
} -output "99.0\n"

TT)

CB
reg ceiling

proc ::constcl::ceiling {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::ceil [$num numval]]
}
CB

TT(

::tcltest::test numbers-21.0 {try ceiling} -body {
    pew "(ceiling 99.9)"
} -output "100.0\n"

TT)

CB
reg truncate

proc ::constcl::truncate {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    MkNumber [::tcl::mathfunc::ceil [$num numval]]
  } else {
    MkNumber [::tcl::mathfunc::floor [$num numval]]
  }
}
CB

TT(

::tcltest::test numbers-22.0 {try truncate} -body {
    pew "(truncate 99.9)"
    pew "(truncate -99.9)"
} -output "99.0\n-99.0\n"

TT)

CB
reg round

proc ::constcl::round {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::round [$num numval]]
}
CB

TT(

::tcltest::test numbers-23.0 {try round} -body {
    pew "(round 99.9)"
    pew "(round 99.3)"
} -output "100\n99\n"

::tcltest::test numbers-23.1 {try various} -body {
    pew "(floor 3.5)"
    pew "(ceiling 3.5)"
    pew "(truncate 3.5)"
    pew "(round 3.5)"
} -output "3.0\n4.0\n3.0\n4\n"

TT)

CB
proc ::constcl::rationalize {x y} {
    # TODO
}
CB

MD(
__exp__

__log__

__sin__

__cos__

__tan__

__asin__

__acos__

__atan__

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
(let ((x (log 2))) (= 2 (exp x)))        =>  #t
(letrec ((a (/ pi 3)) (s (sin a)))
  (= a (asin s)))                        =>  #t

```
MD)

CB
reg exp

proc ::constcl::exp {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::exp [$num numval]]
}
CB

TT(

::tcltest::test numbers-24.0 {try exp} -body {
    pew "(exp 3)"
} -output "20.085536923187668\n"

TT)

CB
reg log

proc ::constcl::log {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::log [$num numval]]
}
CB

TT(

::tcltest::test numbers-25.0 {try log} -body {
    pew "(log 3)"
} -output "1.0986122886681098\n"

TT)

CB
reg sin

proc ::constcl::sin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::sin [$num numval]]
}
CB

CB
reg cos

proc ::constcl::cos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::cos [$num numval]]
}
CB

CB
reg tan

proc ::constcl::tan {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::tan [$num numval]]
}
CB

TT(

::tcltest::test numbers-26.0 {try trig} -body {
    pew "(sin (/ pi 3))"
    pew "(cos (/ pi 3))"
    pew "(tan (/ pi 3))"
} -output "0.8660254037844386\n0.5000000000000001\n1.7320508075688767\n"

::tcltest::test numbers-26.1 {try triggering tan} -body {
    pew "(tan #\\A)"
} -returnCodes error -result "NUMBER expected\n(tan #\\A)"
TT)

CB
reg asin

proc ::constcl::asin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::asin [$num numval]]
}
CB

CB
reg acos

proc ::constcl::acos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::acos [$num numval]]
}
CB

CB
reg atan

proc ::constcl::atan {args} {
  if {[llength $args] == 1} {
    set num [lindex $args 0]
    check {number? $num} {
        NUMBER expected\n([pn] [$num show])
    }
    MkNumber [::tcl::mathfunc::atan [$num numval]]
  } else {
    lassign $args num1 num2
    check {number? $num1} {
        NUMBER expected\n([pn] [$num1 show])
    }
    check {number? $num2} {
        NUMBER expected\n([pn] [$num2 show])
    }
    MkNumber [::tcl::mathfunc::atan2 \
      [$num1 numval] [$num2 numval]]
  }
}
CB

TT(

::tcltest::test numbers-27.0 {try trig} -body {
    pew "(asin 0.3)"
    pew "(acos 0.3)"
    pew "(atan 0.3)"
} -output "0.3046926540153975\n1.2661036727794992\n0.2914567944778671\n"

TT)

MD(
__sqrt__

`sqrt` calculates the square root.
MD)

PR(
sqrt (public);num num -> num
PR)

CB
reg sqrt

proc ::constcl::sqrt {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::sqrt [$num numval]]
}
CB

TT(

::tcltest::test numbers-28.0 {try sqrt} -body {
    pew "(sqrt 16)"
} -output "4.0\n"

TT)

MD(
__expt__

`expt` calculates the _x_ to the power of _y_, or _x<sup>y</sup>_.
MD)

PR(
expt (public);num1 num num2 num -> num
PR)

CB
reg expt

proc ::constcl::expt {num1 num2} {
  check {number? $num1} {
      NUMBER expected\n([pn] [$num1 show] \
        [$num2 show])
  }
  check {number? $num2} {
      NUMBER expected\n([pn] [$num1 show] \
        [$num2 show])
  }
  MkNumber [::tcl::mathfunc::pow [$num1 numval] \
    [$num2 numval]]
}
CB

TT(

::tcltest::test numbers-29.0 {try expt} -body {
    pew "(expt 4 2)"
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
__number->string__

The procedures `number->string` and `string->number` convert between
number and string with optional radix conversion.
MD)

PR(
number->string (public);num num ?radix? num -> str
PR)

MD(
Example:

```
(number->string 23)      =>  "23"
(number->string 23 2)    =>  "10111"
(number->string 23 8)    =>  "27"
(number->string 23 16)   =>  "17"
```
MD)

CB
reg number->string

proc ::constcl::number->string {num args} {
  if {[llength $args] == 0} {
    check {number? $num} {
      NUMBER expected\n([pn] [$num show])
    }
    return [MkString [$num numval]]
  } else {
    lassign $args radix
    check {number? $num} {
      NUMBER expected\n([pn] [$num show])
    }
    check {number? $radix} {
      NUMBER expected\n([pn] [$num show] \
        [$radix show])
    }
    set radices [list [MkNumber 2] [MkNumber 8] \
      [MkNumber 10] [MkNumber 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] \
        [$num show] [$radix show])
    }
    if {[$radix numval] == 10} {
      return [MkString [$num numval]]
    } else {
      return [MkString [base [$radix numval] \
        [$num numval]]]
    }
  }
}
CB

MD(
Due to Richard Suchenwirth[#](https://wiki.tcl-lang.org/page/Based+numbers).
MD)

CB
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

::tcltest::test numbers-30.0 {try number->string} -body {
    pew "(number->string 23)"
    pew "(number->string 23 2)"
    pew "(number->string 23 8)"
    pew "(number->string 23 16)"
} -output "\"23\"\n\"10111\"\n\"27\"\n\"17\"\n"

::tcltest::test numbers-30.1 {try number->string} -body {
    pew "(number->string 23 13)"
} -returnCodes error -result "Radix not in 2, 8, 10, 16\n(number->string  23 13)"

TT)

MD(
__string->number__

As with `number->string`, above.
MD)

PR(
string->number (public);str str ?radix? num -> num
PR)

MD(
Example:

```
(string->number "23")        =>  23
(string->number "10111" 2)   =>  23
(string->number "27" 8)      =>  23
(string->number "17" 16)     =>  23
```
MD)

CB
reg string->number

proc ::constcl::string->number {str args} {
  if {[llength $args] == 0} {
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    return [MkNumber [$str value]]
  } else {
    lassign $args radix
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    set radices [list [MkNumber 2] [MkNumber 8] \
      [MkNumber 10] [MkNumber 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] [$str show] \
        [$radix show])
    }
    if {[$radix numval] == 10} {
      return [MkNumber [$str value]]
    } else {
      return [MkNumber [
        frombase [$radix numval] [$str value]]]
    }
  }
}
CB

MD(
Due to Richard Suchenwirth[#](https://wiki.tcl-lang.org/page/Based+numbers).
MD)

CB
proc frombase {base number} {
  set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
  set negative [regexp ^-(.+) $number -> number]
  set res 0
  foreach digit [split $number {}] {
    # dv = decimal value
    set dv [lsearch $digits $digit]
    if {$dv < 0 || $dv >= $base} {
      ::error "bad digit $dv for base $base"
    }
    set res [expr {$res * $base + $dv}]
  }
  if $negative {set res -$res}
  set res
}
CB

TT(

::tcltest::test numbers-31.0 {try string->number} -body {
    pew {(string->number "23")}
    pew {(string->number "10111" 2)}
    pew {(string->number "27" 8)}
    pew {(string->number "17" 16)}
} -output "23\n23\n23\n23\n"

::tcltest::test numbers-31.1 {try string->number} -body {
    pew {(string->number "23" 13)}
} -returnCodes error -result "Radix not in 2, 8, 10, 16\n(string->number \"23\"  13)"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
