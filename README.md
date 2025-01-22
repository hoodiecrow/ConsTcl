# ConsTcl
A second try at a Lisp interpreter written in Tcl, this time with a real Lisp-like type system.


```
namespace eval ::constcl {
    namespace unknown resolve

    proc resolve {cmd args} {
        if {[regexp {^c([ad]{2,4})r$} $cmd -> ads]} {
            set obj [lindex $args 0]
            foreach ad [lreverse [split $ads {}]] {
                if {$ad eq "a"} {
                    set obj [car $obj]
                } else {
                    set obj [cdr $obj]
                }
            }
            return $obj
        } elseif {no} {
            uplevel 1 [dict get $scope $cmd] $args
        } else {
            return -code error "no such command: \"$cmd\""
        }
    }
}

proc reg {sym impl} {
    dict set ::standard_env $sym $impl
}

```

```
# utility functions
proc ::pep {str} {
    set ::inputstr $str
    namespace eval ::constcl {
        write [eval [read]]
    }
}

proc ::pp {str} {
    set ::inputstr $str
    namespace eval ::constcl {
        write [read]
    }
}
```

```
catch { NIL destroy }

oo::class create NIL {
    constructor {} {}
    method truth {} {return #t}
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method numval {} {throw "Not a number"}
    method write {} {puts -nonewline "()"}
    method show {} {format "()"}
}
```

```
reg null? ::constcl::null?

proc ::constcl::null? {obj} {
    if {[info object isa typeof $obj NIL]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] NIL]} {
        return #t
    } else {
        return #f
    }
}
```

```
catch { EndOfFile destroy }

oo::class create EndOfFile {}
```

```
proc ::eof-object? {obj} {
    if {[info object isa typeof $obj EndOfFile]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] EndOfFile]} {
        return #t
    } else {
        return #f
    }
}
```


The algorithm for reading a Lisp value in Constcl is as follows:

1. Get a string through `[gets stdin]`.
2. Test the first character. If it is `(`, read a list. If it is `+`, `-`, or a digit, disambiguate symbols and otherwise read a number. If it is `#`, test the second character and either read a boolean or a vector. If it is `"`, read a string. Otherwise read an identifier.

The algorithm for reading a list is:

1. Read two Lisp values and cons the first to the second.
2. Return the cons.

The algorithm for reading a number is:

1. Read characters until space, and add to the number.
2. Validate the number as double, throw error if false.
3. Return a Number object.

The algorithm for reading a boolean is:

1. Look at the second character.
2. If it is `t`, return #t
3. If it is `f`, return #f

The algorithm for reading a string is:

1. Read a character. If it is `\`, add it to the string and immediately add the next character to the string. If it is `"`, terminate the string. Otherwise, add the character to the string.
2. Repeat #1.
3. Intern the string in the string store and return a String object.

The algorithm for reading an identifier is:

1. Read a character. If it is a space character, terminate the symbol. Otherwise add the character to the symbol's name.
2. Repeat #1.
3. Return a Symbol object.


set inputstr {}

```
proc ::constcl::advance {args} {
    if {[llength $args] == 1} {
        incr args -1
        set ::inputstr [::string range $::inputstr 1+$args end]
    } else {
        set ::inputstr [::string range $::inputstr 1 end]
    }
}
```

```
proc ::constcl::first {} {
    ::string index $::inputstr 0
}
```

```
proc ::constcl::second {} {
    ::string index $::inputstr 1
}
```

```
reg read ::constcl::read

proc ::constcl::read {args} {
    ::constcl::read-value
}
```

```
proc ::constcl::read-value {} {
    skip-whitespace
    if {$::inputstr eq {}} {set ::inputstr [gets stdin]}
    switch -regexp [first] {
        {^$} {
            return
        }
        {\(} {
            advance
            skip-whitespace
            set p [read-pair ")"]
            skip-whitespace
            if {[first] ne ")"} {
                error "Missing right parenthesis (first=[first])."
            }
            advance
            return $p
        }
        {\[} {
            advance
            skip-whitespace
            set p [read-pair "\]"]
            skip-whitespace
            if {[first] ne "\]"} {
                error "Missing right bracket (first=[first])."
            }
            advance
            return $p
        }
        {'} {
            advance
            set p [read-value]
            return [::constcl::list #Q $p]
        }
        {\+} - {\-} {
            if {![::string is digit [second]]} {
                if {[first] eq "+"} {
                    advance
                    return #+
                } else {
                    advance
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
                    return [::constcl::read-character]
                }
                default {
                    error "Illegal #-literal"
                }
            }
        }
        {"} {
            return [::constcl::read-string]
        }
        {[[:space:]]} {advance}
        {[[:graph:]]} {
            return [::constcl::read-identifier]
        }
        default {
            error "unexpected char [first]"
        }

    }
}
```


```
proc ::constcl::read-number {} {
    while {$::inputstr ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append num [first]
        advance
    }
    if {[::string length $num] && [::string is double $num]} {
        return [MkNumber $num]
    } else {
        error "Invalid numeric constant $num"
    }
}
```


```
proc ::constcl::character-check {name} {
    regexp {^#\\([[:graph:]]|space|newline)$} $name
}

proc ::constcl::read-character {} {
    set name "#"
    while {$::inputstr ne {} && ![::string is space [first]]} {
        ::append name [first]
        advance
    }
    if {[::constcl::character-check $name]} {
        return [MkChar $name]
    } else {
        error "Invalid character constant $name"
    }
}
```


```
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
    advance
    return [MkString $str]
}
```


```
proc ::constcl::read-identifier {} {
        ::append name [first]
        advance
    while {$::inputstr ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append name [first]
        advance
    }
    # idcheck throws error if invalid identifier
    return [MkSymbol [::constcl::idcheck $name]]
}
```


```
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
```

```
proc ::constcl::find-char {c} {
    # take a character, peek beyond whitespace to find it
    set cp 0
    while {[::string is space [::string index $::inputstr $cp]]} {
        incr cp
    }
    return [expr {[::string index $::inputstr $cp] eq $c}]
}
```

```
proc ::constcl::read-pair {c} {
    # ")"
    # "foo . bar)"
    # "foo)"
    # "foo bar)"
    skip-whitespace
    if {[first] eq $c} {
        return #NIL
    }
    set a [read]
    if {[::string equal [::string range $::inputstr 0 2] " . "]} {
        advance 3
        skip-whitespace
        set d [read]
        skip-whitespace
        if {[first] ne $c} {
            error "extra elements in dotted pair"
        }
        return [MkCons $a $d]
    } elseif {[find-char $c]} {
        skip-whitespace
        set d #NIL
        return [MkCons $a $d]
    } else {
        lappend res $a
        while {![find-char $c]} {
            if {[llength $res] > 4} break
            set p [read]
            skip-whitespace
            lappend res $p
        }
        set prev #NIL
        foreach r [lreverse $res] {
            set prev [MkCons $r $prev]
        }
        return $prev
    }

}

proc ::constcl::__read-pair {c} {
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
    return [MkCons $a $d]
}
```

```
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
            error "Missing right parenthesis (first=[first])."
        }
        return $p
    } elseif {[first] eq "\["} {
        # same as above, but with [] instead of ()
        advance
        skip-whitespace
        set p [read-pair "\]"]
        skip-whitespace
        if {[first] != "\]"} {
            error "Missing right parenthesis (first=[first])."
        }
        return $p
    }
    return 0
}
```



### Identifier validation

Some routines for checking if a string is a valid identifier. `idcheckinit` checks the
first character, `idchecksubs` checks the rest. `idcheck` calls the others and raises
errors if they fail. A valid symbol is still an invalid identifier if has the name of
some keyword, which idcheck also checks, for a set of keywords given in the standard.

```
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
        error "Identifier expected ($sym)"
    }
    set sym
}

proc ::constcl::varcheck {sym} {
    if {$sym in {else => define unquote unquote-splicing quote lambda if set! begin
        cond and or case let let* letrec do delay quasiquote}} {
            error "Macro name can't be used as a variable: $sym"
    }
}
```


```
reg write ::constcl::write

proc ::constcl::write {obj args} {
    ::constcl::write-value $obj
    puts {}
}
```

```
proc ::constcl::write-value {obj} {
    # take an object and print the value
    $obj write
}
```

```
proc ::constcl::write-pair {obj} {
    # take an object and print the car and the cdr of the stored value
    set a [car $obj]
    set d [cdr $obj]
    # print car
    write-value $a
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d
    } elseif {$d eq "#NIL"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write-value $d
    }
}
```



## Numbers

```
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
    method show {} { set value }
}

proc ::constcl::MkNumber {v} {
    return [Number create Mem[incr ::M] $v]
}
```

```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
reg zero? ::constcl::zero?

proc ::constcl::zero? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj value] == 0} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(zero? [$obj show])"
    }
}
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
                error "NUMBER expected\n(- [$obj show])"
            }
        }
        return $num
    }
}
```


```
reg * ::constcl::*

proc ::constcl::* {args} {
    if {[llength $args] == 0} {
        return #1
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
                $num mult [$obj value]
            } else {
                error "NUMBER expected\n(- [$obj show])"
            }
        }
        return $num
    }
}
```


```
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
```


```
reg / ::constcl::/

proc ::constcl::/ {args} {
    if {[llength $args] == 0} {
        error "expected arguments"
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::constcl::number? $obj] eq "#t"} {
            return [MkNumber [expr {1 / [$obj value]}]]
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
                $num div [$obj value]
            } else {
                error "NUMBER expected\n(- [$obj show])"
            }
        }
        return $num
    }
}
```


```
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
```


```
proc ::constcl::quotient {n1 n2} {
    # TODO
}
```

```
proc ::constcl::remainder {n1 n2} {
    # TODO
}
```

```
proc ::constcl::modulo {n1 n2} {
    # TODO
}
```

```
proc ::constcl::gcd {args} {
    # TODO
}
```

```
proc ::constcl::lcm {args} {
    # TODO
}
```

```
proc ::constcl::numerator {q} {
    # TODO
}
```

```
proc ::constcl::denominator {q} {
    # TODO
}
```

```
reg floor ::constcl::floor

proc ::constcl::floor {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::floor [$x value]]
    } else {
        error "NUMBER expected\n(floor [$x show])"
    }
}
```


```
reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::ceil [$x value]]
    } else {
        error "NUMBER expected\n(ceiling [$x show])"
    }
}
```


```
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
```


```
reg round ::constcl::round

proc ::constcl::round {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::round [$x value]]
    } else {
        error "NUMBER expected\n(round [$x show])"
    }
}
```


```
proc ::constcl::rationalize {x y} {
    # TODO
}
```

```
reg exp ::constcl::exp

proc ::constcl::exp {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::exp [$z value]]
    } else {
        error "NUMBER expected\n(exp [$z show])"
    }
}
```


```
reg log ::constcl::log

proc ::constcl::log {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::log [$z value]]
    } else {
        error "NUMBER expected\n(log [$z show])"
    }
}
```


```
reg sin ::constcl::sin

proc ::constcl::sin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sin [$z value]]
    } else {
        error "NUMBER expected\n(sin [$z show])"
    }
}
```

```
reg cos ::constcl::cos

proc ::constcl::cos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::cos [$z value]]
    } else {
        error "NUMBER expected\n(cos [$z show])"
    }
}
```

```
reg tan ::constcl::tan

proc ::constcl::tan {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::tan [$z value]]
    } else {
        error "NUMBER expected\n(tan [$z show])"
    }
}
```


```
reg asin ::constcl::asin

proc ::constcl::asin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::asin [$z value]]
    } else {
        error "NUMBER expected\n(asin [$z show])"
    }
}
```

```
reg acos ::constcl::acos

proc ::constcl::acos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::acos [$z value]]
    } else {
        error "NUMBER expected\n(acos [$z show])"
    }
}
```

```
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
```


```
reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sqrt [$z value]]
    } else {
        error "NUMBER expected\n(sqrt [$z show])"
    }
}
```


```
reg expt ::constcl::expt

proc ::constcl::expt {z1 z2} {
    if {[::constcl::number? $z1] eq "#t" && [::constcl::number? $z2] eq "#t"} {
        MkNumber [::tcl::mathfunc::pow [$z1 value] [$z2 value]]
    } else {
        error "NUMBER expected\n(expt [$z1 show] [$z2 show])"
    }
}
```


```
proc ::constcl::make-rectangular {x1 x2} {
    # TODO
}
```

```
proc ::constcl::make-polar {x3 x4} {
    # TODO
}
```

```
proc ::constcl::real-part {z} {
    # TODO
}
```

```
proc ::constcl::imag-part {z} {
    # TODO
}
```

```
proc ::constcl::magnitude {z} {
    # TODO
}
```

```
proc ::constcl::angle {z} {
    # TODO
}
```

```
proc ::constcl::exact->inexact {z} {
    # TODO
}
```

```
proc ::constcl::inexact->exact {z} {
    # TODO
}
```

```
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
```


```
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
```



## Booleans

```
oo::class create Boolean {
    superclass NIL
    variable truth
    constructor {v} {
        if {$v ni {#t #f}} {
            error "bad boolean value $v"
        }
        set truth $v
    }
    method truth {} { set truth }
    method write {} { puts -nonewline [my truth] }
    method show {} {set truth}
}

proc ::constcl::MkBoolean {v} {
    foreach instance [info class instances Boolean] {
        if {[$instance truth] eq $v} {
            return $instance
        }
    }
    return [Boolean create Mem[incr ::M] $v]
}
```


```

reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {obj} {
    if {[info object isa typeof $obj Boolean]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Boolean]} {
        return #t
    } else {
        return #f
    }
}
```


```
reg not ::constcl::not

proc ::constcl::not {obj} {
    if {[$obj truth] eq "#f"} {
        return #t
    } else {
        return #f
    }
}
```



## Pairs and lists

```
catch { Cons destroy }

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
    method show {} {format "(%s . %s)" [my car] [my cdr]}
}

proc ::constcl::MkCons {a d} {
    return [Cons create Mem[incr ::M] $a $d]
}
```

```
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
```


```
reg cons ::constcl::cons

proc ::constcl::cons {car cdr} {
    MkCons $car $cdr
}
```

reg car ::constcl::car


```
proc ::constcl::car {obj} {
    $obj car
}
```

```
reg cdr ::constcl::cdr

proc ::constcl::cdr {obj} {
    $obj cdr
}
```

```
reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {obj val} {
    $obj set-car! $val
}
```

```
reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {obj val} {
    $obj set-cdr! $val
}
```

```
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
```

```
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
```

```
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
```

```
reg append ::constcl::append

proc ::constcl::append {args} {
    # TODO
}
```

```
proc ::constcl::reverse {obj} {
    # TODO
}
```

```
reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {obj k} {
    if {[::constcl::zero? $k]} {
        return $obj
    } else {
        ::constcl::list-tail [::constcl::cdr $obj] [::constcl::- $k #1]
    }
}
```

```
reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {obj k} {
    ::constcl::car [::constcl::list-tail $obj $k]
}
```

```
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
```

```
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
```

```
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
```

```
reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {obj1 obj2} {
    if {[::constcl::eq? $obj1 $obj2] eq "#t"} {
        return #t
    } else {
        return #f
    }
}
```

```
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
```

```
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
```

```
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
```


```
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
```

```
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
```

```
reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {obj} {
    if {[::constcl::symbol? $obj] eq "#t"} {
        return [$obj name]
    } else {
        error "SYMBOL expected\n(symbol->string [$obj show])"
    }
}
```

```
reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkSymbol [$str value]]
    } else {
        error "STRING expected\n(string->symbol [$obj show])"
    }
}
```


## Symbols

```
oo::class create Symbol {
    superclass NIL
    variable name
    constructor {n} {
        # TODO idcheck this
        set name $n
    }
    method name {} {set name}
    method value {} {set name}
    method = {symname} {expr {$name eq $symname}}
    method write {} { puts -nonewline [my name] }
    method show {} {set name}
}

proc ::constcl::MkSymbol {n} {
    if {$n eq {}} {
        error "a symbol must have a name"
    }
    foreach instance [info class instances Symbol] {
        if {[$instance name] eq $n} {
            return $instance
        }
    }
    return [Symbol create Mem[incr ::M] $n]
}
```

```
reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {obj} {
    if {[info object isa typeof $obj Symbol]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Symbol]} {
        return #t
    } else {
        return #f
    }
}
```

```
reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {symbol} {
}
```

```
reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {string} {
}
```


## Characters

```
oo::class create Char {
    superclass NIL
    variable value
    constructor {v} {
        # TODO check for #\ and set character names to lowercase
        set value $v
    }
    method char {} {
        if {[regexp {^#\\[[:graph:]]$} [my value]]} {
            return [::string index [my value] 2]
        } elseif {[regexp {^#\\([[:graph:]]+)$} [my value] -> char_name]} {
            # TODO
            switch $char_name {
                space {return " "}
                newline {return "\n"}
            }
        }
    }
    method alphabetic? {} {
        if {[::string is alpha [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method numeric? {} {
        if {[::string is digit [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method whitespace? {} {
        if {[::string is space [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method upper-case? {} {
        if {[::string is upper [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case? {} {
        if {[::string is lower [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method value {} {return $value}
    method write {} { puts -nonewline "#\\$value" }
    method show {} {set value}
}

proc ::constcl::MkChar {v} {
    foreach instance [info class instances Char] {
        if {[$instance value] eq $v} {
            return $instance
        }
    }
    return [Char create Mem[incr ::M] $v]
}
```

```
reg char? ::constcl::char?

proc ::constcl::char? {obj} {
    if {[info object isa typeof $obj Char]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Char]} {
        return #t
    } else {
        return #f
    }
}
```

```
reg char=? ::constcl::char=?

proc ::constcl::char=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] eq [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 show] [$c2 show])"
    }
}
```

```
reg char<? ::constcl::char<?

proc ::constcl::char<? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] < [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 show] [$c2 show])"
    }
}
```

```
reg char>? ::constcl::char>?

proc ::constcl::char>? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] > [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 show] [$c2 show])"
    }
}
```

```
reg char<=? ::constcl::char<=?

proc ::constcl::char<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] <= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 show] [$c2 show])"
    }
}
```

```
reg char>=? ::constcl::char>=?

proc ::constcl::char>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] >= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 show] [$c2 show])"
    }
}
```

```
reg char-ci=? ::constcl::char-ci=?

proc ::constcl::char-ci=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] eq [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 show] [$c2 show])"
    }
}
```

```
reg char-ci<? ::constcl::char-ci<?

proc ::constcl::char-ci<? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] < [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 show] [$c2 show])"
    }
}
```

```
reg char-ci>? ::constcl::char-ci>?

proc ::constcl::char-ci>? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] > [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 show] [$c2 show])"
    }
}
```

```
reg char-ci<=? ::constcl::char-ci<=?

proc ::constcl::char-ci<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] <= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 show] [$c2 show])"
    }
}
```

```
reg char-ci>=? ::constcl::char-ci>=?

proc ::constcl::char-ci>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] >= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 show] [$c2 show])"
    }
}
```

```
reg char-alphabetic? ::constcl::char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char alphabetic?]
    } else {
        error "CHAR expected\n(char-alphabetic? [$char show])"
    }
}
```

```
reg char-numeric? ::constcl::char-numeric?

proc ::constcl::char-numeric? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char numeric?]
    } else {
        error "CHAR expected\n(char-numeric? [$char show])"
    }
}
```

```
reg char-whitespace? ::constcl::char-whitespace?

proc ::constcl::char-whitespace? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char whitespace?]
    } else {
        error "CHAR expected\n(char-whitespace? [$char show])"
    }
}
```

```
reg char-upper-case? ::constcl::char-upper-case?

proc ::constcl::char-upper-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char show])"
    }
}
```

```
reg char-lower-case? ::constcl::char-lower-case?

proc ::constcl::char-lower-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char show])"
    }
}
```

```
proc ::constcl::char->integer {char} {
    # TODO
}
```

```
proc ::constcl::integer->char {n} {
    # TODO
}
```

```
reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [MkChar [string toupper [$char char]]]
    } else {
        error "CHAR expected\n(char-upcase [$char show])"
    }
}
```


```
reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [MkChar [string tolower [$char char]]]
    } else {
        error "CHAR expected\n(char-downcase [$char show])"
    }
}
```


## Strings

```
oo::class create String {
    superclass NIL
    variable s
    constructor {v} {
        set s -1
        for {set i 0} {$i < $::S} {incr i} {
            if {[::string equal [lindex $::StrSto $i] $v]} {
                set s $i
            }
        }
        if {$s == -1} {
            set s $::S
            lset ::StrSto $s $v
            incr ::S
        }
    }
    method index {} {set s}
    method = {str} {string equal [my value] $str}
    method length {} {string length [my value]}
    method ref {i} {string index [my value] $i}
    method value {} {return [lindex $::StrSto $s]}
    method write {} { puts -nonewline "\"[my value]\"" }
    method show {} {format "\"[my value]\""}
}

proc ::constcl::MkString {v} {
    return [String create Mem[incr ::M] $v]
}
```

```
reg string? ::constcl::string?

proc ::constcl::string? {obj} {
    if {[info object isa typeof $obj String]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] String]} {
        return #t
    } else {
        return #f
    }
}
```

```
reg make-string ::constcl::make-string

proc ::constcl::make-string {args} {
    # TODO
}
```

```
reg string ::constcl::string

proc ::constcl::string {args} {
    set str {}
    foreach char $args {
        if {[::constcl::char? $char] eq "#t"} {
            append str [$char char]
        } else {
            error "CHAR expected\n(string [$char show])"
        }
    }
    return [MkString $str]
}
```

```
reg string-length ::constcl::string-length

proc ::constcl::string-length {str} {
    if {[::constcl::str? $String] eq "#t"} {
        return [MkNumber [$str length]]
    } else {
        error "STRING expected\n(string-length [$str show])"
    }
}
```

```
reg string-ref ::constcl::string-ref

proc ::constcl::string-ref {str k} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-ref [$str show] [$k show])"
        }
        return [$str ref $i]
    } else {
        error "STRING expected\n(string-ref [$str show] [$k show])"
    }
}
```

```
reg string-set! ::constcl::string-set!

proc ::constcl::string-set! {str k char} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-set! [$str show] [$k show] [$char show])"
        }
        if {[::constcl::char? $char] eq "#t"} {
            return [$str set! $i [$char char]]
        } else {
            error "CHAR expected\n(string-set! [$str show] [$k show] [$char show])"
        }
    } else {
        error "STRING expected\n(string-set! [$str show] [$k show] [$char show])"
    }
}
```

```
reg string=? ::constcl::string=?

proc ::constcl::string=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] eq [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string=? [$s1 show] [$s2 show])"
    }
}
```

```
reg string-ci=? ::constcl::string-ci=?

proc ::constcl::string-ci=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] eq [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci=? [$s1 show] [$s2 show])"
    }
}
```

```
reg string<? ::constcl::string<?

proc ::constcl::string<? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] < [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<? [$s1 show] [$s2 show])"
    }
}
```

```
reg string-ci<? ::constcl::string-ci<?

proc ::constcl::string-ci<? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] < [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<? [$s1 show] [$s2 show])"
    }
}
```

```
reg string>? ::constcl::string>?

proc ::constcl::string>? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] > [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>? [$s1 show] [$s2 show])"
    }
}
```

```
reg string-ci>? ::constcl::string-ci>?

proc ::constcl::string-ci>? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] > [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>? [$s1 show] [$s2 show])"
    }
}
```

```
reg string<=? ::constcl::string<=?

proc ::constcl::string<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] <= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<=? [$s1 show] [$s2 show])"
    }
}
```

```
reg string-ci<=? ::constcl::string-ci<=?

proc ::constcl::string-ci<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] <= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<=? [$s1 show] [$s2 show])"
    }
}
```

```
reg string>=? ::constcl::string>=?

proc ::constcl::string>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] >= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>=? [$s1 show] [$s2 show])"
    }
}
```

```
reg string-ci>=? ::constcl::string-ci>=?

proc ::constcl::string-ci>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] >= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>=? [$s1 show] [$s2 show])"
    }
}
```

```
reg substring ::constcl::substring

proc ::constcl::substring {str start end} {
    if {[::constcl::string? $str] eq "t"} {
        if {[::constcl::number? $start] eq "t" && [::constcl::number? $end] eq "t"} {
            return [MkString [$str substring [$start value] [$end value]]]
        } else {
            error "NUMBER expected\n(substring [$str show] [$start show] [$end show])"
        }
    } else {
        error "STRING expected\n(substring [$str show] [$start show] [$end show])"
    }
}
```

```
reg string-append ::constcl::string-append

proc ::constcl::string-append {args} {
    # TODO
}
```

```
reg string->list ::constcl::string->list

proc ::constcl::string->list {str} {
    # TODO
}
```

```
reg list->string ::constcl::list->string

proc ::constcl::list->string {list} {
    # TODO
}
```

```
reg string-copy ::constcl::string-copy

proc ::constcl::string-copy {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkString [$str value]]
    } else {
        error "STRING expected\n(string-copy [$str show])"
    }
}
```

```
reg string-fill! ::constcl::string-fill!

proc ::constcl::string-fill! {str char} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkString [$str fill [$char value]]]
    } else {
        error "STRING expected\n(string-fill [$str show] [$char show])"
    }
}
```


## Vectors

```
oo::class create Vector {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method length {} {string length $value}
    method ref {i} {string index $value $i}
    method value {} {return $value}
    method write {} {puts -nonewline #($value)}
    method show {} {return #($value)}
}

proc ::constcl::MkVector {v} {
    foreach instance [info class instances Vector] {
        if {[$instance value] eq $v} {
            return $instance
        }
    }
    return [Vector create Mem[incr ::M] $v]
}
```

```
reg vector? ::constcl::vector?

proc ::constcl::vector? {obj} {
    if {[info object isa typeof $obj Vector]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Vector]} {
        return #t
    } else {
        return #f
    }
}
```

```
reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {args} {
    # TODO
}
```

```
reg vector ::constcl::vector

proc ::constcl::vector {args} {
    # TODO
}
```

```
reg vector-length ::constcl::vector-length

proc ::constcl::vector-length {vec} {
    if {[::constcl::vector? $vec] eq "#t"} {
        return [MkNumber [$str length]]]
    } else {
        error "VECTOR expected\n(vector-length [$vec show])"
    }
}
```

```
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str ref $k]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec show] [$k show])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec show] [$k show])"
    }
}
```


```
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k obj} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str set! $k $obj]]
        } else {
            error "NUMBER expected\n(vector-set! [$vec show] [$k show] [$obj show])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec show] [$k show] [$obj show])"
    }
}
```

```
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
    # TODO
}
```

```
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
    # TODO
}
```

```
reg vector-fill ::constcl::vector-fill

proc ::constcl::vector-fill {vec fill} {
    if {[::constcl::vector? $vec] eq "#t"} {
        $vec fill $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec show] [$fill show])"
    }
}
```


## Control

```
catch { Procedure destroy }

oo::class create Procedure {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method value {} {
        set value
    }
    method write {} { puts -nonewline Procedure[self] }
    method call {vals} {
        # TODO
    }
}
```

```
reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {obj} {
    if {[info object isa typeof $obj Procedure]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Procedure]} {
        return #t
    } else {
        return #f
    }
}
```

```
reg apply ::constcl::apply

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
```

```
reg map ::constcl::map

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
```

```
reg for-each ::constcl::for-each

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
```

```
proc ::constcl::force {promise} {
    # TODO
}
```

```
proc ::constcl::call-with-current-continuation {proc} {
    # TODO
}
```

```
proc ::constcl::values {args} {
    # TODO
}
```

```
proc ::constcl::call-with-values {producer consumer} {
    # TODO
}
```

```
proc ::constcl::dynamic-wind {before thunk after} {
    # TODO
}
```


## Eval

```
reg eval ::constcl::eval

proc ::constcl::eval {e {env ::global_env}} {
    # TODO
    if {[atom? $e] eq "#t"} {
        if {[symbol? $e] eq "#t"} {
            return [lookup $e $env]
        } elseif {[null? $e] eq "#t" || [number? $e] eq "#t" || [string? $e] eq "#t" || [char? $e] eq "#t" || [boolean? $e] eq "#t" || [vector? $e] eq "#t"} {
            return $e
        } else {
            error "cannot evaluate $e"
        }
    } else {
        switch [[car $e] name] {
            quote {
                return [cadr $e]
            }
            if {
                if {[eval [cadr $e] $env] ne "#f"} {
                    return [eval [caddr $e] $env]]
                } else {
                    return [eval [cadddr $e] $env]]
                }
            }
            begin {
                return [eprogn [cdr $e] $env]
            }
            define {
                declare [cadr $e] [eval [caddr $e] $env] $env
            }
            set! {
                return [update! [cadr $e] $env [eval [caddr $e] $env]]
            }
            lambda {
                return [make-function [cadr $e] [cddr $e] $env]
            }
            default {
                return [invoke [eval [car $e] $env] [evlis [cdr $e] $env]]
            }
        }
    }
}
```

```
proc ::constcl::lookup {sym env} {
    set sym [$sym name]
    [$env find $sym] get $sym
}
```

```
proc ::constcl::declare {sym val env} {
    $env set [$sym name] $val
    return #NIL
}
```

```
proc ::constcl::evlis {exps env} {
    if {[pair? $exps] eq "#t"} {
        return [cons [eval [car $exps] $env] [evlis [cdr $exps] $env]]
    } else {
        return #NIL
    }
}
```

```
proc ::constcl::invoke {pr vals} {
    if {[procedure? $pr] eq "#t"} {
        $pr call {*}[splitlist $vals]
    } elseif {[::string match "::constcl::*" $pr]} {
        $pr {*}[splitlist $vals]
    } else {
        error "PROCEDURE expected\n" ; #([$pr write] [$vals write])"
    }
}
```

```
proc ::constcl::splitlist {vals} {
#puts [info level [info level]]
    set result {}
    while {[pair? $vals] eq "#t"} {
        lappend result [car $vals]
        set vals [cdr $vals]
    }
#puts result=$result
#puts resval=[lmap res $result {$res show}]
    return $result
}
```


```
proc ::constcl::scheme-report-environment {version} {
    # TODO
}
```

```
proc ::constcl::null-environment {version} {
    # TODO
}
```

```
proc ::constcl::interaction-environment {} {
    # TODO
}
```


## Input and output

```
proc ::constcl::call-with-input-file {string proc} {
    # TODO
}
```

```
proc ::constcl::call-with-output-file {string proc} {
    # TODO
}
```

```
proc ::constcl::input-port? {obj} {
    # TODO
}
```

```
proc ::constcl::output-port? {obj} {
    # TODO
}
```

```
proc ::constcl::current-input-port {} {
    # TODO
}
```

```
proc ::constcl::current-output-port {} {
    # TODO
}
```

```
proc ::constcl::with-input-from-file {string thunk} {
    # TODO
}
```


```
proc ::constcl::with-output-to-file {string thunk} {
    # TODO
}
```

```
proc ::constcl::open-input-file {filename} {
    # TODO
}
```

```
proc ::constcl::open-output-file {filename} {
    # TODO
}
```

```
proc ::constcl::close-input-port {port} {
    # TODO
}
```

```
proc ::constcl::close-output-port {port} {
    # TODO
}
```

```
if no {
    # defined in read.tcl
proc ::constcl::read {args} {
    # TODO
}
}
```

```
proc ::constcl::read-char {args} {
    # TODO
}
```

```
proc ::constcl::peek-char {args} {
    # TODO
}
```

```
proc ::constcl::char-ready? {args} {
    # TODO
}
```

```
if no {
proc ::constcl::write {obj args} {
    # TODO write [$obj write]
}
}
```

```
reg display ::constcl::display

proc ::constcl::display {obj args} {
    # TODO write [$obj display]
}
```

```
reg newline ::constcl::newline

proc ::constcl::newline {args} {
    # TODO write newline
}
```

```
proc ::constcl::write-char {args} {
    # TODO
}
```

```
proc ::constcl::load {filename} {
    # TODO
}
```

```
proc ::constcl::transcript-on {filename} {
    # TODO
}
```

```
proc ::constcl::transcript-off {} {
    # TODO
}
```


unset -nocomplain M
# memory cell number
set M 0

unset -nocomplain S
# string store number
set S 0

unset -nocomplain StrSto
set StrSto [list]

interp alias {} #NIL {} [NIL create Mem0]

interp alias {} #t {} [::constcl::MkBoolean #t]

interp alias {} #f {} [::constcl::MkBoolean #f]

interp alias {} #-1 {} [::constcl::MkNumber -1]

interp alias {} #0 {} [::constcl::MkNumber 0]

interp alias {} #1 {} [::constcl::MkNumber 1]

interp alias {} #Q {} [::constcl::MkSymbol quote]

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #EOF {} [EndOfFile create Mem[incr ::M]]

```
dict set ::standard_env pi [::constcl::MkNumber 3.1415926535897931]
```


reg atom? ::constcl::atom?

proc ::constcl::atom? {obj} {
    if {[symbol? $obj] eq "#t" || [number? $obj] eq "#t" || [string? $obj] eq "#t" || [char? $obj] eq "#t" || [boolean? $obj] eq "#t" || [vector? $obj] eq "#t"} {
        return #t
    } else {
        return #f
    }
}


### Environment class and objects

The class for environments is called __Environment__. It is mostly a wrapper around a dictionary,
with the added finesse of keeping a link to the outer environment (starting a chain that goes all
the way to the global environment and then stops at the null environment) which can be traversed
by the find method to find which innermost environment a given symbol is bound in.

```
catch { Environment destroy }

oo::class create Environment {
    variable bindings outer_env
    constructor {syms vals {outer {}}} {
	set bindings [dict create]
        foreach sym $syms val $vals {
            my set $sym $val
        }
        set outer_env $outer
    }
    method find {sym} {
        if {$sym in [dict keys $bindings]} {
            self
        } else {
            $outer_env find $sym
        }
    }
    method get {sym} {
        dict get $bindings $sym
    }
    method set {sym val} {
        dict set bindings $sym $val
    }
}
```

### Procedure class and objects

A __Procedure__ object is basically a
[closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)),
storing the procedure's parameter list, the body, and the environment that is current
when the object is created (when the procedure is defined).


```
catch { Procedure destroy }

oo::class create Procedure {
    variable parms body env
    constructor {p b e} {
        set parms $p
        set body $b
        set env $e
    }
    method call {args} {
	if {[llength $parms] != [llength $args]} {
	    error "Wrong number of arguments passed to procedure"
	}
	set newenv [Environment new $parms $args $env]
	set res {}
	foreach expr $body {
            set res [evaluate $expr $newenv]
	}
	set res
    }
}
```

When a __Procedure__ object is called, the body is evaluated in a new environment
where the parameters are given values from the argument list and the outer link
goes to the closure environment.

On startup, two __Environment__ objects called __null_env__ (the null environment, not the same
as __null-environment__ in Scheme) and __global_env__ (the global environment) are created. 

Make __null_env__ empty and unresponsive: this is where searches for unbound symbols end up.

```
Environment create null_env {} {}

oo::objdefine null_env {
    method find {sym} {self}
    method get {sym} {error "Unbound variable: $sym"}
    method set {sym val} {error "Unbound variable: $sym"}
}
```

Meanwhile, __global_env__ is populated with all the definitions from __standard_env__. This is
where top level evaluation happens.

```
Environment create global_env [dict keys $standard_env] [dict values $standard_env] null_env
```

Thereafter, each time a user-defined procedure is called, a new __Environment__ object is
created to hold the bindings introduced by the call, and also a link to the outer environment
(the one closed over when the procedure was defined).

#### Lexical scoping

A procedure definition form creates a new procedure. Example:

```
Thtcl> (define circle-area (lambda (r) (* pi (* r r))))
Thtcl> (circle-area 10)
314.1592653589793
```

During a call to the procedure `circle-area`, the symbol `r` is bound to the
value 10. But we don't want the binding to go into the global environment,
possibly clobbering an earlier definition of `r`. The solution is to use
separate (but linked) environments, making `r`'s binding a
_[local variable](https://en.wikipedia.org/wiki/Local_variable)_
in its own environment, which the procedure will be evaluated in. The symbols
`*` and `pi` will still be available through the local environment's link
to the outer global environment. This is all part of
_[lexical scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope)_.

In the first image, we see the global environment before we call __circle-area__
(and also the empty null environment which the global environment links to):

![A global environment](/images/env1.png)

During the call. Note how the global `r` is shadowed by the local one, and how
the local environment links to the global one to find `*` and `pi`. 

![A local environment shadows the global](/images/env2.png)

After the call, we are back to the first state again.

![A global environment](/images/env1.png)




