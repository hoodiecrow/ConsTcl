
MD(
## read

`read` represents the interpreter's input facility. Currently input is faked with input
strings.
MD)

MD(
A quick-and-dirty input simulator, using an input buffer object to hold characters to be
read. The `fill` method fills the buffer and sets the first character in the peek position.
The `advance` method consumes one character from the buffer. `first` peeks at the next
character to be read. `skip-ws` advances past whitespace and comments.  `unget` backs up
one position and sets a given character in the peek position. The `find` method looks past
whitespace and comments to find a given character. It returns Tcl truth if it is found.
Or it gets the hose again.

MD)

CB
catch { ::constcl::IB destroy }

oo::class create ::constcl::IB {
    variable peekc buffer
    constructor {} {
        set peekc {}
        set buffer {}
    }
    method fill {str} {
        set buffer $str
        my advance
    }
    method advance {} {
        if {$buffer eq {}} {
            set peekc {}
        } else {
            set peekc [::string index $buffer 0]
            set buffer [::string range $buffer 1 end]
        }
    }
    method first {} {
        return $peekc
    }
    method unget {char} {
        set buffer $peekc$buffer
        set peekc $char
    }
    method find {char} {
        if {[::string is space -strict $peekc]} {
            for {set cp 0} {$cp < [::string length $buffer]} {incr cp} {
                if {![::string is space -strict [::string index $buffer $cp]]} {
                    break
                }
            }
            return [expr {[::string index $buffer $cp] eq $char}]
        } else {
            return [expr {$peekc eq $char}]
        }
    }
    method skip-ws {} {
        while true {
            switch -regexp $peekc {
                {[[:space:]]} {
                    my advance
                }
                {;} {
                    while {$peekc ne "\n" && $peekc ne {}}  {
                        my advance
                    }
                }
                default {
                    return
                }
            }
        }
    }
}

::constcl::IB create ::constcl::ib
CB

MD(
Given a string, `parse` fills the input buffer. It then reads and parses the input.
MD)

CB
proc ::constcl::parse {str} {
    ib fill $str
    return [parse-value]
}
CB

MD(
The standard builtin `read` consumes and parses input into a Lisp expression.
MD)

CB
reg read ::constcl::read

proc ::constcl::read {args} {
    return [parse-value]
}
CB

MD(
The procedure `parse-value` reads a value of any kind.
MD)

CB
proc ::constcl::parse-value {} {
    ib skip-ws
    switch -regexp [ib first] {
        {^$}          { return }
        {\"}          { return [parse-string] }
        {\#}          { return [parse-sharp] }
        {\'}          { return [parse-quoted-value] }
        {\(}          { return [parse-pair-value ")"] }
        {\+} - {\-}   { return [parse-plus-minus] }
        {\,}          { return [parse-unquoted-value] }
        {\.}          { ib advance ; return [Dot new] }
        {\[}          { return [parse-pair-value "\]"] }
        {\`}          { return [parse-quasiquoted-value] }
        {\d}          { return [parse-number-value] }
        {[[:space:]]} { ib advance }
        {[[:graph:]]} { return [parse-identifier-value] }
        default {
            error "unexpected char [ib first]"
        }
    }
}
CB

MD(
`parse-string` reads a string value and returns a [String](https://github.com/hoodiecrow/ConsTcl#strings) object.
MD)

CB
proc ::constcl::parse-string {} {
    set str {}
    ib advance
    while {[ib first] ne {"}} {
        set c [ib first]
        if {$c eq "\\"} {
            ib advance
            ::append str [ib first]
        } else {
            ::append str $c
        }
        ib advance
    }
    ib advance
    ib skip-ws
    set obj [MkString $str]
    $obj mkconstant
    return $obj
}
CB

TT(

::tcltest::test read-4.0 {try reading a string} {
    set obj [::constcl::parse {"foo bar"}]
    $obj value
} "foo bar"

::tcltest::test read-4.1 {try reading a string} {
    set obj [::constcl::parse {"\"foo\" \\ bar"}]
    $obj value
} {"foo" \ bar}

TT)

MD(
`parse-sharp` reads the various kinds of values whose literal begins with
a sharp sign (#).
MD)

CB
proc ::constcl::parse-sharp {} {
    ib advance
    switch [ib first] {
        (    { return [parse-vector-value] }
        t    { ib advance ; ib skip-ws ; return #t }
        f    { ib advance ; ib skip-ws ; return #f }
        "\\" { return [parse-character-value] }
        default {
            error "Illegal #-literal"
        }
    }
}
CB

MD(
The `make-constant` helper procedure is called to set objects to
constants when read as a quoted literal.
MD)

CB
proc ::constcl::make-constant {obj} {
    if {[pair? $obj] eq "#t"} {
        $obj mkconstant
        make-constant [car $obj]
        make-constant [cdr $obj]
    } elseif {[null? $obj] eq "#t"} {
        return #NIL
    } else {
        $obj mkconstant
    }
}
CB

MD(
`parse-quoted-value` reads a value and returns it wrapped in `quote`.
MD)

CB
proc ::constcl::parse-quoted-value {} {
    ib advance
    set val [parse-value]
    ib skip-ws
    make-constant $val
    return [list #Q $val]
}
CB

TT(

::tcltest::test read-1.0 {try reading quoted symbol} -body {
    pp "'foo"
} -output "(quote foo)\n"

TT)

MD(
The `parse-pair-value` procedure reads values and returns a structure of
[Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) objects.
MD)

CB

proc ::constcl::parse-pair {char} {
    if {[ib find $char]} {
        return #NIL
    }
    ib skip-ws
    set a [parse-value]
    ib skip-ws
    set res $a
    set prev #NIL
    while {![ib find $char]} {
        set x [parse-value]
        ib skip-ws
        if {[dot? $x] eq "#t"} {
            set prev [parse-value]
            ib skip-ws
        } else {
            lappend res $x
        }
        if {[llength $res] > 999} break
    }
    foreach r [lreverse $res] {
        set prev [cons $r $prev]
    }
    return $prev
}

proc ::constcl::parse-pair-value {char} {
    ib advance
    ib skip-ws
    set val [parse-pair $char]
    ib skip-ws
    if {[ib first] ne $char} {
        if {$char eq ")"} {
            error "Missing right parenthesis (first=[ib first])."
        } else {
            error "Missing right bracket (first=[ib first])."
        }
    }
    ib advance
    ib skip-ws
    return $val
}
CB

TT(
::tcltest::test read-6.0 {try reading an improper list} -body {
    pp "(a . b)"
} -output "(a . b)\n"

::tcltest::test read-6.1 {try reading an improper list} -body {
    pp "(a b . c)"
} -output "(a b . c)\n"

::tcltest::test read-1.1 {try reading a list} -body {
    namespace eval ::constcl {
        set obj [::constcl::parse "(a (b))"]
        [caadr $obj] name
    }
} -result "b"

::tcltest::test read-1.2 {try reading a list} -body {
    pp "(a)"
} -output "(a)\n"

::tcltest::test read-1.3 {try reading a list} -body {
    pp "(a b)"
} -output "(a b)\n"

::tcltest::test read-1.4 {try reading a list} -body {
    pp "(a b c)"
} -output "(a b c)\n"

::tcltest::test read-1.5 {try reading a list} -body {
    pp "(a b c d)"
} -output "(a b c d)\n"

::tcltest::test read-1.6 {try reading a list} -body {
    pp "(a b c d e)"
} -output "(a b c d e)\n"

::tcltest::test read-1.7 {try reading a list} -body {
    pp "(a (b) )"
} -output "(a (b))\n"

::tcltest::test read-1.8 {try reading a list} -body {
    pp "(a (b))"
} -output "(a (b))\n"

TT)

MD(
`parse-plus-minus` reacts to a plus or minus in the input buffer, and either
returns a `#+` or `#-` symbol, or a number.
MD)

CB
proc ::constcl::parse-plus-minus {} {
    set c [ib first]
    ib advance
    if {[::string is digit -strict [ib first]]} {
        ib unget $c
        return [::constcl::parse-number-value]
    } else {
        if {$c eq "+"} {
            ib skip-ws
            return [MkSymbol "+"]
        } else {
            ib skip-ws
            return [MkSymbol "-"]
        }
    }
}
CB

MD(
`parse-unquoted-value` reads a value and returns it wrapped in `unquote`.
MD)

CB
proc ::constcl::parse-unquoted-value {} {
    ib advance
    set symbol "unquote"
    if {[ib first] eq "@"} {
        set symbol "unquote-splicing"
        ib advance
    }
    set val [parse-value]
    ib skip-ws
    return [list [MkSymbol $symbol] $val]
}
CB

TT(

::tcltest::test read-1.9 {try reading unquoted symbol} -body {
    pp ",foo"
} -output "(unquote foo)\n"

TT)

MD(
`parse-quasiquoted-value` reads a value and returns it wrapped in `quasiquote`.
MD)

CB
proc ::constcl::parse-quasiquoted-value {} {
    ib advance
    set val [parse-value]
    ib skip-ws
    make-constant $val
    return [list [MkSymbol "quasiquote"] $val]
}
CB

TT(

::tcltest::test read-1.10 {try reading unquoted symbol} -body {
    pp "`(list 1 2 ,@foo)"
} -output "(quasiquote (list 1 2 (unquote-splicing foo)))\n"

TT)

MD(
`parse-number-value` reads a number and returns a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

CB
proc ::constcl::parse-number-value {} {
    while {[ib first] ne {} && ![::string is space -strict [ib first]] && [ib first] ni {) \]}} {
        ::append num [ib first]
        ib advance
    }
    ib skip-ws
    if {[::string is double -strict $num]} {
        return [MkNumber $num]
    } else {
        error "Invalid numeric constant $num"
    }
}
CB

TT(
::tcltest::test read-2.0 {try reading a number} {
    set obj [::constcl::parse "99.99"]
    $obj value
} "99.99"

::tcltest::test read-2.1 {try reading a number} {
    set obj [::constcl::parse "     99.99"]
    $obj value
} "99.99"

::tcltest::test read-2.2 {try reading a number} {
    set obj [::constcl::parse "     9"]
    $obj value
} "9"

::tcltest::test read-2.3 {try reading a number} {
    set obj [::constcl::parse "     +9"]
    $obj value
} "+9"

::tcltest::test read-2.4 {try reading a number} {
    set obj [::constcl::parse "     -9"]
    $obj value
} "-9"

::tcltest::test read-2.5 {try reading a number} {
    set obj [::constcl::parse "     - "]
    $obj name
} "-"

::tcltest::test read-2.6 {try reading a number} {
    set obj [::constcl::parse "     + "]
    $obj name
} "+"

TT)

MD(
`parse-identifier-value` reads an identifier value and returns a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

CB
proc ::constcl::parse-identifier-value {} {
    while {[ib first] ne {} && ![::string is space -strict [ib first]] && [ib first] ni {) \]}} {
        ::append name [ib first]
        ib advance
    }
    ib skip-ws
    # idcheck throws error if invalid identifier
    return [MkSymbol [idcheck $name]]
}
CB

TT(

::tcltest::test read-5.0 {try reading an identifier} {
    set obj [::constcl::parse "foo"]
    $obj name
} "foo"

::tcltest::test read-5.1 {try reading an identifier} -body {
    ::constcl::ib fill "+foo"
    set obj [::constcl::parse-identifier-value]
    $obj name
} -returnCodes error -result "Identifier expected (+foo)"

::tcltest::test read-5.2 {try reading an identifier} -body {
    set obj [::constcl::parse "let"]
    ::constcl::varcheck [$obj name]
} -returnCodes error -result "Macro name can't be used as a variable: let"

TT)

MD(
The `character-check` helper procedure compares a potential
character constant to the valid kinds. Returns Tcl truth (1/0).
MD)

CB
proc ::constcl::character-check {name} {
    regexp -nocase {^#\\([[:graph:]]|space|newline)$} $name
}
CB

MD(
`parse-character-value` reads a character and returns a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

CB
proc ::constcl::parse-character-value {} {
    set name "#"
    while {[ib first] ne {} && ![::string is space -strict [ib first]] && [ib first] ni {) ]}} {
        ::append name [ib first]
        ib advance
    }
    if {[::constcl::character-check $name]} {
        return [MkChar $name]
    } else {
        error "Invalid character constant $name"
    }
    ib skip-ws
}
CB

TT(

::tcltest::test read-3.0 {try reading a character} {
    set obj [::constcl::parse {#\A}]
    $obj char
} "A"

::tcltest::test read-3.1 {try reading a character} {
    set obj [::constcl::parse "#\\space"]
    $obj char
} " "

::tcltest::test read-3.2 {try reading a character} {
    set obj [::constcl::parse "#\\newline"]
    $obj char
} "\n"

::tcltest::test read-3.3 {try reading a character} -body {
    set obj [::constcl::parse "#\\foobar"]
    $obj char
} -returnCodes error -result "Invalid character constant #\\foobar"

TT)

MD(
`parse-vector-value` reads a vector value and returns a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

CB
proc ::constcl::parse-vector-value {} {
    ib advance
    ib skip-ws
    set res {}
    while {[ib first] ne {} && [ib first] ne ")"} {
        lappend res [parse-value]
        ib skip-ws
    }
    set vec [MkVector $res]
    $vec mkconstant
    if {[ib first] ne ")"} {
        error "Missing right parenthesis (first=[ib first])."
    }
    ib advance
    ib skip-ws
    return $vec
}
CB

TT(

::tcltest::test read-6.0 {try reading a vector} -body {
    pp "#(1 2 3)"
} -output "#(1 2 3)\n"

TT)
