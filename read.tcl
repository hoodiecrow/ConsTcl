
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

PR(
parse (public);str lisp -> expr
PR)

CB
reg parse

proc ::constcl::parse {str} {
    ib fill $str
    return [parse-expression]
}
CB

MD(
The standard builtin `read` consumes and parses input into a Lisp expression.
MD)

PR(
read (public);args dc -> expr
PR)

CB
reg read ::constcl::read

proc ::constcl::read {args} {
    return [parse-expression]
}
CB

MD(
The procedure `parse-expression` parses an expression of any kind.
MD)

PR(
parse-expression (internal);-> expr
PR)

CB
proc ::constcl::parse-expression {} {
    ib skip-ws
    switch -regexp [ib first] {
        {^$}          { return }
        {\"}          { return [parse-string-expression] }
        {\#}          { return [parse-sharp] }
        {\'}          { return [parse-quoted-expression] }
        {\(}          { return [parse-pair-expression ")"] }
        {\+} - {\-}   { return [parse-plus-minus] }
        {\,}          { return [parse-unquoted-expression] }
        {\.}          { ib advance ; return [Dot new] }
        {\[}          { return [parse-pair-expression "\]"] }
        {\`}          { return [parse-quasiquoted-expression] }
        {\d}          { return [parse-number-expression] }
        {[[:space:]]} { ib advance }
        {[[:graph:]]} { return [parse-identifier-expression] }
        default {
            error "unexpected char [ib first]"
        }
    }
}
CB

MD(
`parse-string-expression` parses a string expression and returns a [String](https://github.com/hoodiecrow/ConsTcl#strings) object.
MD)

PR(
parse-string-expression (internal);-> str
PR)

CB
proc ::constcl::parse-string-expression {} {
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
    set expr [MkString $str]
    $expr mkconstant
    return $expr
}
CB

TT(

::tcltest::test read-4.0 {try reading a string} {
    set expr [::constcl::parse {"foo bar"}]
    $expr value
} "foo bar"

::tcltest::test read-4.1 {try reading a string} {
    set expr [::constcl::parse {"\"foo\" \\ bar"}]
    $expr value
} {"foo" \ bar}

TT)

MD(
`parse-sharp` parses the various kinds of expressions whose literal begins with
a sharp sign (#).
MD)

PR(
parse-sharp (internal);-> sharp
PR)

CB
proc ::constcl::parse-sharp {} {
    ib advance
    switch [ib first] {
        (    { return [parse-vector-expression] }
        t    { ib advance ; ib skip-ws ; return #t }
        f    { ib advance ; ib skip-ws ; return #f }
        "\\" { return [parse-character-expression] }
        default {
            error "Illegal #-literal"
        }
    }
}
CB

MD(
The `make-constant` helper procedure is called to set values to
constants when read as a quoted literal.
MD)

CB
proc ::constcl::make-constant {val} {
    if {[pair? $val] ne "#f"} {
        $val mkconstant
        make-constant [car $val]
        make-constant [cdr $val]
    } elseif {[null? $val] ne "#f"} {
        return #NIL
    } else {
        $val mkconstant
    }
}
CB

MD(
`parse-quoted-expression` parses an expression and returns it wrapped in `quote`.
MD)

PR(
parse-quoted-expression (internal);-> quote
PR)

CB
proc ::constcl::parse-quoted-expression {} {
    ib advance
    set expr [parse-expression]
    ib skip-ws
    make-constant $expr
    return [list #Q $expr]
}
CB

TT(

::tcltest::test read-1.0 {try reading quoted symbol} -body {
    pp "'foo"
} -output "(quote foo)\n"

TT)

MD(
The `parse-pair-expression` procedure parses expressions and returns a structure of
[Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) objects.
MD)

PR(
parse-pair-expression (internal);char pterm -> pstr
PR)

CB

proc ::constcl::parse-pair {char} {
    if {[ib find $char]} {
        return #NIL
    }
    ib skip-ws
    set a [parse-expression]
    ib skip-ws
    set res $a
    set prev #NIL
    while {![ib find $char]} {
        set x [parse-expression]
        ib skip-ws
        if {[dot? $x] ne "#f"} {
            set prev [parse-expression]
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

proc ::constcl::parse-pair-expression {char} {
    ib advance
    ib skip-ws
    set expr [parse-pair $char]
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
    return $expr
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
        set expr [::constcl::parse "(a (b))"]
        [caadr $expr] name
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

PR(
parse-plus-minus (internal);-> pm
PR)

CB
proc ::constcl::parse-plus-minus {} {
    set c [ib first]
    ib advance
    if {[::string is digit -strict [ib first]]} {
        ib unget $c
        return [::constcl::parse-number-expression]
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
`parse-unquoted-expression` reads an expression and returns it wrapped in `unquote`, or possibly
in `unquote-splicing`.
MD)

PR(
parse-unquoted-expression (internal);-> unquote
PR)

CB
proc ::constcl::parse-unquoted-expression {} {
    ib advance
    set symbol "unquote"
    if {[ib first] eq "@"} {
        set symbol "unquote-splicing"
        ib advance
    }
    set expr [parse-expression]
    ib skip-ws
    return [list [MkSymbol $symbol] $expr]
}
CB

TT(

::tcltest::test read-1.9 {try reading unquoted symbol} -body {
    pp ",foo"
} -output "(unquote foo)\n"

TT)

MD(
`parse-quasiquoted-expression` reads an expression and returns it wrapped in `quasiquote`.
MD)

PR(
parse-quasiquoted-expression (internal);-> qquote
PR)

CB
proc ::constcl::parse-quasiquoted-expression {} {
    ib advance
    set expr [parse-expression]
    ib skip-ws
    make-constant $expr
    return [list [MkSymbol "quasiquote"] $expr]
}
CB

TT(

::tcltest::test read-1.10 {try reading unquoted symbol} -body {
    pp "`(list 1 2 ,@foo)"
} -output "(quasiquote (list 1 2 (unquote-splicing foo)))\n"

TT)

MD(
`parse-number-expression` reads a number and returns a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

PR(
parse-number-expression (internal);-> num
PR)

CB
proc ::constcl::parse-number-expression {} {
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
`parse-identifier-expression` reads an identifier expression and returns a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

PR(
parse-identifier-expression (internal);-> sym
PR)

CB
proc ::constcl::parse-identifier-expression {} {
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
    set expr [::constcl::parse "foo"]
    $expr name
} "foo"

::tcltest::test read-5.1 {try reading an identifier} -body {
    ::constcl::ib fill "+foo"
    set expr [::constcl::parse-identifier-expression]
    $expr name
} -returnCodes error -result "Identifier expected (+foo)"

::tcltest::test read-5.2 {try reading an identifier} -body {
    set expr [::constcl::parse "let"]
    ::constcl::varcheck [$expr name]
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
`parse-character-expression` reads a character and returns a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

PR(
parse-character-expression (internal);-> char
PR)

CB
proc ::constcl::parse-character-expression {} {
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
    set expr [::constcl::parse {#\A}]
    $expr char
} "A"

::tcltest::test read-3.1 {try reading a character} {
    set expr [::constcl::parse "#\\space"]
    $expr char
} " "

::tcltest::test read-3.2 {try reading a character} {
    set expr [::constcl::parse "#\\newline"]
    $expr char
} "\n"

::tcltest::test read-3.3 {try reading a character} -body {
    set expr [::constcl::parse "#\\foobar"]
    $expr char
} -returnCodes error -result "Invalid character constant #\\foobar"

TT)

MD(
`parse-vector-expression` reads a vector expression and returns a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

PR(
parse-vector-expression (internal);-> vec
PR)

CB
proc ::constcl::parse-vector-expression {} {
    ib advance
    ib skip-ws
    set res {}
    while {[ib first] ne {} && [ib first] ne ")"} {
        lappend res [parse-expression]
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
