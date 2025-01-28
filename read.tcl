
MD(
## read

`read` represents the interpreter's input facility. Currently input is faked with input
strings.
MD)

MD(
A quick-and-dirty input simulator, using an input buffer variable to hold characters
to be read. The `fill-buffer` command fills the buffer and sets the first character in the
peek position. The `advance` command consumes one character from the buffer. `first` peeks
at the next character to be read. `skip-whitespace` advances past whitespace and comments.
`unget` backs up one position and sets a given character in the peek position.
The `find-char` helper procedure reads past whitespace to find a given character.
It returns Tcl truth if it is found.

MD)

CB
set ::constcl::peekchar __
set ::constcl::inputbuffer {}

proc ::constcl::fill-buffer {str} {
    variable inputbuffer
    set inputbuffer $str
    advance
}

proc ::constcl::advance {} {
    variable peekchar
    variable inputbuffer
    if {$inputbuffer eq {}} {
        set peekchar {}
    } else {
        set peekchar [::string index $inputbuffer 0]
        set inputbuffer [::string range $inputbuffer 1 end]
    }
}

proc ::constcl::first {} {
    variable peekchar
    if {$peekchar == "__"} {
        advance
    }
    return $peekchar
}

proc ::constcl::unget {c} {
    variable peekchar
    variable inputbuffer
    set inputbuffer [first]$inputbuffer
    set peekchar $c
}

proc ::constcl::find-char {c} {
    if {[::string is space [first]]} {
        for {set cp 0} {$cp < [::string length $::constcl::inputbuffer]} {incr cp} {
            if {![::string is space [::string index $::constcl::inputbuffer $cp]]} {
                break
            }
        }
        return [expr {[::string index $::constcl::inputbuffer $cp] eq $c}]
    } else {
        return [expr {[first] eq $c}]
    }
}

proc ::constcl::skip-whitespace {} {
    # move the source pointer past whitespace and comments
    # adapted from Robert Nystrom, 'Crafting Interpreters'
    while true {
        set c [first]
        switch $c {
            " " - "\r" - "\t" -
            "\n" {
                advance
            }
            ";" {
                # a comment goes on until the end of the line
                while {[first] != "\n" && [first] ne {}} {
                    advance
                }
            }
            default {
                return
            }
        }
    }
}
CB

MD(
When given a string, `parse` fills the input buffer. It then reads and parses the input.
MD)

CB
proc ::constcl::parse {args} {
    if {[llength $args]} {
        fill-buffer [lindex $args 0]
    }
    return [parse-value]
}
CB

MD(
The standard builtin `read` consumes and parses input into a Lisp expression.
MD)

CB
reg read ::constcl::read

proc ::constcl::read {args} {
    ::constcl::parse-value
}
CB

MD(
The helper procedure `parse-value` reads a value of any kind.
MD)

CB
proc ::constcl::parse-value {} {
    skip-whitespace
    switch -regexp [first] {
        {^$}          { return }
        {\"}          { return [parse-string] }
        {\#}          { return [parse-sharp] }
        {\'}          { return [parse-quoted-value] }
        {\(}          { return [parse-pair-value ")"] }
        {\+} - {\-}   { return [parse-plus-minus] }
        {\.}          { advance ; return [Dot new] }
        {\[}          { return [parse-pair-value "\]"] }
        {\d}          { return [parse-number] }
        {[[:space:]]} { advance }
        {[[:graph:]]} { return [parse-identifier] }
        default {
            error "unexpected char [first]"
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
    advance
    while {[first] ne {"}} {
        set c [first]
        if {$c eq "\\"} {
            advance
            ::append str [first]
        } else {
            ::append str $c
        }
        advance
    }
    advance
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
    advance
    switch [first] {
        (    { return [parse-vector] }
        t    { advance ; return #t }
        f    { advance ; return #f }
        "\\" { return [parse-character] }
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
    advance
    set val [parse-value]
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
The `parse-pair-value` procedure reads values and returns a [Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) object.
MD)

CB

proc ::constcl::parse-pair {c} {
    skip-whitespace
    if {[find-char $c]} {
        return #NIL
    }
    set a [parse-value]
    skip-whitespace
    set res $a
    set prev #NIL
    while {![find-char $c]} {
        set x [parse-value]
        skip-whitespace
        if {[dot? $x] eq "#t"} {
            set prev [parse-value]
            skip-whitespace
        } else {
            lappend res $x
        }
        if {[llength $res] > 99} break
    }
    foreach r [lreverse $res] {
        set prev [cons $r $prev]
    }
    return $prev
}

proc ::constcl::parse-pair-value {char} {
    advance
    skip-whitespace
    set val [parse-pair $char]
    skip-whitespace
    if {[first] ne $char} {
        if {$char eq ")"} {
            error "Missing right parenthesis (first=[first])."
        } else {
            error "Missing right bracket (first=[first])."
        }
    }
    advance
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
    set c [first]
    advance
    if {[::string is digit [first]]} {
        return [::constcl::parse-number $c]
    } else {
        if {$c eq "+"} {
            return [MkSymbol +]
        } else {
            return [MkSymbol -]
        }
    }
}
CB

MD(
`parse-number` reads a number and returns a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

CB
proc ::constcl::parse-number {args} {
    if {[llength $args] > 0} {
        unget [lindex $args 0]
    }
    while {[first] ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append num [first]
        advance
    }
    if {[::string is double $num]} {
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
`parse-identifier` reads an identifier value and returns a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

CB
proc ::constcl::parse-identifier {} {
    ::append name [first]
    advance
    while {[first] ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append name [first]
        advance
    }
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
    set ::constcl::inputbuffer "+foo"
    set obj [::constcl::parse-identifier]
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
`parse-character` reads a character and returns a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

CB
proc ::constcl::parse-character {} {
    set name "#"
    while {[first] ne {} && ![::string is space [first]] && [first] ni {) ]}} {
        ::append name [first]
        advance
    }
    if {[::constcl::character-check $name]} {
        return [MkChar $name]
    } else {
        error "Invalid character constant $name"
    }
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
`parse-vector` reads a vector value and returns a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

CB
proc ::constcl::parse-vector {} {
    advance
    skip-whitespace
    set res {}
    while {[first] ne {} && [first] ne ")"} {
        lappend res [parse-value]
        skip-whitespace
    }
    set vec [MkVector $res]
    $vec mkconstant
    if {[first] ne ")"} {
        error "Missing right parenthesis (first=[first])."
    }
    advance
    return $vec
}
CB

TT(

::tcltest::test read-6.0 {try reading a vector} -body {
    pp "#(1 2 3)"
} -output "#(1 2 3)\n"

TT)
