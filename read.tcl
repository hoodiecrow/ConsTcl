
MD(
## read
MD)

MD(
A quick-and-dirty input simulator, using an input buffer variable to hold characters
to be read. The `advance` command consumes one character from the buffer. The `first`
command peeks at the first (next) character in the buffer; `second` peeks at the 
second. `skip-whitespace` advances past whitespace and comments.

MD)

CB
set inputbuffer {}

proc ::constcl::advance {args} {
    if {[llength $args] == 1} {
        incr args -1
        set ::inputbuffer [::string range $::inputbuffer 1+$args end]
    } else {
        set ::inputbuffer [::string range $::inputbuffer 1 end]
    }
}

proc ::constcl::first {} {
    ::string index $::inputbuffer 0
}

proc ::constcl::second {} {
    ::string index $::inputbuffer 1
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
                while {[first] != "\n" && $::inputbuffer ne {}} {
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
`parse` fills the input buffer and then reads and parses the input.
MD)

CB
proc ::constcl::parse {str} {
    set ::inputbuffer $str
    return [read]
}
CB

MD(
The standard builtin `read` consumes and parses input into a Lisp expression.
MD)

CB
reg read ::constcl::read

proc ::constcl::read {args} {
    ::constcl::read-value
}
CB

MD(
The helper procedure `read-value` reads a value of any kind.
MD)

CB
proc ::constcl::read-value {} {
    skip-whitespace
    if {$::inputbuffer eq {}} {set ::inputbuffer [gets stdin]}
    switch -regexp [first] {
        {^$} {
            return
        }
        {\.} {
            advance
            return [Dot new]
        }
        {\(} {
            advance
            skip-whitespace
            set val [read-pair ")"]
            skip-whitespace
            if {[first] ne ")"} {
                error "Missing right parenthesis (first=[first])."
            }
            advance
            return $val
        }
        {\[} {
            advance
            skip-whitespace
            set val [read-pair "\]"]
            skip-whitespace
            if {[first] ne "\]"} {
                error "Missing right bracket (first=[first])."
            }
            advance
            return $val
        }
        {'} {
            advance
            set val [read-value]
            make-constant $val
            return [::constcl::list #Q $val]
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
                    advance
                    set val [::constcl::read-vector]
                    if {[first] ne ")"} {
                        error "Missing right parenthesis (first=[first])."
                    }
                    advance
                    return $val
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

TT(

::tcltest::test read-1.0 {try reading quoted symbol} -body {
    pp "'foo"
} -output "(quote foo)\n"

::tcltest::test read-1.1 {try reading a list} -body {
    namespace eval ::constcl {
        set ::inputbuffer "(a (b))"
        set obj [read]
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
`read-number` reads a number and returns a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

CB
proc ::constcl::read-number {} {
    while {$::inputbuffer ne {} && ![::string is space [first]] && [first] ni {) \]}} {
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
    set ::inputbuffer "99.99"
    set obj [::constcl::read]
    $obj value
} "99.99"

::tcltest::test read-2.1 {try reading a number} {
    set ::inputbuffer "     99.99"
    set obj [::constcl::read]
    $obj value
} "99.99"

::tcltest::test read-2.2 {try reading a number} {
    set ::inputbuffer "     9"
    set obj [::constcl::read]
    $obj value
} "9"

::tcltest::test read-2.3 {try reading a number} {
    set ::inputbuffer "     +9"
    set obj [::constcl::read]
    $obj value
} "+9"

::tcltest::test read-2.4 {try reading a number} {
    set ::inputbuffer "     -9"
    set obj [::constcl::read]
    $obj value
} "-9"

::tcltest::test read-2.5 {try reading a number} {
    set ::inputbuffer "     - "
    set obj [::constcl::read]
    $obj name
} "-"

::tcltest::test read-2.6 {try reading a number} {
    set ::inputbuffer "     + "
    set obj [::constcl::read]
    $obj name
} "+"

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
`read-character` reads a character and returns a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

CB
proc ::constcl::read-character {} {
    set name "#"
    while {$::inputbuffer ne {} && ![::string is space [first]] && [first] ni {) ]}} {
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
    set ::inputbuffer {#\A}
    set obj [::constcl::read]
    $obj char
} "A"

::tcltest::test read-3.1 {try reading a character} {
    set ::inputbuffer "#\\space"
    set obj [::constcl::read]
    $obj char
} " "

::tcltest::test read-3.2 {try reading a character} {
    set ::inputbuffer "#\\newline"
    set obj [::constcl::read]
    $obj char
} "\n"

::tcltest::test read-3.3 {try reading a character} -body {
    set ::inputbuffer "#\\foobar"
    set obj [::constcl::read]
    $obj char
} -returnCodes error -result "Invalid character constant #\\foobar"

TT)

MD(
`read-vector` reads a vector value and returns a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

CB
proc ::constcl::read-vector {} {
    set res {}
    skip-whitespace
    while {$::inputbuffer ne {} && [first] ne ")"} {
        lappend res [read]
        skip-whitespace
    }
    set vec [MkVector $res]
    $vec mkconstant
    return $vec
}
CB

MD(
`read-string` reads a string value and returns a [String](https://github.com/hoodiecrow/ConsTcl#strings) object.
MD)

CB
proc ::constcl::read-string {} {
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
    set str [MkString $str]
    $str mkconstant
    return $str
}
CB

TT(

::tcltest::test read-4.0 {try reading a string} {
    set ::inputbuffer {"foo bar"}
    set obj [::constcl::read]
    $obj value
} "foo bar"

::tcltest::test read-4.1 {try reading a string} {
    set ::inputbuffer {"\"foo\" \\ bar"}
    set obj [::constcl::read]
    $obj value
} {"foo" \ bar}

TT)

MD(
`read-identifier` reads an identifier value and returns a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

CB
proc ::constcl::read-identifier {} {
    ::append name [first]
    advance
    while {$::inputbuffer ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append name [first]
        advance
    }
    # idcheck throws error if invalid identifier
    return [MkSymbol [idcheck $name]]
}
CB

TT(

::tcltest::test read-5.0 {try reading an identifier} {
    set ::inputbuffer "foo"
    set obj [::constcl::read]
    $obj name
} "foo"

::tcltest::test read-5.1 {try reading an identifier} -body {
    set ::inputbuffer "+foo"
    set obj [::constcl::read-identifier]
    $obj name
} -returnCodes error -result "Identifier expected (+foo)"

::tcltest::test read-5.2 {try reading an identifier} -body {
    set ::inputbuffer "let"
    set obj [::constcl::read]
    ::constcl::varcheck [$obj name]
} -returnCodes error -result "Macro name can't be used as a variable: let"

TT)

MD(
The `find-char` helper procedure reads past whitespace to find a given character.
Returns Tcl truth if it is found.
MD)

CB
proc ::constcl::find-char {c} {
    set cp 0
    while {[::string is space [::string index $::inputbuffer $cp]]} {
        incr cp
    }
    return [expr {[::string index $::inputbuffer $cp] eq $c}]
}
CB

MD(
The `read-pair` procedure reads values and returns a [Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) object.
MD)

CB

proc ::constcl::dot? {obj} {
    if {[info object isa typeof $obj Dot]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Dot]} {
        return #t
    } else {
        return #f
    }
}

proc ::constcl::read-pair {c} {
    skip-whitespace
    if {[find-char $c]} {
        return #NIL
    }
    set a [read]
    skip-whitespace
    set res $a
    set prev #NIL
    while {![find-char $c]} {
        set x [read]
        skip-whitespace
        if {[dot? $x] eq "#t"} {
            set prev [read]
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

proc ::constcl::__read-pair {c} {
    skip-whitespace
    if {[first] eq $c} {
        return #NIL
    }
    set a [read]
    skip-whitespace
    if {[first] eq "."} {
        advance
        skip-whitespace
        set d [read]
        skip-whitespace
        if {[first] ne $c} {
            error "extra elements in dotted pair"
        }
        return [cons $a $d]
    } elseif {[find-char $c]} {
        skip-whitespace
        set d #NIL
        return [cons $a $d]
    } else {
        lappend res $a
        while {![find-char $c]} {
            #if {[llength $res] > 99} break
            set p [read]
            skip-whitespace
            lappend res $p
        }
        set prev #NIL
        foreach r [lreverse $res] {
            set prev [cons $r $prev]
        }
        return $prev
    }

}
CB

TT(
::tcltest::test read-6.0 {try reading an improper list} -body {
    pp "(a . b)"
} -output "(a . b)\n"

::tcltest::test read-6.1 {try reading an improper list} -body {
    pp "(a b . c)"
} -output "(a b . c)\n"
TT)

