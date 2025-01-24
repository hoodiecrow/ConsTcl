
MD(
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

MD)

set inputstr {}

CB
proc ::constcl::advance {args} {
    if {[llength $args] == 1} {
        incr args -1
        set ::inputstr [::string range $::inputstr 1+$args end]
    } else {
        set ::inputstr [::string range $::inputstr 1 end]
    }
}
CB

CB
proc ::constcl::first {} {
    ::string index $::inputstr 0
}
CB

CB
proc ::constcl::second {} {
    ::string index $::inputstr 1
}
CB

CB
reg read ::constcl::read

proc ::constcl::read {args} {
    ::constcl::read-value
}
CB

CB
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
            make-constant $p
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
                    advance
                    set p [::constcl::read-vector]
                    if {[first] ne ")"} {
                        error "Missing right parenthesis (first=[first])."
                    }
                    advance
                    return $p
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
        set ::inputstr "(a (b))"
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

CB
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
CB

TT(
::tcltest::test read-2.0 {try reading a number} {
    set ::inputstr "99.99"
    set obj [::constcl::read]
    $obj value
} "99.99"

::tcltest::test read-2.1 {try reading a number} {
    set ::inputstr "     99.99"
    set obj [::constcl::read]
    $obj value
} "99.99"

::tcltest::test read-2.2 {try reading a number} {
    set ::inputstr "     9"
    set obj [::constcl::read]
    $obj value
} "9"

::tcltest::test read-2.3 {try reading a number} {
    set ::inputstr "     +9"
    set obj [::constcl::read]
    $obj value
} "+9"

::tcltest::test read-2.4 {try reading a number} {
    set ::inputstr "     -9"
    set obj [::constcl::read]
    $obj value
} "-9"

::tcltest::test read-2.5 {try reading a number} {
    set ::inputstr "     - "
    set obj [::constcl::read]
    $obj name
} "-"

::tcltest::test read-2.6 {try reading a number} {
    set ::inputstr "     + "
    set obj [::constcl::read]
    $obj name
} "+"

TT)

CB
proc ::constcl::character-check {name} {
    regexp -nocase {^#\\([[:graph:]]|space|newline)$} $name
}

proc ::constcl::read-character {} {
    set name "#"
    while {$::inputstr ne {} && ![::string is space [first]] && [first] ni {) ]}} {
        ::append name [first]
        advance
    }
    if {[::constcl::character-check $name]} {
        if {[regexp -nocase {^#\\(space|newline)$} $name]} {
            set name [::string tolower $name]
        }
        return [MkChar $name]
    } else {
        error "Invalid character constant $name"
    }
}
CB

TT(

::tcltest::test read-3.0 {try reading a character} {
    set ::inputstr {#\A}
    set obj [::constcl::read]
    $obj char
} "A"

::tcltest::test read-3.1 {try reading a character} {
    set ::inputstr "#\\space"
    set obj [::constcl::read]
    $obj char
} " "

::tcltest::test read-3.2 {try reading a character} {
    set ::inputstr "#\\newline"
    set obj [::constcl::read]
    $obj char
} "\n"

::tcltest::test read-3.3 {try reading a character} -body {
    set ::inputstr "#\\foobar"
    set obj [::constcl::read]
    $obj char
} -returnCodes error -result "Invalid character constant #\\foobar"

TT)

CB
proc ::constcl::read-vector {} {
    set res {}
    skip-whitespace
    while {$::inputstr ne {} && [first] ne ")"} {
        lappend res [read]
        skip-whitespace
    }
    set vec [MkVector $res]
    $vec mkconstant
    return $vec
}
CB

CB
proc ::constcl::read-string {} {
    set str {}
    advance
    while {[first] ne {"}} {
        set c [first]
        if {$c eq "\\"} {
            #::append str $c
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
    set ::inputstr {"foo bar"}
    set obj [::constcl::read]
    $obj value
} "foo bar"

::tcltest::test read-4.1 {try reading a string} {
    set ::inputstr {"\"foo\" \\ bar"}
    set obj [::constcl::read]
    $obj value
} {"foo" \ bar}

TT)

CB
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
CB

TT(

::tcltest::test read-5.0 {try reading an identifier} {
    set ::inputstr "foo"
    set obj [::constcl::read]
    $obj name
} "foo"

::tcltest::test read-5.1 {try reading an identifier} -body {
    set ::inputstr "+foo"
    set obj [::constcl::read-identifier]
    $obj name
} -returnCodes error -result "Identifier expected (+foo)"

::tcltest::test read-5.2 {try reading an identifier} -body {
    set ::inputstr "let"
    set obj [::constcl::read]
    ::constcl::varcheck [$obj name]
} -returnCodes error -result "Macro name can't be used as a variable: let"

TT)

CB
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
CB

CB
proc ::constcl::find-char {c} {
    # take a character, peek beyond whitespace to find it
    set cp 0
    while {[::string is space [::string index $::inputstr $cp]]} {
        incr cp
    }
    return [expr {[::string index $::inputstr $cp] eq $c}]
}
CB

CB
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
        return [cons $a $d]
    } elseif {[find-char $c]} {
        skip-whitespace
        set d #NIL
        return [cons $a $d]
    } else {
        lappend res $a
        while {![find-char $c]} {
            if {[llength $res] > 99} break
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
    return [cons $a $d]
}
CB

CB
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
CB


