
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
proc ::constcl::advance {} {
    set ::inputstr [string range $::inputstr 1 end]
}
CB

CB
proc ::constcl::first {} {
    string index $::inputstr 0
}
CB

CB
proc ::constcl::read-value {n} {
    if {$::inputstr eq {}} {set ::inputstr [gets stdin]}
    switch -regexp [first] {
        ( {
            advance
            skip-whitespace
            set p [read-pair ")"]
            skip-whitespace
            if {[first] != ")"} {
                error "Missing right parenthesis."
            }
            return $p

        }
        {\+} - {\-} -
        {\d} {
            # TODO check if + / - is a symbol
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
                {\\} {
                    return [::constcl::read-char]
                }
                default {
                    error "Illegal #-literal"
                }
            }
        }
        {"} {
            return [::constcl::read-string]
        }
        default {
            return [::constcl::read-identifier]
        }

    }
}
CB

CB
proc ::constcl::read-number {} {
    set num {}
    while {$::inputstr ne {} && ![string is space [first]]} {
        append num [first]
        advance
    }
    if {[string is double $num]} {
        return [Number create Mem[incr ::M] $num]
    } else {
        error "Invalid numeric constant $num"
    }
}
CB

CB
proc ::constcl::read-char {} {
    set name {}
    while {$::inputstr ne {} && ![string is space [first]]} {
        append name [first]
        advance
    }
    if {[::constcl::character-check $name]} {
        return [Char create Mem[incr ::M] $num]
    } else {
        error "Invalid character constant $name"
    }
}
CB

CB
proc ::constcl::read-string {} {
    set str {}
    while {[first] ne {"}} {
        set c [first]
        if {$c eq "\\"} {
            append str $c
            advance
            append str [first]
        }
        advance
    }
    return [String create Mem[incr ::M] $str]
}
CB

CB
proc ::constcl::read-identifier {} {
    set name {}
    while {$::inputstr ne {} && ![string is space [first]]} {
        append name [first]
        advance
    }
    if {[::constcl::idcheck $name]} {
        return [Symbol create Mem[incr ::M] $num]
    } else {
        error "Invalid identifier $name"
    }
}
CB

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
    while {[string is space [lindex $::inputstr $cp]} {
        incr cp
    }
    return [expr {[lindex $::inputstr $cp] eq c}]
}
CB

CB
proc ::constcl::read-pair {c} {
    # take a character, read a car and a cdr value, pass the char to findC
    skip-whitespace
    set a [read-value]
    if {[string equal [string range $::inputstr 0 3] " . "]} {
        advance
        advance
        advance
        skip-whitespace
        set d [read-value]
    } elseif {[find-char $c]} {
        skip-whitespace
        set d 0
    } else {
        skip-whitespace
        set d [readPair $c]
    }
    skip-whitespace();
    return [Cons create Mem[incr ::M] $a $d
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
            error "Missing right parenthesis."
        }
        return $p
    } elseif {[first] eq "\["} {
        # same as above, but with [] instead of ()
        advance
        skip-whitespace
        set p [read-pair "\]"]
        skip-whitespace
        if {[first] != "\]"} {
            error "Missing right parenthesis."
        }
        return $p
    }
    return 0
}
CB


