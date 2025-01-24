
MD(
### Characters
MD)

CB
oo::class create Char {
    superclass NIL
    variable value
    constructor {v} {
        # TODO check for #\ and set character names to lowercase
        set value $v
    }
    method char {} {
        switch $value {
            "#\\space" {
                return " "
            }
            "#\\newline" {
                return "\n"
            }
            default {
                return [::string index [my value] 2]
            }
        }
    }
    method alphabetic? {} {
        if {[::string is alpha [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method numeric? {} {
        if {[::string is digit [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method whitespace? {} {
        if {[::string is space [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method upper-case? {} {
        if {[::string is upper [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case? {} {
        if {[::string is lower [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method mkconstant {} {}
    method constant {} {return 1}
    method value {} {return $value}
    method write {} { puts -nonewline $value }
    method show {} {set value}
}

proc ::constcl::MkChar {v} {
    foreach instance [info class instances Char] {
        if {[$instance value] eq $v} {
            return $instance
        }
    }
    return [Char new $v]
}
CB

CB
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
CB

TT(

::tcltest::test characters-1.0 {try char?} -body {
    pep {(char? #\A)}
} -output "#t\n"

TT)

CB
reg char=? ::constcl::char=?

proc ::constcl::char=? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {$c1 eq $c2} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.1 {try char=?} -body {
    pep {(char=? #\A #\A)}
    pep {(char=? #\A #\a)}
    pep {(char=? #\Space #\space)}
} -output "#t\n#f\n#t\n"

TT)

CB
reg char<? ::constcl::char<?

proc ::constcl::char<? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] < [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.2 {try char<?} -body {
    pep {(char<? #\A #\A)}
    pep {(char<? #\A #\B)}
    pep {(char<? #\B #\A)}
} -output "#f\n#t\n#f\n"

TT)

CB
reg char>? ::constcl::char>?

proc ::constcl::char>? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] > [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.3 {try char>?} -body {
    pep {(char>? #\A #\A)}
    pep {(char>? #\A #\B)}
    pep {(char>? #\B #\A)}
} -output "#f\n#f\n#t\n"

TT)

CB
reg char<=? ::constcl::char<=?

proc ::constcl::char<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] <= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.4 {try char<=?} -body {
    pep {(char<=? #\A #\A)}
    pep {(char<=? #\A #\B)}
    pep {(char<=? #\B #\A)}
} -output "#t\n#t\n#f\n"

TT)

CB
reg char>=? ::constcl::char>=?

proc ::constcl::char>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] >= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.5 {try char>=?} -body {
    pep {(char>=? #\A #\A)}
    pep {(char>=? #\A #\B)}
    pep {(char>=? #\B #\A)}
} -output "#t\n#f\n#t\n"

TT)

CB
reg char-ci=? ::constcl::char-ci=?

proc ::constcl::char-ci=? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] eq [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.6 {try char-ci=?} -body {
    pep {(char-ci=? #\A #\a)}
    pep {(char-ci=? #\A #\b)}
    pep {(char-ci=? #\B #\a)}
} -output "#t\n#f\n#f\n"

TT)

CB
reg char-ci<? ::constcl::char-ci<?

proc ::constcl::char-ci<? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] < [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.7 {try char-ci<?} -body {
    pep {(char-ci<? #\A #\a)}
    pep {(char-ci<? #\A #\b)}
    pep {(char-ci<? #\B #\a)}
} -output "#f\n#t\n#f\n"

TT)

CB
reg char-ci>? ::constcl::char-ci>?

proc ::constcl::char-ci>? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] > [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.8 {try char-ci>?} -body {
    pep {(char-ci>? #\A #\a)}
    pep {(char-ci>? #\A #\b)}
    pep {(char-ci>? #\B #\a)}
} -output "#f\n#f\n#t\n"

TT)

CB
reg char-ci<=? ::constcl::char-ci<=?

proc ::constcl::char-ci<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] <= [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.9 {try char-ci<=?} -body {
    pep {(char-ci<=? #\A #\a)}
    pep {(char-ci<=? #\A #\b)}
    pep {(char-ci<=? #\B #\a)}
} -output "#t\n#t\n#f\n"

TT)

CB
reg char-ci>=? ::constcl::char-ci>=?

proc ::constcl::char-ci>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "#t" && [::constcl::char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] >= [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 show] [$c2 show])"
    }
}
CB

TT(

::tcltest::test characters-1.10 {try char-ci>=?} -body {
    pep {(char-ci>=? #\A #\a)}
    pep {(char-ci>=? #\A #\b)}
    pep {(char-ci>=? #\B #\a)}
    pep {(char-ci>=? #\A #\Space)}
} -output "#t\n#f\n#t\n#t\n"

TT)

CB
reg char-alphabetic? ::constcl::char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char alphabetic?]
    } else {
        error "CHAR expected\n(char-alphabetic? [$char show])"
    }
}
CB

TT(

::tcltest::test characters-1.11 {try char-alphabetic?} -body {
    pep {(char-alphabetic? #\A)}
    pep {(char-alphabetic? #\9)}
    pep {(char-alphabetic? #\space)}
    pep {(char-alphabetic? #\A)}
    pep {(char-alphabetic? #\a)}
    pep {(char-alphabetic? #\%)}
} -output "#t\n#f\n#f\n#t\n#t\n#f\n"

TT)

CB
reg char-numeric? ::constcl::char-numeric?

proc ::constcl::char-numeric? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char numeric?]
    } else {
        error "CHAR expected\n(char-numeric? [$char show])"
    }
}
CB

TT(

::tcltest::test characters-1.12 {try char-numeric?} -body {
    pep {(char-numeric? #\A)}
    pep {(char-numeric? #\9)}
    pep {(char-numeric? #\space)}
    pep {(char-numeric? #\A)}
    pep {(char-numeric? #\a)}
    pep {(char-numeric? #\%)}
} -output "#f\n#t\n#f\n#f\n#f\n#f\n"

TT)

CB
reg char-whitespace? ::constcl::char-whitespace?

proc ::constcl::char-whitespace? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char whitespace?]
    } else {
        error "CHAR expected\n(char-whitespace? [$char show])"
    }
}
CB

TT(

::tcltest::test characters-1.13 {try char-whitespace?} -body {
    pep {(char-whitespace? #\A)}
    pep {(char-whitespace? #\9)}
    pep {(char-whitespace? #\space)}
    pep {(char-whitespace? #\A)}
    pep {(char-whitespace? #\a)}
    pep {(char-whitespace? #\%)}
} -output "#f\n#f\n#t\n#f\n#f\n#f\n"

TT)

CB
reg char-upper-case? ::constcl::char-upper-case?

proc ::constcl::char-upper-case? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char show])"
    }
}
CB

TT(

::tcltest::test characters-1.14 {try char-upper-case?} -body {
    pep {(char-upper-case? #\A)}
    pep {(char-upper-case? #\9)}
    pep {(char-upper-case? #\space)}
    pep {(char-upper-case? #\A)}
    pep {(char-upper-case? #\a)}
    pep {(char-upper-case? #\%)}
} -output "#t\n#f\n#f\n#t\n#f\n#f\n"

TT)

CB
reg char-lower-case? ::constcl::char-lower-case?

proc ::constcl::char-lower-case? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char show])"
    }
}
CB

TT(

::tcltest::test characters-1.15 {try char-lower-case?} -body {
    pep {(char-lower-case? #\A)}
    pep {(char-lower-case? #\9)}
    pep {(char-lower-case? #\space)}
    pep {(char-lower-case? #\A)}
    pep {(char-lower-case? #\a)}
    pep {(char-lower-case? #\%)}
} -output "#f\n#f\n#f\n#f\n#t\n#f\n"

TT)

CB
proc ::constcl::char->integer {char} {
    # TODO
}
CB

CB
proc ::constcl::integer->char {n} {
    # TODO
}
CB

CB
reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        if {[regexp {^#\\[[:alpha:]]$} [$char value]]} {
            return [MkChar [::string toupper [$char value]]]
        } else {
            return $char
        }
    } else {
        error "CHAR expected\n(char-upcase [$char show])"
    }
}
CB

TT(

::tcltest::test characters-1.16 {try char-upcase?} -body {
    pep {(char-upcase #\A)}
    pep {(char-upcase #\a)}
    pep {(char-upcase #\space)}
} -output "#\\A\n#\\A\n#\\space\n"

TT)


CB
reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        if {[regexp {^#\\[[:alpha:]]$} [$char value]]} {
            return [MkChar [::string tolower [$char value]]]
        } else {
            return $char
        }
    } else {
        error "CHAR expected\n(char-downcase [$char show])"
    }
}
CB

TT(

::tcltest::test characters-1.17 {try char-downcase?} -body {
    pep {(char-downcase #\A)}
    pep {(char-downcase #\a)}
    pep {(char-downcase #\space)}
} -output "#\\a\n#\\a\n#\\space\n"

TT)

