
MD(
### Characters

Characters are any Unicode printing character, and also space and newline space characters.
MD)

CB
oo::class create ::constcl::Char {
    superclass ::constcl::NIL
    variable value
    constructor {v} {
        ::if {[regexp {^#\\([[:graph:]]|space|newline)$} $v]} {
            set value $v
        } else {
            ::if {$v eq "#\\ "} {
                set value #\\space
            } elseif {$v eq "#\\\n"} {
                set value #\\newline
            } else {
                ::error "CHAR expected\n$v"
            }
        }
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
        ::if {[::string is alpha -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method numeric? {} {
        ::if {[::string is digit -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method whitespace? {} {
        ::if {[::string is space -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method upper-case? {} {
        ::if {[::string is upper -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case? {} {
        ::if {[::string is lower -strict [my char]]} {
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
    ::if {[regexp -nocase {^#\\(space|newline)$} $v]} {
        set v [::string tolower $v]
    }
    foreach instance [info class instances ::constcl::Char] {
        ::if {[$instance value] eq $v} {
            return $instance
        }
    }
    return [::constcl::Char new $v]
}
CB

MD(
**char?**

`char?` recognizes Char values by type.
MD)

PR(
char? (public);val val -> bool
PR)

CB
reg char? ::constcl::char?

proc ::constcl::char? {val} {
    ::if {[info object isa typeof $val ::constcl::Char]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Char]} {
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

MD(
**char=?**

**char<?**

**char>?**

**char<=?**

**char>=?**

`char=?`, `char<?`, `char>?`, `char<=?`, and `char>=?` compare character
values. They only compare two characters at a time.
MD)

PR(
char=?, char<?, char>?, char<=?, char>=? (public);char1 char char2 char -> bool
PR)

CB
reg char=? ::constcl::char=?

proc ::constcl::char=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {$char1 eq $char2} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test characters-1.1 {try char=?} -body {
    pep {(char=? #\A #\A)}
    pep {(char=? #\A #\a)}
    pep {(char=? #\Space #\space)}
} -output "#t\n#f\n#t\n"

::tcltest::test characters-check-1.0 {try triggering a check} -body {
    pep {(char=? 99 #\A)}
} -returnCodes error -result "CHAR expected\n(char=? 99 #\\A)"

TT)

CB
reg char<? ::constcl::char<?

proc ::constcl::char<? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] < [$char2 char]} {
        return #t
    } else {
        return #f
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

proc ::constcl::char>? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] > [$char2 char]} {
        return #t
    } else {
        return #f
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

proc ::constcl::char<=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] <= [$char2 char]} {
        return #t
    } else {
        return #f
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

proc ::constcl::char>=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] >= [$char2 char]} {
        return #t
    } else {
        return #f
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

MD(
**char-ci=?**

**char-ci<?**

**char-ci>?**

**char-ci<=?**

**char-ci>=?**

`char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, and `char-ci>=?` compare character
values in a case insensitive manner. They only compare two characters at a time.
MD)

PR(
char-ci=?, char-ci<?, char-ci>?, char-ci<=?, char-ci>=? (public);char1 char char2 char -> bool
PR)

CB
reg char-ci=? ::constcl::char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] eq [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
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

proc ::constcl::char-ci<? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] < [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
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

proc ::constcl::char-ci>? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] > [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
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

proc ::constcl::char-ci<=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] <= [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
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

proc ::constcl::char-ci>=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] >= [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
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

MD(
**char-alphabetic**

**char-numeric**

**char-whitespace**

**char-upper-case**

**char-lower-case**

The predicates `char-alphabetic`, `char-numeric`, `char-whitespace`,
`char-upper-case`, and `char-lower-case` test a character for these
conditions.
MD)

PR(
char-alphabetic?, char-numeric?, char-whitespace?, char-upper-case?, char-lower-case? (public);char char -> bool
PR)

CB
reg char-alphabetic? ::constcl::char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char alphabetic?]
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
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char numeric?]
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
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char whitespace?]
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
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char upper-case?]
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
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char lower-case?]
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

MD(
**char->integer**

**integer->char**

`char->integer` and `integer->char` convert between characters and their
16-bit numeric codes.
MD)

PR(
char->integer (public);char char -> int
PR)

MD(
Example:

```
(char->integer #\A)   =>  65
```
MD)

CB
reg char->integer

proc ::constcl::char->integer {char} {
    return [MkNumber [scan [$char char] %c]]
}
CB

PR(
integer->char (public);int int -> char
PR)

MD(
Example:

```
(integer->char 97)   =>  #\a
```
MD)

CB
reg integer->char

proc ::constcl::integer->char {int} {
    ::if {$int == 10} {
        return [MkChar #\\newline]
    } elseif {$int == 32} {
        return [MkChar #\\space]
    } else {
        return [MkChar #\\[format %c [$int numval]]]
    }
}
CB

TT(

::tcltest::test characters-1.16 {try char-upcase?} -body {
    pep {(char->integer #\A)}
    pep {(integer->char 97)}
} -output "65\n#\\a\n"

TT)

MD(
**char-upcase**

**char-downcase**

`char-upcase` and `char-downcase` alter the case of a character.
MD)

PR(
char-upcase, char-downcase (public);char char -> char
PR)

CB
reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    ::if {[::string is alpha -strict [$char char]]} {
        return [MkChar [::string toupper [$char value]]]
    } else {
        return $char
    }
}
CB

TT(

::tcltest::test characters-1.17 {try char-upcase} -body {
    pep {(char-upcase #\A)}
    pep {(char-upcase #\a)}
    pep {(char-upcase #\space)}
} -output "#\\A\n#\\A\n#\\space\n"

TT)


CB
reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    ::if {[::string is alpha -strict [$char char]]} {
        return [MkChar [::string tolower [$char value]]]
    } else {
        return $char
    }
}
CB

TT(

::tcltest::test characters-1.18 {try char-downcase?} -body {
    pep {(char-downcase #\A)}
    pep {(char-downcase #\a)}
    pep {(char-downcase #\space)}
} -output "#\\a\n#\\a\n#\\space\n"

TT)

# vim: ft=tcl tw=80
