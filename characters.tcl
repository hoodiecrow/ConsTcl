
MD(
## Characters
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
        if {[regexp {^#\\[A-Za-z]$} [my value]]} {
            return [string index [my value] 2]
        } elseif {[regexp {^#\\([[:graph:]]+)$} [my value] -> char_name]} {
            # TODO
            switch $char_name {
                space {return " "}
                newline {return "\n"}
            }
        }
    }
    method alphabetic? {} {
        if {[string is alpha [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method numeric? {} {
        if {[string is digit [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method whitespace? {} {
        if {[string is space [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method upper-case? {} {
        if {[string is upper [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case? {} {
        if {[string is lower [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method value {} {return $value}
    method write {} { puts -nonewline "#\\$value" }
}
CB

CB
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

CB
proc ::constcl::char=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] eq [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char<? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] < [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char>? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] > [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] <= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[$c1 char] >= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char-ci=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] eq [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char-ci<? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] < [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char-ci>? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] > [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char-ci<=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] <= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char-ci>=? {c1 c2} {
    if {[::constcl::char? $c1] eq "t" && [::constcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] >= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 write] [$c2 write])"
    }
}
CB

CB
proc ::constcl::char-alphabetic? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char alphabetic?]
    } else {
        error "CHAR expected\n(char-alphabetic? [$char write])"
    }
}
CB

CB
proc ::constcl::char-numeric? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char numeric?]
    } else {
        error "CHAR expected\n(char-numeric? [$char write])"
    }
}
CB

CB
proc ::constcl::char-whitespace? {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char whitespace?]
    } else {
        error "CHAR expected\n(char-whitespace? [$char write])"
    }
}
CB

CB
proc ::constcl::char-upper-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char write])"
    }
}
CB

CB
proc ::constcl::char-lower-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char write])"
    }
}
CB

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
proc ::constcl::char-upcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [Char create Mem[incr ::M] [string toupper [$char char]]]
    } else {
        error "CHAR expected\n(char-upcase [$char write])"
    }
}
CB


CB
proc ::constcl::char-downcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [Char create Mem[incr ::M] [string tolower [$char char]]]
    } else {
        error "CHAR expected\n(char-downcase [$char write])"
    }
}
CB

