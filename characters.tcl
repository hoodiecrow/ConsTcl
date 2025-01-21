
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

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

CB
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
CB

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

CB
reg char-upper-case? ::constcl::char-upper-case?

proc ::constcl::char-upper-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char show])"
    }
}
CB

CB
reg char-lower-case? ::constcl::char-lower-case?

proc ::constcl::char-lower-case? {letter} {
    if {[::constcl::char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char show])"
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
reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [MkChar [string toupper [$char char]]]
    } else {
        error "CHAR expected\n(char-upcase [$char show])"
    }
}
CB


CB
reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
    if {[::constcl::char? $char] eq "#t"} {
        return [MkChar [string tolower [$char char]]]
    } else {
        error "CHAR expected\n(char-downcase [$char show])"
    }
}
CB

