
MD(
## Strings
MD)

CB
oo::class create String {
    superclass NIL
    variable s
    constructor {v} {
        set s -1
        for {set i 0} {$i < $::S} {incr i} {
            if {[::string equal [lindex $::StrSto $i] $v]} {
                set s $i
            }
        }
        if {$s == -1} {
            set s $::S
            lset ::StrSto $s $v
            incr ::S
        }
    }
    method index {} {set s}
    method = {str} {string equal [lindex $::StrSto $s] $str}
    method length {} {string length [lindex $::StrSto $s]}
    method ref {i} {string index [lindex $::StrSto $s] $i}
    method value {} {return [lindex $::StrSto $s]}
    method write {} { puts -nonewline "\"[lindex $::StrSto $s]\"" }
}

proc ::constcl::MkString {v} {
    return [String create Mem[incr ::M] $v]
}
CB

CB
proc ::constcl::string? {obj} {
    if {[info object isa typeof $obj String]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] String]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
proc ::constcl::make-string {args} {
    # TODO
}
CB

CB
proc ::constcl::string {args} {
    set str {}
    foreach char $args {
        if {[::constcl::char? $char] eq "#t"} {
            append str [$char char]
        } else {
            error "CHAR expected\n(string [$char write])"
        }
    }
    return [MkString $str]
}
CB

CB
proc ::constcl::string-length {str} {
    if {[::constcl::str? $String] eq "#t"} {
        return [MkNumber [$str length]]
    } else {
        error "STRING expected\n(string-length [$str write])"
    }
}
CB

CB
proc ::constcl::string-ref {str k} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-ref [$str write] [$k write])"
        }
        return [$str ref $i]
    } else {
        error "STRING expected\n(string-ref [$str write] [$k write])"
    }
}
CB

CB
proc ::constcl::string-set! {str k char} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-set! [$str write] [$k write] [$char write])"
        }
        if {[::constcl::char? $char] eq "#t"} {
            return [$str set! $i [$char char]]
        } else {
            error "CHAR expected\n(string-set! [$str write] [$k write] [$char write])"
        }
    } else {
        error "STRING expected\n(string-set! [$str write] [$k write] [$char write])"
    }
}
CB

CB
proc ::constcl::string=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] eq [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string=? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string-ci=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] eq [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci=? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string<? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] < [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string-ci<? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] < [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string>? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] > [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string-ci>? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] > [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] <= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<=? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string-ci<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] <= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<=? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] >= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>=? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::string-ci>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "t" && [::constcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] >= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>=? [$s1 write] [$s2 write])"
    }
}
CB

CB
proc ::constcl::substring {str start end} {
    if {[::constcl::string? $str] eq "t"} {
        if {[::constcl::number? $start] eq "t" && [::constcl::number? $end] eq "t"} {
            return [MkString [$str substring [$start value] [$end value]]]
        } else {
            error "NUMBER expected\n(substring [$str write] [$start write] [$end write])"
        }
    } else {
        error "STRING expected\n(substring [$str write] [$start write] [$end write])"
    }
}
CB

CB
proc ::constcl::string-append {args} {
    # TODO
}
CB

CB
proc ::constcl::string->list {str} {
    # TODO
}
CB

CB
proc ::constcl::list->string {list} {
    # TODO
}
CB

CB
proc ::constcl::string-copy {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkString [$str value]]
    } else {
        error "STRING expected\n(string-copy [$str write])"
    }
}
CB

CB
proc ::constcl::string-fill! {str char} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkString [$str fill [$char value]]]
    } else {
        error "STRING expected\n(string-fill [$str write] [$char write])"
    }
}
CB

