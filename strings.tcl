
MD(
### Strings

Procedures for dealing with strings of characters.
MD)

CB
oo::class create ::constcl::String {
    superclass ::constcl::NIL
    variable s constant
    constructor {v} {
        set s [::constcl::find-string-index $v]
        set constant 0
    }
    method index {} {set s}
    method = {str} {::string equal [my value] $str}
    method length {} {::string length [my value]}
    method ref {i} {::string index [my value] $i}
    method set! {k c} {
        if {[my constant]} {
            error "string is constant"
        } else {
            set value [::string replace [my value] $k $k $c]
            set s [::constcl::find-string-index $value]
        }
        return [self]
    }
    method fill! {c} {
        if {[my constant]} {
            error "string is constant"
        } else {
            set value [::string repeat $c [::string length [my value]]]
            set s [::constcl::find-string-index $value]
        }
        return [self]
    }
    method substring {from to} {::string range [my value] $from $to}
    method value {} {return [lindex $::constcl::StrSto $s]}
    method mkconstant {} {set constant 1}
    method constant {} {set constant}
    method write {} { puts -nonewline "\"[my value]\"" }
    method show {} {format "\"[my value]\""}
}

interp alias {} MkString {} ::constcl::String new

reg string? ::constcl::string?

proc ::constcl::string? {obj} {
    if {[info object isa typeof $obj ::constcl::String]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::String]} {
        return #t
    } else {
        return #f
    }
}
CB

MD(
Helper function for finding a string in the string store.
MD)

CB
proc ::constcl::find-string-index {v} {
    set s -1
    for {set i 0} {$i < $::constcl::S} {incr i} {
        if {[::string equal [lindex $::constcl::StrSto $i] $v]} {
            set s $i
        }
    }
    if {$s == -1} {
        set s $::constcl::S
        lset ::constcl::StrSto $s $v
        incr ::constcl::S
    }
    set s
}
CB

TT(

::tcltest::test strings-1.0 {try string?} -body {
    pep {(string? "foo bar")}
    pep {(string? 'foo-bar)}
} -output "#t\n#f\n"

TT)

MD(
`make-string` _k_ _?c?_ creates a string of _k_ characters, optionally
filled with _c_ characters.
MD)

CB
reg make-string ::constcl::make-string

proc ::constcl::make-string {args} {
    if {[llength $args] == 1} {
        lassign $args k
        return [MkString [::string repeat " " [$k value]]]
    } else {
        lassign $args k c
        return [MkString [::string repeat [$c char] [$k value]]]
    }
}
CB

TT(

::tcltest::test strings-1.1 {try make-string} -body {
    pep {(make-string 5 #\x)}
} -output "\"xxxxx\"\n"

TT)

MD(
`string` constructs a string from a Tcl list of Lisp characters.
MD)

CB
reg string ::constcl::string

proc ::constcl::string {args} {
    set str {}
    foreach char $args {
        if {[::constcl::char? $char] eq "#t"} {
            ::append str [$char char]
        } else {
            error "CHAR expected\n(string [$char show])"
        }
    }
    return [MkString $str]
}
CB

TT(

::tcltest::test strings-1.2 {try string} -body {
    pep {(string #\f #\o #\o)}
} -output "\"foo\"\n"

TT)

MD(
`string-length` reports a string's length.
MD)

CB
reg string-length ::constcl::string-length

proc ::constcl::string-length {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkNumber [$str length]]
    } else {
        error "STRING expected\n(string-length [$str show])"
    }
}
CB

TT(

::tcltest::test strings-1.3 {try string-length} -body {
    pep {(string-length "foo bar")}
} -output "7\n"

TT)

MD(
`string-ref` _str_ _k_ yields the _k_-th character (0-based) in _str_.
MD)

CB
reg string-ref ::constcl::string-ref

proc ::constcl::string-ref {str k} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-ref [$str show] [$k show])"
        }
        return [MkChar "#\\[$str ref $i]"]
    } else {
        error "STRING expected\n(string-ref [$str show] [$k show])"
    }
}
CB

TT(

::tcltest::test strings-1.4 {try string-ref} -body {
    pep {(string-ref "foo bar" 4)}
} -output "#\\b\n"

TT)

MD(
`string-set!` _str_ _k_ _char_ replaces the character at _k_ with _char_.
MD)

CB
reg string-set! ::constcl::string-set!

proc ::constcl::string-set! {str k char} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-set! [$str show] [$k show] [$char show])"
        }
        if {[::constcl::char? $char] eq "#t"} {
            $str set! $i [$char char]
            return $str
        } else {
            error "CHAR expected\n(string-set! [$str show] [$k show] [$char show])"
        }
    } else {
        error "STRING expected\n(string-set! [$str show] [$k show] [$char show])"
    }
}
CB

TT(

::tcltest::test strings-1.5 {try string-set!} -body {
    pep {(string-set! (string #\f #\o #\o) 0 #\x)}
} -output "\"xoo\"\n"

::tcltest::test strings-1.6 {try string-set!} -body {
    pep {(define f (lambda () (make-string 3 #\*)))}
    pep {(define g (lambda () "***"))}
    pep {(string-set! (f) 0 #\?)}
} -output "\"?**\"\n"

::tcltest::test strings-1.7 {try string-set!} -body {
    pep {(string-set! (g) 0 #\?)}
} -returnCodes error -result "string is constant"

TT)

MD(
`string=?`, `string<?`, `string>?`, `string<=?`, `string>=?` and their
case insensitive variants `string-ci=?`, `string-ci<?`, `string-ci>?`,
`string-ci<=?`, `string-ci>=?` compare strings.
MD)

CB
reg string=? ::constcl::string=?

proc ::constcl::string=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] eq [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string=? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.8 {try string=?} -body {
    pep {(string=? "foo bar" "faa bor")}
    pep {(string=? "foo bar" "foo bar")}
    pep {(string=? "foo bar" "Foo bar")}
} -output "#f\n#t\n#f\n"

TT)

CB
reg string-ci=? ::constcl::string-ci=?

proc ::constcl::string-ci=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] eq [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci=? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.9 {try string-ci=?} -body {
    pep {(string-ci=? "foo bar" "faa bor")}
    pep {(string-ci=? "foo bar" "foo bar")}
    pep {(string-ci=? "foo bar" "Foo bar")}
} -output "#f\n#t\n#t\n"

TT)

CB
reg string<? ::constcl::string<?

proc ::constcl::string<? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] < [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.10 {try string<?} -body {
    pep {(string<? "bar" "car")}
    pep {(string<? "bar" "bar")}
    pep {(string<? "bar" "aar")}
} -output "#t\n#f\n#f\n"

TT)

CB
reg string-ci<? ::constcl::string-ci<?

proc ::constcl::string-ci<? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] < [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.11 {try string-ci<?} -body {
    pep {(string-ci<? "bar" "Car")}
    pep {(string-ci<? "bar" "Bar")}
    pep {(string-ci<? "bar" "Aar")}
} -output "#t\n#f\n#f\n"

TT)

CB
reg string>? ::constcl::string>?

proc ::constcl::string>? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] > [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.12 {try string>?} -body {
    pep {(string>? "bar" "car")}
    pep {(string>? "bar" "bar")}
    pep {(string>? "bar" "aar")}
} -output "#f\n#f\n#t\n"

TT)

CB
reg string-ci>? ::constcl::string-ci>?

proc ::constcl::string-ci>? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] > [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.13 {try string-ci>?} -body {
    pep {(string-ci>? "bar" "Car")}
    pep {(string-ci>? "bar" "Bar")}
    pep {(string-ci>? "bar" "Aar")}
} -output "#f\n#f\n#t\n"

TT)

CB
reg string<=? ::constcl::string<=?

proc ::constcl::string<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] <= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<=? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.14 {try string<=?} -body {
    pep {(string<=? "bar" "car")}
    pep {(string<=? "bar" "bar")}
    pep {(string<=? "bar" "aar")}
} -output "#t\n#t\n#f\n"

TT)

CB
reg string-ci<=? ::constcl::string-ci<=?

proc ::constcl::string-ci<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] <= [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<=? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.15 {try string-ci<=?} -body {
    pep {(string-ci<=? "bar" "Car")}
    pep {(string-ci<=? "bar" "Bar")}
    pep {(string-ci<=? "bar" "Aar")}
} -output "#t\n#t\n#f\n"

TT)

CB
reg string>=? ::constcl::string>=?

proc ::constcl::string>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] >= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>=? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.16 {try string>=?} -body {
    pep {(string>=? "bar" "car")}
    pep {(string>=? "bar" "bar")}
    pep {(string>=? "bar" "aar")}
} -output "#f\n#t\n#t\n"

TT)

CB
reg string-ci>=? ::constcl::string-ci>=?

proc ::constcl::string-ci>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] >= [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>=? [$s1 show] [$s2 show])"
    }
}
CB

TT(

::tcltest::test strings-1.17 {try string-ci>=?} -body {
    pep {(string-ci>=? "bar" "Car")}
    pep {(string-ci>=? "bar" "Bar")}
    pep {(string-ci>=? "bar" "Aar")}
} -output "#f\n#t\n#t\n"

TT)

MD(
`substring` _str_ _start_ _end_ yields the substring of _str_ that starts at _start_
and ends at _end_.
MD)

CB
reg substring ::constcl::substring

proc ::constcl::substring {str start end} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $start] eq "#t" && [::constcl::number? $end] eq "#t"} {
            return [MkString [$str substring [$start value] [$end value]]]
        } else {
            error "NUMBER expected\n(substring [$str show] [$start show] [$end show])"
        }
    } else {
        error "STRING expected\n(substring [$str show] [$start show] [$end show])"
    }
}
CB

TT(

::tcltest::test strings-1.18 {try substring} -body {
    pep {(substring "foo bar" 0 2)}
} -output "\"foo\"\n"

TT)

MD(
`string-append` joins strings together.
MD)

CB
reg string-append ::constcl::string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {$arg value}]]
}
CB

TT(

::tcltest::test strings-1.19 {try string-append} -body {
    pep {(string-append "foo" " bar")}
} -output "\"foo bar\"\n"

TT)

MD(
`string->list` converts a string to a Lisp list of characters.
MD)

CB
reg string->list ::constcl::string->list

proc ::constcl::string->list {str} {
    list {*}[lmap c [split [$str value] {}] {MkChar "#\\$c"}]
}
CB

TT(

::tcltest::test strings-1.20 {try string->list} -body {
    pep {(string->list "foo")}
} -output "(#\\f #\\o #\\o)\n"

TT)

MD(
`list->string` converts a Lisp list of characters to a string.
MD)

CB
reg list->string ::constcl::list->string

proc ::constcl::list->string {list} {
    MkString [::append --> {*}[lmap c [splitlist $list] {$c char}]]
}
CB

TT(

::tcltest::test strings-1.21 {try list->string} -body {
    pep {(list->string '(#\f #\o #\o))}
} -output "\"foo\"\n"

TT)

MD(
`string-copy` makes a copy of a string.
MD)

CB
reg string-copy ::constcl::string-copy

proc ::constcl::string-copy {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkString [$str value]]
    } else {
        error "STRING expected\n(string-copy [$str show])"
    }
}
CB

TT(

::tcltest::test strings-1.22 {try string-copy} -body {
    pep {(define x (string-copy "foo"))}
    pep {(string-set! x 0 #\x)}
} -output "\"xoo\"\n"

TT)

MD(
`string-fill!` _str_ _char_ fills a non-constant string with _char_.
MD)

CB
reg string-fill! ::constcl::string-fill!

proc ::constcl::string-fill! {str char} {
    if {[::constcl::string? $str] eq "#t"} {
        $str fill! [$char char]
        return $str
    } else {
        error "STRING expected\n(string-fill [$str show] [$char show])"
    }
}
CB

TT(

::tcltest::test strings-1.23 {try string-fill!} -body {
    pep {(define x (string-copy "foo"))}
    pep {(string-fill! x #\x)}
} -output "\"xxx\"\n"

TT)

