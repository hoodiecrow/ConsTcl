
if {![info exist M]} {set M 0} # memory cell number

oo::class create NIL {
    variable truth
    constructor {} {
        set truth Mem1
    }
    method truth {} {
        set truth
    }
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method display {} {return "()"}
}

oo::class create Cons {
    variable truth car cdr
    constructor {a d} {
        set truth Mem1
        set car $a
        set cdr $d
    }
    method truth {} {
        set truth
    }
    method car {} {
        set car
    }
    method cdr {} {
        set cdr
    }
    method set-car! {val} {
        set car $val
    }
    method set-cdr! {val} {
        set cdr $val
    }
    method display {} {
        return "([$car display] . [$cdr display])"
    }
}

oo::class create Boolean {
    superclass NIL
    variable truth
    constructor {v} {
        set truth $v
    }
    method display {} {return $truth}
}

oo::class create Number {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method value {} {
        set value
    }
    method display {} {return $value}
}

oo::class create Symbol {
    superclass NIL
    variable name
    constructor {n} {
        # TODO idcheck this
        set name $n
    }
    method display {} {return $name}
}

oo::class create String {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method length {} {string length $value}
    method ref {i} {string index $value $i}
    method value {} {return $value}
    method display {} {return "\"$value\""}
}

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
        } elseif {[regexp {^#\\([a-z]+)$} [my value] -> char_name]} {
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
    method upper-case? {
        if {[string is upper [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case?
        if {[string is lower [$char char]]} {
            return #t
        } else {
            return #f
        }
    }
    method value {} {return $value}
    method display {} {return "\"$value\""}
}

NIL create Mem0

interp alias {} #t {} Mem1
Boolean create Mem[incr ::M] #t

interp alias {} #f {} Mem2
Boolean create Mem[incr ::M] #f

proc ::thtcl::cons {car cdr} {
    Cons create Mem[incr ::M] $car $cdr
}

proc ::thtcl::car {obj} {
    $obj car
}

proc ::thtcl::cdr {obj} {
    $obj cdr
}

proc ::thtcl::set-car! {obj val} {
    $obj set-car! $val
}

proc ::thtcl::set-cdr! {obj val} {
    $obj set-cdr! $val
}

proc ::thtcl::list {args} {
    set prev C0
    foreach obj [lreverse $args] {
        set prev [::thtcl::cons $obj $prev]
    }
    return $prev
}

proc ::thtcl::pair? {obj} {
    if {[info object isa typeof $obj Cons]} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::null? {obj} {
    if {[info object isa typeof $obj NIL]} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::list? {obj} {
    # TODO need to work on this a bit more
    if {[info object isa typeof $obj Cons] || $obj eq "Mem0"} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::boolean? {obj} {
    if {[info object isa typeof $obj Boolean]} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::number? {obj} {
    if {[info object isa typeof $obj Number]} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::symbol? {obj} {
    if {[info object isa typeof $obj Symbol]} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::char? {obj} {
    if {[info object isa typeof $obj Char]} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::string? {obj} {
    if {[info object isa typeof $obj String]} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::zero? {obj} {
    if {[info object isa typeof $obj Number]} {
        if {[$obj value] == 0} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(zero? [$obj display])"
    }
}

proc ::thtcl::not {obj} {
    if {[$obj truth] eq "#f"} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::length {obj} {
    if {$obj eq "Mem0"} {
        return 0
    } elseif {[info object isa typeof $obj Cons]} {
        if {[info object isa typeof [cdr $obj] Cons]} {
            expr {1 + [::thtcl::length [cdr $obj]]}
        } else {
            error "Ill-formed procedure call"
        }
    } else {
        error "LIST expected\n(length [$obj display])"
    }
}

proc ::thtcl::append {args} {
    # TODO
}

proc ::thtcl::reverse {obj} {
    # TODO
}

proc ::thtcl::list-tail {obj k} {
    if {[::thtcl::zero? $k]} {
        return $obj
    } else {
        ::thtcl::list-tail [::thtcl::cdr $obj] [::thtcl::- $k 1]
    }
}

proc ::thtcl::- {args} {
    if {[llength $args] == 0} {
        error "expected arguments"
    } elseif {[llength $args] == 1} {
        set obj [lindex $args 0]
        if {[::thtcl::number? $obj eq "#t"]} {
            return [Number create Mem[incr ::M] -[$obj value]]
        } elseif {[string is double $obj]} {
            return [Number create Mem[incr ::M] -$obj]
        } else {
            error "NUMBER expected\n(- [$obj display])"
        }
    } else {
        set obj [lindex $args 0]
        if {[::thtcl::number? $obj eq "#t"]} {
            set num [$obj value]
        } elseif {[string is double $obj]} {
            set num $obj
        } else {
            error "NUMBER expected\n(- [$obj display])"
        }
        foreach obj [lrange $args 1 end] {
            if {[::thtcl::number? $obj eq "#t"]} {
                incr num -[$obj value]
            } elseif {[string is double $obj]} {
                incr num -$obj
            } else {
                error "NUMBER expected\n(- [$obj display])"
            }
        }
        return [Number create Mem[incr ::M] $num]
    }
}

proc ::thtcl::list-ref {obj k} {
    ::thtcl::car [::thtcl::list-tail $obj $k]
}

proc ::thtcl::memq {obj1 obj2} {
    if {[::thtcl::list? $obj2] eq "#t"} {
        if {[::thtcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::thtcl::pair? $obj2] eq "#t"} {
            if {[::thtcl::eq? $obj1 [::thtcl::car $obj2]]} {
                return $obj2
            } else {
                return [::thtcl::memq $obj1 [::thtcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 display] [$obj2 display])"
    }
}

proc ::thtcl::eq? {obj1 obj2} {
    # TODO
    if {$obj1 eq $obj2} {
        return #t
    } else {
        return #f
    }
}

proc ::thtcl::memv {obj1 obj2} {
    if {[::thtcl::list? $obj2] eq "#t"} {
        if {[::thtcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::thtcl::pair? $obj2] eq "#t"} {
            if {[::thtcl::eqv? $obj1 [::thtcl::car $obj2]]} {
                return $obj2
            } else {
                return [::thtcl::memq $obj1 [::thtcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 display] [$obj2 display])"
    }
}

proc ::thtcl::eqv? {obj1 obj2} {
    if {[::thtcl::eq? $obj1 $obj2]} {
        return #t
    } elseif {[::thtcl::symbol? $obj1] && [::thtcl::symbol? $obj2]} {
        if {[$obj1 display] eq [$obj2 display]} {
            return #t
        } else {
            return #f
        }
} else {
        # TODO
    }
}

proc ::thtcl::member {obj1 obj2} {
    if {[::thtcl::list? $obj2] eq "#t"} {
        if {[::thtcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::thtcl::pair? $obj2] eq "#t"} {
            if {[::thtcl::equal? $obj1 [::thtcl::car $obj2]]} {
                return $obj2
            } else {
                return [::thtcl::memq $obj1 [::thtcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 display] [$obj2 display])"
    }
}

proc ::thtcl::equal? {obj1 obj2} {
    if {[::thtcl::eqv? $obj1 $obj2]} {
        return #t
    } else {
        # TODO
    }
}

proc ::thtcl::assq {obj1 obj2} {
    if {[::thtcl::list? $obj2] eq "#t"} {
        if {[::thtcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::thtcl::pair? $obj2] eq "#t"} {
            #TODO replace with a-list handling code
            if {[::thtcl::eq? $obj1 [::thtcl::car $obj2]]} {
                return $obj2
            } else {
                return [::thtcl::memq $obj1 [::thtcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 display] [$obj2 display])"
    }
}


proc ::thtcl::assv {obj1 obj2} {
    if {[::thtcl::list? $obj2] eq "#t"} {
        if {[::thtcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::thtcl::pair? $obj2] eq "#t"} {
            #TODO replace with a-list handling code
            if {[::thtcl::eqv? $obj1 [::thtcl::car $obj2]]} {
                return $obj2
            } else {
                return [::thtcl::memq $obj1 [::thtcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 display] [$obj2 display])"
    }
}

proc ::thtcl::assoc {obj1 obj2} {
    if {[::thtcl::list? $obj2] eq "#t"} {
        if {[::thtcl::null? $obj2] eq "#t"} {
            return #f
        } elseif {[::thtcl::pair? $obj2] eq "#t"} {
            #TODO replace with a-list handling code
            if {[::thtcl::equal? $obj1 [::thtcl::car $obj2]]} {
                return $obj2
            } else {
                return [::thtcl::memq $obj1 [::thtcl::cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 display] [$obj2 display])"
    }
}

proc ::thtcl::symbol->string {obj} {
    if {[::thtcl::symbol? $obj] eq "#t"} {
        return [$obj display]
    } else {
        error "SYMBOL expected\n(symbol->string [$obj display])"
    }
}

proc ::thtcl::string->symbol {str} {
    if {[::thtcl::string? $str] eq "#t"} {
        return [Symbol create Mem[incr ::M] [$str value]]
    } else {
        error "STRING expected\n(string->symbol [$obj display])"
    }
}

proc ::thtcl::char=? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[$c1 char] eq [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char<? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[$c1 char] < [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char>? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[$c1 char] > [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char<=? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[$c1 char] <= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char>=? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[$c1 char] >= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char-ci=? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] eq [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char-ci<? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] < [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char-ci>? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] > [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char-ci<=? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] <= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char-ci>=? {c1 c2} {
    if {[::thtcl::char? $c1] eq "t" && [::thtcl::char? $c2] eq "#t"} {
        if {[string tolower [$c1 char]] >= [string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 display] [$c2 display])"
    }
}

proc ::thtcl::char-alphabetic? {char} {
    if {[::thtcl::char? $char] eq "#t"} {
        return [$char alphabetic?]
    } else {
        error "CHAR expected\n(char-alphabetic? [$char display])"
    }
}

proc ::thtcl::char-numeric? {char} {
    if {[::thtcl::char? $char] eq "#t"} {
        return [$char numeric?]
    } else {
        error "CHAR expected\n(char-numeric? [$char display])"
    }
}

proc ::thtcl::char-whitespace? {char} {
    if {[::thtcl::char? $char] eq "#t"} {
        return [$char whitespace?]
    } else {
        error "CHAR expected\n(char-whitespace? [$char display])"
    }
}

proc ::thtcl::char-upper-case? {letter} {
    if {[::thtcl::char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char display])"
    }
}

proc ::thtcl::char-lower-case? {letter} {
    if {[::thtcl::char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char display])"
    }
}

proc ::thtcl::char->integer {char} {
    # TODO
}

proc ::thtcl::integer->char {n} {
    # TODO
}

proc ::thtcl::char-upcase {char} {
    if {[::thtcl::char? $char] eq "#t"} {
        return [Char create Mem[incr ::M] [string toupper [$char char]]]
    } else {
        error "CHAR expected\n(char-upcase [$char display])"
    }
}


proc ::thtcl::char-downcase {char} {
    if {[::thtcl::char? $char] eq "#t"} {
        return [Char create Mem[incr ::M] [string tolower [$char char]]]
    } else {
        error "CHAR expected\n(char-downcase [$char display])"
    }
}

proc ::thtcl::make-string {args} {
    # TODO
}

proc ::thtcl::string {args} {
    set str {}
    foreach char $args {
        if {[::thtcl::char? $char] eq "#t"} {
            append str [$char char]
        } else {
            error "CHAR expected\n(string [$char display])"
        }
    }
    return [String create Mem[incr ::M] $str]
}

proc ::thtcl::string-length {str} {
    if {[::thtcl::str? $String] eq "#t"} {
        return [$str length]
    } else {
        error "STRING expected\n(string-length [$str display])"
    }
}

proc ::thtcl::string-ref {str k} {
    if {[::thtcl::string? $str] eq "#t"} {
        if {[::thtcl::number? $k] eq "#t"} {
            set i [$k value]
        } elseif {[string is double $k]} {
            set i $k
        } else {
            error "Exact INTEGER expected\n(string-ref [$str display] [$k display])"
        }
        return [$str ref $i]
    } else {
        error "STRING expected\n(string-ref [$str display] [$k display])"
    }
}

proc ::thtcl::string-set! {str k char} {
    if {[::thtcl::string? $str] eq "#t"} {
        if {[::thtcl::number? $k] eq "#t"} {
            set i [$k value]
        } elseif {[string is double $k]} {
            set i $k
        } else {
            error "Exact INTEGER expected\n(string-set! [$str display] [$k display] [$char display])"
        }
        if {[::thtcl::char? $char] eq "#t"} {
            return [$str set! $i [$char char]]
        } else {
            error "CHAR expected\n(string-set! [$str display] [$k display] [$char display])"
        }
    } else {
        error "STRING expected\n(string-set! [$str display] [$k display] [$char display])"
    }
}

proc ::thtcl::string=? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[$s1 value] eq [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string=? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string-ci=? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] eq [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci=? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string<? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[$s1 value] < [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string-ci<? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] < [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string>? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[$s1 value] > [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string-ci>? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] > [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string<=? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[$s1 value] <= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<=? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string-ci<=? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] <= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<=? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string>=? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[$s1 value] >= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>=? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::string-ci>=? {s1 s2} {
    if {[::thtcl::string? $s1] eq "t" && [::thtcl::string? $s2] eq "#t"} {
        if {[string tolower [$s1 value]] >= [string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>=? [$s1 display] [$s2 display])"
    }
}

proc ::thtcl::substring {str start end} {
    if {[::thtcl::string? $str] eq "t"} {
        if {[::thtcl::number? $start] eq "t" && [::thtcl::number? $end] eq "t"} {
            return {String create Mem[incr ::M] [$str substring [$start value] [$end value]]
        } else {
            error "NUMBER expected\n(substring [$str display] [$start display] [$end display])"
        }
    } else {
        error "STRING expected\n(substring [$str display] [$start display] [$end display])"
    }
}

proc ::thtcl::string-append {args} {
    # TODO
}

proc ::thtcl::string->list {str} {
    # TODO
}

proc ::thtcl::list->string {list} {
    # TODO
}

proc ::thtcl::string-copy {str} {
    if {[::thtcl::string? $str] eq "#t"} {
        return [String create Mem[incr ::M] [$str value]]
    } else {
        error "STRING expected\n(string-copy [$str display])"
    }
}

