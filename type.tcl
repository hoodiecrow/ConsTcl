
CB
oo::class create NIL {
    constructor {} {}
    method truth {} {return #t}
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method numval {} {throw "Not a number"}
    method write {} {return "()"}
}
CB

CB
proc ::constcl::null? {obj} {
    if {[info object isa typeof $obj NIL]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create EndOfFile {}

CB
proc ::eof-object? {obj} {
    if {[info object isa typeof $obj EndOfFile]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create Cons {
    variable car cdr
    constructor {a d} {
        set truth Mem1
        set car $a
        set cdr $d
    }
    method truth {} {return #t}
    method numval {} {throw "Not a number"}
    method car {} { set car }
    method cdr {} { set cdr }
    method set-car! {val} { set car $val }
    method set-cdr! {val} { set cdr $val }
    method write {} { return "([$car write] . [$cdr write])" }
}
CB

CB
proc ::constcl::pair? {obj} {
    if {[info object isa typeof $obj Cons]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create Boolean {
    superclass NIL
    variable truth
    constructor {v} {
        set truth $v
    }
    method truth {} {
        set truth
    }
    method write {} {return $truth}
}
CB

CB
proc ::constcl::boolean? {obj} {
    if {[info object isa typeof $obj Boolean]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create Number {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method = {num} {expr {$value == $num}}
    method positive {} {expr {$value > 0}}
    method negative {} {expr {$value < 0}}
    method even {} {expr {$value % 2 == 0}}
    method odd {} {expr {$value % 2 == 1}}
    method 1+ {} {incr value}
    method incr {val} {incr value $val}
    method mult {val} {set value [expr {$value * $val}]}
    method decr {val} {incr value -$val}
    method div {val} {set value [expr {$value / $val}]}
    method value {} { set value }
    method numval {} {set value}
    method write {} {return $value}
}
CB

CB
proc ::constcl::number? {obj} {
    if {[info object isa typeof $obj Number]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create Symbol {
    superclass NIL
    variable name
    constructor {n} {
        # TODO idcheck this
        set name $n
    }
    method name {} {set name}
    method = {symname} {expr {$name eq $symname}}
    method write {} {return $name}
}
CB

CB
proc ::constcl::symbol? {obj} {
    if {[info object isa typeof $obj Symbol]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create String {
    superclass NIL
    variable s
    constructor {v} {
        set s $::S
        lset ::StrSto $s $v
        incr ::S
    }
    method index {} {set s}
    method = {str} {string equal [lindex $::StrSto $s] $str}
    method length {} {string length [lindex $::StrSto $s]}
    method ref {i} {string index [lindex $::StrSto $s] $i}
    method value {} {return [lindex $::StrSto $s]}
    method write {} {return "\"[lindex $::StrSto $s]\""}
}
CB

CB
proc ::constcl::string? {obj} {
    if {[info object isa typeof $obj String]} {
        return #t
    } else {
        return #f
    }
}
CB

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
    method write {} {return "\"$value\""}
}
CB

CB
proc ::constcl::char? {obj} {
    if {[info object isa typeof $obj Char]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create Vector {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method length {} {string length $value}
    method ref {i} {string index $value $i}
    method value {} {return $value}
    method write {} {return #($value)}
}
CB

CB
proc ::constcl::vector? {obj} {
    if {[info object isa typeof $obj Vector]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create Procedure {
    superclass NIL
    variable value
    constructor {v} {
        set value $v
    }
    method value {} {
        set value
    }
    method write {} {return $value}
    method call {vals} {
        # TODO
    }
}
CB

CB
proc ::constcl::procedure? {obj} {
    if {[info object isa typeof $obj Procedure]} {
        return #t
    } else {
        return #f
    }
}
CB

