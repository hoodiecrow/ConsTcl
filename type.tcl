
CB
namespace eval ::constcl {}
CB

CB
oo::class create NIL {
    constructor {} {}
    method truth {} {return #t}
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method numval {} {throw "Not a number"}
    method write {} {puts -nonewline "()"}
}
CB

CB
proc ::constcl::null? {obj} {
    if {[info object isa typeof $obj NIL]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] NIL]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
oo::class create EndOfFile {}
CB

CB
proc ::eof-object? {obj} {
    if {[info object isa typeof $obj EndOfFile]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] EndOfFile]} {
        return #t
    } else {
        return #f
    }
}
CB

