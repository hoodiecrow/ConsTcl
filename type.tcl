
CB
namespace eval ::constcl {
    namespace unknown resolve

    proc resolve {cmd args} {
        if {[regexp {^c([ad]{2,4})r$} $cmd -> ads]} {
            set obj [lindex $args 0]
            foreach ad [lreverse [split $ads {}]] {
                if {$ad eq "a"} {
                    set obj [car $obj]
                } else {
                    set obj [cdr $obj]
                }
            }
            return $obj
        } elseif {no} {
            uplevel 1 [dict get $scope $cmd] $args
        } else {
            return -code error "no such command: \"$cmd\""
        }
    }
}

dict set ::standard_env pi 3.1415926535897931

proc reg {sym impl} {
    dict set ::standard_env $sym $impl
}

CB

CB
# utility functions
proc ::pep {str} {
    set ::inputstr $str
    namespace eval ::constcl {
        write [eval [read]]
    }
}

proc ::pp {str} {
    set ::inputstr $str
    namespace eval ::constcl {
        write [read]
    }
}
CB

CB
catch { NIL destroy }

oo::class create NIL {
    constructor {} {}
    method truth {} {return #t}
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method numval {} {throw "Not a number"}
    method write {} {puts -nonewline "()"}
    method show {} {format "()"}
}
CB

CB
reg null? ::constcl::null?

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
catch { EndOfFile destroy }

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

