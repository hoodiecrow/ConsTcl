
MD(
## Booleans
MD)

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
    method write {} {
        puts -nonewline [my truth]
    }
}
CB

TT(

::tcltest::test boolean-1.0 {evaluate boolean values} -body {
    namespace eval ::constcl {
        set ::inputstr "#t"
        write [eval [read]]
    }
} -output "#t\n"

::tcltest::test boolean-1.1 {evaluate boolean values} -body {
    namespace eval ::constcl {
        set ::inputstr "#f"
        write [eval [read]]
    }
} -output "#f\n"

::tcltest::test boolean-1.2 {evaluate boolean values} -body {
    namespace eval ::constcl {
        set ::inputstr "'#f"
        write [eval [read]]
    }
} -output "#f\n"

TT)

CB
# 
proc ::constcl::boolean? {obj} {
    if {[info object isa typeof $obj Boolean]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Boolean]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg not ::constcl::not

proc ::constcl::not {obj} {
    if {[$obj truth] eq "#f"} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test boolean-3.0 {not procedure} -body {
    namespace eval ::constcl {
        set ::inputstr "(not #t)"
        write [eval [read]]
    }
} -output "#f\n"

::tcltest::test boolean-3.1 {not procedure} -body {
    namespace eval ::constcl {
        set ::inputstr "(not 3)"
        write [eval [read]]
    }
} -output "#f\n"

::tcltest::test boolean-3.2 {not procedure} -body {
    namespace eval ::constcl {
        set ::inputstr "(not (list 3))"
        write [eval [read]]
    }
} -output "#f\n"

::tcltest::test boolean-3.3 {not procedure} -body {
    namespace eval ::constcl {
        set ::inputstr "(not #f)"
        write [eval [read]]
    }
} -output "#t\n"

::tcltest::test boolean-3.4 {not procedure} -body {
    namespace eval ::constcl {
        set ::inputstr "(not '())"
        write [eval [read]]
    }
} -output "#f\n"

::tcltest::test boolean-3.5 {not procedure} -body {
    namespace eval ::constcl {
        set ::inputstr "(not (list))"
        #write [eval [read]]
        write [read]
    }
} -output "#f\n"

::tcltest::test boolean-3.6 {not procedure} -body {
    namespace eval ::constcl {
        set ::inputstr "(not 'nil)"
        write [eval [read]]
    }
} -output "#f\n"

TT)

