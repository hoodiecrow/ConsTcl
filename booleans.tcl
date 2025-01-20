
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
proc ::constcl::not {obj} {
    if {[$obj truth] eq "#f"} {
        return #t
    } else {
        return #f
    }
}
CB

