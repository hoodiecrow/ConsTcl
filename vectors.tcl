
MD(
## Vectors
MD)

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
    method write {} {puts -nonewline #($value)}
    method show {} {return #($value)}
}

proc ::constcl::MkVector {v} {
    foreach instance [info class instances Vector] {
        if {[$instance value] eq $v} {
            return $instance
        }
    }
    return [Vector create Mem[incr ::M] $v]
}
CB

CB
reg vector? ::constcl::vector?

proc ::constcl::vector? {obj} {
    if {[info object isa typeof $obj Vector]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Vector]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {args} {
    # TODO
}
CB

CB
reg vector ::constcl::vector

proc ::constcl::vector {args} {
    # TODO
}
CB

CB
reg vector-length ::constcl::vector-length

proc ::constcl::vector-length {vec} {
    if {[::constcl::vector? $vec] eq "#t"} {
        return [MkNumber [$str length]]]
    } else {
        error "VECTOR expected\n(vector-length [$vec show])"
    }
}
CB

CB
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str ref $k]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec show] [$k show])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec show] [$k show])"
    }
}
CB


CB
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k obj} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str set! $k $obj]]
        } else {
            error "NUMBER expected\n(vector-set! [$vec show] [$k show] [$obj show])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec show] [$k show] [$obj show])"
    }
}
CB

CB
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
    # TODO
}
CB

CB
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
    # TODO
}
CB

CB
reg vector-fill ::constcl::vector-fill

proc ::constcl::vector-fill {vec fill} {
    if {[::constcl::vector? $vec] eq "#t"} {
        $vec fill $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec show] [$fill show])"
    }
}
CB

