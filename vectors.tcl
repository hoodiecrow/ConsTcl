
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
    method write {} {return #($value)}
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
proc ::constcl::make-vector {args} {
    # TODO
}
CB

CB
proc ::constcl::vector {args} {
    # TODO
}
CB

CB
proc ::constcl::vector-length {vec} {
    if {[::constcl::vector? $vec] eq "#t"} {
        return [MkNumber [$str length]]]
    } else {
        error "VECTOR expected\n(vector-length [$vec write])"
    }
}
CB

CB
proc ::constcl::vector-ref {vec k} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str ref $k]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec write] [$k write])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec write] [$k write])"
    }
}
CB


CB
proc ::constcl::vector-set! {vec k obj} {
    if {[::constcl::vector? $vec] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            return [$str set! $k $obj]]
        } else {
            error "NUMBER expected\n(vector-set! [$vec write] [$k write] [$obj write])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec write] [$k write] [$obj write])"
    }
}
CB

CB
proc ::constcl::vector->list {vec} {
    # TODO
}
CB

CB
proc ::constcl::list->vector {list} {
    # TODO
}
CB

CB
proc ::constcl::vector-fill {vec fill} {
    if {[::constcl::vector? $vec] eq "#t"} {
        $vec fill $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec write] [$fill write])"
    }
}
CB

