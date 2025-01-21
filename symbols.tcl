
MD(
## Symbols
MD)

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
    method write {} { puts -nonewline [my name] }
}

proc ::constcl::MkSymbol {n} {
    if {$n eq {}} {
        error "a symbol must have a name"
    }
    foreach instance [info class instances Symbol] {
        if {[$instance name] eq $n} {
            return $instance
        }
    }
    return [Symbol create Mem[incr ::M] $n]
}
CB

CB
proc ::constcl::symbol? {obj} {
    if {[info object isa typeof $obj Symbol]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Symbol]} {
        return #t
    } else {
        return #f
    }
}
CB

CB
proc ::constcl::symbol->string {symbol} {
}
CB

CB
proc ::constcl::string->symbol {string} {
}
CB

