MD(
## Built-in procedures

###Equivalence predicates
MD)

CB
reg eq? ::constcl::eq?

proc ::constcl::eq? {obj1 obj2} {
    if {[boolean? $obj1] eq "#t" && [boolean? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[symbol? $obj1] eq "#t" && [symbol? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[number? $obj1] eq "#t" && [number? $obj2] eq "#t" && [$obj1 value] eq [$obj2 value]} {
        return #t
    } elseif {[char? $obj1] eq "#t" && [char? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[null? $obj1] eq "#t" && [null? $obj2] eq "#t"} {
        return #t
    } elseif {[pair? $obj1] eq "#t" && [pair? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[string? $obj1] eq "#t" && [string? $obj2] eq "#t" && [$obj1 index] eq [$obj2 index]} {
        return #t
    } elseif {[vector? $obj1] eq "#t" && [vector? $obj2] eq "#t" && [$obj1 value] eq [$obj2 value]} {
        return #t
    } elseif {[procedure? $obj1] eq "#t" && [procedure? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {obj1 obj2} {
    if {[boolean? $obj1] eq "#t" && [boolean? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[symbol? $obj1] eq "#t" && [symbol? $obj2] eq "#t" && [$obj1 name] eq [$obj2 name]} {
        return #t
    } elseif {[number? $obj1] eq "#t" && [number? $obj2] eq "#t" && [$obj1 value] eq [$obj2 value]} {
        return #t
    } elseif {[char? $obj1] eq "#t" && [char? $obj2] eq "#t" && [$obj1 char] eq [$obj2 char]} {
        return #t
    } elseif {[null? $obj1] eq "#t" && [null? $obj2] eq "#t"} {
        return #t
    } elseif {[pair? $obj1] eq "#t" && [pair? $obj2] eq "#t" && [$obj1 car] eq [$obj2 car] && [$obj1 cdr] eq [$obj2 cdr]} {
        return #t
    } elseif {[string? $obj1] eq "#t" && [string? $obj2] eq "#t" && [$obj1 index] eq [$obj2 index]} {
        return #t
    } elseif {[vector? $obj1] eq "#t" && [vector? $obj2] eq "#t" && [$obj1 value] eq [$obj2 value]} {
        return #t
    } elseif {[procedure? $obj1] eq "#t" && [procedure? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg equal? ::constcl::equal?

proc ::constcl::equal? {obj1 obj2} {
    if {[$obj1 show] eq [$obj2 show]} {
        return #t
    } else {
        return #f
    }
    # TODO
}
CB

