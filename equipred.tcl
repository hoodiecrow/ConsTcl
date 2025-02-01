MD(
## Built-in procedures

### Equivalence predicates

Of the three equivalence predicates, `eq` generally tests for identity (with exceptions for numbers
and strings), `eqv` tests for value equality (except for booleans and procedures, where it tests for
identity), and `equal` tests for whether the output strings are equal.
MD)

PR(
eq?, eqv?, equal? (public);val1 val val2 val -> bool
PR)

CB
reg eq? ::constcl::eq?

proc ::constcl::eq? {val1 val2} {
    if {[boolean? $val1] eq "#t" && [boolean? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] eq "#t" && [symbol? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[number? $val1] eq "#t" && [number? $val2] eq "#t" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[char? $val1] eq "#t" && [char? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[null? $val1] eq "#t" && [null? $val2] eq "#t"} {
        return #t
    } elseif {[pair? $val1] eq "#t" && [pair? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[string? $val1] eq "#t" && [string? $val2] eq "#t" && [$val1 index] eq [$val2 index]} {
        return #t
    } elseif {[vector? $val1] eq "#t" && [vector? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[procedure? $val1] eq "#t" && [procedure? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {val1 val2} {
    if {[boolean? $val1] eq "#t" && [boolean? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] eq "#t" && [symbol? $val2] eq "#t" && [$val1 name] eq [$val2 name]} {
        return #t
    } elseif {[number? $val1] eq "#t" && [number? $val2] eq "#t" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[char? $val1] eq "#t" && [char? $val2] eq "#t" && [$val1 char] eq [$val2 char]} {
        return #t
    } elseif {[null? $val1] eq "#t" && [null? $val2] eq "#t"} {
        return #t
    } elseif {[pair? $val1] eq "#t" && [pair? $val2] eq "#t" && [$val1 car] eq [$val2 car] && [$val1 cdr] eq [$val2 cdr]} {
        return #t
    } elseif {[string? $val1] eq "#t" && [string? $val2] eq "#t" && [$val1 index] eq [$val2 index]} {
        return #t
    } elseif {[vector? $val1] eq "#t" && [vector? $val2] eq "#t" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[procedure? $val1] eq "#t" && [procedure? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg equal? ::constcl::equal?

proc ::constcl::equal? {val1 val2} {
    if {[$val1 show] eq [$val2 show]} {
        return #t
    } else {
        return #f
    }
    # TODO
}
CB

