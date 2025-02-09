MD(
## Built-in procedures

### Equivalence predicates

**eq**

**eqv**

**equal**

Of the three equivalence predicates, `eq` generally tests for identity (with
exception for numbers), `eqv` tests for value equality (except for booleans and
procedures, where it tests for identity), and `equal` tests for whether the
output strings are equal.
MD)

PR(
eq?, eqv?, equal? (public);val1 val val2 val -> bool
PR)

CB
reg eq? ::constcl::eq?

proc ::constcl::eq? {val1 val2} {
    ::if {[boolean? $val1] ne "#f" && [boolean? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] ne "#f" && [symbol? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[number? $val1] ne "#f" && [number? $val2] ne "#f" && [$val1 numval] eq [$val2 numval]} {
        return #t
    } elseif {[char? $val1] ne "#f" && [char? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[null? $val1] ne "#f" && [null? $val2] ne "#f"} {
        return #t
    } elseif {[pair? $val1] ne "#f" && [pair? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[string? $val1] ne "#f" && [string? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[vector? $val1] ne "#f" && [vector? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[procedure? $val1] ne "#f" && [procedure? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {val1 val2} {
    ::if {[boolean? $val1] ne "#f" && [boolean? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] ne "#f" && [symbol? $val2] ne "#f" && [$val1 name] eq [$val2 name]} {
        return #t
    } elseif {[number? $val1] ne "#f" && [number? $val2] ne "#f" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[char? $val1] ne "#f" && [char? $val2] ne "#f" && [$val1 char] eq [$val2 char]} {
        return #t
    } elseif {[null? $val1] ne "#f" && [null? $val2] ne "#f"} {
        return #t
    } elseif {[pair? $val1] ne "#f" && [pair? $val2] ne "#f" && [$val1 car] eq [$val2 car] && [$val1 cdr] eq [$val2 cdr]} {
        return #t
    } elseif {[string? $val1] ne "#f" && [string? $val2] ne "#f" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[vector? $val1] ne "#f" && [vector? $val2] ne "#f" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[procedure? $val1] ne "#f" && [procedure? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}
CB

CB
reg equal? ::constcl::equal?

proc ::constcl::equal? {val1 val2} {
    ::if {[$val1 show] eq [$val2 show]} {
        return #t
    } else {
        return #f
    }
    # TODO
}
CB

# vim: ft=tcl tw=80
