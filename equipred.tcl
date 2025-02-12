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

__eq?__
MD)

PR(
eq?, eqv?, equal? (public);val1 val val2 val -> bool
PR)

CB
reg eq?

proc ::constcl::eq? {val1 val2} {
  if {[typeeq boolean? $val1 $val2] && $val1 eq $val2} {
    return #t
  } elseif {[typeeq symbol? $val1 $val2] && $val1 eq $val2} {
    return #t
  } elseif {[typeeq number? $val1 $val2] && [valeq $val1 $val2]} {
    return #t
  } elseif {[typeeq char? $val1 $val2] && $val1 eq $val2} {
    return #t
  } elseif {[typeeq null? $val1 $val2]} {
    return #t
  } elseif {[typeeq pair? $val1 $val2] && $val1 eq $val2} {
    return #t
  } elseif {[typeeq string? $val1 $val2] && $val1 eq $val2} {
    return #t
  } elseif {[typeeq vector? $val1 $val2] && $val1 eq $val2} {
    return #t
  } elseif {[typeeq procedure? $val1 $val2] && $val1 eq $val2} {
    return #t
  } else {
    return #f
  }
}

proc ::constcl::typeeq {typep val1 val2} {
    return [expr {[$typep $val1] ne "#f" && [$typep $val2] ne "#f"}]
}

proc ::constcl::valeq {val1 val2} {
    return [expr {[$val1 value] eq [$val2 value]}]
}
CB

TT(
::tcltest::test equipred-1.0 {try comparing a boolean and a symbol} -body {
    pep "(eq? #t 'a)"
} -output "#f\n"

::tcltest::test equipred-1.1 {try comparing two booleans} -body {
    pep "(eq? #t #t)"
} -output "#t\n"
TT)

MD(
__eqv?__
MD)

CB
reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {val1 val2} {
  if {[typeeq boolean? $val1 $val2] && $val1 eq $val2} {
    return #t
  } elseif {[typeeq symbol? $val1 $val2] && [valeq $val1 $val2]} {
    return #t
  } elseif {[typeeq number? $val1 $val2] && [valeq $val1 $val2]} {
    return #t
  } elseif {[typeeq char? $val1 $val2] && [valeq $val1 eq $val2]} {
    return #t
  } elseif {[typeeq null? $val1 $val2]} {
    return #t
  } elseif {[pair? $val1] ne "#f" && [pair? $val2] ne "#f" && [$val1 car] eq [$val2 car] && [$val1 cdr] eq [$val2 cdr]} {
    return #t
  } elseif {[typeeq string? $val1 $val2] && [valeq $val1 $val2]} {
    return #t
  } elseif {[typeeq vector? $val1 $val2] && [valeq $val1 $val2]} {
    return #t
  } elseif {[typeeq procedure? $val1 $val2] && $val1 eq $val2} {
    return #t
  } else {
    return #f
  }
}
CB

MD(
__equal?__
MD)

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

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
