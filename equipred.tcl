MD(
## Built-in procedures

### Equivalence predicates

__eq__

__eqv__

__equal__

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
  if {[teq boolean? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq symbol? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq number? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq char? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq null? $val1 $val2]} {
    return #t
  } elseif {[teq pair? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq string? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq vector? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq procedure? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } else {
    return #f
  }
}

proc ::constcl::teq {typep val1 val2} {
    return [expr {[$typep $val1] ne "#f" &&
      [$typep $val2] ne "#f"}]
}

proc ::constcl::veq {val1 val2} {
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
reg eqv?

proc ::constcl::eqv? {val1 val2} {
  if {[teq boolean? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq symbol? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq number? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq char? $val1 $val2] &&
      [veq $val1 eq $val2]} {
    return #t
  } elseif {[teq null? $val1 $val2]} {
    return #t
  } elseif {[pair? $val1] ne "#f" &&
      [pair? $val2] ne "#f" &&
      [$val1 car] eq [$val2 car] &&
      [$val1 cdr] eq [$val2 cdr]} {
    return #t
  } elseif {[teq string? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq vector? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq procedure? $val1 $val2] &&
      $val1 eq $val2} {
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
reg equal?

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
