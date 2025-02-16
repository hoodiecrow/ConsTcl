
MD(
### Booleans

Booleans are logic values, either true (`#t`) or false (`#f`).
All predicates (procedures whose name end with -?) return
boolean values. The conditional `if` operator considers all
values except for `#f` to be true.

__Boolean__ class
MD)

CB
oo::class create ::constcl::Boolean {
  superclass ::constcl::NIL
  variable bvalue
  constructor {v} {
    if {$v ni {#t #f}} {
      ::error "bad boolean value $v"
    }
    set bvalue $v
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method bvalue {} {
    set bvalue
  }
  method value {} {
    set bvalue
  }
  method write {handle} {
    puts -nonewline $handle [my bvalue]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set bvalue
  }
}

proc ::constcl::MkBoolean {v} {
  foreach instance [info class instances \
    ::constcl::Boolean] {
    if {[$instance bvalue] eq $v} {
      return $instance
    }
  }
  return [::constcl::Boolean new $v]
}
CB

TT(

::tcltest::test boolean-1.0 {evaluate boolean values} -body {
        pew "#t"
} -output "#t\n"

::tcltest::test boolean-1.1 {evaluate boolean values} -body {
        pew "#f"
} -output "#f\n"

::tcltest::test boolean-1.2 {evaluate boolean values} -body {
        pew "'#f"
} -output "#f\n"

TT)

MD(
__boolean?__

The `boolean?` predicate recognizes a Boolean by type.
MD)

PR(
boolean? (public);val val -> bool
PR)

CB
reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {val} {
  return [typeof? $val Boolean]
}
CB

TT(

::tcltest::test boolean-2.0 {evaluate boolean values} -body {
        pew "(boolean? #f)"
} -output "#t\n"

::tcltest::test boolean-2.1 {evaluate boolean values} -body {
        pew "(boolean? 0)"
} -output "#f\n"

::tcltest::test boolean-2.2 {evaluate boolean values} -body {
        pew "(boolean? '())"
} -output "#f\n"

TT)

MD(
__not__

The only operation on booleans: `not`, or logical negation.
MD)

PR(
not (public);val val -> bool
PR)

MD(
Example:

```
(not #f)    ⇒  #t   ; #f yields #t, all others #f
(not nil)   ⇒  #f   ; see?
```
MD)

CB
reg not ::constcl::not

proc ::constcl::not {val} {
  if {[$val bvalue] eq "#f"} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test boolean-3.0 {not procedure} -body {
        pew "(not #t)"
} -output "#f\n"

::tcltest::test boolean-3.1 {not procedure} -body {
        pew "(not 3)"
} -output "#f\n"

::tcltest::test boolean-3.2 {not procedure} -body {
        pew "(not (list 3))"
} -output "#f\n"

::tcltest::test boolean-3.3 {not procedure} -body {
        pew "(not #f)"
} -output "#t\n"

::tcltest::test boolean-3.4 {not procedure} -body {
        pew "(not '())"
} -output "#f\n"

::tcltest::test boolean-3.5 {not procedure} -body {
        pew "(not (list))"
} -output "#f\n"

::tcltest::test boolean-3.6 {not procedure} -body {
        pew "(not 'nil)"
} -output "#f\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
