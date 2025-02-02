
MD(
### Booleans

Booleans are logic values, either true (`#t`) or false (`#f`).
All predicates (procedures whose name end with -?) return
boolean values. The conditional `if` operator considers all
values except for `#f` to be true.
MD)

CB
oo::class create ::constcl::Boolean {
    superclass ::constcl::NIL
    variable bvalue
    constructor {v} {
        if {$v ni {#t #f}} {
            error "bad boolean value $v"
        }
        set bvalue $v
    }
    method mkconstant {} {}
    method constant {} {return 1}
    method bvalue {} { set bvalue }
    method value {} { set bvalue }
    method write {} { puts -nonewline [my bvalue] }
    method show {} {set bvalue}
}

proc ::constcl::MkBoolean {v} {
    foreach instance [info class instances ::constcl::Boolean] {
        if {[$instance bvalue] eq $v} {
            return $instance
        }
    }
    return [::constcl::Boolean new $v]
}
CB

TT(

::tcltest::test boolean-1.0 {evaluate boolean values} -body {
        pep "#t"
} -output "#t\n"

::tcltest::test boolean-1.1 {evaluate boolean values} -body {
        pep "#f"
} -output "#f\n"

::tcltest::test boolean-1.2 {evaluate boolean values} -body {
        pep "'#f"
} -output "#f\n"

TT)

MD(
**boolean?**

The `boolean?` predicate recognizes a Boolean by type.
MD)

PR(
boolean? (public);val val -> bool
PR)

CB
reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {val} {
    if {[info object isa typeof $val ::constcl::Boolean]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Boolean]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test boolean-2.0 {evaluate boolean values} -body {
        pep "(boolean? #f)"
} -output "#t\n"

::tcltest::test boolean-2.1 {evaluate boolean values} -body {
        pep "(boolean? 0)"
} -output "#f\n"

::tcltest::test boolean-2.2 {evaluate boolean values} -body {
        pep "(boolean? '())"
} -output "#f\n"

TT)

MD(
**not**

The only operation on booleans: `not`, or logical negation.
MD)

PR(
not (public);val val -> bool
PR)

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
        pep "(not #t)"
} -output "#f\n"

::tcltest::test boolean-3.1 {not procedure} -body {
        pep "(not 3)"
} -output "#f\n"

::tcltest::test boolean-3.2 {not procedure} -body {
        pep "(not (list 3))"
} -output "#f\n"

::tcltest::test boolean-3.3 {not procedure} -body {
        pep "(not #f)"
} -output "#t\n"

::tcltest::test boolean-3.4 {not procedure} -body {
        pep "(not '())"
} -output "#f\n"

::tcltest::test boolean-3.5 {not procedure} -body {
        pep "(not (list))"
} -output "#f\n"

::tcltest::test boolean-3.6 {not procedure} -body {
        pep "(not 'nil)"
} -output "#f\n"

TT)

# vim: ft=tcl tw=80
