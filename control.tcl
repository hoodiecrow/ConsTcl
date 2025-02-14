
MD(
### Control

This section concerns itself with procedures and the application of the same.

A `Procedure` object is a
closure[#](https://en.wikipedia.org/wiki/Closure_(computer_programming)),
storing the procedure's parameter list, the body, and the environment that is current
when the object is created, i.e. when the procedure is defined.

When a `Procedure` object is called, the body is evaluated in a new environment
where the parameters are given values from the argument list and the outer link
goes to the closure environment.

__Procedure__ class
MD)

CB
catch { ::constcl::Procedure destroy }

oo::class create ::constcl::Procedure {
  superclass ::constcl::NIL
  variable parms body env
  constructor {p b e} {
    set parms $p
    set body $b
    set env $e
  }
  method value {} {}
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<proc-$num>"
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    return [self]
  }
  method call {args} {
    ::constcl::eval $body [
      ::constcl::Environment new $parms $args $env]
  }

}

interp alias {} ::constcl::MkProcedure \
  {} ::constcl::Procedure new
CB

MD(
__procedure?__
MD)

PR(
procedure? (public);val val -> bool
PR)

CB
reg procedure?

proc ::constcl::procedure? {val} {
  if {[typeof? $val Procedure] eq "#t"} {
    return #t
  } elseif {[::string match "::constcl::*" $val]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test control-1.0 {try procedure?)} -body {
    pep {(procedure? car)}
    pep {(procedure? 'car)}
    pep {(procedure? (lambda (x) (* x x)))}
} -output "#t\n#f\n#t\n"

TT)

MD(
__apply__

`apply` applies a procedure to a Lisp list of Lisp arguments.
MD)

PR(
apply (public);pr proc vals lvals -> invoke
PR)

MD(
Example:

```
(apply + (list 2 3))   =>  5
```
MD)

CB
reg apply

proc ::constcl::apply {pr vals} {
  check {procedure? $pr} {
    PROCEDURE expected\n([pn] [$pr show] ...)
  }
  invoke $pr $vals
}
CB

TT(

::tcltest::test control-2.0 {try apply)} -body {
    pep {(apply + (list 3 4))}
    pep {(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))}
    pep {((compose sqrt *) 12 75)}
} -output "7\n30.0\n"

::tcltest::test control-2.1 {try triggering a check} -body {
    pep {(apply #\+ (list 3 4))}
} -returnCodes error -result "PROCEDURE expected\n(apply #\\+ ...)"

TT)

MD(
__map__

`map` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The Lisp list of the results of the invocations is 
returned.
MD)

PR(
map (public);pr proc args lists -> lvals
PR)

MD(
Example:

```
(map + '(1 2 3) '(5 6 7))   => (6 8 10)
```
MD)

CB
reg map

proc ::constcl::map {pr args} {
  check {procedure? $pr} {
    PROCEDURE expected\n([pn] [$pr show] ...)
  }
  set arglists $args
  for {set i 0} \
    {$i < [llength $arglists]} \
    {incr i} {
    lset arglists $i [
      splitlist [lindex $arglists $i]]
  }
  set res {}
  for {set item 0} \
    {$item < [llength [lindex $arglists 0]]} \
    {incr item} {
    set arguments {}
    for {set arg 0} \
      {$arg < [llength $arglists]} \
      {incr arg} {
      lappend arguments [
        lindex $arglists $arg $item]
    }
    lappend res [invoke $pr [list {*}$arguments]]
  }
  return [list {*}$res]
}
CB

TT(

::tcltest::test control-3.0 {try map)} -body {
    pep {(map cadr '((a b) (d e) (g h)))}
    pep {(map (lambda (n) (expt n n)) '(1 2 3 4 5))}
    pep {(map + '(1 2 3) '(4 5 6))}
    pep {(let ((count 0))
  (map (lambda (ignored)
         (set! count (+ count 1))
         count)
       '(a b)))}
} -output "(b e h)\n(1.0 4.0 27.0 256.0 3125.0)\n(5 7 9)\n(1 2)\n"

TT)

MD(
__for-each__

`for-each` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The empty list is returned.
MD)

PR(
for-each (public);pr proc args lists -> nil
PR)

MD(
Example: (from R5RS; must be pasted as a oneliner for the ConsTcl repl to stomach
it.)

```
(let ((v (make-vector 5)))
  (for-each (lambda (i)
              (vector-set! v i (* i i)))
            '(0 1 2 3 4))
  v)                        =>  #(0 1 4 9 16)
```
MD)

CB
reg for-each

proc ::constcl::for-each {proc args} {
  check {procedure? $proc} {
    PROCEDURE expected\n([pn] [$proc show] ...)
  }
  set arglists $args
  for {set i 0} \
    {$i < [llength $arglists]} \
    {incr i} {
    lset arglists $i [
      splitlist [lindex $arglists $i]]
  }
  for {set item 0} \
    {$item < [llength [lindex $arglists 0]]} \
    {incr item} {
    set arguments {}
    for {set arg 0} \
      {$arg < [llength $arglists]} \
      {incr arg} {
      lappend arguments [
        lindex $arglists $arg $item]
    }
    invoke $proc [list {*}$arguments]
  }
  return #NIL
}
CB

TT(

::tcltest::test control-4.0 {try for-each)} -body {
    pep {(for-each display '(1 2 3))}
} -output "123()\n"

TT)

CB
proc ::constcl::force {promise} {
    # TODO
}
CB

CB
proc ::constcl::call-with-current-continuation {proc} {
    # TODO
}
CB

CB
proc ::constcl::values {args} {
    # TODO
}
CB

CB
proc ::constcl::call-with-values {producer consumer} {
    # TODO
}
CB

CB
proc ::constcl::dynamic-wind {before thunk after} {
    # TODO
}
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
