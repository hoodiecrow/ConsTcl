
MD(
## Initial declarations
MD)

MD(
First, I need to create the namespace that will be used for most identifiers:
MD)

CB
namespace eval ::constcl {}
CB

MD(
### Utility commands

Next, some procedures that make my life as developer somewhat easier, but
don't really matter to the interpreter (except the two first ones). The other ones
will show up a lot in the test cases.
MD)

MD(
`reg` registers selected built-in procedures in the standard library.
MD)

CB
proc ::reg {key args} {
  if {[llength $args] == 0} {
    set val ::constcl::$key
  } else {
    lassign $args val
  }
  dict set ::constcl::defreg $key $val
}
CB

MD(
`regmacro` registers macro names in the macro list, so the evaluator knows what
to expand.
MD)

CB
proc ::regmacro {name} {
  lappend ::constcl::macrolist $name
}
CB

MD(
`pep` was named after the sequence parse-eval-print, and I never changed the
name. It reads and evals an expression, and prints the result.
MD)

CB
proc ::pep {str} {
  ::constcl::write [
    ::constcl::eval [
      ::constcl::parse $str]]
}
CB

MD(
`pp` is the same, but it doesn't eval the expression. It just prints what is
read.
MD)

CB
proc ::pp {str} {
  ::constcl::write [
    ::constcl::parse $str]
}
CB

MD(
`pe` is still the same, but it doesn't print the expression. It just evals what
is read.
MD)

CB
proc ::pe {str} {
  ::constcl::eval [
    ::constcl::parse $str]
}
CB

MD(
`prp` is a busy thing. It reads an expression, expands macros in it, resolves
defines, and prints the result.
MD)

CB
proc ::prp {str} {
  set expr [::constcl::parse $str]
  set op [::constcl::car $expr]
  set args [::constcl::cdr $expr]
  set env ::constcl::global_env
  while {[$op name] in $::constcl::macrolist} {
    ::constcl::expand-macro $env
  }
  set expr [::constcl::resolve-local-defines $args]
  ::constcl::write $expr
}
CB

MD(
`pxp` attempts to macro-expand whatever it reads, and prints the result. I know
that 'expand' doesn't start with an 'x'.
MD)

CB
proc ::pxp {str} {
  set expr [::constcl::parse $str]
  set op [::constcl::car $expr]
  set args [::constcl::cdr $expr]
  ::constcl::expand-macro ::constcl::global_env
  ::constcl::write [::constcl::cons $op $args]
}
CB

MD(
When called, tells the caller the name of its command.
MD)

CB
proc ::pn {} {
  lindex [split [lindex [info level -1] 0] :] end
}
CB

MD(
This one is a little bit of both, a utility function that is also among the
builtins in the library. It started out as a one-liner by Donal K. Fellows, but
has grown a bit since then to suit my needs.
MD)

CB
reg in-range

#started out as DKF's code
proc ::constcl::in-range {args} {
  set start 0
  set step 1
  switch [llength $args] {
    1 {
      lassign $args e
      set end [$e value]
    }
    2 {
      lassign $args s e
      set start [$s value]
      set end [$e value]
    }
    3 {
      lassign $args s e t
      set start [$s value]
      set end [$e value]
      set step [$t value]
    }
  }
  set res $start
  while {$step > 0 && $end > [incr start $step] || $step < 0 && $end < [incr start $step]} {
    lappend res $start
  }
  return [list {*}[lmap r $res {MkNumber $r}]]
}
CB

MD(
### The NIL class

The `NIL` class has one object: the empty list called `#NIL`. It is also base class for many other
type classes.
MD)

CB
catch { ::constcl::NIL destroy }

oo::class create ::constcl::NIL {
  constructor {} {}
  method bvalue {} {
    return #NIL
  }
  method car {} {
    ::error "PAIR expected"
  }
  method cdr {} {
    ::error "PAIR expected"
  }
  method set-car! {v} {
    ::error "PAIR expected"
  }
  method set-cdr! {v} {
    ::error "PAIR expected"
  }
  method numval {} {
    ::error "NUMBER expected"
  }
  method write {handle} {
    puts -nonewline $handle "()"
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "()"
  }
}
CB

MD(
__null?__

The `null?` standard predicate recognizes the empty list. Predicates
in ConsTcl return #t or #f for true or false, so some care is necessary
when calling them from Tcl code.
MD)

PR(
null? (public);val val -> bool
PR)

CB
reg null?

proc ::constcl::null? {val} {
  if {$val eq "#NIL"} {
    return #t
  } else {
    return #f
  }
}
CB

MD(
### The classes None, Dot, Unspecific, Undefined, and EndOfFile

The `None` class serves but one purpose: to avoid printing a result after `define`.
MD)

CB
catch { ::constcl::None destroy}

oo::class create ::constcl::None {}
CB

MD(
The `Dot` class is a helper class for the parser.
MD)

CB
catch { ::constcl::Dot destroy }

oo::class create ::constcl::Dot {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "."
  }
  method display {handle} {
    my write $handle
  }
}
CB

PR(
dot? (internal);val val -> bool
PR)

CB
proc ::constcl::dot? {val} {
  if {[info object isa typeof $val Dot]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] Dot]} {
    return #t
  } else {
    return #f
  }
}
CB

MD(
The `Unspecific` class is for unspecific things.
MD)

CB
catch { ::constcl::Unspecific destroy }

oo::class create ::constcl::Unspecific {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "#<unspecific>"
  }
  method display {handle} {
    my write $handle
  }
}
CB

MD(
The `Undefined` class is for undefined things.
MD)

CB
catch { ::constcl::Undefined destroy }

oo::class create ::constcl::Undefined {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "#<undefined>"
  }
  method display {handle} {
    my write $handle
  }
}
CB

MD(
The `EndOfFile` class is for end-of-file conditions.
MD)

CB
catch { ::constcl::EndOfFile destroy }

oo::class create ::constcl::EndOfFile {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "#<end-of-file>"
  }
  method display {handle} {
    my write $handle
  }
}
CB

MD(
### The error and check procedures

`error` is used to signal an error, with **msg** being a message string and the
optional arguments being values to show after the message.
MD)

PR(
error (public);msg msg ?args? exprs -> dc
PR)

CB
reg error

proc ::constcl::error {msg args} {
  if {[llength $args]} {
    lappend msg "("
    set times 0
    foreach arg $args {
      if {$times} {
        ::append msg " "
      }
      ::append msg [$arg show]
      incr times
    }
    lappend msg ")"
  }
  ::error $msg
}
CB

MD(
`check` does a check (usually a type check) on something and throws an error if
it fails.
MD)

CB
proc ::constcl::check {cond msg} {
  if {[uplevel $cond] eq "#f"} {
    ::error [
      uplevel [
        ::list subst [
          ::string trim $msg]]]
  }
}
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
