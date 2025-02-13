
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
__reg__

`reg` registers selected built-in procedures in the standard library.
MD)

PR(
reg (internal);key tstr ?val? tstr -> none
PR)

CB
proc ::reg {key args} {
  if {[llength $args]} {
    lassign $args val
  } else {
    set val ::constcl::$key
  }
  dict set ::constcl::defreg $key $val
  return
}
CB

MD(
__regmacro__

`regmacro` registers macro names in the macro list, so the evaluator knows what
to expand.
MD)

PR(
regmacro (internal);name tstr -> none
PR)

CB
proc ::regmacro {name} {
  lappend ::constcl::macrolist $name
  return
}
CB

MD(
__pep__

`pep` was named after the sequence parse-eval-print, and I never changed the
name. It reads and evals an expression, and prints the result.
MD)

PR(
pep (internal);str tstr -> none
PR)

CB
proc ::pep {str} {
  ::constcl::write [
    ::constcl::eval [
      ::constcl::parse $str]]
}
CB

MD(
__pp__

`pp` is the same, only it doesn't eval the expression. It just prints what is
parsed.
MD)

PR(
pp (internal);str tstr -> none
PR)

CB
proc ::pp {str} {
  ::constcl::write [
    ::constcl::parse $str]
}
CB

MD(
__pe__

`pe` is still the same, but it doesn't print the expression. It just evals what
is read.
MD)

PR(
pe (internal);str tstr -> val
PR)

CB
proc ::pe {str} {
  ::constcl::eval [
    ::constcl::parse $str]
}
CB

MD(
__p__

`p` is mostly the same, but it only parses the input, returning an expression.
MD)

PR(
p (internal);str tstr -> expr
PR)

CB
proc ::p {str} {
  ::constcl::parse $str
}
CB

MD(
__e__

`e` is another single-action procedure, eval-ing an expression and returning a
value.
MD)

PR(
e (internal);expr expr -> val
PR)

CB
proc ::e {expr} {
  ::constcl::eval $expr
}
CB

MD(
__w__

`w` is the third single-action procedure, printing a value and that's all.
MD)

PR(
w (internal);val val -> none
PR)

CB
proc ::w {val} {
  ::constcl::write $val
}
CB

MD(
__r__

`r` is an extra single-action procedure, reading from default input or from a
port and returning an expression.
MD)

PR(
r (internal);?port? iport -> expr
PR)

CB
proc ::r {args} {
  ::constcl::read {*}$args
}
CB

MD(
__prp__

`prp` is a busy thing. It reads an expression, expands macros in it, resolves
defines, and prints the result.
MD)

PR(
prp (internal);str tstr -> none
PR)

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
__pxp__

`pxp` attempts to macro-expand whatever it reads, and prints the result. I know
that 'expand' doesn't start with an 'x'.
MD)

PR(
pxp (internal);str tstr -> none
PR)

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
__pn__

"Procedure name" When called, tells the caller the name of its command.
MD)

PR(
pn (internal);-> tstr
PR)

CB
proc ::pn {} {
  lindex [split [lindex [info level -1] 0] :] end
}
CB

MD(
__typeof?__

`typeof?` looks at a value's type and reports if it is the same as the given
type. To be certain, it looks at the value in two ways: once assuming that the value
is a ConsTcl object, and once assuming that the value is an interpreter (the Tcl
interpreter, not ConsTcl) alias for a ConsTcl object. If one of those affirms
the type, the procedure returns #t.
MD)

PR(
typeof? (internal);val val type tstr -> bool
PR)

CB
proc ::constcl::typeof? {val type} {
  if {[info object isa typeof $val $type]} {
    return #t
  } elseif {[info object isa typeof \
      [interp alias {} $val] $type]} {
    return #t
  } else {
    return #f
  }
}
CB

MD(
__in-range__

This one is a little bit of both, a utility function that is also among the
builtins in the library. It started out as a one-liner by Donal K. Fellows, but
has grown a bit since then to suit my needs.

The plan is to arrange a sequence of numbers, given one, two or three ConsTcl
Number objects. If one is passed to the procedure, it is used as the end of the
sequence: the sequence will end just before it. If two numbers are passed, the
first one becomes the start of the sequence: the first number in it. The second
number will become the end of the sequence. If three numbers are passed, they
become start, end, and step, i.e. how much is added to the current number to
find next number in the sequence.
MD)

PR(
in-range (public);x num ?e? num ?t? num -> lnums
PR)

CB
reg in-range

#started out as DKF's code
proc ::constcl::in-range {x args} {
  set start 0
  set step 1
  switch [llength $args] {
    0 {
      set e $x
      set end [$e numval]
    }
    1 {
      set s $x
      lassign $args e
      set start [$s numval]
      set end [$e numval]
    }
    2 {
      set s $x
      lassign $args e t
      set start [$s numval]
      set end [$e numval]
      set step [$t numval]
    }
  }
  set res $start
  while {$step > 0 && $end > [incr start $step] ||
      $step < 0 && $end < [incr start $step]} {
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

The `null?` standard predicate recognizes the empty list. Predicates in ConsTcl
return #t or #f for true or false, so some care is necessary when calling them
from Tcl code (the Tcl `if` command expects 1 or 0 as truth values).
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

MD(
__dot?__

`dot?` is a type predicate that checks for membership in the type `Dot`.
MD)

PR(
dot? (internal);val val -> bool
PR)

CB
proc ::constcl::dot? {val} {
  return [typeof? $val "Dot"]
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

__error__

`error` is used to signal an error, with **msg** being a message string and the
optional arguments being values to show after the message.
MD)

PR(
error (public);msg msg ?exprs? exprs -> dc
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
__check__

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

MD(
### The atom? predicate

__atom?__

`atom?` recognizes an atom by checking for membership in any one of the atomic types.
MD)

PR(
atom? (public);val val -> bool
PR)

CB
reg atom? ::constcl::atom?

proc ::constcl::atom? {val} {
  foreach type {symbol number string
      char boolean vector port} {
    if {[$type? $val] eq "#t"} {
      return #t
    }
  }
  return #f
}
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
