# ConsTcl

## Introduction

### To run the software

First things first. To run, source the file __constcl.tcl__ (with
__schemebase.lsp__ in the directory) in a Tcl console (I use __tkcon__) and use
the command __repl__ for a primitive command dialog.  Source
__all.tcl__ to run the test suite (you need __constcl.test__ for that). The
files can be found on [GitHub/ConsTcl](https://github.com/hoodiecrow/ConsTcl).

### Background

It all started with Peter Norvig's Lisp emulator
[Lispy](https://norvig.com/lispy.html). In January 2025 I was inspired to port
it to Tcl. The result was [Thtcl](https://github.com/hoodiecrow/thtcl). It had
the same features and limitations as Lispy, plus a couple that were due to
shortcomings of Tcl, and I came out of the experience feeling a bit
dissatisfied. In the latter part of January I embarked on a new project,
ConsTcl, a true Lisp interpreter. In Tcl.

#### About ConsTcl

It's written with Vim, my editor of choice. I'm using annotated source which
mixes code with documentation and test cases. Compared to Lispy/Thtcl, ConsTcl
has, quote from Lispy, comments, quote and quasiquote notation, # literals,
the derived expression types (such as cond, derived from if, or let, derived
from lambda), and dotted list notation. And also real consed lists. Again
compared to Lispy/Thtcl, ConsTcl has the data types, quote, strings,
characters, booleans, ports, vectors. And pairs and procedures too. The number
of missing primitive procedures is in the tens, not the 100s. The completeness
comes with a price: due to the sheer number of calls for each action, ConsTcl
is is fairly slow. On my cheap computer, the following code (which calculates
the factorial of 100) takes 0.03 seconds to run. That is ten times slower than
Lispy assuming that Norvig's computer is as slow as mine, which is unlikely.

```
time {pe "(fact 100)"} 10
```

ConsTcl is of course still limited. It doesn't come close to having call/cc or
tail recursion. It doesn't have exact/inexact numbers, or most of the numerical
tower. There is no memory management. Error reporting is spotty, and there is no
error recovery.

## Initial declarations


First, I need to create the namespace that will be used for most identifiers:

```
namespace eval ::constcl {}
```
### Utility commands


Next, some procedures that make my life as developer somewhat easier, but don't really matter to the interpreter (except the two first ones). The other ones will show up a lot in the test cases.



__reg__ procedure


`` reg `` registers selected built-in procedures in the definitions register. That way I don't need to manually keep track of and list procedures. The definitions register's contents will eventually get dumped into the [standard library](https://github.com/hoodiecrow/ConsTcl#environment-startup).


You can call `` reg `` with two values: _key_ and _val_. _Key_ is the string that will eventually become the lookup symbol in the standard library, and _val_ is the name of the Tcl command that will carry out the procedure. If you don't give a value for _val_, `` reg `` creates a value by prepending the `` ::constcl:: `` namespace to the _key_ value, which is sufficient 99% of the time.

<table border=1><thead><tr><th colspan=2 align="left">reg (internal)</th></tr></thead><tr><td>key</td><td>a Tcl string</td></tr><tr><td>?val?</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::reg {key args} {
  if {[llength $args]} {
    lassign $args val
  } else {
    set val ::constcl::$key
  }
  dict set ::constcl::defreg $key $val
  return
}
```


__regmacro__ procedure


ConsTcl has macros, i.e. syntactic forms that are rewritten to concrete--but more verbose--forms. The evaluator passes macro forms to a command for expansion before they are fully processed. `` regmacro `` registers macro names in the macro list, so the evaluator knows what to expand.

<table border=1><thead><tr><th colspan=2 align="left">regmacro (internal)</th></tr></thead><tr><td>name</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::regmacro {name} {
  lappend ::constcl::macrolist $name
  return
}
```


__pew__ procedure


`` pew `` was originally named `` pep `` after the sequence parse-eval-print. Now it's named for parse-eval-write. It reads and evals an expression, and writes the result. It's the most common command in the test cases, since it allows me to write code directly in Scheme and to get proper Lisp output.

<table border=1><thead><tr><th colspan=2 align="left">pew (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::pew {str} {
  ::constcl::write [
    ::constcl::eval [
      ::constcl::parse $str]]
}
```


__pw__ procedure


`` pw `` is a similar command, except it doesn't eval the expression. It just writes what is parsed. It is useful for tests when the evaluator can't (yet) evaluate the form, but I can still check if it gets read and written correctly.

<table border=1><thead><tr><th colspan=2 align="left">pw (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::pw {str} {
  ::constcl::write [
    ::constcl::parse $str]
}
```


__rw__ procedure


`` rw `` is the reading variant of `` pw ``. It just writes what is read.

<table border=1><thead><tr><th colspan=2 align="left">rw (internal)</th></tr></thead><tr><td>?port?</td><td>an input port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::rw {args} {
  ::constcl::write [
    ::constcl::read {*}$args]
}
```


__pe__ procedure


`` pe `` is also similar, but it doesn't write the expression. It just evaluates what is read. That way I get a value object which I can pass to another command, or pick apart in different ways.

<table border=1><thead><tr><th colspan=2 align="left">pe (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::pe {str} {
  ::constcl::eval [
    ::constcl::parse $str]
}
```


__re__ procedure


`` re `` is like `` pe ``, but it reads from a port instead of an input buffer. It evaluates what is read.

<table border=1><thead><tr><th colspan=2 align="left">re (internal)</th></tr></thead><tr><td>?port?</td><td></td></tr><tr><td>val</td><td></td></tr></table>

```
proc ::re {args} {
  ::constcl::eval [
    ::constcl::read {*}$args]
}
```


__p__ procedure


`` p `` only parses the input, returning an expression object.

<table border=1><thead><tr><th colspan=2 align="left">p (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::p {str} {
  ::constcl::parse $str
}
```


__e__ procedure


`` e `` is another single-action procedure, evaluating an expression and returning a value.

<table border=1><thead><tr><th colspan=2 align="left">e (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::e {expr} {
  ::constcl::eval $expr
}
```


__w__ procedure


`` w `` is the third single-action procedure, printing a value and that's all.

<table border=1><thead><tr><th colspan=2 align="left">w (internal)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::w {val} {
  ::constcl::write $val
}
```


__r__ procedure


`` r `` is an extra single-action procedure, reading from default input or from a port and returning an expression object.

<table border=1><thead><tr><th colspan=2 align="left">r (internal)</th></tr></thead><tr><td>?port?</td><td>an input port</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::r {args} {
  ::constcl::read {*}$args
}
```


__prw__ procedure


`` prw `` reads an expression, resolves defines, and writes the result. It was handy during the time I was porting the 'resolve local defines' section.

<table border=1><thead><tr><th colspan=2 align="left">prw (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::prw {str} {
  set expr [::constcl::parse $str]
  set expr [::constcl::resolve-local-defines \
    [::constcl::cdr $expr]]
  ::constcl::write $expr
}
```


__pxw__ procedure


`` pxw `` attempts to macro-expand whatever it reads, and writes the result. I know that 'expand' doesn't start with an 'x'. Again, this command's heyday was when I was developing the macro facility.

<table border=1><thead><tr><th colspan=2 align="left">pxw (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::pxw {str} {
  set expr [::constcl::parse $str]
  set expr [::constcl::expand-macro $expr \
    ::constcl::global_env]
  ::constcl::write $expr
}
```


__pn__ procedure


`` pn `` stands for 'procedure name'. When called, tells the caller the name of its command. I use it for error messages so the error message can automagically tell the user which command failed.

<table border=1><thead><tr><th colspan=2 align="left">pn (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a Tcl string</td></tr></table>

```
proc ::pn {} {
  lindex [split [lindex [info level -1] 0] :] end
}
```


__typeof?__ procedure


`` typeof? `` looks at a value's type and reports if it is the same as the given type. To be certain, it looks at the value in two ways: once assuming that the value is a ConsTcl object, and once assuming that the value is an interpreter (the Tcl interpreter, not ConsTcl) alias for a ConsTcl object. If one of those affirms the type, the procedure returns #t. By Scheme convention, predicates (procedures that return either `` #t `` or `` #f ``) have '?' at the end of their name.

<table border=1><thead><tr><th colspan=2 align="left">typeof? (internal)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td>type</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
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
```


__in-range__ procedure


This one is a little bit of both, a utility function that is also among the builtins in the library. It started out as a one-liner by Donal K. Fellows, but has grown a bit since then to suit my needs.


The plan is to arrange a sequence of numbers, given one, two or three ConsTcl Number objects. If one is passed to the procedure, it is used as the end of the sequence: the sequence will end just before it. If two numbers are passed, the first one becomes the start of the sequence: the first number in it. The second number will become the end of the sequence. If three numbers are passed, they become start, end, and step, i.e. how much is added to the current number to find next number in the sequence.

<table border=1><thead><tr><th colspan=2 align="left">in-range (public)</th></tr></thead><tr><td>x</td><td>a number</td></tr><tr><td>?e?</td><td>a number</td></tr><tr><td>?t?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of numbers</td></tr></table>

```
reg in-range

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
```
### The NIL class


The `` NIL `` class has one object: the empty list called `` #NIL ``. It is also base class for many other type classes.

```
catch { ::constcl::NIL destroy }

oo::singleton create ::constcl::NIL {
  method boolval {} {
    return #t
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
```


__null?__ procedure


The `` null? `` standard predicate recognizes the empty list. Predicates in ConsTcl return #t or #f for true or false, so some care is necessary when calling them from Tcl code (the Tcl `` if `` command expects 1 or 0 as truth values).

<table border=1><thead><tr><th colspan=2 align="left">null? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg null?

proc ::constcl::null? {val} {
  if {$val eq "#NIL"} {
    return #t
  } else {
    return #f
  }
}
```
### The classes Dot, Unspecified, Undefined, and EndOfFile


The `` Dot `` class is a helper class for the parser.

```
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
```


__dot?__ procedure


`` dot? `` is a type predicate that checks for membership in the type `` Dot ``.

<table border=1><thead><tr><th colspan=2 align="left">dot? (internal)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
proc ::constcl::dot? {val} {
  typeof? $val "Dot"
}
```


The `` Unspecified `` class is for unspecified things. It was created to facilitate porting of code from 'Scheme 9 from Empty Space'.

```
catch { ::constcl::Unspecified destroy }

oo::class create ::constcl::Unspecified {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "#<unspecified>"
  }
  method display {handle} {
    my write $handle
  }
}
```


The `` Undefined `` class is for undefined things. Also a S9fES support class.

```
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
```


The `` EndOfFile `` class is for end-of-file conditions.

```
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
```
```
proc eof? {val} {
  if {$val eq "#EOF"} {
    return #t
  } else {
    return #f
  }
}
```
### The error and check procedures


__error__ procedure


`` error `` is used to signal an error, with _msg_ being a message string and the optional arguments being values to show after the message.

<table border=1><thead><tr><th colspan=2 align="left">error (public)</th></tr></thead><tr><td>msg</td><td>a message string</td></tr><tr><td>?exprs?</td><td>some expressions</td></tr><tr><td><i>Returns:</i></td><td>-don't care-</td></tr></table>

```
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
```


__check__ procedure


`` check `` does a check (typically a type check) on something and throws an error if it fails.

```
proc ::constcl::check {cond msg} {
  if {[uplevel $cond] eq "#f"} {
    ::error [
      uplevel [
        ::list subst [
          ::string trim $msg]]]
  }
}
```
### The atom? predicate


__atom?__ procedure


There are two kinds of data in Lisp: lists and atoms. Lists are collections of lists and atoms. Atoms are instances of types such as booleans, characters, numbers, ports, strings, symbols, and vectors. `` Atom? `` recognizes an atom by checking for membership in any one of the atomic types.

<table border=1><thead><tr><th colspan=2 align="left">atom? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg atom? ::constcl::atom?

proc ::constcl::atom? {val} {
  foreach type {symbol number string
      char boolean vector port eof} {
    if {[$type? $val] eq "#t"} {
      return #t
    }
  }
  return #f
}
```
## Input


The first thing an interpreter must be able to do is to take in the user's code and data input, whether from the keyboard or from a source file. `` read `` represents the interpreter's main input facility. The `` read- `` procedures read from standard input, or--if a port is provided--from the port's channel.

### Parsing
#### The parsing process


[Parsing](https://en.wikipedia.org/wiki/Parsing), or syntactic analysis, is analyzing a sequence of letters, digits, and other characters, conforming to the rules of _external representation_. The result of parsing is an _expression_ in internal form.


The parsing process translates an expression from external representation to internal representation. The external representation is a 'recipe' for an expression that expresses it in a unique way.


For example, the external representation for a vector is a sharp sign (`` # ``), a left parenthesis (`` ( ``), the external representation for some values, and a right parenthesis (`` ) ``). When the reader or parser is working through input, a `` #( `` symbol signals that a vector structure is being read. A number of subexpressions for the elements of the vector follow, and then a closing parenthesis `` ) `` signals that the vector is done. The elements are saved in vector memory and the vector gets the address to the first element and the number of elements.

![#](images/vector-representation.png)

The `` parse `` procedure takes in the input buffer character by character, matching each character against a fitting external representation. When done, it creates a ConsTcl object, which is the internal representation of an expression. The object can then be passed to the evaluator.


Given a string, `` parse `` fills the input buffer. It then parses the input and produces the internal representation of an expression.


Example:

```
% ::constcl::parse "(+ 2 3)"
::oo::Obj491
```


Here, `` parse `` parsed the external representation of a list with three elements, +, 2, and 3. It produced the expression that has an internal representation labeled `` ::oo::Obj491 ``. We will later meet procedures like `` eval ``, which transforms an expression into a value, and `` write ``, which prints a printed representation of expressions and values. Putting them together: we can see

```
% ::constcl::write ::oo::Obj491
(+ 2 3)
% ::constcl::eval ::oo::Obj491
::oo::Obj494
% ::constcl::write ::oo::Obj494
5
```


Fortunately, we don't have to work at such a low level. We can use the `` repl `` instead:

```
ConsTcl> (+ 2 3)
5
```


Then, parsing and evaluation and writing goes on in the background and the internal representations of expressions and values are hidden.


Anyway, here is how it really looks like. `` ::oo::Obj491 `` was just the head of the list.

![#](images/intreplist.png)

__parse__ procedure


`` parse `` can be called with either a string input port or a Tcl or ConsTcl string (which `` parse `` uses to open a string input port). Once the input port is established, `` parse `` leaves control to `` read-expr ``.

<table border=1><thead><tr><th colspan=2 align="left">parse (internal)</th></tr></thead><tr><td>inp</td><td>a Tcl string, Lisp string, or an input buffer</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
reg parse

proc ::constcl::parse {inp} {
  set c {}
  set unget {}
  if {[info object isa object $inp]} {
    if {[typeof? $inp StringInputPort] ne "#f"} {
      set port $inp
    } elseif {[typeof? $inp String] ne "#f"} {
      set port [::constcl::StringInputPort new [$inp value]]
    } else {
      ::error "Unknown object [$inp show]"
    }
  } else {
    # It's a Tcl string, we hope
    set port [StringInputPort new $inp]
  }
  set oldport $::constcl::Input_port
  set ::constcl::Input_port $port
  set expr [read-expr]
  set ::constcl::Input_port $oldport
  return $expr
}
```


__make-constant__ procedure


The `` make-constant `` helper procedure is called to set expressions to constants when read as a literal.

```
proc ::constcl::make-constant {val} {
  if {[pair? $val] ne "#f"} {
    $val mkconstant
    make-constant [car $val]
    make-constant [cdr $val]
  } elseif {[null? $val] ne "#f"} {
    return #NIL
  } else {
    $val mkconstant
  }
}
```


__interspace__ procedure


The `` interspace `` helper procedure recognizes whitespace between value representations.

```
proc ::constcl::interspace {c} {
  if {[::string is space $c] || $c eq ";"} {
      return #t
    } else {
      return #f
    }
}
```


__character-check__ procedure


The `` character-check `` helper procedure compares a potential character constant to the valid kinds.

<table border=1><thead><tr><th colspan=2 align="left">character-check (internal)</th></tr></thead><tr><td>name</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
proc ::constcl::character-check {name} {
  if {[regexp {(?i)^#\\([[:graph:]]|space|newline)$} \
      $name]} {
    return #t
  } else {
    return #f
  }
}
```
### read


__read__ procedure


The standard builtin `` read `` reads an input port the same way that `` parse `` takes in an input buffer. The `` read- `` procedures parse their input and produce ConsTcl objects.


One can pass a port to `` read ``, in which case `` read `` sets the standard input port temporarily to the provided port. If not, `` read `` uses the default standard input port (usually the keyboard).

<table border=1><thead><tr><th colspan=2 align="left">read (public)</th></tr></thead><tr><td>?port?</td><td>a port</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
reg read

proc ::constcl::read {args} {
  set c {}
  set unget {}
  if {[llength $args]} {
    lassign $args port
  } else {
    set port $::constcl::Input_port
  }
  set oldport $::constcl::Input_port
  set ::constcl::Input_port $port
  set expr [read-expr]
  set ::constcl::Input_port $oldport
  return $expr
}
```


__read-expr__ procedure


The procedure `` read-expr `` reads input by reading the first available character and delegating to one of the more detailed reading procedures based on that, producing an expression of any kind. A Tcl character value can be passed to it, that character will be used first before reading from the input stream. If end of file is encountered before an expression can be read in full, the procedure returns end of file (`` #EOF ``).

<table border=1><thead><tr><th colspan=2 align="left">read-expr (internal)</th></tr></thead><tr><td>?char?</td><td></td></tr><tr><td><i>Returns:</i></td><td>an expression or end of file</td></tr></table>

```
proc ::constcl::read-expr {args} {
  upvar c c unget unget
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
  set unget {}
  read-eof $c
  if {[::string is space $c] || $c eq ";"} {
    skip-ws
    read-eof $c
  }
  switch -regexp $c {
    {\"}          { read-string-expr }
    {\#}          { read-sharp }
    {\'}          { read-quoted-expr }
    {\(}          { read-pair-expr ")" }
    {\+} - {\-}   { read-plus-minus $c }
    {\,}          { read-unquoted-expr }
    {\.} {
        set x [Dot new]; set c [readc]; set x
    }
    {\:}          { read-object-expr }
    {\[}          { read-pair-expr "\]" }
    {\`}          { read-quasiquoted-expr }
    {\d}          { read-number-expr $c }
    {^$}          { return}
    {[[:graph:]]} { read-identifier-expr $c }
    default {
      read-eof $c
      ::error "unexpected character ($c)"
    }
  }
}
```


__readc__ procedure


`` readc `` reads one character from the unget store if it isn't empty or else from the input stream. If the input stream is at end-of-file, an eof object is returned.

<table border=1><thead><tr><th colspan=2 align="left">readc (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td></td></tr></table>

```
proc ::constcl::readc {} {
  upvar unget unget
  if {$unget ne {}} {
    set c $unget
    set unget {}
  } else {
    set c [$::constcl::Input_port get]
    if {[$::constcl::Input_port eof]} {
      return #EOF
    }
  }
  return $c
}
```


__read-find__ procedure


`` read-find `` reads ahead through whitespace to find a given character. Returns 1 if it has found the character, and 0 if it has stopped at some other character. Returns end of file if eof is encountered.

<table border=1><thead><tr><th colspan=2 align="left">read-find (internal)</th></tr></thead><tr><td>char</td><td></td></tr><tr><td><i>Returns:</i></td><td>a Tcl truth value (1 or 0) or end of file</td></tr></table>

```
proc ::constcl::read-find {char} {
  upvar c c unget unget
  while {[::string is space -strict $c]} {
    set c [readc]
    read-eof $c
    set unget $c
  }
  return [expr {$c eq $char}]
}
```


__read-end__ procedure


`` read-end `` reads one character and returns 1 if it is an interspace character or an ending parenthesis or bracket, or end of file. Otherwise it returns 0. It ungets the character before returning.

<table border=1><thead><tr><th colspan=2 align="left">read-end (internal)</th></tr></thead><tr><td>-&gt;tbooleof</td><td></td></tr></table>

```
proc ::constcl::read-end {} {
  upvar c c unget unget
  set c [readc]
  if {[interspace $c] ne "#f" || $c in {) ]} || $c eq "#EOF"} {
    set unget $c
    return 1
  } else {
    set unget $c
    return 0
  }
}
```


__skip-ws__ procedure


`` skip-ws `` skips whitespace and comments (the `` ; `` to end of line kind). Uses the shared _c_ variable. It leaves the first character not to be skipped in _c_.

<table border=1><thead><tr><th colspan=2 align="left">skip-ws (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::skip-ws {} {
  upvar c c unget unget
  while true {
    switch -regexp $c {
      {[[:space:]]} {
        set c [readc]
      }
      {;} {
        while {$c ne "\n" && $c ne "#EOF"}  {
          set c [readc]
        }
      }
      default {
        set unget $c
        return
      }
    }
  }
}
```


__read-eof__ procedure


`` read-eof `` checks a number of presumed characters for possible end-of-file objects. If it finds one, it returns _from its caller_ with the EOF value.

<table border=1><thead><tr><th colspan=2 align="left">read-eof (internal)</th></tr></thead><tr><td>args</td><td>some characters</td></tr></table>

```
proc ::constcl::read-eof {args} {
  foreach val $args {
    if {$val eq "#EOF"} {
      return -level 1 -code return #EOF
    }
  }
}
```


__read-string-expr__ procedure


`` read-string-expr `` is activated by `` read-expr `` when it reads a double quote. It collects characters until it reaches another (unescaped) double quote. It then returns a string expression--an immutable [String](https://github.com/hoodiecrow/ConsTcl#strings) object.

<table border=1><thead><tr><th colspan=2 align="left">read-string-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a string or end of file</td></tr></table>

```
proc ::constcl::read-string-expr {} {
  upvar c c unget unget
  set str {}
  set c [readc]
  read-eof $c
  while {$c ne "\"" && $c ne "#EOF"} {
    if {$c eq "\\"} {
      ::append str $c
      set c [readc]
    }
    ::append str $c
    set c [readc]
  }
  if {$c eq "#EOF"} {
    error "bad string (no ending double quote)"
  }
  set c [readc]
  set expr [MkString $str]
  read-eof $expr
  $expr mkconstant
  return $expr
}
```


__read-sharp__ procedure


`` read-sharp `` is activated by `` read-expr `` when it reads a sharp sign (`` # ``). It in turn either delegates to the vector reader or the character reader, or returns boolean literals.

<table border=1><thead><tr><th colspan=2 align="left">read-sharp (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a vector, boolean, or character value or end of file</td></tr></table>

```
proc ::constcl::read-sharp {} {
  upvar c c unget unget
  set unget {}
  set c [readc]
  read-eof $c
  switch $c {
    (    { set n [read-vector-expr] }
    t    { if {[read-end]} {set n #t} }
    f    { if {[read-end]} {set n #f} }
    "\\" { set n [read-character-expr] }
    default {
      ::error "Illegal #-literal: #$c"
    }
  }
  return $n
}
```


__read-vector-expr__ procedure


`` read-vector-expr `` is activated by `` read-sharp `` and reads a number of expressions until it finds an ending parenthesis. It produces a vector expression and returns a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.

<table border=1><thead><tr><th colspan=2 align="left">read-vector-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a vector or end of file</td></tr></table>

```
proc ::constcl::read-vector-expr {} {
  upvar c c unget unget
  set res {}
  set c [readc]
  while {$c ne "#EOF" && $c ne ")"} {
    lappend res [read-expr $c]
    skip-ws
    read-eof $c
  }
  if {$c ne ")"} {
    ::error "Missing right paren. ($c)."
  }
  set expr [MkVector $res]
  read-eof $expr
  $expr mkconstant
  return $expr
}
```


__read-character-expr__ procedure


`` read-character-expr `` reads input, producing a character and returning a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.

<table border=1><thead><tr><th colspan=2 align="left">read-character-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a character or end of file</td></tr></table>

```
proc ::constcl::read-character-expr {} {
  upvar c c unget unget
  set name "#\\"
  set c [readc]
  read-eof $c
  while {$c ni {) ]} && [::string is graph $c] && $c ne "#EOF"} {
    ::append name $c
    set c [readc]
  }
  check {character-check $name} {
      Invalid character constant $name
  }
  set expr [MkChar $name]
  read-eof $expr
  return $expr
}
```


__read-quoted-expr__ procedure


`` read-quoted-expr `` is activated by `` read-expr `` when reading a single quote ('). It then reads an entire expression beyond that, returning it wrapped in a list with `` quote ``. The quoted expression is made constant.

<table border=1><thead><tr><th colspan=2 align="left">read-quoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expression wrapped in the quote symbol or end of file</td></tr></table>

```
proc ::constcl::read-quoted-expr {} {
  upvar c c unget unget
  set unget {}
  set expr [read-expr]
  read-eof $expr
  make-constant $expr
  return [list [S quote] $expr]
}
```


__read-pair-expr__ procedure


The `` read-pair-expr `` procedure reads everything between two matching parentheses, or, as the case might be, brackets. It produces either an empty list, or a possibly recursive structure of [Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) objects, either a proper list (one that ends in `` #NIL ``), or an improper one (one that has an atom as its last member).

<table border=1><thead><tr><th colspan=2 align="left">read-pair-expr (internal)</th></tr></thead><tr><td>char</td><td>the terminating paren or bracket</td></tr><tr><td><i>Returns:</i></td><td>a structure of pair expressions or end of file</td></tr></table>

```
proc ::constcl::read-pair-expr {char} {
  upvar c c unget unget
  set unget {}
  set expr [read-pair $char]
  read-eof $expr
  if {$c ne $char} {
    if {$char eq ")"} {
      ::error \
        "Missing right paren. ($c)."
    } else {
      ::error \
        "Missing right bracket ($c)."
    }
  } else {
    set unget {}
    set c [readc]
  }
  return $expr
}
```


`` read-pair `` is a helper procedure that does the heavy lifting in reading a pair structure. First it checks if the list is empty, returning `` #NIL `` in that case. Otherwise it reads the first element in the list and then repeatedly the rest of them. If it reads a Dot object, the following element to be read is the tail end of an improper list. When `` read-pair `` has reached the ending parenthesis or bracket, it conses up the elements starting from the last, and returns the head of the list.

```
proc ::constcl::read-pair {char} {
  upvar c c unget unget
  set c [readc]
  read-eof $c
  if {[read-find $char]} {
    # read right paren/brack
    #set c [readc]
    return #NIL
  }
  set a [read-expr $c]
  set res $a
  skip-ws
  set prev #NIL
  while {![read-find $char]} {
    set x [read-expr $c]
    skip-ws
    read-eof $c
    if {[dot? $x] ne "#f"} {
      set prev [read-expr $c]
      skip-ws
      read-eof $c
    } else {
      lappend res $x
    }
    if {[llength $res] > 99} break
  }
  # read right paren/brack
  foreach r [lreverse $res] {
    set prev [cons $r $prev]
  }
  return $prev
}
```


__read-plus-minus__ procedure


`` read-plus-minus `` is called when a plus or minus is found in the input stream. If the next character is a digit, it delegates to the number reader. Otherwise, it returns a `` + `` or `` - `` symbol.

<table border=1><thead><tr><th colspan=2 align="left">read-plus-minus (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>either the symbols + or - or a number or end of file</td></tr></table>

```
proc ::constcl::read-plus-minus {char} {
  upvar c c unget unget
  set unget {}
  set c [readc]
  read-eof $c
  if {[::string is digit -strict $c]} {
    set n [read-number-expr $c]
    read-eof $n
    if {$char eq "-"} {
      set n [- $n]
    }
    return $n
  } elseif {[::string is space -strict $c] ||
      $c in {) ]}} {
    if {$char eq "+"} {
      return [S "+"]
    } else {
      return [S "-"]
    }
  } else {
    set n [read-identifier-expr $char $c]
    read-eof $n
    return $n
  }
}
```


__read-number-expr__ procedure


`` read-number-expr `` reads numerical input, both integers and floating point numbers. It actually takes in anything that starts out like a number and stops at whitespace or an ending parenthesis or bracket, and then it accepts or rejects the input by comparing it to a Tcl double. It returns a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.

<table border=1><thead><tr><th colspan=2 align="left">read-number-expr (internal)</th></tr></thead><tr><td>?char?</td><td></td></tr><tr><td><i>Returns:</i></td><td>a number or end of file</td></tr></table>

```
proc ::constcl::read-number-expr {args} {
  upvar c c unget unget
  set unget {}
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
  read-eof $c
  while {[interspace $c] ne "#t" && $c ne "#EOF" &&
      $c ni {) ]}} {
    ::append num $c
    set c [readc]
  }
  set unget $c
  check {::string is double -strict $num} {
      Invalid numeric constant $num
  }
  set expr [N $num]
  return $expr
}
```


__read-unquoted-expr__ procedure


When a comma is found in the input stream, `` read-unquoted-expr `` is activated. If it reads an at-sign (`` @ ``) it selects the symbol `` unquote-splicing ``, otherwise it selects the symbol `` unquote ``. Then it reads an entire expression and returns it wrapped in the selected symbol. Both of these expressions are only suppposed to occur inside a quasiquoted expression.

<table border=1><thead><tr><th colspan=2 align="left">read-unquoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expr. wr. in the unquote/-splicing symbol or end of file</td></tr></table>

```
proc ::constcl::read-unquoted-expr {} {
  upvar c c unget unget
  set unget {}
  set c [readc]
  read-eof $c
  if {$c eq "@"} {
    set symbol "unquote-splicing"
    set expr [read-expr]
  } else {
    set symbol "unquote"
    set expr [read-expr $c]
  }
  read-eof $expr
  return [list [S $symbol] $expr]
}
```


__read-quasiquoted-expr__ procedure


`` read-quasiquoted-expr `` is activated when there is a backquote (`` ` ``) in the input stream. It reads an entire expression and returns it wrapped in `` quasiquote ``.

<table border=1><thead><tr><th colspan=2 align="left">read-quasiquoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expr. wr. in the quasiquote symbol or end of file</td></tr></table>

```
proc ::constcl::read-quasiquoted-expr {} {
  upvar c c unget unget
  set unget {}
  set expr [read-expr]
  skip-ws
  read-eof $expr
  make-constant $expr
  return [list [S "quasiquote"] $expr]
}
```


__read-identifier-expr__ procedure


`` read-identifier-expr `` is activated for "anything else", and takes in characters until it finds whitespace or an ending parenthesis or bracket. It checks the input against the rules for identifiers, accepting or rejecting it with an error message. It returns a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.

<table border=1><thead><tr><th colspan=2 align="left">read-identifier-expr (internal)</th></tr></thead><tr><td>?chars?</td><td>some Tcl characters</td></tr><tr><td><i>Returns:</i></td><td>a symbol or end of file</td></tr></table>

```
proc ::constcl::read-identifier-expr {args} {
  upvar c c unget unget
  set unget {}
  if {[llength $args]} {
    set c [join $args {}]
  } else {
    set c [readc]
  }
  read-eof $c
  set name {}
  while {[::string is graph -strict $c]} {
    if {$c eq "#EOF" || $c in {) \]}} {
      break
    }
    ::append name $c
    set c [readc]
    # do not check for EOF here
  }
  if {$c ne "#EOF"} {
    set unget $c
  }
  read-eof $name
  # idcheck throws error if invalid identifier
  idcheck $name
  return [S $name]
}
```


__read-object-expr__ procedure


A non-standard extension, `` read-object-expr `` reads a ConsTcl object of any kind and passes its name along.

<table border=1><thead><tr><th colspan=2 align="left">read-object-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a ConsTcl object or end of file</td></tr></table>

```
proc ::constcl::read-object-expr {} {
  upvar c c unget unget
  # first colon has already been read
  foreach ch [split ":oo::Obj" {}] {
    set c [readc]
    read-eof $c
    if {$c ne $ch} {
      error "bad object name"
    }
  }
  set res "::oo::Obj"
  set c [readc]
  read-eof $c
  while {[::string is digit $c]} {
    ::append res $c
    set c [readc]
    read-eof $c
  }
  set unget $c
  return $res
}
```
## Evaluation


The second thing an interpreter must be able to do is to reduce expressions to their _normal form_, or _evaluate_ them. As an example, 2 + 6 and 8 are two expressions that have the same value, but the latter is in normal form (can't be reduced further) and the former is not.

### Syntactic forms


There are nine diffent forms or classes of expressions in Lisp. The evaluator recognizes each one by its internal representation and chooses the appropriate process to evaluate them. The nine forms will be described in the following sections.



__eval__ procedure


The heart of the Lisp interpreter, `` eval `` takes a Lisp expression and processes it according to its syntactic form.


`` eval `` also does two kinds of rewriting of expressions: 1) _macro expansion_ on a non-atomic expression into a more concrete expression. See the part about [macros](https://github.com/hoodiecrow/ConsTcl#macros) below, and 2) resolving _local defines_, acting on expressions of the form K{(begin (define ...} when in a local environment. See the part about [resolving local defines](https://github.com/hoodiecrow/ConsTcl#resolving-local-defines).

<table border=1><thead><tr><th colspan=2 align="left">eval (public)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg eval

proc ::constcl::eval \
  {expr {env ::constcl::global_env}} {
  if {[symbol? $expr] ne "#f"} {
    lookup $expr $env
  } elseif {[null? $expr] ne "#f" ||
    [atom? $expr] ne "#f"} {
    set expr
  } else {
    while {[[car $expr] name] in
      $::constcl::macrolist} {
      set expr [expand-macro $expr $env]
    }
    set op [car $expr]
    set args [cdr $expr]
    if {$env ne "::constcl::global_env" &&
      [$op name] eq "begin" &&
      ([pair? [car $args]] ne "#f" &&
      [[caar $args] name] eq "define")} {
      set expr [resolve-local-defines $args]
      set op [car $expr]
      set args [cdr $expr]
    }
    switch [$op name] {
      quote {
        car $args
      }
      if {
        if {[eval [car $args] $env] ne "#f"} \
          {eval [cadr $args] $env} \
          {eval [caddr $args] $env}
      }
      begin {
        /begin $args $env
      }
      define {
        /define [car $args] [
          eval [cadr $args] $env] $env
      }
      set! {
        /set! [car $args] [
          eval [cadr $args] $env] $env 
      }
      lambda {
        /lambda [car $args] [
          cdr $args] $env
      }
      default {
        invoke [eval $op $env] [
          eval-list $args $env]
      }
    }
  }
}
```
#### Variable reference


_Example: `` r `` => 10 (a symbol `` r `` is evaluated to 10)_


A variable is an identifier (symbol) bound to a location in the environment. If an expression consists of an identifier it is evaluated to the value stored in that location. This is handled by the helper procedure `` lookup ``. It searches the environment chain for the identifier, and returns the value stored in the location it is bound to. It is an error to do lookup on an unbound symbol.


__lookup__ procedure

<table border=1><thead><tr><th colspan=2 align="left">lookup (internal)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::lookup {sym env} {
  [$env find $sym] get $sym
}
```
#### Constant literal


_Example: `` 99 `` => 99 (a number evaluates to itself)_


Not just numbers but booleans, characters, strings, and vectors evaluate to themselves, to their innate value. Because of this, they are called autoquoting types (see next paragraph).

#### Quotation


_Example: `` (quote r) `` => `` r `` (quotation makes the symbol evaluate to itself, like a constant)_


According to the rules of variable reference, a symbol evaluates to its stored value. Well, sometimes one wishes to use the symbol itself as a value. That is what quotation is for. `` (quote x) `` evaluates to the symbol `` x `` itself and not to any value that might be stored under it. This is so common that there is a shorthand notation for it: `` 'x `` is interpreted as `` (quote x) `` by the Lisp reader.

#### Conditional


_Example: `` (if (> 99 100) (* 2 2) (+ 2 4)) `` => 6_


The conditional form `` if `` evaluates a Lisp list of three expressions. The first, the _condition_, is evaluated first. If it evaluates to anything other than `` #f `` (false), the second expression (the _consequent_) is evaluated and the value returned. Otherwise, the third expression (the _alternate_) is evaluated and the value returned. One of the two latter expressions will be evaluated, and the other will remain unevaluated.


__/if__ procedure

<table border=1><thead><tr><th colspan=2 align="left">/if (internal)</th></tr></thead><tr><td>condition</td><td>an expression</td></tr><tr><td>consequent</td><td>an expression</td></tr><tr><td>alternate</td><td>an expression</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::/if {cond conseq altern} {
  if {[uplevel $cond] ne "#f"} {
    uplevel $conseq
  } {
    uplevel $altern
  }
}
```
#### Sequence


_Example: `` (begin (define r 10) (* r r)) `` => 100_


When expressions are evaluated in sequence, the order is important for two reasons. If the expressions have any side effects, they happen in the same order of sequence. Also, if expressions are part of a pipeline of calculations, then they need to be processed in the order of that pipeline. The `` /begin `` helper procedure takes a Lisp list of expressions and evaluates them in sequence, returning the value of the last one.


__/begin__ procedure

<table border=1><thead><tr><th colspan=2 align="left">/begin (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::/begin {exps env} {
  /if {pair? $exps} {
    /if {pair? [cdr $exps]} {
      eval [car $exps] $env
      return [/begin [cdr $exps] $env]
    } {
      return [eval [car $exps] $env]
    }
  } {
    return #NIL
  }
}
```
#### Definition


_Example: `` (define r 10) `` => ... (a definition doesn't evaluate to anything)_


We've already seen the relationship between symbols and values. Through (variable) definition, a symbol is bound to a value (or rather to the location the value is in), creating a variable. The `` /define `` helper procedure adds a variable to the current environment. It first checks that the symbol name is a valid identifier, then it updates the environment with the new binding.


__/define__ procedure

<table border=1><thead><tr><th colspan=2 align="left">/define (internal)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::/define {sym val env} {
  varcheck [idcheck [$sym name]]
  $env set $sym $val
  return
}
```
#### Assignment


_Example: `` (set! r 20) `` => 20 (`` r `` is a bound symbol, so it's allowed to assign to it)_


Once a variable has been created, the value at the location it is bound to can be changed (hence the name "variable", something that can be modified). The process is called assignment. The `` /set! `` helper does assignment: it modifies an existing variable that is bound somewhere in the environment chain. It finds the variable's environment and updates the binding. It returns the value, so calls to `` set! `` can be chained: `` (set! foo (set! bar 99)) `` sets both variables to 99. By Scheme convention, procedures that modify variables have "!" at the end of their name.


__/set!__ procedure

<table border=1><thead><tr><th colspan=2 align="left">/set! (internal)</th></tr></thead><tr><td>var</td><td>a bound symbol</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::/set! {var val env} {
  [$env find $var] set $var $val
  set val
}
```
#### Procedure definition


_Example: `` (lambda (r) (* r r)) `` => `` ::oo::Obj3601 `` (it will be a different object each time)_


In Lisp, procedures are values just like numbers or characters. They can be defined as the value of a symbol, passed to other procedures, and returned from procedures. One diffence from most values is that procedures need to be defined. Two questions must answered: what is the procedure meant to do? The code that does that will form the body of the procedure. Also, what, if any, items of data will have to be provided to the procedure to make it possible to calculate its result?


As an example, imagine that we want to have a procedure that calculates the square (`` x * x ``) of a given number. In Lisp, expressions are written with the operator first and then the operands: `` (* x x) ``. That is the body of the procedure. Now, what data will we have to provide to the procedure to make it work? A value stored in the variable `` x `` will do. It's only a single variable, but by custom we need to put it in a list: `` (x) ``. The operator that defines procedures is called `` lambda ``, and we define the function with `` (lambda (x) (* x x)) ``.


One more step is needed before we can use the procedure. It must have a name. We could define it like this: `` (define square (lambda (x) (* x x))) `` but there is actually a shortcut notation for it: `` (define (square x) (* x x)) ``.


Now, `` square `` is pretty tame. How about the `` hypotenuse `` procedure? `` (define (hypotenuse a b) (sqrt (+ (square a) (square b)))) ``. It calculates the square root of the sum of two squares.


Under the hood, the helper `` /lambda `` makes a [Procedure](https://github.com/hoodiecrow/ConsTcl#control) object. First it needs to convert the Lisp list `` body ``. It is packed inside a `` begin `` if it has more than one expression (`` S begin `` stands for "the symbol begin".), and taken out of its list if not. The Lisp list `` formals `` is passed on as it is.

##### Scheme formals lists: an aside

A Scheme formals list is either:

*  An _empty list_, `` () ``, meaning that no arguments are accepted,
*  A _proper list_, `` (a b c) ``, meaning it accepts three arguments, one in each symbol,
*  A _symbol_, `` a ``, meaning that all arguments go into `` a ``, or
*  A _dotted list_, `` (a b . c) ``, meaning that two arguments go into `` a `` and `` b ``, and the rest into `` c ``.

__/lambda__ procedure

<table border=1><thead><tr><th colspan=2 align="left">/lambda (internal)</th></tr></thead><tr><td>formals</td><td>a Scheme formals list</td></tr><tr><td>body</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a procedure</td></tr></table>

```
proc ::constcl::/lambda {formals body env} {
  if {[[length $body] value] > 1} {
    set body [cons [S begin] $body]
  } else {
    set body [car $body]
  }
  return [MkProcedure $formals $body $env]
}
```
#### Procedure call


_Example: `` (+ 1 6) `` => 7_


Once we have procedures, we can call them to have their calculations performed and yield results. The procedure name is put in the operator position at the front of a list, and the operands follow in the rest of the list. Our `` square `` procedure would be called for instance like this: `` (square 11) ``, and it will return 121.


`` invoke `` arranges for a procedure to be called with each of the values in the _argument list_ (the list of operands). It checks if pr really is a procedure, and determines whether to call pr as an object or as a Tcl command.


__invoke__ procedure

<table border=1><thead><tr><th colspan=2 align="left">invoke (internal)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>what pr returns</td></tr></table>

```
proc ::constcl::invoke {pr vals} {
  check {procedure? $pr} {
    PROCEDURE expected\n([$pr show] val ...)
  }
  if {[info object isa object $pr]} {
    $pr call {*}[splitlist $vals]
  } else {
    $pr {*}[splitlist $vals]
  }
}
```


__splitlist__ procedure


`` splitlist `` converts a Lisp list to a Tcl list with Lisp objects.

<table border=1><thead><tr><th colspan=2 align="left">splitlist (internal)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of Lisp values</td></tr></table>

```
proc ::constcl::splitlist {vals} {
  set result {}
  while {[pair? $vals] ne "#f"} {
    lappend result [car $vals]
    set vals [cdr $vals]
  }
  return $result
}
```


__eval-list__ procedure


`` eval-list `` successively evaluates the elements of a Lisp list and returns the collected results as a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">eval-list (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
proc ::constcl::eval-list {exps env} {
  # don't convert to /if, it breaks (fact 100)
  if {[pair? $exps] ne "#f"} {
    return [cons [eval [car $exps] $env] \
      [eval-list [cdr $exps] $env]]
  } {
    return #NIL
  }
}
```
### Macros


__expand-macro__ procedure


Macros that allow concise, abstract expressions that are automatically rewritten into other, more concrete but also more verbose expressions is one of Lisp's strong points. This interpreter does macro expansion, but the user can't define new macros--the ones available are hardcoded in the code below.


`` expand-macro `` takes an expression and an environment as a parameter. First, the operator (`` op ``) and operands (`` args ``) are extracted to check if expansion is necessary (the operator `` car ``, for historical reasons, stands for the first element of a list, while `` cdr `` stands for the rest of the elements after the first in a list). If the operator is the symbol `` define `` and the first of the operands is something other than a Pair, then expansion is unnecessary and the procedure returns with a code to break the while loop in `` eval ``.


The operator's symbol name is then used to select the right expansion procedure, and the whole expression and the environment is passed to it. In the end, the expanded expression is passed back to `` eval ``.

<table border=1><thead><tr><th colspan=2 align="left">expand-macro (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-macro {expr env} {
  set op [car $expr]
  set args [cdr $expr]
  if {[$op name] eq "define" &&
      [pair? [car $args]] eq "#f"} {
    return -code break
  }
  return [expand-[$op name] $expr $env]
}
```


__expand-and__ procedure


`` expand-and `` expands the `` and `` macro. It returns a `` begin ``-expression if the macro has 0 or 1 elements, and a nested `` if `` construct otherwise.

<table border=1><thead><tr><th colspan=2 align="left">expand-and (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro and

proc ::constcl::expand-and {expr env} {
  set tail [cdr $expr]
  if {[[length $tail] numval] == 0} {
    list [S begin] #t
  } elseif {[[length $tail] numval] == 1} {
    cons [S begin] $tail
  } else {
    do-and $tail #t $env
  }
}
```


__do-and__ procedure


`` do-and `` is called recursively for every argument of `` expand-or `` if there are more than one.

##### Quasiquote: an aside


In this and many other macro expanders I use a quasiquote construct to lay out how the macro is to be expanded. A quasiquote starts with a backquote (`` ` ``) instead of the single quote that precedes regular quoted material. A quasiquote allows for "unquoting" of selected parts: this is notated with a comma (`` , ``). `` `(foo ,bar baz) `` is very nearly the same as `` ('foo bar 'baz) ``. In both cases `` foo `` and `` baz `` are constants while `` bar `` is a variable which will be evaluated. Like in `` do-and `` here, a quasiquote serves well as a templating mechanism. The variables in the quasiquote need to be a part of the environment in which the quasiquote is expanded: I use `` /define `` to bind them in a temporary environment.

<table border=1><thead><tr><th colspan=2 align="left">do-and (internal)</th></tr></thead><tr><td>tail</td><td>an expression tail</td></tr><tr><td>prev</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::do-and {tail prev env} {
  if {[null? $tail] ne "#f"} {
    return $prev
  } else {
    set env [Environment new #NIL {} $env]
    /define [S first] [car $tail] $env
    /define [S rest] [do-and [cdr $tail] \
        [car $tail] $env] $env
    set qq "`(if ,first ,rest #f)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
```


__expand-case__ procedure


The body of the `` case `` form consists of a key-expression and a number of clauses. Each clause has a list of values and a body. If the key-expression evaluates to a value that occurs in one of the value-lists (considered in order), that clause's body is evaluated and all other clauses are ignored.


The `` case `` macro is expanded by `` expand-case ``. It expands to `` '() `` if there are no clauses (left), and to nested `` if `` constructs if there are some.

##### caar, cadr, cdar, and the rest: an aside


The `` do-case `` procedure uses extensions of the `` car ``/`` cdr `` operators like `` caar `` and `` cdar ``. `` car ``/`` cdr `` notation gets really powerful when combined to form operators from `` caar `` to `` cddddr ``. One can read `` caar L `` as "the first element of the first element of L", implying that the first element of `` L `` is a list. `` cdar L `` is "the rest of the elements of the first element of L", and `` cadr L `` is "the first element of the rest of the elements of L" or in layman's terms, the second element of L.

<table border=1><thead><tr><th colspan=2 align="left">expand-case (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro case

proc ::constcl::expand-case {expr env} {
  set tail [cdr $expr]
  do-case [car $tail] [cdr $tail] $env
}

proc ::constcl::do-case {keyexpr clauses env} {
  if {[null? $clauses] ne "#f"} {
    return [parse "'()"]
  } else {
    set keyl [caar $clauses]
    set body [cdar $clauses]
    set keyl [list [S memv] $keyexpr \
        [list [S quote] $keyl]]
    # if this is the last clause...
    if {[eq? [length $clauses] #1] ne "#f"} {
      # ...allow 'else' in the condition
      if {[eq? [caar $clauses] [S else]] ne "#f"} {
        set keyl #t
      }
    }
    set env [Environment new #NIL {} $env]
    /define [S keyl] $keyl $env
    /define [S body] $body $env
    /define [S rest] [
      do-case $keyexpr [cdr $clauses] $env] $env
    set qq "`(if ,keyl
               (begin ,@body)
               ,rest)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
```


__expand-cond__ procedure


The `` cond `` form has a list of clauses, each with a predicate and a body. The clauses is considered in order, and if a predicate evaluates to something other than `` #f `` the body is evaluated and the remaining clauses are ignored.


The `` cond `` macro is expanded by `` expand-cond ``. It expands to `` '() `` if there are no clauses (left), and to nested `` if `` constructs if there are some.

<table border=1><thead><tr><th colspan=2 align="left">expand-cond (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro cond

proc ::constcl::expand-cond {expr env} {
  return [do-cond [cdr $expr] $env]
}
```


__do-cond__ procedure


`` do-cond `` is called recursively for every clause of the `` cond `` form. It chops up the clause into predicate and body, stepping over any `` => `` symbols in between. In the last clause, the predicate is allowed to be `` else `` (which gets translated to `` #t ``. If there is no body, the body is set to the predicate. The macro is expanded to a recursive `` if `` form.

<table border=1><thead><tr><th colspan=2 align="left">do-cond (internal)</th></tr></thead><tr><td>tail</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::do-cond {tail env} {
  set clauses $tail
  if {[null? $clauses] ne "#f"} {
    return [parse "'()"]
  } else {
    set pred [caar $clauses]
    set body [cdar $clauses]
    if {[symbol? [car $body]] ne "#f" &&
        [[car $body] name] eq "=>"} {
      set body [cddar $clauses]
    }
    # if this is the last clause...
    if {[eq? [length $clauses] #1] ne "#f"} {
      # ...allow 'else' in the predicate
      if {[eq? $pred [S else]] ne "#f"} {
        set pred #t
      }
    }
    if {[null? $body] ne "#f"} {
        set body $pred
    }
    set env [Environment new #NIL {} $env]
    /define [S pred] $pred $env
    /define [S body] $body $env
    /define [S rest] [
      do-cond [cdr $clauses] $env] $env
    set qq "`(if ,pred
               (begin ,@body)
               ,rest)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
```


__expand-define__ procedure


`` define `` has two variants, one of which requires some rewriting. It's the one with an implied `` lambda `` call, the one that defines a procedure.


(define (_symbol_ _formals_) _body_)


is transformed into


(define _symbol_ (lambda _formals_ _body_))


which conforms better to `` eval ``'s standard of (define _symbol_ _value_).

<table border=1><thead><tr><th colspan=2 align="left">expand-define (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro define

proc ::constcl::expand-define {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  /define [S tail] $tail $env
  set qq "`(define ,(caar tail)
             (lambda ,(cdar tail) ,@(cdr tail)))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
```


__expand-del!__ procedure


The macro `` del! `` updates a property list. It removes a key-value pair if the key is present, or leaves the list untouched if it isn't.

<table border=1><thead><tr><th colspan=2 align="left">expand-del! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro del!

proc ::constcl::expand-del! {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
    ::error "too few arguments, 0 of 2"
  }
  /define [S listname] [car $tail] $env
  if {[null? [cdr $tail]] ne "#f"} {
    ::error "too few arguments, 1 of 2"
  }
  /define [S key] [cadr $tail] $env
  set qq "`(set! ,listname
             (delete! ,listname ,key))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
```


__expand-for__ procedure


The `` expand-for `` procedure expands the `` for `` macro. It returns a `` begin `` construct containing the iterations of each clause (multiple clauses weren't implemented for the longest time, but I brought up my strongest brain cells and they did it).

<table border=1><thead><tr><th colspan=2 align="left">expand-for (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro for

proc ::constcl::expand-for {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  lappend res [parse "'()"]
  return [list [S begin] {*}$res]
}
```


__for-seq__ procedure


`` for-seq `` is a helper procedure that sets up the sequence of values that the iteration is based on. First it evaluates the code that generates the sequence, and then it converts it to a Tcl list.

<table border=1><thead><tr><th colspan=2 align="left">for-seq (internal)</th></tr></thead><tr><td>seq</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of Lisp values</td></tr></table>

```
proc ::constcl::for-seq {seq env} {
  if {[number? $seq] ne "#f"} {
    set seq [in-range $seq]
  } else {
    set seq [eval $seq $env]
  }
  # make it a Tcl list, one way or another
  if {[list? $seq] ne "#f"} {
    set seq [splitlist $seq]
  } elseif {[string? $seq] ne "#f"} { 
    set seq [lmap c [split [$seq value] {}] {
      MkChar #\\$c
    }]
  } elseif {[vector? $seq] ne "#f"} {
    set seq [$seq value]
  }
}
```


__do-for__ procedure


`` do-for `` is another helper procedure which does most of the work in the `` for/* `` forms. It iterates over the clauses, extracting and preparing the sequence for each, and stores each of the sequence steps in a dictionary under a double key: the identifier and the ordinal of the step.


Then it creates a `` let `` construct for each step, in which each of the clauses' identifiers is bound to the step's value. The Tcl list of `` let `` constructs is returned.


Each clause's sequence is supposed to be the same length as the others. One weakness of this implementation is that it doesn't ensure this, just hopes that the user does the right thing.

<table border=1><thead><tr><th colspan=2 align="left">do-for (internal)</th></tr></thead><tr><td>tail</td><td>an expression tail</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of expressions</td></tr></table>

```
proc ::constcl::do-for {tail env} {
  # make clauses a Tcl list
  set clauses [splitlist [car $tail]]
  set body [cdr $tail]
  set data [dict create]
  set length 0
  foreach clause $clauses {
    set id [car $clause]
    set sequence [for-seq [cadr $clause] $env]
    set length [llength $sequence]
    # save every id and step of the iteration
    for {set i 0} {$i < $length} {incr i} {
        dict set data $id $i [lindex $sequence $i]
    }
  }
  set res {}
  # for every step of the iteration...
  for {set i 0} {$i < $length} {incr i} {
    set decl {}
    # retrieve the ids
    foreach id [dict keys $data] {
      # list the id and the step
      lappend decl [
        list $id [dict get $data $id $i]]
    }
    # add to the structure of let constructs
    lappend res [list [S let] [
        list {*}$decl] {*}[splitlist $body]]
  }
  return $res
}
```


__expand-for/and__ procedure


The `` expand-for/and `` procedure expands the `` for/and `` macro. It returns an `` and `` construct containing the iterations of the clauses.


The only differences from `` expand-for `` is that it doesn't add `` (quote ()) `` and that it wraps the list of iterations in `` and `` instead of `` begin ``.

<table border=1><thead><tr><th colspan=2 align="left">expand-for/and (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro for/and

proc ::constcl::expand-for/and {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S and] {*}$res]
}
```


__expand-for/list__ procedure


The `` expand-for/list `` procedure expands the `` for/list `` macro. It returns a `` list `` construct containing the iterations of each clause.


The only difference from `` expand for/and `` is that it wraps the list of iterations in `` list `` instead of `` and ``.

<table border=1><thead><tr><th colspan=2 align="left">expand for/list (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro for/list

proc ::constcl::expand-for/list {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S list] {*}$res]
}
```


__expand-for/or__ procedure


The `` expand-for/or `` procedure expands the `` for/or `` macro. It returns an `` or `` construct containing the iterations of each clause.


The only difference from `` expand for/list `` is that it wraps the list of iterations in `` or `` instead of `` list ``.

<table border=1><thead><tr><th colspan=2 align="left">expand-for/or (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro for/or

proc ::constcl::expand-for/or {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S or] {*}$res]
}
```


__expand-let__ procedure


`` expand-let `` expands the named `` let `` and 'regular' `` let `` macros. They ultimately expand to `` lambda `` constructs.


Named `` let `` chops up the expression into variable, bindings, and body. It creates a dictionary with the variable as key and `` #f `` as value. Then it fills up the dictionary with variable/value pairs from the bindings. It uses the dictionary to build a declaration list for a `` let `` form, a variable list for a `` lambda `` form, and a procedure call. Then it assembles a `` let `` form with the declaration list and a body consisting of an assignment and the procedure call. The assignment binds the variable to a `` lambda `` for with the varlist and the original body. The `` let `` form is returned, meaning that the primary expansion of the named `` let `` is a regular `` let `` form.


Regular `` let `` chops up the original expression into bindings and body. It creates an empty dictionary and fills it up with variable/value pairs from the bindings. Then it builds a `` lambda `` operator form with the variable list, the body, and the value list. The `` lambda `` call is returned as the expansion of the regular `` let `` form.

<table border=1><thead><tr><th colspan=2 align="left">expand-let (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro let

proc ::constcl::expand-let {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[symbol? [car $tail]] ne "#f"} {
    # named let
    set variable [car $tail]
    set bindings [cadr $tail]
    set body [cddr $tail]
    set vars [dict create $variable #f]
    parse-bindings vars $bindings
    /define [S decl] [list {*}[dict values [
      dict map {k v} $vars {list $k $v}]]] $env
    /define [S variable] $variable $env
    /define [S varlist] [list {*}[lrange [
      dict keys $vars] 1 end]] $env
    /define [S body] $body $env
    /define [S call] [list {*}[
      dict keys $vars]] $env
    set qq "`(let ,decl
               (set!
                 ,variable
                 (lambda ,varlist ,@body)) ,call)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  } else {
    # regular let
    set bindings [car $tail]
    set body [cdr $tail]
    set vars [dict create]
    parse-bindings vars $bindings
    /define [S varlist] [list {*}[
      dict keys $vars]] $env
    /define [S body] $body $env
    /define [S vallist] [list {*}[
      dict values $vars]] $env
    set qq "`((lambda ,varlist ,@body)
               ,@vallist)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
```


__parse-bindings__ procedure


`` parse-bindings `` is a helper procedure that traverses a `` let `` bindings list and extracts variables and values, which it puts in a dictionary. It throws an error if a variable occurs more than once.

<table border=1><thead><tr><th colspan=2 align="left">parse-bindings (internal)</th></tr></thead><tr><td>name</td><td>a call-by-name name</td></tr><tr><td>bindings</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::parse-bindings {name bindings} {
  upvar $name vars
  foreach binding [splitlist $bindings] {
    set var [car $binding]
    set val [cadr $binding]
    if {$var in [dict keys $vars]} {
        ::error "'$var' occurs more than once"
    }
    dict set vars $var $val
  }
  return
}
```


__expand-or__ procedure


`` expand-or `` expands the `` or `` macro. It returns a `` begin ``-expression if the macro has 0 or 1 elements, and a nested `` if `` construct otherwise.

<table border=1><thead><tr><th colspan=2 align="left">expand-or (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro or

proc ::constcl::expand-or {expr env} {
  set tail [cdr $expr]
  if {[eq? [length $tail] #0] ne "#f"} {
    return [list [S begin] #f]
  } elseif {[eq? [length $tail] #1] ne "#f"} {
    return [cons [S begin] $tail]
  } else {
    return [do-or $tail $env]
  }
}
```


__do-or__ procedure


`` do-or `` is called recursively for each argument to `` expand-or `` if there are more than one argument.

<table border=1><thead><tr><th colspan=2 align="left">do-or (internal)</th></tr></thead><tr><td>tail</td><td>an expression tail</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::do-or {tail env} {
  /if {null? $tail} {
    return #f
  } {
    set env [Environment new #NIL {} $env]
    /define [S first] [car $tail] $env
    /define [S rest] [do-or [cdr $tail] $env] $env
    set qq "`(let ((x ,first)) (if x x ,rest))"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
```


__expand-pop!__ procedure


The macro `` pop! `` updates a list. It removes the first element.

<table border=1><thead><tr><th colspan=2 align="left">expand-pop! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro pop!

proc ::constcl::expand-pop! {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
      ::error "too few arguments:\n(pop! listname)"
  }
  if {[symbol? [car $tail]] eq "#f"} {
      ::error "SYMBOL expected:\n(pop! listname)"
  }
  /define [S listname] [car $tail] $env
  set qq "`(set! ,listname (cdr ,listname))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
```


__expand-push!__ procedure


The macro `` push! `` updates a list. It adds a new element as the new first element.

<table border=1><thead><tr><th colspan=2 align="left">expand-push! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro push!

proc ::constcl::expand-push! {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
    ::error \
      "too few arguments:\n(push! obj listname)"
  }
  /define [S obj] [car $tail] $env
  if {[null? [cdr $tail]] ne "#f"} {
    ::error \
      "too few arguments:\n(push! obj listname)"
  }
  if {[symbol? [cadr $tail]] eq "#f"} {
    ::error \
      "SYMBOL expected:\n(push! obj listname)"
  }
  /define [S listname] [cadr $tail] $env
  set qq "`(set!
             ,listname
             (cons ,obj ,listname))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
```


__expand-put!__ procedure


The macro `` put! `` updates a property list. It adds a key-value pair if the key isn't present, or changes the value in place if it is.

<table border=1><thead><tr><th colspan=2 align="left">expand-put! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro put!

proc ::constcl::expand-put! {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
      ::error "too few arguments, 0 of 3"
  }
  /define [S name] [car $tail] $env
  if {[null? [cdr $tail]] ne "#f"} {
      ::error "too few arguments, 1 of 3"
  }
  /define [S key] [cadr $tail] $env
  if {[null? [cddr $tail]] ne "#f"} {
      ::error "too few arguments, 2 of 3"
  }
  /define [S val] [caddr $tail] $env
  set qq "`(let ((idx (list-find-key ,name ,key)))
             (if (< idx 0)
               (set! 
                 ,name
                 (append (list ,key ,val) ,name))
               (begin
                 (list-set! ,name (+ idx 1) ,val)
                 ,name)))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
```


__expand-quasiquote__ procedure


A quasi-quote isn't a macro, but we will deal with it in this section anyway. `` expand-quasiquote `` traverses the quasi-quoted structure searching for `` unquote `` and `` unquote-splicing ``. This code is brittle and sprawling and I barely understand it myself.

<table border=1><thead><tr><th colspan=2 align="left">expand-quasiquote (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro quasiquote

proc ::constcl::expand-quasiquote {expr env} {
  set tail [cdr $expr]
  set qqlevel 0
  if {[list? [car $tail]] ne "#f"} {
    set node [car $tail]
    return [qq-visit-child $node 0 $env]
  } elseif {[vector? [car $tail]] ne "#f"} {
    set vect [car $tail]
    set res {}
    for {set i 0} {$i < [
        [vector-length $vect] numval]} {incr i} {
      set idx [MkNumber $i]
      set vecref [vector-ref $vect $idx]
      if {[pair? $vecref] ne "#f" &&
          [eq? [car $vecref] [
            S unquote]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res [eval [cadr $vecref] $env]
        }
      } elseif {[pair? $vecref] ne "#f" &&
          [eq? [car $vecref] [
            S unquote-splicing]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res {*}[splitlist [
            eval [cadr $vecref] $env]]
        }
      } elseif {[atom? $vecref] ne "#f"} {
        lappend res $vecref
      } else {
      }
    }
    return [list [S "vector"] {*}$res]
  }
}
```
<table border=1><thead><tr><th colspan=2 align="left">qq-visit-child (internal)</th></tr></thead><tr><td>node</td><td>a Lisp list of expressions</td></tr><tr><td>qqlevel</td><td>a Tcl number</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of expressions</td></tr></table>

```
proc ::constcl::qq-visit-child {node qqlevel env} {
  if {$qqlevel < 0} {
    set qqlevel 0
  }
  if {[list? $node] ne "#f"} {
    set res {}
    foreach child [splitlist $node] {
      if {[pair? $child] ne "#f" &&
          [eq? [car $child] [S unquote]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res [eval [cadr $child] $env]
        } else {
          lappend res [list [S unquote] [
            qq-visit-child [cadr $child] [
            expr {$qqlevel - 1}] $env]]
        }
      } elseif {[pair? $child] ne "#f" &&
          [eq? [car $child] [
          S unquote-splicing]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res {*}[splitlist [
            eval [cadr $child] $env]]
        }
      } elseif {[pair? $child] ne "#f" &&
          [eq? [car $child] [S quasiquote]] ne "#f"} {
        lappend res [list [S quasiquote] [car [
          qq-visit-child [cdr $child] [
            expr {$qqlevel + 1}] $env]]] 
      } elseif {[atom? $child] ne "#f"} {
        lappend res $child
      } else {
        lappend res [
          qq-visit-child $child $qqlevel $env]
      }
    }
  }
  return [list {*}$res]
}
```


__expand-unless__ procedure


`` unless `` is a conditional like `` if ``, with the differences that it takes a number of expressions and only executes them for a false outcome of `` car $tail ``.

<table border=1><thead><tr><th colspan=2 align="left">expand-unless (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro unless

proc ::constcl::expand-unless {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  /define [S tail] $tail $env
  set qq "`(if ,(car tail)
             '()
             (begin ,@(cdr tail)))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
```


__expand-when__ procedure


`` when `` is a conditional like `` if ``, with the differences that it takes a number of expressions and only executes them for a true outcome of `` car $tail ``.

<table border=1><thead><tr><th colspan=2 align="left">expand-when (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro when

proc ::constcl::expand-when {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  /define [S tail] $tail $env
  set qq "`(if ,(car tail)
             (begin ,@(cdr tail))
             '())"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
```
### Resolving local defines


This section is ported from 'Scheme 9 from Empty Space'. `` resolve-local-defines `` is the topmost procedure in rewriting local defines as essentially a `` letrec `` form. It takes a list of expressions and extracts variables and values from the defines in the beginning of the list. It builds a double lambda expression with the variables and values, and the rest of the expressions from the original list as body.


__resolve-local-defines__ procedure

<table border=1><thead><tr><th colspan=2 align="left">resolve-local-defines</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::resolve-local-defines {exps} {
  set rest [lassign [
    extract-from-defines $exps VALS] a error]
  if {$error ne "#f"} {
    return #NIL
  }
  set rest [lassign [
    extract-from-defines $exps VARS] v error]
  if {$error ne "#f"} {
    return #NIL
  }
  if {$rest eq "#NIL"} {
    set rest [cons #UNSP #NIL]
  }
  return [make-lambdas $v $a $rest]
}
```


__extract-from-defines__ procedure


`` extract-from-defines `` visits every define in the given list of expressions and extracts either a variable name or a value, depending on the state of the _part_ flag, from each one of them. A Tcl list of 1) the resulting list of names or values, 2) error state, and 3) the rest of the expressions in the original list is returned.

<table border=1><thead><tr><th colspan=2 align="left">extract-from-defines (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>part</td><td>a flag, VARS or VALS</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of Lisp values</td></tr></table>

```
proc ::constcl::extract-from-defines {exps part} {
  set a #NIL
  while {$exps ne "#NIL"} {
    if {[atom? $exps] ne "#f" ||
        [atom? [car $exps]] ne "#f" ||
        [eq? [caar $exps] [S define]] eq "#f"} {
      break
    }
    set n [car $exps]
    set k [length $n]
    if {[list? $n] eq "#f" ||
        [$k numval] < 3 ||
        [$k numval] > 3 ||
        ([argument-list? [cadr $n]] ne "#f" ||
        [symbol? [cadr $n]] eq "#f")
      eq "#f"} {
        return [::list #NIL "#t" #NIL]
      }
      if {[pair? [cadr $n]] ne "#f"} {
        if {$part eq "VARS"} {
          set a [cons [caadr $n] $a]
        } else {
          set a [cons #NIL $a]
          set new [cons [cdadr $n] [cddr $n]]
          set new [cons [S lambda] $new]
          set-car! $a $new
        }
      } else {
        if {$part eq "VARS"} {
          set a [cons [cadr $n] $a]
        } else {
          set a [cons [caddr $n] $a]
        }
      }
      set exps [cdr $exps]
    }
    return [::list $a #f $exps]
}
```


__argument-list?__ procedure


`` argument-list? `` accepts a Scheme formals list and rejects other values.

<table border=1><thead><tr><th colspan=2 align="left">argument-list? (internal)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
proc ::constcl::argument-list? {val} {
  if {$val eq "#NIL"} {
    return #t
  } elseif {[symbol? $val] ne "#f"} {
    return #t
  } elseif {[atom? $val] ne "#f"} {
    return #f
  }
  while {[pair? $val] ne "#f"} {
    if {[symbol? [car $val]] eq "#f"} {
      return #f
    }
    set val [cdr $val]
  }
  if {$val eq "#NIL"} {
    return #t
  } elseif {[symbol? $val] ne "#f"} {
    return #t
  }
}
```


__make-lambdas__ procedure


`` make-lambdas `` builds the `` letrec `` structure.

<table border=1><thead><tr><th colspan=2 align="left">make-lambdas (internal)</th></tr></thead><tr><td>vars</td><td>a Lisp list of symbols</td></tr><tr><td>args</td><td>a Lisp list of expressions</td></tr><tr><td>body</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::make-lambdas {vars args body} {
  set tmps [make-temporaries $vars]
  set body [append-b [
    make-assignments $vars $tmps] $body]
  set body [cons $body #NIL]
  set n [cons $tmps $body]
  set n [cons [S lambda] $n]
  set n [cons $n $args]
  set n [cons $n #NIL]
  set n [cons $vars $n]
  set n [cons [S lambda] $n]
  set n [cons $n [make-undefineds $vars]]
  return $n
}
```


__make-temporaries__ procedure


`` make-temporaries `` creates the symbols that will act as middlemen in transferring the values to the variables.

<table border=1><thead><tr><th colspan=2 align="left">make-temporaries (internal)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
proc ::constcl::make-temporaries {vals} {
  set res #NIL
  while {$vals ne "#NIL"} {
    set res [cons [gensym "g"] $res]
    set vals [cdr $vals]
  }
  return $res
}
```


__gensym__ procedure


`` gensym `` generates a unique symbol. The candidate symbol is compared to all the symbols in the symbol table to avoid collisions.

<table border=1><thead><tr><th colspan=2 align="left">gensym (internal)</th></tr></thead><tr><td>prefix</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a symbol</td></tr></table>

```
proc ::constcl::gensym {prefix} {
  set symbolnames [
    dict keys $::constcl::symbolTable]
  set s $prefix<[incr ::constcl::gensymnum]>
  while {$s in $symbolnames} {
    set s $prefix[incr ::constcl::gensymnum]
  }
  return [S $s]
}
```


__append-b__ procedure


`` append-b `` joins two lists together.

<table border=1><thead><tr><th colspan=2 align="left">append-b (internal)</th></tr></thead><tr><td>a</td><td>a Lisp list of Lisp values</td></tr><tr><td>b</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
proc ::constcl::append-b {a b} {
  if {$a eq "#NIL"} {
    return $b
  }
  set p $a
  while {$p ne "#NIL"} {
    if {[atom? $p] ne "#f"} {
      ::error "append: improper list"
    }
    set last $p
    set p [cdr $p]
  }
  set-cdr! $last $b
  return $a
}
```


__make-assignments__ procedure


`` make-assignments `` creates the structure that holds the assignment statements. Later on, it will be joined to the body of the finished expression.

<table border=1><thead><tr><th colspan=2 align="left">make-assignments (internal)</th></tr></thead><tr><td>vars</td><td>a Lisp list of symbols</td></tr><tr><td>tmps</td><td>a Lisp list of symbols</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::make-assignments {vars tmps} {
  set res #NIL
  while {$vars ne "#NIL"} {
    set asg [cons [car $tmps] #NIL]
    set asg [cons [car $vars] $asg]
    set asg [cons [S set!] $asg]
    set res [cons $asg $res]
    set vars [cdr $vars]
    set tmps [cdr $tmps]
  }
  return [cons [S begin] $res]
}
```


__make-undefineds__ procedure


Due to a mysterious bug, `` make-undefineds `` actually creates a list of NIL values instead of undefined values.

<table border=1><thead><tr><th colspan=2 align="left">make-undefineds (internal)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of nil values</td></tr></table>

```
proc ::constcl::make-undefineds {vals} {
  # TODO find bug, substitute #UNDF
  set res #NIL
  while {$vals ne "#NIL"} {
    set res [cons #NIL $res]
    set vals [cdr $vals]
  }
  return $res
}
```
## Output


__write__ procedure


The third thing an interpreter must be able to do is to present the resulting code and data so that the user can know what the outcome was.


As long as the object given to `` write `` isn't the empty string, it calls the object's `` write `` method and writes a newline.

<table border=1><thead><tr><th colspan=2 align="left">write (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td>?port?</td><td>a port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg write ::constcl::write

proc ::constcl::write {val args} {
  if {$val ne ""} {
    if {[llength $args]} {
      lassign $args port
      set dealloc 0
    } else {
      set port [MkOutputPort stdout]
      set dealloc 1
    }
    set ::constcl::Output_port $port
    $val write [$::constcl::Output_port handle]
    puts [$::constcl::Output_port handle] {}
    set ::constcl::Output_port [MkOutputPort stdout]
    if {$dealloc} {$port destroy}
  }
  return
}
```


__display__ procedure


The `` display `` procedure is like `` write `` but it calls the method's `` display `` method and doesn't print a newline.

<table border=1><thead><tr><th colspan=2 align="left">display (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td>?port?</td><td>a port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg display ::constcl::display

proc ::constcl::display {val args} {
  if {$val ne ""} {
    if {[llength $args]} {
      lassign $args port
      set dealloc 0
    } else {
      set port [MkOutputPort stdout]
      set dealloc 1
    }
    set ::constcl::Output_port $port
    $val display [$::constcl::Output_port handle]
    flush [$::constcl::Output_port handle]
    set ::constcl::Output_port [MkOutputPort stdout]
    if {$dealloc} {$port destroy}
  }
  return
}
```


__write-pair__ procedure


The `` write-pair `` procedure prints a Pair object.

<table border=1><thead><tr><th colspan=2 align="left">write-pair (internal)</th></tr></thead><tr><td>handle</td><td>a channel handle</td></tr><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::write-pair {handle pair} {
  # take an object and print the car
  # and the cdr of the stored value
  set a [car $pair]
  set d [cdr $pair]
  # print car
  $a write $handle
  if {[pair? $d] ne "#f"} {
    # cdr is a cons pair
    puts -nonewline $handle " "
    write-pair $handle $d
  } elseif {[null? $d] ne "#f"} {
    # cdr is nil
    return
  } else {
    # it is an atom
    puts -nonewline $handle " . "
    $d write $handle
  }
  return
}
```
## Built-in procedures
### Equivalence predicates


Of the three equivalence predicates, `` eq `` generally tests for identity (with exception for numbers), `` eqv `` tests for value equality (except for booleans and procedures, where it tests for identity), and `` equal `` tests for whether the output strings are equal.


__eq?__ procedure

<table border=1><thead><tr><th colspan=2 align="left">eq?, eqv?, equal? (public)</th></tr></thead><tr><td>expr1</td><td>an expression</td></tr><tr><td>expr2</td><td>an expression</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg eq?

proc ::constcl::eq? {expr1 expr2} {
  if {[teq boolean? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } elseif {[teq symbol? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } elseif {[teq number? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return #t
  } elseif {[teq char? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } elseif {[teq null? $expr1 $expr2]} {
    return #t
  } elseif {[teq pair? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } elseif {[teq string? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } elseif {[teq vector? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } elseif {[teq procedure? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } else {
    return #f
  }
}
```


__teq__ procedure


`` teq `` tests for type equality, i.e. that the expressions have the same type.

<table border=1><thead><tr><th colspan=2 align="left">teq (internal)</th></tr></thead><tr><td>typep</td><td>a procedure</td></tr><tr><td>expr1</td><td>an expression</td></tr><tr><td>expr2</td><td>an expression</td></tr><tr><td><i>Returns:</i></td><td>a Tcl truth value (1 or 0)</td></tr></table>

```
proc ::constcl::teq {typep expr1 expr2} {
    return [expr {[$typep $expr1] ne "#f" &&
      [$typep $expr2] ne "#f"}]
}
```


__veq__ procedure


`` veq `` tests for value equality, i.e. that the expressions have the same value.

<table border=1><thead><tr><th colspan=2 align="left">veq (internal)</th></tr></thead><tr><td>expr1</td><td>an expression</td></tr><tr><td>expr2</td><td>an expression</td></tr><tr><td><i>Returns:</i></td><td>a Tcl truth value (1 or 0)</td></tr></table>

```
proc ::constcl::veq {expr1 expr2} {
    return [expr {[$expr1 value] eq [$expr2 value]}]
}
```


__eqv?__ procedure

```
reg eqv?

proc ::constcl::eqv? {expr1 expr2} {
  if {[teq boolean? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } elseif {[teq symbol? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return #t
  } elseif {[teq number? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return #t
  } elseif {[teq char? $expr1 $expr2] &&
      [veq $expr1 eq $expr2]} {
    return #t
  } elseif {[teq null? $expr1 $expr2]} {
    return #t
  } elseif {[pair? $expr1] ne "#f" &&
      [pair? $expr2] ne "#f" &&
      [$expr1 car] eq [$expr2 car] &&
      [$expr1 cdr] eq [$expr2 cdr]} {
    return #t
  } elseif {[teq string? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return #t
  } elseif {[teq vector? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return #t
  } elseif {[teq procedure? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return #t
  } else {
    return #f
  }
}
```


__equal?__ procedure

```
reg equal?

proc ::constcl::equal? {expr1 expr2} {
  if {[$expr1 show] eq [$expr2 show]} {
    return #t
  } else {
    return #f
  }
  # TODO
}
```
### Numbers


I have only implemented a bare-bones version of Scheme's numerical library. The following is a reasonably complete framework for operations on integers and floating-point numbers. No rationals, no complex numbers, no gcd or lcm.


__Number__ class procedure

```
oo::class create ::constcl::Number {
  superclass ::constcl::NIL
  variable value
  constructor {v} {
    if {[::string is double -strict $v]} {
      set value $v
    } else {
      ::error "NUMBER expected\n$v"
    }
  }
  method zero? {} {
    if {$value == 0} {return #t} {return #f}
  }
  method positive? {} {
    if {$value > 0} {return #t} {return #f}
  }
  method negative? {} {
    if {$value < 0} {return #t} {return #f}
  }
  method even? {} {
    if {$value % 2 == 0} {return #t} {return #f}
  }
  method odd? {} {
    if {$value % 2 == 1} {return #t} {return #f}
  }
  method value {} {
    set value
  }
  method numval {} {
    set value
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method write {handle} {
    puts -nonewline $handle [my value]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set value
  }
}

interp alias {} ::constcl::MkNumber \
  {} ::constcl::Number new
interp alias {} N {} ::constcl::Number new
```


__number?__ procedure


`` number? `` recognizes a number by object type, not by content.

<table border=1><thead><tr><th colspan=2 align="left">number? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg number?

proc ::constcl::number? {val} {
  return [typeof? $val Number]
}
```


__=__ procedure


__<__ procedure


__>__ procedure


__<=__ procedure


__>=__ procedure


The predicates `` = ``, `` < ``, `` > ``, `` <= ``, and `` >= `` are implemented.

<table border=1><thead><tr><th colspan=2 align="left">=, &lt;, &gt;, &lt;=, &gt;= (public)</th></tr></thead><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg =

proc ::constcl::= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(= num ...)"
  }
  if {[::tcl::mathop::== {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg <

proc ::constcl::< {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(< num ...)"
  }
  if {[::tcl::mathop::< {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg >

proc ::constcl::> {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(> num ...)"
  }
  if {[::tcl::mathop::> {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg <=

proc ::constcl::<= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(<= num ...)"
  }
  if {[::tcl::mathop::<= {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg >=

proc ::constcl::>= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(>= num ...)"
  }
  if {[::tcl::mathop::>= {*}$vals]} {
    return #t
  } else {
    return #f
  }
}
```


__zero?__ procedure


The `` zero? `` predicate tests if a given number is equal to zero.

<table border=1><thead><tr><th colspan=2 align="left">zero? (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg zero?

proc ::constcl::zero? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num zero?]
}
```


__positive?__ procedure


__negative?__ procedure


__even?__ procedure


__odd?__ procedure


The `` positive? ``/`` negative? ``/`` even? ``/`` odd? `` predicates test a number for those traits.

<table border=1><thead><tr><th colspan=2 align="left">positive?, negative?, even?, odd? (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg positive?

proc ::constcl::positive? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num positive?]
}
```
```
reg negative?

proc ::constcl::negative? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num negative?]
}
```
```
reg even?

proc ::constcl::even? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num even?]
}
```
```
reg odd?

proc ::constcl::odd? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num odd?]
}
```


__max__ procedure


__min__ procedure


The `` max `` function selects the largest number, and the `` min `` function selects the smallest number.


Example:

```
(max 7 1 10 3)   =>  10
(min 7 1 10 3)   =>  1
```
<table border=1><thead><tr><th colspan=2 align="left">max, min (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg max

proc ::constcl::max {num args} {
  lappend args $num
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(max num...)"
  }
  N [::tcl::mathfunc::max {*}$vals]
}
```
```
reg min

proc ::constcl::min {num args} {
  lappend args $num
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(min num...)"
  }
  N [::tcl::mathfunc::min {*}$vals]
}
```


__+__ procedure


__*__ procedure


__-__ procedure


__/__ procedure


The operators `` + ``, `` * ``, `` - ``, and `` / `` stand for the respective mathematical operations.

<table border=1><thead><tr><th colspan=2 align="left">+, * (public)</th></tr></thead><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



They take a number of operands, but at least one for `` - `` and `` / ``.

<table border=1><thead><tr><th colspan=2 align="left">-, / (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(list (+ 2 2) (* 2 2) (- 10 6) (/ 20 5))   =>  (4 4 4 4)
(+ 21 7 3)                                 =>  31
(* 21 7 3)                                 =>  441
(- 21 7 3)                                 =>  11
(/ 21 7 3)                                 =>  1
(- 5)                                      =>  -5
(/ 5)                                      =>  0.2
```
```
reg +

proc ::constcl::+ {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(+ num ...)"
  }
  N [::tcl::mathop::+ {*}$vals]
}
```
```
reg *

proc ::constcl::* {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(* num ...)"
  }
  N [::tcl::mathop::* {*}$vals]
}
```
```
reg -

proc ::constcl::- {num args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(- num ...)"
  }
  N [::tcl::mathop::- [$num numval] {*}$vals]
}
```
```
reg /

proc ::constcl::/ {num args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(/ num ...)"
  }
  N [::tcl::mathop::/ [$num numval] {*}$vals]
}
```


__abs__ procedure


The `` abs `` function yields the absolute value of a number.

<table border=1><thead><tr><th colspan=2 align="left">abs (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg abs

proc ::constcl::abs {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    return [N [expr {[$num numval] * -1}]]
  } else {
    return $num
  }
}
```


__quotient__ procedure


`` quotient `` calculates the quotient between two numbers.

<table border=1><thead><tr><th colspan=2 align="left">quotient (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(quotient 7 3)   =>  2.0
```
```
reg quotient

proc ::constcl::quotient {num1 num2} {
  set q [::tcl::mathop::/ [$num1 numval] \
    [$num2 numval]]
  if {$q > 0} {
    return [N [::tcl::mathfunc::floor $q]]
  } elseif {$q < 0} {
    return [N [::tcl::mathfunc::ceil $q]]
  } else {
    return #0
  }
}
```


__remainder__ procedure


`` remainder `` is a variant of the modulus function. (I'm a programmer, not a mathematician!)

<table border=1><thead><tr><th colspan=2 align="left">remainder (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(remainder 7 3)   =>  1
```
```
reg remainder

proc ::constcl::remainder {num1 num2} {
  set n [::tcl::mathop::% [[abs $num1] numval] \
    [[abs $num2] numval]]
  if {[$num1 negative?] ne "#f"} {
    set n -$n
  }
  return [N $n]
}
```


__modulo__ procedure

<table border=1><thead><tr><th colspan=2 align="left">modulo (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(modulo 7 3)   =>  1
```
```
reg modulo

proc ::constcl::modulo {num1 num2} {
  return [N [::tcl::mathop::% [$num1 numval] \
    [$num2 numval]]]
}
```
```
proc ::constcl::gcd {args} {
    # TODO
}
```
```
proc ::constcl::lcm {args} {
    # TODO
}
```
```
proc ::constcl::numerator {q} {
    # TODO
}
```
```
proc ::constcl::denominator {q} {
    # TODO
}
```


__floor__ procedure


__ceiling__ procedure


__truncate__ procedure


__round__ procedure


`` floor ``, `` ceiling ``, `` truncate ``, and `` round `` are different methods for converting a real number to an integer.

<table border=1><thead><tr><th colspan=2 align="left">floor, ceiling, truncate, round (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(floor 7.5)      =>  7.0
(ceiling 7.5)    =>  8.0
(truncate 7.5)   =>  7.0
(round 7.5)      =>  8
```
```
reg floor

proc ::constcl::floor {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::floor [$num numval]]
}
```
```
reg ceiling

proc ::constcl::ceiling {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::ceil [$num numval]]
}
```
```
reg truncate

proc ::constcl::truncate {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    N [::tcl::mathfunc::ceil [$num numval]]
  } else {
    N [::tcl::mathfunc::floor [$num numval]]
  }
}
```
```
reg round

proc ::constcl::round {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::round [$num numval]]
}
```
```
proc ::constcl::rationalize {x y} {
    # TODO
}
```


__exp__ procedure


__log__ procedure


__sin__ procedure


__cos__ procedure


__tan__ procedure


__asin__ procedure


__acos__ procedure


__atan__ procedure


The mathematical functions e<sup>x</sup>, natural logarithm, sine, cosine, tangent, arcsine, arccosine, and arctangent are calculated by `` exp ``, `` log ``, `` sin ``, `` cos ``, `` tan ``, `` asin ``, `` acos ``, and `` atan ``, respectively.

<table border=1><thead><tr><th colspan=2 align="left">exp, log, sin, cos, tan, asin, acos, atan (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



`` atan `` can be called both as a unary (one argument) function and a binary (two arguments) one.

<table border=1><thead><tr><th colspan=2 align="left">(binary) atan (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(let ((x (log 2))) (= 2 (exp x)))        =>  #t
(letrec ((a (/ pi 3)) (s (sin a)))
  (= a (asin s)))                        =>  #t
```
```
reg exp

proc ::constcl::exp {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::exp [$num numval]]
}
```
```
reg log

proc ::constcl::log {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::log [$num numval]]
}
```
```
reg sin

proc ::constcl::sin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::sin [$num numval]]
}
```
```
reg cos

proc ::constcl::cos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::cos [$num numval]]
}
```
```
reg tan

proc ::constcl::tan {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::tan [$num numval]]
}
```
```
reg asin

proc ::constcl::asin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::asin [$num numval]]
}
```
```
reg acos

proc ::constcl::acos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::acos [$num numval]]
}
```
```
reg atan

proc ::constcl::atan {args} {
  if {[llength $args] == 1} {
    set num [lindex $args 0]
    check {number? $num} {
        NUMBER expected\n([pn] [$num show])
    }
    N [::tcl::mathfunc::atan [$num numval]]
  } else {
    lassign $args num1 num2
    check {number? $num1} {
        NUMBER expected\n([pn] [$num1 show])
    }
    check {number? $num2} {
        NUMBER expected\n([pn] [$num2 show])
    }
    N [::tcl::mathfunc::atan2 \
      [$num1 numval] [$num2 numval]]
  }
}
```


__sqrt__ procedure


`` sqrt `` calculates the square root.

<table border=1><thead><tr><th colspan=2 align="left">sqrt (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg sqrt

proc ::constcl::sqrt {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  N [::tcl::mathfunc::sqrt [$num numval]]
}
```


__expt__ procedure


`` expt `` calculates the x to the power of y, or x<sup>y</sup>.

<table border=1><thead><tr><th colspan=2 align="left">expt (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg expt

proc ::constcl::expt {num1 num2} {
  check {number? $num1} {
      NUMBER expected\n([pn] [$num1 show] \
        [$num2 show])
  }
  check {number? $num2} {
      NUMBER expected\n([pn] [$num1 show] \
        [$num2 show])
  }
  N [::tcl::mathfunc::pow [$num1 numval] \
    [$num2 numval]]
}
```
```
proc ::constcl::make-rectangular {x1 x2} {
    # TODO
}
```
```
proc ::constcl::make-polar {x3 x4} {
    # TODO
}
```
```
proc ::constcl::real-part {z} {
    # TODO
}
```
```
proc ::constcl::imag-part {z} {
    # TODO
}
```
```
proc ::constcl::magnitude {z} {
    # TODO
}
```
```
proc ::constcl::angle {z} {
    # TODO
}
```
```
proc ::constcl::exact->inexact {z} {
    # TODO
}
```
```
proc ::constcl::inexact->exact {z} {
    # TODO
}
```


__number->string__ procedure


The procedures `` number->string `` and `` string->number `` convert between number and string with optional radix conversion.

<table border=1><thead><tr><th colspan=2 align="left">number-&gt;string (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>?radix?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(number->string 23)      =>  "23"
(number->string 23 2)    =>  "10111"
(number->string 23 8)    =>  "27"
(number->string 23 16)   =>  "17"
```
```
reg number->string

proc ::constcl::number->string {num args} {
  if {[llength $args] == 0} {
    check {number? $num} {
      NUMBER expected\n([pn] [$num show])
    }
    return [MkString [$num numval]]
  } else {
    lassign $args radix
    check {number? $num} {
      NUMBER expected\n([pn] [$num show])
    }
    check {number? $radix} {
      NUMBER expected\n([pn] [$num show] \
        [$radix show])
    }
    set radices [list [N 2] [N 8] [N 10] [N 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] \
        [$num show] [$radix show])
    }
    if {[$radix numval] == 10} {
      return [MkString [$num numval]]
    } else {
      return [MkString [base [$radix numval] \
        [$num numval]]]
    }
  }
}
```


Due to [Richard Suchenwirth](https://wiki.tcl-lang.org/page/Based+numbers).

```
proc base {base number} {
  set negative [regexp ^-(.+) $number -> number]
  set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
  set res {}
  while {$number} {
    set digit [expr {$number % $base}]
    set res [lindex $digits $digit]$res
    set number [expr {$number / $base}]
  }
  if $negative {set res -$res}
  set res
}
```


__string->number__ procedure


As with `` number->string ``, above.

<table border=1><thead><tr><th colspan=2 align="left">string-&gt;number (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>?radix?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(string->number "23")        =>  23
(string->number "10111" 2)   =>  23
(string->number "27" 8)      =>  23
(string->number "17" 16)     =>  23
```
```
reg string->number

proc ::constcl::string->number {str args} {
  if {[llength $args] == 0} {
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    return [N [$str value]]
  } else {
    lassign $args radix
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    set radices [list [N 2] [N 8] [N 10] [N 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] [$str show] \
        [$radix show])
    }
    if {[$radix numval] == 10} {
      return [N [$str value]]
    } else {
      return [N [
        frombase [$radix numval] [$str value]]]
    }
  }
}
```


Due to [Richard Suchenwirth](https://wiki.tcl-lang.org/page/Based+numbers).

```
proc frombase {base number} {
  set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
  set negative [regexp ^-(.+) $number -> number]
  set res 0
  foreach digit [split $number {}] {
    # dv = decimal value
    set dv [lsearch $digits $digit]
    if {$dv < 0 || $dv >= $base} {
      ::error "bad digit $dv for base $base"
    }
    set res [expr {$res * $base + $dv}]
  }
  if $negative {set res -$res}
  set res
}
```
### Booleans


Booleans are logic values, either true (`` #t ``) or false (`` #f ``). All predicates (procedures whose name end with -?) return boolean values. The conditional `` if `` operator considers all values except for `` #f `` to be true.


__Boolean__ class

```
oo::class create ::constcl::Boolean {
  superclass ::constcl::NIL
  variable boolval
  constructor {v} {
    if {$v ni {#t #f}} {
      ::error "bad boolean value $v"
    }
    set boolval $v
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method boolval {} {
    set boolval
  }
  method value {} {
    set boolval
  }
  method write {handle} {
    puts -nonewline $handle [my boolval]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set boolval
  }
}

proc ::constcl::MkBoolean {v} {
  foreach instance [info class instances \
    ::constcl::Boolean] {
    if {[$instance boolval] eq $v} {
      return $instance
    }
  }
  return [::constcl::Boolean new $v]
}
```


__boolean?__ procedure


The `` boolean? `` predicate recognizes a Boolean by type.

<table border=1><thead><tr><th colspan=2 align="left">boolean? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {val} {
  return [typeof? $val Boolean]
}
```


__not__ procedure


The only operation on booleans: `` not ``, or logical negation.

<table border=1><thead><tr><th colspan=2 align="left">not (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>



Example:

```
(not #f)    ⇒  #t   ; #f yields #t, all others #f
(not nil)   ⇒  #f   ; see?
```
```
reg not ::constcl::not

proc ::constcl::not {val} {
  if {[$val boolval] eq "#f"} {
    return #t
  } else {
    return #f
  }
}
```
### Characters


Characters are any Unicode printing character, and also space and newline space characters. External representation is `` #\A `` (change A to relevant character) or `` #\space `` or `` #\newline ``. Internal representation is simply a Tcl character.


__Char__ class

```
oo::class create ::constcl::Char {
  superclass ::constcl::NIL
  variable value
  constructor {v} {
    switch -regexp $v {
      {(?i)#\\space} {
        set v " "
      }
      {(?i)#\\newline} {
        set v "\n"
      }
      {#\\[[:graph:]]} {
        set v [::string index $v 2]
      }
    }
    set value $v
  }
  method char {} {
    set value
  }
  method alphabetic? {} {
    if {[::string is alpha -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method numeric? {} {
    if {[::string is digit -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method whitespace? {} {
    if {[::string is space -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method upper-case? {} {
    if {[::string is upper -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method lower-case? {} {
    if {[::string is lower -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method value {} {
    return $value
  }
  method external {} {
    switch $value {
      " " {
        return "#\\space"
      }
      "\n" {
        return "#\\newline"
      }
      default {
        return "#\\$value"
      }
    }
  }
  method write {handle} {
    puts -nonewline $handle [my external]
  }
  method display {handle} {
    puts -nonewline $handle [my char]
  }
  method show {} {
    my external
  }
}

proc ::constcl::MkChar {v} {
  if {[regexp -nocase {space|newline} $v]} {
      set v [::string tolower $v]
  }
  foreach instance [
    info class instances Char] {
    if {[$instance external] eq $v} {
      return $instance
    }
  }
  return [::constcl::Char new $v]
}
```


__char?__ procedure


`` char? `` recognizes Char values by type.

<table border=1><thead><tr><th colspan=2 align="left">char? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char?

proc ::constcl::char? {val} {
  return [typeof? $val Char]
}
```


__char=?__ procedure


__char<?__ procedure


__char>?__ procedure


__char<=?__ procedure


__char>=?__ procedure


`` char=? ``, `` char<? ``, `` char>? ``, `` char<=? ``, and `` char>=? `` compare character values. They only compare two characters at a time.

<table border=1><thead><tr><th colspan=2 align="left">char=?, char&lt;?, char&gt;?, char&lt;=?, char&gt;=? (public)</th></tr></thead><tr><td>char1</td><td>a character</td></tr><tr><td>char2</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char=?

proc ::constcl::char=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {$char1 eq $char2} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char<?

proc ::constcl::char<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] < [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char>?

proc ::constcl::char>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] > [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char<=?

proc ::constcl::char<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] <= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char>=?

proc ::constcl::char>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] >= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```


__char-ci=?__ procedure


__char-ci<?__ procedure


__char-ci>?__ procedure


__char-ci<=?__ procedure


__char-ci>=?__ procedure


`` char-ci=? ``, `` char-ci<? ``, `` char-ci>? ``, `` char-ci<=? ``, and `` char-ci>=? `` compare character values in a case insensitive manner. They only compare two characters at a time.

<table border=1><thead><tr><th colspan=2 align="left">char-ci=?, char-ci&lt;?, char-ci&gt;?, char-ci&lt;=?, char-ci&gt;=? (public)</th></tr></thead><tr><td>char1</td><td>a character</td></tr><tr><td>char2</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] eq
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char-ci<?

proc ::constcl::char-ci<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] <
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char-ci>?

proc ::constcl::char-ci>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] >
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char-ci<=?

proc ::constcl::char-ci<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] <=
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg char-ci>=?

proc ::constcl::char-ci>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] >=
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```


__char-alphabetic?__ procedure


__char-numeric?__ procedure


__char-whitespace?__ procedure


__char-upper-case?__ procedure


__char-lower-case?__ procedure


The predicates `` char-alphabetic? ``, `` char-numeric? ``, and `` char-whitespace? `` test a character for these conditions.

<table border=1><thead><tr><th colspan=2 align="left">char-alphabetic?, char-numeric?, char-whitespace? (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>



`` char-upper-case? `` and `` char-lower-case? `` too.

<table border=1><thead><tr><th colspan=2 align="left">char-upper-case?, char-lower-case? (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char alphabetic?]
}
```
```
reg char-numeric?

proc ::constcl::char-numeric? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char numeric?]
}
```
```
reg char-whitespace?

proc ::constcl::char-whitespace? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char whitespace?]
}
```
```
reg char-upper-case?

proc ::constcl::char-upper-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char upper-case?]
}
```
```
reg char-lower-case?

proc ::constcl::char-lower-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char lower-case?]
}
```


__char->integer__ procedure


__integer->char__ procedure


`` char->integer `` and `` integer->char `` convert between characters and their 16-bit numeric codes.

<table border=1><thead><tr><th colspan=2 align="left">char-&gt;integer (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>an integer</td></tr></table>



Example:

```
(char->integer #\A)   =>  65
```
```
reg char->integer

proc ::constcl::char->integer {char} {
  return [MkNumber [scan [$char char] %c]]
}
```
<table border=1><thead><tr><th colspan=2 align="left">integer-&gt;char (public)</th></tr></thead><tr><td>int</td><td>an integer</td></tr><tr><td><i>Returns:</i></td><td>a character</td></tr></table>



Example:

```
(integer->char 97)   =>  #\a
```
```
reg integer->char

proc ::constcl::integer->char {int} {
  if {$int == 10} {
    return [MkChar #\\newline]
  } elseif {$int == 32} {
    return [MkChar #\\space]
  } else {
    return [MkChar #\\[format %c [$int numval]]]
  }
}
```


__char-upcase__ procedure


__char-downcase__ procedure


`` char-upcase `` and `` char-downcase `` alter the case of a character.

<table border=1><thead><tr><th colspan=2 align="left">char-upcase, char-downcase (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a character</td></tr></table>

```
reg char-upcase

proc ::constcl::char-upcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [MkChar [
    ::string toupper [$char value]]]
}
```
```
reg char-downcase

proc ::constcl::char-downcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [MkChar [
    ::string tolower [$char value]]]
}
```
### Control


This section concerns itself with procedures and the application of the same.


A `` Procedure `` object is a [closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)), storing the procedure's parameter list, the body, and the environment that is current when the object is created, i.e. when the procedure is defined.


When a `` Procedure `` object is called, the body is evaluated in a new environment where the parameters are given values from the argument list and the outer link goes to the closure environment.


__Procedure__ class

```
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
```


__procedure?__ procedure

<table border=1><thead><tr><th colspan=2 align="left">procedure? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
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
```


__apply__ procedure


`` apply `` applies a procedure to a Lisp list of Lisp arguments.

<table border=1><thead><tr><th colspan=2 align="left">apply (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>what pr returns</td></tr></table>



Example:

```
(apply + (list 2 3))   =>  5
```
```
reg apply

proc ::constcl::apply {pr vals} {
  check {procedure? $pr} {
    PROCEDURE expected\n([pn] [$pr show] ...)
  }
  invoke $pr $vals
}
```


__map__ procedure


`` map `` iterates over one or more lists, taking an element from each list to pass to a procedure as an argument. The Lisp list of the results of the invocations is returned.

<table border=1><thead><tr><th colspan=2 align="left">map (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>args</td><td>some lists</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>



Example:

```
(map + '(1 2 3) '(5 6 7))   => (6 8 10)
```
```
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
```


__for-each__ procedure


`` for-each `` iterates over one or more lists, taking an element from each list to pass to a procedure as an argument. The empty list is returned.

<table border=1><thead><tr><th colspan=2 align="left">for-each (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>args</td><td>some lists</td></tr><tr><td><i>Returns:</i></td><td>the empty list</td></tr></table>



Example: (from R5RS; must be pasted as a oneliner for the ConsTcl repl to stomach it.)

```
(let ((v (make-vector 5)))
  (for-each (lambda (i)
              (vector-set! v i (* i i)))
            '(0 1 2 3 4))
  v)                        =>  #(0 1 4 9 16)
```
```
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
```
### Input and output


__Port__ class

```
catch { ::constcl::Port destroy }

oo::class create ::constcl::Port {
  superclass ::constcl::NIL
  variable handle
  constructor {args} {
    if {[llength $args]} {
      lassign $args handle
    } else {
      set handle #NIL
    }
  }
  method handle {} {
    set handle
  }
  method close {} {
    close $handle
    set handle #NIL
    return
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<port-$num>"
  }
  method display {h} {
    my write $h
  }
}
```


__InputPort__ class

```
oo::class create ::constcl::InputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    try {
      set handle [open [$name value] "r"]
    } on error {} {
      set handle #NIL
      return -1
    }
    return $handle
  }
  method get {} {
    read $handle 1
  }
  method eof {} {
    eof $handle
  }
  method copy {} {
    ::constcl::InputPort new $handle
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<input-port-$num>"
  }
  method display {h} {
    my write $h
  }
}
```


__StringInputPort__ class

```
oo::class create ::constcl::StringInputPort {
  superclass ::constcl::Port
  variable buffer read_eof
  constructor {str} {
    set buffer $str
    set read_eof 0
  }
  method open {name} {}
  method close {} {}
  method get {} {
    if {[::string length $buffer] == 0} {
      set read_eof 1
      return #EOF
    }
    set c [::string index $buffer 0]
    set buffer [::string range $buffer 1 end]
    return $c
  }
  method eof {} {
    if {$read_eof} {
      return 1
    } else {
      return 0
    }
  }
  method copy {} {
    ::constcl::StringInputPort new $buffer
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<string-input-port-$num>"
  }
  method display {h} {
    my write $h
  }
}
```


__OutputPort__ class

```
oo::class create ::constcl::OutputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    try {
      set handle [open [$name value] "w"]
    } on error {} {
      set handle #NIL
      return -1
    }
    return $handle
  }
  method put {c} {
    puts -nonewline $handle $c
  }
  method copy {} {
    ::constcl::OutputPort new $handle
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<output-port-$num>"
  }
  method display {h} {
    my write $h
  }
}

interp alias {} ::constcl::MkInputPort \
  {} ::constcl::InputPort new
interp alias {} ::constcl::MkOutputPort \
  {} ::constcl::OutputPort new
```


__Input_Port__ variable


__Output_port__ variable


These two variables store globally the current configuration of the shared input and output ports. They are initially set to standard input and output respectively.

```
set ::constcl::Input_port [
  ::constcl::MkInputPort stdin]
set ::constcl::Output_port [
  ::constcl::MkOutputPort stdout]
```


__port?__ procedure


`` port? `` recognizes Port objects.

```
reg port?

proc ::constcl::port? {val} {
  typeof? $val Port
}
```


__call-with-input-file__ procedure


`` call-with-input-file `` opens a file for input and passes the port to `` proc ``. The file is closed again once `` proc `` returns. The result of the call is returned.

<table border=1><thead><tr><th colspan=2 align="left">call-with-input-file (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td>proc</td><td>a procedure</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg call-with-input-file

proc ::constcl::call-with-input-file {filename proc} {
  set port [open-input-file $filename]
  set res [invoke $proc [list $port]]
  close-input-port $port
  $port destroy
  return $res
}
```


__call-with-output-file__ procedure


`` call-with-output-file `` opens a file for output and passes the port to `` proc ``. The file is closed again once `` proc `` returns. The result of the call is returned.


You can't use this procedure without deleting the first line. I take no responsibility for damage to your files due to overwriting the contents.

<table border=1><thead><tr><th colspan=2 align="left">call-with-output-file (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td>proc</td><td>a procedure</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg call-with-output-file

proc ::constcl::call-with-output-file {filename proc} {
  ::error "remove this line to use"
  set port [open-output-file $filename]
  set res [invoke $proc [list $port]]
  close-output-port $port
  $port destroy
  return $res
}
```


__input-port?__ procedure


`` input-port? `` recognizes an InputPort object.

<table border=1><thead><tr><th colspan=2 align="left">input-port? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg input-port?

proc ::constcl::input-port? {val} {
  typeof? $val InputPort
}
```


__output-port?__ procedure


`` output-port? `` recognizes an OutputPort object.

<table border=1><thead><tr><th colspan=2 align="left">output-port? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg output-port?

proc ::constcl::output-port? {val} {
  typeof? $val OutputPort
}
```


__current-input-port__ procedure


`` current-input-port `` makes a copy of the current shared input port.

<table border=1><thead><tr><th colspan=2 align="left">current-input-port (public)</th></tr></thead><tr><td><i>Returns:</i></td><td>a port</td></tr></table>

```
reg current-input-port

proc ::constcl::current-input-port {} {
  return [$::constcl::Input_port copy]
}
```


__current-output-port__ procedure


`` current-output-port `` makes a copy of the current shared output port.

<table border=1><thead><tr><th colspan=2 align="left">current-output-port (public)</th></tr></thead><tr><td><i>Returns:</i></td><td>a port</td></tr></table>

```
reg current-output-port

proc ::constcl::current-output-port {} {
  return [$::constcl::Output_port copy]
}
```


__with-input-from-file__ procedure


`` with-input-from-file `` opens a file for input and calls a 'thunk' while the file is open. The file is closed again when the call is done.

<table border=1><thead><tr><th colspan=2 align="left">with-input-from-file (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td>thunk</td><td>a procedure</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg with-input-from-file

proc ::constcl::with-input-from-file {filename thunk} {
  set newport [open-input-file $filename]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Input_port
    set ::constcl::Input_port $newport
    $thunk call
    set ::constcl::Input_port $oldport
    close-input-port $newport
  }
  $newport destroy
}
```


__with-output-to-file__ procedure


`` with-output-to-file `` opens a file for output and calls a 'thunk' while the file is open. The file is closed again when the call is done.

<table border=1><thead><tr><th colspan=2 align="left">with-output-to-file (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td>thunk</td><td>a procedure</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg with-output-to-file

proc ::constcl::with-output-to-file {filename thunk} {
  ::error "remove this line to use"
  set newport [open-output-file $filename]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Output_port
    set ::constcl::Output_port $newport
    $thunk call
    set ::constcl::Output_port $oldport
    close-input-port $newport
  }
  $newport destroy
}
```


__open-input-file__ procedure


`` open-input-file `` opens a file for input and returns the port.

<table border=1><thead><tr><th colspan=2 align="left">open-input-file (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>an input port</td></tr></table>

```
reg open-input-file

proc cnof {} {return "could not open file"}
proc fae {} {return "file already exists"}

proc ::constcl::open-input-file {filename} {
  set p [MkInputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-input-file: [cnof] $filename"
  }
  return $p
}
```


__open-output-file__ procedure


`` open-output-file `` opens a file for output and returns the port.

<table border=1><thead><tr><th colspan=2 align="left">open-output-file (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>an output port</td></tr></table>

```
reg open-output-file

proc ::constcl::open-output-file {filename} {
  ::error "remove this line to use"
  if {[file exists $filename]} {
    error "open-output-file: [fae] $filename"
  }
  set p [MkOutputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-output-file: [cnof] $filename"
  }
  return $p
}
```


__close-input-port__ procedure


`` close-input-port `` closes an input port.

<table border=1><thead><tr><th colspan=2 align="left">close-input-port (public)</th></tr></thead><tr><td>port</td><td>an input port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg close-input-port

proc ::constcl::close-input-port {port} {
  if {[$port handle] eq "stdin"} {
    error "don't close the standard input port"
  }
  $port close
}
```


__close-output-port__ procedure


`` close-output-port `` closes an output port.

<table border=1><thead><tr><th colspan=2 align="left">close-output-port (public)</th></tr></thead><tr><td>port</td><td>an output port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg close-output-port

proc ::constcl::close-output-port {port} {
  if {[$port handle] eq "stdout"} {
    error "don't close the standard output port"
  }
  $port close
}
```
```
proc ::constcl::read-char {args} {
    # TODO
}
```
```
proc ::constcl::peek-char {args} {
    # TODO
}
```
```
proc ::constcl::char-ready? {args} {
    # TODO
}
```


`` write `` is implemented in the [output](https://github.com/hoodiecrow/ConsTcl#output) chapter.



`` display `` is implemented in the output chapter.



__newline__ procedure


`` newline `` outputs a newline character. Especially helpful when using `` display `` for output, since it doesn't end lines with newline.

<table border=1><thead><tr><th colspan=2 align="left">newline (public)</th></tr></thead><tr><td>?port?</td><td>an output port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg newline

proc ::constcl::newline {args} {
  if {[llength $args]} {
    lassign $args port
  } else {
    set port [current-output-port]
  }
  pe "(display #\\newline $port)"
}
```
```
proc ::constcl::write-char {args} {
    # TODO
}
```


__load__


`` load `` reads a Lisp source file and evals the expressions in it in the global environment. The procedure is a ConsTcl mix of Scheme calls and Tcl syntax.

<table border=1><thead><tr><th colspan=2 align="left">load (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg load

proc ::constcl::load {filename} {
  set p [open-input-file $filename]
  if {[$p handle] ne "#NIL"} {
    set n [read $p]
    while {$n ne "#EOF"} {
      eval $n
      set n [read $p]
    }
    close-input-port $p
  }
  $p destroy
}
```
```
proc ::constcl::transcript-on {filename} {
    # TODO
}
```
```
proc ::constcl::transcript-off {} {
    # TODO
}
```
### Pairs and lists


List processing is another of Lisp's great strengths.


__Pair__ class

```
catch { ::constcl::Pair destroy }

oo::class create ::constcl::Pair {
  superclass ::constcl::NIL
  variable car cdr constant
  constructor {a d} {
    set car $a
    set cdr $d
    set constant 0
  }
  method name {} {}
  method value {} {
    my show
  }
  method car {} {
    set car
  }
  method cdr {} {
    set cdr
  }
  method set-car! {val} {
    ::constcl::check {my mutable?} {
      Can't modify a constant pair
    }
    set car $val
    self
  }
  method set-cdr! {val} {
    ::constcl::check {my mutable?} {
      Can't modify a constant pair
    }
    set cdr $val
    self
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    return $constant
  }
  method mutable? {} {
    expr {$constant ? "#f" : "#t"}
  }
  method write {handle} {
    puts -nonewline $handle "("
    ::constcl::write-pair $handle [self]
    puts -nonewline $handle ")"
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "(%s)" [::constcl::show-pair [self]]
  }
}


interp alias {} ::constcl::MkPair \
  {} ::constcl::Pair new
```


__pair?__ procedure

<table border=1><thead><tr><th colspan=2 align="left">pair? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg pair?

proc ::constcl::pair? {val} {
  typeof? $val Pair
}
```


__show-pair__ procedure


Helper procedure to make a string representation of a list.

<table border=1><thead><tr><th colspan=2 align="left">show-pair (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Tcl string</td></tr></table>

```
proc ::constcl::show-pair {pair} {
  # take an object and print the car
  # and the cdr of the stored value
  set str {}
  set a [car $pair]
  set d [cdr $pair]
  # print car
  ::append str [$a show]
  if {[pair? $d] ne "#f"} {
    # cdr is a cons pair
    ::append str " "
    ::append str [show-pair $d]
  } elseif {[null? $d] ne "#f"} {
    # cdr is nil
    return $str
  } else {
    # it is an atom
    ::append str " . "
    ::append str [$d show]
  }
  return $str
}
```


__cons__ procedure


`` cons `` joins two values in a pair; useful in many operations such as pushing a new value onto a list.

<table border=1><thead><tr><th colspan=2 align="left">cons (public)</th></tr></thead><tr><td>car</td><td>a Lisp value</td></tr><tr><td>cdr</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a pair</td></tr></table>



Example:

```
(cons 'a 'b)              =>  (a . b)
(cons 'a nil)             =>  (a)
(cons 'a (cons 'b nil))   =>  (a b)
```
![#](images/consing.png)
```
reg cons

proc ::constcl::cons {car cdr} {
  MkPair $car $cdr
}
```


__car__ procedure


`` car `` gets the contents of the first cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">car (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>



Example:

```
(car '(a b))   =>  a
```
```
reg car

proc ::constcl::car {pair} {
  $pair car
}
```


__cdr__ procedure


`` cdr `` gets the contents of the second cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">cdr (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>



Example:

```
(cdr '(a b))   =>  (b)
```
```
reg cdr

proc ::constcl::cdr {pair} {
  $pair cdr
}
```


__caar__ to __cddddr__


`` car `` and `` cdr `` can be combined to form 28 composite access operations.

```
foreach ads {
  aa
  ad
  da
  dd
  aaa
  ada
  daa
  dda
  aad
  add
  dad
  ddd
  aaaa
  adaa
  daaa
  ddaa
  aada
  adda
  dada
  ddda
  aaad
  adad
  daad
  ddad
  aadd
  addd
  dadd
  dddd
} {
    reg c${ads}r

    proc ::constcl::c${ads}r {pair} "
        foreach c \[lreverse \[split $ads {}\]\] {
            if {\$c eq \"a\"} {
                set pair \[car \$pair\]
            } else {
                set pair \[cdr \$pair\]
            }
        }
        return \$pair
    "

}
```


__set-car!__ procedure


`` set-car! `` sets the contents of the first cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">set-car! (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a pair</td></tr></table>



Example:

```
(let ((pair (cons 'a 'b)) (val 'x))
  (set-car! pair val))                =>  (x . b)
```
```
reg set-car!

proc ::constcl::set-car! {pair val} {
  $pair set-car! $val
}
```


__set-cdr!__ procedure


`` set-cdr! `` sets the contents of the second cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">set-cdr! (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a pair</td></tr></table>



Example:

```
(let ((pair (cons 'a 'b)) (val 'x))
  (set-cdr! pair val))                =>  (a . x)
```
```
reg set-cdr!

proc ::constcl::set-cdr! {pair val} {
  $pair set-cdr! $val
}
```


__list?__ procedure


The `` list? `` predicate tests if a pair is part of a proper list, one that ends with NIL.

<table border=1><thead><tr><th colspan=2 align="left">list? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg list?

proc ::constcl::list? {val} {
  set visited {}
  if {[null? $val] ne "#f"} {
      return #t
  } elseif {[pair? $val] ne "#f"} {
      return [listp $val]
  } else {
      return #f
  }
}
```


__listp__ procedure


`` listp `` is a helper procedure that recursively traverses a cons trail to find out if it is cyclic or ends in an atom, which means that the procedure returns false, or if it ends in `` #NIL ``, which means that it returns true.

<table border=1><thead><tr><th colspan=2 align="left">listp (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
proc ::constcl::listp {pair} {
  upvar visited visited
  if {$pair in $visited} {
    return #f
  }
  lappend visited $pair
  if {[null? $pair] ne "#f"} {
    return #t
  } elseif {[pair? $pair] ne "#f"} {
    return [listp [cdr $pair]]
  } else {
    return #f
  }
}
```


__list__ procedure


`` list `` constructs a Lisp list from a number of values.

<table border=1><thead><tr><th colspan=2 align="left">list (public)</th></tr></thead><tr><td>args</td><td>some Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>



Example:

```
(list 1 2 3)   =>  (1 2 3)
```
```
reg list

proc ::constcl::list {args} {
  if {[llength $args] == 0} {
    return #NIL
  } else {
    set prev #NIL
    foreach obj [lreverse $args] {
      set prev [cons $obj $prev]
    }
    return $prev
  }
}
```


__length__ procedure


`` length `` reports the length of a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">length (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(length '(a b c d))   =>  4
```
```
reg length

proc ::constcl::length {pair} {
  check {list? $pair} {
    LIST expected\n([pn] lst)
  }
  MkNumber [length-helper $pair]
}
```


__length-helper__ procedure


`` length-helper `` is a helper procedure which measures a list recursively.

<table border=1><thead><tr><th colspan=2 align="left">length-helper (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Tcl number</td></tr></table>

```
proc ::constcl::length-helper {pair} {
  if {[null? $pair] ne "#f"} {
    return 0
  } else {
    return [expr {1 +
      [length-helper [cdr $pair]]}]
  }
}
```


__append__ procedure


`` append `` joins lists together.



Example:

```
(append '(a b) '(c d))   =>  (a b c d)
```
<table border=1><thead><tr><th colspan=2 align="left">append (public)</th></tr></thead><tr><td>args</td><td>some lists</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
reg append

proc ::constcl::append {args} {
  set prev [lindex $args end]
  foreach r [lreverse [lrange $args 0 end-1]] {
    check {list? $r} {
      LIST expected\n([pn] [$r show])
    }
    set prev [copy-list $r $prev]
  }
  set prev
}
```


__copy-list__ procedure


`` copy-list `` joins together two lists by recursively consing items from the first list towards the second.

<table border=1><thead><tr><th colspan=2 align="left">copy-list (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>next</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
proc ::constcl::copy-list {pair next} {
  # TODO only fresh conses in the direct chain to NIL
  if {[null? $pair] ne "#f"} {
    set next
  } elseif {[null? [cdr $pair]] ne "#f"} {
    cons [car $pair] $next
  } else {
    cons [car $pair] [copy-list [cdr $pair] $next]
  }
}
```


__reverse__ procedure


`` reverse `` produces a reversed copy of a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">reverse (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>



Example:

```
(reverse '(a b c))   =>  (c b a)
```
```
reg reverse

proc ::constcl::reverse {vals} {
  list {*}[lreverse [splitlist $vals]]
}
```


__list-tail__ procedure


Given a list index, `` list-tail `` yields the sublist starting from that index.

<table border=1><thead><tr><th colspan=2 align="left">list-tail (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>



Example:

```
(let ((lst '(a b c d e f)) (k 3))
  (list-tail lst k))                =>  (d e f)
```
```
reg list-tail

proc ::constcl::list-tail {vals k} {
  if {[zero? $k] ne "#f"} {
    return $vals
  } else {
    list-tail [cdr $vals] [- $k #1]
  }
}
```


__list-ref__ procedure


`` list-ref `` yields the list item at a given index.

<table border=1><thead><tr><th colspan=2 align="left">list-ref (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>



Example:

```
(let ((lst '(a b c d e f)) (k 3))
  (list-ref lst k))                 =>  d
```
```
reg list-ref

proc ::constcl::list-ref {vals k} {
  car [list-tail $vals $k]
}
```


__memq__ procedure


__memv__ procedure


__member__ procedure


`` memq ``, `` memv ``, and `` member `` return the sublist starting with a given item, or `` #f `` if there is none. They use `` eq? ``, `` eqv? ``, and `` equal? ``, respectively, for the comparison.

<table border=1><thead><tr><th colspan=2 align="left">memq (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>



Example:

```
(let ((lst '(a b c d e f)) (val 'd))
  (memq val lst))                      =>  (d e f)
```
```
reg memq

proc ::constcl::memq {val1 val2} {
  return [member-proc eq? $val1 $val2]
}
```
<table border=1><thead><tr><th colspan=2 align="left">memv (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg memv

proc ::constcl::memv {val1 val2} {
  return [member-proc eqv? $val1 $val2]
}
```
<table border=1><thead><tr><th colspan=2 align="left">member (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg member

proc ::constcl::member {val1 val2} {
  return [member-proc equal? $val1 $val2]
}
```


__member-proc__ procedure


`` member-proc `` helper procedure which does the work for the `` memq ``, `` memv ``, and `` member `` procedures. Works by recursively taking the `` cdr `` of the list, comparing against the `` car `` of the list.

<table border=1><thead><tr><th colspan=2 align="left">member-proc (internal)</th></tr></thead><tr><td>epred</td><td>an equivalence predicate</td></tr><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
proc ::constcl::member-proc {epred val1 val2} {
  switch $epred {
    eq? { set name "memq" }
    eqv? { set name "memv" }
    equal? { set name "member" }
  }
  check {list? $val2} {
    LIST expected\n($name [$val1 show] [$val2 show])
  }
  if {[null? $val2] ne "#f"} {
    return #f
  } elseif {[pair? $val2] ne "#f"} {
    if {[$epred $val1 [car $val2]] ne "#f"} {
      return $val2
    } else {
      return [member-proc $epred $val1 [cdr $val2]]
    }
  }
}
```


__assq__ procedure


__assv__ procedure


__assoc__ procedure


`` assq ``, `` assv ``, and `` assoc `` return the associative item marked with a given key, or `` #f `` if there is none. They use `` eq? ``, `` eqv? ``, and `` equal? ``, respectively, for the comparison. They implement lookup in the form of lookup table known as an association list, or _alist_.


Example:

```
(define e '((a 1) (b 2) (c 3)))
(assq 'a e)                       => (a 1)
```
<table border=1><thead><tr><th colspan=2 align="left">assq (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>an association list</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg assq

proc ::constcl::assq {val1 val2} {
  return [assoc-proc eq? $val1 $val2]
}
```
<table border=1><thead><tr><th colspan=2 align="left">assv (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>an association list</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg assv

proc ::constcl::assv {val1 val2} {
  return [assoc-proc eqv? $val1 $val2]
}
```
<table border=1><thead><tr><th colspan=2 align="left">assoc (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>an association list</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg assoc

proc ::constcl::assoc {val1 val2} {
  return [assoc-proc equal? $val1 $val2]
}
```


__assoc-proc__ procedure


`` assoc-proc `` is a helper procedure which does the work for `` assq ``, `` assv ``, and `` assoc ``.

<table border=1><thead><tr><th colspan=2 align="left">assoc-proc (internal)</th></tr></thead><tr><td>epred</td><td>an equivalence predicate</td></tr><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>an association list</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
proc ::constcl::assoc-proc {epred val1 val2} {
  switch $epred {
    eq? { set name "assq" }
    eqv? { set name "assv" }
    equal? { set name "assoc" }
  }
  check {list? $val2} {
    LIST expected\n($name [$val1 show] [$val2 show])
  }
  if {[null? $val2] ne "#f"} {
    return #f
  } elseif {[pair? $val2] ne "#f"} {
    if {[pair? [car $val2]] ne "#f" && 
      [$epred $val1 [caar $val2]] ne "#f"} {
      return [car $val2]
    } else {
      return [assoc-proc $epred $val1 [cdr $val2]]
    }
  }
}
```
### Strings


Procedures for dealing with strings of characters.


__String__ class


Strings have the internal representation of a vector of character objects, with the data elements of the vector address of the first element, and the length of the vector. External representation is surrounded by double quotes, with double quotes and backslashes within the string escaped with a backslash.


As a ConsTcl extension, a `` \n `` pair in the external representation is stored as a newline character. It is restored to `` \n `` on write.

```
oo::class create ::constcl::String {
  superclass ::constcl::NIL
  variable data constant
  constructor {v} {
    set v [string map {\\\\ \\ \\\" \" \\n \n} $v]
    set len [::string length $v]
    set vsa [::constcl::vsAlloc $len]
    set idx $vsa
    foreach elt [split $v {}] {
      if {$elt eq " "} {
        set c #\\space
      } elseif {$elt eq "\n"} {
        set c #\\newline
      } else {
        set c #\\$elt
      }
      lset ::constcl::vectorSpace $idx \
        [::constcl::MkChar $c]
      incr idx
    }
    set data [
      ::constcl::cons [N $vsa] [N $len]]
    set constant 0
  }
  method = {str} {
    ::string equal [my value] [$str value]
  }
  method cmp {str} {
    ::string compare [my value] [$str value]
  }
  method length {} {
    ::constcl::cdr $data
  }
  method ref {k} {
    set k [$k numval]
    if {$k < 0 || $k >= [[my length] numval]} {
      ::error "index out of range\n$k"
    }
    lindex [my store] $k
  }
  method store {} {
    set base [[::constcl::car $data] numval]
    set end [expr {[[my length] numval] +
      $base - 1}]
    lrange $::constcl::vectorSpace $base $end
  }
  method value {} {
    join [lmap c [my store] {$c char}] {}
  }
  method set! {k c} {
    if {[my constant]} {
      ::error "string is constant"
    } else {
      set k [$k numval]
      if {$k < 0 ||
        $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[::constcl::car $data] numval]
      lset ::constcl::vectorSpace $k+$base $c
    }
    return [self]
  }
  method fill! {c} {
    if {[my constant]} {
      ::error "string is constant"
    } else {
      set base [[::constcl::car $data] numval]
      set len [[my length] numval]
      for {set idx $base} \
        {$idx < $len+$base} \
        {incr idx} {
        lset ::constcl::vectorSpace $idx $c
      }
    }
    return [self]
  }
  method substring {from to} {
    join [lmap c [lrange [my store] \
      [$from numval] [$to numval]] {$c char}] {}
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method external {} {
    return "\"[
      string map {\\ \\\\ \" \\\" \n \\n} [my value]]\""
  }
  method write {handle} {
    puts -nonewline $handle [my external]
  }
  method display {handle} {
    puts -nonewline $handle [my value]
  }
  method show {} {
    my external
  }
}

interp alias {} ::constcl::MkString \
  {} ::constcl::String new
```


__string?__ procedure


`` string? `` recognizes a string by type.

<table border=1><thead><tr><th colspan=2 align="left">string? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg string?

proc ::constcl::string? {val} {
  typeof? $val String
}
```


__make-string__ procedure


`` make-string `` creates a string of _k_ characters, optionally filled with _char_ characters. If _char_ is omitted, the string will be filled with space characters.

<table border=1><thead><tr><th colspan=2 align="left">make-string (public)</th></tr></thead><tr><td>k</td><td>a number</td></tr><tr><td>?char?</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(let ((k 5))
  (make-string k))        =>  "     "
(let ((k 5) (char #\A))
  (make-string k char))   =>  "AAAAA"
```
```
reg make-string

proc ::constcl::make-string {k args} {
  set i [$k numval]
  if {[llength $args] == 0} {
    set char " "
  } else {
    lassign $args c
    set char [$c char]
  }
  return [MkString [::string repeat $char $i]]
}
```


__string__ procedure


`` string `` constructs a string from a number of Lisp characters.

<table border=1><thead><tr><th colspan=2 align="left">string (public)</th></tr></thead><tr><td>args</td><td>some characters</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(string #\f #\o #\o)   =>  "foo"
```
```
reg string

proc ::constcl::string {args} {
  set str {}
  foreach char $args {
    check {::constcl::char? $char} {
      CHAR expected\n([pn] [lmap c $args \
        {$c show}])
    }
    ::append str [$char char]
  }
  return [MkString $str]
}
```


__string-length__ procedure


`` string-length `` reports a string's length.

<table border=1><thead><tr><th colspan=2 align="left">string-length (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(string-length "foobar")   => 6
```
```
reg string-length

proc ::constcl::string-length {str} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [$str length]
}
```


__string-ref__ procedure


`` string-ref `` yields the _k_-th character (0-based) in _str_.

<table border=1><thead><tr><th colspan=2 align="left">string-ref (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a character</td></tr></table>



Example:

```
(string-ref "foobar" 3)   => #\b
```
```
reg string-ref

proc ::constcl::string-ref {str k} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show] \
      [$k show])
  }
  check {::constcl::number? $k} {
    INTEGER expected\n([pn] [$str show] \
      [$k show])
  }
  return [$str ref $k]
}
```


__string-set!__ procedure


`` string-set! `` replaces the character at _k_ with _char_ in a non-constant string.

<table border=1><thead><tr><th colspan=2 align="left">string-set! (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>k</td><td>a number</td></tr><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(let ((str (string #\f #\o #\o))
      (k 2)
      (char #\x))
  (string-set! str k char))         =>  "fox"
```
```
reg string-set!

proc ::constcl::string-set! {str k char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$k show] \
      [$char show])
  }
  check {number? $k} {
    INTEGER expected\n([pn] [$str show] \
      [$k show] [$char show])
  }
  check {char? $char} {
    CHAR expected\n([pn] [$str show] [$k show] \
      [$char show])
  }
  $str set! $k $char
  return $str
}
```


__string=?__, __string-ci=?__


__string<?__, __string-ci<?__


__string>?__, __string-ci>?__


__string<=?__, __string-ci<=?__


__string>=?__, __string-ci>=?__


`` string=? ``, `` string<? ``, `` string>? ``, `` string<=? ``, `` string>=? `` and their case insensitive variants `` string-ci=? ``, `` string-ci<? ``, `` string-ci>? ``, `` string-ci<=? ``, `` string-ci>=? `` compare strings.

<table border=1><thead><tr><th colspan=2 align="left">string=?, string&lt;?, string&gt;?, string&lt;=?, string&gt;=? (public)</th></tr></thead><tr><td>str1</td><td>a string</td></tr><tr><td>str2</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

<table border=1><thead><tr><th colspan=2 align="left">string-ci=?, string-ci&lt;?, string-ci&gt;? (public)</th></tr></thead><tr><td>str1</td><td>a string</td></tr><tr><td>str2</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

<table border=1><thead><tr><th colspan=2 align="left">string-ci&lt;=?, string-ci&gt;=? (public)</th></tr></thead><tr><td>str1</td><td>a string</td></tr><tr><td>str2</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg string=?

proc ::constcl::string=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] eq [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string-ci=?

proc ::constcl::string-ci=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] eq
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string<?

proc ::constcl::string<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] < [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string-ci<?

proc ::constcl::string-ci<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] <
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string>?

proc ::constcl::string>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] > [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string-ci>?

proc ::constcl::string-ci>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] >
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string<=?

proc ::constcl::string<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] <= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string-ci<=?

proc ::constcl::string-ci<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] <=
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string>=?

proc ::constcl::string>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] >= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```
```
reg string-ci>=?

proc ::constcl::string-ci>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] >=
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```


__substring__ procedure


`` substring `` yields the substring of _str_ that starts at _start_ and ends at _end_.

<table border=1><thead><tr><th colspan=2 align="left">substring (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>start</td><td>a number</td></tr><tr><td>end</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(substring "foobar" 2 4)   => "oba"
```
```
reg substring

proc ::constcl::substring {str start end} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  check {number? $start} {
    NUMBER expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  check {number? $end} {
    NUMBER expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  return [MkString [$str substring $start $end]]
}
```


__string-append__ procedure


`` string-append `` joins strings together.

<table border=1><thead><tr><th colspan=2 align="left">string-append (public)</th></tr></thead><tr><td>args</td><td>some strings</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(string-append "foo" "bar")   =>  "foobar"
```
```
reg string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {
      $arg value
    }]]
}
```


__string->list__ procedure


`` string->list `` converts a string to a Lisp list of characters.

<table border=1><thead><tr><th colspan=2 align="left">string-&gt;list (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of characters</td></tr></table>



Example:

```
(string->list "foo")   =>  (#\f #\o #\o)
```
```
reg string->list

proc ::constcl::string->list {str} {
  list {*}[$str store]
}
```


__list->string__ procedure


`` list->string `` converts a Lisp list of characters to a string.

<table border=1><thead><tr><th colspan=2 align="left">list-&gt;string (public)</th></tr></thead><tr><td>list</td><td>a Lisp list of characters</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(list->string '(#\1 #\2 #\3))   => "123"
```
```
reg list->string

proc ::constcl::list->string {list} {
  MkString [::append --> {*}[
    lmap c [splitlist $list] {$c char}]]
}
```


__string-copy__ procedure


`` string-copy `` makes a copy of a string.

<table border=1><thead><tr><th colspan=2 align="left">string-copy (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(let ((str (string-copy "abc"))
      (k 0)
      (char #\x))
  (string-set! str k char))       =>  "xbc"
```
```
reg string-copy

proc ::constcl::string-copy {str} {
  check {string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkString [$str value]]
}
```


__string-fill!__ procedure


`` string-fill! `` _str_ _char_ fills a non-constant string with _char_.

<table border=1><thead><tr><th colspan=2 align="left">string-fill! (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>



Example:

```
(let ((str (string-copy "foobar"))
      (char #\X))
  (string-fill! str char))           =>  "XXXXXX"
```
```
reg string-fill!

proc ::constcl::string-fill! {str char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] \
      [$char show])
  }
  $str fill! $char
  return $str
}
```
### Symbols


Symbols are like little strings that are used to refer to things (variables, including procedure names, etc) or for comparing against each other.


__Symbol__ class

```
oo::class create ::constcl::Symbol {
  superclass ::constcl::NIL
  variable name caseconstant
  constructor {n} {
    if {   no &&   $n eq {}} {
      ::error "a symbol must have a name"
    }
    ::constcl::idcheck $n
    set name $n
    set caseconstant 0
  }
  method name {} {
    set name
  }
  method value {} {
    set name
  }
  method = {symname} {
    expr {$name eq $symname}
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method make-case-constant {} {
    set caseconstant 1
  }
  method case-constant {} {
    set caseconstant
  }
  method write {handle} {
    puts -nonewline $handle [my name]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set name
  }
}

unset -nocomplain ::constcl::symbolTable
set ::constcl::symbolTable [dict create]

proc ::constcl::MkSymbol {n} {
  if {[dict exists $::constcl::symbolTable $n]} {
    return [dict get $::constcl::symbolTable $n]
  } else {
    set sym [::constcl::Symbol new $n]
    dict set ::constcl::symbolTable $n $sym
    return $sym
  }
}
interp alias {} S {} ::constcl::MkSymbol
```
<table border=1><thead><tr><th colspan=2 align="left">symbol? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {val} {
  typeof? $val Symbol
}
```


__symbol->string__


`` symbol->string `` yields a string consisting of the symbol name, usually lower-cased.

<table border=1><thead><tr><th colspan=2 align="left">symbol-&gt;string (public)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

```
reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {sym} {
  check {symbol? $sym} {
    SYMBOL expected\n([pn] [$sym show])
  }
  if {![$sym case-constant]} {
    set str [MkString [
      ::string tolower [$sym name]]]
  } else {
    set str [MkString [$sym name]]
  }
  $str mkconstant
  return $str
}
```


Example:

```
(let ((sym 'Foobar))
  (symbol->string sym))   =>  "foobar"
```


__string->symbol__


`` string->symbol `` creates a symbol with the name given by the string. The symbol is 'case-constant', i.e. it will not be lower-cased.

<table border=1><thead><tr><th colspan=2 align="left">string-&gt;symbol (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a symbol</td></tr></table>



Example:

```
(define sym (let ((str "Foobar"))
              (string->symbol str)))
sym                                    =>  Foobar
(symbol->string sym)                   =>  "Foobar"
```
```
reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {str} {
  check {string? $str} {
    STRING expected\n([pn] [$obj show])
  }
  set sym [MkSymbol [$str value]]
  $sym make-case-constant
  return $sym
}
```
### Vectors


Vectors are heterogenous structures of fixed length whose elements are indexed by integers.


The number of elements that a vector contains (the _length_) is set when the vector is created. Elements can be indexed by integers from zero to length minus one.


__Vector__ class

```
oo::class create ::constcl::Vector {
  superclass ::constcl::NIL
  variable data constant
  constructor {v} {
    set len [llength $v]
    set vsa [::constcl::vsAlloc $len]
    set idx $vsa
    foreach elt $v {
      lset ::constcl::vectorSpace $idx $elt
      incr idx
    }
    set data [::constcl::cons [N $vsa] [N $len]]
    set constant 0
  }
  method baseadr {} {
    ::constcl::car $data
  }
  method length {} {
    ::constcl::cdr $data
  }
  method ref {k} {
    set k [$k numval]
    if {$k < 0 || $k >= [[my length] numval]} {
      ::error "index out of range\n$k"
    }
    lindex [my store] $k
  }
  method store {} {
    set base [[my baseadr] numval]
    set end [expr {[[my length] numval] +
      $base - 1}]
    lrange $::constcl::vectorSpace $base $end
  }
  method value {} {
    my store
  }
  method set! {k obj} {
    if {[my constant]} {
      ::error "vector is constant"
    } else {
      set k [$k numval]
      if {$k < 0 || $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[my baseadr] numval]
      lset ::constcl::vectorSpace $k+$base $obj
    }
    return [self]
  }
  method fill! {val} {
    if {[my constant]} {
      ::error "vector is constant"
    } else {
      set base [[my baseadr] numval]
      set len [[my length] numval]
      for {set idx $base} \
        {$idx < $len+$base} \
        {incr idx} {
        lset ::constcl::vectorSpace $idx $val
      }
    }
    return [self]
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method write {handle} {
    puts -nonewline $handle [my show]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "#(%s)" [
      join [lmap val [my value] {$val show}]]
  }
}

interp alias {} ::constcl::MkVector \
  {} ::constcl::Vector new
```


__vector?__

<table border=1><thead><tr><th colspan=2 align="left">vector? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg vector? ::constcl::vector?

proc ::constcl::vector? {val} {
  typeof? $val Vector
}
```


__make-vector__


`` make-vector `` creates a vector with a given length and optionally a fill value. If a fill value isn't given, the empty list will be used.

<table border=1><thead><tr><th colspan=2 align="left">make-vector? (public)</th></tr></thead><tr><td>k</td><td>a number</td></tr><tr><td>?fill?</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>



Example:

```
(let ((k 3))
  (make-vector k))        =>  #(() () ())
(let ((k 3) (fill #\A))
  (make-vector k fill))   =>  #(#\A #\A #\A)
```
```
reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {k args} {
  if {[llength $args] == 0} {
    set fill #NIL
  } else {
    lassign $args fill
  }
  MkVector [lrepeat [$k numval] $fill]
}
```


__vector__


Given a number of Lisp values, `` vector `` creates a vector containing them.

<table border=1><thead><tr><th colspan=2 align="left">vector (public)</th></tr></thead><tr><td>args</td><td>some Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>



Example:

```
(vector 'a 'b 'c)   =>  #(a b c)
```
```
reg vector ::constcl::vector

proc ::constcl::vector {args} {
  MkVector $args
}
```


__vector-length__


`` vector-length `` returns the length of a vector.

<table border=1><thead><tr><th colspan=2 align="left">vector-length (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>



Example:

```
(vector-length #(a b c))   =>  3
```
```
reg vector-length

proc ::constcl::vector-length {vec} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show])
  }
  return [$vec length]
}
```


__vector-ref__


`` vector-ref `` returns the element of _vec_ at index _k_ (0-based).

<table border=1><thead><tr><th colspan=2 align="left">vector-ref (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>



Example:

```
(let ((vec #(a b c)) (k 1))
  (vector-ref vec k))          =>  b
```
```
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] [$k show])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec show] [$k show])
  }
  return [$vec ref $k]
}
```


__vector-set!__


`` vector-set! ``, for a non-constant vector, sets the element at index _k_ to _val_.

<table border=1><thead><tr><th colspan=2 align="left">vector-set! (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td>k</td><td>a number</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>



Example:

```
(let ((vec #(a b c))
      (k 1)
      (val 'x))
  (vector-set! vec k val))      =>  *error*
(let ((vec (vector 'a 'b 'c))
      (k 1)
      (val 'x))
  (vector-set! vec k val))      =>  #(a x c)
```
```
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k val} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] [$k show])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec show] [$k show])
  }
  return [$vec set! $k $val]
}
```


__vector->list__


`` vector->list `` converts a vector value to a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">vector-&gt;list (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>



Example:

```
(vector->list #(a b c))   =>  (a b c)
```
```
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
  list {*}[$vec value]
}
```


__list->vector__


`` list->vector `` converts a Lisp list value to a vector.

<table border=1><thead><tr><th colspan=2 align="left">list-&gt;vector (public)</th></tr></thead><tr><td>list</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>



Example:

```
(list->vector '(1 2 3))   =>  #(1 2 3)
```

VB( (list->vector '(1 2 3)) => #(1 2 3) VB)

```
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
  vector {*}[splitlist $list]
}
```


__vector-fill!__


`` vector-fill! `` fills a non-constant vector with a given value.

<table border=1><thead><tr><th colspan=2 align="left">vector-fill! (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td>fill</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>



Example:

```
(define vec (vector 'a 'b 'c))
(vector-fill! vec 'x)             =>  #(x x x)
vec                               =>  #(x x x)
```
```
reg vector-fill! ::constcl::vector-fill!

proc ::constcl::vector-fill! {vec fill} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] \
      [$fill show])
  }
  $vec fill! $fill
}
```
## Identifier validation


__idcheckinit__


__idchecksubs__


__idcheck__


__varcheck__


Some routines for checking if a string is a valid identifier. `` idcheckinit `` checks the first character, `` idchecksubs `` checks the rest. `` idcheck `` calls the others and raises an error if they fail. A valid symbol is still an invalid identifier if has the name of some keyword, which `` varcheck `` checks, for a set of keywords given in the standard.

```
proc ::constcl::idcheckinit {init} {
  if {[::string is alpha -strict $init] ||
    $init in {! $ % & * / : < = > ? ^ _ ~}} {
    return true
  } else {
    return false
  }
}
```
```
proc ::constcl::idchecksubs {subs} {
  foreach c [split $subs {}] {
    if {!([::string is alnum -strict $c] ||
      $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
      return false
    }
  }
  return true
}
```
```
proc ::constcl::idcheck {sym} {
  if {$sym eq {}} {return $sym}
  if {(![idcheckinit [::string index $sym 0]] ||
    ![idchecksubs [::string range $sym 1 end]]) &&
    $sym ni {+ - ...}} {
    ::error "Identifier expected ($sym)"
  }
  set sym
}
```
```
proc ::constcl::varcheck {sym} {
  if {$sym in {
    else => define unquote unquote-splicing
    quote lambda if set! begin cond and or
    case let let* letrec do delay quasiquote
  }} {
    ::error "Variable name is reserved: $sym"
  }
  return $sym
}
```
## S9fES


I've begun porting parts of S9fES (_Scheme 9 from Empty Space_, by Nils M Holm) to fill out the blanks in e.g. I/O. It remains to be seen if it is successful.


I've already mixed this up with my own stuff.

```
proc ::constcl::new-atom {pa pd} {
  cons3 $pa $pd $::constcl::ATOM_TAG
}
```
```
proc cons3 {pcar pcdr ptag} {
  # TODO counters
  set n [MkPair $pcar $pcdr]
  $n settag $ptag
  return $n
}
```
```
proc ::constcl::xread {} {
  if {[$::constcl::InputPort handle] eq "#NIL"} {
    error "input port is not open"
  }
  set ::constcl::Level 0
  return [read-form 0]
}
```
```
proc ::constcl::read_c_ci {} {
  tolower [
    ::read [
      $::constcl::Input_port handle] 1]]
}
```
## Environment class and objects


The class for environments is called `` Environment ``. It is mostly a wrapper around a dictionary, with the added finesse of keeping a link to the outer environment (starting a chain that goes all the way to the global environment and then stops at the null environment) which can be traversed by the find method to find which innermost environment a given symbol is bound in.


The long and complex constructor is to accommodate the variations of Scheme parameter lists, which can be empty, a proper list, a symbol, or a dotted list.


__Environment__ class

```
catch { ::constcl::Environment destroy }

oo::class create ::constcl::Environment {
  variable bindings outer_env
  constructor {syms vals {outer {}}} {
    set bindings [dict create]
    if {[::constcl::null? $syms] eq "#t"} {
      if {[llength $vals]} {
        error "too many arguments"
      }
    } elseif {[::constcl::list? $syms] eq "#t"} {
      set syms [::constcl::splitlist $syms]
      set symsn [llength $syms]
      set valsn [llength $vals]
      if {$symsn != $valsn} {
        error [
          ::append --> "wrong # of arguments, " \
            "$valsn instead of $symsn"]
      }
      foreach sym $syms val $vals {
        my set $sym $val
      }
    } elseif {[::constcl::symbol? $syms] eq "#t"} {
      my set $syms [::constcl::list {*}$vals]
    } else {
      while true {
        if {[llength $vals] < 1} {
          error "too few arguments"
        }
        my set [::constcl::car $syms] \
          [lindex $vals 0]
        set vals [lrange $vals 1 end]
        if {[
          ::constcl::symbol? [
            ::constcl::cdr $syms]] eq "#t"} {
          my set [::constcl::cdr $syms] \
            [::constcl::list {*}$vals]
          set vals {}
          break
        } else {
          set syms [::constcl::cdr $syms]
        }
      }
    }
    set outer_env $outer
  }
  method find {sym} {
    if {$sym in [dict keys $bindings]} {
      self
    } else {
      $outer_env find $sym
    }
  }
  method get {sym} {
    dict get $bindings $sym
  }
  method set {sym val} {
    dict set bindings $sym $val
  }
}
```
### Lexical scoping


Example:

```
ConsTcl> (define (circle-area r) (* pi (* r r)))
ConsTcl> (circle-area 10)
314.1592653589793
```


During a call to the procedure `` circle-area ``, the symbol `` r `` is bound to the value 10. But we don't want the binding to go into the global environment, possibly clobbering an earlier definition of `` r ``. The solution is to use separate (but linked) environments, making `` r ``'s binding a _local variable_W{Local_variable} in its own environment, which the procedure will be evaluated in. The symbols `` * `` and `` pi `` will still be available through the local environment's link to the outer global environment. This is all part of _lexical scoping_W{Scope_(computer_science)#Lexical_scope}.


In the first image, we see the global environment before we call `` circle-area `` (and also the empty null environment which the global environment links to):

![#](images/env1.png)

During the call. Note how the global `` r `` is shadowed by the local one, and how the local environment links to the global one to find `` * `` and `` pi ``.

![#](images/env2.png)

After the call, we are back to the first state again.

![#](images/env1.png)
## Initialization


Initialize the memory space for vector contents.

```
set ::constcl::vectorSpace [lrepeat 1024 #NIL]

set ::constcl::vectorAssign 0

proc ::constcl::vsAlloc {num} {
  # TODO calculate free space
  set va $::constcl::vectorAssign
  incr ::constcl::vectorAssign $num
  return $va
}
```
```
set ::constcl::gensymnum 0
```


Pre-make a set of constants (e.g. #NIL, #t, and #f) and give them aliases for use in source text.

```
interp alias {} #NIL {} [::constcl::NIL new]

interp alias {} #t {} [::constcl::MkBoolean #t]

interp alias {} #f {} [::constcl::MkBoolean #f]

interp alias {} #-1 {} [N -1]

interp alias {} #0 {} [N 0]

interp alias {} #1 {} [N 1]

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #UNS {} [::constcl::Unspecified new]

interp alias {} #UND {} [::constcl::Undefined new]

interp alias {} #EOF {} [::constcl::EndOfFile new]
```


Initialize the definition register with the queen of numbers (or at least a double-precision floating point approximation).

```
dict set ::constcl::defreg pi [N 3.1415926535897931]
```


In this interpreter, `` nil `` does refer to the empty list.

```
reg nil #NIL
```
### Environment startup


On startup, two `` Environment `` objects called `` null_env `` (the null environment, not the same as `` null-environment `` in Scheme) and `` global_env `` (the global environment) are created.


Make `` null_env `` empty and judgemental: this is where searches for unbound symbols end up.

```
::constcl::Environment create \
  ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
  method find {sym} {
    self
  }
  method get {sym} {
    ::error "Unbound variable: [$sym name]"
  }
  method set {sym val} {
    ::error "Unbound variable: [$sym name]"
  }
}
```


Meanwhile, `` global_env `` is populated with all the definitions from the definitions register, `` defreg ``. This is where top level evaluation happens.

```
namespace eval ::constcl {
  set keys [list {*}[lmap key [dict keys $defreg] {
    S $key
  }]]
  set vals [dict values $defreg]
  Environment create global_env $keys $vals \
    ::constcl::null_env
}
```


Load the Scheme base to add more definitions to the global environment.

```
pe {(load "schemebase.scm")}
```


Thereafter, each time a user-defined procedure is called, a new `` Environment `` object is created to hold the bindings introduced by the call, and also a link to the outer environment (the one closed over when the procedure was defined).

## The REPL


The REPL (read-eval-print loop) is a loop that repeatedly _reads_ a Scheme source string from the user through the command `` ::constcl::input `` (breaking the loop if given an empty line) and `` ::constcl::parse ``, _evaluates_ it using `` ::constcl::eval ``, and _prints_ using `` ::constcl::write ``.



__input__


`` input `` is modelled after the Python 3 function. It displays a prompt and reads a string.

```
proc ::constcl::input {prompt} {
  puts -nonewline $prompt
  flush stdout
  set buf [gets stdin]
  set openpars [regexp -all -inline {\(} $buf]
  set clsepars [regexp -all -inline {\)} $buf]
  set openbrak [regexp -all -inline {\[} $buf]
  set clsebrak [regexp -all -inline {\]} $buf]
  while {[llength $openpars] > [llength $clsepars] ||
         [llength $openbrak] > [llength $clsebrak]} {
    ::append buf [gets stdin]
    set openpars [regexp -all -inline {\(} $buf]
    set clsepars [regexp -all -inline {\)} $buf]
    set openbrak [regexp -all -inline {\[} $buf]
    set clsebrak [regexp -all -inline {\]} $buf]
  }
  return $buf
}
```


__repl__


`` repl `` puts the 'loop' in the read-eval-print loop. It repeats prompting for a string until given a blank input. Given non-blank input, it parses and evaluates the string, printing the resulting value.

```
proc ::repl {{prompt "ConsTcl> "}} {
  set cur_env [Environment new #NIL {} ::global_env]
  set str [::constcl::input $prompt]
  while {$str ne ""} {
    set expr [::constcl::parse $str]
    set val [::constcl::eval $expr $cur_env]
    ::constcl::write $val
    set str [::constcl::input $prompt]
  }
}
```


## A Scheme base

```

; An assortment of procedures to supplement the builtins.

(define (get plist key)
  (let ((v (memq key plist)))
    (if v
      (cadr v)
      #f)))

(define (list-find-key lst key)

(define (lfk lst key count)
  (if (null? lst)
    -1
    (if (eq? (car lst) key)
      count
      (lfk (cddr lst) key (+ count 2)))))

(define (list-set! lst idx val)
  (if (zero? idx)
    (set-car! lst val)
    (list-set! (cdr lst) (- idx 1) val)))

(define (delete! lst key)
  (let ((idx (list-find-key lst key)))
      lst
        (set! lst (cddr lst))
        (let ((bef (del-seek lst (- idx 1)))
              (aft (del-seek lst (+ idx 2))))
          (set-cdr! bef aft))))
    lst))

(define (del-seek lst idx)
  (if (zero? idx)
    lst
    (del-seek (cdr lst) (- idx 1))))

(define (get-alist lst key)
  (let ((item (assq key lst)))
    (if item
      (cdr item)
      #f)))

(define (pairlis a b)
  (if (null? a)
    ()
    (cons
      (cons (car a) (car b))
      (pairlis (cdr a) (cdr b)))))

(define (set-alist! lst key val)
  (let ((item (assq key lst)))
    (if item
      (begin (set-cdr! item val) lst)
      lst)))

(define (fact n)
  (if (<= n 1)
    1
    (* n (fact (- n 1)))))

```
