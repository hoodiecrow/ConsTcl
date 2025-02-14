# ConsTcl

## Introduction

### To run the software

First things first. To run, source the file __constcl.tcl__ (with
__schemebase.lsp__ in the directory) in a Tcl console (I use __tkcon__) and use
the command __repl__ for a primitive command dialog.  Source
__all.tcl__ to run the test suite (you need __constcl.test__ for that).

### Background

ConsTcl is a second try at a Lisp interpreter written in Tcl--the first one was 
Thtcl[#](https://github.com/hoodiecrow/thtcl)--this time with a real Lisp-like 
type system. 

### About ConsTcl

It's written with Vim, the one and only editor. 

It steps over and back over the border between Tcl and Lisp a lot
of times while working, and as a result is fairly slow.
On my cheap computer, the following code (which calculates the factorial of
100) takes 0.027 seconds to run.

```
time {pe "(fact 100)"} 10
```

Speed aside, it is an amusing piece of machinery. The types are implemented as TclOO
classes, and evaluation is to a large extent applying Lisp methods to Tcl data.

It is limited. Quite a few standard procedures are missing. It doesn't come
near to having call/cc or tail recursion. It doesn't have exact/inexact
numbers, or most of the numerical tower. Error reporting is spotty, and there
is no error recovery.

### About the book

I like writing documentation, and occasionally I'm good at it. When I work on a
software project, I like to annotate the source code with bits of
documentation, which I then extract and put together using document stream
editing tools like `sed` and `awk` (The pipeline is Vim to create annotated
source > sed/awk > a markdown README document for GitHub's benefit > awk > a
(La)TeX document > TeXworks > a PDF document: all the steps except the last are
automated using make). On finishing up ConsTcl, it struck me that the
documentation for this piece of software was fit for a book.

### About the program listings

I have tried to write clear, readable code, but the page format forces me to
shorten lines. I have used two-space indents instead of four-space, a smaller
font, and broken off long lines with a \ at the end of the first line (a
so-called "tucked-in tail"). Neither of these measures improve readability, but
the alternative is overwriting the margins.

### About me

I'm a 60 year old former system manager who has dabbled in programming since
1979--46 years. Currently, since around 25 years, my language of choice is the
rather marginal Tcl (it's not even in the 100 most used languages). Tcl suits
me, and there are things that one can do in Tcl that one can't easily do in
other languages. Lisp is a runner-up in my affections, a language that
fascinates me but doesn't fit my brain very well (though I have written one
large piece of software in AutoLisp).

In addition to my terms as programmer and system manager, I have worked as a
teacher (teaching C/C++ in upper secondary school) and for a short while I
produced teaching materials for the department for information technology at
the University of Skövde. I've also been active writing answers at
question-and-answer sites on the web, mainly Stack Overflow.

## Initial declarations

First, I need to create the namespace that will be used for most identifiers:

```
namespace eval ::constcl {}
```

### Utility commands

Next, some procedures that make my life as developer somewhat easier, but
don't really matter to the interpreter (except the two first ones). The other ones
will show up a lot in the test cases.

__reg__

`reg` registers selected built-in procedures in the standard library.

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

__regmacro__

`regmacro` registers macro names in the macro list, so the evaluator knows what
to expand.

<table border=1><thead><tr><th colspan=2 align="left">regmacro (internal)</th></tr></thead><tr><td>name</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::regmacro {name} {
  lappend ::constcl::macrolist $name
  return
}
```

__pep__

`pep` was named after the sequence parse-eval-print, and I never changed the
name. It reads and evals an expression, and prints the result.

<table border=1><thead><tr><th colspan=2 align="left">pep (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::pep {str} {
  ::constcl::write [
    ::constcl::eval [
      ::constcl::parse $str]]
}
```

__pp__

`pp` is the same, only it doesn't eval the expression. It just prints what is
parsed.

<table border=1><thead><tr><th colspan=2 align="left">pp (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::pp {str} {
  ::constcl::write [
    ::constcl::parse $str]
}
```

__pe__

`pe` is still the same, but it doesn't print the expression. It just evals what
is read.

<table border=1><thead><tr><th colspan=2 align="left">pe (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::pe {str} {
  ::constcl::eval [
    ::constcl::parse $str]
}
```

__p__

`p` is mostly the same, but it only parses the input, returning an expression.

<table border=1><thead><tr><th colspan=2 align="left">p (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::p {str} {
  ::constcl::parse $str
}
```

__e__

`e` is another single-action procedure, eval-ing an expression and returning a
value.

<table border=1><thead><tr><th colspan=2 align="left">e (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::e {expr} {
  ::constcl::eval $expr
}
```

__w__

`w` is the third single-action procedure, printing a value and that's all.

<table border=1><thead><tr><th colspan=2 align="left">w (internal)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::w {val} {
  ::constcl::write $val
}
```

__r__

`r` is an extra single-action procedure, reading from default input or from a
port and returning an expression.

<table border=1><thead><tr><th colspan=2 align="left">r (internal)</th></tr></thead><tr><td>?port?</td><td>an input port</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::r {args} {
  ::constcl::read {*}$args
}
```

__prp__

`prp` is a busy thing. It reads an expression, expands macros in it, resolves
defines, and prints the result.

<table border=1><thead><tr><th colspan=2 align="left">prp (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
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
```

__pxp__

`pxp` attempts to macro-expand whatever it reads, and prints the result. I know
that 'expand' doesn't start with an 'x'.

<table border=1><thead><tr><th colspan=2 align="left">pxp (internal)</th></tr></thead><tr><td>str</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::pxp {str} {
  set expr [::constcl::parse $str]
  set expr [::constcl::expand-macro $expr ::constcl::global_env]
  ::constcl::write $expr
}
```

__pn__

"Procedure name" When called, tells the caller the name of its command.

<table border=1><thead><tr><th colspan=2 align="left">pn (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a Tcl string</td></tr></table>

```
proc ::pn {} {
  lindex [split [lindex [info level -1] 0] :] end
}
```

__typeof?__

`typeof?` looks at a value's type and reports if it is the same as the given
type. To be certain, it looks at the value in two ways: once assuming that the value
is a ConsTcl object, and once assuming that the value is an interpreter (the Tcl
interpreter, not ConsTcl) alias for a ConsTcl object. If one of those affirms
the type, the procedure returns #t.

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

<table border=1><thead><tr><th colspan=2 align="left">in-range (public)</th></tr></thead><tr><td>x</td><td>a number</td></tr><tr><td>?e?</td><td>a number</td></tr><tr><td>?t?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of numbers</td></tr></table>

```
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
```

### The NIL class

The `NIL` class has one object: the empty list called `#NIL`. It is also base class for many other
type classes.

```
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
```

__null?__

The `null?` standard predicate recognizes the empty list. Predicates in ConsTcl
return #t or #f for true or false, so some care is necessary when calling them
from Tcl code (the Tcl `if` command expects 1 or 0 as truth values).

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

### The classes None, Dot, Unspecific, Undefined, and EndOfFile

The `None` class serves but one purpose: to avoid printing a result after `define`.

```
catch { ::constcl::None destroy}

oo::class create ::constcl::None {}
```

The `Dot` class is a helper class for the parser.

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

__dot?__

`dot?` is a type predicate that checks for membership in the type `Dot`.

<table border=1><thead><tr><th colspan=2 align="left">dot? (internal)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
proc ::constcl::dot? {val} {
  return [typeof? $val "Dot"]
}
```

The `Unspecific` class is for unspecific things.

```
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
```

The `Undefined` class is for undefined things.

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

The `EndOfFile` class is for end-of-file conditions.

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

### The error and check procedures

__error__

`error` is used to signal an error, with **msg** being a message string and the
optional arguments being values to show after the message.

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

__check__

`check` does a check (usually a type check) on something and throws an error if
it fails.

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

__atom?__

`atom?` recognizes an atom by checking for membership in any one of the atomic types.

<table border=1><thead><tr><th colspan=2 align="left">atom? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
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
```

## S9fES

I've begun porting parts of S9fES (**Scheme 9 from Empty Space**, by Nils M Holm) to fill out the blanks in e.g. I/O. It remains to be seen if it is successful.

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

proc ::constcl::read_c_ci {} {
  tolower [
    ::read [
      $::constcl::Input_port handle] 1]]
}
```



## Input

The first thing an interpreter must be able to do is to take in the user's code
and data input, whether from the keyboard or from a source file.  `read`
represents the interpreter's main input facility. As a complement, a similar set
of procedures that read input from an input buffer exists (the `parse-`
procedures). The main set (the `read-` procedures) read from standard input,
or--if a port is provided--from the port's channel.

__IB__ class

A quick-and-dirty input simulator, using an input buffer object to hold
characters to be read.

```
catch { ::constcl::IB destroy }

oo::class create ::constcl::IB {
  variable peekc buffer
  constructor {str} {
    set peekc {}
    my fill $str
  }
}
```

The `fill` method fills the buffer and sets the first character in the peek position.  

```
oo::define ::constcl::IB method fill {str} {
  set buffer $str
  my advance
}
```

The `advance` method consumes one character from the buffer. 

```
oo::define ::constcl::IB method advance {} {
  if {$buffer eq {}} {
    set peekc {}
  } else {
    set peekc [::string index $buffer 0]
    set buffer [::string range $buffer 1 end]
  }
}
```

`peek` peeks at the next character to be read. 

```
oo::define ::constcl::IB method peek {} {
  return $peekc
}
```

`unget` backs up one position and sets a given character in the peek position. 

```
oo::define ::constcl::IB method unget {char} {
  set buffer $peekc$buffer
  set peekc $char
}
```

The `find` method looks past whitespace to find a given character. It returns
Tcl truth if it is found.  Or it gets the hose again. 

```
oo::define ::constcl::IB method find {char} {
  if {[::string is space -strict $peekc]} {
    for {set cp 0} \
        {$cp < [::string length $buffer]} \
        {incr cp} {
      if {![::string is space -strict [
        ::string index $buffer $cp]]} {
        break
      }
    }
    return [expr {[
      ::string index $buffer $cp] eq $char}]
  } else {
    return [expr {$peekc eq $char}]
  }
}
```

`skip-ws` advances past whitespace and comments.  

```
oo::define ::constcl::IB method skip-ws {} {
  while true {
    switch -regexp $peekc {
      {[[:space:]]} {
        my advance
      }
      {;} {
        while {$peekc ne "\n" && $peekc ne {}} {
          my advance
        }
      }
      default {
        return
      }
    }
  }
}
```

### The parse procedure

#### Parsing

Parsing[#](https://en.wikipedia.org/wiki/Parsing), or syntactic analysis, is
analyzing a sequence of letters, digits, and other characters, conforming to the
rules of **external representation**. The result of parsing is an **expression**
in internal form.

The parsing process translates an expression from external representation to
internal representation. The external representation is a 'recipe' for an
expression that expresses it in a unique way. 

For example, the external representation for a vector is a sharp sign (#), a
left parenthesis ((), the external representation for some values, and a right
parenthesis ()). When the reader or parser is working through input, a `#(`
symbol signals that a vector structure is being read. A number of subexpressions
for the elements of the vector follow, and then a closing parenthesis `)`
signals that the vector is done. The elements are saved in vector memory and the
vector gets the address to the first element and the number of elements.

![vrep](/images/vector-representation)

The `parse` procedure takes in the input buffer character by character, matching
each character against a fitting external representation. When done, it creates
a ConsTcl object, which is the internal representation of an expression.  The
object can then be passed to the evaluator.

Given a string, `parse` fills the input buffer. It then parses the input and
produces the internal representation of an expression.

Example:

```
% ::constcl::parse "(+ 2 3)"
::oo::Obj491
```

Here, `parse` parsed the external representation of a list with three elements,
+, 2, and 3. It produced the expression that has an internal representation
labeled `::oo::Obj491`. We will later meet procedures like `eval`, which
transforms an expression into a value, and `write`, which prints a printed
representation of expressions and values. Putting them together: we can see

```
% ::constcl::write ::oo::Obj491
(+ 2 3)
% ::constcl::eval ::oo::Obj491
::oo::Obj494
% ::constcl::write ::oo::Obj494
5
```

Fortunately, we don't have to work at such a low level. We can use the `repl`
instead:

```
ConsTcl> (+ 2 3)
5
```

Then, parsing and evaluation and writing goes on in the background and the
internal representations of expressions and values are hidden.

Anyway, here is how it really looks like. `::oo::Obj491` was just the head of
the list.

![intreplist](/images/intreplist.png)

__parse__

<table border=1><thead><tr><th colspan=2 align="left">parse (internal)</th></tr></thead><tr><td>inp</td><td>a Tcl string or an input buffer</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
reg parse

proc ::constcl::parse {inp} {
  if {[info object isa object $inp]} {
    set ib $inp
  } else {
    set ib [IB new $inp]
  }
  return [parse-expr]
}
```

__parse-expr__

The procedure `parse-expr` parses input by peeking at the first available
character and delegating to one of the more detailed parsing procedures based on
that, producing an expression of any kind.

<table border=1><thead><tr><th colspan=2 align="left">parse-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::parse-expr {} {
  upvar ib ib
  $ib skip-ws
  switch -regexp [$ib peek] {
    {\"}          { parse-string-expr }
    {\#}          { parse-sharp }
    {\'}          { parse-quoted-expr }
    {\(}          { parse-pair-expr ")" }
    {\+} - {\-}   { parse-plus-minus }
    {\,}          { parse-unquoted-expr }
    {\.} { $ib advance ; return [Dot new] }
    {\:}          { parse-object-expr }
    {\[}          { parse-pair-expr "\]" }
    {\`}          { parse-quasiquoted-expr }
    {\d}          { parse-number-expr }
    {^$}          { return #NONE}
    {[[:graph:]]} { parse-identifier-expr }
    default {
      ::error "unexpected character ([$ib peek])"
    }
  }
}
```

__parse-string-expr__

`parse-string-expr` parses input starting with a double quote and collects
characters until it reaches another (unescaped) double quote. It then returns a
string expression--a String[#](https://github.com/hoodiecrow/ConsTcl#strings) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-string-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

```
proc ::constcl::parse-string-expr {} {
  upvar ib ib
  set str {}
  $ib advance
  while {[$ib peek] ne "\"" && [$ib peek] ne {}} {
    set c [$ib peek]
    if {$c eq "\\"} {
      $ib advance
      ::append str [$ib peek]
    } else {
      ::append str $c
    }
    $ib advance
  }
  if {[$ib peek] ne "\""} {
    ::error "no ending double quote"
  }
  $ib advance
  $ib skip-ws
  set expr [MkString $str]
  $expr mkconstant
  return $expr
}
```


__parse-sharp__

`parse-sharp` parses input starting with a sharp sign (#) and produces the various kinds of
expressions whose external representation begins with a sharp sign.

<table border=1><thead><tr><th colspan=2 align="left">parse-sharp (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a vector, boolean, or character value</td></tr></table>

```
proc ::constcl::parse-sharp {} {
  upvar ib ib
  $ib advance
  switch [$ib peek] {
    (    { return [parse-vector-expr] }
    t    { $ib advance ; $ib skip-ws ; return #t }
    f    { $ib advance ; $ib skip-ws ; return #f }
    "\\" { return [parse-character-expr] }
    default {
      ::error "Illegal #-literal: #[$ib peek]"
    }
  }
}
```

__make-constant__

The `make-constant` helper procedure is called to set components of expressions to
constants when read as a quoted literal.

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

__parse-quoted-expr__

`parse-quoted-expr` parses input starting with a "'", and then parses an entire
expression beyond that, returning it wrapped in a list with `quote`.

<table border=1><thead><tr><th colspan=2 align="left">parse-quoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expression wrapped in the quote symbol</td></tr></table>

```
proc ::constcl::parse-quoted-expr {} {
  upvar ib ib
  $ib advance
  set expr [parse-expr]
  $ib skip-ws
  make-constant $expr
  return [list [S quote] $expr]
}
```


__parse-pair-expr__

The `parse-pair-expr` procedure parses input and produces a structure of
Pair[#](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists)s expression.

<table border=1><thead><tr><th colspan=2 align="left">parse-pair-expr (internal)</th></tr></thead><tr><td>char</td><td>the terminating paren or bracket</td></tr><tr><td><i>Returns:</i></td><td>a structure of pair expressions</td></tr></table>

```

proc ::constcl::parse-pair {char} {
  upvar ib ib
  if {[$ib find $char]} {
    return #NIL
  }
  $ib skip-ws
  set a [parse-expr]
  $ib skip-ws
  set res $a
  set prev #NIL
  while {![$ib find $char]} {
    set x [parse-expr]
    $ib skip-ws
    if {[dot? $x] ne "#f"} {
      set prev [parse-expr]
      $ib skip-ws
    } else {
      lappend res $x
    }
    if {[llength $res] > 999} break
  }
  foreach r [lreverse $res] {
    set prev [cons $r $prev]
  }
  return $prev
}

proc ::constcl::parse-pair-expr {char} {
  upvar ib ib
  $ib advance
  $ib skip-ws
  set expr [parse-pair $char]
  $ib skip-ws
  if {[$ib peek] ne $char} {
    if {$char eq ")"} {
      ::error \
        "Missing right paren. ([$ib peek])."
    } else {
      ::error \
        "Missing right bracket ([$ib peek])."
    }
  }
  $ib advance
  $ib skip-ws
  return $expr
}
```


__parse-plus-minus__

`parse-plus-minus` reacts to a plus or minus in the input buffer, and either
returns a `+` or `-` symbol, or a number.

<table border=1><thead><tr><th colspan=2 align="left">parse-plus-minus (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>either the symbols + or - or a number</td></tr></table>

```
proc ::constcl::parse-plus-minus {} {
  upvar ib ib
  set c [$ib peek]
  $ib advance
  if {[::string is digit -strict [$ib peek]]} {
    $ib unget $c
    return [::constcl::parse-number-expr]
  } else {
    if {$c eq "+"} {
      $ib skip-ws
      return [MkSymbol "+"]
    } else {
      $ib skip-ws
      return [MkSymbol "-"]
    }
  }
}
```

__parse-unquoted-expr__

`parse-unquoted-expr` parses input, producing an expression and returning
it wrapped in `unquote`, or in `unquote-splicing` if an @-sign is present in
the input stream.

<table border=1><thead><tr><th colspan=2 align="left">parse-unquoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expression wrapped in the unquote/-splicing symbol</td></tr></table>

```
proc ::constcl::parse-unquoted-expr {} {
  upvar ib ib
  $ib advance
  set symbol "unquote"
  if {[$ib peek] eq "@"} {
    set symbol "unquote-splicing"
    $ib advance
  }
  set expr [parse-expr]
  $ib skip-ws
  return [list [MkSymbol $symbol] $expr]
}
```


__parse-quasiquoted-expr__

`parse-quasiquoted-expr` parses input, producing an expression and returning it wrapped in `quasiquote`.

<table border=1><thead><tr><th colspan=2 align="left">parse-quasiquoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expression wrapped in the quasiquote symbol</td></tr></table>

```
proc ::constcl::parse-quasiquoted-expr {} {
  upvar ib ib
  $ib advance
  set expr [parse-expr]
  $ib skip-ws
  make-constant $expr
  return [list [MkSymbol "quasiquote"] $expr]
}
```


__interspace__

The `interspace` helper procedure recognizes whitespace between value
representations.

```
proc ::constcl::interspace {c} {
  # don't add #EOF: parse-* uses this one too
  if {$c eq {} ||
    [::string is space $c] ||
    $c eq ";"} {
      return #t
    } else {
      return #f
    }
}
```

__parse-number-expr__

`parse-number-expr` parses input, producing a number and returning a Number[#](https://github.com/hoodiecrow/ConsTcl#numbers) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-number-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
proc ::constcl::parse-number-expr {} {
  upvar ib ib
  while {[interspace [$ib peek]] ne "#t" && \
    [$ib peek] ni {) \]}} {
      ::append num [$ib peek]
      $ib advance
    }
    $ib skip-ws
    check {::string is double -strict $num} {
      Invalid numeric constant $num
    }
    return [MkNumber $num]
}
```


__parse-identifier-expr__

`parse-identifier-expr` parses input, producing an identifier expression and returning a Symbol[#](https://github.com/hoodiecrow/ConsTcl#symbols) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-identifier-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a symbol</td></tr></table>

```
proc ::constcl::parse-identifier-expr {} {
  upvar ib ib
  while {[interspace [$ib peek]] ne "#t" &&
      [$ib peek] ni {) \]}} {
    ::append name [$ib peek]
    $ib advance
  }
  $ib skip-ws
  # idcheck throws error if invalid identifier
  return [MkSymbol [idcheck $name]]
}
```


__character-check__

The `character-check` helper procedure compares a potential
character constant to the valid kinds.

<table border=1><thead><tr><th colspan=2 align="left">character-check (internal)</th></tr></thead><tr><td>name</td><td>a Tcl string</td></tr><tr><td><i>Returns:</i></td><td>a Tcl truth value (1 or 0)</td></tr></table>

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

__parse-character-expr__

`parse-character-expr` parses input, producing a character and returning
a Char[#](https://github.com/hoodiecrow/ConsTcl#characters) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-character-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a character</td></tr></table>

```
proc ::constcl::parse-character-expr {} {
  upvar ib ib
  set name "#"
  while {[interspace [$ib peek]] ne "#t" &&
      [$ib peek] ni {) ]}} {
    ::append name [$ib peek]
    $ib advance
  }
  check {character-check $name} {
    Invalid character constant $name
  }
  $ib skip-ws
  return [MkChar $name]
}
```


__parse-vector-expr__

`parse-vector-expr` parses input, producing a vector expression and returning a Vector[#](https://github.com/hoodiecrow/ConsTcl#vectors) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-vector-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>

```
proc ::constcl::parse-vector-expr {} {
  upvar ib ib
  $ib advance
  $ib skip-ws
  set res {}
  while {[$ib peek] ne {} && [$ib peek] ne ")"} {
    lappend res [parse-expr]
    $ib skip-ws
  }
  set vec [MkVector $res]
  $vec mkconstant
  if {[$ib peek] ne ")"} {
    ::error "Missing right parenthesis."
  }
  $ib advance
  $ib skip-ws
  return $vec
}
```


__parse-object-expr__

A non-standard extension, `parse-object-expr` reads one of the ConsTcl objects
and passes its name along.

<table border=1><thead><tr><th colspan=2 align="left">parse-object-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a ConsTcl object</td></tr></table>

```
proc ::constcl::parse-object-expr {} {
  upvar ib ib
  foreach ch [split "::oo::Obj" {}] {
    if {[$ib peek] ne $ch} {
      error "bad object name"
    }
    $ib advance
  }
  set res "::oo::Obj"
  while {[::string is digit [$ib peek]]} {
    ::append res [$ib peek]
    $ib advance
  }
  return $res
}
```

### read

__read__

The standard builtin `read` reads and parses input into a Lisp expression in a
similar manner to how `parse` parses a string buffer.

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

__read-expr__

The procedure `read-expr` parses input by reading the first available
character and delegating to one of the more detailed reading procedures based on
that, producing an expression of any kind. A Tcl character value can be passed
to it, that character will be used first before reading from the input stream.
If the end of file is encountered before an expression can be read in full, the
procedure returns end of file.

<table border=1><thead><tr><th colspan=2 align="left">read-expr (internal)</th></tr></thead><tr><td>?char?</td><td>a Tcl character</td></tr><tr><td><i>Returns:</i></td><td>an expression or end of file</td></tr></table>

```
proc ::constcl::read-expr {args} {
  upvar c c unget unget
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
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
    {\.}          { Dot new }
    {\:}          { read-object-expr }
    {\[}          { read-pair-expr "\]" }
    {\`}          { read-quasiquoted-expr }
    {\d}          { read-number-expr $c }
    {^$}          { return #NONE}
    {[[:graph:]]} { read-identifier-expr $c }
    default {
      read-eof $c
      ::error "unexpected character ($c)"
    }
  }
}
```


`readc` reads one character either from the unget store or from the input
stream. If the input stream is at end-of-file, an eof object is returned.

<table border=1><thead><tr><th colspan=2 align="left">readc (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a Tcl character or end of file</td></tr></table>

```
proc readc {} {
  upvar unget unget
  if {$unget ne {}} {
    set c $unget
    set unget {}
  } else {
    set c [::read [$::constcl::Input_port handle] 1]
    if {[eof [$::constcl::Input_port handle]]} {
      return #EOF
    }
  }
  return $c
}
```

`read-find` reads ahead through whitespace to find a given character. Returns 1
if it has found the character, and 0 if it has stopped at some other character.
Returns end of file if eof is encountered.

<table border=1><thead><tr><th colspan=2 align="left">read-find (internal)</th></tr></thead><tr><td>char</td><td>a Tcl character</td></tr><tr><td><i>Returns:</i></td><td>a Tcl truth value (1 or 0) or end of file</td></tr></table>

```
proc read-find {char} {
  upvar c c unget unget
  while {[::string is space -strict $c]} {
    set c [readc]
    read-eof $c
    set unget $c
  }
  return [expr {$c eq $char}]
}
```

`skip-ws` skips whitespace and comments (the ; to end of line kind). Uses the
shared _c_ character. It leaves the first character not to be skipped in _c_.

<table border=1><thead><tr><th colspan=2 align="left">skip-ws (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc skip-ws {} {
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
        return
      }
    }
  }
}
```

`read-eof` checks a number of characters for possible end-of-file objects. If it
finds one, it returns **from its caller** with the EOF value.

<table border=1><thead><tr><th colspan=2 align="left">read-eof (internal)</th></tr></thead><tr><td>args</td><td>some characters</td></tr></table>

```
proc read-eof {args} {
  foreach val $args {
    if {$val eq "#EOF"} {
      return -level 1 -code return #EOF
    }
  }
}
```

__read-string-expr__

`read-string-expr` parses input starting with a double quote and collects
characters until it reaches another (unescaped) double quote. It then returns a
string expression--an immutable
String[#](https://github.com/hoodiecrow/ConsTcl#strings) object.

<table border=1><thead><tr><th colspan=2 align="left">read-string-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a string or end of file</td></tr></table>

```
proc ::constcl::read-string-expr {} {
  upvar c c unget unget
  set str {}
  set c [readc]
  read-eof $c
  while {$c ne "\"" && $c ne "#EOF"} {
    if {$c eq "\\"} {
      set c [readc]
    }
    ::append str $c
    set c [readc]
  }
  if {$c ne "\""} {
    error "bad string (no ending double quote)"
  }
  set c [readc]
  set expr [MkString $str]
  read-eof $expr
  $expr mkconstant
  return $expr
}
```

__read-sharp__

`read-sharp` parses input starting with a sharp sign (#) and produces the various kinds of
expressions whose external representation begins with a sharp sign.

<table border=1><thead><tr><th colspan=2 align="left">read-sharp (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a vector, boolean, or character value or end of file</td></tr></table>

```
proc ::constcl::read-sharp {} {
  upvar c c unget unget
  set c [readc]
  read-eof $c
  switch $c {
    (    { set n [read-vector-expr] }
    t    { set n #t }
    f    { set n #f }
    "\\" { set n [read-character-expr] }
    default {
      read-eof $c
      ::error "Illegal #-literal: #$c"
    }
  }
  set c [readc]
  return $n
}
```

__read-vector-expr__

`read-vector-expr` parses input, producing a vector expression and returning a Vector[#](https://github.com/hoodiecrow/ConsTcl#vectors) object.

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
  set expr [MkVector $res]
  read-eof $expr
  $expr mkconstant
  if {$c ne ")"} {
    ::error "Missing right paren. ($c)."
  }
  set c [readc]
  return $expr
}
```

__read-character-expr__

`read-character-expr` parses input, producing a character and returning
a Char[#](https://github.com/hoodiecrow/ConsTcl#characters) object.

<table border=1><thead><tr><th colspan=2 align="left">read-character-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a character or end of file</td></tr></table>

```
proc ::constcl::read-character-expr {} {
  upvar c c unget unget
  set name "#\\"
  set c [readc]
  read-eof $c
  while {[::string is alpha $c]} {
    ::append name $c
    set c [readc]
    read-eof $c
  }
  check {character-check $name} {
      Invalid character constant $name
  }
  set expr [MkChar $name]
  read-eof $expr
  return $expr
}
```

__read-quoted-expr__

`read-quoted-expr` parses input starting with a "'", and then parses an entire
expression beyond that, returning it wrapped in a list with `quote`.

<table border=1><thead><tr><th colspan=2 align="left">read-quoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expression wrapped in the quote symbol or end of file</td></tr></table>

```
proc ::constcl::read-quoted-expr {} {
  upvar c c unget unget
  set expr [read-expr]
  read-eof $expr
  make-constant $expr
  return [list [S quote] $expr]
}
```

__read-pair-expr__

The `read-pair-expr` procedure parses input and produces a structure of
Pair[#](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists)s expression.

<table border=1><thead><tr><th colspan=2 align="left">read-pair-expr (internal)</th></tr></thead><tr><td>char</td><td>the terminating paren or bracket</td></tr><tr><td><i>Returns:</i></td><td>a structure of pair expressions or end of file</td></tr></table>

```
proc ::constcl::read-pair-expr {char} {
  upvar c c unget unget
  set expr [read-pair $char]
  read-eof $expr
  skip-ws
  read-eof $c
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

proc ::constcl::read-pair {char} {
  upvar c c unget unget
  if {[read-find $char]} {
    # read right paren/brack
    set c [readc]
    return #NIL
  }
  set c [readc]
  read-eof $c
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
      skip-ws $c]
      read-eof $c
    } else {
      lappend res $x
    }
    if {[llength $res] > 999} break
  }
  # read right paren/brack
  foreach r [lreverse $res] {
    set prev [cons $r $prev]
  }
  return $prev
}
```

__read-plus-minus__

`read-plus-minus` reacts to a plus or minus in the input stream, and either
returns a `+` or `-` symbol, or a number.

<table border=1><thead><tr><th colspan=2 align="left">read-plus-minus (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>either the symbols + or - or a number or end of file</td></tr></table>

```
proc ::constcl::read-plus-minus {char} {
  upvar c c unget unget
  set c [readc]
  read-eof $c
  if {[::string is digit -strict $c]} {
    set n [read-number-expr $c]
    if {$char eq "-"} {
      set n [- $n]
    }
    return $n
  } else {
    if {$char eq "+"} {
      return [MkSymbol "+"]
    } else {
      return [MkSymbol "-"]
    }
  }
}
```

__read-number-expr__

`read-number-expr` parses input, producing a number and returning a Number[#](https://github.com/hoodiecrow/ConsTcl#numbers) object.

<table border=1><thead><tr><th colspan=2 align="left">read-number-expr (internal)</th></tr></thead><tr><td>?char?</td><td>a Tcl character</td></tr><tr><td><i>Returns:</i></td><td>a number or end of file</td></tr></table>

```
proc ::constcl::read-number-expr {args} {
  upvar c c unget unget
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
  read-eof $c
  while {[interspace $c] ne "#t" && $c ne "#EOF" &&
      $c ni {) \]}} {
    ::append num $c
    set c [readc]
  }
  set unget $c
  check {::string is double -strict $num} {
      Invalid numeric constant $num
  }
  set expr [MkNumber $num]
  read-eof $expr
  return $expr
}
```

__read-unquoted-expr__

`read-unquoted-expr` parses input, producing an expression and returning
it wrapped in `unquote`, or in `unquote-splicing` if an @-sign is present in
the input stream.

<table border=1><thead><tr><th colspan=2 align="left">read-unquoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expr. wr. in the unquote/-splicing symbol or end of file</td></tr></table>

```
proc ::constcl::read-unquoted-expr {} {
  upvar c c unget unget
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
  return [list [MkSymbol $symbol] $expr]
}
```

__read-object-expr__

A non-standard extension, `read-object-expr` reads one of the ConsTcl objects
and passes its name along.

<table border=1><thead><tr><th colspan=2 align="left">read-object-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a ConsTcl object or end of file</td></tr></table>

```
proc ::constcl::read-object-expr {} {
  upvar c c unget unget
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

__read-quasiquoted-expr__

`read-quasiquoted-expr` parses input, producing an expression and returning it wrapped in `quasiquote`.

<table border=1><thead><tr><th colspan=2 align="left">read-quasiquoted-expr (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>an expr. wr. in the quasiquote symbol or end of file</td></tr></table>

```
proc ::constcl::read-quasiquoted-expr {} {
  upvar c c unget unget
  set expr [read-expr]
  skip-ws
  read-eof $expr
  make-constant $expr
  return [list [MkSymbol "quasiquote"] $expr]
}
```

__read-identifier-expr__

`read-identifier-expr` parses input, producing an identifier expression and returning a Symbol[#](https://github.com/hoodiecrow/ConsTcl#symbols) object.

<table border=1><thead><tr><th colspan=2 align="left">read-identifier-expr (internal)</th></tr></thead><tr><td>?char?</td><td>a Tcl character</td></tr><tr><td><i>Returns:</i></td><td>a symbol or end of file</td></tr></table>

```
proc ::constcl::read-identifier-expr {args} {
  upvar c c unget unget
  if {[llength $args]} {
    lassign $args c
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
  }
  if {$c ne "#EOF"} {
    set unget $c
  }
  read-eof $name
  # idcheck throws error if invalid identifier
  idcheck $name
  return [MkSymbol $name]
}
```


## Evaluation

### Syntactic forms

The second thing an interpreter must be able to do is to reduce expressions to
their normal form, or **evaluate** them. As an example, 2 + 6 and 8 are two
expressions that have the same value, but the latter is in normal form (can't be
reduced further) and the former is not.

There are nine diffent forms or classes of expressions in Lisp.

<table id="syntaxforms"><thead>
<tr><th>Syntactic form</th> <th>Syntax</th> </tr>
</thead>
<tbody>
<tr> <td>Variable reference</td><td>variable</td></tr>
<tr> <td>Constant literal</td><td>number or boolean, etc</td></tr>
<tr> <td>Quotation</td><td>quote datum</td></tr>
<tr> <td>Sequence</td><td>begin expression...</td></tr>
<tr> <td>Conditional</td><td>if test conseq alt</td></tr>
<tr> <td>Definition</td><td>define identifier expression</td></tr>
<tr> <td>Assignment</td><td>set! variable expression</td><td></tr>
<tr> <td>Procedure definition</td><td>lambda formals body</td></tr>
<tr> <td>Procedure call</td><td>operator operand...</td></tr>
</tbody></table>


__eval__

The heart of the Lisp interpreter, `eval` takes a Lisp expression and processes
it according to its syntactic form.

`eval`:

1. processes an **expression** to get a **value**. The exact method depends on the form of expression, see above and below.
1. does a form of **macro expansion** on the car and cdr of a non-atomic expression before processing it further. See the part about macros[#](https://github.com/hoodiecrow/ConsTcl#macros) below.
1. resolves **local defines**, acting on expressions of the form `(begin (define ...` when in a local environment. See the part about resolving local defines[#](https://github.com/hoodiecrow/ConsTcl#resolving-local-defines).

<table border=1><thead><tr><th colspan=2 align="left">eval (public)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg eval ::constcl::eval

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
      quote { car $args }
      if { if {[eval [car $args] $env] ne "#f"} \
        {eval [cadr $args] $env} \
        {eval [caddr $args] $env} }
      begin { /begin $args $env }
      define { /define [car $args] [
        eval [cadr $args] $env] $env }
      set! { /set! [car $args] [
        eval [cadr $args] $env] $env }
      lambda { /lambda [car $args] [
        cdr $args] $env }
      default { invoke [eval $op $env] [
        eval-list $args $env] }
    }
  }
}
```


#### Variable reference

Example: `r` => 10 (a symbol `r` is evaluated to 10)

A variable is an identifier (symbol) bound to a location in the environment. If
an expression consists of the identifier it is evaluated to the value stored in
that location. This is handled by the helper procedure `lookup`. It searches the
environment chain for the identifier, and returns the value stored in the
location it is bound to.  It is an error to lookup an unbound symbol.

__lookup__

<table border=1><thead><tr><th colspan=2 align="left">lookup (internal)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::lookup {sym env} {
  [$env find $sym] get $sym
}
```

#### Constant literal

Example: `99` => 99 (a number evaluates to itself)

Not just numbers but booleans, characters, strings, and vectors evaluate to
themselves, to their innate value. Because of this, they are called autoquoting
types (see next paragraph).

#### Quotation

Example: `(quote r)` => `r` (quotation makes the symbol evaluate to itself, like a
constant)

According to the rules of Variable reference, a symbol evaluates to its stored
value. Well, sometimes one wishes to use the symbol itself as a value. That is
what quotation is for. `(quote x)` evaluates to the symbol x itself and not to
any value that might be stored under it. This is so common that there is a
shorthand notation for it: `'x` is interpreted as `(quote x)` by the Lisp
reader.

#### Conditional

Example: `(if (> 99 100) (* 2 2) (+ 2 4))` => 6

The conditional form `if` evaluates a Lisp list of three expressions. The first,
the _condition_, is evaluated first. If it evaluates to anything other than `#f`
(false), the second expression (the _consequent_) is evaluated and the value
returned. Otherwise, the third expression (the _alternate_) is evaluated and the
value returned. One of the two latter expressions will be evaluated, and the
other will remain unevaluated.

__/if__

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

Example: `(begin (define r 10) (* r r))` => 100

When expressions are evaluated in sequence, the order is important for two
reasons. If the expressions have any side effects, they happen in the same order
of sequence. Also, if expressions are part of a pipeline of calculations, then
they need to be processed in the order of that pipeline. The `/begin` helper
procedure takes a Lisp list of expressions and evaluates them in sequence,
returning the value of the last one.

__/begin__

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

Example: `(define r 10)` => ... (a definition doesn't evaluate to anything)

We've already seen the relationship between symbols and values. A symbol is
bound to a value (or rather to the location the value is in), creating a
variable, through definition. The `/define` helper procedure adds a variable to
the current environment. It first checks that the symbol name is a valid
identifier, then it updates the environment with the new binding.

__/define__

<table border=1><thead><tr><th colspan=2 align="left">/define (internal)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::/define {sym val env} {
  varcheck [idcheck [$sym name]]
  $env set $sym $val
  return #NONE
}
```

#### Assignment

Example: `(set! r 20)` => 20 (`r` is a bound symbol, so it's allowed to assign
to it)

Once a variable has been created, the value at the location it is bound to can
be changed (hence the name "variable", something that can be modified). The
process is called assignment. The `/set!` helper does assignment: it modifies
an existing variable that is bound somewhere in the environment chain. It finds
the variable's environment and updates the binding. It returns the value, so
calls to `set!` can be chained: `(set! foo (set! bar 99))` sets both variables
to 99.

__/set!__

<table border=1><thead><tr><th colspan=2 align="left">/set! (internal)</th></tr></thead><tr><td>var</td><td>a bound symbol</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::/set! {var val env} {
  [$env find $var] set $var $val
  set val
}
```

#### Procedure definition

Example: `(lambda (r) (* r r))` => ::oo::Obj3601 (it will be a different object
each time)

In Lisp, procedures are values just like numbers or characters. They can be
defined as the value of a symbol, passed to other procedures, and returned from
procedures. One diffence from most values is that procedures need to be defined.
Two questions must answered: what is the procedure meant to do? The code that
does that will form the body of the procedure. Also, what, if any, items of data
will have to be provided to the procedure to make it possible to calculate its
result?

As an example, imagine that we want to have a procedure that calculates the
square (`x · x`) of a given number. In Lisp, expressions are written with
the operator first and then the operands: `(* x x)`. That is the body of the
procedure. Now, what data will we have to provide to the procedure to make it
work? A value stored in the variable `x` will do. It's only a single variable,
but by custom we need to put it in a list: `(x)`. The operator that defines
procedures is called `lambda`, and we define the function with `(lambda (x) (* x
x))`.

One more step is needed before we can use the procedure. It must have a name. We
could define it like this: `(define square (lambda (x) (* x x)))` but there is
actually a shortcut notation for it: `(define (square x) (* x x))`.

Now, `square` is pretty tame. How about the `hypotenuse` procedure? `(define
(hypotenuse a b) (sqrt (+ (square a) (square b))))`. It calculates the square
root of the sum of two squares.

Under the hood, the helper `/lambda` makes a
Procedure[#](https://github.com/hoodiecrow/ConsTcl#control) object. First it
needs to convert the Lisp list `body`. It is packed inside a `begin` if it has
more than one expression, and taken out of its list if not. The Lisp list
`formals` is passed on as is.

A Scheme formals list is either:

* An **empty list**, `()`, meaning that no arguments are accepted,
* A **proper list**, `(a b c)`, meaning it accepts three arguments, one in each symbol,
* A **symbol**, `a`, meaning that all arguments go into `a`, or
* A **dotted list**, `(a b . c)`, meaning that two arguments go into `a` and `b`, and the rest into `c`.

__/lambda__

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

Example: `(+ 1 6)` => 7

Once we have procedures, we can call them to have their calculations performed
and yield results. The procedure name is put in the operator position at the
front of a list, and the operands follow in the rest of the list. Our `square`
procedure would be called for instance like this: `(square 11)`, and it will
return 121.

`invoke` arranges for a procedure to be called with each of the values in
the _argument list_ (the list of operands). It checks if pr really is a
procedure, and determines whether to call pr as an object or as a Tcl command.

__invoke__

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


__splitlist__

`splitlist` converts a Lisp list to a Tcl list with Lisp objects.

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

__eval-list__

`eval-list` successively evaluates the elements of a Lisp list and returns the
collected results as a Lisp list.

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


```
proc ::constcl::scheme-report-environment {version} {
    # TODO
}
```

```
proc ::constcl::null-environment {version} {
    # TODO
}
```

```
proc ::constcl::interaction-environment {} {
    # TODO
}
```


### Macros

__expand-macro__

Macros that rewrite expressions into other, more concrete expressions is one of
Lisp's strong points. This interpreter does macro expansion, but the user can't
define new macros--the ones available are hardcoded in the code below.

`expand-macro` takes an expression and an environment as a parameter. First, the
operator (`op`) and operands (`args`) are extracted to check if expansion is
necessary. If the operator is the symbol `define` and the first of the
operands is something other than a Pair, then expansion is unnecessary and the procedure
returns with a code to break the while loop in `eval`.

The operator's symbol name is then used to select the right expansion procedure,
and the whole expression and the environment is passed to it. In the end, the
expanded expression is passed back to `eval`.

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

__expand-and__

`expand-and` expands the `and` macro. It returns a `begin`-expression if the
macro has 0 or 1 elements, and a nested `if` construct otherwise. `S begin`
stands for "the symbol begin".

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

<table border=1><thead><tr><th colspan=2 align="left">do-and (internal)</th></tr></thead><tr><td>tail</td><td>an expression tail</td></tr><tr><td>prev</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::do-and {tail prev env} {
  set env [Environment new #NIL {} $env]
  if {[[length $tail] numval] == 0} {
    return $prev
  } else {
    $env set [S first] [car $tail]
    $env set [S rest] [do-and [cdr $tail] \
        [car $tail] $env]
    set qq "`(if ,first ,rest #f)"
    return [expand-quasiquote [parse $qq] $env]
  }
}
```

__expand-case__

The `case` macro is expanded by `expand-case`. It returns `'()` if there are no clauses (left), 
and nested `if` constructs if there are some.

<table border=1><thead><tr><th colspan=2 align="left">expand-case (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro case

proc ::constcl::expand-case {expr env} {
  set tail [cdr $expr]
  do-case [car $tail] [cdr $tail]
}

proc ::constcl::do-case {keyexpr clauses} {
  if {[eq? [length $clauses] #0] ne "#f"} {
    return [list [S quote] #NIL]
  } else {
    set keyl [caar $clauses]
    set body [cdar $clauses]
    set keyl [list [S memv] $keyexpr \
        [list [S quote] $keyl]]
    if {[eq? [length $clauses] #1] ne "#f"} {
      if {[eq? [caar $clauses] [S else]] ne "#f"} {
        set keyl #t
      }
    }
    return [list [S if] $keyl \
        [cons [S begin] $body] \
        [do-case $keyexpr [cdr $clauses]]]
  }
}
```

__expand-cond__

The `cond` macro is expanded by `expand-cond`. It returns `'()` if there are no
clauses (left), and nested `if` constructs if there are some.


<table border=1><thead><tr><th colspan=2 align="left">expand-cond (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro cond

proc ::constcl::expand-cond {expr env} {
  return [do-cond [cdr $expr] $env]
}

proc ::constcl::do-cond {tail env} {
  set clauses $tail
  if {[eq? [length $clauses] #0] ne "#f"} {
    return [list [S quote] #NIL]
  } else {
    set pred [caar $clauses]
    set body [cdar $clauses]
    if {[symbol? [car $body]] ne "#f" &&
        [[car $body] name] eq "=>"} {
      set body [cddar $clauses]
    }
    if {[eq? [length $clauses] #1] ne "#f"} {
      if {[eq? $pred [S else]] ne "#f"} {
        set pred #t
      }
    }
    if {[null? $body] ne "#f"} {
        set body $pred
    }
    return [list [S if] $pred \
        [cons [S begin] $body] \
        [do-cond [cdr $clauses] $env]]
  }
}
```

__expand-define__

`define` has two variants, one of which requires some rewriting. It's the one
with an implied `lambda` call, the one that defines a procedure. 

(define (**symbol** **formals**) **body**)

is transformed into

(define **symbol** (lambda **formals** **body**))

which conforms better to `eval`'s standard of (define **symbol** **value**).

<table border=1><thead><tr><th colspan=2 align="left">expand-define (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro define

proc ::constcl::expand-define {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  $env set [S tail] $tail
  set qq "`(define ,(caar tail)
             (lambda ,(cdar tail) ,@(cdr tail)))"
  return [expand-quasiquote [parse $qq] $env]
}
```

__expand-del!__

The macro `del!` updates a property list. It removes a key-value pair if the key
is present, or leaves the list untouched if it isn't.

<table border=1><thead><tr><th colspan=2 align="left">expand-del! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro del!

proc ::constcl::expand-del! {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
    ::error "too few arguments, 0 of 2"
  }
  $env set [S listname] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {
    ::error "too few arguments, 1 of 2"
  }
  $env set [S key] [cadr $tail]
  set qq "`(set! ,listname
             (delete! ,listname ,key))"
  return [expand-quasiquote [parse $qq] $env]
}
```

__expand-for__

The `expand-for` procedure expands the `for` macro. It returns a `begin`
construct containing the iterations of each clause (multiple clauses
weren't implemented, but I brought up my strongest brain cells and they
did it).

<table border=1><thead><tr><th colspan=2 align="left">for-seq (internal)</th></tr></thead><tr><td>seq</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of Lisp values</td></tr></table>

```
regmacro for

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
    set seq [lmap c [split [$seq value] {}] \
        {MkChar #\\$c}]
  } elseif {[vector? $seq] ne "#f"} {
    set seq [$seq value]
  }
}
```

<table border=1><thead><tr><th colspan=2 align="left">do-for (internal)</th></tr></thead><tr><td>tail</td><td>an expression tail</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of expressions</td></tr></table>

```
proc ::constcl::do-for {tail env} {
  # make clauses a Tcl list
  set clauses [splitlist [car $tail]]
  set body [cdr $tail]
  set ids {}
  set seqs {}
  for {set i 0} \
      {$i < [llength $clauses]} \
      {incr i} {
    set clause [lindex $clauses $i]
    # insert the first part of the
    # clause in the ids structure
    lset ids $i [car $clause]
    # run the second part of the clause
    # through for-seq and insert in seqs
    lset seqs $i [for-seq [cadr $clause] $env]
  }
  set res {}
  for {set item 0} \
      {$item < [llength [lindex $seqs 0]]} \
      {incr item} {
    # for each iteration of the sequences
    set x {}
    for {set clause 0} \
        {$clause < [llength $clauses]} \
        {incr clause} {
      # for each clause
      # list append to x the Lisp list
      # of the id and the iteration
      lappend x [list [lindex $ids $clause] \
          [lindex $seqs $clause $item]]
    }
    # list append to res a let expression
    # with the ids and iterations and the body
    lappend res [list [S let] [
        list {*}$x] {*}[splitlist $body]]
  }
  return $res
}
```

<table border=1><thead><tr><th colspan=2 align="left">expand-for (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-for {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  lappend res [list [S quote] #NIL]
  return [list [S begin] {*}$res]
}
```

__expand-for/and__

The `expand-for/and` procedure expands the `for/and` macro. It returns an `and`
construct containing the iterations of the clauses.

<table border=1><thead><tr><th colspan=2 align="left">expand-for/and (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro for/and

proc ::constcl::expand-for/and {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [MkSymbol "and"] {*}$res]
}
```

__expand-for/list__

The `expand-for/list` procedure expands the `for/list` macro. It returns a `list`
construct containing the iterations of each clause.

<table border=1><thead><tr><th colspan=2 align="left">expand for/list (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro for/list

proc ::constcl::expand-for/list {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [MkSymbol "list"] {*}$res]
}
```

__expand-for/or__

The `expand-for/or` procedure expands the `for/or` macro. It returns an `or`
construct containing the iterations of each clause.

<table border=1><thead><tr><th colspan=2 align="left">expand-for/or (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro for/or

proc ::constcl::expand-for/or {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [MkSymbol "or"] {*}$res]
}
```

__expand-let__

`expand-let` expands the named `let` and 'regular' `let` macros. They ultimately
expand to `lambda` constructs.

<table border=1><thead><tr><th colspan=2 align="left">expand-let (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro let

proc ::constcl::expand-let {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  if {[symbol? [car $tail]] ne "#f"} {
    # named let
    set variable [car $tail]
    set bindings [cadr $tail]
    set body [cddr $tail]
    set vars [dict create $variable #f]
    parse-bindings vars $bindings
    $env set [S decl] [list {*}[dict values [dict map {k v} $vars {list $k $v}]]]
    $env set [S variable] $variable
    $env set [S varlist] [list {*}[lrange [dict keys $vars] 1 end]]
    $env set [S body] $body
    $env set [S call] [list {*}[dict keys $vars]]
    set qq "`(let ,decl (set! ,variable (lambda ,varlist ,@body)) ,call)"
    return [expand-quasiquote [parse $qq] $env]
  } else {
    # regular let
    set bindings [car $tail]
    set body [cdr $tail]
    set vars [dict create]
    parse-bindings vars $bindings
    $env set [S varlist] [list {*}[dict keys $vars]]
    $env set [S body] $body
    $env set [S vallist] [list {*}[dict values $vars]]
    set qq "`((lambda ,varlist ,@body) ,@vallist)"
    return [expand-quasiquote [parse $qq] $env]
  }
}

proc ::constcl::parse-bindings {name bindings} {
  upvar $name vars
  foreach binding [splitlist $bindings] {
    set var [car $binding]
    set val [cadr $binding]
    if {$var in [dict keys $vars]} {::error "variable '$var' occurs more than once in let construct"}
    dict set vars $var $val
  }
}
```

__expand-or__

`expand-or` expands the `or` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.

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

<table border=1><thead><tr><th colspan=2 align="left">do-or (internal)</th></tr></thead><tr><td>tail</td><td>an expression tail</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::do-or {tail env} {
  set env [::constcl::Environment new #NIL {} $env]
  /if {eq? [length $tail] #0} {
    return #f
  } {
    $env set [S first] [car $tail]
    $env set [S rest] [do-or [cdr $tail] $env]
    set qq "`(let ((x ,first)) (if x x ,rest))"
    return [expand-quasiquote [parse $qq] $env]
  }
}
```

__expand-pop!__

The macro `push!` updates a list. It adds a new element as the new first element.

<table border=1><thead><tr><th colspan=2 align="left">expand-pop! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro pop!

proc ::constcl::expand-pop! {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
  $env set [MkSymbol "obj"] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
  if {[symbol? [cadr $tail]] eq "#f"} {::error "SYMBOL expected:\n(push! obj listname)"}
  $env set [MkSymbol "listname"] [cadr $tail]
  set qq "`(set! ,listname (cdr ,listname))"
  return [expand-quasiquote [parse $qq] $env]
}
```

__expand-push!__

The macro `push!` updates a list. It adds a new element as the new first element.

<table border=1><thead><tr><th colspan=2 align="left">expand-push! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro push!

proc ::constcl::expand-push! {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
  $env set [MkSymbol "obj"] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
  if {[symbol? [cadr $tail]] eq "#f"} {::error "SYMBOL expected:\n(push! obj listname)"}
  $env set [MkSymbol "listname"] [cadr $tail]
  set qq "`(set! ,listname (cons ,obj ,listname))"
  return [expand-quasiquote [parse $qq] $env]
}
```

__expand-put!__

The macro `put!` updates a property list. It adds a key-value pair if the key
isn't present, or changes the value in place if it is.

<table border=1><thead><tr><th colspan=2 align="left">expand-put! (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro put!

proc ::constcl::expand-put! {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {::error "too few arguments, 3 expected, got 0"}
  $env set [MkSymbol "listname"] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments, 3 expected, got 1"}
  $env set [MkSymbol "key"] [cadr $tail]
  if {[null? [cddr $tail]] ne "#f"} {::error "too few arguments, 3 expected, got 2"}
  $env set [MkSymbol "val"] [caddr $tail]
  set qq "`(let ((idx (list-find-key ,listname ,key)))
             (if (< idx 0)
               (set! ,listname (append (list ,key ,val) ,listname))
               (begin (list-set! ,listname (+ idx 1) ,val) ,listname)))"
  return [expand-quasiquote [parse $qq] $env]
}
```

__expand-quasiquote__

A quasi-quote isn't a macro, but we will deal with it in this section anyway. `expand-quasiquote`
traverses the quasi-quoted structure searching for `unquote` and `unquote-splicing`. This code is
brittle and sprawling and I barely understand it myself.

<table border=1><thead><tr><th colspan=2 align="left">qq-visit-child (internal)</th></tr></thead><tr><td>node</td><td>a Lisp list of expressions</td></tr><tr><td>qqlevel</td><td>a Tcl number</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of expressions</td></tr></table>

```
regmacro quasiquote

proc ::constcl::qq-visit-child {node qqlevel env} {
  if {$qqlevel < 0} {
    set qqlevel 0
  }
  if {[list? $node] ne "#f"} {
    set res {}
    foreach child [splitlist $node] {
      if {[pair? $child] ne "#f" && [eq? [car $child] [MkSymbol "unquote"]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res [eval [cadr $child] $env]
        } else {
          lappend res [list [S unquote] [qq-visit-child [cadr $child] [expr {$qqlevel - 1}] $env]]
        }
      } elseif {[pair? $child] ne "#f" && [eq? [car $child] [MkSymbol "unquote-splicing"]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res {*}[splitlist [eval [cadr $child] $env]]
        }
      } elseif {[pair? $child] ne "#f" && [eq? [car $child] [MkSymbol "quasiquote"]] ne "#f"} {
        lappend res [list [MkSymbol "quasiquote"] [car [qq-visit-child [cdr $child] [expr {$qqlevel + 1}] $env]]] 
      } elseif {[atom? $child] ne "#f"} {
        lappend res $child
      } else {
        lappend res [qq-visit-child $child $qqlevel $env]
      }
    }
  }
  return [list {*}$res]
}
```

<table border=1><thead><tr><th colspan=2 align="left">expand-quasiquote (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-quasiquote {expr env} {
  set tail [cdr $expr]
  set qqlevel 0
  if {[list? [car $tail]] ne "#f"} {
    set node [car $tail]
    return [qq-visit-child $node 0 $env]
  } elseif {[vector? [car $tail]] ne "#f"} {
    set vect [car $tail]
    set res {}
    for {set i 0} {$i < [[vector-length $vect] numval]} {incr i} {
      set idx [MkNumber $i]
      set vecref [vector-ref $vect $idx]
      if {[pair? $vecref] ne "#f" && [eq? [car $vecref] [MkSymbol "unquote"]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res [eval [cadr $vecref] $env]
        }
      } elseif {[pair? $vecref] ne "#f" && [eq? [car $vecref] [MkSymbol "unquote-splicing"]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res {*}[splitlist [eval [cadr $vecref] $env]]
        }
      } elseif {[atom? $vecref] ne "#f"} {
        lappend res $vecref
      } else {
      }
    }
    return [list [MkSymbol "vector"] {*}$res]
  }
}
```

__expand-unless__

`unless` is a conditional like `if`, with the differences that it takes a number
of expressions and only executes them for a false outcome of `car $tail`.

<table border=1><thead><tr><th colspan=2 align="left">expand-unless (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro unless

proc ::constcl::expand-unless {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  $env set [S tail] $tail
  set qq "`(if ,(car tail) (quote ()) (begin ,@(cdr tail)))"
  return [expand-quasiquote [parse $qq] $env]
}
```

__expand-when__

`when` is a conditional like `if`, with the differences that it takes a number
of expressions and only executes them for a true outcome of `car $tail`.

<table border=1><thead><tr><th colspan=2 align="left">expand-when (internal)</th></tr></thead><tr><td>expr</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
regmacro when

proc ::constcl::expand-when {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  $env set [S tail] $tail
  set qq "`(if ,(car tail) (begin ,@(cdr tail)) (quote ()))"
  return [expand-quasiquote [parse $qq] $env]
}
```



### Resolving local defines

This section is ported from 'Scheme 9 from Empty Space'. `resolve-local-defines`
is the topmost procedure in rewriting local defines as essentially a `letrec`
form. It takes a list of expressions and extracts variables and values from the
defines in the beginning of the list. It builds a double lambda expression with
the variables and values, and the rest of the expressions from the original list
as body.

__resolve-local-defines__

<table border=1><thead><tr><th colspan=2 align="left">resolve-local-defines</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::resolve-local-defines {exps} {
  set rest [lassign [extract-from-defines $exps VALS] a error]
  if {$error ne "#f"} {
    return #NIL
  }
  set rest [lassign [extract-from-defines $exps VARS] v error]
  if {$rest eq "#NIL"} {
    set rest [cons #UNSP #NIL]
  }
  return [make-recursive-lambda $v $a $rest]
}
```

__extract-from-defines__

`extract-from-defines` visits every define in the given list of expressions and
extracts either a variable name or a value, depending on the state of the _part_
flag, from each one of them. A Tcl list of 1) the resulting list of names or
values, 2) error state, and 3) the rest of the expressions in the original list
is returned.

<table border=1><thead><tr><th colspan=2 align="left">extract-from-defines (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>part</td><td>a flag, VARS or VALS</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of Lisp values</td></tr></table>

```
proc ::constcl::extract-from-defines {exps part} {
  set a #NIL
  while {$exps ne "#NIL"} {
    if {[atom? $exps] ne "#f" || [atom? [car $exps]] ne "#f" || [eq? [caar $exps] [MkSymbol "define"]] eq "#f"} {
      break
    }
    set n [car $exps]
    set k [length $n]
    if {[list? $n] eq "#f" || [$k numval] < 3 || [$k numval] > 3 ||
      ([argument-list? [cadr $n]] ne "#f" || [symbol? [cadr $n]] eq "#f")
      eq "#f"} {
        return [::list {} "#t" {}]
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

__argument-list?__

`argument-list?` accepts a Scheme formals list and rejects other values.

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

__make-recursive-lambda__

`make-recursive-lambda` builds the `letrec` structure.

<table border=1><thead><tr><th colspan=2 align="left">make-recursive-lambda (internal)</th></tr></thead><tr><td>vars</td><td>a Lisp list of symbols</td></tr><tr><td>args</td><td>a Lisp list of expressions</td></tr><tr><td>body</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::make-recursive-lambda {vars args body} {
  set tmps [make-temporaries $vars]
  set body [append-b [make-assignments $vars $tmps] $body]
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

__make-temporaries__

`make-temporaries` creates the symbols that will act as middlemen in
transferring the values to the variables.

<table border=1><thead><tr><th colspan=2 align="left">make-temporaries (internal)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
proc ::constcl::make-temporaries {vals} {
  set n #NIL
  while {$vals ne "#NIL"} {
    set sym [gensym "g"]
    set n [cons $sym $n]
    set vals [cdr $vals]
  }
  return $n
}
```

__gensym__

`gensym` generates an unique symbol.

<table border=1><thead><tr><th colspan=2 align="left">gensym (internal)</th></tr></thead><tr><td>prefix</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a symbol</td></tr></table>

```
proc ::constcl::gensym {prefix} {
  set symbolnames [lmap s [info class instances ::constcl::Symbol] {$s name}]
  set s $prefix<[incr ::constcl::gensymnum]>
  while {$s in $symbolnames} {
    set s $prefix[incr ::constcl::gensymnum]
  }
  return [MkSymbol $s]
}
```

__append-b__

`append-b` joins two lists together.

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

__make-assignments__

`make-assignments` creates the structure that holds the assignment statements.
Later on, it will be joined to the body of the finished expression.

<table border=1><thead><tr><th colspan=2 align="left">make-assignments (internal)</th></tr></thead><tr><td>vars</td><td>a Lisp list of symbols</td></tr><tr><td>tmps</td><td>a Lisp list of symbols</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::make-assignments {vars tmps} {
  set n #NIL
  while {$vars ne "#NIL"} {
    set asg [cons [car $tmps] #NIL]
    set asg [cons [car $vars] $asg]
    set asg [cons [S set!] $asg]
    set n [cons $asg $n]
    set vars [cdr $vars]
    set tmps [cdr $tmps]
  }
  return [cons [S begin] $n]
}
```

__make-undefineds__

Due to a mysterious bug, `make-undefineds` actually creates a list of NIL
values instead of undefined values.

<table border=1><thead><tr><th colspan=2 align="left">make-undefineds (internal)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of nil values</td></tr></table>

```
proc ::constcl::make-undefineds {vals} {
  # Use #NIL instead of #UNDF because of some strange bug with eval-list.
  set n #NIL
  while {$vals ne "#NIL"} {
    set n [cons #NIL $n]
    set vals [cdr $vals]
  }
  return $n
}
```



## Output

__write__

The third member in the great triad is `write`. As long as the object
given to it isn't `#NONE`, it passes it to `write-value` and prints
a newline.

<table border=1><thead><tr><th colspan=2 align="left">write (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td>?port?</td><td>a port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg write ::constcl::write

proc ::constcl::write {val args} {
  if {$val ne "#NONE"} {
    if {[llength $args]} {
      lassign $args port
    } else {
      set port [MkOutputPort stdout]
    }
    set ::constcl::Output_port $port
    write-value [$::constcl::Output_port handle] $val
    puts [$::constcl::Output_port handle] {}
    set ::constcl::Output_port [MkOutputPort stdout]
  }
  return
}
```

__write-value__

`write-value` simply calls an object's `write` method, letting the object
write itself.

<table border=1><thead><tr><th colspan=2 align="left">write-value (internal)</th></tr></thead><tr><td>handle</td><td>a channel handle</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::write-value {handle val} {
  $val write $handle
  return
}
```

__display__

The `display` procedure is like `write` but doesn't print a newline.

<table border=1><thead><tr><th colspan=2 align="left">display (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td>?port?</td><td>a port</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg display ::constcl::display

proc ::constcl::display {val args} {
  if {$val ne "#NONE"} {
    if {[llength $args]} {
      lassign $args port
    } else {
      set port [MkOutputPort stdout]
    }
    set ::constcl::Output_port $port
    $val display [$::constcl::Output_port handle]
    flush [$::constcl::Output_port handle]
    set ::constcl::Output_port [MkOutputPort stdout]
  }
  return
}
```

__write-pair__

The `write-pair` procedure prints a Pair object.

<table border=1><thead><tr><th colspan=2 align="left">write-pair (internal)</th></tr></thead><tr><td>handle</td><td>a channel handle</td></tr><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::write-pair {handle pair} {
  # take an object and print the car and the cdr of the stored value
  set a [car $pair]
  set d [cdr $pair]
  # print car
  write-value $handle $a
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
    write-value $handle $d
  }
  return
}
```


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

<table border=1><thead><tr><th colspan=2 align="left">eq?, eqv?, equal? (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
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
```


__eqv?__

```
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
```

__equal?__

```
reg equal? ::constcl::equal?

proc ::constcl::equal? {val1 val2} {
  if {[$val1 show] eq [$val2 show]} {
    return #t
  } else {
    return #f
  }
  # TODO
}
```


### Numbers

I have only implemented a bare-bones version of Scheme's numerical
library. The following is a reasonably complete framework for operations
on integers and floating-point numbers. No rationals, no complex numbers,
no gcd or lcm.

__Number__ class

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
    if {$value == 0} then {return #t} else {return #f}
  }
  method positive? {} {
    if {$value > 0} then {return #t} else {return #f}
  }
  method negative? {} {
    if {$value < 0} then {return #t} else {return #f}
  }
  method even? {} {
    if {$value % 2 == 0} then {return #t} else {return #f}
  }
  method odd? {} {
    if {$value % 2 == 1} then {return #t} else {return #f}
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

interp alias {} ::constcl::MkNumber {} ::constcl::Number new
```

__number?__

`number?` recognizes a number by object type, not by content.

<table border=1><thead><tr><th colspan=2 align="left">number? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg number?

proc ::constcl::number? {val} {
  if {[info object isa typeof $val ::constcl::Number]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Number]} {
    return #t
  } else {
    return #f
  }
}
```


__=__

__<__

__>__

__<=__

__>=__

The predicates `=`, `<`, `>`, `<=`, and `>=` are implemented.


<table border=1><thead><tr><th colspan=2 align="left">=, &lt;, &gt;, &lt;=, &gt;= (public)</th></tr></thead><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg = ::constcl::=

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
reg < ::constcl::<

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
reg > ::constcl::>

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
reg <= ::constcl::<=

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
reg >= ::constcl::>=

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


__zero?__

The `zero?` predicate tests if a given number is equal to zero.

<table border=1><thead><tr><th colspan=2 align="left">zero? (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg zero? ::constcl::zero?

proc ::constcl::zero? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num zero?]
}
```


__positive?__

__negative?__

__even?__

__odd?__

The `positive?`/`negative?`/`even?`/`odd?` predicates test a number
for those traits.

<table border=1><thead><tr><th colspan=2 align="left">positive?, negative?, even?, odd? (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg positive? ::constcl::positive?

proc ::constcl::positive? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num positive?]
}
```


```
reg negative? ::constcl::negative?

proc ::constcl::negative? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num negative?]
}
```


```
reg even? ::constcl::even?

proc ::constcl::even? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num even?]
}
```


```
reg odd? ::constcl::odd?

proc ::constcl::odd? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num odd?]
}
```


__max__

__min__

The `max` function selects the largest number, and the `min` function
selects the smallest number.

<table border=1><thead><tr><th colspan=2 align="left">max, min (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(max 7 1 10 3)   =>  10
(min 7 1 10 3)   =>  1
```

```
reg max ::constcl::max

proc ::constcl::max {num args} {
  try {
    set vals [lmap arg [::list $num {*}$args] {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(max num...)"
  }
  MkNumber [::tcl::mathfunc::max {*}$vals]
}
```


```
reg min ::constcl::min

proc ::constcl::min {num args} {
  try {
    set vals [lmap arg [::list $num {*}$args] {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(min num...)"
  }
  MkNumber [::tcl::mathfunc::min {*}$vals]
}
```


__+__

__*__

__-__

__/__

The operators `+`, `*`, `-`, and `/` stand for the respective
mathematical operations. They take a number of operands, but
at least one for `-` and `/`.

<table border=1><thead><tr><th colspan=2 align="left">+, * (public)</th></tr></thead><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

<table border=1><thead><tr><th colspan=2 align="left">-, / (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>args</td><td>some numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(list [+ 2 2] [* 2 2] [- 10 6] [/ 20 5])   =>  (4 4 4 4)
(+ 21 7 3)                                 =>  31
(* 21 7 3)                                 =>  441
(- 21 7 3)                                 =>  11
(/ 21 7 3)                                 =>  1
(- 5)                                      =>  -5
(/ 5)                                      =>  0.2
```

```
reg + ::constcl::+

proc ::constcl::+ {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(+ num ...)"
  }
  MkNumber [::tcl::mathop::+ {*}$vals]
}
```


```
reg * ::constcl::*

proc ::constcl::* {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(* num ...)"
  }
  MkNumber [::tcl::mathop::* {*}$vals]
}
```


```
reg - ::constcl::-

proc ::constcl::- {num args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(- num ...)"
  }
  MkNumber [::tcl::mathop::- [$num numval] {*}$vals]
}
```


```
reg / ::constcl::/

proc ::constcl::/ {num args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(/ num ...)"
  }
  MkNumber [::tcl::mathop::/ [$num numval] {*}$vals]
}
```


__abs__

The `abs` function yields the absolute value of a number.

<table border=1><thead><tr><th colspan=2 align="left">abs (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg abs ::constcl::abs

proc ::constcl::abs {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    return [MkNumber [expr {[$num numval] * -1}]]
  } else {
    return $num
  }
}
```


__quotient__

`quotient` calculates the quotient between two numbers.

<table border=1><thead><tr><th colspan=2 align="left">quotient (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(quotient 7 3)   =>  2.0
```

```
reg quotient

proc ::constcl::quotient {num1 num2} {
  set q [::tcl::mathop::/ [$num1 numval] [$num2 numval]]
  if {$q > 0} {
    return [MkNumber [::tcl::mathfunc::floor $q]]
  } elseif {$q < 0} {
    return [MkNumber [::tcl::mathfunc::ceil $q]]
  } else {
    return #0
  }
}
```

__remainder__

`remainder` is a variant of the modulus function. (I'm a programmer, not
a mathematician!)

<table border=1><thead><tr><th colspan=2 align="left">remainder (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(remainder 7 3)   =>  1
```

```
reg remainder

proc ::constcl::remainder {num1 num2} {
  set n [::tcl::mathop::% [[abs $num1] numval] [[abs $num2] numval]]
  if {[$num1 negative?] ne "#f"} {
    set n -$n
  }
  return [MkNumber $n]
}
```

__modulo__

<table border=1><thead><tr><th colspan=2 align="left">modulo (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(modulo 7 3)   =>  1
```

```
reg modulo

proc ::constcl::modulo {num1 num2} {
  return [MkNumber [::tcl::mathop::% [$num1 numval] [$num2 numval]]]
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

__floor__

__ceiling__

__truncate__

__round__

`floor`, `ceiling`, `truncate`, and `round` are different methods for
converting a real number to an integer.

<table border=1><thead><tr><th colspan=2 align="left">floor, ceiling, truncate, round (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(floor 7.5)      =>  7.0
(ceiling 7.5)    =>  8.0
(truncate 7.5)   =>  7.0
(round 7.5)      =>  8
```

```
reg floor ::constcl::floor

proc ::constcl::floor {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::floor [$num numval]]
}
```


```
reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::ceil [$num numval]]
}
```


```
reg truncate ::constcl::truncate

proc ::constcl::truncate {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    MkNumber [::tcl::mathfunc::ceil [$num numval]]
  } else {
    MkNumber [::tcl::mathfunc::floor [$num numval]]
  }
}
```


```
reg round ::constcl::round

proc ::constcl::round {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::round [$num numval]]
}
```


```
proc ::constcl::rationalize {x y} {
    # TODO
}
```

__exp__

__log__

__sin__

__cos__

__tan__

__asin__

__acos__

__atan__

The mathematical functions _e<sup>x</sup>_, natural logarithm,
sine, cosine, tangent, arcsine, arccosine, and arctangent are
calculated by `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`,
and `atan`, respectively.

<table border=1><thead><tr><th colspan=2 align="left">exp, log, sin, cos, tan, asin, acos, atan (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

<table border=1><thead><tr><th colspan=2 align="left">(binary) atan (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(let ((x (log 2))) (= 2 (exp x)))                         =>  #t
(let ((a (/ pi 3))) (let ((s (sin a))) (= a (asin s))))   =>  #t
```

```
reg exp ::constcl::exp

proc ::constcl::exp {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::exp [$num numval]]
}
```


```
reg log ::constcl::log

proc ::constcl::log {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::log [$num numval]]
}
```


```
reg sin ::constcl::sin

proc ::constcl::sin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::sin [$num numval]]
}
```

```
reg cos ::constcl::cos

proc ::constcl::cos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::cos [$num numval]]
}
```

```
reg tan ::constcl::tan

proc ::constcl::tan {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::tan [$num numval]]
}
```


```
reg asin ::constcl::asin

proc ::constcl::asin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::asin [$num numval]]
}
```

```
reg acos ::constcl::acos

proc ::constcl::acos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::acos [$num numval]]
}
```

```
reg atan ::constcl::atan

proc ::constcl::atan {args} {
  if {[llength $args] == 1} {
    set num [lindex $args 0]
    check {number? $num} {
        NUMBER expected\n([pn] [$num show])
    }
    MkNumber [::tcl::mathfunc::atan [$num numval]]
  } else {
    lassign $args num1 num2
    check {number? $num1} {
        NUMBER expected\n([pn] [$num1 show])
    }
    check {number? $num2} {
        NUMBER expected\n([pn] [$num2 show])
    }
    MkNumber [::tcl::mathfunc::atan2 [$num1 numval] [$num2 numval]]
  }
}
```


__sqrt__

`sqrt` calculates the square root.

<table border=1><thead><tr><th colspan=2 align="left">sqrt (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::sqrt [$num numval]]
}
```


__expt__

`expt` calculates the _x_ to the power of _y_, or _x<sup>y</sup>_.

<table border=1><thead><tr><th colspan=2 align="left">expt (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg expt ::constcl::expt

proc ::constcl::expt {num1 num2} {
  check {number? $num1} {
      NUMBER expected\n([pn] [$num1 show] [$num2 show])
  }
  check {number? $num2} {
      NUMBER expected\n([pn] [$num1 show] [$num2 show])
  }
  MkNumber [::tcl::mathfunc::pow [$num1 numval] [$num2 numval]]
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

__number->string__

The procedures `number->string` and `string->number` convert between
number and string with optional radix conversion.

<table border=1><thead><tr><th colspan=2 align="left">number-&gt;string (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>?radix?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(number->string 23)      =>  "23"
(number->string 23 2)    =>  "10111"
(number->string 23 8)    =>  "27"
(number->string 23 16)   =>  "17"
```

```
reg number->string ::constcl::number->string

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
      NUMBER expected\n([pn] [$num show] [$radix show])
    }
    set radices [list [MkNumber 2] [MkNumber 8] [MkNumber 10] [MkNumber 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] [$num show] [$radix show])
    }
    if {[$radix numval] == 10} {
      return [MkString [$num numval]]
    } else {
      return [MkString [base [$radix numval] [$num numval]]]
    }
  }
}

 # due to Richard Suchenwirth,
 # <URL: https://wiki.tcl-lang.org/page/Based+numbers>
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


__string->number__

As with `number->string`, above.

<table border=1><thead><tr><th colspan=2 align="left">string-&gt;number (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>?radix?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(string->number "23")        =>  23
(string->number "10111" 2)   =>  23
(string->number "27" 8)      =>  23
(string->number "17" 16)     =>  23
```

```
reg string->number ::constcl::string->number

proc ::constcl::string->number {str args} {
  if {[llength $args] == 0} {
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    return [MkNumber [$str value]]
  } else {
    lassign $args radix
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    set radices [list [MkNumber 2] [MkNumber 8] [MkNumber 10] [MkNumber 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] [$str show] [$radix show])
    }
    if {[$radix numval] == 10} {
      return [MkNumber [$str value]]
    } else {
      return [MkNumber [frombase [$radix numval] [$str value]]]
    }
  }
}

 # due to Richard Suchenwirth,
 # <URL: https://wiki.tcl-lang.org/page/Based+numbers>
proc frombase {base number} {
  set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
  set negative [regexp ^-(.+) $number -> number]
  set res 0
  foreach digit [split $number {}] {
    set decimalvalue [lsearch $digits $digit]
    if {$decimalvalue < 0 || $decimalvalue >= $base} {
      ::error "bad digit $decimalvalue for base $base"
    }
    set res [expr {$res * $base + $decimalvalue}]
  }
  if $negative {set res -$res}
  set res
}
```



### Booleans

Booleans are logic values, either true (`#t`) or false (`#f`).
All predicates (procedures whose name end with -?) return
boolean values. The conditional `if` operator considers all
values except for `#f` to be true.

__Boolean__ class

```
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
  foreach instance [info class instances ::constcl::Boolean] {
    if {[$instance bvalue] eq $v} {
      return $instance
    }
  }
  return [::constcl::Boolean new $v]
}
```


__boolean?__

The `boolean?` predicate recognizes a Boolean by type.

<table border=1><thead><tr><th colspan=2 align="left">boolean? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
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
```


__not__

The only operation on booleans: `not`, or logical negation.

<table border=1><thead><tr><th colspan=2 align="left">not (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

Example:

```
(not #f)    ⇒  #t   ; the only argument that returns #t, all others return #f
(not nil)   ⇒  #f   ; see?
```

```
reg not ::constcl::not

proc ::constcl::not {val} {
  if {[$val bvalue] eq "#f"} {
    return #t
  } else {
    return #f
  }
}
```



### Characters

Characters are any Unicode printing character, and also space and newline space characters.

__Char__ class

```
oo::class create ::constcl::Char {
  superclass ::constcl::NIL
  variable value
  constructor {v} {
    if {[regexp {^#\\([[:graph:]]|space|newline)$} $v]} {
      set value $v
    } else {
      if {$v eq "#\\ "} {
        set value #\\space
      } elseif {$v eq "#\\\n"} {
        set value #\\newline
      } else {
        ::error "CHAR expected\n$v"
      }
    }
  }
  method char {} {
    switch $value {
      "#\\space" {
        return " "
      }
      "#\\newline" {
        return "\n"
      }
      default {
        return [::string index [my value] 2]
      }
    }
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
  method write {handle} {
    puts -nonewline $handle $value
  }
  method display {handle} {
    puts -nonewline $handle [my char]
  }
  method show {} {
    set value
  }
}

proc ::constcl::MkChar {v} {
  if {[regexp -nocase {^#\\(space|newline)$} $v]} {
    set v [::string tolower $v]
  }
  foreach instance [info class instances ::constcl::Char] {
    if {[$instance value] eq $v} {
      return $instance
    }
  }
  return [::constcl::Char new $v]
}
```

__char?__

`char?` recognizes Char values by type.

<table border=1><thead><tr><th colspan=2 align="left">char? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char? ::constcl::char?

proc ::constcl::char? {val} {
  if {[info object isa typeof $val ::constcl::Char]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Char]} {
    return #t
  } else {
    return #f
  }
}
```


__char=?__

__char<?__

__char>?__

__char<=?__

__char>=?__

`char=?`, `char<?`, `char>?`, `char<=?`, and `char>=?` compare character
values. They only compare two characters at a time.

<table border=1><thead><tr><th colspan=2 align="left">char=?, char&lt;?, char&gt;?, char&lt;=?, char&gt;=? (public)</th></tr></thead><tr><td>char1</td><td>a character</td></tr><tr><td>char2</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char=? ::constcl::char=?

proc ::constcl::char=? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {$char1 eq $char2} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char<? ::constcl::char<?

proc ::constcl::char<? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[$char1 char] < [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char>? ::constcl::char>?

proc ::constcl::char>? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[$char1 char] > [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char<=? ::constcl::char<=?

proc ::constcl::char<=? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[$char1 char] <= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char>=? ::constcl::char>=?

proc ::constcl::char>=? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[$char1 char] >= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
```


__char-ci=?__

__char-ci<?__

__char-ci>?__

__char-ci<=?__

__char-ci>=?__

`char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, and `char-ci>=?` compare character
values in a case insensitive manner. They only compare two characters at a time.

<table border=1><thead><tr><th colspan=2 align="left">char-ci=?, char-ci&lt;?, char-ci&gt;?, char-ci&lt;=?, char-ci&gt;=? (public)</th></tr></thead><tr><td>char1</td><td>a character</td></tr><tr><td>char2</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char-ci=? ::constcl::char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[::string tolower [$char1 char]] eq [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char-ci<? ::constcl::char-ci<?

proc ::constcl::char-ci<? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[::string tolower [$char1 char]] < [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char-ci>? ::constcl::char-ci>?

proc ::constcl::char-ci>? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[::string tolower [$char1 char]] > [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char-ci<=? ::constcl::char-ci<=?

proc ::constcl::char-ci<=? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[::string tolower [$char1 char]] <= [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg char-ci>=? ::constcl::char-ci>=?

proc ::constcl::char-ci>=? {char1 char2} {
  check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
  if {[::string tolower [$char1 char]] >= [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
```


__char-alphabetic__

__char-numeric__

__char-whitespace__

__char-upper-case__

__char-lower-case__

The predicates `char-alphabetic`, `char-numeric`, `char-whitespace`,
`char-upper-case`, and `char-lower-case` test a character for these
conditions.

<table border=1><thead><tr><th colspan=2 align="left">char-alphabetic?, char-numeric?, char-whitespace?, char-upper-case?, char-lower-case? (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char-alphabetic? ::constcl::char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
  check {char? $char} {CHAR expected\n([pn] [$char show])}
  return [$char alphabetic?]
}
```


```
reg char-numeric? ::constcl::char-numeric?

proc ::constcl::char-numeric? {char} {
  check {char? $char} {CHAR expected\n([pn] [$char show])}
  return [$char numeric?]
}
```


```
reg char-whitespace? ::constcl::char-whitespace?

proc ::constcl::char-whitespace? {char} {
  check {char? $char} {CHAR expected\n([pn] [$char show])}
  return [$char whitespace?]
}
```


```
reg char-upper-case? ::constcl::char-upper-case?

proc ::constcl::char-upper-case? {char} {
  check {char? $char} {CHAR expected\n([pn] [$char show])}
  return [$char upper-case?]
}
```


```
reg char-lower-case? ::constcl::char-lower-case?

proc ::constcl::char-lower-case? {char} {
  check {char? $char} {CHAR expected\n([pn] [$char show])}
  return [$char lower-case?]
}
```


__char->integer__

__integer->char__

`char->integer` and `integer->char` convert between characters and their
16-bit numeric codes.

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


__char-upcase__

__char-downcase__

`char-upcase` and `char-downcase` alter the case of a character.

<table border=1><thead><tr><th colspan=2 align="left">char-upcase, char-downcase (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a character</td></tr></table>

```
reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
  check {char? $char} {CHAR expected\n([pn] [$char show])}
  if {[::string is alpha -strict [$char char]]} {
    return [MkChar [::string toupper [$char value]]]
  } else {
    return $char
  }
}
```



```
reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
  check {char? $char} {CHAR expected\n([pn] [$char show])}
  if {[::string is alpha -strict [$char char]]} {
    return [MkChar [::string tolower [$char value]]]
  } else {
    return $char
  }
}
```



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

```
catch { ::constcl::Procedure destroy }

oo::class create ::constcl::Procedure {
  superclass ::constcl::NIL
  variable parms body env
  constructor {p b e} {
    set parms $p         ;# a Lisp list|improper list|symbol denoting parameter names
    set body $b          ;# a Lisp list of expressions under 'begin, or a single expression
    set env $e           ;# the closed over environment
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

interp alias {} ::constcl::MkProcedure {} ::constcl::Procedure new
```

__procedure?__

<table border=1><thead><tr><th colspan=2 align="left">procedure? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {val} {
  if {[info object isa typeof $val ::constcl::Procedure]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Procedure]} {
    return #t
  } elseif {[::string match "::constcl::*" $val]} {
    return #t
  } else {
    return #f
  }
}
```


__apply__

`apply` applies a procedure to a Lisp list of Lisp arguments.

<table border=1><thead><tr><th colspan=2 align="left">apply (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>what pr returns</td></tr></table>

Example:

```
(apply + (list 2 3))   ⇒  5
```

```
reg apply ::constcl::apply

proc ::constcl::apply {pr vals} {
  check {procedure? $pr} {PROCEDURE expected\n([pn] [$pr show] ...)}
  invoke $pr $vals
}
```


__map__

`map` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The Lisp list of the results of the invocations is 
returned.

<table border=1><thead><tr><th colspan=2 align="left">map (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>args</td><td>some lists</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

Example:

```
(map + '(1 2 3) '(5 6 7))   ⇒ (6 8 10)
```

```
reg map ::constcl::map

proc ::constcl::map {pr args} {
  check {procedure? $pr} {PROCEDURE expected\n([pn] [$pr show] ...)}
  set arglists $args
  for {set i 0} {$i < [llength $arglists]} {incr i} {
    lset arglists $i [splitlist [lindex $arglists $i]]
  }
  set res {}
  for {set item 0} {$item < [llength [lindex $arglists 0]]} {incr item} {
    set arguments {}
    for {set arg 0} {$arg < [llength $arglists]} {incr arg} {
      lappend arguments [lindex $arglists $arg $item]
    }
    lappend res [invoke $pr [list {*}$arguments]]
  }
  return [list {*}$res]
}
```


__for-each__

`for-each` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The empty list is returned.

<table border=1><thead><tr><th colspan=2 align="left">for-each (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>args</td><td>some lists</td></tr><tr><td><i>Returns:</i></td><td>the empty list</td></tr></table>

Example: (from R5RS; must be pasted as a oneliner for the ConsTcl repl to stomach
it.)

```
(let ((v (make-vector 5)))
  (for-each (lambda (i)
              (vector-set! v i (* i i)))
            '(0 1 2 3 4))
  v)                                      ⇒  #(0 1 4 9 16)
```

```
reg for-each ::constcl::for-each

proc ::constcl::for-each {proc args} {
  check {procedure? $proc} {PROCEDURE expected\n([pn] [$proc show] ...)}
  set arglists $args
  for {set i 0} {$i < [llength $arglists]} {incr i} {
    lset arglists $i [splitlist [lindex $arglists $i]]
  }
  for {set item 0} {$item < [llength [lindex $arglists 0]]} {incr item} {
    set arguments {}
    for {set arg 0} {$arg < [llength $arglists]} {incr arg} {
      lappend arguments [lindex $arglists $arg $item]
    }
    invoke $proc [list {*}$arguments]
  }
  return [list]
}
```


```
proc ::constcl::force {promise} {
    # TODO
}
```

```
proc ::constcl::call-with-current-continuation {proc} {
    # TODO
}
```

```
proc ::constcl::values {args} {
    # TODO
}
```

```
proc ::constcl::call-with-values {producer consumer} {
    # TODO
}
```

```
proc ::constcl::dynamic-wind {before thunk after} {
    # TODO
}
```


### Input and output


```
catch { ::constcl::Port destroy }

oo::class create ::constcl::Port {
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
  }
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<port-$num>"
  }
  method display {handle} {
    my write $handle
  }
}

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
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<input-port-$num>"
  }
  method display {handle} {
    my write $handle
  }
}

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
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<output-port-$num>"
  }
  method display {handle} {
    my write $handle
  }
}

interp alias {} ::constcl::MkInputPort {} ::constcl::InputPort new
interp alias {} ::constcl::MkOutputPort {} ::constcl::OutputPort new

set ::constcl::Input_port [::constcl::MkInputPort stdin]
set ::constcl::Output_port [::constcl::MkOutputPort stdout]

reg port?

proc ::constcl::port? {val} {
  if {[info object isa typeof $val ::constcl::Port]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Port]} {
    return #t
  } else {
    return #f
  }
}
```

```
proc ::constcl::call-with-input-file {string proc} {
    # TODO
}
```

```
proc ::constcl::call-with-output-file {string proc} {
    # TODO
}
```

```
reg input-port?

proc ::constcl::input-port? {obj} {
  if {[info object isa typeof $val ::constcl::InputPort]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::InputPort]} {
    return #t
  } else {
    return #f
  }
}
```

```
reg output-port?

proc ::constcl::output-port? {obj} {
  if {[info object isa typeof $val ::constcl::OutputPort]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::OutputPort]} {
    return #t
  } else {
    return #f
  }
}
```

```
reg current-input-port

proc ::constcl::current-input-port {} {
  return $::constcl::Input_port
}
```

```
reg current-output-port

proc ::constcl::current-output-port {} {
  return $::constcl::Output_port
}
```

```
reg with-input-from-file

proc ::constcl::with-input-from-file {string thunk} {
  set newport [open-input-file $string]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Input_port
    set ::constcl::Input_port $newport
    eval $thunk
    set ::constcl::Input_port $oldport
    close-input-port $newport
  }
}
```


```
reg with-output-to-file

proc ::constcl::with-output-to-file {string thunk} {
  set newport [open-output-file $string]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Output_port
    set ::constcl::Output_port $newport
    eval $thunk
    set ::constcl::Output_port $oldport
    close-input-port $newport
  }
}
```

```
reg open-input-file

proc ::constcl::open-input-file {filename} {
  set p [MkInputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-input-file: could not open file $filename"
  }
  return $p
}
```

```
reg open-output-file

proc ::constcl::open-output-file {filename} {
  if {[file exists $filename]} {
    error "open-output-file: file already exists $filename"
  }
  set p [MkOutputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-output-file: could not open file $filename"
  }
  return $p
}
```

```
reg close-input-port

proc ::constcl::close-input-port {port} {
  if {[$port handle] eq "stdin"} {
    error "don't close the standard input port"
  }
  $port close
}
```

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

`write` is implemented in the write[#](https://github.com/hoodiecrow/ConsTcl#write) section.

`display` is implemented in the write section.

`newline` outputs a newline character. Especially helpful when using `display`
for output, since it doesn't end lines with newline.

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

`load` reads a Lisp source file and evals the expressions in it in the global
environment. The procedure is a ConsTcl mix of Scheme calls and Tcl syntax.

<table border=1><thead><tr><th colspan=2 align="left">load (public)</th></tr></thead><tr><td>filename</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg load

proc ::constcl::load {filename} {
  set p [open-input-file $filename]
  set n [read $p]
  while {$n ne "#EOF"} {
    eval $n
    set n [read $p]
  }
  close-input-port $p
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


interp alias {} ::constcl::MkPair {} ::constcl::Pair new
```


__pair?__

<table border=1><thead><tr><th colspan=2 align="left">pair? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg pair?

proc ::constcl::pair? {val} {
  if {[info object isa typeof $val ::constcl::Pair]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Pair]} {
    return #t
  } else {
    return #f
  }
}
```


__show-pair__

Helper procedure to make a string representation of a list.

<table border=1><thead><tr><th colspan=2 align="left">show-pair (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Tcl string</td></tr></table>

```
proc ::constcl::show-pair {pair} {
  # take an object and print the car and the cdr of the stored value
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


__cons__

`cons` joins two values in a pair; useful in many operations such as pushing
a new value onto a list.

<table border=1><thead><tr><th colspan=2 align="left">cons (public)</th></tr></thead><tr><td>car</td><td>a Lisp value</td></tr><tr><td>cdr</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a pair</td></tr></table>

Example:

```
(cons 'a 'b)              =>  (a . b)
(cons 'a nil)             =>  (a)
(cons 'a (cons 'b nil))   =>  (a b)
```

![a small schematic to make it clearer](/images/consing.png)


```
reg cons ::constcl::cons

proc ::constcl::cons {car cdr} {
  MkPair $car $cdr
}
```


__car__

`car` gets the contents of the first cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">car (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

Example:

```
(car '(a b))   =>  a
```

```
reg car ::constcl::car

proc ::constcl::car {pair} {
  $pair car
}
```


__cdr__

`cdr` gets the contents of the second cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">cdr (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

Example:

```
(cdr '(a b))   =>  (b)
```

```
reg cdr ::constcl::cdr

proc ::constcl::cdr {pair} {
  $pair cdr
}
```


__caar__ to __cddddr__

`car` and `cdr` can be combined to form 28 composite access
operations.

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

__set-car!__

`set-car!` sets the contents of the first cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">set-car! (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a pair</td></tr></table>

Example:

```
(let ((pair (cons 'a 'b)) (val 'x)) (set-car! pair val))   =>  (x . b)
```

```
reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {pair val} {
  $pair set-car! $val
}
```


__set-cdr!__

`set-cdr!` sets the contents of the second cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">set-cdr! (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a pair</td></tr></table>

Example:

```
(let ((pair (cons 'a 'b)) (val 'x)) (set-cdr! pair val))   =>  (a . x)
```

```
reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {pair val} {
  $pair set-cdr! $val
}
```


__list?__

The `list?` predicate tests if a pair is part of a proper list, one that
ends with NIL.

<table border=1><thead><tr><th colspan=2 align="left">list? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg list? ::constcl::list?

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


__list__

`list` constructs a Lisp list from a number of values.

<table border=1><thead><tr><th colspan=2 align="left">list (public)</th></tr></thead><tr><td>args</td><td>some Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

Example:

```
(list 1 2 3)   =>  (1 2 3)
```

```
reg list ::constcl::list

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


__length__

`length` reports the length of a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">length (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(length '(a b c d))   =>  4
```

```
reg length ::constcl::length

proc ::constcl::length {pair} {
  check {list? $pair} {
    LIST expected\n([pn] lst)
  }
  MkNumber [length-helper $pair]
}
```

<table border=1><thead><tr><th colspan=2 align="left">length-helper (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Tcl number</td></tr></table>

```
proc ::constcl::length-helper {pair} {
  if {[null? $pair] ne "#f"} {
    return 0
  } else {
    return [expr {1 + [length-helper [cdr $pair]]}]
  }
}
```


__append__

`append` joins lists together.

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


__reverse__

`reverse` produces a reversed copy of a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">reverse (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

Example:

```
(reverse '(a b c))   =>  (c b a)
```

```
reg reverse ::constcl::reverse

proc ::constcl::reverse {vals} {
  list {*}[lreverse [splitlist $vals]]
}
```


__list-tail__

Given a list index, `list-tail` yields the sublist starting from that index.

<table border=1><thead><tr><th colspan=2 align="left">list-tail (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

Example:

```
(let ((lst '(a b c d e f)) (k 3)) (list-tail lst k))   =>  (d e f)
```

```
reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {vals k} {
  if {[zero? $k] ne "#f"} {
    return $vals
  } else {
    list-tail [cdr $vals] [- $k #1]
  }
}
```


__list-ref__

`list-ref` yields the list item at a given index.

<table border=1><thead><tr><th colspan=2 align="left">list-ref (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

Example:

```
(let ((lst '(a b c d e f)) (k 3)) (list-ref lst k))   =>  d
```

```
reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {vals k} {
  car [list-tail $vals $k]
}
```


__memq__

__memv__

__member__

`memq`, `memv`, and `member` return the sublist starting with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison.

<table border=1><thead><tr><th colspan=2 align="left">memq (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

Example:

```
(let ((lst '(a b c d e f)) (val 'd)) (memq val lst))   =>  (d e f)
```

```
reg memq ::constcl::memq

proc ::constcl::memq {val1 val2} {
  return [member-proc eq? $val1 $val2]
}
```


<table border=1><thead><tr><th colspan=2 align="left">memv (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg memv ::constcl::memv

proc ::constcl::memv {val1 val2} {
  return [member-proc eqv? $val1 $val2]
}
```

<table border=1><thead><tr><th colspan=2 align="left">member (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg member ::constcl::member

proc ::constcl::member {val1 val2} {
  return [member-proc equal? $val1 $val2]
}
```

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

__assq__

__assv__

__assoc__

`assq`, `assv`, and `assoc` return the associative item marked with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison. They implement lookup in the form of lookup
table known as an association list, or _alist_.

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
    if {[pair? [car $val2]] ne "#f" && [$epred $val1 [caar $val2]] ne "#f"} {
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

```
oo::class create ::constcl::String {
  superclass ::constcl::NIL
  variable data constant
  constructor {v} {
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
      lset ::constcl::vectorSpace $idx [::constcl::MkChar $c]
      incr idx
    }
    set data [
      ::constcl::cons [
        ::constcl::MkNumber $vsa] [::constcl::MkNumber $len]]
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
    set end [expr {[[my length] numval] + $base - 1}]
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
      if {$k < 0 || $k >= [[my length] numval]} {
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
      for {set idx $base} {$idx < $len+$base} {incr idx} {
        lset ::constcl::vectorSpace $idx $c
      }
    }
    return [self]
  }
  method substring {from to} {
    join [lmap c [lrange [my store] [$from numval] [$to numval]] {$c char}] {}
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method write {handle} {
    puts -nonewline $handle "\"[my value]\""
  }
  method display {handle} {
    puts -nonewline $handle [my value]
  }
  method show {} {
    format "\"[my value]\""
  }
}

interp alias {} MkString {} ::constcl::String new
```

<table border=1><thead><tr><th colspan=2 align="left">string? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg string? ::constcl::string?

proc ::constcl::string? {val} {
  if {[info object isa typeof $val ::constcl::String]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::String]} {
    return #t
  } else {
    return #f
  }
}
```


__make-string__

`make-string` creates a string of _k_ characters, optionally filled with _char_
characters. If _char_ is omitted, the string will be filled with space characters.

<table border=1><thead><tr><th colspan=2 align="left">make-string (public)</th></tr></thead><tr><td>k</td><td>a number</td></tr><tr><td>?char?</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(let ((k 5)) (make-string k))                   =>  "     "
(let ((k 5) (char #\A)) (make-string k char))   =>  "AAAAA"
```

```
reg make-string ::constcl::make-string

proc ::constcl::make-string {k args} {
  if {[llength $args] == 0} {
    return [MkString [::string repeat " " [$k numval]]]
  } else {
    lassign $args char
    return [MkString [::string repeat [$char char] [$k numval]]]
  }
}
```


__string__

`string` constructs a string from a number of Lisp characters.

<table border=1><thead><tr><th colspan=2 align="left">string (public)</th></tr></thead><tr><td>args</td><td>some characters</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(string #\f #\o #\o)   =>  "foo"
```

```
reg string ::constcl::string

proc ::constcl::string {args} {
  set str {}
  foreach char $args {
    check {::constcl::char? $char} {
      CHAR expected\n([pn] [lmap c $args {$c show}])
    }
    ::append str [$char char]
  }
  return [MkString $str]
}
```


__string-length__

`string-length` reports a string's length.

<table border=1><thead><tr><th colspan=2 align="left">string-length (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(string-length "foobar")   => 6
```

```
reg string-length ::constcl::string-length

proc ::constcl::string-length {str} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkNumber [[$str length] numval]]
}
```


__string-ref__

`string-ref` yields the _k_-th character (0-based) in _str_.

<table border=1><thead><tr><th colspan=2 align="left">string-ref (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a character</td></tr></table>

Example:

```
(string-ref "foobar" 3)   => #\b
```

```
reg string-ref ::constcl::string-ref

proc ::constcl::string-ref {str k} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show] [$k show])
  }
  check {::constcl::number? $k} {
    Exact INTEGER expected\n([pn] [$str show] [$k show])
  }
  return [$str ref $k]
}
```


__string-set!__

`string-set!` replaces the character at _k_ with _char_ in a non-constant string.

<table border=1><thead><tr><th colspan=2 align="left">string-set! (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>k</td><td>a number</td></tr><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(let ((str (string #\f #\o #\o)) (k 2) (char #\x)) (string-set! str k char))   =>  "fox"
```

```
reg string-set!

proc ::constcl::string-set! {str k char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$k show] [$char show])
  }
  check {number? $k} {
    Exact INTEGER expected\n([pn] [$str show] [$k show] [$char show])
  }
  check {char? $char} {
    CHAR expected\n([pn] [$str show] [$k show] [$char show])
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

`string=?`, `string<?`, `string>?`, `string<=?`, `string>=?` and their
case insensitive variants `string-ci=?`, `string-ci<?`, `string-ci>?`,
`string-ci<=?`, `string-ci>=?` compare strings.

<table border=1><thead><tr><th colspan=2 align="left">string=?, string&lt;?, string&gt;?, string&lt;=?, string&gt;=? (public)</th></tr></thead><tr><td>str1</td><td>a string</td></tr><tr><td>str2</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

<table border=1><thead><tr><th colspan=2 align="left">string-ci=?, string-ci&lt;?, string-ci&gt;?, string-ci&lt;=?, string-ci&gt;=? (public)</th></tr></thead><tr><td>str1</td><td>a string</td></tr><tr><td>str2</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg string=? ::constcl::string=?

proc ::constcl::string=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] eq [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string-ci=? ::constcl::string-ci=?

proc ::constcl::string-ci=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] eq [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string<? ::constcl::string<?

proc ::constcl::string<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] < [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string-ci<? ::constcl::string-ci<?

proc ::constcl::string-ci<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] < [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string>? ::constcl::string>?

proc ::constcl::string>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] > [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string-ci>? ::constcl::string-ci>?

proc ::constcl::string-ci>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] > [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string<=? ::constcl::string<=?

proc ::constcl::string<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] <= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string-ci<=? ::constcl::string-ci<=?

proc ::constcl::string-ci<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] <= [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string>=? ::constcl::string>=?

proc ::constcl::string>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] >= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
```


```
reg string-ci>=? ::constcl::string-ci>=?

proc ::constcl::string-ci>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] >= [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
```


__substring__

`substring` yields the substring of _str_ that starts at _start_ and ends at _end_.

<table border=1><thead><tr><th colspan=2 align="left">substring (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>start</td><td>a number</td></tr><tr><td>end</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(substring "foobar" 2 4)   => "oba"
```

```
reg substring ::constcl::substring

proc ::constcl::substring {str start end} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$start show] [$end show])
  }
  check {number? $start} {
    NUMBER expected\n([pn] [$str show] [$start show] [$end show])
  }
  check {number? $end} {
    NUMBER expected\n([pn] [$str show] [$start show] [$end show])
  }
  return [MkString [$str substring $start $end]]
}
```


__string-append__

`string-append` joins strings together.

<table border=1><thead><tr><th colspan=2 align="left">string-append (public)</th></tr></thead><tr><td>args</td><td>some strings</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(string-append "foo" "bar")   =>  "foobar"
```

```
reg string-append ::constcl::string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {$arg value}]]
}
```


__string->list__

`string->list` converts a string to a Lisp list of characters.

<table border=1><thead><tr><th colspan=2 align="left">string-&gt;list (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of characters</td></tr></table>

Example:

```
(string->list "foo")   =>  (#\f #\o #\o)
```

```
reg string->list ::constcl::string->list

proc ::constcl::string->list {str} {
  list {*}[$str store]
}
```


__list->string__

`list->string` converts a Lisp list of characters to a string.

<table border=1><thead><tr><th colspan=2 align="left">list-&gt;string (public)</th></tr></thead><tr><td>list</td><td>a Lisp list of characters</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(list->string '(#\1 #\2 #\3))   => "123"
```

```
reg list->string ::constcl::list->string

proc ::constcl::list->string {list} {
  MkString [::append --> {*}[lmap c [splitlist $list] {$c char}]]
}
```


__string-copy__

`string-copy` makes a copy of a string.

<table border=1><thead><tr><th colspan=2 align="left">string-copy (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(let ((str (string-copy "abc")) (k 0) (char #\x)) (string-set! str k char))            =>  "xbc"
```

```
reg string-copy ::constcl::string-copy

proc ::constcl::string-copy {str} {
  check {string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkString [$str value]]
}
```


__string-fill!__

`string-fill!` _str_ _char_ fills a non-constant string with _char_.

<table border=1><thead><tr><th colspan=2 align="left">string-fill! (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

Example:

```
(let ((str (string-copy "foobar")) (char #\X)) (string-fill! str char))   =>  "XXXXXX"
```

```
reg string-fill! ::constcl::string-fill!

proc ::constcl::string-fill! {str char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$char show])
  }
  $str fill! $char
  return $str
}
```



### Symbols

Symbols are like little strings that are used to refer to things (variables, including
procedure names, etc) or for comparing against each other.

**Symbol** class

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
  method name {} {set name}
  method value {} {set name}
  method = {symname} {expr {$name eq $symname}}
  method mkconstant {} {}
  method constant {} {return 1}
  method make-case-constant {} {set caseconstant 1}
  method case-constant {} {set caseconstant}
  method write {handle} { puts -nonewline $handle [my name] }
  method display {} { puts -nonewline [my name] }
  method show {} {set name}
}

proc ::constcl::MkSymbol {n} {
  foreach instance [info class instances ::constcl::Symbol] {
    if {[$instance name] eq $n} {
      return $instance
    }
  }
  return [::constcl::Symbol new $n]
}
interp alias {} S {} ::constcl::MkSymbol
```

<table border=1><thead><tr><th colspan=2 align="left">symbol? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {val} {
  if {[info object isa typeof $val ::constcl::Symbol]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Symbol]} {
    return #t
  } else {
    return #f
  }
}
```


**symbol->string**

`symbol->string` yields a string consisting of the symbol name, usually
lower-cased.

<table border=1><thead><tr><th colspan=2 align="left">symbol-&gt;string (public)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

```
reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {sym} {
  check {symbol? $sym} {
    SYMBOL expected\n([pn] [$sym show])
  }
  if {![$sym case-constant]} {
    set str [MkString [::string tolower [$sym name]]]
  } else {
    set str [MkString [$sym name]]
  }
  $str mkconstant
  return $str
}
```

Example:

```
(let ((sym 'Foobar)) (symbol->string sym))   =>  "foobar"
```


**string->symbol**

`string->symbol` creates a symbol with the name given by the string. The symbol
is 'case-constant', i.e. it will not be lower-cased.

<table border=1><thead><tr><th colspan=2 align="left">string-&gt;symbol (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td><i>Returns:</i></td><td>a symbol</td></tr></table>

Example:

```
(define sym (let ((str "Foobar")) (string->symbol str)))
sym                                                        =>  Foobar
(symbol->string sym)                                       =>  "Foobar"
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
They are implemented as Tcl lists of Lisp values.

The number of elements that a vector contains (the _length_) is set when the vector is created.
Elements can be indexed by integers from zero to length minus one.

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
    set data [::constcl::cons [::constcl::MkNumber $vsa] [::constcl::MkNumber $len]]
    set constant 0
  }
  method length {} {::constcl::cdr $data}
  method ref {k} {
    set k [$k numval]
    if {$k < 0 || $k >= [[my length] numval]} {
      ::error "index out of range\n$k"
    }
    lindex [my store] $k
  }
  method store {} {
    set base [[::constcl::car $data] numval]
    set end [expr {[[my length] numval] + $base - 1}]
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
      set base [[::constcl::car $data] numval]
      lset ::constcl::vectorSpace $k+$base $obj
    }
    return [self]
  }
  method fill! {val} {
    if {[my constant]} {
      ::error "vector is constant"
    } else {
      set base [[::constcl::car $data] numval]
      set len [[my length] numval]
      for {set idx $base} {$idx < $len+$base} {incr idx} {
        lset ::constcl::vectorSpace $idx $val
      }
    }
    return [self]
  }
  method mkconstant {} {set constant 1}
  method constant {} {set constant}
  method write {handle} { puts -nonewline $handle [my show]}
  method display {} {puts -nonewline [my show]}
  method show {} {format "#(%s)" [join [lmap val [my value] {$val show}] " "]}
}

interp alias {} ::constcl::MkVector {} ::constcl::Vector new
```

__vector?__

<table border=1><thead><tr><th colspan=2 align="left">vector? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg vector? ::constcl::vector?

proc ::constcl::vector? {val} {
  if {[info object isa typeof $val ::constcl::Vector]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Vector]} {
    return #t
  } else {
    return #f
  }
}
```


__make-vector__

`make-vector` creates a vector with a given length and optionally a fill value.
If a fill value isn't given, the empty list will be used.

<table border=1><thead><tr><th colspan=2 align="left">make-vector? (public)</th></tr></thead><tr><td>k</td><td>a number</td></tr><tr><td>?fill?</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>

Example:

```
(let ((k 5)) (make-vector k))                  =>  #(() () () () ())
(let ((k 5) (fill #\A)) (make-vector k fill))  =>  #(#\A #\A #\A #\A #\A)
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

Given a number of Lisp values, `vector` creates a vector containing them.

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

`vector-length` returns the length of a vector.

<table border=1><thead><tr><th colspan=2 align="left">vector-length (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

Example:

```
(vector-length '#(a b c))   =>  3
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

`vector-ref` returns the element of _vec_ at index _k_ (0-based).

<table border=1><thead><tr><th colspan=2 align="left">vector-ref (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

Example:

```
(let ((vec '#(a b c)) (k 1)) (vector-ref vec k))   =>  b
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

`vector-set!`, for a non-constant vector, sets the element at index _k_ to _val_.

<table border=1><thead><tr><th colspan=2 align="left">vector-set! (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td>k</td><td>a number</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>

Example:

```
(let ((vec '#(a b c)) (k 1) (val 'x)) (vector-set! vec k val))           =>  *error*
(let ((vec (vector 'a 'b 'c)) (k 1) (val 'x)) (vector-set! vec k val))   =>  #(a x c)
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

`vector->list` converts a vector value to a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">vector-&gt;list (public)</th></tr></thead><tr><td>vec</td><td>a vector</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

Example:

```
(vector->list '#(a b c))   =>  (a b c)
```

```
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
  list {*}[$vec value]
}
```


__list->vector__

`list->vector` converts a Lisp list value to a vector.

<table border=1><thead><tr><th colspan=2 align="left">list-&gt;vector (public)</th></tr></thead><tr><td>list</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>

Example:

```
(list->vector '(1 2 3))   =>  #(1 2 3)
```

```
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
  vector {*}[splitlist $list]
}
```


__vector-fill!__

`vector-fill!` fills a non-constant vector with a given value.

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
    VECTOR expected\n([pn] [$vec show] [$fill show])
  }
  $vec fill! $fill
}
```



## Identifier validation

__idcheckinit__

__idchecksubs__

__idcheck__

__varcheck__

Some routines for checking if a string is a valid identifier. `idcheckinit`
checks the first character, `idchecksubs` checks the rest. `idcheck` calls the
others and raises errors if they fail. A valid symbol is still an invalid
identifier if has the name of some keyword, which `varcheck` checks, for a set
of keywords given in the standard.


```
proc ::constcl::idcheckinit {init} {
  if {[::string is alpha -strict $init] || $init in {! $ % & * / : < = > ? ^ _ ~}} {
    return true
  } else {
    return false
  }
}

proc ::constcl::idchecksubs {subs} {
  foreach c [split $subs {}] {
    if {!([::string is alnum -strict $c] || $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
      return false
    }
  }
  return true
}

proc ::constcl::idcheck {sym} {
  if {$sym eq {}} {return $sym}
  if {(![idcheckinit [::string index $sym 0]] || ![idchecksubs [::string range $sym 1 end]]) && $sym ni {+ - ...}} {
    ::error "Identifier expected ($sym)"
  }
  set sym
}

proc ::constcl::varcheck {sym} {
  if {$sym in {else => define unquote unquote-splicing quote lambda if set! begin cond and or case let let* letrec do delay quasiquote}} {
    ::error "Macro name can't be used as a variable: $sym"
  }
  return $sym
}
```

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

Pre-make a set of constants (mostly symbols but also e.g. #NIL, #t, and #f)
and give them aliases for use in source text.

```
interp alias {} #NIL {} [::constcl::NIL new]

interp alias {} #t {} [::constcl::MkBoolean #t]

interp alias {} #f {} [::constcl::MkBoolean #f]

interp alias {} #-1 {} [::constcl::MkNumber -1]

interp alias {} #0 {} [::constcl::MkNumber 0]

interp alias {} #1 {} [::constcl::MkNumber 1]

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #NONE {} [::constcl::None new]

interp alias {} #UNSP {} [::constcl::Unspecific new]

interp alias {} #UNDF {} [::constcl::Undefined new]

interp alias {} #EOF {} [::constcl::EndOfFile new]

```

Initialize the definition register with the queen of numbers (or at least
a double floating point approximation).

```
dict set ::constcl::defreg pi [::constcl::MkNumber 3.1415926535897931]
```

In this interpreter, `nil` refers to the empty list.

```
reg nil #NIL
```




## The REPL

The REPL ([read-eval-print loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)) is a
loop that repeatedly _reads_ a Scheme source string from the user through the
command `::constcl::input` (breaking the loop if given an empty line) and
`::constcl::parse`, _evaluates_ it using `::constcl::eval`, and _prints_ using
`::constcl::write`.

__input__

`input` is modelled after the Python 3 function. It displays a prompt and reads a string.

```
proc ::constcl::input {prompt} {
  puts -nonewline $prompt
  flush stdout
  set buf [gets stdin]
  set openpars [regexp -all -inline {\(} $buf]
  set clsepars [regexp -all -inline {\)} $buf]
  set openbrak [regexp -all -inline {\[} $buf]
  set clsebrak [regexp -all -inline {\]} $buf]
  while {[llength $openpars] > [llength $clsepars] || [llength $openbrak] > [llength $clsebrak]} {
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

`repl` puts the loop in the read-eval-print loop. It repeats prompting for a
string until given a blank input. Given non-blank input, it parses and evaluates
the string, printing the resulting value.

```
proc ::repl {{prompt "ConsTcl> "}} {
  set str [::constcl::input $prompt]
  while {$str ne ""} {
    pep $str
    set str [::constcl::input $prompt]
  }
}
```


## Environment class and objects

The class for environments is called `Environment`. It is mostly a wrapper around a dictionary,
with the added finesse of keeping a link to the outer environment (starting a chain that goes all
the way to the global environment and then stops at the null environment) which can be traversed
by the find method to find which innermost environment a given symbol is bound in.

The long and complex constructor is to accommodate the variations of Scheme parameter lists, which 
can be empty, a proper list, a symbol, or a dotted list.

__Environment__ class

```
catch { ::constcl::Environment destroy }

oo::class create ::constcl::Environment {
  variable bindings outer_env
  constructor {syms vals {outer {}}} {
    set bindings [dict create]
    if {[::constcl::null? $syms] eq "#t"} {
      if {[llength $vals]} { error "too many arguments" }
    } elseif {[::constcl::list? $syms] eq "#t"} {
      set syms [::constcl::splitlist $syms]
      set symsn [llength $syms]
      set valsn [llength $vals]
      if {$symsn != $valsn} {
        error "wrong number of arguments, $valsn instead of $symsn"
      }
      foreach sym $syms val $vals {
        my set $sym $val
      }
    } elseif {[::constcl::symbol? $syms] eq "#t"} {
      my set $syms [::constcl::list {*}$vals]
    } else {
      while true {
        if {[llength $vals] < 1} { error "too few arguments" }
        my set [::constcl::car $syms] [lindex $vals 0]
        set vals [lrange $vals 1 end]
        if {[::constcl::symbol? [::constcl::cdr $syms]] eq "#t"} {
          my set [::constcl::cdr $syms] [::constcl::list {*}$vals]
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


On startup, two `Environment` objects called `null_env` (the null environment,
not the same as `null-environment` in Scheme) and `global_env` (the global
environment) are created. 

Make `null_env` empty and unresponsive: this is where searches for unbound
symbols end up.

```
::constcl::Environment create ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
  method find {sym} {self}
  method get {sym} {::error "Unbound variable: [$sym name]"}
  method set {sym val} {::error "Unbound variable: [$sym name]"}
}
```

Meanwhile, `global_env` is populated with all the definitions from the
definitions register, `defreg`. This is where top level evaluation happens.

```
namespace eval ::constcl {
  set keys [list {*}[lmap k [dict keys $defreg] {MkSymbol $k}]]
  set vals [dict values $defreg]
  Environment create global_env $keys $vals ::constcl::null_env
}
```

Load the Scheme base to add more definitions to the global environment.

```
pe {(load "schemebase.lsp")}
```

Thereafter, each time a user-defined procedure is called, a new `Environment`
object is created to hold the bindings introduced by the call, and also a link
to the outer environment (the one closed over when the procedure was defined).

#### Lexical scoping


Example:

```
ConsTcl> (define (circle-area r) (* pi (* r r)))
ConsTcl> (circle-area 10)
314.1592653589793
```

During a call to the procedure `circle-area`, the symbol `r` is bound to the
value 10. But we don't want the binding to go into the global environment,
possibly clobbering an earlier definition of `r`. The solution is to use
separate (but linked) environments, making `r`'s binding a
*local variable[#](https://en.wikipedia.org/wiki/Local_variable)*
in its own environment, which the procedure will be evaluated in. The symbols
`*` and `pi` will still be available through the local environment's link
to the outer global environment. This is all part of
**lexical scoping[#](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope)**.

In the first image, we see the global environment before we call `circle-area`
(and also the empty null environment which the global environment links to):

![A global environment](/images/env1.png)

During the call. Note how the global `r` is shadowed by the local one, and how
the local environment links to the global one to find `*` and `pi`. 

![A local environment shadows the global](/images/env2.png)

After the call, we are back to the first state again.

![A global environment](/images/env1.png)




if no {

## Lookup tables

Lisp languages have two simple variants of key/value lookup tables: property
lists (plists) and association lists (alists).

A property list is simply a list where every odd-numbered item (starting from 1)
is a key and every even-numbered item is a value. Example:

```
'(a 1 b 2 c 3 d 4 e 5)
```

Values can be retrieved in a two-step process:

```
> (define plist (list 'a 1 'b 2 'c 3 'd 4 'e 5))
> (define v '())
> (set! v (memq 'c plist))
(c 3 d 4 e 5)
> (set! v (cadr v))
3
```

If a key doesn't occur in the plist, `memq` returns `#f`.

Alternatively, ConsTcl users can use `get` to access the value in one step. 

```
> (get plist 'c)
3
```

`get` returns `#f` if the key isn't present in the plist.


Values can be added with a single statement:

```
> (set! plist (append '(f 6) plist))
(f 6 a 1 b 2 c 3 d 4 e 5)
```

or with the `put!` macro, which can both update existing values and add new ones:

```
> (put! plist 'c 9)
(f 6 a 1 b 2 c 9 d 4 e 5)
> (put! plist 'g 7)
(g 7 f 6 a 1 b 2 c 9 d 4 e 5)
```


To get rid of a key/value pair, the simplest way is to add a masking pair:

```
> (set! plist (append '(d #f) plist))
(d #f g 7 f 6 a 1 b 2 c 3 d 4 e 5)
```

But instead, one can use the `del!` macro:

```
> plist
(g 7 f 6 a 1 b 2 c 9 d 4 e 5)
> (del! plist 'd)
(g 7 f 6 a 1 b 2 c 9 e 5)
```


An alist is a list where the items are pairs, with the key as the `car` and the
value as the `cdr`. Example:

```
> (define alist (list (cons 'a 1) (cons 'b 2) (cons 'c 3) (cons 'd 4)))
> alist
((a . 1) (b . 2) (c . 3) (d . 4))
```

An alist can also be created from scratch using the `pairlis` procedure:

```
> (define alist (pairlis '(a b c) '(1 2 3)))
((a . 1) (b . 2) (c . 3))
```

The procedure `assq` retrieves one pair based on the key:

```
> (assq 'a alist)
(a . 1)
> (cdr (assq 'a alist))
1
> (assq 'x alist)
#f
```

As an alternative, the `get-alist` procedure fetches the value directly, or #f
for a missing item:

```
> (get-alist 'a)
1
> (get-alist 'x)
#f
```

We can add another item to the alist with the `push!` macro:

```
> (push! (cons 'e 5) alist)
((e . 5) (a . 1) (b . 2) (c . 3) (d . 4))
```

The `set-alist!` procedure can be used to update a value (it returns the alist
unchanged if the key isn't present):

```
> alist
((a . 1) (b . 2) (c . 3) (d . 4))
> (set-alist! alist 'b 7)
((a . 1) (b . 7) (c . 3) (d . 4))
```

}

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
        (let ((node-before (delete-seek lst (- idx 1)))
              (node-after (delete-seek lst (+ idx 2))))
          (set-cdr! node-before node-after))))
    lst))

(define (delete-seek lst idx)
  (if (zero? idx)
    lst
    (delete-seek (cdr lst) (- idx 1))))

(define (get-alist lst key)
  (let ((item (assq key lst)))
    (if item
      (cdr item)
      #f)))

(define (pairlis a b)
  (if (null? a)
    ()
    (cons (cons (car a) (car b)) (pairlis (cdr a) (cdr b)))))

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
