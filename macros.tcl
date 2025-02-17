
MD(
### Macros

__expand-macro__

Macros that rewrite expressions into other, more concrete expressions is one of
Lisp's strong points. This interpreter does macro expansion, but the user can't
define new macros--the ones available are hardcoded in the code below.

`expand-macro` takes an expression and an environment as a parameter. First, the
operator (`op`) and operands (`args`) are extracted to check if expansion is
necessary (the operator `car`, for historical reasons, stands for the first
element of a list, while `cdr` stands for the rest of the elements after the
first in a list). If the operator is the symbol `define` and the first of the
operands is something other than a Pair, then expansion is unnecessary and the
procedure returns with a code to break the while loop in `eval`.

The operator's symbol name is then used to select the right expansion procedure,
and the whole expression and the environment is passed to it. In the end, the
expanded expression is passed back to `eval`.
MD)

PR(
expand-macro (internal);expr expr env env -> expr
PR)

CB
proc ::constcl::expand-macro {expr env} {
  set op [car $expr]
  set args [cdr $expr]
  if {[$op name] eq "define" &&
      [pair? [car $args]] eq "#f"} {
    return -code break
  }
  return [expand-[$op name] $expr $env]
}
CB

MD(
__expand-and__

`expand-and` expands the `and` macro. It returns a `begin`-expression if the
macro has 0 or 1 elements, and a nested `if` construct otherwise. `S begin`
stands for "the symbol begin".
MD)

PR(
expand-and (internal);expr expr env env -> expr
PR)

CB
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
CB

MD(
##### Quasiquote: an aside

In this and many other macro expanders I use a quasiquote construct to lay out
how the is to be expanded. A quasiquote starts with a backquote (`&grave;`)
instead of the single quote that precedes regular quoted material. A quasiquote
allows for "unquoting" of selected parts: this is notated with a comma (`,`).
`&grave;(foo ,bar baz)` is very nearly the same as `('foo bar 'baz)`, except that
the list structure itself is constant in a quasiquote. Anyway, in both cases
`foo` and `baz` are constants while `bar` is a variable which will be evaluated.
Like in `do-and` here, a quasiquote serves well as a templating mechanism. The
variables in the quasiquote need to be a part of the environment in which the
quasiquote is expanded: I use `/define` to bind them in a temporary environment.
MD)

PR(
do-and (internal);tail exprtail prev expr env env -> expr
PR)

CB
proc ::constcl::do-and {tail prev env} {
  set env [Environment new #NIL {} $env]
  if {[[length $tail] numval] == 0} {
    return $prev
  } else {
    /define [S first] [car $tail] $env
    /define [S rest] [do-and [cdr $tail] \
        [car $tail] $env] $env
    set qq "`(if ,first ,rest #f)"
    return [expand-quasiquote [parse $qq] $env]
  }
}
CB

TT(
::tcltest::test macros-1.0 {expand and macro} -body {
    pxw "(and)"
    pxw "(and #t)"
    pxw "(and (> 3 2))"
    pxw "(and (> 3 2) (= 7 8))"
} -output "(begin #t)\n(begin #t)\n(begin (> 3 2))\n(if (> 3 2) (if (= 7 8) (= 7 8) #f) #f)\n"

::tcltest::test macros-1.1 {run and macro} -body {
    pew "(and)"
    pew "(and #t)"
    pew "(and (> 3 2))"
    pew "(and (> 3 2) (= 7 8))"
} -output "#t\n#t\n#t\n#f\n"

TT)

MD(
__expand-case__

The body of the `case` form consists of a key-expression and a number of
clauses. Each clause has a list of values and a body. If the key-expression
evaluates to a value that occurs in one of the value-lists (considered in
order), that clause's body is evaluated and all other clauses are ignored.

The `case` macro is expanded by `expand-case`. It expands to `'()` if there are
no clauses (left), and to nested `if` constructs if there are some.

##### caar, cadr, cdar, and the rest, an aside

The `do-case` procedure uses extensions of the `car`/`cdr` operators like `caar`
and `cdar`. `car`/`cdr` notation gets really powerful when combined to form
operators from `caar` to `cddddr`. One can read `caar L` as "the first element of
the first element of L", implying that the first element of `L` is a list. `cdar
L` is "the rest of the elements of the first element of L", and `cadr L` is
"the first element of the rest of the elements of L" or in layman's terms, the
second element of L.
MD)

PR(
expand-case (internal);expr expr env env -> expr
PR)

CB
regmacro case

proc ::constcl::expand-case {expr env} {
  set tail [cdr $expr]
  do-case [car $tail] [cdr $tail] $env
}

proc ::constcl::do-case {keyexpr clauses env} {
  if {[eq? [length $clauses] #0] ne "#f"} {
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
    return [expand-quasiquote [parse $qq] $env]
  }
}
CB

TT(
::tcltest::test macros-3.0 {expand case macro} -body {
    pxw "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"
} -output "(if (memv (* 2 3) (quote (2 3 5 7))) (begin (quote prime)) (if (memv (* 2 3) (quote (1 4 6 8 9))) (begin (quote composite)) (quote ())))\n"

::tcltest::test macros-3.1 {run case macro} -body {
    pew "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"
} -output "composite\n"

::tcltest::test macros-3.2 {expand case macro} -body {
    pxw "(case (car (quote (c d))) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))"
} -output "(if (memv (car (quote (c d))) (quote (a e i o u))) (begin (quote vowel)) (if (memv (car (quote (c d))) (quote (w y))) (begin (quote semivowel)) (if #t (begin (quote consonant)) (quote ()))))\n"

::tcltest::test macros-3.3 {run case macro} -body {
    pew "(case (car (quote (c d))) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))"
} -output "consonant\n"
TT)

MD(
__expand-cond__

The `cond` form has a list of clauses, each with a predicate and a body. The
clauses is considered in order, and if a predicate evaluates to something other
than `#f` the body is evaluated and the remaining clauses are ignored.

The `cond` macro is expanded by `expand-cond`. It expands to `'()` if there are no
clauses (left), and to nested `if` constructs if there are some.

MD)

PR(
expand-cond (internal);expr expr env env -> expr
PR)

CB
regmacro cond

proc ::constcl::expand-cond {expr env} {
  return [do-cond [cdr $expr] $env]
}

proc ::constcl::do-cond {tail env} {
  set clauses $tail
  if {[eq? [length $clauses] #0] ne "#f"} {
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
    return [expand-quasiquote [parse $qq] $env]
  }
}
CB

TT(
::tcltest::test macros-2.0 {expand cond macro} -body {
    pxw "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)) (else (- 8 5)))"
} -output "(if (> 3 4) (begin (+ 4 2)) (if (> 1 2) (begin (+ 5 5)) (if #t (begin (- 8 5)) (quote ()))))\n"

::tcltest::test macros-2.1 {run cond macro} -body {
    pew "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)) (else (- 8 5)))"
    pew "(cond ((> 3 4) => (+ 4 2)) ((> 1 2) => (+ 5 5)) (else (- 8 5)))"
} -output "3\n3\n"

::tcltest::test macros-2.2 {expand cond macro} -body {
    pxw "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)))"
} -output "(if (> 3 4) (begin (+ 4 2)) (if (> 1 2) (begin (+ 5 5)) (quote ())))\n"

::tcltest::test macros-2.3 {run cond macro} -body {
    pew "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)))"
} -output "()\n"

::tcltest::test macros-2.4 {expand cond macro} -body {
    pxw "(cond ((> 3 4) (+ 4 2) (+ 3 5)) ((> 1 2) (+ 5 5)))"
} -output "(if (> 3 4) (begin (+ 4 2) (+ 3 5)) (if (> 1 2) (begin (+ 5 5)) (quote ())))\n"

::tcltest::test macros-2.5 {expand cond macro} -body {
    pxw "(cond ((> 3 4) => (+ 4 2) (+ 3 5)) ((> 1 2) => (+ 5 5)))"
} -output "(if (> 3 4) (begin (+ 4 2) (+ 3 5)) (if (> 1 2) (begin (+ 5 5)) (quote ())))\n"

TT)

MD(
__expand-define__

`define` has two variants, one of which requires some rewriting. It's the one
with an implied `lambda` call, the one that defines a procedure. 

(define (**symbol** **formals**) **body**)

is transformed into

(define **symbol** (lambda **formals** **body**))

which conforms better to `eval`'s standard of (define **symbol** **value**).
MD)

PR(
expand-define (internal);expr expr env env -> expr
PR)

CB
regmacro define

proc ::constcl::expand-define {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  /define [S tail] $tail $env
  set qq "`(define ,(caar tail)
             (lambda ,(cdar tail) ,@(cdr tail)))"
  return [expand-quasiquote [parse $qq] $env]
}
CB

MD(
__expand-del!__

The macro `del!` updates a property list. It removes a key-value pair if the key
is present, or leaves the list untouched if it isn't.
MD)

PR(
expand-del! (internal);expr expr env env -> expr
PR)

CB
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
  return [expand-quasiquote [parse $qq] $env]
}
CB

MD(
__expand-for__

The `expand-for` procedure expands the `for` macro. It returns a `begin`
construct containing the iterations of each clause (multiple clauses
weren't implemented, but I brought up my strongest brain cells and they
did it).
MD)

PR(
for-seq (internal);seq val env env -> tvals
PR)

CB
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
    set seq [lmap c [split [$seq value] {}] {
      MkChar #\\$c
    }]
  } elseif {[vector? $seq] ne "#f"} {
    set seq [$seq value]
  }
}
CB

PR(
do-for (internal);tail exprtail env env -> texprs
PR)

CB
proc ::constcl::do-for {tail env} {
  # make clauses a Tcl list
  set clauses [splitlist [car $tail]]
  set body [cdr $tail]
  set data [dict create]
  set length 0
  foreach clause $clauses {
    set id [car $clause]
    set iter [for-seq [cadr $clause] $env]
    set length [llength $iter]
    # save every id and step of the iteration
    for {set i 0} {$i < $length} {incr i} {
        dict set data $id $i [lindex $iter $i]
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
CB

PR(
expand-for (internal);expr expr env env -> expr
PR)

CB
proc ::constcl::expand-for {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  lappend res [parse "'()"]
  return [list [S begin] {*}$res]
}
CB

TT(
::tcltest::test macros-4.0 {expand for macro} -body {
    pxw "(for ((i '(1 2 3))) (display i))"
} -output "(begin (let ((i 1)) (display i)) (let ((i 2)) (display i)) (let ((i 3)) (display i)) (quote ()))\n"

::tcltest::test macros-4.1 {run for macro} -body {
    pew "(for ((i (quote (1 2 3)))) (display i))"
} -result "" -output 123()\n

::tcltest::test macros-4.2 {expand for macro} -body {
    pxw "(for ((i 4)) (display i))"
} -output "(begin (let ((i 0)) (display i)) (let ((i 1)) (display i)) (let ((i 2)) (display i)) (let ((i 3)) (display i)) (quote ()))\n"

::tcltest::test macros-4.3 {run for macro} -body {
    pew "(for ((i 4)) (display i))"
} -result "" -output "0123()\n"

TT)

MD(
__expand-for/and__

The `expand-for/and` procedure expands the `for/and` macro. It returns an `and`
construct containing the iterations of the clauses.
MD)

PR(
expand-for/and (internal);expr expr env env -> expr
PR)

CB
regmacro for/and

proc ::constcl::expand-for/and {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S and] {*}$res]
}
CB

MD(
__expand-for/list__

The `expand-for/list` procedure expands the `for/list` macro. It returns a `list`
construct containing the iterations of each clause.
MD)

PR(
expand for/list (internal);expr expr env env -> expr
PR)

CB
regmacro for/list

proc ::constcl::expand-for/list {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S list] {*}$res]
}
CB

TT(
::tcltest::test macros-5.0 {expand for/list macro} -body {
    pxw {(for/list ([i (quote (1 2 3))]) (* i i))}
} -output "(list (let ((i 1)) (* i i)) (let ((i 2)) (* i i)) (let ((i 3)) (* i i)))\n"

::tcltest::test macros-5.1 {run for/list macro} -body {
    pew {(for/list ([i (quote (1 2 3))]) (* i i))}
} -output "(1 4 9)\n"

::tcltest::test macros-5.2 {expand for/list macro} -body {
    pxw {(for/list ([c "abc"]) (char-upcase c))}
} -output "(list (let ((c #\\a)) (char-upcase c)) (let ((c #\\b)) (char-upcase c)) (let ((c #\\c)) (char-upcase c)))\n"

::tcltest::test macros-5.3 {run for/list macro} -body {
    pew {(for/list ([c "abc"]) (char-upcase c))}
} -output "(#\\A #\\B #\\C)\n"

::tcltest::test macros-5.4 {expand for/list macro} -body {
    pxw {(for/list ([i (in-range 1 4)]) (* i i))}
} -output "(list (let ((i 1)) (* i i)) (let ((i 2)) (* i i)) (let ((i 3)) (* i i)))\n"

::tcltest::test macros-5.5 {run for/list macro} -body {
    pew {(for/list ([i (in-range 1 4)]) (* i i))}
} -output "(1 4 9)\n"

::tcltest::test macros-5.6 {expand for/list macro} -body {
    pxw {(for/list ([i (in-range 1 4)] [j "abc"]) (list i j))}
} -output "(list (let ((i 1) (j #\\a)) (list i j)) (let ((i 2) (j #\\b)) (list i j)) (let ((i 3) (j #\\c)) (list i j)))\n"

::tcltest::test macros-5.7 {run for/list macro} -body {
    pew {(for/list ([i (in-range 1 4)] [j "abc"]) (list i j))}
} -output "((1 #\\a) (2 #\\b) (3 #\\c))\n"

TT)

MD(
__expand-for/or__

The `expand-for/or` procedure expands the `for/or` macro. It returns an `or`
construct containing the iterations of each clause.
MD)

PR(
expand-for/or (internal);expr expr env env -> expr
PR)

CB
regmacro for/or

proc ::constcl::expand-for/or {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S or] {*}$res]
}
CB

MD(
__expand-let__

`expand-let` expands the named `let` and 'regular' `let` macros. They ultimately
expand to `lambda` constructs.
MD)

PR(
expand-let (internal);expr expr env env -> expr
PR)

CB
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
    return [expand-quasiquote [parse $qq] $env]
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
    return [expand-quasiquote [parse $qq] $env]
  }
}

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
}
CB

MD(
__expand-or__

`expand-or` expands the `or` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.
MD)

PR(
expand-or (internal);expr expr env env -> expr
PR)

CB
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
CB

PR(
do-or (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::do-or {tail env} {
  set env [Environment new #NIL {} $env]
  /if {eq? [length $tail] #0} {
    return #f
  } {
    /define [S first] [car $tail] $env
    /define [S rest] [do-or [cdr $tail] $env] $env
    set qq "`(let ((x ,first)) (if x x ,rest))"
    return [expand-quasiquote [parse $qq] $env]
  }
}
CB

MD(
__expand-pop!__

The macro `pop!` updates a list. It removes the first element.
MD)

PR(
expand-pop! (internal);expr expr env env -> expr
PR)

CB
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
  return [expand-quasiquote [parse $qq] $env]
}
CB

TT(
::tcltest::test macros-11.0 {try pop!} -body {
  pew "(let ((x '(a b c))) (pop! x))"
} -output "(b c)\n"

::tcltest::test macros-11.1 {try pop!, badly} -body {
  pew "(let ((x '(a b c))) (pop!))"
} -returnCodes error -result "too few arguments:\n(pop! listname)"

::tcltest::test macros-11.2 {try pop!, badly} -body {
  pew "(let ((x '(a b c))) (pop! '(a b c)))"
} -returnCodes error -result "SYMBOL expected:\n(pop! listname)"

TT)

MD(
__expand-push!__

The macro `push!` updates a list. It adds a new element as the new first element.
MD)

PR(
expand-push! (internal);expr expr env env -> expr
PR)

CB
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
  return [expand-quasiquote [parse $qq] $env]
}
CB

TT(
::tcltest::test macros-12.0 {try push!} -body {
  pew "(let ((x '(a b c))) (push! 'd x))"
} -output "(d a b c)\n"
TT)

MD(
__expand-put!__

The macro `put!` updates a property list. It adds a key-value pair if the key
isn't present, or changes the value in place if it is.
MD)

PR(
expand-put! (internal);expr expr env env -> expr
PR)

CB
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
  return [expand-quasiquote [parse $qq] $env]
}
CB

TT(
::tcltest::test macros-9.0 {expand put!} -body {
    pxw "(put! plist 'c 7)"
} -output "(let ((idx (list-find-key plist (quote c)))) (if (< idx 0) (set! plist (append (list (quote c) 7) plist)) (begin (list-set! plist (+ idx 1) 7) plist)))\n"

::tcltest::test macros-9.1 {run put!} -body {
    pew "(define plst (list 'a 1 'b 2 'c 3 'd 4 'e 5))"
    pew "(put! plst 'c 7)"
    pew "(put! plst 'f 6)"
    pew "plst"
} -output "(a 1 b 2 c 7 d 4 e 5)\n(f 6 a 1 b 2 c 7 d 4 e 5)\n(f 6 a 1 b 2 c 7 d 4 e 5)\n"

::tcltest::test macros-9.2 {expand put!} -body {
    pew "(define listname 'plist)"
    pew "(define key ''c)"
    pew "(define val 7)"
    pxw "`(let ((idx (list-find-key ,listname ,key))) (if (< idx 0) (set! ,listname (append (list ,key ,val) ,listname)) (begin (list-set! plist (+ idx 1) ,val) ,listname)))"
} -output "(let ((idx (list-find-key plist (quote c)))) (if (< idx 0) (set! plist (append (list (quote c) 7) plist)) (begin (list-set! plist (+ idx 1) 7) plist)))\n"

TT)

MD(
__expand-quasiquote__

A quasi-quote isn't a macro, but we will deal with it in this section anyway.
`expand-quasiquote` traverses the quasi-quoted structure searching for `unquote`
and `unquote-splicing`. This code is brittle and sprawling and I barely
understand it myself.
MD)

PR(
qq-visit-child (internal);node lexprs qqlevel tnum env env -> texprs
PR)

CB
regmacro quasiquote

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
CB

PR(
expand-quasiquote (internal);expr expr env env -> expr
PR)

CB
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
CB

MD(
__expand-unless__

`unless` is a conditional like `if`, with the differences that it takes a number
of expressions and only executes them for a false outcome of `car $tail`.
MD)

PR(
expand-unless (internal);expr expr env env -> expr
PR)

CB
regmacro unless

proc ::constcl::expand-unless {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  /define [S tail] $tail $env
  set qq "`(if ,(car tail)
             '()
             (begin ,@(cdr tail)))"
  return [expand-quasiquote [parse $qq] $env]
}
CB

MD(
__expand-when__

`when` is a conditional like `if`, with the differences that it takes a number
of expressions and only executes them for a true outcome of `car $tail`.
MD)

PR(
expand-when (internal);expr expr env env -> expr
PR)

CB
regmacro when

proc ::constcl::expand-when {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  /define [S tail] $tail $env
  set qq "`(if ,(car tail)
             (begin ,@(cdr tail))
             '())"
  return [expand-quasiquote [parse $qq] $env]
}
CB

TT(

::tcltest::test macros-1.2 {expand or macro} -body {
    pxw "(or)"
    pxw "(or #f)"
    pxw "(or (> 3 2))"
    pxw "(or (> 3 2) (= 7 8))"
} -output "(begin #f)\n(begin #f)\n(begin (> 3 2))\n(let ((x (> 3 2))) (if x x (let ((x (= 7 8))) (if x x #f))))\n"

::tcltest::test macros-1.3 {expand let macro} -body {
    pxw "(let ((x 10)) (* x x))"
    pxw "(let ((x 10) (y 5)) (* x y))"
    pxw "(let ((x 10) (y 5)) (define z 7) (* x y z))"
} -output "((lambda (x) (* x x)) 10)\n((lambda (x y) (* x y)) 10 5)\n((lambda (x y) (define z 7) (* x y z)) 10 5)\n"

::tcltest::test macros-1.4 {run let macro} -body {
    pew "(let ((x 10)) (* x x))"
    pew "(let ((x 10) (y 5)) (* x y))"
    pew "(let ((x 10) (y 5) (z 7)) (+ 2 3) (* x y z))"
} -output "100\n50\n350\n"

::tcltest::test macros-1.5 {expand named let macro} -body {
    pxw {(let loop ((lst lst) (result '()))
    (if (null? lst)
        (reverse result)
        (let ((item (car lst)))
          (loop (cdr lst)
                (if (fn item) result (cons item result))))))}
} -output "(let ((loop #f) (lst lst) (result (quote ()))) (set! loop (lambda (lst result) (if (null? lst) (reverse result) (let ((item (car lst))) (loop (cdr lst) (if (fn item) result (cons item result))))))) (loop lst result))\n"

::tcltest::test macros-6.0 {quasiquotation} -body {
    pw {`(list ,(+ 1 2) 4)}
} -output "(quasiquote (list (unquote (+ 1 2)) 4))\n"

::tcltest::test macros-6.1 {quasiquotation} -body {
    w [::constcl::expand-quasiquote [p {`(list ,(+ 1 2) 4)}] ::constcl::global_env]
    pew {(define name 'a)}
    w [::constcl::expand-quasiquote [p {`(list ,name ',name)}] ::constcl::global_env]
    w [::constcl::expand-quasiquote [p {`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)}] ::constcl::global_env]
    w [::constcl::expand-quasiquote [p {`(( foo ,(- 10 3)) ,@(cdr '(c)) ,(car '(cons)))}] ::constcl::global_env]
    w [::constcl::expand-quasiquote [p {`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)}] ::constcl::global_env]
} -output "(list 3 4)\n(list a (quote a))\n(a 3 4 5 6 b)\n((foo 7) cons)\n(vector 10 5 2.0 4.0 3.0 8)\n"

::tcltest::test macros-6.2 {quasiquotation} -body {
    w [::constcl::expand-quasiquote [p {`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)}] ::constcl::global_env]
if no {
    pew {(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e))}
    pew {'(quasiquote (list (unquote (+ 1 2)) 4))}
}
} -output "(a (quasiquote (b (unquote (+ 1 2)) (unquote (foo 4 d)) e)) f)\n"

#(a `(b (unquote x) (unquote (quote y)) d) e)\n(quasiquote (list (unquote (+ 1 2)) 4))\n"

::tcltest::test macros-7.0 {define} -body {
    pxw {(define (foo a b) (+ a b) (* a b))}
} -output "(define foo (lambda (a b) (+ a b) (* a b)))\n"

::tcltest::test macros-7.1 {define} -body {
    pxw "(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"
} -output "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))\n"

::tcltest::test macros-7.2 {define} -body {
    pxw "(define (f) (define r 20) (* r r))"
} -output "(define f (lambda () (define r 20) (* r r)))\n"

::tcltest::test macros-7.3 {define} -body {
    pxw "(define (list-find-key lst key)\n(lfk lst key 0))"
} -output "(define list-find-key (lambda (lst key) (lfk lst key 0)))\n"

::tcltest::test macros-8.0 {conditional: expand unless macro} -body {
    pxw "(unless (zero? 0) (* 4 4) (- 5 5))"
} -output "(if (zero? 0) (quote ()) (begin (* 4 4) (- 5 5)))\n"

::tcltest::test macros-8.1 {conditional: run unless macro} -body {
    pew "(unless (zero? 0) (* 4 4) (- 5 5))"
} -output "()\n"

::tcltest::test macros-8.2 {conditional: expand when macro} -body {
    pxw "(when (zero? 0) (* 4 4) (- 5 5))"
} -output "(if (zero? 0) (begin (* 4 4) (- 5 5)) (quote ()))\n"

::tcltest::test macros-8.3 {conditional: run when macro} -body {
    pew "(when (zero? 0) (* 4 4) (- 5 5))"
} -output "0\n"

::tcltest::test macros-10.0 {expand let, experimental code} -body {
    #set env [::constcl::Environment new #NIL {} ::constcl::global_env]
    set ::constcl::env ::constcl::global_env
    pew "(define varlist '(a b c))"
    pew "(define body    '((+ a b) (* c 4)))"
    pew "(define vallist '(1 2 3))"
    pxw "`((lambda (,@varlist) ,@body) ,@vallist)"
} -output "((lambda (a b c) (+ a b) (* c 4)) 1 2 3)\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
