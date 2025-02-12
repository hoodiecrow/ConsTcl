
MD(
## Evaluation

The second thing an interpreter must be able to do is to reduce expressions to
their normal form, or **evaluate** them. As an example, 2 + 6 and 8 are two
expressions that have the same value, but the latter is in normal form (can't be
reduced further) and the former is not.

There are nine diffent forms or classes of expressions in Lisp.

TblSynForms

MD)

MD(
__eval__

The heart of the Lisp interpreter, `eval` takes a Lisp expression and processes
it according to its form.

`eval`:

1. processes an **expression** to get a **value**. The exact method depends on the form of expression, see above and below.
1. does a form of **macro expansion** on the car and cdr of a non-atomic expression before processing it further. See the part about macros[#](https://github.com/hoodiecrow/ConsTcl#macros) below.
1. resolves **local defines**, acting on expressions of the form `(begin (define ...` when in a local environment. See the part about resolving local defines[#](https://github.com/hoodiecrow/ConsTcl#resolving-local-defines).
MD)

PR(
eval (public);expr expr env env -> val
PR)

CB
reg eval ::constcl::eval

proc ::constcl::eval {expr {env ::constcl::global_env}} {
  if {[symbol? $expr] ne "#f"} {
    lookup $expr $env
  } elseif {[null? $expr] ne "#f" || [atom? $expr] ne "#f"} {
    set expr
  } else {
    set op [car $expr]
    set args [cdr $expr]
    while {[$op name] in $::constcl::macrolist} {
      expand-macro $env
    }
    if {$env ne "::constcl::global_env" && [$op name] eq "begin" && ([pair? [car $args]] ne "#f" && [[caar $args] name] eq "define")} {
      set expr [resolve-local-defines $args]
      set op [car $expr]
      set args [cdr $expr]
    }
    switch [$op name] {
      quote { car $args }
      if { if {[eval [car $args] $env] ne "#f"} {eval [cadr $args] $env} {eval [caddr $args] $env} }
      begin { eprogn $args $env }
      define { declare [car $args] [eval [cadr $args] $env] $env }
      set! { update! [car $args] [eval [cadr $args] $env] $env }
      lambda { make-function [car $args] [cdr $args] $env }
      default { invoke [eval $op $env] [eval-list $args $env] }
    }
  }
}
CB

MD(

#### Syntactic forms

##### Variable reference

Example: `r` => 10 (a symbol `r` is evaluated to 10)

A variable is an identifier (symbol) bound to a location in the environment. If
an expression consists of the identifier it is evaluated to the value stored in
that location. This is handled by the helper procedure `lookup`. It searches the
environment chain for the identifier, and returns the value stored in the
location it is bound to.  It is an error to lookup an unbound symbol.

__lookup__
MD)

PR(
lookup (internal);sym sym env env -> val
PR)

CB
proc ::constcl::lookup {sym env} {
  [$env find $sym] get $sym
}
CB

MD(
##### Constant literal

Example: `99` => 99 (a number evaluates to itself)

Not just numbers but booleans, characters, strings, and vectors evaluate to
themselves, to their innate value. Because of this, they are called autoquoting
types (see next paragraph).
MD)

MD(
##### Quotation

Example: `(quote r)` => `r` (quotation makes the symbol evaluate to itself, like a
constant)

According to the rules of Variable reference, a symbol evaluates to its stored
value. Well, sometimes one wishes to use the symbol itself as a value. That is
what quotation is for. `(quote x)` evaluates to the symbol x itself and not to
any value that might be stored under it. This is so common that there is a
shorthand notation for it: `'x` is interpreted as `(quote x)` by the Lisp
reader.
MD)

MD(
##### Conditional

Example: `(if (> 99 100) (* 2 2) (+ 2 4))` => 6

The conditional form `if` evaluates a Lisp list of three expressions. The first,
the _condition_, is evaluated first. If it evaluates to anything other than `#f`
(false), the second expression (the _consequent_) is evaluated and the value
returned. Otherwise, the third expression (the _alternate_) is evaluated and the
value returned. One of the two latter expressions will be evaluated, and the
other will remain unevaluated.

__if__
MD)

PR(
_if (internal);condition expr consequent expr alternate expr -> val
PR)

CB
proc ::constcl::_if {cond conseq altern} {
  if {[uplevel $cond] ne "#f"} {uplevel $conseq} {uplevel $altern}
}
CB

MD(
##### Sequence

Example: `(begin (define r 10) (* r r))` => 100

When expressions are evaluated in sequence, the order is important for two
reasons. If the expressions have any side effects, they happen in the same order
of sequence. Also, if expressions are part of a pipeline of calculations, then
they need to be processed in the order of that pipeline. The `eprogn` helper
procedure takes a Lisp list of expressions and evaluates them in sequence,
returning the value of the last one.

__eprogn__
MD)

PR(
eprogn (internal);exps lexprs env env -> val
PR)

CB
proc ::constcl::eprogn {exps env} {
  _if {pair? $exps} {
    _if {pair? [cdr $exps]} {
      eval [car $exps] $env
      return [eprogn [cdr $exps] $env]
    } {
      return [eval [car $exps] $env]
    }
  } {
    return #NIL
  }
}
CB

MD(
##### Definition

Example: `(define r 10)` => ... (a definition doesn't evaluate to anything)

We've already seen the relationship between symbols and values. A symbol is
bound to a value (or rather to the location the value is in), creating a
variable, through definition. The `declare` helper procedure adds a variable to
the current environment. It first checks that the symbol name is a valid
identifier, then it updates the environment with the new binding.

__declare__
MD)

PR(
declare (internal);sym sym val val env env -> none
PR)

CB
proc ::constcl::declare {sym val env} {
  varcheck [idcheck [$sym name]]
  $env set $sym $val
  return #NONE
}
CB

MD(
##### Assignment

Example: `(set! r 20)` => 20 (`r` is a bound symbol, so it's allowed to assign
to it)

Once a variable has been created, the value at the location it is bound to can
be changed (hence the name "variable", something that can be modified). The
process is called assignment. The `update!` helper does assignment: it modifies
an existing variable that is bound somewhere in the environment chain. It finds
the variable's environment and updates the binding. It returns the value, so
calls to `set!` can be chained: `(set! foo (set! bar 99))` sets both variables
to 99.

__update!__
MD)

PR(
update! (internal);var bsym val val env env -> val
PR)

CB
proc ::constcl::update! {var val env} {
  [$env find $var] set $var $val
  set val
}
CB

MD(
##### Procedure definition

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

Under the hood, the helper `make-function` makes a
Procedure[#](https://github.com/hoodiecrow/ConsTcl#control) object. First it
needs to convert the Lisp list `body`. It is packed inside a `begin` if it has
more than one expression, and taken out of its list if not. The Lisp list
`formals` is passed on as is.

A Scheme formals list is either:

* An **empty list**, `()`, meaning that no arguments are accepted,
* A **proper list**, `(a b c)`, meaning it accepts three arguments, one in each symbol,
* A **symbol**, `a`, meaning that all arguments go into `a`, or
* A **dotted list**, `(a b . c)`, meaning that two arguments go into `a` and `b`, and the rest into `c`.

__make-function__
MD)

PR(
make-function (internal);formals formals body lexprs env env -> proc
PR)

CB
proc ::constcl::make-function {formals body env} {
  if {[[length $body] value] > 1} {
    set body [cons #B $body]
  } else {
    set body [car $body]
  }
  return [MkProcedure $formals $body $env]
}
CB

MD(
##### Procedure call

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
MD)

PR(
invoke (internal);pr proc vals lvals -> invoke
PR)

CB
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
CB

TT(
::tcltest::test eval-check-1.0 {try triggering a check} -body {
    ::constcl::invoke #NIL [list #NIL #NIL]
} -returnCodes error -result "PROCEDURE expected\n(() val ...)"
TT)

MD(
__splitlist__

`splitlist` converts a Lisp list to a Tcl list with Lisp objects.
MD)

PR(
splitlist (internal);vals lvals -> tvals
PR)

CB
proc ::constcl::splitlist {vals} {
  set result {}
  while {[pair? $vals] ne "#f"} {
    lappend result [car $vals]
    set vals [cdr $vals]
  }
  return $result
}
CB

MD(
__eval-list__

`eval-list` successively evaluates the elements of a Lisp list and returns the
collected results as a Lisp list.
MD)

PR(
eval-list (internal);exps lexprs env env -> lvals
PR)

CB
proc ::constcl::eval-list {exps env} {
  # don't convert to ::constcl::if, it breaks (fact 100)
  if {[pair? $exps] ne "#f"} {
    return [cons [eval [car $exps] $env] \
      [eval-list [cdr $exps] $env]]
  } {
    return #NIL
  }
}
CB

MD(
### Macros

__expand-macro__

Macros that rewrite expressions into other, more concrete expressions is one of
Lisp's strong points. This interpreter does macro expansion, but the user can't
define new macros--the ones available are hardcoded in the code below.

`expand-macro` only takes the environment as a parameter, but internally it uses
variable sharing to get the expression it is to process. It shares the variables
`op` and `args` with its caller, `eval`. `op` is used to delegate to the correct
expansion procedure, and the value of `args` is passed to the expansion
procedures. In the end, the expanded expression is passed back to `eval` by
assigning to `op` and `args`.

Before it gets to the expansion call, the procedure does some extra processing
if the operator is `define`. If the car of the `args` is something other than a
Pair, or if the caar of the `args` is the symbol `lambda`, then no expansion is
needed and the procedure returns with a code to break the while loop in `eval`.
MD)

PR(
expand-macro (internal);env env -> nil
PR)

CB
proc ::constcl::expand-macro {env} {
  upvar op op args args
  if {[$op name] eq "define" && ([pair? [car $args]] eq "#f" || [[caar $args] name] eq "lambda")} {
    return -code break
  }
  set expr [expand-[$op name] [cons $op $args] $env]
  set op [car $expr]
  set args [cdr $expr]
  return #NIL
}
CB

MD(
__expand-and__

`expand-and` expands the `and` macro. It returns a `begin`-expression if the
macro has 0 or 1 elements, and a nested `if` construct otherwise.
MD)

PR(
expand-and (internal);expr expr env env -> expr
PR)

CB
regmacro and

proc ::constcl::expand-and {expr env} {
  set tail [cdr $expr]
  _if {eq? [length $tail] #0} {
    return [list #B #t]
  } {
    _if {eq? [length $tail] #1} {
      return [cons #B $tail]
    } {
      return [do-and $tail #NIL $env]
    }
  }
}
CB

PR(
do-and (internal);tail exprtail prev expr env env -> expr
PR)

CB
proc ::constcl::do-and {tail prev env} {
  set env [::constcl::Environment new #NIL {} $env]
  _if {eq? [length $tail] #0} {
    return $prev
  } {
    $env setstr "first" [car $tail]
    $env setstr "rest" [do-and [cdr $tail] \
      [car $tail] $env]
    set qq "`(if ,first ,rest #f)"
    return [expand-quasiquote [parse $qq] $env]
  }
}
CB

MD(
__expand-case__

The `case` macro is expanded by `expand-case`. It returns `'()` if there are no clauses (left), 
and nested `if` constructs if there are some.
MD)

PR(
expand-case (internal);expr expr env env -> expr
PR)

CB
regmacro case

proc ::constcl::expand-case {expr env} {
  set tail [cdr $expr]
  do-case [car $tail] [cdr $tail]
}

proc ::constcl::do-case {keyexpr clauses} {
  if {[eq? [length $clauses] #0] ne "#f"} {
    return [list #Q #NIL]
  } else {
    set keyl [caar $clauses]
    set body [cdar $clauses]
    set keyl [list [MkSymbol "memv"] $keyexpr [list #Q $keyl]]
    if {[eq? [length $clauses] #1] ne "#f"} {
      if {[eq? [caar $clauses] [MkSymbol "else"]] ne "#f"} {
        set keyl #t
      }
    }
    return [list #I $keyl [cons #B $body] [do-case $keyexpr [cdr $clauses]]]
  }
}
CB

MD(
__expand-cond__

The `cond` macro is expanded by `expand-cond`. It returns `'()` if there are no
clauses (left), and nested `if` constructs if there are some.

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
    return [list #Q #NIL]
  } else {
    set pred [caar $clauses]
    set body [cdar $clauses]
    if {[symbol? [car $body]] ne "#f" && [[car $body] name] eq "=>"} {
      set body [cddar $clauses]
    }
    if {[eq? [length $clauses] #1] ne "#f"} {
      if {[eq? $pred [MkSymbol "else"]] ne "#f"} {
        set pred #t
      }
    }
    if {[null? $body] ne "#f"} {set body $pred}
    return [list #I $pred [cons #B $body] [do-cond [cdr $clauses] $env]]
  }
}
CB

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
  $env setstr "tail" $tail
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
  set env [::constcl::Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
    ::error "too few arguments, 2 expected, got 0"
  }
  $env setstr "listname" [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {
    ::error "too few arguments, 2 expected, got 1"
  }
  $env setstr "key" [cadr $tail]
  set qq "`(set! ,listname (delete! ,listname ,key))"
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
    set seq [lmap c [split [$seq value] {}] {MkChar #\\$c}]
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
  set ids {}
  set seqs {}
  for {set i 0} {$i < [llength $clauses]} {incr i} {
    set clause [lindex $clauses $i]
    # insert the first part of the clause in the ids structure
    lset ids $i [car $clause]
    # run the second part of the clause through for-seq and insert in seqs
    lset seqs $i [for-seq [cadr $clause] $env]
  }
  set res {}
  for {set item 0} {$item < [llength [lindex $seqs 0]]} {incr item} {
    # for each iteration of the sequences
    set x {}
    for {set clause 0} {$clause < [llength $clauses]} {incr clause} {
      # for each clause
      # list append to x the Lisp list of the id and the iteration
      lappend x [list [lindex $ids $clause] [lindex $seqs $clause $item]]
    }
    # list append to res a let expression with the ids and iterations and the body
    lappend res [list #L [list {*}$x] {*}[splitlist $body]]
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
  lappend res [list #Q #NIL]
  return [list #B {*}$res]
}
CB

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
  return [list [MkSymbol "and"] {*}$res]
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
  return [list [MkSymbol "list"] {*}$res]
}
CB

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
  return [list [MkSymbol "or"] {*}$res]
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
  set env [::constcl::Environment new #NIL {} $env]
  if {[symbol? [car $tail]] ne "#f"} {
    # named let
    set variable [car $tail]
    set bindings [cadr $tail]
    set body [cddr $tail]
    set vars [dict create $variable #f]
    parse-bindings vars $bindings
    $env setstr "decl" [list {*}[dict values [dict map {k v} $vars {list $k $v}]]]
    $env setstr "variable" $variable
    $env setstr "varlist" [list {*}[lrange [dict keys $vars] 1 end]]
    $env setstr "body" $body
    $env setstr "call" [list {*}[dict keys $vars]]
    set qq "`(let ,decl (set! ,variable (lambda ,varlist ,@body)) ,call)"
    return [expand-quasiquote [parse $qq] $env]
  } else {
    # regular let
    set bindings [car $tail]
    set body [cdr $tail]
    set vars [dict create]
    parse-bindings vars $bindings
    $env setstr "varlist" [list {*}[dict keys $vars]]
    $env setstr "body" $body
    $env setstr "vallist" [list {*}[dict values $vars]]
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
    return [list #B #f]
  } elseif {[eq? [length $tail] #1] ne "#f"} {
    return [cons #B $tail]
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
  set env [::constcl::Environment new #NIL {} $env]
  _if {eq? [length $tail] #0} {
    return #f
  } {
    $env setstr "first" [car $tail]
    $env setstr "rest" [do-or [cdr $tail] $env]
    set qq "`(let ((x ,first)) (if x x ,rest))"
    return [expand-quasiquote [parse $qq] $env]
  }
}
CB

MD(
__expand-pop!__

The macro `push!` updates a list. It adds a new element as the new first element.
MD)

PR(
expand-pop! (internal);expr expr env env -> expr
PR)

CB
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
CB

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
  set env [::constcl::Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
  $env set [MkSymbol "obj"] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
  if {[symbol? [cadr $tail]] eq "#f"} {::error "SYMBOL expected:\n(push! obj listname)"}
  $env set [MkSymbol "listname"] [cadr $tail]
  set qq "`(set! ,listname (cons ,obj ,listname))"
  return [expand-quasiquote [parse $qq] $env]
}
CB

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
CB

MD(
__expand-quasiquote__

A quasi-quote isn't a macro, but we will deal with it in this section anyway. `expand-quasiquote`
traverses the quasi-quoted structure searching for `unquote` and `unquote-splicing`. This code is
brittle and sprawling and I barely understand it myself.
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
      if {[pair? $child] ne "#f" && [eq? [car $child] [MkSymbol "unquote"]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res [eval [cadr $child] $env]
        } else {
          lappend res [list #U [qq-visit-child [cadr $child] [expr {$qqlevel - 1}] $env]]
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
  set env [::constcl::Environment new #NIL {} $env]
  $env setstr "tail" $tail
  set qq "`(if ,(car tail) (quote ()) (begin ,@(cdr tail)))"
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
  set env [::constcl::Environment new #NIL {} $env]
  $env setstr "tail" $tail
  set qq "`(if ,(car tail) (begin ,@(cdr tail)) (quote ()))"
  return [expand-quasiquote [parse $qq] $env]
}
CB

MD(
### Resolving local defines

This section is ported from 'Scheme 9 from Empty Space'. `resolve-local-defines`
is the topmost procedure in rewriting local defines as essentially a `letrec`
form. It takes a list of expressions and extracts variables and values from the
defines in the beginning of the list. It builds a double lambda expression with
the variables and values, and the rest of the expressions from the original list
as body.

__resolve-local-defines__
MD)

PR(
resolve-local-defines;exps lexprs -> expr
PR)

CB
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
CB

MD(
__extract-from-defines__

`extract-from-defines` visits every define in the given list of expressions and
extracts either a variable name or a value, depending on the state of the _part_
flag, from each one of them. A Tcl list of 1) the resulting list of names or
values, 2) error state, and 3) the rest of the expressions in the original list
is returned.
MD)

PR(
extract-from-defines (internal);exps lexprs part varsvals -> tvals
PR)

CB
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
          set new [cons #λ $new]
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
CB

MD(
__argument-list?__

`argument-list?` accepts a Scheme formals list and rejects other values.
MD)

PR(
argument-list? (internal);val val -> bool
PR)

CB
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
CB

MD(
__make-recursive-lambda__

`make-recursive-lambda` builds the `letrec` structure.
MD)

PR(
make-recursive-lambda (internal);vars lsyms args lexprs body lexprs -> expr
PR)

CB
proc ::constcl::make-recursive-lambda {vars args body} {
  set tmps [make-temporaries $vars]
  set body [append-b [make-assignments $vars $tmps] $body]
  set body [cons $body #NIL]
  set n [cons $tmps $body]
  set n [cons #λ $n]
  set n [cons $n $args]
  set n [cons $n #NIL]
  set n [cons $vars $n]
  set n [cons #λ $n]
  set n [cons $n [make-undefineds $vars]]
  return $n
}
CB

MD(
__make-temporaries__

`make-temporaries` creates the symbols that will act as middlemen in
transferring the values to the variables.
MD)

PR(
make-temporaries (internal);vals lvals -> lvals
PR)

CB
proc ::constcl::make-temporaries {vals} {
  set n #NIL
  while {$vals ne "#NIL"} {
    set sym [gensym "g"]
    set n [cons $sym $n]
    set vals [cdr $vals]
  }
  return $n
}
CB

MD(
__gensym__

`gensym` generates an unique symbol.
MD)

PR(
gensym (internal);prefix str -> sym
PR)

CB
proc ::constcl::gensym {prefix} {
  set symbolnames [lmap s [info class instances ::constcl::Symbol] {$s name}]
  set s $prefix<[incr ::constcl::gensymnum]>
  while {$s in $symbolnames} {
    set s $prefix[incr ::constcl::gensymnum]
  }
  return [MkSymbol $s]
}
CB

MD(
__append-b__

`append-b` joins two lists together.
MD)

PR(
append-b (internal);a lvals b lvals -> lvals
PR)

CB
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
CB

MD(
__make-assignments__

`make-assignments` creates the structure that holds the assignment statements.
Later on, it will be joined to the body of the finished expression.
MD)

PR(
make-assignments (internal);vars lsyms tmps lsyms -> expr
PR)

CB
proc ::constcl::make-assignments {vars tmps} {
  set n #NIL
  while {$vars ne "#NIL"} {
    set asg [cons [car $tmps] #NIL]
    set asg [cons [car $vars] $asg]
    set asg [cons #S $asg]
    set n [cons $asg $n]
    set vars [cdr $vars]
    set tmps [cdr $tmps]
  }
  return [cons #B $n]
}
CB

MD(
__make-undefineds__

Due to a mysterious bug, `make-undefineds` actually creates a list of NIL
values instead of undefined values.
MD)

PR(
make-undefineds (internal);vals lvals -> lnils
PR)

CB
proc ::constcl::make-undefineds {vals} {
  # Use #NIL instead of #UNDF because of some strange bug with eval-list.
  set n #NIL
  while {$vals ne "#NIL"} {
    set n [cons #NIL $n]
    set vals [cdr $vals]
  }
  return $n
}
CB

TT(
::tcltest::test eval-1.0 {expand and macro} -body {
    pxp "(and)"
    pxp "(and #t)"
    pxp "(and (> 3 2))"
    pxp "(and (> 3 2) (= 7 8))"
} -output "(begin #t)\n(begin #t)\n(begin (> 3 2))\n(if (> 3 2) (if (= 7 8) (= 7 8) #f) #f)\n"

::tcltest::test eval-1.1 {run and macro} -body {
    pep "(and)"
    pep "(and #t)"
    pep "(and (> 3 2))"
    pep "(and (> 3 2) (= 7 8))"
} -output "#t\n#t\n#t\n#f\n"

::tcltest::test eval-1.2 {expand or macro} -body {
    pxp "(or)"
    pxp "(or #f)"
    pxp "(or (> 3 2))"
    pxp "(or (> 3 2) (= 7 8))"
} -output "(begin #f)\n(begin #f)\n(begin (> 3 2))\n(let ((x (> 3 2))) (if x x (let ((x (= 7 8))) (if x x #f))))\n"

::tcltest::test eval-1.3 {expand let macro} -body {
    pxp "(let ((x 10)) (* x x))"
    pxp "(let ((x 10) (y 5)) (* x y))"
    pxp "(let ((x 10) (y 5)) (define z 7) (* x y z))"
} -output "((lambda (x) (* x x)) 10)\n((lambda (x y) (* x y)) 10 5)\n((lambda (x y) (define z 7) (* x y z)) 10 5)\n"

::tcltest::test eval-1.4 {run let macro} -body {
    pep "(let ((x 10)) (* x x))"
    pep "(let ((x 10) (y 5)) (* x y))"
    pep "(let ((x 10) (y 5) (z 7)) (+ 2 3) (* x y z))"
} -output "100\n50\n350\n"

::tcltest::test eval-1.5 {expand named let macro} -body {
    pxp {(let loop ((lst lst) (result '()))
    (if (null? lst)
        (reverse result)
        (let ((item (car lst)))
          (loop (cdr lst)
                (if (fn item) result (cons item result))))))}
} -output "(let ((loop #f) (lst lst) (result (quote ()))) (set! loop (lambda (lst result) (if (null? lst) (reverse result) (let ((item (car lst))) (loop (cdr lst) (if (fn item) result (cons item result))))))) (loop lst result))\n"

::tcltest::test eval-2.0 {expand cond macro} -body {
    pxp "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)) (else (- 8 5)))"
} -output "(if (> 3 4) (begin (+ 4 2)) (if (> 1 2) (begin (+ 5 5)) (if #t (begin (- 8 5)) (quote ()))))\n"

::tcltest::test eval-2.1 {run cond macro} -body {
    pep "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)) (else (- 8 5)))"
    pep "(cond ((> 3 4) => (+ 4 2)) ((> 1 2) => (+ 5 5)) (else (- 8 5)))"
} -output "3\n3\n"

::tcltest::test eval-2.2 {expand cond macro} -body {
    pxp "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)))"
} -output "(if (> 3 4) (begin (+ 4 2)) (if (> 1 2) (begin (+ 5 5)) (quote ())))\n"

::tcltest::test eval-2.3 {run cond macro} -body {
    pep "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)))"
} -output "()\n"

::tcltest::test eval-2.4 {expand cond macro} -body {
    pxp "(cond ((> 3 4) (+ 4 2) (+ 3 5)) ((> 1 2) (+ 5 5)))"
} -output "(if (> 3 4) (begin (+ 4 2) (+ 3 5)) (if (> 1 2) (begin (+ 5 5)) (quote ())))\n"

::tcltest::test eval-2.5 {expand cond macro} -body {
    pxp "(cond ((> 3 4) => (+ 4 2) (+ 3 5)) ((> 1 2) => (+ 5 5)))"
} -output "(if (> 3 4) (begin (+ 4 2) (+ 3 5)) (if (> 1 2) (begin (+ 5 5)) (quote ())))\n"

::tcltest::test eval-3.0 {expand case macro} -body {
    pxp "(case (* 2 3) ((2 3 5 7) (quote prime)) ((1 4 6 8 9) (quote composite)))"
} -output "(if (memv (* 2 3) (quote (2 3 5 7))) (begin (quote prime)) (if (memv (* 2 3) (quote (1 4 6 8 9))) (begin (quote composite)) (quote ())))\n"

::tcltest::test eval-3.1 {run case macro} -body {
    pep "(case (* 2 3) ((2 3 5 7) (quote prime)) ((1 4 6 8 9) (quote composite)))"
} -output "composite\n"

::tcltest::test eval-3.2 {expand case macro} -body {
    pxp "(case (car (quote (c d))) ((a e i o u) (quote vowel)) ((w y) (quote semivowel)) (else (quote consonant)))"
} -output "(if (memv (car (quote (c d))) (quote (a e i o u))) (begin (quote vowel)) (if (memv (car (quote (c d))) (quote (w y))) (begin (quote semivowel)) (if #t (begin (quote consonant)) (quote ()))))\n"

::tcltest::test eval-3.3 {run case macro} -body {
    pep "(case (car (quote (c d))) ((a e i o u) (quote vowel)) ((w y) (quote semivowel)) (else (quote consonant)))"
} -output "consonant\n"

::tcltest::test eval-4.0 {expand for macro} -body {
    pxp "(for ((i (quote (1 2 3)))) (display i))"
} -output "(begin (let ((i 1)) (display i)) (let ((i 2)) (display i)) (let ((i 3)) (display i)) (quote ()))\n"

::tcltest::test eval-4.1 {run for macro} -body {
    pep "(for ((i (quote (1 2 3)))) (display i))"
} -result "" -output 123()\n

::tcltest::test eval-4.2 {expand for macro} -body {
    pxp "(for ((i 4)) (display i))"
} -output "(begin (let ((i 0)) (display i)) (let ((i 1)) (display i)) (let ((i 2)) (display i)) (let ((i 3)) (display i)) (quote ()))\n"

::tcltest::test eval-4.3 {run for macro} -body {
    pep "(for ((i 4)) (display i))"
} -result "" -output "0123()\n"

::tcltest::test macro-5.0 {expand for/list macro} -body {
    pxp {(for/list ([i (quote (1 2 3))]) (* i i))}
} -output "(list (let ((i 1)) (* i i)) (let ((i 2)) (* i i)) (let ((i 3)) (* i i)))\n"

::tcltest::test macro-5.1 {run for/list macro} -body {
    pep {(for/list ([i (quote (1 2 3))]) (* i i))}
} -output "(1 4 9)\n"

::tcltest::test macro-5.2 {expand for/list macro} -body {
    pxp {(for/list ([c "abc"]) (char-upcase c))}
} -output "(list (let ((c #\\a)) (char-upcase c)) (let ((c #\\b)) (char-upcase c)) (let ((c #\\c)) (char-upcase c)))\n"

::tcltest::test macro-5.3 {run for/list macro} -body {
    pep {(for/list ([c "abc"]) (char-upcase c))}
} -output "(#\\A #\\B #\\C)\n"

::tcltest::test macro-5.4 {expand for/list macro} -body {
    pxp {(for/list ([i (in-range 1 4)]) (* i i))}
} -output "(list (let ((i 1)) (* i i)) (let ((i 2)) (* i i)) (let ((i 3)) (* i i)))\n"

::tcltest::test macro-5.5 {run for/list macro} -body {
    pep {(for/list ([i (in-range 1 4)]) (* i i))}
} -output "(1 4 9)\n"

::tcltest::test macro-5.6 {expand for/list macro} -body {
    pxp {(for/list ([i (in-range 1 4)] [j "abc"]) (list i j))}
} -output "(list (let ((i 1) (j #\\a)) (list i j)) (let ((i 2) (j #\\b)) (list i j)) (let ((i 3) (j #\\c)) (list i j)))\n"

::tcltest::test macro-5.7 {run for/list macro} -body {
    pep {(for/list ([i (in-range 1 4)] [j "abc"]) (list i j))}
} -output "((1 #\\a) (2 #\\b) (3 #\\c))\n"


::tcltest::test eval-5.0 {lambda parameter lists} -body {
    pep {((lambda (x y z) (list x y z)) 3 4 5)}
    pep {((lambda x x) 3 4 5 6)}
    pep {((lambda (x y . z) (list x y)) 3 4 5 6)}
    pep {((lambda (x y . z) z) 3 4 5 6)}
} -output "(3 4 5)\n(3 4 5 6)\n(3 4)\n(5 6)\n"


::tcltest::test eval-6.0 {quasiquotation} -body {
    pp {`(list ,(+ 1 2) 4)}
} -output "(quasiquote (list (unquote (+ 1 2)) 4))\n"

::tcltest::test eval-6.1 {quasiquotation} -body {
    ::constcl::write [::constcl::expand-quasiquote [::constcl::parse {`(list ,(+ 1 2) 4)}] ::constcl::global_env]
    ::constcl::global_env set [::constcl::list [::constcl::MkSymbol "name"]] a
    pep {(define name 'a)}
    ::constcl::write [::constcl::expand-quasiquote [::constcl::parse {`(list ,name ',name)}] ::constcl::global_env]
    ::constcl::write [::constcl::expand-quasiquote [::constcl::parse {`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)}] ::constcl::global_env]
    ::constcl::write [::constcl::expand-quasiquote [::constcl::parse {`(( foo ,(- 10 3)) ,@(cdr '(c)) ,(car '(cons)))}] ::constcl::global_env]
    ::constcl::write [::constcl::expand-quasiquote [::constcl::parse {`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)}] ::constcl::global_env]
if no {
}
} -output "(list 3 4)\n(list a (quote a))\n(a 3 4 5 6 b)\n((foo 7) cons)\n(vector 10 5 2.0 4.0 3.0 8)\n"

::tcltest::test eval-6.2 {quasiquotation} -body {
    ::constcl::write [::constcl::expand-quasiquote [::constcl::parse {`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)}] ::constcl::global_env]
if no {
    pep {(let ((name1 'x)
      (name2 'y))
  `(a `(b ,,name1 ,',name2 d) e))}
    pep {'(quasiquote (list (unquote (+ 1 2)) 4))}
}
} -output "(a (quasiquote (b (unquote (+ 1 2)) (unquote (foo 4 d)) e)) f)\n"

#(a `(b (unquote x) (unquote (quote y)) d) e)\n(quasiquote (list (unquote (+ 1 2)) 4))\n"

::tcltest::test eval-7.0 {define} -body {
    pxp {(define (foo a b) (+ a b) (* a b))}
} -output "(define foo (lambda (a b) (+ a b) (* a b)))\n"

::tcltest::test eval-7.1 {define} -body {
    pxp "(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"
} -output "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))\n"

::tcltest::test eval-7.2 {define} -body {
    pxp "(define (f) (define r 20) (* r r))"
} -output "(define f (lambda () (define r 20) (* r r)))\n"

::tcltest::test eval-7.3 {define} -body {
    pxp "(define (list-find-key lst key)\n(lfk lst key 0))"
} -output "(define list-find-key (lambda (lst key) (lfk lst key 0)))\n"

::tcltest::test eval-8.0 {conditional: does internal if accept a #t? Yes, b/c of the ne in condition handling} -body {
    pep "(if (zero? 0) (* 4 4) (- 5 5))"
} -output "16\n"

::tcltest::test eval-9.0 {conditional: expand unless macro} -body {
    pxp "(unless (zero? 0) (* 4 4) (- 5 5))"
} -output "(if (zero? 0) (quote ()) (begin (* 4 4) (- 5 5)))\n"

::tcltest::test eval-9.1 {conditional: run unless macro} -body {
    pep "(unless (zero? 0) (* 4 4) (- 5 5))"
} -output "()\n"

::tcltest::test eval-9.2 {conditional: expand when macro} -body {
    pxp "(when (zero? 0) (* 4 4) (- 5 5))"
} -output "(if (zero? 0) (begin (* 4 4) (- 5 5)) (quote ()))\n"

::tcltest::test eval-9.3 {conditional: run when macro} -body {
    pep "(when (zero? 0) (* 4 4) (- 5 5))"
} -output "0\n"

::tcltest::test eval-10.0 {expand local defines} -body {
    set x [::constcl::parse [::constcl::IB new "((define n 0) (define a 3) (define b 4) (set! n (+ a b)) (* n n))"]]
    ::constcl::write [::constcl::resolve-local-defines $x]
} -output "((lambda (b a n) ((lambda (g<3> g<2> g<1>) (begin (set! n g<1>) (set! a g<2>) (set! b g<3>) (set! n (+ a b)) (* n n))) 4 3 0)) () () ())\n"

::tcltest::test eval-10.1 {run local defines} -body {
    set x [::constcl::parse [::constcl::IB new "((define n 0) (define a 3) (define b 4) (set! n (+ a b)) (* n n))"]]
    ::constcl::write [::constcl::eval [::constcl::resolve-local-defines $x]]
} -output "49\n"

::tcltest::test eval-10.2 {expand local defines w/o defines} -body {
    set x [::constcl::parse [::constcl::IB new "((set! n (+ a b)) (* n n))"]]
    ::constcl::write [::constcl::resolve-local-defines $x]
} -output "((lambda () ((lambda () (begin (set! n (+ a b)) (* n n))))))\n"

::tcltest::test eval-10.3 {expand local defines with begin} -body {
    set x [::constcl::parse [::constcl::IB new "(begin (set! n (+ a b)) (* n n))"]]
    ::constcl::write [::constcl::resolve-local-defines $x]
} -output "((lambda () ((lambda () (begin begin (set! n (+ a b)) (* n n))))))\n"

::tcltest::test eval-10.4 {expand local defines with proc definition} -body {
    set x [::constcl::parse [::constcl::IB new "((define (foo x) (* x x)) (set! n (+ a b)) (* n n))"]]
    ::constcl::write [::constcl::resolve-local-defines $x]
} -output "((lambda (foo) ((lambda (g<7>) (begin (set! foo g<7>) (set! n (+ a b)) (* n n))) (lambda (x) (* x x)))) ())\n"

::tcltest::test eval-11.0 {expand put!} -body {
    pxp "(put! plist 'c 7)"
} -output "(let ((idx (list-find-key plist (quote c)))) (if (< idx 0) (set! plist (append (list (quote c) 7) plist)) (begin (list-set! plist (+ idx 1) 7) plist)))\n"

::tcltest::test eval-11.1 {run put!} -body {
    pep "(define plst (list 'a 1 'b 2 'c 3 'd 4 'e 5))"
    pep "(put! plst 'c 7)"
    pep "(put! plst 'f 6)"
    pep "plst"
} -output "(a 1 b 2 c 7 d 4 e 5)\n(f 6 a 1 b 2 c 7 d 4 e 5)\n(f 6 a 1 b 2 c 7 d 4 e 5)\n"

::tcltest::test eval-11.2 {expand put!} -body {
    pep "(define listname 'plist)"
    pep "(define key ''c)"
    pep "(define val 7)"
    pxp "`(let ((idx (list-find-key ,listname ,key))) (if (< idx 0) (set! ,listname (append (list ,key ,val) ,listname)) (begin (list-set! plist (+ idx 1) ,val) ,listname)))"
} -output "(let ((idx (list-find-key plist (quote c)))) (if (< idx 0) (set! plist (append (list (quote c) 7) plist)) (begin (list-set! plist (+ idx 1) 7) plist)))\n"

::tcltest::test eval-12.0 {expand let, experimental code} -body {
    #set env [::constcl::Environment new #NIL {} ::constcl::global_env]
    set ::constcl::env ::constcl::global_env
    pep "(define varlist '(a b c))"
    pep "(define body    '((+ a b) (* c 4)))"
    pep "(define vallist '(1 2 3))"
    pxp "`((lambda (,@varlist) ,@body) ,@vallist)"
} -output "((lambda (a b c) (+ a b) (* c 4)) 1 2 3)\n"

TT)

CB
proc ::constcl::scheme-report-environment {version} {
    # TODO
}
CB

CB
proc ::constcl::null-environment {version} {
    # TODO
}
CB

CB
proc ::constcl::interaction-environment {} {
    # TODO
}
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
