
MD(
## Evaluation

The second thing an interpreter must be able to do is to reduce expressions to
their **normal form**, or **evaluate** them. As an example, 2 + 6 and 8 are two
expressions that have the same value, but the latter is in normal form (can't be
reduced further) and the former is not.

### Syntactic forms

There are nine diffent forms or classes of expressions in Lisp.

TblSynForms

MD)

MD(
__eval__

The heart of the Lisp interpreter, `eval` takes a Lisp expression and processes
it according to its syntactic form.

`eval` also does two kinds of rewriting of expressions:

1. **macro expansion** on a non-atomic expression into a more concrete expression. See the part about macros[#](https://github.com/hoodiecrow/ConsTcl#macros) below, and
2. resolving **local defines**, acting on expressions of the form `(begin (define ...` when in a local environment. See the part about resolving local defines[#](https://github.com/hoodiecrow/ConsTcl#resolving-local-defines).
MD)

PR(
eval (public);expr expr env env -> val
PR)

CB
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
CB

MD(

#### Variable reference

**Example: `r` => 10 (a symbol `r` is evaluated to 10)**

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
#### Constant literal

**Example: `99` => 99 (a number evaluates to itself)**

Not just numbers but booleans, characters, strings, and vectors evaluate to
themselves, to their innate value. Because of this, they are called autoquoting
types (see next paragraph).
MD)

MD(
#### Quotation

**Example: `(quote r)` => `r` (quotation makes the symbol evaluate to itself, like a
constant)**

According to the rules of variable reference, a symbol evaluates to its stored
value. Well, sometimes one wishes to use the symbol itself as a value. That is
what quotation is for. `(quote x)` evaluates to the symbol x itself and not to
any value that might be stored under it. This is so common that there is a
shorthand notation for it: `'x` is interpreted as `(quote x)` by the Lisp
reader.
MD)

MD(
#### Conditional

**Example: `(if (> 99 100) (+ 2 2) (+ 2 4))` => 6**

The conditional form `if` evaluates a Lisp list of three expressions. The first,
the **condition**, is evaluated first. If it evaluates to anything other than `#f`
(false), the second expression (the **consequent**) is evaluated and the value
returned. Otherwise, the third expression (the **alternate**) is evaluated and the
value returned. One of the two latter expressions will be evaluated, and the
other will remain unevaluated.

__/if__
MD)

PR(
/if (internal);condition expr consequent expr alternate expr -> val
PR)

CB
proc ::constcl::/if {cond conseq altern} {
  if {[uplevel $cond] ne "#f"} {
    uplevel $conseq
  } {
    uplevel $altern
  }
}
CB

MD(
#### Sequence

**Example: `(begin (define r 10) (+ r r))` => 20**

When expressions are evaluated in sequence, the order is important for two
reasons. If the expressions have any side effects, they happen in the same order
of sequence. Also, if expressions are part of a pipeline of calculations, then
they need to be processed in the order of that pipeline. The `/begin` helper
procedure takes a Lisp list of expressions and evaluates them in sequence,
returning the value of the last one.

__/begin__
MD)

PR(
/begin (internal);exps lexprs env env -> val
PR)

CB
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
CB

MD(
#### Definition

**Example: `(define r 10)` => ... (a definition doesn't evaluate to anything)**

We've already seen the relationship between symbols and values. A symbol is
bound to a value (or rather to the location the value is in), creating a
variable, through definition. The `/define` helper procedure adds a variable to
the current environment. It first checks that the symbol name is a valid
identifier, then it updates the environment with the new binding.

__/define__
MD)

PR(
/define (internal);sym sym val val env env -> none
PR)

CB
proc ::constcl::/define {sym val env} {
  varcheck [idcheck [$sym name]]
  $env set $sym $val
  return
}
CB

MD(
#### Assignment

**Example: `(set! r 20)` => 20 (`r` is a bound symbol, so it's allowed to assign
to it)**

Once a variable has been created, the value at the location it is bound to can
be changed (hence the name "variable", something that can be modified). The
process is called assignment. The `/set!` helper does assignment: it modifies
an existing variable that is bound somewhere in the environment chain. It finds
the variable's environment and updates the binding. It returns the value, so
calls to `set!` can be chained: `(set! foo (set! bar 99))` sets both variables
to 99. By Scheme convention, procedures that modify variables have "!" at the
end of their name.

__/set!__
MD)

PR(
/set! (internal);var bsym val val env env -> val
PR)

CB
proc ::constcl::/set! {var val env} {
  [$env find $var] set $var $val
  set val
}
CB

MD(
#### Procedure definition

**Example: `(lambda (r) (+ r r))` => ::oo::Obj3601 (it will be a different object
each time)**

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
MD)

PR(
/lambda (internal);formals formals body lexprs env env -> proc
PR)

CB
proc ::constcl::/lambda {formals body env} {
  if {[[length $body] value] > 1} {
    set body [cons [S begin] $body]
  } else {
    set body [car $body]
  }
  return [MkProcedure $formals $body $env]
}
CB

MD(
#### Procedure call

**Example: `(+ 1 6)` => 7**

Once we have procedures, we can call them to have their calculations performed
and yield results. The procedure name is put in the operator position at the
front of a list, and the operands follow in the rest of the list. Our `square`
procedure would be called for instance like this: `(square 11)`, and it will
return 121.

`invoke` arranges for a procedure to be called with each of the values in
the **argument list** (the list of operands). It checks if pr really is a
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
::tcltest::test eval-1.0 {try triggering a check} -body {
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
  # don't convert to /if, it breaks (fact 100)
  if {[pair? $exps] ne "#f"} {
    return [cons [eval [car $exps] $env] \
      [eval-list [cdr $exps] $env]]
  } {
    return #NIL
  }
}
CB

TT(

::tcltest::test eval-2.0 {lambda parameter lists} -body {
    pew {((lambda (x y z) (list x y z)) 3 4 5)}
    pew {((lambda x x) 3 4 5 6)}
    pew {((lambda (x y . z) (list x y)) 3 4 5 6)}
    pew {((lambda (x y . z) z) 3 4 5 6)}
} -output "(3 4 5)\n(3 4 5 6)\n(3 4)\n(5 6)\n"


::tcltest::test eval-3.0 {conditional: does internal if accept a #t? Yes, b/c of the ne in condition handling} -body {
    pew "(if (zero? 0) (* 4 4) (- 5 5))"
} -output "16\n"

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
