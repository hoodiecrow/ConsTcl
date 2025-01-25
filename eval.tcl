
MD(
## eval

The heart of the Lisp interpreter, `eval` takes a Lisp expression and processes it according to its form.

| Syntactic form | Syntax | Semantics |
|----------------|--------|-----------|
| [variable reference](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.1) | _variable_ | An expression consisting of a identifier is a variable reference. It evaluates to the value the identifier is bound to. An unbound identifier can't be evaluated. Example: `r` ⇒ 10 if _r_ is bound to 10 |
| [constant literal](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.2) | _number_ or _boolean_ | Numerical and boolean constants evaluate to themselves. Example: `99` ⇒ 99 |
| [quotation](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.2) | __quote__ _datum_ | (__quote__ _datum_) evaluates to _datum_, making it a constant. Example: `(quote r)` ⇒ r
| [sequence](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.3) | __begin__ _expression_... | The _expression_ s are evaluated sequentially, and the value of the last <expression> is returned. Example: `(begin (define r 10) (* r r))` ⇒ the square of 10 |
| [conditional](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.5) | __if__ _test_ _conseq_ _alt_ | An __if__ expression is evaluated like this: first, _test_ is evaluated. If it yields a true value, then _conseq_ is evaluated and its value is returned. Otherwise _alt_ is evaluated and its value is returned. Example: `(if (> 99 100) (* 2 2) (+ 2 4))` ⇒ 6 |
| [definition](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-8.html#%_sec_5.2) | __define__ _identifier_ _expression_ | A definition binds the _identifier_ to the value of the _expression_. A definition does not evaluate to anything. Example: `(define r 10)` ⇒ |
| [assignment](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.6) | __set!__ _variable_ _expression_ | _Expression_ is evaluated, and the resulting value is stored in the location to which _variable_ is bound. It is an error to assign to an unbound _identifier_. Example: `(set! r 20)` ⇒ 20 |
| [procedure definition](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.4) | __lambda__ _formals_ _body_ | _Formals_ is a list of identifiers. _Body_ is zero or more expressions. A __lambda__ expression evaluates to a Procedure object. Example: `(lambda (r) (* r r))` ⇒ ::oo::Obj3601 |
| [procedure call](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.3) | _operator_ _operand_... | If _operator_ is anything other than __quote__, __begin__, __if__, __define__, __set!__, or __lambda__, it is treated as a procedure. Evaluate _operator_ and all the _operands_, and then the resulting procedure is applied to the resulting list of argument values. Example: `(sqrt (+ 4 12))` ⇒ 4.0 |

The evaluator also does a simple form of macro expansion on `op` and `args` before processing them in the big `switch`. 
See the part about [macros](https://github.com/hoodiecrow/ConsTcl#macros) below.

MD)

CB
reg eval ::constcl::eval

proc ::constcl::eval {e {env ::global_env}} {
    # TODO
    if {[atom? $e] eq "#t"} {
        if {[symbol? $e] eq "#t"} {
            return [lookup $e $env]
        } elseif {[null? $e] eq "#t" || [atom? $e] eq "#t"} {
            return $e
        } else {
            error "cannot evaluate $e"
        }
    } else {
        set op [car $e]
        set args [cdr $e]
        while {[$op name] in {and case cond for for/and for/list for/or let or}} {
            expand-macro op args $env
        }
        switch [$op name] {
            quote {
                return [car $args]
            }
            if {
                if {[eval [car $args] $env] ne "#f"} {
                    return [eval [cadr $args] $env]
                } else {
                    return [eval [caddr $args] $env]
                }
            }
            begin {
                return [eprogn $args $env]
            }
            define {
                return [declare [car $args] [eval [cadr $args] $env] $env]
            }
            set! {
                return [update! [car $args] [eval [cadr $args] $env] $env]
            }
            lambda {
                return [make-function [car $args] [cdr $args] $env]
            }
            default {
                return [invoke [eval $op $env] [evlis $args $env]]
            }
        }
    }
}
CB

MD(
Variable reference, or _lookup_, is handled by the helper `lookup`. It searches the
environment chain for the symbol's name, and returns the value it is bound to.
MD)

CB
proc ::constcl::lookup {sym env} {
    set name [$sym name]
    [$env find $name] get $name
}
CB

MD(
The _conditional_ form evaluates a Lisp list of three expressions. The first, the _condition_,
is evaluated first. If it evaluates to anything other than `#f`, the second expression (the
_consequent_) is evaluated and the value returned. Otherwise, the third expression (the 
_alternate_) is evaluated and the value returned.
MD)

MD(
The `eprogn` helper procedure takes a Lisp list of expressions and evaluates them in
_sequence_, returning the value of the last one.
MD)

CB
proc ::constcl::eprogn {exps env} {
    if {[pair? $exps] eq "#t"} {
        if {[pair? [cdr $exps]] eq "#t"} {
            eval [car $exps] $env
            return [eprogn [cdr $exps] $env]
        } else {
            return [eval [car $exps] $env]
        }
    } else {
        return #NIL
    }
}
CB

MD(
The `declare` helper adds a variable to the current environment. It first checks that the
symbol name is a valid identifier, then it updates the environment with the new binding.
MD)

CB
proc ::constcl::declare {sym val env} {
    set var [varcheck [idcheck [$sym name]]]
    $env set $var $val
    return #NONE
}
CB

MD(
The `update!` helper modifies an existing variable that is bound somewhere in the 
environment chain. It finds the variable's environment and updates the binding. It
returns the expression, so calls to `set!` can be chained: `(set! foo (set! bar 99))`
sets both variables to 99.
MD)

CB
proc ::constcl::update! {var expr env} {
    [$env find [$var name]] set [$var name] $expr
    set expr
}
CB

MD(
`make-function` makes a [Procedure](https://github.com/hoodiecrow/ConsTcl#control) object. First it needs to convert the Lisp lists
`formals` and `body`. The former is reworked to a Tcl list of parameter names, and 
the latter is packed inside a `begin` if it has more than one expression, and taken
out of its list if not.
MD)

CB
proc ::constcl::make-function {formals body env} {
    set parms [lmap formal [splitlist $formals] {$formal name}]
    if {[[length $body] value] > 1} {
        set body [cons #B $body]
    } else {
        set body [car $body]
    }
    return [MkProcedure $parms $body $env]
}
CB

MD(
`invoke` arranges for a procedure to be called with a Tcl list of Lisp values. It checks
if `pr`really is a procedure, and determines whether to call `pr` as an object or as a Tcl
command.
MD)

CB
proc ::constcl::invoke {pr vals} {
    if {[procedure? $pr] eq "#t"} {
        if {[info object isa object $pr]} {
            $pr call {*}[splitlist $vals]
        } else {
            $pr {*}[splitlist $vals]
        }
    } else {
        error "PROCEDURE expected\n" ; #([$pr write] [$vals write])"
    }
}
CB

MD(
`splitlist` converts a Lisp list to a Tcl list with Lisp objects.
MD)

CB
proc ::constcl::splitlist {vals} {
    set result {}
    while {[pair? $vals] eq "#t"} {
        lappend result [car $vals]
        set vals [cdr $vals]
    }
    return $result
}
CB

MD(
`evlis` successively evaluates the elements of a Lisp list and returns the results
as a Lisp list.
MD)

CB
proc ::constcl::evlis {exps env} {
    if {[pair? $exps] eq "#t"} {
        return [cons [eval [car $exps] $env] [evlis [cdr $exps] $env]]
    } else {
        return #NIL
    }
}
CB

MD(
### Macros

Macros that rewrite expressions into other, more concrete expressions is one of Lisp's strong
points. This interpreter does macro expansion, but the user can't define new macros--the ones
available are hardcoded in the code below.
MD)

CB
proc ::constcl::expand-macro {n1 n2 env} {
    upvar $n1 op $n2 args
    switch [$op name] {
        and {
            set p [expand-and $args]
        }
        case {
            set p [do-case [car $args] [cdr $args]]
        }
        cond {
            set p [do-cond $args]
        }
        for {
            set p [expand-for $args $env]
        }
        for/and {
            set p [expand-for/and $args $env]
        }
        for/list {
            set p [expand-for/list $args $env]
        }
        for/or {
            set p [expand-for/or $args $env]
        }
        let {
            set p [expand-let $args]
        }
        or {
            set p [expand-or $args]
        }
    }
    set op [car $p]
    set args [cdr $p]
}
CB

MD(
`expand-and` expands the `and` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.
MD)

CB
proc ::constcl::expand-and {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return [list #B #t]
    } elseif {[eq? [length $exps] #1] eq "#t"} {
        return [cons #B $exps]
    } else {
        return [do-and $exps #NIL]
    }
}

proc ::constcl::do-and {exps prev} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return $prev
    } else {
        return [list #I [car $exps] [do-and [cdr $exps] [car $exps]] #f]
    }
}
CB

MD(
The `case` macro is expanded by `do-case`. It returns `'()` if there are no clauses, 
and nested `if` constructs if there are some.
MD)

CB
proc ::constcl::do-case {keyexpr clauses} {
    if {[eq? [length $clauses] #0] eq "#t"} {
        return [list #Q #NIL]
    } elseif {[eq? [length $clauses] #1] eq "#t"} {
        set keyl [caar $clauses]
        set body [cdar $clauses]
        if {[eq? $keyl [MkSymbol "else"]] eq "#t"} {
            set keyl #t
        } else {
            set keyl [list [MkSymbol "memv"] $keyexpr [list #Q $keyl]]
        }
        return [list #I $keyl [cons #B $body] [do-case $keyexpr [cdr $clauses]]]
    } else {
        set keyl [caar $clauses]
        set body [cdar $clauses]
        set keyl [list [MkSymbol "memv"] $keyexpr [list #Q $keyl]]
        return [list #I $keyl [cons #B $body] [do-case $keyexpr [cdr $clauses]]]
    }
}
CB

MD(
The `cond` macro is expanded by `do-cond`. It returns `'()` if there are no clauses, 
and nested `if` constructs if there are some.
MD)

CB
proc ::constcl::do-cond {clauses} {
    if {[eq? [length $clauses] #0] eq "#t"} {
        return [list #Q #NIL]
    } elseif {[eq? [length $clauses] #1] eq "#t"} {
        set pred [caar $clauses]
        set body [cdar $clauses]
        if {[eq? $pred [MkSymbol "else"]] eq "#t"} {
            set pred #t
        }
        if {[null? $body] eq "#t"} {set body $pred}
        return [list #I $pred [cons #B $body] [do-cond [cdr $clauses]]]
    } else {
        set pred [caar $clauses]
        set body [cdar $clauses]
        if {[null? $body] eq "#t"} {set body $pred}
        return [list #I $pred [cons #B $body] [do-cond [cdr $clauses]]]
    }
}
CB

MD(
The `expand-for` procedure expands the `for` macro. It returns a `begin`
construct containing the iterations of the first clause (multiple clauses
isn't implemented yet).
MD)

CB
proc ::constcl::do-for {exps env} {
    #single-clause
    set clauses [car $exps]
    set body [cdr $exps]
    set clause [car $clauses]
    set id [car $clause]
    set seq [cadr $clause]
    if {[number? $seq] eq "#t"} {
        set seq [in-range $seq]
    } else {
        set seq [eval $seq $env]
        if {[list? $seq] eq "#t"} {
            set seq [splitlist $seq]
        } elseif {[string? $seq] eq "#t"} {
            set seq [lmap c [split [$seq value] {}] {MkChar #\\$c}]
        } elseif {[vector? $seq] eq "#t"} {
            set seq [$seq value]
        }
    }
    set res {}
    foreach v $seq {
        lappend res [list #L [list [list $id $v]] {*}[splitlist $body]]
    }
    return $res
}

proc ::constcl::expand-for {exps env} {
    set res [do-for $exps $env]
    lappend res [list #Q #NIL]
    return [list #B {*}$res]
}
CB

MD(
The `expand-for/and` procedure expands the `for/and` macro. It returns an `and`
construct containing the iterations of the first clause (multiple clauses
isn't implemented yet).
MD)

CB
proc ::constcl::expand-for/and {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "and"] {*}$res]
}
CB

MD(
The `expand-for/list` procedure expands the `for/list` macro. It returns a `list`
construct containing the iterations of the first clause (multiple clauses
isn't implemented yet).
MD)

CB
proc ::constcl::expand-for/list {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "list"] {*}$res]
}
CB

MD(
The `expand-for/or` procedure expands the `for/or` macro. It returns an `or`
construct containing the iterations of the first clause (multiple clauses
isn't implemented yet).
MD)

CB
proc ::constcl::expand-for/or {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "or"] {*}$res]
}
CB

MD(
`expand-let` expands the named `let` and 'regular' `let` macros. The ultimately
expand to ´lambda` constructs.
MD)

CB
proc ::constcl::expand-let {exps} {
    if {[symbol? [car $exps]] eq "#t"} {
        # named let
        set variable [car $exps]
        set bindings [cadr $exps]
        set body [cddr $exps]
        set vars [dict create $variable #f]
        foreach binding [splitlist $bindings] {
            set var [car $binding]
            set val [cadr $binding]
            if {$var in [dict keys $vars]} {error "variable '$var' occurs more than once in let construct"}
            dict set vars $var $val
        }
        set decl [dict values [dict map {k v} $vars {list $k $v}]]
        set func [list #λ [list {*}[lrange [dict keys $vars] 1 end]] {*}[splitlist $body]]
        set call [list $variable {*}[lrange [dict keys $vars] 1 end]]
        return [list #L [list {*}$decl] [list #S $variable $func] $call]
    } else {
        # regular let
        set bindings [car $exps]
        set body [cdr $exps]
        set vars [dict create]
        foreach binding [splitlist $bindings] {
            set var [car $binding]
            set val [cadr $binding]
            if {$var in [dict keys $vars]} {error "variable '$var' occurs more than once in let construct"}
            dict set vars $var $val
        }
        return [list [list #λ [list {*}[dict keys $vars]] [cons #B $body]] {*}[dict values $vars]]
    }
}
CB

MD(
`expand-or` expands the `or` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.
MD)

CB
proc ::constcl::expand-or {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return [list #B #f]
    } elseif {[eq? [length $exps] #1] eq "#t"} {
        return [cons #B $exps]
    } else {
        return [do-or $exps]
    }
}

proc ::constcl::do-or {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return #f
    } else {
        return [list #L [list [list #x [car $exps]]] [list #I #x #x [do-or [cdr $exps]]]]
    }
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
} -output "((lambda (x) (begin (* x x))) 10)\n((lambda (x y) (begin (* x y))) 10 5)\n"

::tcltest::test eval-1.4 {expand named let macro} -body {
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
} -output "3\n"

::tcltest::test eval-2.2 {expand cond macro} -body {
    pxp "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)))"
} -output "(if (> 3 4) (begin (+ 4 2)) (if (> 1 2) (begin (+ 5 5)) (quote ())))\n"

::tcltest::test eval-2.3 {run cond macro} -body {
    pep "(cond ((> 3 4) (+ 4 2)) ((> 1 2) (+ 5 5)))"
} -output "()\n"

::tcltest::test eval-2.4 {expand cond macro} -body {
    pxp "(cond ((> 3 4) (+ 4 2) (+ 3 5)) ((> 1 2) (+ 5 5)))"
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
    pep "(for ((i (quote (1 2 3)))) (write i))"
} -result "" -output 1\n2\n3\n()\n

::tcltest::test eval-4.2 {expand for macro} -body {
    pxp "(for ((i 4)) (display i))"
} -output "(begin (let ((i 0)) (display i)) (let ((i 1)) (display i)) (let ((i 2)) (display i)) (let ((i 3)) (display i)) (quote ()))\n"

::tcltest::test eval-4.3 {run for macro} -body {
    pep "(for ((i 4)) (write i))"
} -result "" -output "0\n1\n2\n3\n()\n"

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

::tcltest::test macro-5.4 {expand for/list macro} -constraints knownBug -body { ;# bug: in-range can't accept Lisp values
    pxp {(for/list ([i (in-range 1 4)]) (* i i))}
} -output "\n"

::tcltest::test macro-5.5 {run for/list macro} -constraints knownBug -body {
    pep {(for/list ([i (in-range 1 4)]) (* i i))}
} -result "(1 4 9)"


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

