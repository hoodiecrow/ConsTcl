
MD(
## eval

The heart of the Lisp interpreter, `eval` takes a Lisp expression and processes it according to its form.

| Syntactic form | Syntax | Semantics |
|----------------|--------|-----------|
| variable reference | *variable* | An expression consisting of an identifier is a variable reference. It evaluates to the value the identifier is bound to. An unbound identifier can't be evaluated. Example: `r` ⇒ 10 if _r_ is bound to 10 |
| constant literal | *number* or *boolean*, etc | Constants evaluate to themselves. Example: `99` ⇒ 99 |
| quotation | __quote__ *datum* | (__quote__ *datum*) evaluates to *datum*, making it a constant. Example: `(quote r)` ⇒ r
| sequence | __begin__ *expression*... | The *expressions* are evaluated sequentially, and the value of the last *expression* is returned. Example: `(begin (define r 10) (* r r))` ⇒ the square of 10 |
| conditional | __if__ *test* *conseq* *alt* | An __if__ expression is evaluated like this: first, *test* is evaluated. If it yields a true value, then *conseq* is evaluated and its value is returned. Otherwise *alt* is evaluated and its value is returned. Example: `(if (> 99 100) (* 2 2) (+ 2 4))` ⇒ 6 |
| definition | __define__ *identifier* *expression* | A definition binds the *identifier_ to the location where the value of the *expression* is stored. A definition does not evaluate to anything. Example: `(define r 10)` ⇒ |
| assignment | __set!__ *variable* *expression* | *Expression* is evaluated, and the resulting value is stored in the location to which *variable* is bound. It is an error to assign to an unbound *identifier*. Example: `(set! r 20)` ⇒ 20 |
| procedure definition | __lambda__ *formals* *body* | *Formals* is a list of identifiers. *Body* is zero or more expressions. A __lambda__ expression evaluates to a Procedure object. Example: `(lambda (r) (* r r))` ⇒ ::oo::Obj3601 |
| procedure call | *operator* *operand*... | If *operator* is anything other than __quote__, __begin__, __if__, __define__, __set!__, or __lambda__, it is treated as a procedure. Evaluate *operator* and all the *operands*, and then the resulting procedure is applied to the resulting list of argument values. Example: `(sqrt (+ 4 12))` ⇒ 4.0 |


MD)

MD(
**eval**

`eval` processes an _expression_ to get a _value_. The exact method depends on
the form of expression, see above.

The evaluator also does a simple form of macro expansion on `op` and `args` (the
car and cdr of the expression) before processing them in the big `switch`. See
the part about macros[#](https://github.com/hoodiecrow/ConsTcl#macros) below.

The evaluator also resolves local defines, acting on expressions of the form
"(begin (define ..." when the environment is other than the global one. See the
part about resolving local
defines[#](https://github.com/hoodiecrow/ConsTcl#resolving-local-defines).
MD)

PR(
eval (public);expr expr env env -> val
PR)

CB
reg eval ::constcl::eval

proc ::constcl::eval {expr {env ::constcl::global_env}} {
    ::if {[symbol? $expr] ne "#f"} {
        lookup $expr $env
    } elseif {[null? $expr] ne "#f" || [atom? $expr] ne "#f"} {
        set expr
    } else {
        set op [car $expr]
        set args [cdr $expr]
        while {[$op name] in {
            and case cond define del! for for/and for/list for/or
            let or pop! push! put! quasiquote unless when}} {
                expand-macro $env
        }
        ::if {$env ne "::constcl::global_env" && [$op name] eq "begin" &&
            ([pair? [car $args]] ne "#f" && [[caar $args] name] eq "define")} {
            set expr [resolve-local-defines $args]
            set op [car $expr]
            set args [cdr $expr]
        }
        switch [$op name] {
            quote   { car $args }
            if      { ::if {[eval [car $args] $env] ne "#f"} \
                        {eval [cadr $args] $env} \
                        {eval [caddr $args] $env} }
            begin   { eprogn $args $env }
            define  { declare [car $args] [eval [cadr $args] $env] $env }
            set!    { update! [car $args] [eval [cadr $args] $env] $env }
            lambda  { make-function [car $args] [cdr $args] $env }
            default { invoke [eval $op $env] [eval-list $args $env] }
        }
    }
}
CB

MD(
**lookup**

_Variable reference_ is handled by the helper `lookup`. It searches the
environment chain for the symbol's name, and returns the value it is bound to.
It is an error to lookup an unbound symbol.
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
**if**

The _conditional_ form evaluates a Lisp list of three expressions. The first,
the _condition_, is evaluated first. If it evaluates to anything other than
`#f`, the second expression (the _consequent_) is evaluated and the value
returned. Otherwise, the third expression (the _alternate_) is evaluated and the
value returned.
MD)

PR(
if (internal);condition expr consequent expr alternate expr -> val
PR)

CB
proc ::constcl::if {cond conseq altern} {
    ::if {[uplevel $cond] ne "#f"} {uplevel $conseq} {uplevel $altern}
}
CB

MD(
**eprogn**

The `eprogn` helper procedure takes a Lisp list of expressions and evaluates them in
_sequence_, returning the value of the last one.
MD)

PR(
eprogn (internal);exps lexprs env env -> val
PR)

CB
proc ::constcl::eprogn {exps env} {
    if {pair? $exps} {
        if {pair? [cdr $exps]} {
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
**declare**

The `declare` helper adds a variable to the current environment. It first checks that the
symbol name is a valid identifier, then it updates the environment with the new binding.
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
**update!**

The `update!` helper does _assignment_: it modifies an existing variable that is bound
somewhere in the environment chain. It finds the variable's environment and updates the
binding. It returns the value, so calls to `set!` can be chained: `(set! foo (set! bar 99))`
sets both variables to 99.
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
**make-function**

`make-function` makes a Procedure[#](https://github.com/hoodiecrow/ConsTcl#control)
object. First it needs to convert the Lisp list `body`. It is packed inside a `begin`
if it has more than one expression, and taken out of its list if not. The Lisp list
`formals` is passed on as is.

A Scheme formals list is either:

* An *empty list*, `()`, meaning that no arguments are accepted,
* A *proper list*, `(a b c)`, meaning it accepts three arguments, one in each symbol,
* A *symbol*, `a`, meaning that all arguments go into `a`, or
* A *dotted list*, `(a b . c)`, meaning that two arguments go into `a` and `b`, and the rest into `c`.
MD)

PR(
make-function (internal);formals formals body lexprs env env -> proc
PR)

CB
proc ::constcl::make-function {formals body env} {
    ::if {[[length $body] value] > 1} {
        set body [cons #B $body]
    } else {
        set body [car $body]
    }
    return [MkProcedure $formals $body $env]
}
CB

MD(
**invoke**

`invoke` arranges for a procedure to be called with each of the values in _vals_. It checks if
_pr_ really is a procedure, and determines whether to call _pr_ as an object or as a Tcl command.
MD)

PR(
invoke (internal);pr proc vals lvals -> invoke
PR)

CB
proc ::constcl::invoke {pr vals} {
    check {procedure? $pr} {PROCEDURE expected\n([$pr show] val ...)}
    ::if {[info object isa object $pr]} {
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
**splitlist**

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
**eval-list**

`eval-list` successively evaluates the elements of a Lisp list and returns the results
as a Lisp list.
MD)

PR(
eval-list (internal);exps lexprs env env -> lvals
PR)

CB
proc ::constcl::eval-list {exps env} {
    # don't convert to ::constcl::if, it breaks (fact 100)
    ::if {[pair? $exps] ne "#f"} {
        return [cons [eval [car $exps] $env] [eval-list [cdr $exps] $env]]
    } {
        return #NIL
    }
}
CB

MD(
### Macros

**expand-macro**

Macros that rewrite expressions into other, more concrete expressions is one of
Lisp's strong points. This interpreter does macro expansion, but the user can't
define new macros--the ones available are hardcoded in the code below.

`expand-macro` only takes the environment as a parameter, but internally it uses
variable sharing to get the expression it is to process. It shares the variables
`op` and `args` with its caller, `eval`. `op` is used to delegate to the correct
expansion procedure, and the value of `args` is passed to the expansion
procedures. In the end, the expanded expression is passed back to `eval` by
assigning to `op` and `args`.
MD)

PR(
expand-macro (internal);env env -> nil
PR)

CB
proc ::constcl::expand-macro {env} {
    upvar op op args args
    ::if {[$op name] eq "define" && ([pair? [car $args]] eq "#f" || [[caar $args] name] eq "lambda")} {
        return -code break
    }
    switch [$op name] {
        and {
            set expr [expand-and $args $env]
        }
        case {
            set expr [expand-case [car $args] [cdr $args]]
        }
        cond {
            set expr [expand-cond $args]
        }
        define {
            set expr [expand-define $args $env]
        }
        del! {
            set expr [expand-del! $args $env]
        }
        for {
            set expr [expand-for $args $env]
        }
        for/and {
            set expr [expand-for/and $args $env]
        }
        for/list {
            set expr [expand-for/list $args $env]
        }
        for/or {
            set expr [expand-for/or $args $env]
        }
        let {
            set expr [expand-let $args $env]
        }
        or {
            set expr [expand-or $args $env]
        }
        pop! {
            set expr [expand-pop! $args $env]
        }
        push! {
            set expr [expand-push! $args $env]
        }
        put! {
            set expr [expand-put! $args $env]
        }
        quasiquote {
            set expr [expand-quasiquote $args $env]
        }
        unless {
            set expr [expand-unless $args $env]
        }
        when {
            set expr [expand-when $args $env]
        }
    }
    set op [car $expr]
    set args [cdr $expr]
    return #NIL
}
CB

MD(
**expand-and**

`expand-and` expands the `and` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.
MD)

PR(
expand-and (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-and {tail env} {
    if {eq? [length $tail] #0} {
        return [list #B #t]
    } {
        if {eq? [length $tail] #1} {
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
    if {eq? [length $tail] #0} {
        return $prev
    } {
        $env setstr "first" [car $tail]
        $env setstr "rest" [do-and [cdr $tail] [car $tail] $env]
        set qq "`(if ,first ,rest #f)"
        return [expand-quasiquote [cdr [parse $qq]] $env]
    }
}
CB

MD(
**expand-case**

The `case` macro is expanded by `expand-case`. It returns `'()` if there are no clauses (left), 
and nested `if` constructs if there are some.
MD)

PR(
expand-case (internal);keyexpr expr clauses lvals -> expr
PR)

CB
proc ::constcl::expand-case {keyexpr clauses} {
    ::if {[eq? [length $clauses] #0] ne "#f"} {
        return [list #Q #NIL]
    } else {
        set keyl [caar $clauses]
        set body [cdar $clauses]
        set keyl [list [MkSymbol "memv"] $keyexpr [list #Q $keyl]]
        ::if {[eq? [length $clauses] #1] ne "#f"} {
            ::if {[eq? [caar $clauses] [MkSymbol "else"]] ne "#f"} {
                set keyl #t
            }
        }
        return [list #I $keyl [cons #B $body] [expand-case $keyexpr [cdr $clauses]]]
    }
}
CB

MD(
**expand-cond**

The `cond` macro is expanded by `expand-cond`. It returns `'()` if there are no
clauses (left), and nested `if` constructs if there are some.

MD)

PR(
expand-cond (internal);clauses lvals -> expr
PR)

CB
proc ::constcl::expand-cond {clauses} {
    ::if {[eq? [length $clauses] #0] ne "#f"} {
        return [list #Q #NIL]
    } else {
        set pred [caar $clauses]
        set body [cdar $clauses]
        ::if {[symbol? [car $body]] ne "#f" && [[car $body] name] eq "=>"} {
            set body [cddar $clauses]
        }
        ::if {[eq? [length $clauses] #1] ne "#f"} {
            ::if {[eq? $pred [MkSymbol "else"]] ne "#f"} {
                set pred #t
            }
        }
        ::if {[null? $body] ne "#f"} {set body $pred}
        return [list #I $pred [cons #B $body] [expand-cond [cdr $clauses]]]
    }
}
CB

MD(
**expand-define**

`define` has two variants, one of which requires some rewriting. It's the one
with an implied `lambda` call, the one that defines a procedure. 

(__define__ (_symbol_ _formals_) _body_)

is transformed into

(__define__ _symbol_ (__lambda__ _formals_ _body_))

which conforms better to `eval`'s standard of (__define__ _symbol_ _value_).
MD)

PR(
expand-define (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-define {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    $env setstr "tail" $tail
    set qq "`(define ,(caar tail) (lambda ,(cdar tail) ,@(cdr tail)))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB

MD(
**expand-del!**

The macro `del!` updates a property list. It removes a key-value pair if the key
is present, or leaves the list untouched if it isn't.
MD)

PR(
expand-del! (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-del! {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    ::if {[null? $tail] ne "#f"} {::error "too few arguments, 2 expected, got 0"}
    $env setstr "listname" [car $tail]
    ::if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments, 2 expected, got 1"}
    $env setstr "key" [cadr $tail]
    set qq "`(set! ,listname (delete! ,listname ,key))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB

MD(
**expand-for**

The `expand-for` procedure expands the `for` macro. It returns a `begin`
construct containing the iterations of each clause (multiple clauses
weren't implemented, but I brought up my strongest brain cells and they
did it).
MD)

PR(
for-seq (internal);seq val env env -> tvals
PR)

CB
proc ::constcl::for-seq {seq env} {
    ::if {[number? $seq] ne "#f"} {
        set seq [in-range $seq]
    } else {
        set seq [eval $seq $env]
    }
    # make it a Tcl list, one way or another
    ::if {[list? $seq] ne "#f"} {
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
expand-for (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-for {tail env} {
    set res [do-for $tail $env]
    lappend res [list #Q #NIL]
    return [list #B {*}$res]
}
CB

MD(
**expand-for/and**

The `expand-for/and` procedure expands the `for/and` macro. It returns an `and`
construct containing the iterations of the clauses.
MD)

PR(
expand-for/and (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-for/and {tail env} {
    set res [do-for $tail $env]
    return [list [MkSymbol "and"] {*}$res]
}
CB

MD(
**expand-for/list**

The `expand-for/list` procedure expands the `for/list` macro. It returns a `list`
construct containing the iterations of each clause.
MD)

PR(
expand for/list (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-for/list {tail env} {
    set res [do-for $tail $env]
    return [list [MkSymbol "list"] {*}$res]
}
CB

MD(
**expand-for/or**

The `expand-for/or` procedure expands the `for/or` macro. It returns an `or`
construct containing the iterations of each clause.
MD)

PR(
expand-for/or (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-for/or {tail env} {
    set res [do-for $tail $env]
    return [list [MkSymbol "or"] {*}$res]
}
CB

MD(
**expand-let**

`expand-let` expands the named `let` and 'regular' `let` macros. They ultimately
expand to `lambda` constructs.
MD)

PR(
expand-let (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-let {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    ::if {[symbol? [car $tail]] ne "#f"} {
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
        return [expand-quasiquote [cdr [parse $qq]] $env]
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
        return [expand-quasiquote [cdr [parse $qq]] $env]
    }
}

proc ::constcl::parse-bindings {name bindings} {
    upvar $name vars
    foreach binding [splitlist $bindings] {
        set var [car $binding]
        set val [cadr $binding]
        ::if {$var in [dict keys $vars]} {::error "variable '$var' occurs more than once in let construct"}
        dict set vars $var $val
    }
}
CB

MD(
**expand-or**

`expand-or` expands the `or` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.
MD)

PR(
expand-or (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-or {tail env} {
    ::if {[eq? [length $tail] #0] ne "#f"} {
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
    if {eq? [length $tail] #0} {
        return #f
    } {
        $env setstr "first" [car $tail]
        $env setstr "rest" [do-or [cdr $tail] $env]
        set qq "`(let ((x ,first)) (if x x ,rest))"
        return [expand-quasiquote [cdr [parse $qq]] $env]
    }
}
CB

MD(
**expand-pop!**

The macro `push!` updates a list. It adds a new element as the new first element.
MD)

PR(
expand-pop! (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-pop! {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    ::if {[null? $tail] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
    $env set [MkSymbol "obj"] [car $tail]
    ::if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
    ::if {[symbol? [cadr $tail]] eq "#f"} {::error "SYMBOL expected:\n(push! obj listname)"}
    $env set [MkSymbol "listname"] [cadr $tail]
    set qq "`(set! ,listname (cdr ,listname))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB

MD(
**expand-push!**

The macro `push!` updates a list. It adds a new element as the new first element.
MD)

PR(
expand-push! (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-push! {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    ::if {[null? $tail] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
    $env set [MkSymbol "obj"] [car $tail]
    ::if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments:\n(push! obj listname)"}
    ::if {[symbol? [cadr $tail]] eq "#f"} {::error "SYMBOL expected:\n(push! obj listname)"}
    $env set [MkSymbol "listname"] [cadr $tail]
    set qq "`(set! ,listname (cons ,obj ,listname))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB

MD(
**expand-put!**

The macro `put!` updates a property list. It adds a key-value pair if the key
isn't present, or changes the value in place if it is.
MD)

PR(
expand-put! (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-put! {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    ::if {[null? $tail] ne "#f"} {::error "too few arguments, 3 expected, got 0"}
    $env set [MkSymbol "listname"] [car $tail]
    ::if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments, 3 expected, got 1"}
    $env set [MkSymbol "key"] [cadr $tail]
    ::if {[null? [cddr $tail]] ne "#f"} {::error "too few arguments, 3 expected, got 2"}
    $env set [MkSymbol "val"] [caddr $tail]
    set qq "`(let ((idx (list-find-key ,listname ,key)))
               (if (< idx 0)
                 (set! ,listname (append (list ,key ,val) ,listname))
                 (begin (list-set! ,listname (+ idx 1) ,val) ,listname)))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB

MD(
**expand-quasiquote**

A quasi-quote isn't a macro, but we will deal with it in this section anyway. `expand-quasiquote`
traverses the quasi-quoted structure searching for `unquote` and `unquote-splicing`. This code is
brittle and sprawling and I barely understand it myself.
MD)

PR(
qq-visit-child (internal);node lexprs qqlevel tnum env env -> texprs
PR)

CB
proc ::constcl::qq-visit-child {node qqlevel env} {
    ::if {$qqlevel < 0} {
        set qqlevel 0
    }
    ::if {[list? $node] ne "#f"} {
        set res {}
        foreach child [splitlist $node] {
            ::if {[pair? $child] ne "#f" && [eq? [car $child] [MkSymbol "unquote"]] ne "#f"} {
                ::if {$qqlevel == 0} {
                    lappend res [eval [cadr $child] $env]
                } else {
                    lappend res [list #U [qq-visit-child [cadr $child] [expr {$qqlevel - 1}] $env]]
                }
            } elseif {[pair? $child] ne "#f" && [eq? [car $child] [MkSymbol "unquote-splicing"]] ne "#f"} {
                ::if {$qqlevel == 0} {
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
expand-quasiquote (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-quasiquote {tail env} {
    set qqlevel 0
    ::if {[list? [car $tail]] ne "#f"} {
        set node [car $tail]
        return [qq-visit-child $node 0 $env]
    } elseif {[vector? [car $tail]] ne "#f"} {
        set vect [car $tail]
        set res {}
        for {set i 0} {$i < [[vector-length $vect] numval]} {incr i} {
            set idx [MkNumber $i]
            set vecref [vector-ref $vect $idx]
            ::if {[pair? $vecref] ne "#f" && [eq? [car $vecref] [MkSymbol "unquote"]] ne "#f"} {
                ::if {$qqlevel == 0} {
                    lappend res [eval [cadr $vecref] $env]
                }
            } elseif {[pair? $vecref] ne "#f" && [eq? [car $vecref] [MkSymbol "unquote-splicing"]] ne "#f"} {
                ::if {$qqlevel == 0} {
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
`unless` is a conditional like `if`, with the differences that it takes a number
of expressions and only executes them for a false outcome of `car $tail`.
MD)

PR(
expand-unless (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-unless {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    $env setstr "tail" $tail
    set qq "`(if ,(car tail) (quote ()) (begin ,@(cdr tail)))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB

MD(
`when` is a conditional like `if`, with the differences that it takes a number
of expressions and only executes them for a true outcome of `car $tail`.
MD)

PR(
expand-when (internal);tail exprtail env env -> expr
PR)

CB
proc ::constcl::expand-when {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    $env setstr "tail" $tail
    set qq "`(if ,(car tail) (begin ,@(cdr tail)) (quote ()))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
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
MD)

PR(
resolve-local-defines;exps lexprs -> expr
PR)

CB
proc ::constcl::resolve-local-defines {exps} {
    set rest [lassign [extract-from-defines $exps VALS] a error]
    ::if {$error ne "#f"} {
        return #NIL
    }
    set rest [lassign [extract-from-defines $exps VARS] v error]
    ::if {$rest eq "#NIL"} {
        set rest [cons #UNSP #NIL]
    }
    return [make-recursive-lambda $v $a $rest]
}
CB

MD(
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
        ::if {[atom? $exps] ne "#f" || [atom? [car $exps]] ne "#f" || [eq? [caar $exps] [MkSymbol "define"]] eq "#f"} {
            break
        }
        set n [car $exps]
        set k [length $n]
        ::if {[list? $n] eq "#f" || [$k numval] < 3 || [$k numval] > 3 ||
            ([argument-list? [cadr $n]] ne "#f" || [symbol? [cadr $n]] eq "#f")
            eq "#f"} {
            return [::list {} "#t" {}]
        }
        ::if {[pair? [cadr $n]] ne "#f"} {
            ::if {$part eq "VARS"} {
                set a [cons [caadr $n] $a]
            } else {
                set a [cons #NIL $a]
                set new [cons [cdadr $n] [cddr $n]]
                set new [cons #λ $new]
                set-car! $a $new
            }
        } else {
            ::if {$part eq "VARS"} {
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
`argument-list?` accepts a Scheme formals list and rejects other values.
MD)

PR(
argument-list? (internal);val val -> bool
PR)

CB
proc ::constcl::argument-list? {val} {
    ::if {$val eq "#NIL"} {
        return #t
    } elseif {[symbol? $val] ne "#f"} {
        return #t
    } elseif {[atom? $val] ne "#f"} {
        return #f
    }
    while {[pair? $val] ne "#f"} {
        ::if {[symbol? [car $val]] eq "#f"} {
            return #f
        }
        set val [cdr $val]
    }
    ::if {$val eq "#NIL"} {
        return #t
    } elseif {[symbol? $val] ne "#f"} {
        return #t
    }
}
CB

MD(
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
`append-b` joins two lists together.
MD)

PR(
append-b (internal);a lvals b lvals -> lvals
PR)

CB
proc ::constcl::append-b {a b} {
    ::if {$a eq "#NIL"} {
        return $b
    }
    set p $a
    while {$p ne "#NIL"} {
        ::if {[atom? $p] ne "#f"} {
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
    ::constcl::write [::constcl::expand-quasiquote [::constcl::cdr [::constcl::parse [::constcl::IB new {`(list ,(+ 1 2) 4)}]]] ::constcl::global_env]
    ::constcl::global_env set [::constcl::list [::constcl::MkSymbol "name"]] a
    pep {(define name 'a)}
    ::constcl::write [::constcl::expand-quasiquote [::constcl::cdr [::constcl::parse [::constcl::IB new {`(list ,name ',name)}]]] ::constcl::global_env]
    ::constcl::write [::constcl::expand-quasiquote [::constcl::cdr [::constcl::parse [::constcl::IB new {`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)}]]] ::constcl::global_env]
    ::constcl::write [::constcl::expand-quasiquote [::constcl::cdr [::constcl::parse [::constcl::IB new {`(( foo ,(- 10 3)) ,@(cdr '(c)) ,(car '(cons)))}]]] ::constcl::global_env]
    ::constcl::write [::constcl::expand-quasiquote [::constcl::cdr [::constcl::parse [::constcl::IB new {`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)}]]] ::constcl::global_env]
if no {
}
} -output "(list 3 4)\n(list a (quote a))\n(a 3 4 5 6 b)\n((foo 7) cons)\n(vector 10 5 2.0 4.0 3.0 8)\n"

::tcltest::test eval-6.2 {quasiquotation} -body {
    ::constcl::write [::constcl::expand-quasiquote [::constcl::cdr [::constcl::parse [::constcl::IB new {`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)}]]] ::constcl::global_env]
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

# vim: ft=tcl tw=80
