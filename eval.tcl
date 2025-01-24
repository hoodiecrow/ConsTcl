
MD(
## Eval
MD)

CB
reg eval ::constcl::eval

proc ::constcl::eval {e {env ::global_env}} {
    # TODO
    if {[atom? $e] eq "#t"} {
        if {[symbol? $e] eq "#t"} {
            return [lookup $e $env]
        } elseif {[null? $e] eq "#t" || [number? $e] eq "#t" || [string? $e] eq "#t" || [char? $e] eq "#t" || [boolean? $e] eq "#t" || [vector? $e] eq "#t"} {
            return $e
        } else {
            error "cannot evaluate $e"
        }
    } else {
        set op [car $e]
        set args [cdr $e]
        while {[$op name] in {and case cond let or}} {
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

CB
proc ::constcl::lookup {sym env} {
    set sym [$sym name]
    [$env find $sym] get $sym
}
CB

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

CB
proc ::constcl::declare {sym val env} {
    set var [varcheck [idcheck [$sym name]]]
    $env set [$sym name] $val
    return #NIL
}
CB

CB
proc ::constcl::update! {var expr env} {
    set var [varcheck [idcheck [$var name]]]
    [$env find $var] set $var $expr
    set expr
}
CB

CB
proc ::constcl::evlis {exps env} {
    if {[pair? $exps] eq "#t"} {
        return [cons [eval [car $exps] $env] [evlis [cdr $exps] $env]]
    } else {
        return #NIL
    }
}
CB

CB
proc ::constcl::invoke {pr vals} {
    if {[procedure? $pr] eq "#t"} {
        if {[::string match "::constcl::Mem*" $pr]} {
            $pr call {*}[splitlist $vals]
        } else {
            $pr {*}[splitlist $vals]
        }
    } else {
        error "PROCEDURE expected\n" ; #([$pr write] [$vals write])"
    }
}
CB

CB
proc ::constcl::splitlist {vals} {
#puts [info level [info level]]
    set result {}
    while {[pair? $vals] eq "#t"} {
        lappend result [car $vals]
        set vals [cdr $vals]
    }
#puts result=$result
#puts resval=[lmap res $result {$res show}]
    return $result
}
CB

CB
proc ::constcl::make-function {formals exps env} {
    set parms [splitlist $formals]
    #set body [cons [MkSymbol begin] [list [splitlist $exps]]]
    set body [cons [MkSymbol begin] $exps]
    return [MkProcedure [lmap parm $parms {$parm name}] $body $env]
}
CB

CB
proc ::constcl::do-case {keyexpr clauses} {
    if {[eq? [length $clauses] #1] eq "#t"} {
        set keyl [caar $clauses]
        set body [cdar $clauses]
        if {[eq? $keyl [MkSymbol "else"]] eq "#t"} {
            set keyl #t
        } else {
            set keyl [list [MkSymbol "memv"] $keyexpr [list #Q $keyl]]
        }
        return [list #I $keyl [list #B {*}[splitlist $body]] [do-case $keyexpr [cdr $clauses]]]
    } elseif {[eq? [length $clauses] #0] eq "#t"} {
        return [list #Q #NIL]
    } else {
        set keyl [caar $clauses]
        set body [cdar $clauses]
        set keyl [list [MkSymbol "memv"] $keyexpr [list #Q $keyl]]
        return [list #I $keyl [list #B {*}[splitlist $body]] [do-case $keyexpr [cdr $clauses]]]
    }
}

proc ::constcl::do-cond {clauses} {
    if {[eq? [length $clauses] #1] eq "#t"} {
        set pred [caar $clauses]
        set body [cdar $clauses]
        if {[eq? $pred [MkSymbol "else"]] eq "#t"} {
            set pred #t
        }
        if {[null? $body] eq "#t"} {set body $pred}
        return [list #I $pred [list #B {*}[splitlist $body]] [do-cond [cdr $clauses]]]
    } elseif {[eq? [length $clauses] #0] eq "#t"} {
        return [list #Q #NIL]
    } else {
        set pred [caar $clauses]
        set body [cdar $clauses]
        if {[null? $body] eq "#t"} {set body $pred}
        return [list #I $pred [list #B {*}[splitlist $body]] [do-cond [cdr $clauses]]]
    }
}

proc ::constcl::do-and {exps prev} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return $prev
    } else {
        return [list #I [car $exps] [do-and [cdr $exps] [car $exps]] #f]
    }
}

proc ::constcl::do-or {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return #f
    } else {
        return [list #L [list [list [MkSymbol x] [car $exps]]] [list #I [MkSymbol x] [MkSymbol x] [do-or [cdr $exps]]]]
    }
}
CB

CB
proc ::constcl::expand-macro {n1 n2 env} {
    upvar $n1 op $n2 args
    switch [$op name] {
        and {
            if {[eq? [length $args] #0] eq "#t"} {
                set op #B
                set args [list #t]
            } elseif {[eq? [length $args] #1] eq "#t"} {
                set op #B
            } else {
                set p [do-and $args #NIL]
                set op [car $p]
                set args [cdr $p]
            }
        }
        case {
            set p [do-case [car $args] [cdr $args]]
            set op [car $p]
            set args [cdr $p]
        }
        cond {
            set p [do-cond $args]
            set op [car $p]
            set args [cdr $p]
        }
        let {
            if {[atom? [car $args]] eq "#t"} {
                # named let
                set variable [car $args]
                set bindings [cadr $args]
                set body [cddr $args]
                set vars [dict create $variable #f]
                foreach binding [splitlist $bindings] {
                    set var [car $binding]
                    set val [cadr $binding]
                    if {$var in [dict keys $vars]} {error "variable '$var' occurs more than once in let construct"}
                    dict set vars $var $val
                }
                set op #L
                set decl [dict values [dict map {k v} $vars {list $k $v}]]
                set func [list #λ [list {*}[lrange [dict keys $vars] 1 end]] {*}[splitlist $body]]
                set call [list $variable {*}[lrange [dict keys $vars] 1 end]]
                set args [list [list {*}$decl] [list #S $variable $func] $call]
            } else {
                # regular let
                set bindings [car $args]
                set body [cdr $args]
                set vars [dict create]
                foreach binding [splitlist $bindings] {
                    set var [car $binding]
                    set val [cadr $binding]
                    if {$var in [dict keys $vars]} {error "variable '$var' occurs more than once in let construct"}
                    dict set vars $var $val
                }
                set op [list #λ [list {*}[dict keys $vars]] [cons #B $body]]
                set args [list {*}[dict values $vars]]
            }
        }
        or {
            if {[eq? [length $args] #0] eq "#t"} {
                set op #B
                set args [list #f]
            } elseif {[eq? [length $args] #1] eq "#t"} {
                set op #B
            } else {
                set p [do-or $args]
                set op [car $p]
                set args [cdr $p]
            }
        }
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

::tcltest::test eval-1.1 {expand or macro} -body {
    pep "(and)"
    pep "(and #t)"
    pep "(and (> 3 2))"
    pep "(and (> 3 2) (= 7 8))"
} -output "#t\n#t\n#t\n#f\n"

::tcltest::test eval-1.2 {expand let macro} -body {
    pxp "(let ((x 10)) (* x x))"
    pxp "(let ((x 10) (y 5)) (* x y))"
} -output "((lambda (x) (begin (* x x))) 10)\n((lambda (x y) (begin (* x y))) 10 5)\n"

::tcltest::test eval-1.3 {expand named let macro} -body {
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
} -output "(if (or (eqv? (quote 6) (quote 2)) (eqv? (quote 6) (quote 3)) (eqv? (quote 6) (quote 5)) (eqv? (quote 6) (quote 7))) (begin (quote prime)) (if (or (eqv? (quote 6) (quote 1)) (eqv? (quote 6) (quote 4)) (eqv? (quote 6) (quote 6)) (eqv? (quote 6) (quote 8)) (eqv? (quote 6) (quote 9))) (begin (quote composite)) (quote ())))"

::tcltest::test eval-3.1 {run case macro} -body {
    pep "(case (* 2 3) ((2 3 5 7) (quote prime)) ((1 4 6 8 9) (quote composite)))"
} -output "composite\n"

::tcltest::test eval-3.2 {expand case macro} -body {
    pxp "(case (car (quote (c d))) ((a e i o u) (quote vowel)) ((w y) (quote semivowel)) (else (quote consonant)))"
} -output "(if (or (eqv? (quote c) (quote a)) (eqv? (quote c) (quote e)) (eqv? (quote c) (quote i)) (eqv? (quote c) (quote o)) (eqv? (quote c) (quote u))) (begin (quote vowel)) (if (or (eqv? (quote c) (quote w)) (eqv? (quote c) (quote y))) (begin (quote semivowel)) (if #t (begin (quote consonant)) (quote ()))))"

::tcltest::test macro-3.3 {run case macro} {
    pep "(case (car (quote (c d))) ((a e i o u) (quote vowel)) ((w y) (quote semivowel)) (else (quote consonant)))"
} "consonant"


if no {
(if (> 3 4) (begin (+ 4 2)) (if (> 1 2) (begin (+ 5 5)) (if #t (begin (- 8 5)) ())))

---- Output should have been (exact matching):
(if (> 3 4) (begin (+ 4 2)) (if (> 1 2) (begin (+ 5 5)) (if #t (begin (- 8 5)) (quote ()))))}


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

