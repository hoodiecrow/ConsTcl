
proc ::constcl::__eval {expr {env ::constcl::global_env}} {
    if {[atom? $expr] ne "#f"} {
        if {[symbol? $expr] ne "#f"} {
            return [lookup $expr $env]
        } elseif {[null? $expr] ne "#f" || [atom? $expr] ne "#f"} {
            return $expr
        } else {
            error "cannot evaluate $expr"
        }
    } else {
        set op [car $expr]
        set args [cdr $expr]
        while {[$op name] in {
                and case cond define for for/and
                for/list for/or let or quasiquote}} {
            expand-macro $env
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
                return [invoke [eval $op $env] [eval-list $args $env]]
            }
        }
    }
}


CB
proc ::constcl::expand-put! {exps} {
    ::if {[null? $exps] ne "#f"} {::error "too few arguments, 3 expected, got 0"}
    set listname [car $exps]
    ::if {[null? [cdr $exps]] ne "#f"} {::error "too few arguments, 3 expected, got 1"}
    set key [cadr $exps]
    ::if {[null? [cddr $exps]] ne "#f"} {::error "too few arguments, 3 expected, got 2"}
    set val [caddr $exps]
    set keypresent [list #B [list [MkSymbol "list-set!"] $listname [list [MkSymbol "+"] [MkSymbol "idx"] #1] $val] $listname]
    set keynotpresent [list #S $listname [list [MkSymbol "append"] [list [MkSymbol "list"] $key $val] $listname]]
    set conditional [list #I [list [MkSymbol "<"] [MkSymbol "idx"] #0] $keynotpresent $keypresent]
    set let [list #L [list [list [MkSymbol "idx"] [list [MkSymbol "list-find-key"] $listname $key]]] $conditional]
    #return $let
    set qq "`(let ((idx (list-find-key ,listname ,key))) (if (< idx 0) (set! ,listname (append (list ,key ,val) ,listname)) (begin (list-set! plist (+ idx 1) ,val) ,listname)))"
    set env [::constcl::Environment new #NIL {} ::global_env]
    $env set [MkSymbol "listname"] $listname
    $env set [MkSymbol "key"] $key
    $env set [MkSymbol "val"] $val
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB
