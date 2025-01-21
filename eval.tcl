
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
        switch [[car $e] name] {
            quote {
                return [cadr $e]
            }
            if {
                if {[eval [cadr $e] $env] ne "#f"} {
                    return [eval [caddr $e] $env]]
                } else {
                    return [eval [cadddr $e] $env]]
                }
            }
            begin {
                return [eprogn [cdr $e] $env]
            }
            define {
                declare [cadr $e] [eval [caddr $e] $env]
                return {}
            }
            set! {
                return [update! [cadr $e] $env [eval [caddr $e] $env]]
            }
            lambda {
                return [make-function [cadr $e] [cddr $e] $env]
            }
            default {
                return [invoke [eval [car $e] $env] [evlis [cdr $e] $env]]
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
        $pr call {*}[splitlist $vals]
    } elseif {[::string match "::constcl::*" $pr]} {
        $pr {*}[splitlist $vals]
    } else {
        error "PROCEDURE expected\n" ; #([$pr write] [$vals write])"
    }
}
CB

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

TT(

::tcltest::test eval-1.0 {try eval:ing an unbound symbol} -body {
    set ::inputstr "foo"
    ::constcl::eval [::constcl::read]
} -returnCodes error -result "Unbound variable: foo"

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

