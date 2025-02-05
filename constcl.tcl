


namespace eval ::constcl {}


# utility functions
proc ::reg {key args} {
    ::if {[llength $args] == 0} {
        set val ::constcl::$key
    } else {
        set val [lindex $args 0]
    }
    dict set ::constcl::defreg $key $val
}

proc ::pep {str} {
    ::constcl::write [::constcl::eval [::constcl::parse $str]]
}

proc ::pp {str} {
    ::constcl::write [::constcl::parse $str]
}

proc ::pxp {str} {
    set val [::constcl::parse $str]
    set op [::constcl::car $val]
    set args [::constcl::cdr $val]
    ::constcl::expand-macro ::constcl::global_env
    ::constcl::write [::constcl::cons $op $args]
}

proc ::constcl::check {cond msg} {
    ::if {[uplevel $cond] eq "#f"} {
        ::error [uplevel [::list subst $msg]]
    }
}

proc ::pn {} {
    lindex [split [lindex [info level -1] 0] :] end
}



reg in-range ::constcl::in-range

#started out as DKF's code
proc ::constcl::in-range {args} {
    set start 0
    set step 1
    switch [llength $args] {
        1 { lassign $args e ; set end [$e value]}
        2 { lassign $args s e ; set start [$s value] ; set end [$e value]}
        3 { lassign $args s e t ; set start [$s value] ; set end [$e value] ; set step [$t value]}
    }
    set res $start
    while {$step > 0 && $end > [incr start $step] || $step < 0 && $end < [incr start $step]} {
        lappend res $start
    }
    return [list {*}[lmap r $res {MkNumber $r}]]
}


catch { ::constcl::NIL destroy }

oo::class create ::constcl::NIL {
    constructor {} {}
    method bvalue {} {return #NIL}
    method car {} {::error "PAIR expected"}
    method cdr {} {::error "PAIR expected"}
    method set-car! {v} {::error "PAIR expected"}
    method set-cdr! {v} {::error "PAIR expected"}
    method numval {} {::error "Not a number"}
    method write {} {puts -nonewline "()"}
    method show {} {format "()"}
}


reg null? ::constcl::null?

proc ::constcl::null? {obj} {
    ::if {$obj eq "#NIL"} {
        return #t
    } else {
        return #f
    }
}


catch { ::constcl::None destroy}

oo::class create ::constcl::None {}


catch { ::constcl::Dot destroy }

oo::class create ::constcl::Dot {
    method mkconstant {} {}
}

proc ::constcl::dot? {obj} {
    ::if {[info object isa typeof $obj Dot]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Dot]} {
        return #t
    } else {
        return #f
    }
}


catch { ::constcl::Unspecific destroy }

oo::class create ::constcl::Unspecific {
    method mkconstant {} {}
}


catch { ::constcl::Undefined destroy }

oo::class create ::constcl::Undefined {
    method mkconstant {} {}
}



reg error

proc ::constcl::error {msg args} {
    ::if {[llength $args]} {
        lappend msg "("
        set times 0
        foreach arg $args {
            ::if {$times} {
                ::append msg " "
            }
            ::append msg [$arg show]
            incr times
        }
        lappend msg ")"
    }
    ::error $msg
}

# vim: ft=tcl tw=80



catch { ::constcl::IB destroy }

oo::class create ::constcl::IB {
    variable peekc buffer
    constructor {} {
        set peekc {}
        set buffer {}
    }
    method fill {str} {
        set buffer $str
        my advance
    }
    method advance {} {
        ::if {$buffer eq {}} {
            set peekc {}
        } else {
            set peekc [::string index $buffer 0]
            set buffer [::string range $buffer 1 end]
        }
    }
    method first {} {
        return $peekc
    }
    method unget {char} {
        set buffer $peekc$buffer
        set peekc $char
    }
    method find {char} {
        ::if {[::string is space -strict $peekc]} {
            for {set cp 0} {$cp < [::string length $buffer]} {incr cp} {
                ::if {![::string is space -strict [::string index $buffer $cp]]} {
                    break
                }
            }
            return [expr {[::string index $buffer $cp] eq $char}]
        } else {
            return [expr {$peekc eq $char}]
        }
    }
    method skip-ws {} {
        while true {
            switch -regexp $peekc {
                {[[:space:]]} {
                    my advance
                }
                {;} {
                    while {$peekc ne "\n" && $peekc ne {}}  {
                        my advance
                    }
                }
                default {
                    return
                }
            }
        }
    }
}

::constcl::IB create ::constcl::ib




reg parse

proc ::constcl::parse {str} {
    ib fill $str
    return [parse-expression]
}



reg read ::constcl::read

proc ::constcl::read {args} {
    return [parse-expression]
}



proc ::constcl::parse-expression {} {
    ib skip-ws
    switch -regexp [ib first] {
        {^$}          { return #NONE}
        {\"}          { return [parse-string-expression] }
        {\#}          { return [parse-sharp] }
        {\'}          { return [parse-quoted-expression] }
        {\(}          { return [parse-pair-expression ")"] }
        {\+} - {\-}   { return [parse-plus-minus] }
        {\,}          { return [parse-unquoted-expression] }
        {\.}          { ib advance ; return [Dot new] }
        {\[}          { return [parse-pair-expression "\]"] }
        {\`}          { return [parse-quasiquoted-expression] }
        {\d}          { return [parse-number-expression] }
        {[[:graph:]]} { return [parse-identifier-expression] }
        default {
            ::error "unexpected character ([ib first])"
        }
    }
}



proc ::constcl::parse-string-expression {} {
    set str {}
    ib advance
    while {[ib first] ne "\"" && [ib first] ne {}} {
        set c [ib first]
        ::if {$c eq "\\"} {
            ib advance
            ::append str [ib first]
        } else {
            ::append str $c
        }
        ib advance
    }
    ::if {[ib first] ne "\""} {
        ::error "malformed string (no ending double quote)"
    }
    ib advance
    ib skip-ws
    set expr [MkString $str]
    $expr mkconstant
    return $expr
}




proc ::constcl::parse-sharp {} {
    ib advance
    switch [ib first] {
        (    { return [parse-vector-expression] }
        t    { ib advance ; ib skip-ws ; return #t }
        f    { ib advance ; ib skip-ws ; return #f }
        "\\" { return [parse-character-expression] }
        default {
            ::error "Illegal #-literal"
        }
    }
}


proc ::constcl::make-constant {val} {
    ::if {[pair? $val] ne "#f"} {
        $val mkconstant
        make-constant [car $val]
        make-constant [cdr $val]
    } elseif {[null? $val] ne "#f"} {
        return #NIL
    } else {
        $val mkconstant
    }
}



proc ::constcl::parse-quoted-expression {} {
    ib advance
    set expr [parse-expression]
    ib skip-ws
    make-constant $expr
    return [list #Q $expr]
}





proc ::constcl::parse-pair {char} {
    ::if {[ib find $char]} {
        return #NIL
    }
    ib skip-ws
    set a [parse-expression]
    ib skip-ws
    set res $a
    set prev #NIL
    while {![ib find $char]} {
        set x [parse-expression]
        ib skip-ws
        ::if {[dot? $x] ne "#f"} {
            set prev [parse-expression]
            ib skip-ws
        } else {
            lappend res $x
        }
        ::if {[llength $res] > 999} break
    }
    foreach r [lreverse $res] {
        set prev [cons $r $prev]
    }
    return $prev
}

proc ::constcl::parse-pair-expression {char} {
    ib advance
    ib skip-ws
    set expr [parse-pair $char]
    ib skip-ws
    ::if {[ib first] ne $char} {
        ::if {$char eq ")"} {
            ::error "Missing right parenthesis (first=[ib first])."
        } else {
            ::error "Missing right bracket (first=[ib first])."
        }
    }
    ib advance
    ib skip-ws
    return $expr
}




proc ::constcl::parse-plus-minus {} {
    set c [ib first]
    ib advance
    ::if {[::string is digit -strict [ib first]]} {
        ib unget $c
        return [::constcl::parse-number-expression]
    } else {
        ::if {$c eq "+"} {
            ib skip-ws
            return [MkSymbol "+"]
        } else {
            ib skip-ws
            return [MkSymbol "-"]
        }
    }
}



proc ::constcl::parse-unquoted-expression {} {
    ib advance
    set symbol "unquote"
    ::if {[ib first] eq "@"} {
        set symbol "unquote-splicing"
        ib advance
    }
    set expr [parse-expression]
    ib skip-ws
    return [list [MkSymbol $symbol] $expr]
}




proc ::constcl::parse-quasiquoted-expression {} {
    ib advance
    set expr [parse-expression]
    ib skip-ws
    make-constant $expr
    return [list [MkSymbol "quasiquote"] $expr]
}



proc ::constcl::interspace {c} {
    ::if {$c eq {} || [::string is space -strict $c] || $c eq ";"} {
        return #t
    } else {
        return #f
    }
}



proc ::constcl::parse-number-expression {} {
    while {[interspace [ib first]] ne "#t" && [ib first] ni {) \]}} {
        ::append num [ib first]
        ib advance
    }
    ib skip-ws
    check {::string is double -strict $num} {Invalid numeric constant $num}
    return [MkNumber $num]
}




proc ::constcl::parse-identifier-expression {} {
    while {[interspace [ib first]] ne "#t" && [ib first] ni {) \]}} {
        ::append name [ib first]
        ib advance
    }
    ib skip-ws
    # idcheck throws error if invalid identifier
    return [MkSymbol [idcheck $name]]
}



proc ::constcl::character-check {name} {
    ::if {[regexp -nocase {^#\\([[:graph:]]|space|newline)$} $name]} {
        return #t
    } else {
        return #f
    }
}



proc ::constcl::parse-character-expression {} {
    set name "#"
    while {[interspace [ib first]] ne "#t" && [ib first] ni {) ]}} {
        ::append name [ib first]
        ib advance
    }
    check {character-check $name} {Invalid character constant $name}
    ib skip-ws
    return [MkChar $name]
}




proc ::constcl::parse-vector-expression {} {
    ib advance
    ib skip-ws
    set res {}
    while {[ib first] ne {} && [ib first] ne ")"} {
        lappend res [parse-expression]
        ib skip-ws
    }
    set vec [MkVector $res]
    $vec mkconstant
    ::if {[ib first] ne ")"} {
        ::error "Missing right parenthesis (first=[ib first])."
    }
    ib advance
    ib skip-ws
    return $vec
}


# vim: ft=tcl tw=80




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
            and case cond define for for/and for/list
            for/or let or quasiquote unless when}} {
                expand-macro $env
        }
        switch [$op name] {
            quote   { car $args }
            if      { if {eval [car $args] $env} \
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



proc ::constcl::lookup {sym env} {
    [$env find $sym] get $sym
}



proc ::constcl::if {cond conseq altern} {
    ::if {[uplevel $cond] ne "#f"} {uplevel $conseq} {uplevel $altern}
}



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



proc ::constcl::declare {sym val env} {
    varcheck [idcheck [$sym name]]
    $env set $sym $val
    return #NONE
}



proc ::constcl::update! {var val env} {
    [$env find $var] set $var $val
    set val
}



proc ::constcl::make-function {formals body env} {
    ::if {[[length $body] value] > 1} {
        set body [cons #B $body]
    } else {
        set body [car $body]
    }
    return [MkProcedure $formals $body $env]
}



proc ::constcl::invoke {pr vals} {
    check {procedure? $pr} {PROCEDURE expected\n([$pr show] val ...)}
    ::if {[info object isa object $pr]} {
        $pr call {*}[splitlist $vals]
    } else {
        $pr {*}[splitlist $vals]
    }
}




proc ::constcl::splitlist {vals} {
    set result {}
    while {[pair? $vals] ne "#f"} {
        lappend result [car $vals]
        set vals [cdr $vals]
    }
    return $result
}



proc ::constcl::eval-list {exps env} {
    # don't convert to ::constcl::if, it breaks (fact 100)
    ::if {[pair? $exps] ne "#f"} {
        return [cons [eval [car $exps] $env] [eval-list [cdr $exps] $env]]
    } {
        return #NIL
    }
}



proc ::constcl::expand-macro {env} {
    upvar op op args args
    ::if {[$op name] eq "define" && ([pair? [car $args]] eq "#f" || [[caar $args] name] eq "lambda")} {
        return -code break
    }
    switch [$op name] {
        and {
            set expr [expand-and $args]
        }
        case {
            set expr [expand-case [car $args] [cdr $args]]
        }
        cond {
            set expr [expand-cond $args]
        }
        define {
            set expr [expand-define $args]
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
            set expr [expand-let $args]
        }
        or {
            set expr [expand-or $args]
        }
        quasiquote {
            set expr [expand-quasiquote $args $env]
        }
        unless {
            set expr [expand-unless $args]
        }
        when {
            set expr [expand-when $args]
        }
    }
    set op [car $expr]
    set args [cdr $expr]
    return #NIL
}



proc ::constcl::expand-and {exps} {
    if {eq? [length $exps] #0} {
        return [list #B #t]
    } {
        if {eq? [length $exps] #1} {
            return [cons #B $exps]
        } {
            return [do-and $exps #NIL]
        }
    }
}


proc ::constcl::do-and {exps prev} {
    if {eq? [length $exps] #0} {
        return $prev
    } {
        return [list #I [car $exps] [do-and [cdr $exps] [car $exps]] #f]
    }
}



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



proc ::constcl::expand-define {exps} {
    set symbol [caar $exps]
    set formals [cdar $exps]
    set body [cdr $exps]
    return [list [MkSymbol "define"] $symbol [list #λ $formals {*}[splitlist $body]]]
}



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


proc ::constcl::do-for {exps env} {
    # make clauses a Tcl list
    set clauses [splitlist [car $exps]]
    set body [cdr $exps]
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


proc ::constcl::expand-for {exps env} {
    set res [do-for $exps $env]
    lappend res [list #Q #NIL]
    return [list #B {*}$res]
}



proc ::constcl::expand-for/and {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "and"] {*}$res]
}



proc ::constcl::expand-for/list {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "list"] {*}$res]
}



proc ::constcl::expand-for/or {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "or"] {*}$res]
}



proc ::constcl::expand-let {exps} {
    ::if {[symbol? [car $exps]] ne "#f"} {
        # named let
        set variable [car $exps]
        set bindings [cadr $exps]
        set body [cddr $exps]
        set vars [dict create $variable #f]
        foreach binding [splitlist $bindings] {
            set var [car $binding]
            set val [cadr $binding]
            ::if {$var in [dict keys $vars]} {::error "variable '$var' occurs more than once in let construct"}
            dict set vars $var $val
        }
        set decl [dict values [dict map {k v} $vars {list $k $v}]]
        set func [list #λ [list {*}[lrange [dict keys $vars] 1 end]] {*}[splitlist $body]]
        set call [list {*}[dict keys $vars]]
        return [list #L [list {*}$decl] [list #S $variable $func] $call]
    } else {
        # regular let
        set bindings [car $exps]
        set body [cdr $exps]
        set vars [dict create]
        foreach binding [splitlist $bindings] {
            set var [car $binding]
            set val [cadr $binding]
            ::if {$var in [dict keys $vars]} {::error "variable '$var' occurs more than once in let construct"}
            dict set vars $var $val
        }
        return [list [list #λ [list {*}[dict keys $vars]] {*}[splitlist $body]] {*}[dict values $vars]]
    }
}



proc ::constcl::expand-or {exps} {
    ::if {[eq? [length $exps] #0] ne "#f"} {
        return [list #B #f]
    } elseif {[eq? [length $exps] #1] ne "#f"} {
        return [cons #B $exps]
    } else {
        return [do-or $exps]
    }
}


proc ::constcl::do-or {exps} {
    if {eq? [length $exps] #0} {
        return #f
    } {
        return [list #L [list [list #x [car $exps]]] [list #I #x #x [do-or [cdr $exps]]]]
    }
}



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


proc ::constcl::expand-quasiquote {exps env} {
    set qqlevel 0
    ::if {[list? [car $exps]] ne "#f"} {
        set node [car $exps]
        return [qq-visit-child $node 0 $env]
    } elseif {[vector? [car $exps]] ne "#f"} {
        set vect [car $exps]
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



proc ::constcl::expand-unless {exps} {
    return [list #I [car $exps] [list #Q #NIL] [cons #B [cdr $exps]]]
}



proc ::constcl::expand-when {exps} {
    return [list #I [car $exps] [cons #B [cdr $exps]] [list #Q #NIL]]
}

### Resolving local defines



proc resolve-local-defines {expr} {
    set rest [lassign [extract-from-defines $expr VALS] a error]

    if {$error ne "#f"} {
        return #NIL
    }

    set rest [lassign [extract-from-defines $expr VALS] v error]

    if {$rest eq "#NIL"} {
        set rest [cons #UNSP #NIL]
    }

    return [make-recursive-lambda $v $a $rest]
}


proc extract-from-defines {expr part} {
    set a #NIL

    while {$expr ne "#NIL"} {
        if {[atom? $x] ne "#f" || [atom? [car $x]] ne "#f" || [eq? [caar $x] [MkSymbol "define"]] eq "#f"} {
            break
        }

        set n [car $x]
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
        set x [cdr $x]
    }
    return [::list $a #f x]
}


proc argument-list? {n} {
    if {$n eq "#NIL"} {
        return #t
    } elseif {[symbol? $n] ne "#f"} {
        return #t
    } elseif {[atom? $n] ne "#f"} {
        return #f
    }
    while {[pair? $n] ne "#f"} {
        if {[symbol? [car $n]] eq "#f"} {
            return #f
        }
        set n [cdr $n]
    }
    if {$n eq "#NIL"} {
        return #t
    } elseif {[symbol? $n] ne "#f"} {
        return #t
    }
}


proc make-recursive-lambda {v a body} {
    set t [make-temporaries $v]

    set body [append-b [make-assignments $v $t] $body]
    set body [cons $body #NIL]
    set n [cons $t $body]
    set n [cons #λ $n]
    set n [cons $n $a]
    set n [cons $n #NIL]
    set n [cons $v $n]
    set n [cons #λ $n]
    set n [cons $n [make-undefineds $v]]
    return $n
}


proc make-temporaries {x} {
    set n #NIL
    while {$x ne "#NIL"} {
        set v [gensym "g"]
        set n [cons $v $n]
        set x [cdr $x]
    }
    return $n
}


proc append-b {a b} {
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


proc make-assignments {x t} {
    set n #NIL
    while {$x ne "#NIL"} {
       set asg [cons [car $t] #NIL]
       set asg [cons [car $x] $asg]
       set asg [cons #S $asg]
       set n [cons $asg $n]
       set x [cdr $x]
       set t [cdr $t]
   }
   return [cons #B $n]
}


proc make-undefineds {x} {
    set n #NIL
    while {$x ne "#NIL"} {
        
}


proc ::constcl::scheme-report-environment {version} {
    # TODO
}

proc ::constcl::null-environment {version} {
    # TODO
}

proc ::constcl::interaction-environment {} {
    # TODO
}

# vim: ft=tcl tw=80



reg write ::constcl::write

proc ::constcl::write {val args} {
    ::if {$val ne "#NONE"} {
        ::constcl::write-value $val
        puts {}
    }
    return
}



proc ::constcl::write-value {val} {
    $val write
    return
}



reg display ::constcl::display

proc ::constcl::display {val args} {
    ::if {$val ne "#NONE"} {
        ::constcl::write-value $val
        flush stdout
    }
    return
}



proc ::constcl::write-pair {pair} {
    # take an object and print the car and the cdr of the stored value
    set a [car $pair]
    set d [cdr $pair]
    # print car
    write-value $a
    ::if {[pair? $d] ne "#f"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d
    } elseif {[null? $d] ne "#f"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write-value $d
    }
    return
}


# vim: ft=tcl tw=80


reg eq? ::constcl::eq?

proc ::constcl::eq? {val1 val2} {
    ::if {[boolean? $val1] ne "#f" && [boolean? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] ne "#f" && [symbol? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[number? $val1] ne "#f" && [number? $val2] ne "#f" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[char? $val1] ne "#f" && [char? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[null? $val1] ne "#f" && [null? $val2] ne "#f"} {
        return #t
    } elseif {[pair? $val1] ne "#f" && [pair? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[string? $val1] ne "#f" && [string? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[vector? $val1] ne "#f" && [vector? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[procedure? $val1] ne "#f" && [procedure? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}

reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {val1 val2} {
    ::if {[boolean? $val1] ne "#f" && [boolean? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] ne "#f" && [symbol? $val2] ne "#f" && [$val1 name] eq [$val2 name]} {
        return #t
    } elseif {[number? $val1] ne "#f" && [number? $val2] ne "#f" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[char? $val1] ne "#f" && [char? $val2] ne "#f" && [$val1 char] eq [$val2 char]} {
        return #t
    } elseif {[null? $val1] ne "#f" && [null? $val2] ne "#f"} {
        return #t
    } elseif {[pair? $val1] ne "#f" && [pair? $val2] ne "#f" && [$val1 car] eq [$val2 car] && [$val1 cdr] eq [$val2 cdr]} {
        return #t
    } elseif {[string? $val1] ne "#f" && [string? $val2] ne "#f" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[vector? $val1] ne "#f" && [vector? $val2] ne "#f" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[procedure? $val1] ne "#f" && [procedure? $val2] ne "#f" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}

reg equal? ::constcl::equal?

proc ::constcl::equal? {val1 val2} {
    ::if {[$val1 show] eq [$val2 show]} {
        return #t
    } else {
        return #f
    }
    # TODO
}

# vim: ft=tcl tw=80


oo::class create ::constcl::Number {
    superclass ::constcl::NIL
    variable value
    constructor {v} {
        ::if {[::string is double -strict $v]} {
            set value $v
        } else {
            ::error "NUMBER expected\n$v"
        }
    }
    method zero? {} {::if {$value == 0} then {return #t} else {return #f}}
    method positive? {} {::if {$value > 0} then {return #t} else {return #f}}
    method negative? {} {::if {$value < 0} then {return #t} else {return #f}}
    method even? {} {::if {$value % 2 == 0} then {return #t} else {return #f}}
    method odd? {} {::if {$value % 2 == 1} then {return #t} else {return #f}}
    method value {} { set value }
    method numval {} {set value}
    method mkconstant {} {}
    method constant {} {return 1}
    method write {} { puts -nonewline [my value] }
    method show {} { set value }
}

interp alias {} ::constcl::MkNumber {} ::constcl::Number new




reg number? ::constcl::number?

proc ::constcl::number? {val} {
    ::if {[info object isa typeof $val ::constcl::Number]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Number]} {
        return #t
    } else {
        return #f
    }
}




reg = ::constcl::=

proc ::constcl::= {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(= num ...)"
    }
    ::if {[::tcl::mathop::== {*}$vals]} {
        return #t
    } else {
        return #f
    }
}


reg < ::constcl::<

proc ::constcl::< {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(< num ...)"
    }
    ::if {[::tcl::mathop::< {*}$vals]} {
        return #t
    } else {
        return #f
    }
}


reg > ::constcl::>

proc ::constcl::> {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(> num ...)"
    }
    ::if {[::tcl::mathop::> {*}$vals]} {
        return #t
    } else {
        return #f
    }
}


reg <= ::constcl::<=

proc ::constcl::<= {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(<= num ...)"
    }
    ::if {[::tcl::mathop::<= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}


reg >= ::constcl::>=

proc ::constcl::>= {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(>= num ...)"
    }
    ::if {[::tcl::mathop::>= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}




reg zero? ::constcl::zero?

proc ::constcl::zero? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num zero?]
}




reg positive? ::constcl::positive?

proc ::constcl::positive? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num positive?]
}


reg negative? ::constcl::negative?

proc ::constcl::negative? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num negative?]
}


reg even? ::constcl::even?

proc ::constcl::even? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num even?]
}


reg odd? ::constcl::odd?

proc ::constcl::odd? {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    return [$num odd?]
}





reg max ::constcl::max

proc ::constcl::max {num args} {
    try {
        set vals [lmap arg [::list $num {*}$args] {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(max num...)"
    }
    MkNumber [::tcl::mathfunc::max {*}$vals]
}


reg min ::constcl::min

proc ::constcl::min {num args} {
    try {
        set vals [lmap arg [::list $num {*}$args] {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(min num...)"
    }
    MkNumber [::tcl::mathfunc::min {*}$vals]
}






reg + ::constcl::+

proc ::constcl::+ {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(+ num ...)"
    }
    MkNumber [::tcl::mathop::+ {*}$vals]
}


reg * ::constcl::*

proc ::constcl::* {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(* num ...)"
    }
    MkNumber [::tcl::mathop::* {*}$vals]
}


reg - ::constcl::-

proc ::constcl::- {num args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(- num ...)"
    }
    MkNumber [::tcl::mathop::- [$num numval] {*}$vals]
}


reg / ::constcl::/

proc ::constcl::/ {num args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        ::error "NUMBER expected\n(/ num ...)"
    }
    MkNumber [::tcl::mathop::/ [$num numval] {*}$vals]
}




reg abs ::constcl::abs

proc ::constcl::abs {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    ::if {[$num negative?] ne "#f"} {
        return [MkNumber [expr {[$num numval] * -1}]]
    } else {
        return $num
    }
}





reg quotient

proc ::constcl::quotient {num1 num2} {
    set q [::tcl::mathop::/ [$num1 numval] [$num2 numval]]
    ::if {$q > 0} {
        return [MkNumber [::tcl::mathfunc::floor $q]]
    } elseif {$q < 0} {
        return [MkNumber [::tcl::mathfunc::ceil $q]]
    } else {
        return #0
    }
}




reg remainder

proc ::constcl::remainder {num1 num2} {
    set n [::tcl::mathop::% [[abs $num1] numval] [[abs $num2] numval]]
    ::if {[$num1 negative?] ne "#f"} {
        set n -$n
    }
    return [MkNumber $n]
}




reg modulo

proc ::constcl::modulo {num1 num2} {
    return [MkNumber [::tcl::mathop::% [$num1 numval] [$num2 numval]]]
}


proc ::constcl::gcd {args} {
    # TODO
}

proc ::constcl::lcm {args} {
    # TODO
}

proc ::constcl::numerator {q} {
    # TODO
}

proc ::constcl::denominator {q} {
    # TODO
}




reg floor ::constcl::floor

proc ::constcl::floor {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::floor [$num numval]]
}


reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::ceil [$num numval]]
}


reg truncate ::constcl::truncate

proc ::constcl::truncate {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    ::if {[$num negative?] ne "#f"} {
        MkNumber [::tcl::mathfunc::ceil [$num numval]]
    } else {
        MkNumber [::tcl::mathfunc::floor [$num numval]]
    }
}


reg round ::constcl::round

proc ::constcl::round {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::round [$num numval]]
}


proc ::constcl::rationalize {x y} {
    # TODO
}





reg exp ::constcl::exp

proc ::constcl::exp {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::exp [$num numval]]
}


reg log ::constcl::log

proc ::constcl::log {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::log [$num numval]]
}


reg sin ::constcl::sin

proc ::constcl::sin {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::sin [$num numval]]
}

reg cos ::constcl::cos

proc ::constcl::cos {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::cos [$num numval]]
}

reg tan ::constcl::tan

proc ::constcl::tan {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::tan [$num numval]]
}


reg asin ::constcl::asin

proc ::constcl::asin {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::asin [$num numval]]
}

reg acos ::constcl::acos

proc ::constcl::acos {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::acos [$num numval]]
}

reg atan ::constcl::atan

proc ::constcl::atan {args} {
    ::if {[llength $args] == 1} {
        set num [lindex $args 0]
        check {number? $num} {NUMBER expected\n([pn] [$num show])}
        MkNumber [::tcl::mathfunc::atan [$num numval]]
    } else {
        lassign $args num1 num2
        check {number? $num1} {NUMBER expected\n([pn] [$num1 show])}
        check {number? $num2} {NUMBER expected\n([pn] [$num2 show])}
        MkNumber [::tcl::mathfunc::atan2 [$num1 numval] [$num2 numval]]
    }
}




reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {num} {
    check {number? $num} {NUMBER expected\n([pn] [$num show])}
    MkNumber [::tcl::mathfunc::sqrt [$num numval]]
}




reg expt ::constcl::expt

proc ::constcl::expt {num1 num2} {
    check {number? $num1} {NUMBER expected\n([pn] [$num1 show] [$num2 show])}
    check {number? $num2} {NUMBER expected\n([pn] [$num1 show] [$num2 show])}
    MkNumber [::tcl::mathfunc::pow [$num1 numval] [$num2 numval]]
}


proc ::constcl::make-rectangular {x1 x2} {
    # TODO
}

proc ::constcl::make-polar {x3 x4} {
    # TODO
}

proc ::constcl::real-part {z} {
    # TODO
}

proc ::constcl::imag-part {z} {
    # TODO
}

proc ::constcl::magnitude {z} {
    # TODO
}

proc ::constcl::angle {z} {
    # TODO
}

proc ::constcl::exact->inexact {z} {
    # TODO
}

proc ::constcl::inexact->exact {z} {
    # TODO
}




reg number->string ::constcl::number->string

proc ::constcl::number->string {num args} {
    ::if {[llength $args] == 0} {
        check {number? $num} {NUMBER expected\n([pn] [$num show])}
        return [MkString [$num numval]]
    } else {
        lassign $args radix
        check {number? $num} {NUMBER expected\n([pn] [$num show])}
        check {number? $radix} {NUMBER expected\n([pn] [$num show] [$radix show])}
        check {memv $radix [list [MkNumber 2] [MkNumber 8] [MkNumber 10] [MkNumber 16]]} {Radix not in 2, 8, 10, 16\n([pn] [$num show] [$radix show])}
        ::if {[$radix numval] == 10} {
            return [MkString [$num numval]]
        } else {
            return [MkString [base [$radix numval] [$num numval]]]
        }
    }
}

# due to Richard Suchenwirth, <URL: https://wiki.tcl-lang.org/page/Based+numbers>
proc base {base number} {
    set negative [regexp ^-(.+) $number -> number]
    set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
    set res {}
    while {$number} {
        set digit [expr {$number % $base}]
        set res [lindex $digits $digit]$res
        set number [expr {$number / $base}]
    }
    ::if $negative {set res -$res}
    set res
}





reg string->number ::constcl::string->number

proc ::constcl::string->number {str args} {
    ::if {[llength $args] == 0} {
        check {string? $str} {STRING expected\n([pn] [$str show])}
        return [MkNumber [$str value]]
    } else {
        lassign $args radix
        check {string? $str} {STRING expected\n([pn] [$str show])}
        check {memv $radix [list [MkNumber 2] [MkNumber 8] [MkNumber 10] [MkNumber 16]]} {Radix not in 2, 8, 10, 16\n([pn] [$str show] [$radix show])}
        ::if {[$radix numval] == 10} {
            return [MkNumber [$str value]]
        } else {
            return [MkNumber [frombase [$radix numval] [$str value]]]
        }
    }
}

# due to Richard Suchenwirth, <URL: https://wiki.tcl-lang.org/page/Based+numbers>
proc frombase {base number} {
    set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
    set negative [regexp ^-(.+) $number -> number]
    set res 0
    foreach digit [split $number {}] {
        set decimalvalue [lsearch $digits $digit]
        ::if {$decimalvalue < 0 || $decimalvalue >= $base} {
            ::error "bad digit $decimalvalue for base $base"
        }
        set res [expr {$res * $base + $decimalvalue}]
    }
    ::if $negative {set res -$res}
    set res
}


# vim: ft=tcl tw=80


oo::class create ::constcl::Boolean {
    superclass ::constcl::NIL
    variable bvalue
    constructor {v} {
        ::if {$v ni {#t #f}} {
            ::error "bad boolean value $v"
        }
        set bvalue $v
    }
    method mkconstant {} {}
    method constant {} {return 1}
    method bvalue {} { set bvalue }
    method value {} { set bvalue }
    method write {} { puts -nonewline [my bvalue] }
    method show {} {set bvalue}
}

proc ::constcl::MkBoolean {v} {
    foreach instance [info class instances ::constcl::Boolean] {
        ::if {[$instance bvalue] eq $v} {
            return $instance
        }
    }
    return [::constcl::Boolean new $v]
}




reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {val} {
    ::if {[info object isa typeof $val ::constcl::Boolean]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Boolean]} {
        return #t
    } else {
        return #f
    }
}





reg not ::constcl::not

proc ::constcl::not {val} {
    ::if {[$val bvalue] eq "#f"} {
        return #t
    } else {
        return #f
    }
}


# vim: ft=tcl tw=80


oo::class create ::constcl::Char {
    superclass ::constcl::NIL
    variable value
    constructor {v} {
        ::if {[regexp {^#\\([[:graph:]]|space|newline)$} $v]} {
            set value $v
        } else {
            ::if {$v eq "#\\ "} {
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
        ::if {[::string is alpha -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method numeric? {} {
        ::if {[::string is digit -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method whitespace? {} {
        ::if {[::string is space -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method upper-case? {} {
        ::if {[::string is upper -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case? {} {
        ::if {[::string is lower -strict [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method mkconstant {} {}
    method constant {} {return 1}
    method value {} {return $value}
    method write {} { puts -nonewline $value }
    method show {} {set value}
}

proc ::constcl::MkChar {v} {
    ::if {[regexp -nocase {^#\\(space|newline)$} $v]} {
        set v [::string tolower $v]
    }
    foreach instance [info class instances ::constcl::Char] {
        ::if {[$instance value] eq $v} {
            return $instance
        }
    }
    return [::constcl::Char new $v]
}



reg char? ::constcl::char?

proc ::constcl::char? {val} {
    ::if {[info object isa typeof $val ::constcl::Char]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Char]} {
        return #t
    } else {
        return #f
    }
}




reg char=? ::constcl::char=?

proc ::constcl::char=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {$char1 eq $char2} {
        return #t
    } else {
        return #f
    }
}


reg char<? ::constcl::char<?

proc ::constcl::char<? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] < [$char2 char]} {
        return #t
    } else {
        return #f
    }
}


reg char>? ::constcl::char>?

proc ::constcl::char>? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] > [$char2 char]} {
        return #t
    } else {
        return #f
    }
}


reg char<=? ::constcl::char<=?

proc ::constcl::char<=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] <= [$char2 char]} {
        return #t
    } else {
        return #f
    }
}


reg char>=? ::constcl::char>=?

proc ::constcl::char>=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[$char1 char] >= [$char2 char]} {
        return #t
    } else {
        return #f
    }
}




reg char-ci=? ::constcl::char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] eq [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
    }
}


reg char-ci<? ::constcl::char-ci<?

proc ::constcl::char-ci<? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] < [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
    }
}


reg char-ci>? ::constcl::char-ci>?

proc ::constcl::char-ci>? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] > [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
    }
}


reg char-ci<=? ::constcl::char-ci<=?

proc ::constcl::char-ci<=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] <= [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
    }
}


reg char-ci>=? ::constcl::char-ci>=?

proc ::constcl::char-ci>=? {char1 char2} {
    check {char? $char1} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    check {char? $char2} {CHAR expected\n([pn] [$char1 show] [$char2 show])}
    ::if {[::string tolower [$char1 char]] >= [::string tolower [$char2 char]]} {
        return #t
    } else {
        return #f
    }
}




reg char-alphabetic? ::constcl::char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char alphabetic?]
}


reg char-numeric? ::constcl::char-numeric?

proc ::constcl::char-numeric? {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char numeric?]
}


reg char-whitespace? ::constcl::char-whitespace?

proc ::constcl::char-whitespace? {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char whitespace?]
}


reg char-upper-case? ::constcl::char-upper-case?

proc ::constcl::char-upper-case? {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char upper-case?]
}


reg char-lower-case? ::constcl::char-lower-case?

proc ::constcl::char-lower-case? {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    return [$char lower-case?]
}





reg char->integer

proc ::constcl::char->integer {char} {
    return [MkNumber [scan [$char char] %c]]
}



reg integer->char

proc ::constcl::integer->char {int} {
    ::if {$int == 10} {
        return [MkChar #\\newline]
    } elseif {$int == 32} {
        return [MkChar #\\space]
    } else {
        return [MkChar #\\[format %c [$int numval]]]
    }
}




reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    ::if {[::string is alpha -strict [$char char]]} {
        return [MkChar [::string toupper [$char value]]]
    } else {
        return $char
    }
}



reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
    check {char? $char} {CHAR expected\n([pn] [$char show])}
    ::if {[::string is alpha -strict [$char char]]} {
        return [MkChar [::string tolower [$char value]]]
    } else {
        return $char
    }
}


# vim: ft=tcl tw=80


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
    method write {} {
        regexp {(\d+)} [self] -> num
        puts -nonewline "#<proc-$num>"
    }
    method show {} { return [self] }
    method call {args} {
        ::constcl::eval $body [::constcl::Environment new $parms $args $env]
    }

}

interp alias {} ::constcl::MkProcedure {} ::constcl::Procedure new



reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {val} {
    ::if {[info object isa typeof $val ::constcl::Procedure]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Procedure]} {
        return #t
    } elseif {[::string match "::constcl::*" $val]} {
        return #t
    } else {
        return #f
    }
}





reg apply ::constcl::apply

proc ::constcl::apply {pr vals} {
    check {procedure? $pr} {PROCEDURE expected\n([pn] [$pr show] ...)}
    invoke $pr $vals
}





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


proc ::constcl::force {promise} {
    # TODO
}

proc ::constcl::call-with-current-continuation {proc} {
    # TODO
}

proc ::constcl::values {args} {
    # TODO
}

proc ::constcl::call-with-values {producer consumer} {
    # TODO
}

proc ::constcl::dynamic-wind {before thunk after} {
    # TODO
}

# vim: ft=tcl tw=80


proc ::constcl::call-with-input-file {string proc} {
    # TODO
}

proc ::constcl::call-with-output-file {string proc} {
    # TODO
}

proc ::constcl::input-port? {obj} {
    # TODO
}

proc ::constcl::output-port? {obj} {
    # TODO
}

proc ::constcl::current-input-port {} {
    # TODO
}

proc ::constcl::current-output-port {} {
    # TODO
}

proc ::constcl::with-input-from-file {string thunk} {
    # TODO
}


proc ::constcl::with-output-to-file {string thunk} {
    # TODO
}

proc ::constcl::open-input-file {filename} {
    # TODO
}

proc ::constcl::open-output-file {filename} {
    # TODO
}

proc ::constcl::close-input-port {port} {
    # TODO
}

proc ::constcl::close-output-port {port} {
    # TODO
}


proc ::constcl::read-char {args} {
    # TODO
}

proc ::constcl::peek-char {args} {
    # TODO
}

proc ::constcl::char-ready? {args} {
    # TODO
}



reg newline ::constcl::newline

proc ::constcl::newline {args} {
    # TODO write newline
}

proc ::constcl::write-char {args} {
    # TODO
}

proc ::constcl::load {filename} {
    # TODO
}

proc ::constcl::transcript-on {filename} {
    # TODO
}

proc ::constcl::transcript-off {} {
    # TODO
}



catch { ::constcl::Pair destroy }

oo::class create ::constcl::Pair {
    superclass ::constcl::NIL
    variable car cdr constant
    constructor {a d} {
        set car $a
        set cdr $d
        set constant 0
    }
    method name {} {} ;# for eval to call when dealing with an application form
    method value {} {my show}
    method car {} { set car }
    method cdr {} { set cdr }
    method set-car! {val} {
        ::constcl::check {my mutable?} {Can't modify a constant pair}
        set car $val
        self
    }
    method set-cdr! {val} {
        ::constcl::check {my mutable?} {Can't modify a constant pair}
        set cdr $val
        self
    }
    method mkconstant {} {set constant 1}
    method constant {} {return $constant}
    method mutable? {} {expr {$constant?"#f":"#t"}}
    method write {} {
        puts -nonewline "("
        ::constcl::write-pair [self]
        puts -nonewline ")"
    }
    method show {} {format "(%s)" [::constcl::show-pair [self]]}
}


interp alias {} ::constcl::MkPair {} ::constcl::Pair new



reg pair? ::constcl::pair?

proc ::constcl::pair? {val} {
    ::if {[info object isa typeof $val ::constcl::Pair]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Pair]} {
        return #t
    } else {
        return #f
    }
}



proc ::constcl::show-pair {pair} {
    # take an object and print the car and the cdr of the stored value
    set str {}
    set a [car $pair]
    set d [cdr $pair]
    # print car
    ::append str [$a show]
    ::if {[pair? $d] ne "#f"} {
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





reg cons ::constcl::cons

proc ::constcl::cons {car cdr} {
    MkPair $car $cdr
}





reg car ::constcl::car

proc ::constcl::car {pair} {
    $pair car
}





reg cdr ::constcl::cdr

proc ::constcl::cdr {pair} {
    $pair cdr
}



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
            ::if {\$c eq \"a\"} {
                set pair \[car \$pair\]
            } else {
                set pair \[cdr \$pair\]
            }
        }
        return \$pair
    "

}




reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {pair val} {
    $pair set-car! $val
}





reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {pair val} {
    $pair set-cdr! $val
}




reg list? ::constcl::list?

proc ::constcl::list? {pair} {
    set visited {}
    return [listp $pair]
}


proc ::constcl::listp {pair} {
    upvar visited visited
    ::if {$pair in $visited} {
        return #f
    }
    lappend visited $pair
    ::if {[null? $pair] ne "#f"} {
        return #t
    } elseif {[pair? $pair] ne "#f"} {
        return [listp [cdr $pair]]
    } else {
        return #f
    }
}





reg list ::constcl::list

proc ::constcl::list {args} {
    ::if {[llength $args] == 0} {
        return #NIL
    } else {
        set prev #NIL
        foreach obj [lreverse $args] {
            set prev [cons $obj $prev]
        }
        return $prev
    }
}





reg length ::constcl::length

proc ::constcl::length {pair} {
    check {list? $pair} {LIST expected\n([pn] lst)}
    MkNumber [length-helper $pair]
}


proc ::constcl::length-helper {pair} {
    ::if {[null? $pair] ne "#f"} {
        return 0
    } else {
        return [expr {1 + [length-helper [cdr $pair]]}]
    }
}





reg append ::constcl::append

proc ::constcl::append {args} {
    set prev [lindex $args end]
    foreach r [lreverse [lrange $args 0 end-1]] {
        set prev [copy-list $r $prev]
    }
    set prev
}


proc ::constcl::copy-list {pair next} {
    # TODO only fresh conses in the direct chain to NIL
    ::if {[null? $pair] ne "#f"} {
        set next
    } elseif {[null? [cdr $pair]] ne "#f"} {
        cons [car $pair] $next
    } else {
        cons [car $pair] [copy-list [cdr $pair] $next]
    }
}





reg reverse ::constcl::reverse

proc ::constcl::reverse {vals} {
    list {*}[lreverse [splitlist $vals]]
}





reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {vals k} {
    ::if {[zero? $k] ne "#f"} {
        return $vals
    } else {
        list-tail [cdr $vals] [- $k #1]
    }
}





reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {vals k} {
    car [list-tail $vals $k]
}





reg memq ::constcl::memq

proc ::constcl::memq {val1 val2} {
    return [member-proc eq? $val1 $val2]
}



reg memv ::constcl::memv

proc ::constcl::memv {val1 val2} {
    return [member-proc eqv? $val1 $val2]
}


reg member ::constcl::member

proc ::constcl::member {val1 val2} {
    return [member-proc equal? $val1 $val2]
}



proc ::constcl::member-proc {epred val1 val2} {
    switch $epred {
        eq? { set name "memq" }
        eqv? { set name "memv" }
        equal? { set name "member" }
    }
    check {list? $val2} {LIST expected\n($name [$val1 show] [$val2 show])}
    ::if {[null? $val2] ne "#f"} {
        return #f
    } elseif {[pair? $val2] ne "#f"} {
        ::if {[$epred $val1 [car $val2]] ne "#f"} {
            return $val2
        } else {
            return [member-proc $epred $val1 [cdr $val2]]
        }
    }
}



reg assq

proc ::constcl::assq {val1 val2} {
    return [assoc-proc eq? $val1 $val2]
}



reg assv

proc ::constcl::assv {val1 val2} {
    return [assoc-proc eqv? $val1 $val2]
}



reg assoc

proc ::constcl::assoc {val1 val2} {
    return [assoc-proc equal? $val1 $val2]
}


proc ::constcl::assoc-proc {epred val1 val2} {
    switch $epred {
        eq? { set name "assq" }
        eqv? { set name "assv" }
        equal? { set name "assoc" }
    }
    check {list? $val2} {LIST expected\n($name [$val1 show] [$val2 show])}
    ::if {[null? $val2] ne "#f"} {
        return #f
    } elseif {[pair? $val2] ne "#f"} {
        ::if {[pair? [car $val2]] ne "#f" && [$epred $val1 [caar $val2]] ne "#f"} {
            return [car $val2]
        } else {
            return [assoc-proc $epred $val1 [cdr $val2]]
        }
    }
}


# vim: ft=tcl tw=80


oo::class create ::constcl::String {
    superclass ::constcl::NIL
    variable data constant
    constructor {v} {
        set len [::string length $v]
        set vsa [::constcl::vsAlloc $len]
        set idx $vsa
        foreach elt [split $v {}] {
            ::if {$elt eq " "} {
                set c #\\space
            } elseif {$elt eq "\n"} {
                set c #\\newline
            } else {
                set c #\\$elt
            }
            lset ::constcl::vectorSpace $idx [::constcl::MkChar $c]
            incr idx
        }
        set data [::constcl::cons [::constcl::MkNumber $vsa] [::constcl::MkNumber $len]]
        set constant 0
    }
    method = {str} {::string equal [my value] [$str value]}
    method cmp {str} {::string compare [my value] [$str value]}
    method length {} {::constcl::cdr $data}
    method ref {k} {
        set k [$k numval]
        ::if {$k < 0 || $k >= [[my length] numval]} {
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
        ::if {[my constant]} {
            ::error "string is constant"
        } else {
            set k [$k numval]
            ::if {$k < 0 || $k >= [[my length] numval]} {
                ::error "index out of range\n$k"
            }
            set base [[::constcl::car $data] numval]
            lset ::constcl::vectorSpace $k+$base $c
        }
        return [self]
    }
    method fill! {c} {
        ::if {[my constant]} {
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
    method mkconstant {} {set constant 1}
    method constant {} {set constant}
    method write {} { puts -nonewline "\"[my value]\"" }
    method show {} {format "\"[my value]\""}
}

interp alias {} MkString {} ::constcl::String new


reg string? ::constcl::string?

proc ::constcl::string? {val} {
    ::if {[info object isa typeof $val ::constcl::String]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::String]} {
        return #t
    } else {
        return #f
    }
}





reg make-string ::constcl::make-string

proc ::constcl::make-string {k args} {
    ::if {[llength $args] == 0} {
        return [MkString [::string repeat " " [$k numval]]]
    } else {
        lassign $args char
        return [MkString [::string repeat [$char char] [$k numval]]]
    }
}





reg string ::constcl::string

proc ::constcl::string {args} {
    set str {}
    foreach char $args {
        check {::constcl::char? $char} {CHAR expected\n([pn] [lmap c $args {$c show}])}
        ::append str [$char char]
    }
    return [MkString $str]
}





reg string-length ::constcl::string-length

proc ::constcl::string-length {str} {
    check {::constcl::string? $str} {STRING expected\n([pn] [$str show])}
    return [MkNumber [[$str length] numval]]
}





reg string-ref ::constcl::string-ref

proc ::constcl::string-ref {str k} {
    check {::constcl::string? $str} {STRING expected\n([pn] [$str show] [$k show])}
    check {::constcl::number? $k} {Exact INTEGER expected\n([pn] [$str show] [$k show])}
    return [$str ref $k]
}





reg string-set!

proc ::constcl::string-set! {str k char} {
    check {string? $str} {STRING expected\n([pn] [$str show] [$k show] [$char show])}
    check {number? $k} {Exact INTEGER expected\n([pn] [$str show] [$k show] [$char show])}
    check {char? $char} {CHAR expected\n([pn] [$str show] [$k show] [$char show])}
    $str set! $k $char
    return $str
}





reg string=? ::constcl::string=?

proc ::constcl::string=? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[$str1 value] eq [$str2 value]} {
        return #t
    } else {
        return #f
    }
}


reg string-ci=? ::constcl::string-ci=?

proc ::constcl::string-ci=? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[::string tolower [$str1 value]] eq [::string tolower [$str2 value]]} {
        return #t
    } else {
        return #f
    }
}


reg string<? ::constcl::string<?

proc ::constcl::string<? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[$str1 value] < [$str2 value]} {
        return #t
    } else {
        return #f
    }
}


reg string-ci<? ::constcl::string-ci<?

proc ::constcl::string-ci<? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[::string tolower [$str1 value]] < [::string tolower [$str2 value]]} {
        return #t
    } else {
        return #f
    }
}


reg string>? ::constcl::string>?

proc ::constcl::string>? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[$str1 value] > [$str2 value]} {
        return #t
    } else {
        return #f
    }
}


reg string-ci>? ::constcl::string-ci>?

proc ::constcl::string-ci>? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[::string tolower [$str1 value]] > [::string tolower [$str2 value]]} {
        return #t
    } else {
        return #f
    }
}


reg string<=? ::constcl::string<=?

proc ::constcl::string<=? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[$str1 value] <= [$str2 value]} {
        return #t
    } else {
        return #f
    }
}


reg string-ci<=? ::constcl::string-ci<=?

proc ::constcl::string-ci<=? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[::string tolower [$str1 value]] <= [::string tolower [$str2 value]]} {
        return #t
    } else {
        return #f
    }
}


reg string>=? ::constcl::string>=?

proc ::constcl::string>=? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[$str1 value] >= [$str2 value]} {
        return #t
    } else {
        return #f
    }
}


reg string-ci>=? ::constcl::string-ci>=?

proc ::constcl::string-ci>=? {str1 str2} {
    check {string? $str1} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    check {string? $str2} {STRING expected\n([pn] [$str1 show] [$str2 show])}
    ::if {[::string tolower [$str1 value]] >= [::string tolower [$str2 value]]} {
        return #t
    } else {
        return #f
    }
}





reg substring ::constcl::substring

proc ::constcl::substring {str start end} {
    check {string? $str} {STRING expected\n([pn] [$str show] [$start show] [$end show])}
    check {number? $start} {NUMBER expected\n([pn] [$str show] [$start show] [$end show])}
    check {number? $end} {NUMBER expected\n([pn] [$str show] [$start show] [$end show])}
    return [MkString [$str substring $start $end]]
}





reg string-append ::constcl::string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {$arg value}]]
}





reg string->list ::constcl::string->list

proc ::constcl::string->list {str} {
    list {*}[$str store]
}





reg list->string ::constcl::list->string

proc ::constcl::list->string {list} {
    MkString [::append --> {*}[lmap c [splitlist $list] {$c char}]]
}





reg string-copy ::constcl::string-copy

proc ::constcl::string-copy {str} {
    check {string? $str} {STRING expected\n([pn] [$str show])}
    return [MkString [$str value]]
}





reg string-fill! ::constcl::string-fill!

proc ::constcl::string-fill! {str char} {
    check {string? $str} {STRING expected\n([pn] [$str show] [$char show])}
    $str fill! $char
    return $str
}


# vim: ft=tcl tw=80


oo::class create ::constcl::Symbol {
    superclass ::constcl::NIL
    variable name caseconstant
    constructor {n} {
        ::if {$n eq {}} {
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
    method write {} { puts -nonewline [my name] }
    method show {} {set name}
}

proc ::constcl::MkSymbol {n} {
    foreach instance [info class instances ::constcl::Symbol] {
        ::if {[$instance name] eq $n} {
            return $instance
        }
    }
    return [::constcl::Symbol new $n]
}


reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {val} {
    ::if {[info object isa typeof $val ::constcl::Symbol]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Symbol]} {
        return #t
    } else {
        return #f
    }
}




reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {sym} {
    check {symbol? $sym} {SYMBOL expected\n([pn] [$sym show])}
    ::if {![$sym case-constant]} {
        set str [MkString [::string tolower [$sym name]]]
    } else {
        set str [MkString [$sym name]]
    }
    $str mkconstant
    return $str
}






reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {str} {
    check {string? $str} {STRING expected\n([pn] [$obj show])}
    set sym [MkSymbol [$str value]]
    $sym make-case-constant
    return $sym
}

# vim: ft=tcl tw=80


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
        ::if {$k < 0 || $k >= [[my length] numval]} {
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
        ::if {[my constant]} {
            ::error "vector is constant"
        } else {
            set k [$k numval]
            ::if {$k < 0 || $k >= [[my length] numval]} {
                ::error "index out of range\n$k"
            }
            set base [[::constcl::car $data] numval]
            lset ::constcl::vectorSpace $k+$base $obj
        }
        return [self]
    }
    method fill! {val} {
        ::if {[my constant]} {
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
    method write {} {puts -nonewline [my show]}
    method show {} {format "#(%s)" [join [lmap val [my value] {$val show}] " "]}
}

interp alias {} ::constcl::MkVector {} ::constcl::Vector new



reg vector? ::constcl::vector?

proc ::constcl::vector? {val} {
    ::if {[info object isa typeof $val ::constcl::Vector]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Vector]} {
        return #t
    } else {
        return #f
    }
}





reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {k args} {
    ::if {[llength $args] == 0} {
        set fill #NIL
    } else {
        lassign $args fill
    }
    MkVector [lrepeat [$k numval] $fill]
}




reg vector ::constcl::vector

proc ::constcl::vector {args} {
    MkVector $args
}





reg vector-length

proc ::constcl::vector-length {vec} {
    check {vector? $vec} {VECTOR expected\n([pn] [$vec show])}
    return [$vec length]
}





reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    check {vector? $vec} {VECTOR expected\n([pn] [$vec show] [$k show])}
    check {number? $k} {NUMBER expected\n([pn] [$vec show] [$k show])}
    return [$vec ref $k]
}





reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k val} {
    check {vector? $vec} {VECTOR expected\n([pn] [$vec show] [$k show])}
    check {number? $k} {NUMBER expected\n([pn] [$vec show] [$k show])}
    return [$vec set! $k $val]
}





reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
    list {*}[$vec value]
}





reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
    vector {*}[splitlist $list]
}





reg vector-fill! ::constcl::vector-fill!

proc ::constcl::vector-fill! {vec fill} {
    check {vector? $vec} {VECTOR expected\n([pn] [$vec show] [$fill show])}
    $vec fill! $fill
}


# vim: ft=tcl tw=80


proc ::constcl::idcheckinit {init} {
    ::if {[::string is alpha -strict $init] || $init in {! $ % & * / : < = > ? ^ _ ~}} {
        return true
    } else {
        return false
    }
}

proc ::constcl::idchecksubs {subs} {
    foreach c [split $subs {}] {
        ::if {!([::string is alnum -strict $c] || $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
            return false
        }
    }
    return true
}

proc ::constcl::idcheck {sym} {
    ::if {(![idcheckinit [::string index $sym 0]] ||
        ![idchecksubs [::string range $sym 1 end]]) && $sym ni {+ - ...}} {
        ::error "Identifier expected ($sym)"
    }
    set sym
}

proc ::constcl::varcheck {sym} {
    ::if {$sym in {else => define unquote unquote-splicing quote lambda if set! begin
        cond and or case let let* letrec do delay quasiquote}} {
            ::error "Macro name can't be used as a variable: $sym"
    }
    return $sym
}

# vim: ft=tcl tw=80

unset -nocomplain ::constcl::vectorSpace
set ::constcl::vectorSpace [lrepeat 1024 #NIL]

unset -nocomplain ::constcl::vectorAssign
set ::constcl::vectorAssign 0

proc ::constcl::vsAlloc {num} {
    # TODO calculate free space
    set va $::constcl::vectorAssign
    incr ::constcl::vectorAssign $num
    return $va
}


interp alias {} #NIL {} [::constcl::NIL new]

interp alias {} #t {} [::constcl::MkBoolean #t]

interp alias {} #f {} [::constcl::MkBoolean #f]

interp alias {} #-1 {} [::constcl::MkNumber -1]

interp alias {} #0 {} [::constcl::MkNumber 0]

interp alias {} #1 {} [::constcl::MkNumber 1]

interp alias {} #B {} [::constcl::MkSymbol begin]

interp alias {} #I {} [::constcl::MkSymbol if]

interp alias {} #L {} [::constcl::MkSymbol let]

interp alias {} #Q {} [::constcl::MkSymbol quote]

interp alias {} #U {} [::constcl::MkSymbol unquote]

interp alias {} #S {} [::constcl::MkSymbol set!]

interp alias {} #x {} [::constcl::MkSymbol x]

interp alias {} #y {} [::constcl::MkSymbol y]

interp alias {} #λ {} [::constcl::MkSymbol lambda]

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #NONE {} [::constcl::None new]

interp alias {} #UNSP {} [::constcl::Unspecific new]

interp alias {} #UNDF {} [::constcl::Undefined new]



dict set ::constcl::defreg pi [::constcl::MkNumber 3.1415926535897931]


reg nil #NIL



reg atom? ::constcl::atom?

proc ::constcl::atom? {val} {
    ::if {[symbol? $val] ne "#f" || [number? $val] ne "#f" || [string? $val] ne "#f" || [char? $val] ne "#f" || [boolean? $val] ne "#f" || [vector? $val] ne "#f"} {
        return #t
    } else {
        return #f
    }
}


# vim: ft=tcl tw=80




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


proc ::constcl::repl {{prompt "ConsTcl> "}} {
    set str [input $prompt]
    while {$str ne ""} {
        write [eval [parse $str]]
        set str [input $prompt]
    }
}

# vim: ft=tcl tw=80

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

# vim: set filetype=tcl:


::constcl::Environment create ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
    method find {sym} {self}
    method get {sym} {::error "Unbound variable: [$sym name]"}
    method set {sym val} {::error "Unbound variable: [$sym name]"}
}


namespace eval ::constcl {
    set keys [list {*}[lmap k [dict keys $defreg] {MkSymbol $k}]]
    set vals [dict values $defreg]
    Environment create global_env $keys $vals ::constcl::null_env
}




# vim: ft=tcl tw=80
