



namespace eval ::constcl {}


# utility functions
proc ::reg {key args} {
    if {[llength $args] == 0} {
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
    ::constcl::expand-macro op args ::constcl::global_env
    set val [::constcl::cons $op $args]
    ::constcl::write $val
}


reg in-range ::constcl::in-range

#started out as DKF's code
proc ::constcl::tcl-in-range {args} {
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
    return [lmap r $res {MkNumber $r}]
}

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
    method car {} {error "PAIR expected"}
    method cdr {} {error "PAIR expected"}
    method set-car! {v} {error "PAIR expected"}
    method set-cdr! {v} {error "PAIR expected"}
    method numval {} {error "Not a number"}
    method write {} {puts -nonewline "()"}
    method show {} {format "()"}
}


reg null? ::constcl::null?

proc ::constcl::null? {obj} {
    if {$obj eq "#NIL"} {
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
    if {[info object isa typeof $obj Dot]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Dot]} {
        return #t
    } else {
        return #f
    }
}





catch { ib destroy }

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
        if {$buffer eq {}} {
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
        if {[::string is space $peekc]} {
            for {set cp 0} {$cp < [::string length $buffer]} {incr cp} {
                if {![::string is space [::string index $buffer $cp]]} {
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


proc ::constcl::parse {args} {
    if {[llength $args]} {
        ib fill [lindex $args 0]
    }
    return [parse-value]
}


reg read ::constcl::read

proc ::constcl::read {args} {
    return [parse-value]
}


proc ::constcl::parse-value {} {
    ib skip-ws
    switch -regexp [ib first] {
        {^$}          { return }
        {\"}          { return [parse-string] }
        {\#}          { return [parse-sharp] }
        {\'}          { return [parse-quoted-value] }
        {\(}          { return [parse-pair-value ")"] }
        {\+} - {\-}   { return [parse-plus-minus] }
        {\,}          { return [parse-unquoted-value] }
        {\.}          { ib advance ; return [Dot new] }
        {\[}          { return [parse-pair-value "\]"] }
        {\`}          { return [parse-quasiquoted-value] }
        {\d}          { return [parse-number] }
        {[[:space:]]} { ib advance }
        {[[:graph:]]} { return [parse-identifier] }
        default {
            error "unexpected char [ib first]"
        }
    }
}


proc ::constcl::parse-string {} {
    set str {}
    ib advance
    while {[ib first] ne {"}} {
        set c [ib first]
        if {$c eq "\\"} {
            ib advance
            ::append str [ib first]
        } else {
            ::append str $c
        }
        ib advance
    }
    ib advance
    ib skip-ws
    set obj [MkString $str]
    $obj mkconstant
    return $obj
}



proc ::constcl::parse-sharp {} {
    ib advance
    switch [ib first] {
        (    { return [parse-vector] }
        t    { ib advance ; ib skip-ws ; return #t }
        f    { ib advance ; ib skip-ws ; return #f }
        "\\" { return [parse-character] }
        default {
            error "Illegal #-literal"
        }
    }
}


proc ::constcl::make-constant {obj} {
    if {[pair? $obj] eq "#t"} {
        $obj mkconstant
        make-constant [car $obj]
        make-constant [cdr $obj]
    } elseif {[null? $obj] eq "#t"} {
        return #NIL
    } else {
        $obj mkconstant
    }
}


proc ::constcl::parse-quoted-value {} {
    ib advance
    set val [parse-value]
    ib skip-ws
    make-constant $val
    return [list #Q $val]
}




proc ::constcl::parse-pair {char} {
    if {[ib find $char]} {
        return #NIL
    }
    ib skip-ws
    set a [parse-value]
    ib skip-ws
    set res $a
    set prev #NIL
    while {![ib find $char]} {
        set x [parse-value]
        ib skip-ws
        if {[dot? $x] eq "#t"} {
            set prev [parse-value]
            ib skip-ws
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

proc ::constcl::parse-pair-value {char} {
    ib advance
    ib skip-ws
    set val [parse-pair $char]
    ib skip-ws
    if {[ib first] ne $char} {
        if {$char eq ")"} {
            error "Missing right parenthesis (first=[ib first])."
        } else {
            error "Missing right bracket (first=[ib first])."
        }
    }
    ib advance
    ib skip-ws
    return $val
}



proc ::constcl::parse-plus-minus {} {
    set c [ib first]
    ib advance
    if {[::string is digit [ib first]]} {
        ib unget $c
        return [::constcl::parse-number]
    } else {
        if {$c eq "+"} {
            ib skip-ws
            return [MkSymbol +]
        } else {
            ib skip-ws
            return [MkSymbol -]
        }
    }
}


proc ::constcl::parse-unquoted-value {} {
    ib advance
    set symbol "unquote"
    if {[ib first] eq "@"} {
        set symbol "unquote-splicing"
        ib advance
    }
    set val [parse-value]
    ib skip-ws
    return [list [MkSymbol $symbol] $val]
}



proc ::constcl::parse-quasiquoted-value {} {
    ib advance
    set val [parse-value]
    ib skip-ws
    make-constant $val
    return [list [MkSymbol "quasiquote"] $val]
}



proc ::constcl::parse-number {} {
    while {[ib first] ne {} && ![::string is space [ib first]] && [ib first] ni {) \]}} {
        ::append num [ib first]
        ib advance
    }
    ib skip-ws
    if {[::string is double $num]} {
        return [MkNumber $num]
    } else {
        error "Invalid numeric constant $num"
    }
}



proc ::constcl::parse-identifier {} {
    ::append name [ib first]
    ib advance
    while {[ib first] ne {} && ![::string is space [ib first]] && [ib first] ni {) \]}} {
        ::append name [ib first]
        ib advance
    }
    ib skip-ws
    # idcheck throws error if invalid identifier
    return [MkSymbol [idcheck $name]]
}



proc ::constcl::character-check {name} {
    regexp -nocase {^#\\([[:graph:]]|space|newline)$} $name
}


proc ::constcl::parse-character {} {
    set name "#"
    while {[ib first] ne {} && ![::string is space [ib first]] && [ib first] ni {) ]}} {
        ::append name [ib first]
        ib advance
    }
    if {[::constcl::character-check $name]} {
        return [MkChar $name]
    } else {
        error "Invalid character constant $name"
    }
    ib skip-ws
}



proc ::constcl::parse-vector {} {
    ib advance
    ib skip-ws
    set res {}
    while {[ib first] ne {} && [ib first] ne ")"} {
        lappend res [parse-value]
        ib skip-ws
    }
    set vec [MkVector $res]
    $vec mkconstant
    if {[ib first] ne ")"} {
        error "Missing right parenthesis (first=[ib first])."
    }
    ib advance
    ib skip-ws
    return $vec
}



reg eval ::constcl::eval

proc ::constcl::eval {e {env ::constcl::global_env}} {
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
        while {[$op name] in {and case cond for for/and for/list for/or let or quasiquote}} {
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


proc ::constcl::lookup {sym env} {
    [$env find $sym] get $sym
}



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


proc ::constcl::declare {sym val env} {
    varcheck [idcheck [$sym name]]
    $env set $sym $val
    return #NONE
}


proc ::constcl::update! {var expr env} {
    [$env find $var] set $var $expr
    set expr
}


proc ::constcl::make-function {formals body env} {
    if {[[length $body] value] > 1} {
        set body [cons #B $body]
    } else {
        set body [car $body]
    }
    return [MkProcedure $formals $body $env]
}


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


proc ::constcl::splitlist {vals} {
    set result {}
    while {[pair? $vals] eq "#t"} {
        lappend result [car $vals]
        set vals [cdr $vals]
    }
    return $result
}


proc ::constcl::evlis {exps env} {
    if {[pair? $exps] eq "#t"} {
        return [cons [eval [car $exps] $env] [evlis [cdr $exps] $env]]
    } else {
        return #NIL
    }
}


proc ::constcl::expand-macro {n1 n2 env} {
    upvar $n1 op $n2 args
    switch [$op name] {
        and {
            set val [expand-and $args]
        }
        case {
            set val [do-case [car $args] [cdr $args]]
        }
        cond {
            set val [do-cond $args]
        }
        for {
            set val [expand-for $args $env]
        }
        for/and {
            set val [expand-for/and $args $env]
        }
        for/list {
            set val [expand-for/list $args $env]
        }
        for/or {
            set val [expand-for/or $args $env]
        }
        let {
            set val [expand-let $args]
        }
        or {
            set val [expand-or $args]
        }
        quasiquote {
            set val [expand-quasiquote $args $env]
        }
    }
    set op [car $val]
    set args [cdr $val]
}


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


proc ::constcl::do-cond {clauses} {
    if {[eq? [length $clauses] #0] eq "#t"} {
        return [list #Q #NIL]
    } elseif {[eq? [length $clauses] #1] eq "#t"} {
        set pred [caar $clauses]
        set body [cdar $clauses]
        if {[symbol? [car $body]] eq "#t" && [$body name] eq "=>"} {
            set body [cddar $clauses]
        }
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


proc ::constcl::for-seq {seq env} {
    if {[number? $seq] eq "#t"} {
        set seq [in-range $seq]
    } else {
        set seq [eval $seq $env]
    }
    if {[list? $seq] eq "#t"} {
        set seq [splitlist $seq]
    } elseif {[string? $seq] eq "#t"} { 
        set seq [lmap c [split [$seq value] {}] {MkChar #\\$c}]
    } elseif {[vector? $seq] eq "#t"} {
        set seq [$seq value]
    }
}

proc ::constcl::do-for {exps env} {
    #single-clause
    set clauses [splitlist [car $exps]]
    set body [cdr $exps]
    set ids {}
    set seqs {}
    for {set i 0} {$i < [llength $clauses]} {incr i} {
        set clause [lindex $clauses $i]
        lset ids $i [car $clause]
        lset seqs $i [for-seq [cadr $clause] $env]
    }
    set res {}
    for {set item 0} {$item < [llength [lindex $seqs 0]]} {incr item} {
        set x {}
        for {set clause 0} {$clause < [llength $clauses]} {incr clause} {
            lappend x [list [lindex $ids $clause] [lindex $seqs $clause $item]]
        }
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


proc ::constcl::qq-visit-child {node qqlevel env} {
    if {$qqlevel < 0} {
        set qqlevel 0
    }
    if {[list? $node] eq "#t"} {
        set res {}
        foreach child [splitlist $node] {
            if {[pair? $child] eq "#t" && [eq? [car $child] [MkSymbol "unquote"]] eq "#t"} {
                if {$qqlevel == 0} {
                    lappend res [eval [cadr $child] $env]
                } else {
                    lappend res [list #U [qq-visit-child [cadr $child] [expr {$qqlevel - 1}] $env]]
                }
            } elseif {[pair? $child] eq "#t" && [eq? [car $child] [MkSymbol "unquote-splicing"]] eq "#t"} {
                if {$qqlevel == 0} {
                    lappend res {*}[splitlist [eval [cadr $child] $env]]
                }
            } elseif {[pair? $child] eq "#t" && [eq? [car $child] [MkSymbol "quasiquote"]] eq "#t"} {
                lappend res [list [MkSymbol "quasiquote"] [car [qq-visit-child [cdr $child] [expr {$qqlevel + 1}] $env]]] 
            } elseif {[atom? $child] eq "#t"} {
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
    if {[list? [car $exps]] eq "#t"} {
        set node [car $exps]
        return [qq-visit-child $node 0 $env]
    } elseif {[vector? [car $exps]] eq "#t"} {
        set vect [car $exps]
        set res {}
        for {set i 0} {$i < [[vector-length $vect] numval]} {incr i} {
            if {[pair? [vector-ref $vect [MkNumber $i]]] eq "#t" && [eq? [car [vector-ref $vect [MkNumber $i]]] [MkSymbol "unquote"]] eq "#t"} {
                if {$qqlevel == 0} {
                    lappend res [eval [cadr [vector-ref $vect [MkNumber $i]]] $env]
                }
            } elseif {[pair? [vector-ref $vect [MkNumber $i]]] eq "#t" && [eq? [car [vector-ref $vect [MkNumber $i]]] [MkSymbol "unquote-splicing"]] eq "#t"} {
                if {$qqlevel == 0} {
                    lappend res {*}[splitlist [eval [cadr [vector-ref $vect [MkNumber $i]]] $env]]
                }
            } elseif {[atom? [vector-ref $vect [MkNumber $i]]] eq "#t"} {
                lappend res [vector-ref $vect [MkNumber $i]]
            } else {
            }
        }
        return [list [MkSymbol "vector"] {*}$res]
    }
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



reg write ::constcl::write

proc ::constcl::write {obj args} {
    if {$obj ne "#NONE"} {
        ::constcl::write-value $obj
        puts {}
    }
}


proc ::constcl::write-value {obj} {
    $obj write
}


reg display ::constcl::display

proc ::constcl::display {obj args} {
    ::constcl::write-value $obj
    flush stdout
}


proc ::constcl::write-pair {obj} {
    # take an object and print the car and the cdr of the stored value
    set a [car $obj]
    set d [cdr $obj]
    # print car
    write-value $a
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d
    } elseif {$d eq "#NIL"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write-value $d
    }
}



reg eq? ::constcl::eq?

proc ::constcl::eq? {obj1 obj2} {
    if {[boolean? $obj1] eq "#t" && [boolean? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[symbol? $obj1] eq "#t" && [symbol? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[number? $obj1] eq "#t" && [number? $obj2] eq "#t" && [$obj1 value] eq [$obj2 value]} {
        return #t
    } elseif {[char? $obj1] eq "#t" && [char? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[null? $obj1] eq "#t" && [null? $obj2] eq "#t"} {
        return #t
    } elseif {[pair? $obj1] eq "#t" && [pair? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[string? $obj1] eq "#t" && [string? $obj2] eq "#t" && [$obj1 index] eq [$obj2 index]} {
        return #t
    } elseif {[vector? $obj1] eq "#t" && [vector? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[procedure? $obj1] eq "#t" && [procedure? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } else {
        return #f
    }
}

reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {obj1 obj2} {
    if {[boolean? $obj1] eq "#t" && [boolean? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } elseif {[symbol? $obj1] eq "#t" && [symbol? $obj2] eq "#t" && [$obj1 name] eq [$obj2 name]} {
        return #t
    } elseif {[number? $obj1] eq "#t" && [number? $obj2] eq "#t" && [$obj1 value] eq [$obj2 value]} {
        return #t
    } elseif {[char? $obj1] eq "#t" && [char? $obj2] eq "#t" && [$obj1 char] eq [$obj2 char]} {
        return #t
    } elseif {[null? $obj1] eq "#t" && [null? $obj2] eq "#t"} {
        return #t
    } elseif {[pair? $obj1] eq "#t" && [pair? $obj2] eq "#t" && [$obj1 car] eq [$obj2 car] && [$obj1 cdr] eq [$obj2 cdr]} {
        return #t
    } elseif {[string? $obj1] eq "#t" && [string? $obj2] eq "#t" && [$obj1 index] eq [$obj2 index]} {
        return #t
    } elseif {[vector? $obj1] eq "#t" && [vector? $obj2] eq "#t" && [$obj1 value] eq [$obj2 value]} {
        return #t
    } elseif {[procedure? $obj1] eq "#t" && [procedure? $obj2] eq "#t" && $obj1 eq $obj2} {
        return #t
    } else {
        return #f
    }
}

reg equal? ::constcl::equal?

proc ::constcl::equal? {obj1 obj2} {
    if {[$obj1 show] eq [$obj2 show]} {
        return #t
    } else {
        return #f
    }
    # TODO
}



oo::class create ::constcl::Number {
    superclass ::constcl::NIL
    variable value
    constructor {v} {
        if {[::string is double $v]} {
            set value $v
        } else {
            error "NUMBER expected\n$v"
        }
    }
    method zero? {} {if {$value == 0} then {return #t} else {return #f}}
    method positive? {} {if {$value > 0} then {return #t} else {return #f}}
    method negative? {} {if {$value < 0} then {return #t} else {return #f}}
    method even? {} {if {$value % 2 == 0} then {return #t} else {return #f}}
    method odd? {} {if {$value % 2 == 1} then {return #t} else {return #f}}
    method value {} { set value }
    method numval {} {set value}
    method mkconstant {} {}
    method constant {} {return 1}
    method write {} { puts -nonewline [my value] }
    method show {} { set value }
}

interp alias {} ::constcl::MkNumber {} ::constcl::Number new


reg number? ::constcl::number?

proc ::constcl::number? {obj} {
    if {[info object isa typeof $obj ::constcl::Number]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Number]} {
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
        error "NUMBER expected\n(= num ...)"
    }
    if {[::tcl::mathop::== {*}$vals]} {
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
        error "NUMBER expected\n(< num ...)"
    }
    if {[::tcl::mathop::< {*}$vals]} {
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
        error "NUMBER expected\n(> num ...)"
    }
    if {[::tcl::mathop::> {*}$vals]} {
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
        error "NUMBER expected\n(<= num ...)"
    }
    if {[::tcl::mathop::<= {*}$vals]} {
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
        error "NUMBER expected\n(>= num ...)"
    }
    if {[::tcl::mathop::>= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}



reg zero? ::constcl::zero?

proc ::constcl::zero? {obj} {
    if {[number? $obj] eq "#t"} {
        return [$obj zero?]
    } else {
        error "NUMBER expected\n(zero? [$obj show])"
    }
}



reg positive? ::constcl::positive?

proc ::constcl::positive? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        return [$obj positive?]
    } else {
        error "NUMBER expected\n(positive? [$obj show])"
    }
}


reg negative? ::constcl::negative?

proc ::constcl::negative? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        return [$obj negative?]
    } else {
        error "NUMBER expected\n(negative? [$obj show])"
    }
}


reg even? ::constcl::even?

proc ::constcl::even? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        return [$obj even?]
    } else {
        error "NUMBER expected\n(even? [$obj show])"
    }
}


reg odd? ::constcl::odd?

proc ::constcl::odd? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        return [$obj odd?]
    } else {
        error "NUMBER expected\n(odd? [$obj show])"
    }
}



reg max ::constcl::max

proc ::constcl::max {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(max num...)"
    }
    MkNumber [::tcl::mathfunc::max {*}$vals]
}


reg min ::constcl::min

proc ::constcl::min {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(min num...)"
    }
    MkNumber [::tcl::mathfunc::min {*}$vals]
}



reg + ::constcl::+

proc ::constcl::+ {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(+ num ...)"
    }
    MkNumber [::tcl::mathop::+ {*}$vals]
}


reg * ::constcl::*

proc ::constcl::* {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(* num ...)"
    }
    MkNumber [::tcl::mathop::* {*}$vals]
}


reg - ::constcl::-

proc ::constcl::- {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(- num ...)"
    }
    if {[llength $vals] == 0} {
        error "wrong # args: should be \"- value ?value ...?\""
    }
    MkNumber [::tcl::mathop::- {*}$vals]
}


reg / ::constcl::/

proc ::constcl::/ {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(/ num ...)"
    }
    if {[llength $vals] == 0} {
        error "wrong # args: should be \"/ value ?value ...?\""
    }
    MkNumber [::tcl::mathop::/ {*}$vals]
}



reg abs ::constcl::abs

proc ::constcl::abs {x} {
    if {[::constcl::number? $x] eq "#t"} {
        if {[$x negative?] eq "#t"} {
            return [MkNumber [expr {[$x numval] * -1}]]
        } else {
            return $x
        }
    } else {
        error "NUMBER expected\n(abs [$x show])"
    }
}


reg quotient

proc ::constcl::quotient {n1 n2} {
    set q [::tcl::mathop::/ [$n1 numval] [$n2 numval]]
    if {$q > 0} {
        return [MkNumber [::tcl::mathfunc::floor $q]]
    } elseif {$q < 0} {
        return [MkNumber [::tcl::mathfunc::ceil $q]]
    } else {
        return #0
    }
}

reg remainder

proc ::constcl::remainder {n1 n2} {
    set n [::tcl::mathop::% [[abs $n1] numval] [[abs $n2] numval]]
    if {[$n1 negative?] eq "#t"} {
        set n -$n
    }
    return [MkNumber $n]
}

reg modulo

proc ::constcl::modulo {n1 n2} {
    return [MkNumber [::tcl::mathop::% [$n1 numval] [$n2 numval]]]
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

proc ::constcl::floor {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::floor [$x numval]]
    } else {
        error "NUMBER expected\n(floor [$x show])"
    }
}


reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::ceil [$x numval]]
    } else {
        error "NUMBER expected\n(ceiling [$x show])"
    }
}


reg truncate ::constcl::truncate

proc ::constcl::truncate {x} {
    if {[::constcl::number? $x] eq "#t"} {
        if {[$x negative?] eq "#t"} {
            MkNumber [::tcl::mathfunc::ceil [$x numval]]
        } else {
            MkNumber [::tcl::mathfunc::floor [$x numval]]
        }
    } else {
        error "NUMBER expected\n(truncate [$x show])"
    }
}


reg round ::constcl::round

proc ::constcl::round {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::round [$x numval]]
    } else {
        error "NUMBER expected\n(round [$x show])"
    }
}


proc ::constcl::rationalize {x y} {
    # TODO
}


reg exp ::constcl::exp

proc ::constcl::exp {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::exp [$z numval]]
    } else {
        error "NUMBER expected\n(exp [$z show])"
    }
}


reg log ::constcl::log

proc ::constcl::log {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::log [$z numval]]
    } else {
        error "NUMBER expected\n(log [$z show])"
    }
}


reg sin ::constcl::sin

proc ::constcl::sin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sin [$z numval]]
    } else {
        error "NUMBER expected\n(sin [$z show])"
    }
}

reg cos ::constcl::cos

proc ::constcl::cos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::cos [$z numval]]
    } else {
        error "NUMBER expected\n(cos [$z show])"
    }
}

reg tan ::constcl::tan

proc ::constcl::tan {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::tan [$z numval]]
    } else {
        error "NUMBER expected\n(tan [$z show])"
    }
}


reg asin ::constcl::asin

proc ::constcl::asin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::asin [$z numval]]
    } else {
        error "NUMBER expected\n(asin [$z show])"
    }
}

reg acos ::constcl::acos

proc ::constcl::acos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::acos [$z numval]]
    } else {
        error "NUMBER expected\n(acos [$z show])"
    }
}

reg atan ::constcl::atan

proc ::constcl::atan {args} {
    if {[llength $args] == 1} {
        set z [lindex $args 0]
        if {[::constcl::number? $z] eq "#t"} {
            MkNumber [::tcl::mathfunc::atan [$z numval]]
        } else {
            error "NUMBER expected\n(atan [$z show])"
        }
    } else {
        lassign $args y x
        if {[::constcl::number? $y] eq "#t" && [::constcl::number? $x] eq "#t"} {
            MkNumber [::tcl::mathfunc::atan2 [$y numval] [$x numval]]
        } else {
            error "NUMBER expected\n(atan [$y show] [$x show])"
        }
    }
}



reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sqrt [$z numval]]
    } else {
        error "NUMBER expected\n(sqrt [$z show])"
    }
}



reg expt ::constcl::expt

proc ::constcl::expt {z1 z2} {
    if {[::constcl::number? $z1] eq "#t" && [::constcl::number? $z2] eq "#t"} {
        MkNumber [::tcl::mathfunc::pow [$z1 numval] [$z2 numval]]
    } else {
        error "NUMBER expected\n(expt [$z1 show] [$z2 show])"
    }
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

proc ::constcl::number->string {args} {
    if {[llength $args] == 1} {
        set num [lindex $args 0]
        if {[number? $num] eq "#t"} {
            return [MkString [$num numval]]
        } else {
            error "NUMBER expected\n(string->number [$num show])"
        }
    } else {
        lassign $args num radix
        if {[number? $num] eq "#t"} {
            if {[$radix numval] == 10} {
                return [MkString [$num numval]]
            } elseif {[$radix numval] in {2 8 16}} {
                return [MkString [base [$radix numval] [$num numval]]]
            } else {
                error "radix not in 2, 8, 10, 16"
            }
        } else {
            error "NUMBER expected\n(string->number [$num show])"
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
    if $negative {set res -$res}
    set res
}


reg string->number ::constcl::string->number

proc ::constcl::string->number {args} {
    if {[llength $args] == 1} {
        set str [lindex $args 0]
        if {[string? $str] eq "#t"} {
            return [MkNumber [$str value]]
        } else {
            error "STRING expected\n(string->number [$str show])"
        }
    } else {
        lassign $args str radix
        if {[string? $str] eq "#t"} {
            if {[$radix numval] == 10} {
                return [MkNumber [$str value]]
            } elseif {[$radix numval] in {2 8 16}} {
                return [MkNumber [frombase [$radix numval] [$str value]]]
            } else {
                error "radix not in 2, 8, 10, 16"
            }
        } else {
            error "STRING expected\n(string->number [$str show])"
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
        if {$decimalvalue < 0 || $decimalvalue >= $base} {
            error "bad digit $decimalvalue for base $base"
        }
        set res [expr {$res * $base + $decimalvalue}]
    }
    if $negative {set res -$res}
    set res
}




oo::class create ::constcl::Boolean {
    superclass ::constcl::NIL
    variable bvalue
    constructor {v} {
        if {$v ni {#t #f}} {
            error "bad boolean value $v"
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
        if {[$instance bvalue] eq $v} {
            return $instance
        }
    }
    return [::constcl::Boolean new $v]
}



reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {obj} {
    if {[info object isa typeof $obj ::constcl::Boolean]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Boolean]} {
        return #t
    } else {
        return #f
    }
}



reg not ::constcl::not

proc ::constcl::not {obj} {
    if {[$obj bvalue] eq "#f"} {
        return #t
    } else {
        return #f
    }
}




oo::class create ::constcl::Char {
    superclass ::constcl::NIL
    variable value
    constructor {v} {
        if {[regexp {^#\\([[:graph:]]|space|newline)$} $v]} {
            set value $v
        } else {
            error "CHAR expected\n$v"
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
        if {[::string is alpha [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method numeric? {} {
        if {[::string is digit [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method whitespace? {} {
        if {[::string is space [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method upper-case? {} {
        if {[::string is upper [my char]]} {
            return #t
        } else {
            return #f
        }
    }
    method lower-case? {} {
        if {[::string is lower [my char]]} {
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


reg char? ::constcl::char?

proc ::constcl::char? {obj} {
    if {[info object isa typeof $obj ::constcl::Char]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Char]} {
        return #t
    } else {
        return #f
    }
}



reg char=? ::constcl::char=?

proc ::constcl::char=? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {$c1 eq $c2} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 show] [$c2 show])"
    }
}


reg char<? ::constcl::char<?

proc ::constcl::char<? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[$c1 char] < [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 show] [$c2 show])"
    }
}


reg char>? ::constcl::char>?

proc ::constcl::char>? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[$c1 char] > [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 show] [$c2 show])"
    }
}


reg char<=? ::constcl::char<=?

proc ::constcl::char<=? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[$c1 char] <= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 show] [$c2 show])"
    }
}


reg char>=? ::constcl::char>=?

proc ::constcl::char>=? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[$c1 char] >= [$c2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 show] [$c2 show])"
    }
}



reg char-ci=? ::constcl::char-ci=?

proc ::constcl::char-ci=? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] eq [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$c1 show] [$c2 show])"
    }
}


reg char-ci<? ::constcl::char-ci<?

proc ::constcl::char-ci<? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] < [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$c1 show] [$c2 show])"
    }
}


reg char-ci>? ::constcl::char-ci>?

proc ::constcl::char-ci>? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] > [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$c1 show] [$c2 show])"
    }
}


reg char-ci<=? ::constcl::char-ci<=?

proc ::constcl::char-ci<=? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] <= [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$c1 show] [$c2 show])"
    }
}


reg char-ci>=? ::constcl::char-ci>=?

proc ::constcl::char-ci>=? {c1 c2} {
    if {[char? $c1] eq "#t" && [char? $c2] eq "#t"} {
        if {[::string tolower [$c1 char]] >= [::string tolower [$c2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$c1 show] [$c2 show])"
    }
}



reg char-alphabetic? ::constcl::char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
    if {[char? $char] eq "#t"} {
        return [$char alphabetic?]
    } else {
        error "CHAR expected\n(char-alphabetic? [$char show])"
    }
}


reg char-numeric? ::constcl::char-numeric?

proc ::constcl::char-numeric? {char} {
    if {[char? $char] eq "#t"} {
        return [$char numeric?]
    } else {
        error "CHAR expected\n(char-numeric? [$char show])"
    }
}


reg char-whitespace? ::constcl::char-whitespace?

proc ::constcl::char-whitespace? {char} {
    if {[char? $char] eq "#t"} {
        return [$char whitespace?]
    } else {
        error "CHAR expected\n(char-whitespace? [$char show])"
    }
}


reg char-upper-case? ::constcl::char-upper-case?

proc ::constcl::char-upper-case? {char} {
    if {[char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char show])"
    }
}


reg char-lower-case? ::constcl::char-lower-case?

proc ::constcl::char-lower-case? {char} {
    if {[char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char show])"
    }
}



reg char->integer

proc ::constcl::char->integer {char} {
    return [MkNumber [scan [$char char] %c]]
}

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



reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
    if {[char? $char] eq "#t"} {
        if {[regexp {^#\\[[:alpha:]]$} [$char value]]} {
            return [MkChar [::string toupper [$char value]]]
        } else {
            return $char
        }
    } else {
        error "CHAR expected\n(char-upcase [$char show])"
    }
}



reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
    if {[char? $char] eq "#t"} {
        if {[regexp {^#\\[[:alpha:]]$} [$char value]]} {
            return [MkChar [::string tolower [$char value]]]
        } else {
            return $char
        }
    } else {
        error "CHAR expected\n(char-downcase [$char show])"
    }
}




catch { ::constcl::Procedure destroy }

oo::class create ::constcl::Procedure {
    superclass ::constcl::NIL
    variable parms body env
    constructor {p b e} {
        set parms $p         ;# a Lisp list|improper list|symbol denoting parameter names
        set body $b          ;# a Lisp list of expressions under 'begin
        set env $e           ;# an environment
    }
    method value {} {}
    method write {} { puts -nonewline [self] }
    method show {} { return [self] }
    method call {args} {
        ::constcl::eval $body [::constcl::Environment new $parms $args $env]
    }

}

interp alias {} ::constcl::MkProcedure {} ::constcl::Procedure new

reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {obj} {
    if {[info object isa typeof $obj ::constcl::Procedure]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Procedure]} {
        return #t
    } elseif {[::string match "::constcl::*" $obj]} {
        return #t
    } else {
        return #f
    }
}



reg apply ::constcl::apply

proc ::constcl::apply {proc vals} {
    if {[procedure? $proc] eq "#t"} {
        invoke $proc $vals
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}



reg map ::constcl::map

proc ::constcl::map {proc args} {
    if {[procedure? $proc] eq "#t"} {
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
            lappend res [invoke $proc [list {*}$arguments]]
        }
        return [list {*}$res]
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}



reg for-each ::constcl::for-each

proc ::constcl::for-each {proc args} {
    if {[procedure? $proc] eq "#t"} {
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
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
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

if no {
proc ::constcl::write {obj args} {
    # TODO write [$obj write]
}
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
    variable car cdr constant
    constructor {a d} {
        set car $a
        set cdr $d
        set constant 0
    }
    method bvalue {} {return #NIL}
    method name {} {} ;# for eval
    method numval {} {throw "Not a number"}
    method value {} {my show}
    method car {} { set car }
    method cdr {} { set cdr }
    method set-car! {val} {
        if {$constant} {
            error "Can't modify a constant pair"
        } else {
            set car $val
        }
    }
    method set-cdr! {val} {
        if {$constant} {
            error "Can't modify a constant pair"
        } else {
            set cdr $val
        }
    }
    method mkconstant {} {set constant 1}
    method constant {} {return $constant}
    method write {} {
        puts -nonewline "("
        ::constcl::write-pair [self]
        puts -nonewline ")"
    }
    method show {} {format "(%s)" [::constcl::show-pair [self]]}
}


interp alias {} ::constcl::MkPair {} ::constcl::Pair new

reg pair? ::constcl::pair?

proc ::constcl::pair? {obj} {
    if {[info object isa typeof $obj ::constcl::Pair]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Pair]} {
        return #t
    } else {
        return #f
    }
}


proc ::constcl::show-pair {obj} {
    # take an object and print the car and the cdr of the stored value
    set str {}
    set a [car $obj]
    set d [cdr $obj]
    # print car
    ::append str [$a show]
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        ::append str " "
        ::append str [show-pair $d]
    } elseif {$d eq "#NIL"} {
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

proc ::constcl::car {obj} {
    $obj car
}



reg cdr ::constcl::cdr

proc ::constcl::cdr {obj} {
    $obj cdr
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
    reg c${ads}r ::constcl::c${ads}r

    proc ::constcl::c${ads}r {obj} "
        foreach c \[lreverse \[split $ads {}\]\] {
            if {\$c eq \"a\"} {
                set obj \[car \$obj\]
            } else {
                set obj \[cdr \$obj\]
            }
        }
        return \$obj
    "

}


reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {obj val} {
    $obj set-car! $val
}



reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {obj val} {
    $obj set-cdr! $val
}



proc ::constcl::listp {obj} {
    upvar visited visited
    if {$obj in $visited} {
        return #f
    }
    lappend visited $obj
    if {$obj eq "#NIL"} {
        return #t
    } elseif {[pair? $obj] eq "#t"} {
        if {[cdr $obj] eq "#NIL"} {
            return #t
        } else {
            return [listp [cdr $obj]]
        }
    } else {
        return #f
    }
}

reg list? ::constcl::list?

proc ::constcl::list? {obj} {
    set visited {}
    return [listp $obj]
}



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



proc ::constcl::length-helper {obj} {
    if {$obj eq "#NIL"} {
        return 0
    } else {
        return [expr {1 + [length-helper [cdr $obj]]}]
    }
}

reg length ::constcl::length

proc ::constcl::length {obj} {
    if {[list? $obj] eq "#t"} {
        MkNumber [length-helper $obj]
    } else {
        error "LIST expected\n(list lst)"
    }
}



proc ::constcl::copy-list {obj next} {
    # TODO only fresh conses in the direct chain to NIL
    if {[null? $obj] eq "#t"} {
        set next
    } elseif {[null? [cdr $obj]] eq "#t"} {
        cons [car $obj] $next
    } else {
        cons [car $obj] [copy-list [cdr $obj] $next]
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



reg reverse ::constcl::reverse

proc ::constcl::reverse {obj} {
    list {*}[lreverse [splitlist $obj]]
}



reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {obj k} {
    if {[zero? $k] eq "#t"} {
        return $obj
    } else {
        list-tail [cdr $obj] [- $k #1]
    }
}



reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {obj k} {
    car [list-tail $obj $k]
}



reg memq ::constcl::memq

proc ::constcl::memq {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[eq? $obj1 [car $obj2]] eq "#t"} {
                return $obj2
            } else {
                return [memq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memq [$obj1 show] [$obj2 show])"
    }
}


reg memv ::constcl::memv

proc ::constcl::memv {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[eqv? $obj1 [car $obj2]] eq "#t"} {
                return $obj2
            } else {
                return [memv $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(memv [$obj1 show] [$obj2 show])"
    }
}

reg member ::constcl::member

proc ::constcl::member {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[equal? $obj1 [car $obj2]] eq "#t"} {
                return $obj2
            } else {
                return [member $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(member [$obj1 show] [$obj2 show])"
    }
}


reg assq

proc ::constcl::assq {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[pair? [car $obj2]] eq "#t" && [eq? $obj1 [caar $obj2]] eq "#t"} {
                return [car $obj2]
            } else {
                return [assq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(assq [$obj1 show] [$obj2 show])"
    }
}


reg assv

proc ::constcl::assv {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[pair? [car $obj2]] eq "#t" && [eqv? $obj1 [caar $obj2]] eq "#t"} {
                return [car $obj2]
            } else {
                return [assq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(assv [$obj1 show] [$obj2 show])"
    }
}

reg assoc

proc ::constcl::assoc {obj1 obj2} {
    if {[list? $obj2] eq "#t"} {
        if {[null? $obj2] eq "#t"} {
            return #f
        } elseif {[pair? $obj2] eq "#t"} {
            if {[pair? [car $obj2]] eq "#t" && [equal? $obj1 [caar $obj2]] eq "#t"} {
                return [car $obj2]
            } else {
                return [assq $obj1 [cdr $obj2]]
            }
        }
    } else {
        error "LIST expected\n(assoc [$obj1 show] [$obj2 show])"
    }
}




oo::class create ::constcl::String {
    superclass ::constcl::NIL
    variable s constant
    constructor {v} {
        set s [::constcl::find-string-index $v]
        set constant 0
    }
    method index {} {set s}
    method = {str} {::string equal [my value] $str}
    method length {} {::string length [my value]}
    method ref {i} {::string index [my value] $i}
    method set! {k c} {
        if {[my constant]} {
            error "string is constant"
        } else {
            set value [::string replace [my value] $k $k $c]
            set s [::constcl::find-string-index $value]
        }
        return [self]
    }
    method fill! {c} {
        if {[my constant]} {
            error "string is constant"
        } else {
            set value [::string repeat $c [::string length [my value]]]
            set s [::constcl::find-string-index $value]
        }
        return [self]
    }
    method substring {from to} {::string range [my value] $from $to}
    method value {} {return [lindex $::constcl::StrSto $s]}
    method mkconstant {} {set constant 1}
    method constant {} {set constant}
    method write {} { puts -nonewline "\"[my value]\"" }
    method show {} {format "\"[my value]\""}
}

interp alias {} MkString {} ::constcl::String new

reg string? ::constcl::string?

proc ::constcl::string? {obj} {
    if {[info object isa typeof $obj ::constcl::String]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::String]} {
        return #t
    } else {
        return #f
    }
}


proc ::constcl::find-string-index {v} {
    set s -1
    for {set i 0} {$i < $::constcl::S} {incr i} {
        if {[::string equal [lindex $::constcl::StrSto $i] $v]} {
            set s $i
        }
    }
    if {$s == -1} {
        set s $::constcl::S
        lset ::constcl::StrSto $s $v
        incr ::constcl::S
    }
    set s
}



reg make-string ::constcl::make-string

proc ::constcl::make-string {args} {
    if {[llength $args] == 1} {
        lassign $args k
        return [MkString [::string repeat " " [$k value]]]
    } else {
        lassign $args k c
        return [MkString [::string repeat [$c char] [$k value]]]
    }
}



reg string ::constcl::string

proc ::constcl::string {args} {
    set str {}
    foreach char $args {
        if {[::constcl::char? $char] eq "#t"} {
            ::append str [$char char]
        } else {
            error "CHAR expected\n(string [$char show])"
        }
    }
    return [MkString $str]
}



reg string-length ::constcl::string-length

proc ::constcl::string-length {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkNumber [$str length]]
    } else {
        error "STRING expected\n(string-length [$str show])"
    }
}



reg string-ref ::constcl::string-ref

proc ::constcl::string-ref {str k} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-ref [$str show] [$k show])"
        }
        return [MkChar "#\\[$str ref $i]"]
    } else {
        error "STRING expected\n(string-ref [$str show] [$k show])"
    }
}



reg string-set! ::constcl::string-set!

proc ::constcl::string-set! {str k char} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k value]
        } else {
            error "Exact INTEGER expected\n(string-set! [$str show] [$k show] [$char show])"
        }
        if {[::constcl::char? $char] eq "#t"} {
            $str set! $i [$char char]
            return $str
        } else {
            error "CHAR expected\n(string-set! [$str show] [$k show] [$char show])"
        }
    } else {
        error "STRING expected\n(string-set! [$str show] [$k show] [$char show])"
    }
}



reg string=? ::constcl::string=?

proc ::constcl::string=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] eq [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string=? [$s1 show] [$s2 show])"
    }
}


reg string-ci=? ::constcl::string-ci=?

proc ::constcl::string-ci=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] eq [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci=? [$s1 show] [$s2 show])"
    }
}


reg string<? ::constcl::string<?

proc ::constcl::string<? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] < [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<? [$s1 show] [$s2 show])"
    }
}


reg string-ci<? ::constcl::string-ci<?

proc ::constcl::string-ci<? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] < [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<? [$s1 show] [$s2 show])"
    }
}


reg string>? ::constcl::string>?

proc ::constcl::string>? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] > [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>? [$s1 show] [$s2 show])"
    }
}


reg string-ci>? ::constcl::string-ci>?

proc ::constcl::string-ci>? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] > [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>? [$s1 show] [$s2 show])"
    }
}


reg string<=? ::constcl::string<=?

proc ::constcl::string<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] <= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string<=? [$s1 show] [$s2 show])"
    }
}


reg string-ci<=? ::constcl::string-ci<=?

proc ::constcl::string-ci<=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] <= [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci<=? [$s1 show] [$s2 show])"
    }
}


reg string>=? ::constcl::string>=?

proc ::constcl::string>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[$s1 value] >= [$s2 value]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string>=? [$s1 show] [$s2 show])"
    }
}


reg string-ci>=? ::constcl::string-ci>=?

proc ::constcl::string-ci>=? {s1 s2} {
    if {[::constcl::string? $s1] eq "#t" && [::constcl::string? $s2] eq "#t"} {
        if {[::string tolower [$s1 value]] >= [::string tolower [$s2 value]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "STRING expected\n(string-ci>=? [$s1 show] [$s2 show])"
    }
}



reg substring ::constcl::substring

proc ::constcl::substring {str start end} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $start] eq "#t" && [::constcl::number? $end] eq "#t"} {
            return [MkString [$str substring [$start value] [$end value]]]
        } else {
            error "NUMBER expected\n(substring [$str show] [$start show] [$end show])"
        }
    } else {
        error "STRING expected\n(substring [$str show] [$start show] [$end show])"
    }
}



reg string-append ::constcl::string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {$arg value}]]
}



reg string->list ::constcl::string->list

proc ::constcl::string->list {str} {
    list {*}[lmap c [split [$str value] {}] {MkChar "#\\$c"}]
}



reg list->string ::constcl::list->string

proc ::constcl::list->string {list} {
    MkString [::append --> {*}[lmap c [splitlist $list] {$c char}]]
}



reg string-copy ::constcl::string-copy

proc ::constcl::string-copy {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkString [$str value]]
    } else {
        error "STRING expected\n(string-copy [$str show])"
    }
}



reg string-fill! ::constcl::string-fill!

proc ::constcl::string-fill! {str char} {
    if {[::constcl::string? $str] eq "#t"} {
        $str fill! [$char char]
        return $str
    } else {
        error "STRING expected\n(string-fill [$str show] [$char show])"
    }
}




oo::class create ::constcl::Symbol {
    superclass ::constcl::NIL
    variable name caseconstant
    constructor {n} {
        if {$n eq {}} {
            error "a symbol must have a name"
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
        if {[$instance name] eq $n} {
            return $instance
        }
    }
    return [::constcl::Symbol new $n]
}

reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {obj} {
    if {[info object isa typeof $obj ::constcl::Symbol]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Symbol]} {
        return #t
    } else {
        return #f
    }
}



reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {obj} {
    if {[symbol? $obj] eq "#t"} {
        if {![$obj case-constant]} {
            set str [MkString [::string tolower [$obj name]]]
        } else {
            set str [MkString [$obj name]]
        }
        $str mkconstant
        return $str
    } else {
        error "SYMBOL expected\n(symbol->string [$obj show])"
    }
}



reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {str} {
    if {[string? $str] eq "#t"} {
        set sym [MkSymbol [$str value]]
        $sym make-case-constant
        return $sym
    } else {
        error "STRING expected\n(string->symbol [$obj show])"
    }
}



oo::class create ::constcl::Vector {
    superclass ::constcl::NIL
    variable value constant
    constructor {v} {
        set value $v
        set constant 0
    }
    method length {} {llength $value}
    method ref {i} {lindex $value $i}
    method value {} {set value}
    method set! {i obj} {
        if {[my constant]} {
            error "vector is constant"
        } else {
            if {$i < 0 || $i >= [my length]} {
                error "index out of range\n$i"
            } else {
                set value [::lreplace [my value] $i $i $obj]
            }
        }
        return [self]
    }
    method fill! {c} {
        if {[my constant]} {
            error "vector is constant"
        } else {
            set value [::lrepeat [::llength [my value]] $c]
        }
        return [self]
    }
    method mkconstant {} {set constant 1}
    method constant {} {set constant}
    method write {} {puts -nonewline [my show]}
    method show {} {format "#(%s)" [join [lmap val [my value] {$val show}] " "]}
}

proc ::constcl::MkVector {v} {
    foreach instance [info class instances ::constcl::Vector] {
        if {$instance eq $v} {
            return $instance
        }
    }
    return [::constcl::Vector new $v]
}

reg vector? ::constcl::vector?

proc ::constcl::vector? {obj} {
    if {[info object isa typeof $obj ::constcl::Vector]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] ::constcl::Vector]} {
        return #t
    } else {
        return #f
    }
}



reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {args} {
    if {[llength $args] == 1} {
        lassign $args k
        set fill #NIL
    } else {
        lassign $args k fill
    }
    MkVector [lrepeat [$k value] $fill]
}


reg vector ::constcl::vector

proc ::constcl::vector {args} {
    MkVector $args
}



reg vector-length ::constcl::vector-length

proc ::constcl::vector-length {vec} {
    if {[vector? $vec] eq "#t"} {
        return [MkNumber [$vec length]]
    } else {
        error "VECTOR expected\n(vector-length [$vec show])"
    }
}



reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[vector? $vec] eq "#t"} {
        if {[number? $k] eq "#t"} {
            return [$vec ref [$k value]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec show] [$k show])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec show] [$k show])"
    }
}



reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k obj} {
    if {[vector? $vec] eq "#t"} {
        if {[number? $k] eq "#t"} {
            return [$vec set! [$k value] $obj]
        } else {
            error "NUMBER expected\n(vector-set! [$vec show] [$k show] [$obj show])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec show] [$k show] [$obj show])"
    }
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
    if {[vector? $vec] eq "#t"} {
        $vec fill! $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec show] [$fill show])"
    }
}




proc ::constcl::idcheckinit {init} {
    if {[::string is alpha $init] || $init in {! $ % & * / : < = > ? ^ _ ~}} {
        return true
    } else {
        return false
    }
}

proc ::constcl::idchecksubs {subs} {
    foreach c [split $subs {}] {
        if {!([::string is alnum $c] || $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
            return false
        }
    }
    return true
}

proc ::constcl::idcheck {sym} {
    if {(![idcheckinit [::string index $sym 0]] ||
        ![idchecksubs [::string range $sym 1 end]]) && $sym ni {+ - ...}} {
        error "Identifier expected ($sym)"
    }
    set sym
}

proc ::constcl::varcheck {sym} {
    if {$sym in {else => define unquote unquote-splicing quote lambda if set! begin
        cond and or case let let* letrec do delay quasiquote}} {
            error "Macro name can't be used as a variable: $sym"
    }
    return $sym
}


unset -nocomplain ::constcl::S ;# string store number
set ::constcl::S 0

unset -nocomplain ::constcl::StrSto
set ::constcl::StrSto [list]


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



dict set ::constcl::defreg pi [::constcl::MkNumber 3.1415926535897931]


reg atom? ::constcl::atom?

proc ::constcl::atom? {obj} {
    if {[symbol? $obj] eq "#t" || [number? $obj] eq "#t" || [string? $obj] eq "#t" || [char? $obj] eq "#t" || [boolean? $obj] eq "#t" || [vector? $obj] eq "#t"} {
        return #t
    } else {
        return #f
    }
}







proc ::constcl::input {prompt} {
    puts -nonewline $prompt
    gets stdin
}


proc ::constcl::repl {{prompt "ConsTcl> "}} {
    set str [input $prompt]
    while {$str ne ""} {
        write [eval [parse $str]]
        set str [input $prompt]
    }
}

catch { ::constcl::Environment destroy }

oo::class create ::constcl::Environment {
    variable bindings outer_env
    constructor {syms vals {outer {}}} {
        set bindings [dict create]
        if {$syms eq "#NIL"} {
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



::constcl::Environment create ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
    method find {sym} {self}
    method get {sym} {error "Unbound variable: [$sym name]"}
    method set {sym val} {error "Unbound variable: [$sym name]"}
}


namespace eval ::constcl {
    set keys [list {*}[lmap k [dict keys $defreg] {MkSymbol $k}]]
    set vals [dict values $defreg]
    Environment create global_env $keys $vals ::constcl::null_env
    format "[llength [dict keys $defreg]] built-in procedures in definition register"
}




