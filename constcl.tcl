


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
    ::constcl::write [::constcl::eval [::constcl::parse [::constcl::IB new $str]]]
}

proc ::pp {str} {
    ::constcl::write [::constcl::parse [::constcl::IB new $str]]
}

proc ::prp {str} {
    set val [::constcl::parse [::constcl::IB new $str]]
    set op [::constcl::car $val]
    set args [::constcl::cdr $val]
    set env ::constcl::global_env
    while {[$op name] in {
            and case cond define del! for for/and for/list for/or
            let or pop! push! put! quasiquote unless when}} {
            ::constcl::expand-macro $env
    }
    set args [::constcl::resolve-local-defines $args]
    ::constcl::write $args
}

proc ::pxp {str} {
    set val [::constcl::parse [::constcl::IB new $str]]
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
    method write {handle} {puts -nonewline $handle "()"}
    method display {} { puts -nonewline "()" }
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
    method write {handle} {puts -nonewline $handle "."}
    method display {} { puts -nonewline "." }
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
    method write {} {puts -nonewline #<undefined>}
}


catch { ::constcl::EndOfFile destroy }

oo::class create ::constcl::EndOfFile {
    method mkconstant {} {}
    method write {handle} {puts -nonewline #<eof>}
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

proc ::constcl::new-atom {pa pd} {
    cons3 $pa $pd $::constcl::ATOM_TAG
}

proc cons3 {pcar pcdr ptag} {
    # TODO counters
    set n [MkPair $pcar $pcdr]
    $n settag $ptag
    return $n
}

proc ::constcl::xread {} {
    ::if {[$::constcl::InputPort handle] eq "#NIL"} {
        error "input port is not open"
    }
    set ::constcl::Level 0
    return [read-form 0]
}

proc ::constcl::read_c_ci {} {
    return [tolower [::read [$::constcl::Input_port handle] 1]]
}




catch { ::constcl::IB destroy }

oo::class create ::constcl::IB {
    variable peekc buffer
    constructor {str} {
        set peekc {}
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





reg parse

proc ::constcl::parse {inp} {
    ::if {[info object isa object $inp]} {
        set ib $inp
    } else {
        set ib [IB new $inp]
    }
    return [parse-expression]
}



proc ::constcl::parse-expression {} {
    upvar ib ib
    $ib skip-ws
    switch -regexp [$ib first] {
        {^$}          { return #NONE}
        {\"}          { return [parse-string-expression] }
        {\#}          { return [parse-sharp] }
        {\'}          { return [parse-quoted-expression] }
        {\(}          { return [parse-pair-expression ")"] }
        {\+} - {\-}   { return [parse-plus-minus] }
        {\,}          { return [parse-unquoted-expression] }
        {\.}          { $ib advance ; return [Dot new] }
        {\[}          { return [parse-pair-expression "\]"] }
        {\`}          { return [parse-quasiquoted-expression] }
        {\d}          { return [parse-number-expression] }
        {[[:graph:]]} { return [parse-identifier-expression] }
        default {
            ::error "unexpected character ([$ib first])"
        }
    }
}



proc ::constcl::parse-string-expression {} {
    upvar ib ib
    set str {}
    $ib advance
    while {[$ib first] ne "\"" && [$ib first] ne {}} {
        set c [$ib first]
        ::if {$c eq "\\"} {
            $ib advance
            ::append str [$ib first]
        } else {
            ::append str $c
        }
        $ib advance
    }
    ::if {[$ib first] ne "\""} {
        ::error "malformed string (no ending double quote)"
    }
    $ib advance
    $ib skip-ws
    set expr [MkString $str]
    $expr mkconstant
    return $expr
}




proc ::constcl::parse-sharp {} {
    upvar ib ib
    $ib advance
    switch [$ib first] {
        (    { return [parse-vector-expression] }
        t    { $ib advance ; $ib skip-ws ; return #t }
        f    { $ib advance ; $ib skip-ws ; return #f }
        "\\" { return [parse-character-expression] }
        default {
            ::error "Illegal #-literal: #[$ib first]"
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
    upvar ib ib
    $ib advance
    set expr [parse-expression]
    $ib skip-ws
    make-constant $expr
    return [list #Q $expr]
}





proc ::constcl::parse-pair {char} {
    upvar ib ib
    ::if {[$ib find $char]} {
        return #NIL
    }
    $ib skip-ws
    set a [parse-expression]
    $ib skip-ws
    set res $a
    set prev #NIL
    while {![$ib find $char]} {
        set x [parse-expression]
        $ib skip-ws
        ::if {[dot? $x] ne "#f"} {
            set prev [parse-expression]
            $ib skip-ws
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
    upvar ib ib
    $ib advance
    $ib skip-ws
    set expr [parse-pair $char]
    $ib skip-ws
    ::if {[$ib first] ne $char} {
        ::if {$char eq ")"} {
            ::error "Missing right parenthesis (first=[$ib first])."
        } else {
            ::error "Missing right bracket (first=[$ib first])."
        }
    }
    $ib advance
    $ib skip-ws
    return $expr
}




proc ::constcl::parse-plus-minus {} {
    upvar ib ib
    set c [$ib first]
    $ib advance
    ::if {[::string is digit -strict [$ib first]]} {
        $ib unget $c
        return [::constcl::parse-number-expression]
    } else {
        ::if {$c eq "+"} {
            $ib skip-ws
            return [MkSymbol "+"]
        } else {
            $ib skip-ws
            return [MkSymbol "-"]
        }
    }
}



proc ::constcl::parse-unquoted-expression {} {
    upvar ib ib
    $ib advance
    set symbol "unquote"
    ::if {[$ib first] eq "@"} {
        set symbol "unquote-splicing"
        $ib advance
    }
    set expr [parse-expression]
    $ib skip-ws
    return [list [MkSymbol $symbol] $expr]
}




proc ::constcl::parse-quasiquoted-expression {} {
    upvar ib ib
    $ib advance
    set expr [parse-expression]
    $ib skip-ws
    make-constant $expr
    return [list [MkSymbol "quasiquote"] $expr]
}



proc ::constcl::interspace {c} {
    # don't add #EOF: parse-* uses this one too
    ::if {$c eq {} || [::string is space -strict $c] || $c eq ";"} {
        return #t
    } else {
        return #f
    }
}



proc ::constcl::parse-number-expression {} {
    upvar ib ib
    while {[interspace [$ib first]] ne "#t" && [$ib first] ni {) \]}} {
        ::append num [$ib first]
        $ib advance
    }
    $ib skip-ws
    check {::string is double -strict $num} {Invalid numeric constant $num}
    return [MkNumber $num]
}




proc ::constcl::parse-identifier-expression {} {
    upvar ib ib
    while {[interspace [$ib first]] ne "#t" && [$ib first] ni {) \]}} {
        ::append name [$ib first]
        $ib advance
    }
    $ib skip-ws
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
    upvar ib ib
    set name "#"
    while {[interspace [$ib first]] ne "#t" && [$ib first] ni {) ]}} {
        ::append name [$ib first]
        $ib advance
    }
    check {character-check $name} {Invalid character constant $name}
    $ib skip-ws
    return [MkChar $name]
}




proc ::constcl::parse-vector-expression {} {
    upvar ib ib
    $ib advance
    $ib skip-ws
    set res {}
    while {[$ib first] ne {} && [$ib first] ne ")"} {
        lappend res [parse-expression]
        $ib skip-ws
    }
    set vec [MkVector $res]
    $vec mkconstant
    ::if {[$ib first] ne ")"} {
        ::error "Missing right parenthesis (first=[$ib first])."
    }
    $ib advance
    $ib skip-ws
    return $vec
}




reg read ::constcl::read

proc ::constcl::read {args} {
    set c {}
    set unget {}
    ::if {[llength $args]} {
        lassign $args port
    } else {
        set port $::constcl::Input_port
    }
    set oldport $::constcl::Input_port
    set ::constcl::Input_port $port
    set expr [read-expression]
    set ::constcl::Input_port $oldport
    set unget {}
    return $expr
}



proc ::constcl::read-expression {args} {
    upvar c c unget unget
    ::if {[llength $args]} {
        lassign $args c
    } else {
        set c [readc]
    }
    read-eof $c
    ::if {[::string is space $c] || $c eq ";"} {
        skip-ws
        read-eof $c
    }
    switch -regexp $c {
        {^$}          { return #NONE}
        {\"}          { set n [read-string-expression]       ; read-eof $n; return $n }
        {\#}          { set n [read-sharp]                   ; read-eof $n; return $n }
        {\'}          { set n [read-quoted-expression]       ; read-eof $n; return $n }
        {\(}          { set n [read-pair-expression ")"]     ; read-eof $n; return $n }
        {\+} - {\-}   { set n [read-plus-minus $c]           ; read-eof $n; return $n }
        {\,}          { set n [read-unquoted-expression]     ; read-eof $n; return $n }
        {\.}          { set n [Dot new]; set c [readc]       ; read-eof $n; return $n }
        {\[}          { set n [read-pair-expression "\]"]    ; read-eof $n; return $n }
        {\`}          { set n [read-quasiquoted-expression]  ; read-eof $n; return $n }
        {\d}          { set n [read-number-expression $c]    ; read-eof $n; return $n }
        {[[:graph:]]} { set n [read-identifier-expression $c]; read-eof $n; return $n }
        default {
            read-eof $c
            ::error "unexpected character ($c)"
        }
    }
}




proc readc {} {
    upvar unget unget
    ::if {$unget ne {}} {
        set c $unget
        set unget {}
    } else {
        set c [::read [$::constcl::Input_port handle] 1]
        ::if {[eof [$::constcl::Input_port handle]]} {
            return #EOF
        }
    }
    return $c
}



proc read-find {char} {
    upvar c c unget unget
    while {[::string is space -strict $c]} {
        set c [readc]
        read-eof $c
        set unget $c
    }
    return [expr {$c eq $char}]
}



proc skip-ws {} {
    upvar c c unget unget
    while true {
        switch -regexp $c {
            {[[:space:]]} {
                set c [readc]
            }
            {;} {
                while {$c ne "\n" && $c ne "#EOF"}  {
                    set c [readc]
                }
            }
            default {
                return
            }
        }
    }
}



proc read-eof {args} {
    foreach val $args {
        ::if {$val eq "#EOF"} {
            return -level 1 -code return #EOF
        }
    }
}



proc ::constcl::read-string-expression {} {
    upvar c c unget unget
    set str {}
    set c [readc]
    read-eof $c
    while {$c ne "\"" && $c ne "#EOF"} {
        ::if {$c eq "\\"} {
            set c [readc]
        }
        ::append str $c
        set c [readc]
    }
    ::if {$c ne "\""} {
        error "malformed string (no ending double quote)"
    }
    set c [readc]
    set expr [MkString $str]
    $expr mkconstant
    return $expr
}



proc ::constcl::read-sharp {} {
    upvar c c unget unget
    set c [readc]
    read-eof $c
    switch $c {
        (    { set n [read-vector-expression]   ; set c [readc]; return $n }
        t    { set n #t                         ; set c [readc]; return $n }
        f    { set n #f                         ; set c [readc]; return $n }
        "\\" { set n [read-character-expression];                return $n }
        default {
            read-eof $c
            ::error "Illegal #-literal: #$c"
        }
    }
}



proc ::constcl::read-vector-expression {} {
    upvar c c unget unget
    set res {}
    set c [readc]
    while {$c ne "#EOF" && $c ne ")"} {
        lappend res [read-expression $c]
        skip-ws
        read-eof $c
    }
    set expr [MkVector $res]
    $expr mkconstant
    ::if {$c ne ")"} {
        ::error "Missing right parenthesis (first=$c)."
    }
    set c [readc]
    return $expr
}



proc ::constcl::read-character-expression {} {
    upvar c c unget unget
    set name "#\\"
    set c [readc]
    read-eof $c
    while {[::string is alpha $c]} {
        ::append name $c
        set c [readc]
        read-eof $c
    }
    check {character-check $name} {Invalid character constant $name}
    set expr [MkChar $name]
    return $expr
}



proc ::constcl::read-quoted-expression {} {
    upvar c c unget unget
    set expr [read-expression]
    read-eof $expr
    make-constant $expr
    return [list #Q $expr]
}



proc ::constcl::read-pair-expression {char} {
    upvar c c unget unget
    set expr [read-pair $char]
    skip-ws
    read-eof $c
    ::if {$c ne $char} {
        ::if {$char eq ")"} {
            ::error "Missing right parenthesis (first=$c)."
        } else {
            ::error "Missing right bracket (first=$c)."
        }
    } else {
        set unget {}
        set c [readc]
    }
    return $expr
}

proc ::constcl::read-pair {char} {
    upvar c c unget unget
    ::if {[read-find $char]} {
        # read right paren/brack
        set c [readc]
        return #NIL
    }
    set c [readc]
    read-eof $c
    set a [read-expression $c]
    set res $a
    skip-ws
    set prev #NIL
    while {![read-find $char]} {
        set x [read-expression $c]
        skip-ws
        read-eof $c
        ::if {[dot? $x] ne "#f"} {
            set prev [read-expression $c]
            skip-ws $c]
            read-eof $c
        } else {
            lappend res $x
        }
        ::if {[llength $res] > 999} break
    }
    # read right paren/brack
    foreach r [lreverse $res] {
        set prev [cons $r $prev]
    }
    return $prev
}



proc ::constcl::read-plus-minus {char} {
    upvar c c unget unget
    set c [readc]
    read-eof $c
    ::if {[::string is digit -strict $c]} {
        set n [read-number-expression $c]
        ::if {$char eq "-"} {
            set n [- $n]
        }
        return $n
    } else {
        ::if {$char eq "+"} {
            return [MkSymbol "+"]
        } else {
            return [MkSymbol "-"]
        }
    }
}



proc ::constcl::read-number-expression {args} {
    upvar c c unget unget
    ::if {[llength $args]} {
        lassign $args c
    } else {
        set c [readc]
    }
    read-eof $c
    while {[interspace $c] ne "#t" && $c ne "#EOF" && $c ni {) \]}} {
        ::append num $c
        set c [readc]
    }
    set unget $c
    check {::string is double -strict $num} {Invalid numeric constant $num}
    return [MkNumber $num]
}



proc ::constcl::read-unquoted-expression {} {
    upvar c c unget unget
    set c [readc]
    read-eof $c
    ::if {$c eq "@"} {
        set symbol "unquote-splicing"
        set expr [read-expression]
    } else {
        set symbol "unquote"
        set expr [read-expression $c]
    }
    read-eof $expr
    return [list [MkSymbol $symbol] $expr]
}



proc ::constcl::read-quasiquoted-expression {} {
    upvar c c unget unget
    set expr [read-expression]
    skip-ws
    read-eof $expr
    make-constant $expr
    return [list [MkSymbol "quasiquote"] $expr]
}



proc ::constcl::read-identifier-expression {args} {
    upvar c c unget unget
    ::if {[llength $args]} {
        set c [lindex $args 0]
    } else {
        set c [readc]
    }
    read-eof $c
    set name {}
    while {[::string is graph -strict $c]} {
        ::if {$c eq "#EOF" || $c in {) \]}} {
            break
        }
        ::append name $c
        set c [readc]
    }
    ::if {$c ne "#EOF"} {
        set unget $c
    }
    # idcheck throws error if invalid identifier
    idcheck $name
    return [MkSymbol $name]
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



proc ::constcl::expand-define {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    $env setstr "tail" $tail
    set qq "`(define ,(caar tail) (lambda ,(cdar tail) ,@(cdr tail)))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}



proc ::constcl::expand-del! {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    ::if {[null? $tail] ne "#f"} {::error "too few arguments, 2 expected, got 0"}
    $env setstr "listname" [car $tail]
    ::if {[null? [cdr $tail]] ne "#f"} {::error "too few arguments, 2 expected, got 1"}
    $env setstr "key" [cadr $tail]
    set qq "`(set! ,listname (delete! ,listname ,key))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
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


proc ::constcl::expand-for {tail env} {
    set res [do-for $tail $env]
    lappend res [list #Q #NIL]
    return [list #B {*}$res]
}



proc ::constcl::expand-for/and {tail env} {
    set res [do-for $tail $env]
    return [list [MkSymbol "and"] {*}$res]
}



proc ::constcl::expand-for/list {tail env} {
    set res [do-for $tail $env]
    return [list [MkSymbol "list"] {*}$res]
}



proc ::constcl::expand-for/or {tail env} {
    set res [do-for $tail $env]
    return [list [MkSymbol "or"] {*}$res]
}



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



proc ::constcl::expand-or {tail env} {
    ::if {[eq? [length $tail] #0] ne "#f"} {
        return [list #B #f]
    } elseif {[eq? [length $tail] #1] ne "#f"} {
        return [cons #B $tail]
    } else {
        return [do-or $tail $env]
    }
}


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



proc ::constcl::expand-unless {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    $env setstr "tail" $tail
    set qq "`(if ,(car tail) (quote ()) (begin ,@(cdr tail)))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}



proc ::constcl::expand-when {tail env} {
    set env [::constcl::Environment new #NIL {} $env]
    $env setstr "tail" $tail
    set qq "`(if ,(car tail) (begin ,@(cdr tail)) (quote ()))"
    return [expand-quasiquote [cdr [parse $qq]] $env]
}



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



proc ::constcl::make-temporaries {vals} {
    set n #NIL
    while {$vals ne "#NIL"} {
        set sym [gensym "g"]
        set n [cons $sym $n]
        set vals [cdr $vals]
    }
    return $n
}



proc ::constcl::gensym {prefix} {
    set symbolnames [lmap s [info class instances ::constcl::Symbol] {$s name}]
    set s $prefix<[incr ::constcl::gensymnum]>
    while {$s in $symbolnames} {
        set s $prefix[incr ::constcl::gensymnum]
    }
    return [MkSymbol $s]
}



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



proc ::constcl::make-undefineds {vals} {
    # Use #NIL instead of #UNDF because of some strange bug with eval-list.
    set n #NIL
    while {$vals ne "#NIL"} {
        set n [cons #NIL $n]
        set vals [cdr $vals]
    }
    return $n
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
        ::if {[llength $args]} {
            lassign $args port
        } else {
            set port [MkOutputPort stdout]
        }
        set ::constcl::Output_port $port
        write-value [$::constcl::Output_port handle] $val
        puts [$::constcl::Output_port handle] {}
        set ::constcl::Output_port [MkOutputPort stdout]
    }
    return
}



proc ::constcl::write-value {handle val} {
    $val write $handle
    return
}



reg display ::constcl::display

proc ::constcl::display {val args} {
    ::if {$val ne "#NONE"} {
        $val display
        flush stdout
    }
    return
}



proc ::constcl::write-pair {handle pair} {
    # take an object and print the car and the cdr of the stored value
    set a [car $pair]
    set d [cdr $pair]
    # print car
    write-value $handle $a
    ::if {[pair? $d] ne "#f"} {
        # cdr is a cons pair
        puts -nonewline $handle " "
        write-pair $handle $d
    } elseif {[null? $d] ne "#f"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline $handle " . "
        write-value $handle $d
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
    } elseif {[number? $val1] ne "#f" && [number? $val2] ne "#f" && [$val1 numval] eq [$val2 numval]} {
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
    method write {handle} { puts -nonewline $handle [my value] }
    method display {} { puts -nonewline [my value] }
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
    method write {handle} { puts -nonewline $handle [my bvalue] }
    method display {} { puts -nonewline [my bvalue] }
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
    method write {handle} { puts -nonewline $handle $value }
    method display {} { puts -nonewline [my char] }
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
    method write {handle} {
        regexp {(\d+)} [self] -> num
        puts -nonewline $handle "#<proc-$num>"
    }
    method display {} {my write}
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


catch { Port destroy }

oo::class create Port {
    variable handle
    constructor {args} {
        if {[llength $args]} {
            lassign $args handle
        } else {
            set handle #NIL
        }
    }
    method handle {} {set handle}
    method close {} {
        close $handle
        set handle #NIL
    }
}

oo::class create InputPort {
    superclass Port
    variable handle
    method open {name} {
        try {
            set handle [open $name "r"]
        } on error {} {
            set handle #NIL
            return -1
        }
    }
}

oo::class create OutputPort {
    superclass Port
    variable handle
    method open {name} {
        try {
            set handle [open $name "w"]
        } on error {} {
            set handle #NIL
            return -1
        }
    }
}

interp alias {} ::constcl::MkInputPort {} InputPort new
interp alias {} ::constcl::MkOutputPort {} OutputPort new

set ::constcl::Input_port [::constcl::MkInputPort stdin]
set ::constcl::Output_port [::constcl::MkOutputPort stdout]

proc ::constcl::port? {val} {
    ::if {[info object isa typeof $val ::constcl::Port]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Port]} {
        return #t
    } else {
        return #f
    }
}

proc ::constcl::call-with-input-file {string proc} {
    # TODO
}

proc ::constcl::call-with-output-file {string proc} {
    # TODO
}

proc ::constcl::input-port? {obj} {
    ::if {[info object isa typeof $val ::constcl::InputPort]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::InputPort]} {
        return #t
    } else {
        return #f
    }
}

proc ::constcl::output-port? {obj} {
    ::if {[info object isa typeof $val ::constcl::OutputPort]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::OutputPort]} {
        return #t
    } else {
        return #f
    }
}

proc ::constcl::current-input-port {} {
    return $::constcl::Input_port
}

proc ::constcl::current-output-port {} {
    return $::constcl::Output_port
}

proc ::constcl::with-input-from-file {string thunk} {
    # TODO
}


proc ::constcl::with-output-to-file {string thunk} {
    # TODO
}

reg open-input-file

proc ::constcl::open-input-file {filename} {
    set p [MkInputPort]
    $p open $filename
    ::if {[$p handle] eq "#NIL"} {
        error "open-input-file: could not open file $filename"
    }
    return $p
}

reg open-output-file

proc ::constcl::open-output-file {filename} {
    ::if {[file exists $filename]} {
        error "open-output-file: file already exists $filename"
    }
    set p [MkOutputPort]
    $p open $filename
    ::if {[$p handle] eq "#NIL"} {
        error "open-output-file: could not open file $filename"
    }
    return $p
}

proc ::constcl::close-input-port {port} {
    ::if {[$port handle] eq "stdin"} {
        error "don't close the standard input port"
    }
    $port close
}

proc ::constcl::close-output-port {port} {
    ::if {[$port handle] eq "stdout"} {
        error "don't close the standard output port"
    }
    $port close
}


proc ::constcl::__read {args} {
    ::if {[llength $args]} {
        set new_port [lindex $args 0]
    } else {
        set new_port $::constcl::Input_port
    }
    set old_port $::constcl::Input_port
    set ::constcl::Input_port $new_port
    set n [xread]
    set ::constcl::Input_port $old_port
    return $n
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



reg newline

proc ::constcl::newline {args} {
    ::if {[llength $args]} {
        lassign $args port
    } else {
        set port [current-output-port]
    }
    write #\\newline $port
}

proc ::constcl::write-char {args} {
    # TODO
}


proc ::constcl::__load {filename} {
    set new_port [MkInputPort]
    $new_port open $filename
    if {[$new_port handle] eq "#NIL"} {
        return -1
    }
    set ::constcl::File_list [cons [MkString $filename] $::constcl::File_list]
    set save_env $env
    set env ::constcl::global_env
    set outer_loading [$::constcl::S_loading cdr]
    set-cdr! ::constcl::S_loading #t
    set old_port $::constcl::Input_port
    set outer_lno $::constcl::Line_no
    set ::constcl::Line_no 1
    while true {
        set ::constcl::Input_port $new_port
        set n [xread]
        set ::constcl::Input_port $old_port
        ::if {$n == $::constcl::END_OF_FILE} {
            break
        }
        set n [eval $n $env]
    }
    $new_port close
    set $::constcl::Line_no $outer_lno
    set-cdr! ::constcl::S_loading $outer_loading
    set ::constcl::File_list [cdr $::constcl::File_list]
    set env $save_env
    return 0
}

proc ::constcl::____load {filename} {
    set f [open $filename]
    set src [::read $f]
    close $f
    set ib [::constcl::IB new $src]
    while {[$ib first] ne {}} {
        eval [parse $ib]
    }
}

proc ::constcl::load {filename} {
    set p [open-input-file $filename]
    set n [read $p]
    while {$n ne "#EOF"} {
        eval $n ::constcl::global_env
        set n [read $p]
    }
    close-input-port $p
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
    method write {handle} {
        puts -nonewline $handle "("
        ::constcl::write-pair $handle [self]
        puts -nonewline $handle ")"
    }
    method display {} { [my write] }
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
    method write {handle} { puts -nonewline $handle "\"[my value]\"" }
    method display {} { puts -nonewline [my value] }
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
        ::if {   no &&   $n eq {}} {
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
    method write {handle} { puts -nonewline $handle [my name] }
    method display {} { puts -nonewline [my name] }
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
    method write {handle} { puts -nonewline $handle [my show]}
    method display {} {puts -nonewline [my show]}
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
::if {$sym eq {}} {return $sym}
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

set ::constcl::vectorSpace [lrepeat 1024 #NIL]

set ::constcl::vectorAssign 0

proc ::constcl::vsAlloc {num} {
    # TODO calculate free space
    set va $::constcl::vectorAssign
    incr ::constcl::vectorAssign $num
    return $va
}

set ::constcl::gensymnum 0


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

interp alias {} #EOF {} [::constcl::EndOfFile new]



dict set ::constcl::defreg pi [::constcl::MkNumber 3.1415926535897931]


reg nil #NIL



reg atom? ::constcl::atom?

proc ::constcl::atom? {val} {
    ::if {[symbol? $val] ne "#f" || [number? $val] ne "#f" || [string? $val] ne "#f" ||
        [char? $val] ne "#f" || [boolean? $val] ne "#f" || [vector? $val] ne "#f" ||
        [port? $val] ne "#f"} {
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
    method setstr {str val} {
        dict set bindings [::constcl::MkSymbol $str] $val
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



::constcl::load schemebase.lsp




# vim: ft=tcl tw=80
if no {


'(a 1 b 2 c 3 d 4 e 5)


> (define plist (list 'a 1 'b 2 'c 3 'd 4 'e 5))
> (define v '())
> (set! v (memq 'c plist))
(c 3 d 4 e 5)
> (set! v (cadr v))
3


> (get plist 'c)
3




> (set! plist (append '(f 6) plist))
(f 6 a 1 b 2 c 3 d 4 e 5)


> (put! plist 'c 9)
(f 6 a 1 b 2 c 9 d 4 e 5)
> (put! plist 'g 7)
(g 7 f 6 a 1 b 2 c 9 d 4 e 5)



> (set! plist (append '(d #f) plist))
(d #f g 7 f 6 a 1 b 2 c 3 d 4 e 5)


> plist
(g 7 f 6 a 1 b 2 c 9 d 4 e 5)
> (del! plist 'd)
(g 7 f 6 a 1 b 2 c 9 e 5)



> (define alist (list (cons 'a 1) (cons 'b 2) (cons 'c 3) (cons 'd 4)))
> alist
((a . 1) (b . 2) (c . 3) (d . 4))


> (define alist (pairlis '(a b c) '(1 2 3)))
((a . 1) (b . 2) (c . 3))


> (assq 'a alist)
(a . 1)
> (cdr (assq 'a alist))
1
> (assq 'x alist)
#f


> (get-alist 'a)
1
> (get-alist 'x)
#f


> (push! (cons 'e 5) alist)
((e . 5) (a . 1) (b . 2) (c . 3) (d . 4))


> alist
((a . 1) (b . 2) (c . 3) (d . 4))
> (set-alist! alist 'b 7)
((a . 1) (b . 7) (c . 3) (d . 4))

}

# vim: ft=tcl tw=80
