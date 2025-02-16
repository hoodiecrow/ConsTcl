


namespace eval ::constcl {}




proc ::reg {key args} {
  if {[llength $args]} {
    lassign $args val
  } else {
    set val ::constcl::$key
  }
  dict set ::constcl::defreg $key $val
  return
}



proc ::regmacro {name} {
  lappend ::constcl::macrolist $name
  return
}



proc ::pep {str} {
  ::constcl::write [
    ::constcl::eval [
      ::constcl::parse $str]]
}



proc ::pp {str} {
  ::constcl::write [
    ::constcl::parse $str]
}



proc ::pe {str} {
  ::constcl::eval [
    ::constcl::parse $str]
}



proc ::p {str} {
  ::constcl::parse $str
}



proc ::e {expr} {
  ::constcl::eval $expr
}



proc ::w {val} {
  ::constcl::write $val
}



proc ::r {args} {
  ::constcl::read {*}$args
}



proc ::prp {str} {
  set expr [::constcl::parse $str]
  set expr [::constcl::resolve-local-defines \
    [::constcl::cdr $expr]]
  ::constcl::write $expr
}



proc ::pxp {str} {
  set expr [::constcl::parse $str]
  set expr [::constcl::expand-macro $expr \
    ::constcl::global_env]
  ::constcl::write $expr
}



proc ::pn {} {
  lindex [split [lindex [info level -1] 0] :] end
}



proc ::constcl::typeof? {val type} {
  if {[info object isa typeof $val $type]} {
    return #t
  } elseif {[info object isa typeof \
      [interp alias {} $val] $type]} {
    return #t
  } else {
    return #f
  }
}



reg in-range

proc ::constcl::in-range {x args} {
  set start 0
  set step 1
  switch [llength $args] {
    0 {
      set e $x
      set end [$e numval]
    }
    1 {
      set s $x
      lassign $args e
      set start [$s numval]
      set end [$e numval]
    }
    2 {
      set s $x
      lassign $args e t
      set start [$s numval]
      set end [$e numval]
      set step [$t numval]
    }
  }
  set res $start
  while {$step > 0 && $end > [incr start $step] ||
      $step < 0 && $end < [incr start $step]} {
    lappend res $start
  }
  return [list {*}[lmap r $res {MkNumber $r}]]
}


catch { ::constcl::NIL destroy }

oo::singleton create ::constcl::NIL {
  method bvalue {} {
    return #NIL
  }
  method car {} {
    ::error "PAIR expected"
  }
  method cdr {} {
    ::error "PAIR expected"
  }
  method set-car! {v} {
    ::error "PAIR expected"
  }
  method set-cdr! {v} {
    ::error "PAIR expected"
  }
  method numval {} {
    ::error "NUMBER expected"
  }
  method write {handle} {
    puts -nonewline $handle "()"
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "()"
  }
}



reg null?

proc ::constcl::null? {val} {
  if {$val eq "#NIL"} {
    return #t
  } else {
    return #f
  }
}


catch { ::constcl::Dot destroy }

oo::class create ::constcl::Dot {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "."
  }
  method display {handle} {
    my write $handle
  }
}



proc ::constcl::dot? {val} {
  typeof? $val "Dot"
}


catch { ::constcl::Unspecified destroy }

oo::class create ::constcl::Unspecified {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "#<unspecified>"
  }
  method display {handle} {
    my write $handle
  }
}


catch { ::constcl::Undefined destroy }

oo::class create ::constcl::Undefined {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "#<undefined>"
  }
  method display {handle} {
    my write $handle
  }
}


catch { ::constcl::EndOfFile destroy }

oo::class create ::constcl::EndOfFile {
  method mkconstant {} {}
  method write {handle} {
    puts -nonewline $handle "#<end-of-file>"
  }
  method display {handle} {
    my write $handle
  }
}



reg error

proc ::constcl::error {msg args} {
  if {[llength $args]} {
    lappend msg "("
    set times 0
    foreach arg $args {
      if {$times} {
        ::append msg " "
      }
      ::append msg [$arg show]
      incr times
    }
    lappend msg ")"
  }
  ::error $msg
}


proc ::constcl::check {cond msg} {
  if {[uplevel $cond] eq "#f"} {
    ::error [
      uplevel [
        ::list subst [
          ::string trim $msg]]]
  }
}



reg atom? ::constcl::atom?

proc ::constcl::atom? {val} {
  foreach type {symbol number string
      char boolean vector port} {
    if {[$type? $val] eq "#t"} {
      return #t
    }
  }
  return #f
}

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 



catch { ::constcl::IB destroy }

oo::class create ::constcl::IB {
  variable peekc buffer
  constructor {str} {
    set peekc {}
    my fill $str
  }
}


oo::define ::constcl::IB method fill {str} {
  set buffer $str
  my advance
}


oo::define ::constcl::IB method advance {} {
  if {$buffer eq {}} {
    set peekc {}
  } else {
    set peekc [::string index $buffer 0]
    set buffer [::string range $buffer 1 end]
  }
}


oo::define ::constcl::IB method peek {} {
  return $peekc
}


oo::define ::constcl::IB method unget {char} {
  set buffer $peekc$buffer
  set peekc $char
}


oo::define ::constcl::IB method find {char} {
  if {[::string is space -strict $peekc]} {
    for {set cp 0} \
        {$cp < [::string length $buffer]} \
        {incr cp} {
      if {![::string is space -strict [
        ::string index $buffer $cp]]} {
        break
      }
    }
    return [expr {[
      ::string index $buffer $cp] eq $char}]
  } else {
    return [expr {$peekc eq $char}]
  }
}


oo::define ::constcl::IB method skip-ws {} {
  while true {
    switch -regexp $peekc {
      {[[:space:]]} {
        my advance
      }
      {;} {
        while {$peekc ne "\n" && $peekc ne {}} {
          my advance
        }
      }
      default {
        return
      }
    }
  }
}



reg parse

proc ::constcl::parse {inp} {
  if {[info object isa object $inp]} {
    if {[typeof? $inp IB] ne "#f"} {
      set ib $inp
    } elseif {[typeof? $inp String] ne "#f"} {
      set ib [IB new [$inp value]]
    } else {
      ::error "Unknown object [$inp show]"
    }
  } else {
    # It's a Tcl string, we hope
    set ib [IB new $inp]
  }
  return [parse-expr]
}




proc ::constcl::parse-expr {} {
  upvar ib ib
  $ib skip-ws
  switch -regexp [$ib peek] {
    {\"}          { parse-string-expr }
    {\#}          { parse-sharp }
    {\'}          { parse-quoted-expr }
    {\(}          { parse-pair-expr ")" }
    {\+} - {\-}   { parse-plus-minus }
    {\,}          { parse-unquoted-expr }
    {\.} { $ib advance ; return [Dot new] }
    {\:}          { parse-object-expr }
    {\[}          { parse-pair-expr "\]" }
    {\`}          { parse-quasiquoted-expr }
    {\d}          { parse-number-expr }
    {^$}          { return}
    {[[:graph:]]} { parse-identifier-expr }
    default {
      ::error "unexpected character ([$ib peek])"
    }
  }
}



proc ::constcl::parse-string-expr {} {
  upvar ib ib
  set str {}
  $ib advance
  while {[$ib peek] ne "\"" && [$ib peek] ne {}} {
    set c [$ib peek]
    if {$c eq "\\"} {
      ::append str $c
      $ib advance
      ::append str [$ib peek]
    } else {
      ::append str $c
    }
    $ib advance
  }
  if {[$ib peek] ne "\""} {
    ::error "no ending double quote"
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
  switch [$ib peek] {
    "("  { return [parse-vector-expr] }
    "t"  { $ib advance ; $ib skip-ws ; return #t }
    "f"  { $ib advance ; $ib skip-ws ; return #f }
    "\\" { return [parse-character-expr] }
    default {
      ::error "Illegal #-literal: #[$ib peek]"
    }
  }
}


proc ::constcl::make-constant {val} {
  if {[pair? $val] ne "#f"} {
    $val mkconstant
    make-constant [car $val]
    make-constant [cdr $val]
  } elseif {[null? $val] ne "#f"} {
    return #NIL
  } else {
    $val mkconstant
  }
}



proc ::constcl::parse-quoted-expr {} {
  upvar ib ib
  $ib advance
  set expr [parse-expr]
  $ib skip-ws
  make-constant $expr
  return [list [S quote] $expr]
}




proc ::constcl::parse-pair-expr {char} {
  upvar ib ib
  $ib advance
  $ib skip-ws
  set expr [parse-pair $char]
  $ib skip-ws
  if {[$ib peek] ne $char} {
    if {$char eq ")"} {
      ::error \
        "Missing right paren. ([$ib peek])."
    } else {
      ::error \
        "Missing right bracket ([$ib peek])."
    }
  }
  $ib advance
  $ib skip-ws
  return $expr
}


proc ::constcl::parse-pair {char} {
  upvar ib ib
  if {[$ib find $char]} {
    return #NIL
  }
  $ib skip-ws
  set a [parse-expr]
  $ib skip-ws
  set res $a
  set prev #NIL
  while {![$ib find $char]} {
    set x [parse-expr]
    $ib skip-ws
    if {[dot? $x] ne "#f"} {
      set prev [parse-expr]
      $ib skip-ws
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




proc ::constcl::parse-plus-minus {} {
  upvar ib ib
  set c [$ib peek]
  $ib advance
  if {[::string is digit -strict [$ib peek]]} {
    $ib unget $c
    return [::constcl::parse-number-expr]
  } else {
    if {$c eq "+"} {
      $ib skip-ws
      return [S "+"]
    } else {
      $ib skip-ws
      return [S "-"]
    }
  }
}



proc ::constcl::parse-unquoted-expr {} {
  upvar ib ib
  $ib advance
  if {[$ib peek] eq "@"} {
    set symbol "unquote-splicing"
    $ib advance
  } else {
    set symbol "unquote"
  }
  set expr [parse-expr]
  $ib skip-ws
  return [list [S $symbol] $expr]
}




proc ::constcl::parse-quasiquoted-expr {} {
  upvar ib ib
  $ib advance
  set expr [parse-expr]
  $ib skip-ws
  # TODO make semi-constant
  make-constant $expr
  return [list [S "quasiquote"] $expr]
}



proc ::constcl::interspace {c} {
  # don't add #EOF: parse-* uses this one too
  if {$c eq {} ||
    [::string is space $c] ||
    $c eq ";"} {
      return #t
    } else {
      return #f
    }
}



proc ::constcl::parse-number-expr {} {
  upvar ib ib
  while {[interspace [$ib peek]] ne "#t" && \
    [$ib peek] ni {) \]}} {
      ::append num [$ib peek]
      $ib advance
    }
    $ib skip-ws
    check {::string is double -strict $num} {
      Invalid numeric constant $num
    }
    return [N $num]
}




proc ::constcl::parse-identifier-expr {} {
  upvar ib ib
  while {[interspace [$ib peek]] ne "#t" &&
      [$ib peek] ni {) \]}} {
    ::append name [$ib peek]
    $ib advance
  }
  $ib skip-ws
  # idcheck throws error if invalid identifier
  return [S [idcheck $name]]
}




proc ::constcl::character-check {name} {
  if {[regexp {(?i)^#\\([[:graph:]]|space|newline)$} \
      $name]} {
    return #t
  } else {
    return #f
  }
}



proc ::constcl::parse-character-expr {} {
  upvar ib ib
  set name "#"
  while {[interspace [$ib peek]] ne "#t" &&
      [$ib peek] ni {) ]}} {
    ::append name [$ib peek]
    $ib advance
  }
  check {character-check $name} {
    Invalid character constant $name
  }
  $ib skip-ws
  return [MkChar $name]
}




proc ::constcl::parse-vector-expr {} {
  upvar ib ib
  $ib advance
  $ib skip-ws
  set res {}
  while {[$ib peek] ne {} && [$ib peek] ne ")"} {
    lappend res [parse-expr]
    $ib skip-ws
  }
  set vec [MkVector $res]
  $vec mkconstant
  if {[$ib peek] ne ")"} {
    ::error "Missing right parenthesis."
  }
  $ib advance
  $ib skip-ws
  return $vec
}




proc ::constcl::parse-object-expr {} {
  upvar ib ib
  foreach ch [split "::oo::Obj" {}] {
    if {[$ib peek] ne $ch} {
      error "bad object name"
    }
    $ib advance
  }
  set res "::oo::Obj"
  while {[::string is digit [$ib peek]]} {
    ::append res [$ib peek]
    $ib advance
  }
  return $res
}



reg read

proc ::constcl::read {args} {
  set c {}
  set unget {}
  if {[llength $args]} {
    lassign $args port
  } else {
    set port $::constcl::Input_port
  }
  set oldport $::constcl::Input_port
  set ::constcl::Input_port $port
  set expr [read-expr]
  set ::constcl::Input_port $oldport
  return $expr
}



proc ::constcl::read-expr {args} {
  upvar c c unget unget
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
  read-eof $c
  if {[::string is space $c] || $c eq ";"} {
    skip-ws
    read-eof $c
  }
  switch -regexp $c {
    {\"}          { read-string-expr }
    {\#}          { read-sharp }
    {\'}          { read-quoted-expr }
    {\(}          { read-pair-expr ")" }
    {\+} - {\-}   { read-plus-minus $c }
    {\,}          { read-unquoted-expr }
    {\.}          { Dot new }
    {\:}          { read-object-expr }
    {\[}          { read-pair-expr "\]" }
    {\`}          { read-quasiquoted-expr }
    {\d}          { read-number-expr $c }
    {^$}          { return}
    {[[:graph:]]} { read-identifier-expr $c }
    default {
      read-eof $c
      ::error "unexpected character ($c)"
    }
  }
}




proc ::constcl::readc {} {
  upvar unget unget
  if {$unget ne {}} {
    set c $unget
    set unget {}
  } else {
    set c [::read [$::constcl::Input_port handle] 1]
    if {[eof [$::constcl::Input_port handle]]} {
      return #EOF
    }
  }
  return $c
}



proc ::constcl::read-find {char} {
  upvar c c unget unget
  while {[::string is space -strict $c]} {
    set c [readc]
    read-eof $c
    set unget $c
  }
  return [expr {$c eq $char}]
}



proc ::constcl::read-end {} {
  upvar c c unget unget
  set c [readc]
  if {[interspace $c] ne "#f" || $c in {) ]}} {
    set unget $c
    return 1
  } else {
    read-eof $c
    set unget $c
    return 0
  }
}



proc ::constcl::skip-ws {} {
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



proc ::constcl::read-eof {args} {
  foreach val $args {
    if {$val eq "#EOF"} {
      return -level 1 -code return #EOF
    }
  }
}



proc ::constcl::read-string-expr {} {
  upvar c c unget unget
  set str {}
  set c [readc]
  read-eof $c
  while {$c ne "\"" && $c ne "#EOF"} {
    if {$c eq "\\"} {
      ::append str $v
      set c [readc]
    }
    ::append str $c
    set c [readc]
  }
  if {$c ne "\""} {
    error "bad string (no ending double quote)"
  }
  set expr [MkString $str]
  $expr mkconstant
  return $expr
}



proc ::constcl::read-sharp {} {
  upvar c c unget unget
  set c [readc]
  read-eof $c
  switch $c {
    (    { set n [read-vector-expr] }
    t    { if {[read-end]} {set n #t} }
    f    { if {[read-end]} {set n #f} }
    "\\" { set n [read-character-expr] }
    default {
      ::error "Illegal #-literal: #$c"
    }
  }
  return $n
}



proc ::constcl::read-vector-expr {} {
  upvar c c unget unget
  set res {}
  set c [readc]
  while {$c ne "#EOF" && $c ne ")"} {
    lappend res [read-expr $c]
    skip-ws
    read-eof $c
  }
  if {$c ne ")"} {
    ::error "Missing right paren. ($c)."
  }
  set expr [MkVector $res]
  $expr mkconstant
  return $expr
}



proc ::constcl::read-character-expr {} {
  upvar c c unget unget
  set name "#\\"
  set c [readc]
  read-eof $c
  while {[::string is alpha $c]} {
    ::append name $c
    set c [readc]
    read-eof $c
  }
  check {character-check $name} {
      Invalid character constant $name
  }
  set expr [MkChar $name]
  read-eof $expr
  return $expr
}



proc ::constcl::read-quoted-expr {} {
  upvar c c unget unget
  set expr [read-expr]
  read-eof $expr
  make-constant $expr
  return [list [S quote] $expr]
}



proc ::constcl::read-pair-expr {char} {
  upvar c c unget unget
  set expr [read-pair $char]
  read-eof $expr
  skip-ws
  read-eof $c
  if {$c ne $char} {
    if {$char eq ")"} {
      ::error \
        "Missing right paren. ($c)."
    } else {
      ::error \
        "Missing right bracket ($c)."
    }
  } else {
    set unget {}
    set c [readc]
  }
  return $expr
}

proc ::constcl::read-pair {char} {
  upvar c c unget unget
  if {[read-find $char]} {
    # read right paren/brack
    set c [readc]
    return #NIL
  }
  set c [readc]
  read-eof $c
  set a [read-expr $c]
  set res $a
  skip-ws
  set prev #NIL
  while {![read-find $char]} {
    set x [read-expr $c]
    skip-ws
    read-eof $c
    if {[dot? $x] ne "#f"} {
      set prev [read-expr $c]
      skip-ws $c]
      read-eof $c
    } else {
      lappend res $x
    }
    if {[llength $res] > 999} break
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
  if {[::string is digit -strict $c]} {
    set n [read-number-expr $c]
    if {$char eq "-"} {
      set n [- $n]
    }
    return $n
  } else {
    if {$char eq "+"} {
      return [S "+"]
    } else {
      return [S "-"]
    }
  }
}



proc ::constcl::read-number-expr {args} {
  upvar c c unget unget
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
  read-eof $c
  while {[interspace $c] ne "#t" && $c ne "#EOF" &&
      $c ni {) \]}} {
    ::append num $c
    set c [readc]
  }
  set unget $c
  check {::string is double -strict $num} {
      Invalid numeric constant $num
  }
  set expr [N $num]
  read-eof $expr
  return $expr
}



proc ::constcl::read-unquoted-expr {} {
  upvar c c unget unget
  set c [readc]
  read-eof $c
  if {$c eq "@"} {
    set symbol "unquote-splicing"
    set expr [read-expr]
  } else {
    set symbol "unquote"
    set expr [read-expr $c]
  }
  read-eof $expr
  return [list [S $symbol] $expr]
}



proc ::constcl::read-object-expr {} {
  upvar c c unget unget
  foreach ch [split ":oo::Obj" {}] {
    set c [readc]
    read-eof $c
    if {$c ne $ch} {
      error "bad object name"
    }
  }
  set res "::oo::Obj"
  set c [readc]
  read-eof $c
  while {[::string is digit $c]} {
    ::append res $c
    set c [readc]
    read-eof $c
  }
  set unget $c
  return $res
}



proc ::constcl::read-quasiquoted-expr {} {
  upvar c c unget unget
  set expr [read-expr]
  skip-ws
  read-eof $expr
  make-constant $expr
  return [list [S "quasiquote"] $expr]
}



proc ::constcl::read-identifier-expr {args} {
  upvar c c unget unget
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
  read-eof $c
  set name {}
  while {[::string is graph -strict $c]} {
    if {$c eq "#EOF" || $c in {) \]}} {
      break
    }
    ::append name $c
    set c [readc]
  }
  if {$c ne "#EOF"} {
    set unget $c
  }
  read-eof $name
  # idcheck throws error if invalid identifier
  idcheck $name
  return [S $name]
}

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 




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
      quote { car $args }
      if { if {[eval [car $args] $env] ne "#f"} \
        {eval [cadr $args] $env} \
        {eval [caddr $args] $env} }
      begin { /begin $args $env }
      define { /define [car $args] [
        eval [cadr $args] $env] $env }
      set! { /set! [car $args] [
        eval [cadr $args] $env] $env }
      lambda { /lambda [car $args] [
        cdr $args] $env }
      default { invoke [eval $op $env] [
        eval-list $args $env] }
    }
  }
}



proc ::constcl::lookup {sym env} {
  [$env find $sym] get $sym
}





proc ::constcl::/if {cond conseq altern} {
  if {[uplevel $cond] ne "#f"} {
    uplevel $conseq
  } {
    uplevel $altern
  }
}



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



proc ::constcl::/define {sym val env} {
  varcheck [idcheck [$sym name]]
  $env set $sym $val
  return
}



proc ::constcl::/set! {var val env} {
  [$env find $var] set $var $val
  set val
}



proc ::constcl::/lambda {formals body env} {
  if {[[length $body] value] > 1} {
    set body [cons [S begin] $body]
  } else {
    set body [car $body]
  }
  return [MkProcedure $formals $body $env]
}



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




proc ::constcl::splitlist {vals} {
  set result {}
  while {[pair? $vals] ne "#f"} {
    lappend result [car $vals]
    set vals [cdr $vals]
  }
  return $result
}



proc ::constcl::eval-list {exps env} {
  # don't convert to /if, it breaks (fact 100)
  if {[pair? $exps] ne "#f"} {
    return [cons [eval [car $exps] $env] \
      [eval-list [cdr $exps] $env]]
  } {
    return #NIL
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

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 



proc ::constcl::expand-macro {expr env} {
  set op [car $expr]
  set args [cdr $expr]
  if {[$op name] eq "define" &&
      [pair? [car $args]] eq "#f"} {
    return -code break
  }
  return [expand-[$op name] $expr $env]
}



regmacro and

proc ::constcl::expand-and {expr env} {
  set tail [cdr $expr]
  if {[[length $tail] numval] == 0} {
    list [S begin] #t
  } elseif {[[length $tail] numval] == 1} {
    cons [S begin] $tail
  } else {
    do-and $tail #t $env
  }
}


proc ::constcl::do-and {tail prev env} {
  set env [Environment new #NIL {} $env]
  if {[[length $tail] numval] == 0} {
    return $prev
  } else {
    $env set [S first] [car $tail]
    $env set [S rest] [do-and [cdr $tail] \
        [car $tail] $env]
    set qq "`(if ,first ,rest #f)"
    return [expand-quasiquote [parse $qq] $env]
  }
}



regmacro case

proc ::constcl::expand-case {expr env} {
  set tail [cdr $expr]
  do-case [car $tail] [cdr $tail]
}

proc ::constcl::do-case {keyexpr clauses} {
  if {[eq? [length $clauses] #0] ne "#f"} {
    return [list [S quote] #NIL]
  } else {
    set keyl [caar $clauses]
    set body [cdar $clauses]
    set keyl [list [S memv] $keyexpr \
        [list [S quote] $keyl]]
    if {[eq? [length $clauses] #1] ne "#f"} {
      if {[eq? [caar $clauses] [S else]] ne "#f"} {
        set keyl #t
      }
    }
    return [list [S if] $keyl \
        [cons [S begin] $body] \
        [do-case $keyexpr [cdr $clauses]]]
  }
}



regmacro cond

proc ::constcl::expand-cond {expr env} {
  return [do-cond [cdr $expr] $env]
}

proc ::constcl::do-cond {tail env} {
  set clauses $tail
  if {[eq? [length $clauses] #0] ne "#f"} {
    return [list [S quote] #NIL]
  } else {
    set pred [caar $clauses]
    set body [cdar $clauses]
    if {[symbol? [car $body]] ne "#f" &&
        [[car $body] name] eq "=>"} {
      set body [cddar $clauses]
    }
    if {[eq? [length $clauses] #1] ne "#f"} {
      if {[eq? $pred [S else]] ne "#f"} {
        set pred #t
      }
    }
    if {[null? $body] ne "#f"} {
        set body $pred
    }
    return [list [S if] $pred \
        [cons [S begin] $body] \
        [do-cond [cdr $clauses] $env]]
  }
}



regmacro define

proc ::constcl::expand-define {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  $env set [S tail] $tail
  set qq "`(define ,(caar tail)
             (lambda ,(cdar tail) ,@(cdr tail)))"
  return [expand-quasiquote [parse $qq] $env]
}



regmacro del!

proc ::constcl::expand-del! {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
    ::error "too few arguments, 0 of 2"
  }
  $env set [S listname] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {
    ::error "too few arguments, 1 of 2"
  }
  $env set [S key] [cadr $tail]
  set qq "`(set! ,listname
             (delete! ,listname ,key))"
  return [expand-quasiquote [parse $qq] $env]
}



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
    set seq [lmap c [split [$seq value] {}] \
        {MkChar #\\$c}]
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
  for {set i 0} \
      {$i < [llength $clauses]} \
      {incr i} {
    set clause [lindex $clauses $i]
    # insert the first part of the
    # clause in the ids structure
    lset ids $i [car $clause]
    # run the second part of the clause
    # through for-seq and insert in seqs
    lset seqs $i [for-seq [cadr $clause] $env]
  }
  set res {}
  for {set item 0} \
      {$item < [llength [lindex $seqs 0]]} \
      {incr item} {
    # for each iteration of the sequences
    set x {}
    for {set clause 0} \
        {$clause < [llength $clauses]} \
        {incr clause} {
      # for each clause
      # list append to x the Lisp list
      # of the id and the iteration
      lappend x [list [lindex $ids $clause] \
          [lindex $seqs $clause $item]]
    }
    # list append to res a let expression
    # with the ids and iterations and the body
    lappend res [list [S let] [
        list {*}$x] {*}[splitlist $body]]
  }
  return $res
}


proc ::constcl::expand-for {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  lappend res [list [S quote] #NIL]
  return [list [S begin] {*}$res]
}



regmacro for/and

proc ::constcl::expand-for/and {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [MkSymbol "and"] {*}$res]
}



regmacro for/list

proc ::constcl::expand-for/list {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [MkSymbol "list"] {*}$res]
}



regmacro for/or

proc ::constcl::expand-for/or {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [MkSymbol "or"] {*}$res]
}



regmacro let

proc ::constcl::expand-let {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[symbol? [car $tail]] ne "#f"} {
    # named let
    set variable [car $tail]
    set bindings [cadr $tail]
    set body [cddr $tail]
    set vars [dict create $variable #f]
    parse-bindings vars $bindings
    $env set [S decl] [list {*}[dict values [
      dict map {k v} $vars {list $k $v}]]]
    $env set [S variable] $variable
    $env set [S varlist] [list {*}[lrange [
      dict keys $vars] 1 end]]
    $env set [S body] $body
    $env set [S call] [list {*}[
      dict keys $vars]]
    set qq "`(let ,decl
               (set!
                 ,variable
                 (lambda ,varlist ,@body)) ,call)"
    return [expand-quasiquote [parse $qq] $env]
  } else {
    # regular let
    set bindings [car $tail]
    set body [cdr $tail]
    set vars [dict create]
    parse-bindings vars $bindings
    $env set [S varlist] [list {*}[
      dict keys $vars]]
    $env set [S body] $body
    $env set [S vallist] [list {*}[
      dict values $vars]]
    set qq "`((lambda ,varlist ,@body)
               ,@vallist)"
    return [expand-quasiquote [parse $qq] $env]
  }
}

proc ::constcl::parse-bindings {name bindings} {
  upvar $name vars
  foreach binding [splitlist $bindings] {
    set var [car $binding]
    set val [cadr $binding]
    if {$var in [dict keys $vars]} {
        ::error "'$var' occurs more than once"
    }
    dict set vars $var $val
  }
}



regmacro or

proc ::constcl::expand-or {expr env} {
  set tail [cdr $expr]
  if {[eq? [length $tail] #0] ne "#f"} {
    return [list [S begin] #f]
  } elseif {[eq? [length $tail] #1] ne "#f"} {
    return [cons [S begin] $tail]
  } else {
    return [do-or $tail $env]
  }
}


proc ::constcl::do-or {tail env} {
  set env [Environment new #NIL {} $env]
  /if {eq? [length $tail] #0} {
    return #f
  } {
    $env set [S first] [car $tail]
    $env set [S rest] [do-or [cdr $tail] $env]
    set qq "`(let ((x ,first)) (if x x ,rest))"
    return [expand-quasiquote [parse $qq] $env]
  }
}



regmacro pop!

proc ::constcl::expand-pop! {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
      ::error "too few arguments:\n(pop! listname)"
  }
  $env set [MkSymbol "obj"] [car $tail]
  if {[symbol? [car $tail]] eq "#f"} {
      ::error "SYMBOL expected:\n(pop! listname)"
  }
  $env set [S listname] [car $tail]
  set qq "`(set! ,listname (cdr ,listname))"
  return [expand-quasiquote [parse $qq] $env]
}



regmacro push!

proc ::constcl::expand-push! {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
    ::error \
      "too few arguments:\n(push! obj listname)"
  }
  $env set [S obj] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {
    ::error \
      "too few arguments:\n(push! obj listname)"
  }
  if {[symbol? [cadr $tail]] eq "#f"} {
    ::error \
      "SYMBOL expected:\n(push! obj listname)"
  }
  $env set [S listname] [cadr $tail]
  set qq "`(set!
             ,listname
             (cons ,obj ,listname))"
  return [expand-quasiquote [parse $qq] $env]
}



regmacro put!

proc ::constcl::expand-put! {expr env} {
  set tail [cdr $expr]
  set env [::constcl::Environment new #NIL {} $env]
  if {[null? $tail] ne "#f"} {
      ::error "too few arguments, 0 of 3"
  }
  $env set [MkSymbol "name"] [car $tail]
  if {[null? [cdr $tail]] ne "#f"} {
      ::error "too few arguments, 1 of 3"
  }
  $env set [MkSymbol "key"] [cadr $tail]
  if {[null? [cddr $tail]] ne "#f"} {
      ::error "too few arguments, 2 of 3"
  }
  $env set [MkSymbol "val"] [caddr $tail]
  set qq "`(let ((idx (list-find-key ,name ,key)))
             (if (< idx 0)
               (set! 
                 ,name
                 (append (list ,key ,val) ,name))
               (begin
                 (list-set! ,name (+ idx 1) ,val)
                 ,name)))"
  return [expand-quasiquote [parse $qq] $env]
}



regmacro quasiquote

proc ::constcl::qq-visit-child {node qqlevel env} {
  if {$qqlevel < 0} {
    set qqlevel 0
  }
  if {[list? $node] ne "#f"} {
    set res {}
    foreach child [splitlist $node] {
      if {[pair? $child] ne "#f" &&
          [eq? [car $child] [S unquote]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res [eval [cadr $child] $env]
        } else {
          lappend res [list [S unquote] [
            qq-visit-child [cadr $child] [
            expr {$qqlevel - 1}] $env]]
        }
      } elseif {[pair? $child] ne "#f" &&
          [eq? [car $child] [
          S unquote-splicing]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res {*}[splitlist [
            eval [cadr $child] $env]]
        }
      } elseif {[pair? $child] ne "#f" &&
          [eq? [car $child] [S quasiquote]] ne "#f"} {
        lappend res [list [S quasiquote] [car [
          qq-visit-child [cdr $child] [
            expr {$qqlevel + 1}] $env]]] 
      } elseif {[atom? $child] ne "#f"} {
        lappend res $child
      } else {
        lappend res [
          qq-visit-child $child $qqlevel $env]
      }
    }
  }
  return [list {*}$res]
}


proc ::constcl::expand-quasiquote {expr env} {
  set tail [cdr $expr]
  set qqlevel 0
  if {[list? [car $tail]] ne "#f"} {
    set node [car $tail]
    return [qq-visit-child $node 0 $env]
  } elseif {[vector? [car $tail]] ne "#f"} {
    set vect [car $tail]
    set res {}
    for {set i 0} {$i < [
        [vector-length $vect] numval]} {incr i} {
      set idx [MkNumber $i]
      set vecref [vector-ref $vect $idx]
      if {[pair? $vecref] ne "#f" &&
          [eq? [car $vecref] [
            S unquote]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res [eval [cadr $vecref] $env]
        }
      } elseif {[pair? $vecref] ne "#f" &&
          [eq? [car $vecref] [
            S unquote-splicing]] ne "#f"} {
        if {$qqlevel == 0} {
          lappend res {*}[splitlist [
            eval [cadr $vecref] $env]]
        }
      } elseif {[atom? $vecref] ne "#f"} {
        lappend res $vecref
      } else {
      }
    }
    return [list [MkSymbol "vector"] {*}$res]
  }
}



regmacro unless

proc ::constcl::expand-unless {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  $env set [S tail] $tail
  set qq "`(if ,(car tail)
             '()
             (begin ,@(cdr tail)))"
  return [expand-quasiquote [parse $qq] $env]
}



regmacro when

proc ::constcl::expand-when {expr env} {
  set tail [cdr $expr]
  set env [Environment new #NIL {} $env]
  $env set [S tail] $tail
  set qq "`(if ,(car tail)
             (begin ,@(cdr tail))
             '())"
  return [expand-quasiquote [parse $qq] $env]
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 



proc ::constcl::resolve-local-defines {exps} {
  set rest [lassign [
    extract-from-defines $exps VALS] a error]
  if {$error ne "#f"} {
    return #NIL
  }
  set rest [lassign [
    extract-from-defines $exps VARS] v error]
  if {$error ne "#f"} {
    return #NIL
  }
  if {$rest eq "#NIL"} {
    set rest [cons #UNSP #NIL]
  }
  return [make-lambdas $v $a $rest]
}



proc ::constcl::extract-from-defines {exps part} {
  set a #NIL
  while {$exps ne "#NIL"} {
    if {[atom? $exps] ne "#f" ||
        [atom? [car $exps]] ne "#f" ||
        [eq? [caar $exps] [S define]] eq "#f"} {
      break
    }
    set n [car $exps]
    set k [length $n]
    if {[list? $n] eq "#f" ||
        [$k numval] < 3 ||
        [$k numval] > 3 ||
        ([argument-list? [cadr $n]] ne "#f" ||
        [symbol? [cadr $n]] eq "#f")
      eq "#f"} {
        return [::list #NIL "#t" #NIL]
      }
      if {[pair? [cadr $n]] ne "#f"} {
        if {$part eq "VARS"} {
          set a [cons [caadr $n] $a]
        } else {
          set a [cons #NIL $a]
          set new [cons [cdadr $n] [cddr $n]]
          set new [cons [S lambda] $new]
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



proc ::constcl::make-lambdas {vars args body} {
  set tmps [make-temporaries $vars]
  set body [append-b [
    make-assignments $vars $tmps] $body]
  set body [cons $body #NIL]
  set n [cons $tmps $body]
  set n [cons [S lambda] $n]
  set n [cons $n $args]
  set n [cons $n #NIL]
  set n [cons $vars $n]
  set n [cons [S lambda] $n]
  set n [cons $n [make-undefineds $vars]]
  return $n
}



proc ::constcl::make-temporaries {vals} {
  set res #NIL
  while {$vals ne "#NIL"} {
    set res [cons [gensym "g"] $res]
    set vals [cdr $vals]
  }
  return $res
}



proc ::constcl::gensym {prefix} {
  set symbolnames [
    dict keys $::constcl::symbolTable]
  set s $prefix<[incr ::constcl::gensymnum]>
  while {$s in $symbolnames} {
    set s $prefix[incr ::constcl::gensymnum]
  }
  return [MkSymbol $s]
}



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



proc ::constcl::make-assignments {vars tmps} {
  set res #NIL
  while {$vars ne "#NIL"} {
    set asg [cons [car $tmps] #NIL]
    set asg [cons [car $vars] $asg]
    set asg [cons [S set!] $asg]
    set res [cons $asg $res]
    set vars [cdr $vars]
    set tmps [cdr $tmps]
  }
  return [cons [S begin] $res]
}



proc ::constcl::make-undefineds {vals} {
  # TODO find bug, substitute #UNDF
  set res #NIL
  while {$vals ne "#NIL"} {
    set res [cons #NIL $res]
    set vals [cdr $vals]
  }
  return $res
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 



reg write ::constcl::write

proc ::constcl::write {val args} {
  if {$val ne ""} {
    if {[llength $args]} {
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
  if {$val ne ""} {
    if {[llength $args]} {
      lassign $args port
    } else {
      set port [MkOutputPort stdout]
    }
    set ::constcl::Output_port $port
    $val display [$::constcl::Output_port handle]
    flush [$::constcl::Output_port handle]
    set ::constcl::Output_port [MkOutputPort stdout]
  }
  return
}



proc ::constcl::write-pair {handle pair} {
  # take an object and print the car
  # and the cdr of the stored value
  set a [car $pair]
  set d [cdr $pair]
  # print car
  write-value $handle $a
  if {[pair? $d] ne "#f"} {
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


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


reg eq?

proc ::constcl::eq? {val1 val2} {
  if {[teq boolean? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq symbol? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq number? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq char? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq null? $val1 $val2]} {
    return #t
  } elseif {[teq pair? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq string? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq vector? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq procedure? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } else {
    return #f
  }
}

proc ::constcl::teq {typep val1 val2} {
    return [expr {[$typep $val1] ne "#f" &&
      [$typep $val2] ne "#f"}]
}

proc ::constcl::veq {val1 val2} {
    return [expr {[$val1 value] eq [$val2 value]}]
}



reg eqv?

proc ::constcl::eqv? {val1 val2} {
  if {[teq boolean? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } elseif {[teq symbol? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq number? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq char? $val1 $val2] &&
      [veq $val1 eq $val2]} {
    return #t
  } elseif {[teq null? $val1 $val2]} {
    return #t
  } elseif {[pair? $val1] ne "#f" &&
      [pair? $val2] ne "#f" &&
      [$val1 car] eq [$val2 car] &&
      [$val1 cdr] eq [$val2 cdr]} {
    return #t
  } elseif {[teq string? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq vector? $val1 $val2] &&
      [veq $val1 $val2]} {
    return #t
  } elseif {[teq procedure? $val1 $val2] &&
      $val1 eq $val2} {
    return #t
  } else {
    return #f
  }
}


reg equal?

proc ::constcl::equal? {val1 val2} {
  if {[$val1 show] eq [$val2 show]} {
    return #t
  } else {
    return #f
  }
  # TODO
}

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


oo::class create ::constcl::Number {
  superclass ::constcl::NIL
  variable value
  constructor {v} {
    if {[::string is double -strict $v]} {
      set value $v
    } else {
      ::error "NUMBER expected\n$v"
    }
  }
  method zero? {} {
    if {$value == 0} {return #t} {return #f}
  }
  method positive? {} {
    if {$value > 0} {return #t} {return #f}
  }
  method negative? {} {
    if {$value < 0} {return #t} {return #f}
  }
  method even? {} {
    if {$value % 2 == 0} {return #t} {return #f}
  }
  method odd? {} {
    if {$value % 2 == 1} {return #t} {return #f}
  }
  method value {} {
    set value
  }
  method numval {} {
    set value
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method write {handle} {
    puts -nonewline $handle [my value]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set value
  }
}

interp alias {} ::constcl::MkNumber \
  {} ::constcl::Number new
interp alias {} N {} ::constcl::Number new



reg number?

proc ::constcl::number? {val} {
  return [typeof? $val Number]
}




reg =

proc ::constcl::= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(= num ...)"
  }
  if {[::tcl::mathop::== {*}$vals]} {
    return #t
  } else {
    return #f
  }
}


reg <

proc ::constcl::< {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(< num ...)"
  }
  if {[::tcl::mathop::< {*}$vals]} {
    return #t
  } else {
    return #f
  }
}


reg >

proc ::constcl::> {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(> num ...)"
  }
  if {[::tcl::mathop::> {*}$vals]} {
    return #t
  } else {
    return #f
  }
}


reg <=

proc ::constcl::<= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(<= num ...)"
  }
  if {[::tcl::mathop::<= {*}$vals]} {
    return #t
  } else {
    return #f
  }
}


reg >=

proc ::constcl::>= {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(>= num ...)"
  }
  if {[::tcl::mathop::>= {*}$vals]} {
    return #t
  } else {
    return #f
  }
}




reg zero?

proc ::constcl::zero? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num zero?]
}




reg positive?

proc ::constcl::positive? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num positive?]
}


reg negative?

proc ::constcl::negative? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num negative?]
}


reg even?

proc ::constcl::even? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num even?]
}


reg odd?

proc ::constcl::odd? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  return [$num odd?]
}





reg max

proc ::constcl::max {num args} {
  lappend args $num
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(max num...)"
  }
  MkNumber [::tcl::mathfunc::max {*}$vals]
}


reg min

proc ::constcl::min {num args} {
  lappend args $num
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(min num...)"
  }
  MkNumber [::tcl::mathfunc::min {*}$vals]
}






reg +

proc ::constcl::+ {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(+ num ...)"
  }
  MkNumber [::tcl::mathop::+ {*}$vals]
}


reg *

proc ::constcl::* {args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(* num ...)"
  }
  MkNumber [::tcl::mathop::* {*}$vals]
}


reg -

proc ::constcl::- {num args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(- num ...)"
  }
  MkNumber [::tcl::mathop::- [$num numval] {*}$vals]
}


reg /

proc ::constcl::/ {num args} {
  try {
    set vals [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(/ num ...)"
  }
  MkNumber [::tcl::mathop::/ [$num numval] {*}$vals]
}




reg abs

proc ::constcl::abs {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    return [MkNumber [expr {[$num numval] * -1}]]
  } else {
    return $num
  }
}





reg quotient

proc ::constcl::quotient {num1 num2} {
  set q [::tcl::mathop::/ [$num1 numval] \
    [$num2 numval]]
  if {$q > 0} {
    return [MkNumber [::tcl::mathfunc::floor $q]]
  } elseif {$q < 0} {
    return [MkNumber [::tcl::mathfunc::ceil $q]]
  } else {
    return #0
  }
}




reg remainder

proc ::constcl::remainder {num1 num2} {
  set n [::tcl::mathop::% [[abs $num1] numval] \
    [[abs $num2] numval]]
  if {[$num1 negative?] ne "#f"} {
    set n -$n
  }
  return [MkNumber $n]
}




reg modulo

proc ::constcl::modulo {num1 num2} {
  return [MkNumber [::tcl::mathop::% [$num1 numval] \
    [$num2 numval]]]
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




reg floor

proc ::constcl::floor {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::floor [$num numval]]
}


reg ceiling

proc ::constcl::ceiling {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::ceil [$num numval]]
}


reg truncate

proc ::constcl::truncate {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  if {[$num negative?] ne "#f"} {
    MkNumber [::tcl::mathfunc::ceil [$num numval]]
  } else {
    MkNumber [::tcl::mathfunc::floor [$num numval]]
  }
}


reg round

proc ::constcl::round {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::round [$num numval]]
}


proc ::constcl::rationalize {x y} {
    # TODO
}





reg exp

proc ::constcl::exp {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::exp [$num numval]]
}


reg log

proc ::constcl::log {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::log [$num numval]]
}


reg sin

proc ::constcl::sin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::sin [$num numval]]
}

reg cos

proc ::constcl::cos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::cos [$num numval]]
}

reg tan

proc ::constcl::tan {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::tan [$num numval]]
}


reg asin

proc ::constcl::asin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::asin [$num numval]]
}

reg acos

proc ::constcl::acos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::acos [$num numval]]
}

reg atan

proc ::constcl::atan {args} {
  if {[llength $args] == 1} {
    set num [lindex $args 0]
    check {number? $num} {
        NUMBER expected\n([pn] [$num show])
    }
    MkNumber [::tcl::mathfunc::atan [$num numval]]
  } else {
    lassign $args num1 num2
    check {number? $num1} {
        NUMBER expected\n([pn] [$num1 show])
    }
    check {number? $num2} {
        NUMBER expected\n([pn] [$num2 show])
    }
    MkNumber [::tcl::mathfunc::atan2 \
      [$num1 numval] [$num2 numval]]
  }
}




reg sqrt

proc ::constcl::sqrt {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num show])
  }
  MkNumber [::tcl::mathfunc::sqrt [$num numval]]
}




reg expt

proc ::constcl::expt {num1 num2} {
  check {number? $num1} {
      NUMBER expected\n([pn] [$num1 show] \
        [$num2 show])
  }
  check {number? $num2} {
      NUMBER expected\n([pn] [$num1 show] \
        [$num2 show])
  }
  MkNumber [::tcl::mathfunc::pow [$num1 numval] \
    [$num2 numval]]
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




reg number->string

proc ::constcl::number->string {num args} {
  if {[llength $args] == 0} {
    check {number? $num} {
      NUMBER expected\n([pn] [$num show])
    }
    return [MkString [$num numval]]
  } else {
    lassign $args radix
    check {number? $num} {
      NUMBER expected\n([pn] [$num show])
    }
    check {number? $radix} {
      NUMBER expected\n([pn] [$num show] \
        [$radix show])
    }
    set radices [list [MkNumber 2] [MkNumber 8] \
      [MkNumber 10] [MkNumber 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] \
        [$num show] [$radix show])
    }
    if {[$radix numval] == 10} {
      return [MkString [$num numval]]
    } else {
      return [MkString [base [$radix numval] \
        [$num numval]]]
    }
  }
}


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





reg string->number

proc ::constcl::string->number {str args} {
  if {[llength $args] == 0} {
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    return [MkNumber [$str value]]
  } else {
    lassign $args radix
    check {string? $str} {
      STRING expected\n([pn] [$str show])
    }
    set radices [list [MkNumber 2] [MkNumber 8] \
      [MkNumber 10] [MkNumber 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] [$str show] \
        [$radix show])
    }
    if {[$radix numval] == 10} {
      return [MkNumber [$str value]]
    } else {
      return [MkNumber [
        frombase [$radix numval] [$str value]]]
    }
  }
}


proc frombase {base number} {
  set digits {0 1 2 3 4 5 6 7 8 9 A B C D E F}
  set negative [regexp ^-(.+) $number -> number]
  set res 0
  foreach digit [split $number {}] {
    # dv = decimal value
    set dv [lsearch $digits $digit]
    if {$dv < 0 || $dv >= $base} {
      ::error "bad digit $dv for base $base"
    }
    set res [expr {$res * $base + $dv}]
  }
  if $negative {set res -$res}
  set res
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


oo::class create ::constcl::Boolean {
  superclass ::constcl::NIL
  variable bvalue
  constructor {v} {
    if {$v ni {#t #f}} {
      ::error "bad boolean value $v"
    }
    set bvalue $v
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method bvalue {} {
    set bvalue
  }
  method value {} {
    set bvalue
  }
  method write {handle} {
    puts -nonewline $handle [my bvalue]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set bvalue
  }
}

proc ::constcl::MkBoolean {v} {
  foreach instance [info class instances \
    ::constcl::Boolean] {
    if {[$instance bvalue] eq $v} {
      return $instance
    }
  }
  return [::constcl::Boolean new $v]
}




reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {val} {
  return [typeof? $val Boolean]
}





reg not ::constcl::not

proc ::constcl::not {val} {
  if {[$val bvalue] eq "#f"} {
    return #t
  } else {
    return #f
  }
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


oo::class create ::constcl::Char {
  superclass ::constcl::NIL
  variable value
  constructor {v} {
    switch -regexp $v {
      {(?i)#\\space} {
        set v " "
      }
      {(?i)#\\newline} {
        set v "\n"
      }
      {#\\[[:graph:]]} {
        set v [::string index $v 2]
      }
    }
    set value $v
  }
  method char {} {
    set value
  }
  method alphabetic? {} {
    if {[::string is alpha -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method numeric? {} {
    if {[::string is digit -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method whitespace? {} {
    if {[::string is space -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method upper-case? {} {
    if {[::string is upper -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method lower-case? {} {
    if {[::string is lower -strict [my char]]} {
      return #t
    } else {
      return #f
    }
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method value {} {
    return $value
  }
  method external {} {
    switch $value {
      " " {
        return "#\\space"
      }
      "\n" {
        return "#\\newline"
      }
      default {
        return "#\\$value"
      }
    }
  }
  method write {handle} {
    puts -nonewline $handle [my external]
  }
  method display {handle} {
    puts -nonewline $handle [my char]
  }
  method show {} {
    my external
  }
}

proc ::constcl::MkChar {v} {
  if {[regexp -nocase {space|newline} $v]} {
      set v [::string tolower $v]
  }
  foreach instance [
    info class instances Char] {
    if {[$instance external] eq $v} {
      return $instance
    }
  }
  return [::constcl::Char new $v]
}



reg char?

proc ::constcl::char? {val} {
  return [typeof? $val Char]
}




reg char=?

proc ::constcl::char=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {$char1 eq $char2} {
    return #t
  } else {
    return #f
  }
}


reg char<?

proc ::constcl::char<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] < [$char2 char]} {
    return #t
  } else {
    return #f
  }
}


reg char>?

proc ::constcl::char>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] > [$char2 char]} {
    return #t
  } else {
    return #f
  }
}


reg char<=?

proc ::constcl::char<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] <= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}


reg char>=?

proc ::constcl::char>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] >= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}




reg char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] eq
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}


reg char-ci<?

proc ::constcl::char-ci<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] <
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}


reg char-ci>?

proc ::constcl::char-ci>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] >
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}


reg char-ci<=?

proc ::constcl::char-ci<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] <=
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}


reg char-ci>=?

proc ::constcl::char-ci>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] >=
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}





reg char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char alphabetic?]
}


reg char-numeric?

proc ::constcl::char-numeric? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char numeric?]
}


reg char-whitespace?

proc ::constcl::char-whitespace? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char whitespace?]
}


reg char-upper-case?

proc ::constcl::char-upper-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char upper-case?]
}


reg char-lower-case?

proc ::constcl::char-lower-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char lower-case?]
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




reg char-upcase

proc ::constcl::char-upcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [MkChar [
    ::string toupper [$char value]]]
}



reg char-downcase

proc ::constcl::char-downcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [MkChar [
    ::string tolower [$char value]]]
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


catch { ::constcl::Procedure destroy }

oo::class create ::constcl::Procedure {
  superclass ::constcl::NIL
  variable parms body env
  constructor {p b e} {
    set parms $p
    set body $b
    set env $e
  }
  method value {} {}
  method write {handle} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $handle "#<proc-$num>"
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    return [self]
  }
  method call {args} {
    ::constcl::eval $body [
      ::constcl::Environment new $parms $args $env]
  }

}

interp alias {} ::constcl::MkProcedure \
  {} ::constcl::Procedure new



reg procedure?

proc ::constcl::procedure? {val} {
  if {[typeof? $val Procedure] eq "#t"} {
    return #t
  } elseif {[::string match "::constcl::*" $val]} {
    return #t
  } else {
    return #f
  }
}





reg apply

proc ::constcl::apply {pr vals} {
  check {procedure? $pr} {
    PROCEDURE expected\n([pn] [$pr show] ...)
  }
  invoke $pr $vals
}





reg map

proc ::constcl::map {pr args} {
  check {procedure? $pr} {
    PROCEDURE expected\n([pn] [$pr show] ...)
  }
  set arglists $args
  for {set i 0} \
    {$i < [llength $arglists]} \
    {incr i} {
    lset arglists $i [
      splitlist [lindex $arglists $i]]
  }
  set res {}
  for {set item 0} \
    {$item < [llength [lindex $arglists 0]]} \
    {incr item} {
    set arguments {}
    for {set arg 0} \
      {$arg < [llength $arglists]} \
      {incr arg} {
      lappend arguments [
        lindex $arglists $arg $item]
    }
    lappend res [invoke $pr [list {*}$arguments]]
  }
  return [list {*}$res]
}





reg for-each

proc ::constcl::for-each {proc args} {
  check {procedure? $proc} {
    PROCEDURE expected\n([pn] [$proc show] ...)
  }
  set arglists $args
  for {set i 0} \
    {$i < [llength $arglists]} \
    {incr i} {
    lset arglists $i [
      splitlist [lindex $arglists $i]]
  }
  for {set item 0} \
    {$item < [llength [lindex $arglists 0]]} \
    {incr item} {
    set arguments {}
    for {set arg 0} \
      {$arg < [llength $arglists]} \
      {incr arg} {
      lappend arguments [
        lindex $arglists $arg $item]
    }
    invoke $proc [list {*}$arguments]
  }
  return #NIL
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

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


catch { ::constcl::Port destroy }

oo::class create ::constcl::Port {
  variable handle
  constructor {args} {
    if {[llength $args]} {
      lassign $args handle
    } else {
      set handle #NIL
    }
  }
  method handle {} {
    set handle
  }
  method close {} {
    close $handle
    set handle #NIL
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<port-$num>"
  }
  method display {h} {
    my write $h
  }
}

oo::class create ::constcl::InputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    try {
      set handle [open [$name value] "r"]
    } on error {} {
      set handle #NIL
      return -1
    }
    return $handle
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<input-port-$num>"
  }
  method display {h} {
    my write $h
  }
}

oo::class create ::constcl::OutputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    try {
      set handle [open [$name value] "w"]
    } on error {} {
      set handle #NIL
      return -1
    }
    return $handle
  }
  method write {h} {
    regexp {(\d+)} [self] -> num
    puts -nonewline $h "#<output-port-$num>"
  }
  method display {h} {
    my write $h
  }
}

interp alias {} ::constcl::MkInputPort \
  {} ::constcl::InputPort new
interp alias {} ::constcl::MkOutputPort \
  {} ::constcl::OutputPort new

set ::constcl::Input_port [
  ::constcl::MkInputPort stdin]
set ::constcl::Output_port [
  ::constcl::MkOutputPort stdout]

reg port?

proc ::constcl::port? {val} {
  typeof? $val Port
}

reg call-with-input-file

proc ::constcl::call-with-input-file {string proc} {
  set port [open-input-file $string]
  set res [invoke $proc [list $port]]
  close-input-port $port
  return $res
}

reg call-with-output-file

proc ::constcl::call-with-output-file {string proc} {
  ::error "remove this line to use"
  set port [open-output-file $string]
  set res [invoke $proc [list $port]]
  close-output-port $port
  return $res
}

reg input-port?

proc ::constcl::input-port? {val} {
  typeof? $val InputPort
}

reg output-port?

proc ::constcl::output-port? {val} {
  typeof? $val OutputPort
}

reg current-input-port

proc ::constcl::current-input-port {} {
  return $::constcl::Input_port
}

reg current-output-port

proc ::constcl::current-output-port {} {
  return $::constcl::Output_port
}

reg with-input-from-file

proc ::constcl::with-input-from-file {string thunk} {
  set newport [open-input-file $string]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Input_port
    set ::constcl::Input_port $newport
    eval $thunk
    set ::constcl::Input_port $oldport
    close-input-port $newport
  }
}


reg with-output-to-file

proc ::constcl::with-output-to-file {string thunk} {
  ::error "remove this line to use"
  set newport [open-output-file $string]
  if {[$newport handle] ne "#NIL"} {
    set oldport $::constcl::Output_port
    set ::constcl::Output_port $newport
    eval $thunk
    set ::constcl::Output_port $oldport
    close-input-port $newport
  }
}

reg open-input-file

proc cnof {} {return "could not open file"}
proc fae {} {return "file already exists"}

proc ::constcl::open-input-file {filename} {
  set p [MkInputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-input-file: [cnof] $filename"
  }
  return $p
}

reg open-output-file

proc ::constcl::open-output-file {filename} {
  ::error "remove this line to use"
  if {[file exists $filename]} {
    error "open-output-file: [fae] $filename"
  }
  set p [MkOutputPort]
  $p open $filename
  if {[$p handle] eq "#NIL"} {
    error "open-output-file: [cnof] $filename"
  }
  return $p
}

reg close-input-port

proc ::constcl::close-input-port {port} {
  if {[$port handle] eq "stdin"} {
    error "don't close the standard input port"
  }
  $port close
}

reg close-output-port

proc ::constcl::close-output-port {port} {
  if {[$port handle] eq "stdout"} {
    error "don't close the standard output port"
  }
  $port close
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
  if {[llength $args]} {
    lassign $args port
  } else {
    set port [current-output-port]
  }
  pe "(display #\\newline $port)"
}


proc ::constcl::write-char {args} {
    # TODO
}



reg load

proc ::constcl::load {filename} {
  set p [open-input-file $filename]
  set n [read $p]
  while {$n ne "#EOF"} {
    eval $n
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

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


catch { ::constcl::Pair destroy }

oo::class create ::constcl::Pair {
  superclass ::constcl::NIL
  variable car cdr constant
  constructor {a d} {
    set car $a
    set cdr $d
    set constant 0
  }
  method name {} {}
  method value {} {
    my show
  }
  method car {} {
    set car
  }
  method cdr {} {
    set cdr
  }
  method set-car! {val} {
    ::constcl::check {my mutable?} {
      Can't modify a constant pair
    }
    set car $val
    self
  }
  method set-cdr! {val} {
    ::constcl::check {my mutable?} {
      Can't modify a constant pair
    }
    set cdr $val
    self
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    return $constant
  }
  method mutable? {} {
    expr {$constant ? "#f" : "#t"}
  }
  method write {handle} {
    puts -nonewline $handle "("
    ::constcl::write-pair $handle [self]
    puts -nonewline $handle ")"
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "(%s)" [::constcl::show-pair [self]]
  }
}


interp alias {} ::constcl::MkPair \
  {} ::constcl::Pair new




reg pair?

proc ::constcl::pair? {val} {
  typeof? $val Pair
}




proc ::constcl::show-pair {pair} {
  # take an object and print the car
  # and the cdr of the stored value
  set str {}
  set a [car $pair]
  set d [cdr $pair]
  # print car
  ::append str [$a show]
  if {[pair? $d] ne "#f"} {
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





reg cons

proc ::constcl::cons {car cdr} {
  MkPair $car $cdr
}





reg car

proc ::constcl::car {pair} {
  $pair car
}





reg cdr

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
            if {\$c eq \"a\"} {
                set pair \[car \$pair\]
            } else {
                set pair \[cdr \$pair\]
            }
        }
        return \$pair
    "

}




reg set-car!

proc ::constcl::set-car! {pair val} {
  $pair set-car! $val
}





reg set-cdr!

proc ::constcl::set-cdr! {pair val} {
  $pair set-cdr! $val
}




reg list?

proc ::constcl::list? {val} {
  set visited {}
  if {[null? $val] ne "#f"} {
      return #t
  } elseif {[pair? $val] ne "#f"} {
      return [listp $val]
  } else {
      return #f
  }
}


proc ::constcl::listp {pair} {
  upvar visited visited
  if {$pair in $visited} {
    return #f
  }
  lappend visited $pair
  if {[null? $pair] ne "#f"} {
    return #t
  } elseif {[pair? $pair] ne "#f"} {
    return [listp [cdr $pair]]
  } else {
    return #f
  }
}





reg list

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





reg length

proc ::constcl::length {pair} {
  check {list? $pair} {
    LIST expected\n([pn] lst)
  }
  MkNumber [length-helper $pair]
}


proc ::constcl::length-helper {pair} {
  if {[null? $pair] ne "#f"} {
    return 0
  } else {
    return [expr {1 +
      [length-helper [cdr $pair]]}]
  }
}





reg append

proc ::constcl::append {args} {
  set prev [lindex $args end]
  foreach r [lreverse [lrange $args 0 end-1]] {
    check {list? $r} {
      LIST expected\n([pn] [$r show])
    }
    set prev [copy-list $r $prev]
  }
  set prev
}


proc ::constcl::copy-list {pair next} {
  # TODO only fresh conses in the direct chain to NIL
  if {[null? $pair] ne "#f"} {
    set next
  } elseif {[null? [cdr $pair]] ne "#f"} {
    cons [car $pair] $next
  } else {
    cons [car $pair] [copy-list [cdr $pair] $next]
  }
}





reg reverse

proc ::constcl::reverse {vals} {
  list {*}[lreverse [splitlist $vals]]
}





reg list-tail

proc ::constcl::list-tail {vals k} {
  if {[zero? $k] ne "#f"} {
    return $vals
  } else {
    list-tail [cdr $vals] [- $k #1]
  }
}





reg list-ref

proc ::constcl::list-ref {vals k} {
  car [list-tail $vals $k]
}





reg memq

proc ::constcl::memq {val1 val2} {
  return [member-proc eq? $val1 $val2]
}



reg memv

proc ::constcl::memv {val1 val2} {
  return [member-proc eqv? $val1 $val2]
}


reg member

proc ::constcl::member {val1 val2} {
  return [member-proc equal? $val1 $val2]
}



proc ::constcl::member-proc {epred val1 val2} {
  switch $epred {
    eq? { set name "memq" }
    eqv? { set name "memv" }
    equal? { set name "member" }
  }
  check {list? $val2} {
    LIST expected\n($name [$val1 show] [$val2 show])
  }
  if {[null? $val2] ne "#f"} {
    return #f
  } elseif {[pair? $val2] ne "#f"} {
    if {[$epred $val1 [car $val2]] ne "#f"} {
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
  check {list? $val2} {
    LIST expected\n($name [$val1 show] [$val2 show])
  }
  if {[null? $val2] ne "#f"} {
    return #f
  } elseif {[pair? $val2] ne "#f"} {
    if {[pair? [car $val2]] ne "#f" && 
      [$epred $val1 [caar $val2]] ne "#f"} {
      return [car $val2]
    } else {
      return [assoc-proc $epred $val1 [cdr $val2]]
    }
  }
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


oo::class create ::constcl::String {
  superclass ::constcl::NIL
  variable data constant
  constructor {v} {
    set v [string map {\\\\ \\ \\\" \" \\n \n} $v]
    set len [::string length $v]
    set vsa [::constcl::vsAlloc $len]
    set idx $vsa
    foreach elt [split $v {}] {
      if {$elt eq " "} {
        set c #\\space
      } elseif {$elt eq "\n"} {
        set c #\\newline
      } else {
        set c #\\$elt
      }
      lset ::constcl::vectorSpace $idx \
        [::constcl::MkChar $c]
      incr idx
    }
    set data [
      ::constcl::cons [N $vsa] [N $len]]
    set constant 0
  }
  method = {str} {
    ::string equal [my value] [$str value]
  }
  method cmp {str} {
    ::string compare [my value] [$str value]
  }
  method length {} {
    ::constcl::cdr $data
  }
  method ref {k} {
    set k [$k numval]
    if {$k < 0 || $k >= [[my length] numval]} {
      ::error "index out of range\n$k"
    }
    lindex [my store] $k
  }
  method store {} {
    set base [[::constcl::car $data] numval]
    set end [expr {[[my length] numval] +
      $base - 1}]
    lrange $::constcl::vectorSpace $base $end
  }
  method value {} {
    join [lmap c [my store] {$c char}] {}
  }
  method set! {k c} {
    if {[my constant]} {
      ::error "string is constant"
    } else {
      set k [$k numval]
      if {$k < 0 ||
        $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[::constcl::car $data] numval]
      lset ::constcl::vectorSpace $k+$base $c
    }
    return [self]
  }
  method fill! {c} {
    if {[my constant]} {
      ::error "string is constant"
    } else {
      set base [[::constcl::car $data] numval]
      set len [[my length] numval]
      for {set idx $base} \
        {$idx < $len+$base} \
        {incr idx} {
        lset ::constcl::vectorSpace $idx $c
      }
    }
    return [self]
  }
  method substring {from to} {
    join [lmap c [lrange [my store] \
      [$from numval] [$to numval]] {$c char}] {}
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method external {} {
    return "\"[
      string map {\\ \\\\ \" \\\" \n \\n} [my value]]\""
  }
  method write {handle} {
    puts -nonewline $handle [my external]
  }
  method display {handle} {
    puts -nonewline $handle [my value]
  }
  method show {} {
    my external
  }
}

interp alias {} ::constcl::MkString \
  {} ::constcl::String new



reg string?

proc ::constcl::string? {val} {
  typeof? $val String
}





reg make-string

proc ::constcl::make-string {k args} {
  if {[llength $args] == 0} {
    return [MkString [::string repeat " " \
      [$k numval]]]
  } else {
    lassign $args char
    return [MkString [::string repeat \
      [$char char] [$k numval]]]
  }
}





reg string

proc ::constcl::string {args} {
  set str {}
  foreach char $args {
    check {::constcl::char? $char} {
      CHAR expected\n([pn] [lmap c $args \
        {$c show}])
    }
    ::append str [$char char]
  }
  return [MkString $str]
}





reg string-length

proc ::constcl::string-length {str} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkNumber [[$str length] numval]]
}





reg string-ref

proc ::constcl::string-ref {str k} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show] \
      [$k show])
  }
  check {::constcl::number? $k} {
    Exact INTEGER expected\n([pn] [$str show] \
      [$k show])
  }
  return [$str ref $k]
}





reg string-set!

proc ::constcl::string-set! {str k char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$k show] \
      [$char show])
  }
  check {number? $k} {
    Exact INTEGER expected\n([pn] [$str show] \
      [$k show] [$char show])
  }
  check {char? $char} {
    CHAR expected\n([pn] [$str show] [$k show] \
      [$char show])
  }
  $str set! $k $char
  return $str
}






reg string=?

proc ::constcl::string=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] eq [$str2 value]} {
    return #t
  } else {
    return #f
  }
}


reg string-ci=?

proc ::constcl::string-ci=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] eq
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}


reg string<?

proc ::constcl::string<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] < [$str2 value]} {
    return #t
  } else {
    return #f
  }
}


reg string-ci<?

proc ::constcl::string-ci<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] <
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}


reg string>?

proc ::constcl::string>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] > [$str2 value]} {
    return #t
  } else {
    return #f
  }
}


reg string-ci>?

proc ::constcl::string-ci>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] >
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}


reg string<=?

proc ::constcl::string<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] <= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}


reg string-ci<=?

proc ::constcl::string-ci<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] <=
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}


reg string>=?

proc ::constcl::string>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] >= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}


reg string-ci>=?

proc ::constcl::string-ci>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] >=
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}





reg substring

proc ::constcl::substring {str start end} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  check {number? $start} {
    NUMBER expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  check {number? $end} {
    NUMBER expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  return [MkString [$str substring $start $end]]
}





reg string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {
      $arg value
    }]]
}





reg string->list

proc ::constcl::string->list {str} {
  list {*}[$str store]
}





reg list->string

proc ::constcl::list->string {list} {
  MkString [::append --> {*}[
    lmap c [splitlist $list] {$c char}]]
}





reg string-copy

proc ::constcl::string-copy {str} {
  check {string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkString [$str value]]
}





reg string-fill!

proc ::constcl::string-fill! {str char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] \
      [$char show])
  }
  $str fill! $char
  return $str
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


oo::class create ::constcl::Symbol {
  superclass ::constcl::NIL
  variable name caseconstant
  constructor {n} {
    if {   no &&   $n eq {}} {
      ::error "a symbol must have a name"
    }
    ::constcl::idcheck $n
    set name $n
    set caseconstant 0
  }
  method name {} {
    set name
  }
  method value {} {
    set name
  }
  method = {symname} {
    expr {$name eq $symname}
  }
  method mkconstant {} {}
  method constant {} {
    return 1
  }
  method make-case-constant {} {
    set caseconstant 1
  }
  method case-constant {} {
    set caseconstant
  }
  method write {handle} {
    puts -nonewline $handle [my name]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    set name
  }
}

unset -nocomplain ::constcl::symbolTable
set ::constcl::symbolTable [dict create]

proc ::constcl::MkSymbol {n} {
  if {[dict exists $::constcl::symbolTable $n]} {
    return [dict get $::constcl::symbolTable $n]
  } else {
    set sym [::constcl::Symbol new $n]
    dict set ::constcl::symbolTable $n $sym
    return $sym
  }
}
interp alias {} S {} ::constcl::MkSymbol


reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {val} {
  typeof? $val Symbol
}




reg symbol->string ::constcl::symbol->string

proc ::constcl::symbol->string {sym} {
  check {symbol? $sym} {
    SYMBOL expected\n([pn] [$sym show])
  }
  if {![$sym case-constant]} {
    set str [MkString [
      ::string tolower [$sym name]]]
  } else {
    set str [MkString [$sym name]]
  }
  $str mkconstant
  return $str
}






reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {str} {
  check {string? $str} {
    STRING expected\n([pn] [$obj show])
  }
  set sym [MkSymbol [$str value]]
  $sym make-case-constant
  return $sym
}

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et  


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
    set data [::constcl::cons [N $vsa] [N $len]]
    set constant 0
  }
  method baseadr {} {
    ::constcl::car $data
  }
  method length {} {
    ::constcl::cdr $data
  }
  method ref {k} {
    set k [$k numval]
    if {$k < 0 || $k >= [[my length] numval]} {
      ::error "index out of range\n$k"
    }
    lindex [my store] $k
  }
  method store {} {
    set base [[my baseadr] numval]
    set end [expr {[[my length] numval] +
      $base - 1}]
    lrange $::constcl::vectorSpace $base $end
  }
  method value {} {
    my store
  }
  method set! {k obj} {
    if {[my constant]} {
      ::error "vector is constant"
    } else {
      set k [$k numval]
      if {$k < 0 || $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[my baseadr] numval]
      lset ::constcl::vectorSpace $k+$base $obj
    }
    return [self]
  }
  method fill! {val} {
    if {[my constant]} {
      ::error "vector is constant"
    } else {
      set base [[my baseadr] numval]
      set len [[my length] numval]
      for {set idx $base} \
        {$idx < $len+$base} \
        {incr idx} {
        lset ::constcl::vectorSpace $idx $val
      }
    }
    return [self]
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method write {handle} {
    puts -nonewline $handle [my show]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "#(%s)" [
      join [lmap val [my value] {$val show}]]
  }
}

interp alias {} ::constcl::MkVector \
  {} ::constcl::Vector new



reg vector? ::constcl::vector?

proc ::constcl::vector? {val} {
  typeof? $val Vector
}





reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {k args} {
  if {[llength $args] == 0} {
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
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show])
  }
  return [$vec length]
}





reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] [$k show])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec show] [$k show])
  }
  return [$vec ref $k]
}





reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k val} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] [$k show])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec show] [$k show])
  }
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
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] \
      [$fill show])
  }
  $vec fill! $fill
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


proc ::constcl::idcheckinit {init} {
  if {[::string is alpha -strict $init] ||
    $init in {! $ % & * / : < = > ? ^ _ ~}} {
    return true
  } else {
    return false
  }
}

proc ::constcl::idchecksubs {subs} {
  foreach c [split $subs {}] {
    if {!([::string is alnum -strict $c] ||
      $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
      return false
    }
  }
  return true
}

proc ::constcl::idcheck {sym} {
  if {$sym eq {}} {return $sym}
  if {(![idcheckinit [::string index $sym 0]] ||
    ![idchecksubs [::string range $sym 1 end]]) &&
    $sym ni {+ - ...}} {
    ::error "Identifier expected ($sym)"
  }
  set sym
}

proc ::constcl::varcheck {sym} {
  if {$sym in {
    else => define unquote unquote-splicing
    quote lambda if set! begin cond and or
    case let let* letrec do delay quasiquote
  }} {
    ::error "Variable name is reserved: $sym"
  }
  return $sym
}

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 

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
  if {[$::constcl::InputPort handle] eq "#NIL"} {
    error "input port is not open"
  }
  set ::constcl::Level 0
  return [read-form 0]
}

proc ::constcl::read_c_ci {} {
  tolower [
    ::read [
      $::constcl::Input_port handle] 1]]
}


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 

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

interp alias {} #-1 {} [N -1]

interp alias {} #0 {} [N 0]

interp alias {} #1 {} [N 1]

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #UNS {} [::constcl::Unspecified new]

interp alias {} #UND {} [::constcl::Undefined new]

interp alias {} #EOF {} [::constcl::EndOfFile new]



dict set ::constcl::defreg pi [N 3.1415926535897931]


reg nil #NIL


# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 




proc ::constcl::input {prompt} {
  puts -nonewline $prompt
  flush stdout
  set buf [gets stdin]
  set openpars [regexp -all -inline {\(} $buf]
  set clsepars [regexp -all -inline {\)} $buf]
  set openbrak [regexp -all -inline {\[} $buf]
  set clsebrak [regexp -all -inline {\]} $buf]
  while {[llength $openpars] > [llength $clsepars] ||
         [llength $openbrak] > [llength $clsebrak]} {
    ::append buf [gets stdin]
    set openpars [regexp -all -inline {\(} $buf]
    set clsepars [regexp -all -inline {\)} $buf]
    set openbrak [regexp -all -inline {\[} $buf]
    set clsebrak [regexp -all -inline {\]} $buf]
  }
  return $buf
}


proc ::repl {{prompt "ConsTcl> "}} {
  set str [::constcl::input $prompt]
  while {$str ne ""} {
    pep $str
    set str [::constcl::input $prompt]
  }
}

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 

catch { ::constcl::Environment destroy }

oo::class create ::constcl::Environment {
  variable bindings outer_env
  constructor {syms vals {outer {}}} {
    set bindings [dict create]
    if {[::constcl::null? $syms] eq "#t"} {
      if {[llength $vals]} {
        error "too many arguments"
      }
    } elseif {[::constcl::list? $syms] eq "#t"} {
      set syms [::constcl::splitlist $syms]
      set symsn [llength $syms]
      set valsn [llength $vals]
      if {$symsn != $valsn} {
        error [
          ::append --> "wrong # of arguments, " \
            "$valsn instead of $symsn"
      }
      foreach sym $syms val $vals {
        my set $sym $val
      }
    } elseif {[::constcl::symbol? $syms] eq "#t"} {
      my set $syms [::constcl::list {*}$vals]
    } else {
      while true {
        if {[llength $vals] < 1} {
          error "too few arguments"
        }
        my set [::constcl::car $syms] \
          [lindex $vals 0]
        set vals [lrange $vals 1 end]
        if {[
          ::constcl::symbol? [
            ::constcl::cdr $syms]] eq "#t"} {
          my set [::constcl::cdr $syms] \
            [::constcl::list {*}$vals]
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

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 


::constcl::Environment create \
  ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
  method find {sym} {
    self
  }
  method get {sym} {
    ::error "Unbound variable: [$sym name]"
  }
  method set {sym val} {
    ::error "Unbound variable: [$sym name]"
  }
}


namespace eval ::constcl {
  set keys [list {*}[lmap key [dict keys $defreg] {
    S $key
  }]]
  set vals [dict values $defreg]
  Environment create global_env $keys $vals \
    ::constcl::null_env
}


pe {(load "schemebase.scm")}





# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
