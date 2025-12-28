namespace eval ::constcl {}


${::log}::info "In the beginning" 


unset -nocomplain ::constcl::defreg


lappend ::auto_path /mnt/c/Tcl/tcl8.6.17/library

package require logger

# due to Nathan Coulter
proc log_to_file {lvl txt} {
  set logfile "mylog.log"
  set msg "\[[clock format [clock seconds]]\] $txt"
  try {
    open $logfile {WRONLY CREAT APPEND}
  } on ok f {
    fconfigure $f -encoding utf-8
    set c [::string index $lvl 0]
    puts $f $msg
    # .y.txt insert end "$c: $msg\n"
  } on error m {
    ::error $m
  } finally {
    catch {close $f}
  }
}

set log [logger::init global]
foreach lvl [logger::levels] {
  interp alias {} log_to_file_$lvl {} log_to_file $lvl
  ${log}::logproc $lvl log_to_file_$lvl
}

${log}::info "Logging to a file" 


proc ::assert {str expr {body {}}} {

  set truth [T [V [expr $expr]]]

  ::if {!$truth} {
    ::if {$body ne {}} {
      set resultList [::list "Failed assertion" $str]
      ::error [join $resultList ": "]
    } else {
      uplevel $body
    }
  }
  return {}
}


proc reg {args} {
  assert "# of parameters = (1|2)" [expr {[llength $args] in {1 2}}]
  ::if {[llength $args] == 2} {
    lassign $args btype name
  } elseif {[llength $args] == 1} {
    lassign $args name
    set btype {}
  }
  assert "defreg exists" [expr {[info exists ::constcl::defreg]}] {
    set ::constcl::defreg [dict create]
  }
  switch $btype {
    special {
      set val [::list SPECIAL ::constcl::special-$name]
    }
    macro {
      set val [::list SYNTAX ::constcl::expand-$name]
    }
    default {
      set val [::list VARIABLE ::constcl::$name]
    }
  }
  set idx [dict size $::constcl::defreg]
  dict set ::constcl::defreg $idx [::list $name $val]
  return
}

proc regvar {name value} {
  assert "defreg exists" [expr {[info exists ::constcl::defreg]}] {
    set ::constcl::defreg [dict create]
  }
  set val [::list VARIABLE $value]
  set idx [dict size $::constcl::defreg]
  dict set ::constcl::defreg $idx [::list $name $val]
  return
}


::if {$tcl_version eq "8.6"} {
  set singletonClass oo::class
} elseif {$tcl_version eq "9.0"} {
  set singletonClass oo::singleton
} else {
  set singletonClass oo::class
}

proc singleton {objclass objname} {
  # a fake singleton, but it will do
  set obj [uplevel #0 [::list oo::object create $objname]]
  oo::objdefine $obj class ::constcl::$objclass
  oo::objdefine $obj {
    unexport destroy
  }
  uplevel #0 [::list set $objname $obj]
  return $obj
}



$singletonClass create ::constcl::EndOfFile {
  method write {{val {}} {port {}}} {
    format "#<end-of-file>"
  }
}

singleton EndOfFile #EOF


proc eof? {val} {
  ::if {$val eq ${::#EOF}} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}


#oo::singleton create ::constcl::NIL
$singletonClass create ::constcl::NIL {
  method write {{val {}} {port {}}} {
    return "()"
  }
}

singleton NIL #NIL


reg null?

proc ::constcl::null? {val} {
  ::if {$val eq ${::#NIL}} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}


#oo::singleton create ::constcl::Undefined
$singletonClass create ::constcl::Undefined {
  method write {{val {}} {port {}}} {
    format "#<undefined>"
  }
}

singleton Undefined #UND


#oo::singleton create ::constcl::Unspecified
$singletonClass create ::constcl::Unspecified {
  method write {{val {}} {port {}}} {
    format "#<unspecified>"
  }
}

singleton Unspecified #UNS


$singletonClass create ::constcl::True {
  method write {val {port {}}} {
    return ${::#t}
  }
}

singleton True #t


$singletonClass create ::constcl::False {
  method write {val {port {}}} {
    return ${::#f}
  }
}

singleton False #f

proc ::T {val} {
  ::if {$val eq ${::#f}} {
    return 0
  } else {
    return 1
  }
}


proc ::V {val} {
  ::if {[string is true -strict $val]} {
    return ${::#t}
  } elseif {[string is false -strict $val]} {
    return ${::#f}
  } else {
    ::error "not a Tcl truth value"
  }
}

reg atom?

proc ::constcl::atom? {val} {
  foreach type {symbol number string
    char boolean vector port eof} {
      ::if {[$type? $val] eq ${::#t}} {
        return ${::#t}
      }
    }
    return ${::#f}
}

proc ::constcl::pairlis-tcl {a b} {
  ::if {[T [null? $a]]} {
    parse {'()}
  } else {
    cons \
      [cons [car $a] [car $b]] \
      [pairlis-tcl [cdr $a] [cdr $b]]
  }
}
proc ::pn {} {
  namespace tail [lindex [info level -1] 0]
}
proc ::unbind {args} {
  try {
    uplevel [list subst \$env]
  } on ok env {
  } on error {} {
    set env ::constcl::global_env
  }
  set syms $args
  foreach sym $syms {
    set e [$env find $sym]
    ::if {$e ne "::constcl::null_env"} {
      $e unbind $sym
    }
  }
}
proc ::constcl::typeof? {val type} {
  ::if {[info object isa typeof $val $type]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
proc ::constcl::splitlist {vals} {
  set result {}
  while {[T [pair? $vals]]} {
    lappend result [car $vals]
    set vals [cdr $vals]
  }
  return $result
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
reg error

proc ::constcl::error {msg args} {
  set exprs $args
  ::if {[llength $exprs]} {
    set res [lmap expr $exprs {
      $expr tstr
    }]
    ::append msg " (" [join $res] ")"
  }
  ::error $msg
}
proc ::constcl::check {cond msg} {
  ::if {[uplevel $cond] eq ${::#f}} {
    ::error [
      uplevel [
        ::list subst [
          ::string trim $msg]]]
  }
}
proc ::pew {str {env ::constcl::global_env}} {
  ::constcl::write [
    ::constcl::eval [parse $str] $env]
}
proc ::rew {port {env ::constcl::global_env}} {
  ::constcl::write [
    ::constcl::eval [
      ::constcl::read $port] $env]
}
proc ::pw {str} {
  ::constcl::write [parse $str]
}
proc ::rw {args} {
  ::constcl::write [::constcl::read {*}$args]
}
proc ::pe {str {env ::constcl::global_env}} {
  ::constcl::eval [parse $str] $env
}
proc ::re {port {env ::constcl::global_env}} {
  ::constcl::eval [::constcl::read $port] $env
}
proc ::parse {str} {
  ::constcl::read [
    ::constcl::MkStringInputPort $str]
}
proc ::e {expr {env ::constcl::global_env}} {
  ::constcl::eval $expr $env
}
proc ::w {val} {
  ::constcl::write $val
}
proc ::r {args} {
  ::constcl::read {*}$args
}
proc ::prw {str} {
  set expr [parse $str]
  set expr [::constcl::resolve-local-defines \
    [::constcl::cdr $expr]]
  ::constcl::write $expr
}
proc ::pxw {str {env ::constcl::global_env}} {
  set expr [parse $str]
  set op [::constcl::car $expr]
  lassign [::constcl::binding-info $op $env] btype hinfo
  ::if {$btype eq "SYNTAX"} {
    set expr [$hinfo $expr $env]
    ::constcl::write $expr
  } else {
    ::error "not a macro"
  }
}
catch { ::constcl::Base destroy }

::if {$tcl_version eq "8.6"} {
    set abstractClass oo::class
} elseif {$tcl_version eq "9.0"} {
    set abstractClass oo::abstract
} else {
    set abstractClass oo::class
}

$abstractClass create ::constcl::Base {
  method mkconstant {} {}
  method write {{val {}} {port {}}} {
    return "#<base>"
  }


  method display {{val {}} {port {}}} {
    my write
  }

}

  method unknown {name args} {
    switch $name {
      car - cdr - set-car! -
      set-cdr {
        ::error "PAIR expected"
      }
      numval {
        ::error "NUMBER expected"
      }
    }
  }
}


oo::class create ::constcl::Dot {
  superclass ::constcl::Base
  method write {} {
    format "."
  }
}


proc ::constcl::dot? {val} {
  typeof? $val "Dot"
}



reg read

proc ::constcl::read {args} {
  upvar #0 c c unget unget
  set c {}
  set unget {}
  set expr ${::#NIL}
  set oldport $::constcl::Input_port
  ::if {[llength $args]} {
    lassign $args port
    set ::constcl::Input_port $port
  } else {
    set port $oldport
  }
  if {[$port eof]} {
    long-eof ${::#EOF}
  } else {
    set expr [read-expr]
    long-eof $expr
  }
  set expr [read-expr]
  set ::constcl::Input_port $oldport
  return $expr
}
proc ::constcl::read-expr {args} {
  upvar c c unget unget
  ::if {[llength $args]} {
    lassign $args c
  } else {
    set c [readchar]
  }
  set unget {}
  if {[::string is space $c] || $c eq ";"} {
    skip-ws
    read-eof $c
  }
  switch -regexp $c {
    {\"}          { read-string-expr }
    {\#}          { read-pound }
    {\'}          { read-quoted-expr }
    {\(}          { read-pair-expr ")" }
    {\+} - {\-}   { read-plus-minus $c }
    {\,}          { read-unquoted-expr }
    {\.} {
        set x [Dot new]; set c [readchar]; set x
    }
    {\[}          { read-pair-expr "\]" }
    {\`}          { read-quasiquoted-expr }
    {\d}          { read-number-expr $c }
    {^$}          { return ${::#EOF} }
    {[[:graph:]]} { read-identifier-expr $c }
    default {
      read-eof $c
      ::error "unexpected character ($c)"
    }
  }
}
proc ::constcl::read-character-expr {} {
  upvar c c unget unget
  set name "#\\"
  set c [readchar]
  read-eof $c
  ::if {[::string is graph $c]} {
    ::append name $c
    set c [readchar]
    while {[::string is alpha $c]} {
      ::append name $c
      set c [readchar]
    }
  }
  assert "$name is valid" [valid-char? $name]
  set expr [MkChar $name]
  read-eof $expr
  return $expr
}
proc ::constcl::read-identifier-expr {args} {
  upvar c c unget unget
  set unget {}
  ::if {[llength $args]} {
    set c [join $args {}]
  } else {
    set c [readchar]
  }
  read-eof $c
  set name {}
  while {[::string is graph -strict $c]} {
    ::if {$c eq "${::#EOF}" || [T [delimiter? $c]]} {
      break
    }
    ::append name $c
    set c [readchar]
    # do not check for EOF here
  }
  ::if {[T [delimiter? $c]]} {
    set unget $c
  }
  # idcheck throws error if invalid identifier
  idcheck $name
  return [S $name]
}
proc ::constcl::read-number-expr {args} {
  upvar c c unget unget
  set unget {}
  ::if {[llength $args]} {
    lassign $args c
  } else {
    set c [readchar]
  }
  read-eof $c
  while {![T [interspace? $c]] && $c ne "${::#EOF}" &&
      ![T [delimiter? $c]]} {
    ::append num $c
    set c [readchar]
  }
  ::if {[T [delimiter? $c]]} {
    set unget $c
  }
  check {::string is double -strict $num} {
      Invalid numeric constant $num
  }
  set expr [N $num]
  return $expr
}
proc ::constcl::read-pair-expr {char} {
  upvar c c unget unget
  set unget {}
  set expr [read-pair $char]
  read-eof $expr
  ::if {$c ne $char} {
    ::if {$char eq ")"} {
      ::error \
        "Missing right paren. ($c)."
    } else {
      ::error \
        "Missing right bracket ($c)."
    }
  } else {
    set unget {}
    set c [readchar]
  }
  return $expr
}
proc ::constcl::read-pair {char} {
  upvar c c unget unget
  set c [readchar]
  read-eof $c
  ::if {[T [find-char? $char]]} {
    return ${::#NIL}
  }
  set a [read-expr $c]
  set res $a
  skip-ws
  set prev ${::#NIL}
  while {![T [find-char? $char]]} {
    set x [read-expr $c]
    skip-ws
    read-eof $c
    ::if {[T [dot? $x]]} {
      set prev [read-expr $c]
      skip-ws
      read-eof $c
    } else {
      lappend res $x
    }
  }
  foreach r [lreverse $res] {
    set prev [cons $r $prev]
  }
  return $prev
}
proc ::constcl::read-plus-minus {char} {
  upvar c c unget unget
  set unget {}
  set c [readchar]
  read-eof $c
  ::if {[::string is digit -strict $c]} {
    set expr [read-number-expr $c]
    read-eof $expr
    ::if {$char eq "-"} {
      set expr [- $expr]
    }
    return $expr
  } elseif {[T [interspace? $c]] ||
      [T [delimiter? $c]]} {
    ::if {$char eq "+"} {
      return [S "+"]
    } else {
      return [S "-"]
    }
  } else {
    set expr [read-identifier-expr $char $c]
    read-eof $expr
    return $expr
  }
}
proc ::constcl::read-pound {} {
  upvar c c unget unget
  set unget {}
  set c [readchar]
  read-eof $c
  switch $c {
    (    { set expr [read-vector-expr] }
    t    { ::if {[T [read-end?]]} {set expr ${::#t}} }
    f    { ::if {[T [read-end?]]} {set expr ${::#f}} }
    "\\" { set expr [read-character-expr] }
    default {
      ::error "Illegal #-literal: #$c"
    }
  }
  return $expr
}
proc ::constcl::read-quasiquoted-expr {} {
  upvar c c unget unget
  set unget {}
  set expr [read-expr]
  read-eof $expr
  make-constant $expr
  return [list [S quasiquote] $expr]
}
proc ::constcl::read-quoted-expr {} {
  upvar c c unget unget
  set unget {}
  set expr [read-expr]
  read-eof $expr
  make-constant $expr
  return [list [S quote] $expr]
}
proc ::constcl::read-string-expr {} {
  upvar c c unget unget
  set str {}
  set c [readchar]
  read-eof $c
  while {$c ne "\"" && $c ne "${::#EOF}"} {
    ::if {$c eq "\\"} {
      ::append str $c
      set c [readchar]
      read-eof $c
    }
    ::append str $c
    set c [readchar]
  }
  ::if {$c eq "${::#EOF}"} {
    ::error "bad string (no ending double quote)"
  }
  set c [readchar]
  set expr [MkString $str]
  make-constant $expr
  return $expr
}
proc ::constcl::read-unquoted-expr {} {
  upvar c c unget unget
  set unget {}
  set c [readchar]
  read-eof $c
  ::if {$c eq "@"} {
    set symbol "unquote-splicing"
    set expr [read-expr]
  } else {
    set symbol "unquote"
    set expr [read-expr $c]
  }
  read-eof $expr
  return [list [S $symbol] $expr]
}
proc ::constcl::read-vector-expr {} {
  upvar c c unget unget
  set res {}
  set last {}
  set c [readchar]
  while {$c ne "${::#EOF}" && $c ne ")"} {
    set e [read-expr $c]
    read-eof $e
    set elem [cons $e ${::#NIL}]
    ::if {$res eq {}} {
      set res $elem
    } else {
      set-cdr! $last $elem
    }
    set last $elem
    skip-ws
    read-eof $c
  }
  ::if {$c ne ")"} {
    ::error "Missing right paren. ($c)."
  }
  set unget {}
  set c [readchar]
  set expr [MkVector $res]
  $expr mkconstant
  return $expr
}
proc ::constcl::make-constant {val} {
  ::if {[T [pair? $val]]} {
    $val mkconstant
    make-constant [car $val]
    make-constant [cdr $val]
  } elseif {[T [null? $val]]} {
    return
  } else {
    $val mkconstant
    return
  }
}
proc ::constcl::interspace? {c} {
  ::if {[::string is space $c]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
proc ::constcl::delimiter? {c} {
  ::if {$c in {( ) ; \" ' ` | [ ] \{ \}}} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
proc ::constcl::valid-char? {name} {
  ::if {[regexp {(?i)^#\\([[:graph:]]|space|newline)$} \
      $name]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
proc ::constcl::readchar {} {
  upvar unget unget
  ::if {$unget ne {}} {
    set c $unget
    set unget {}
  } else {
    set c [$::constcl::Input_port get]
    ::if {[$::constcl::Input_port eof]} {
      return ${::#EOF}
    }
  }
  return $c
}
proc ::constcl::find-char? {char} {
  upvar c c unget unget
  # start with stored c
  while {[::string is space -strict $c]} {
    # this order seems strange but works
    set c [readchar]
    read-eof $c
    set unget $c
  }
  expr {($c eq $char) ? ${::#t} : ${::#f}}
}
proc ::constcl::read-end? {} {
  upvar c c unget unget
  set c [readchar]
  ::if {[T [interspace? $c]]} {
    return ${::#t}
  } elseif {[T [delimiter? $c]]} {
    set unget $c
    return ${::#t}
  } elseif {$c eq "${::#EOF}"} {
    return ${::#EOF}
  } else {
    set unget $c
    return ${::#f}
  }
}
proc ::constcl::skip-ws {} {
  upvar c c unget unget
  while true {
    switch -regexp $c {
      {[[:space:]]} {
        set c [readchar]
      }
      {;} {
        while {$c ne "\n" && $c ne "${::#EOF}"}  {
          set c [readchar]
        }
      }
      default {
        set unget $c
        return
      }
    }
  }
}
proc ::constcl::read-eof {args} {
  set chars $args
  foreach char $chars {
    ::if {$char eq "${::#EOF}"} {
      return -level 1 -code return ${::#EOF}
    }
  }
}
proc ::constcl::lookup {sym env} {
  lassign [binding-info $sym $env] type value
  ::if {$type eq "VARIABLE"} {
    return $value
  } else {
    error "not a variable name" $sym
  }
}
proc ::constcl::self-evaluating? {val} {
  ::if {[T [number? $val]] ||
    [T [string? $val]] ||
    [T [char? $val]] ||
    [T [boolean? $val]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg special quote

proc ::constcl::special-quote {expr env} {
  cadr $expr
}
reg special if

proc ::constcl::special-if {expr env} {
  set args [cdr $expr]
  ::if {[T [null? [cddr $args]]]} {
    ::if {[T [eval [car $args] $env]]} \
      {eval [cadr $args] $env}
  } else {
    ::if {[T [eval [car $args] $env]]} \
      {eval [cadr $args] $env} \
      {eval [caddr $args] $env}
  }
}
reg special case

proc ::constcl::special-case {expr env} {
  set tail [cdr $expr]
  set expr [do-case [car $tail] [cdr $tail] $env]
  eval $expr $env
}
proc ::constcl::do-case {keyexpr clauses env} {
  ::if {[T [null? $clauses]]} {
    return [parse "'()"]
  } else {
    set datumlist [caar $clauses]
    set body [cdar $clauses]
    set predicate [list [S memv] $keyexpr \
        [list [S quote] $datumlist]]
    ::if {[T [eq? [length $clauses] [N 1]]]} {
      ::if {[T [eq? [caar $clauses] [S else]]]} {
        set predicate ${::#t}
      }
    }
    set env [MkEnv $env]
    /define [S predicate] $predicate $env
    /define [S body] $body $env
    /define [S rest] [
      do-case $keyexpr [cdr $clauses] $env] $env
    set qq "`(if ,predicate
               (begin ,@body)
               ,rest)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
reg special cond

proc ::constcl::special-cond {expr env} {
  set expr [do-cond [cdr $expr] $env]
  eval $expr $env
}
proc ::constcl::do-cond {tail env} {
  set clauses $tail
  ::if {[T [null? $clauses]]} {
    return [parse "'()"]
  } else {
    set predicate [caar $clauses]
    set body [cdar $clauses]
    ::if {[T [eq? [length $clauses] [N 1]]]} {
      ::if {[T [eq? $predicate [S else]]]} {
        set predicate ${::#t}
      }
    }
    ::if {[T [symbol? [car $body]]] &&
        [[car $body] name] eq "=>"} {
      set body [list [caddar $clauses] $predicate]
    } else {
      ::if {[[length $body] numval] == 0} {
        set body $predicate
      } else {
        set body [cons [S begin] $body]
      }
    }
    set env [MkEnv $env]
    /define [S predicate] $predicate $env
    /define [S body] $body $env
    /define [S rest] [
      do-cond [cdr $clauses] $env] $env
    set qq "`(if ,predicate
               ,body
               ,rest)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
reg special begin

proc ::constcl::special-begin {expr env} {
  ::if {$env ne "::constcl::global_env" &&
    [T [pair? [cadr $expr]]] &&
    [T [eq? [caadr $expr] [S define]]]
  } then {
    set expr [resolve-local-defines $expr]
    eval $expr $env
  } else {
    /begin [cdr $expr] $env
  }
}
proc ::constcl::/begin {exps env} {
  ::if {[T [pair? $exps]]} {
    ::if {[T [pair? [cdr $exps]]]} {
      eval [car $exps] $env
      return [/begin [cdr $exps] $env]
    } else {
      return [eval [car $exps] $env]
    }
  } else {
    return [parse "'()"]
  }
}
reg special define

proc ::constcl::special-define {expr env} {
  set expr [rewrite-define $expr $env]
  set sym [cadr $expr]
  set val [eval [caddr $expr] $env]
  /define $sym $val $env
}
proc ::constcl::rewrite-define {expr env} {
  ::if {[T [pair? [cadr $expr]]]} {
    set tail [cdr $expr]
    set env [::constcl::MkEnv $env]
    /define [S tail] $tail $env
    set qq "`(define ,(caar tail)
               (lambda ,(cdar tail) ,@(cdr tail)))"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
  }
  return $expr
}
proc ::constcl::/define {sym val env} {
  varcheck [idcheck [$sym name]]
  # will throw an error if $sym is already bound
  $env bind $sym VARIABLE $val
  return
}
reg special set!

proc ::constcl::special-set! {expr env} {
  set args [cdr $expr]
  set var [car $args]
  set val [eval [cadr $args] $env]
  [$env find $var] assign $var VARIABLE $val
  set val
}
reg special lambda

proc ::constcl::special-lambda {expr env} {
  set args [cdr $expr]
  set formals [car $args]
  set body [cons [S begin] [cdr $args]]
  return [MkProcedure $formals $body $env]
}
proc ::constcl::invoke {pr vals} {
  check {procedure? $pr} {
    PROCEDURE expected\n([$pr tstr] val ...)
  }
  ::if {[info object isa object $pr]} {
    $pr call {*}[splitlist $vals]
  } else {
    $pr {*}[splitlist $vals]
  }
}
reg special let

proc ::constcl::special-let {expr env} {
  ::if {[T [symbol? [cadr $expr]]]} {
    set expr [rewrite-named-let $expr $env]
  }
  set expr [rewrite-let $expr $env]
  eval $expr $env
}
proc ::constcl::rewrite-named-let {expr env} {
  set variable [cadr $expr]
  set bindings [caddr $expr]
  set body [cdddr $expr]
  set vars [dict create $variable ${::#f}]
  parse-bindings vars $bindings
  set env [MkEnv $env]
  /define [S decl] [list {*}[dict values [
    dict map {k v} $vars {list $k $v}]]] $env
  /define [S variable] $variable $env
  /define [S varlist] [list {*}[lrange [
    dict keys $vars] 1 end]] $env
  /define [S body] $body $env
  /define [S call] [list {*}[
    dict keys $vars]] $env
  set qq "`(let ,decl
             (set!
               ,variable
                 (lambda ,varlist ,@body)) ,call)"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
proc ::constcl::rewrite-let {expr env} {
  set bindings [cadr $expr]
  set body [cddr $expr]
  set vars [dict create]
  parse-bindings vars $bindings
  set env [MkEnv $env]
  /define [S varlist] [list {*}[
    dict keys $vars]] $env
  /define [S body] $body $env
  /define [S vallist] [list {*}[
    dict values $vars]] $env
  set qq "`((lambda ,varlist ,@body)
             ,@vallist)"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
proc ::constcl::parse-bindings {name bindings} {
  upvar $name vars
  foreach binding [splitlist $bindings] {
    set var [car $binding]
    set val [cadr $binding]
    ::if {$var in [dict keys $vars]} {
        ::error "'[$var name]' occurs more than once"
    }
    dict set vars $var $val
  }
  return
}
reg special letrec

proc ::constcl::special-letrec {expr env} {
  set expr [rewrite-letrec $expr $env]
  eval $expr $env
}
proc ::constcl::rewrite-letrec {expr env} {
  set bindings [cadr $expr]
  set body [cddr $expr]
  set vars [dict create]
  parse-bindings vars $bindings
  foreach {key val} $vars {
    dict set outer $key [list [S quote] ${::#UND}]
    dict set inner [set g [gensym "g"]] $val
    dict set assigns $key $g
  }
  set env [MkEnv $env]
  # outer vars
  /define [S ovars] [
    list {*}[dict keys $outer]] $env
  # outer vals
  /define [S ovals] [
    list {*}[dict values $outer]] $env
  # inner vars
  /define [S ivars] [
    list {*}[dict keys $inner]] $env
  # inner vals
  /define [S ivals] [
    list {*}[dict values $inner]] $env
  /define [S assigns] [list {*}[lmap {k v} $assigns {
      list [S set!] $k $v
    }]] $env
  /define [S body] $body $env
  set qq "`((lambda ,ovars
             ((lambda ,ivars ,@assigns) ,@ivals)
             ,@body) ,@ovals)"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
reg special let*

proc ::constcl::special-let* {expr env} {
  set expr [rewrite-let* [cadr $expr] [cddr $expr] $env]
  eval $expr $env
}
proc ::constcl::rewrite-let* {bindings body env} {
  set env [MkEnv $env]
  assert "$bindings are non-NIL" [$bindings ne ${::#NIL}] {
    /define [S body] $body $env
    set qq "`(begin ,@body)"
    set expr [expand-quasiquote [parse $qq] $env]
  } else {
    /define [S var] [caar $bindings] $env
    /define [S val] [cadar $bindings] $env
    /define [S rest] [rewrite-let* [cdr $bindings] \
      $body $env] $env
    set qq "`((lambda (,var)
               ,rest) ,val)"
    set expr [expand-quasiquote [parse $qq] $env]
  }
  $env destroy
  return $expr
}
reg eval

proc ::constcl::eval \
  {expr {env ::constcl::global_env}} {
  ::if {[T [symbol? $expr]]} {
    lookup $expr $env
  } elseif {[T [self-evaluating? $expr]]} {
    set expr
  } elseif {[T [pair? $expr]]} {
    eval-form $expr $env
  } else {
    ::error "unknown expression type [$expr tstr]"
  }
}
proc ::constcl::eval-form {expr env} {
  set op [car $expr]
  set args [cdr $expr]
  ::if {[T [symbol? $op]]} {
    lassign [binding-info $op $env] btype hinfo
    switch $btype {
      UNBOUND {
        error "unbound symbol" $op
      }
      SPECIAL {
        $hinfo $expr $env
      }
      VARIABLE {
        invoke $hinfo [eval-list $args $env]
      }
      SYNTAX {
        eval [$hinfo $expr $env] $env
      }
      default {
        error "unrecognized binding type" $btype
      }
    }
  } else {
    invoke [eval $op $env] [eval-list $args $env]
  }
}
proc ::constcl::binding-info {op env} {
  set actual_env [$env find $op]
  ::if {$actual_env eq "::constcl::null_env"} {
    return [::list UNBOUND {}]
  } else {
    return [$actual_env get $op]
  }
}
proc ::constcl::eval-list {exps env} {
  ::if {[T [pair? $exps]]} {
    return [cons [eval [car $exps] $env] \
      [eval-list [cdr $exps] $env]]
  } else {
    return ${::#NIL}
  }
}
reg macro and

proc ::constcl::expand-and {expr env} {
  set tail [cdr $expr]
  ::if {[[length $tail] numval] == 0} {
    list [S begin] ${::#t}
  } elseif {[[length $tail] numval] == 1} {
    cons [S begin] $tail
  } else {
    do-and $tail ${::#t} $env
  }
}
proc ::constcl::do-and {tail prev env} {
  ::if {[T [null? $tail]]} {
    return $prev
  } else {
    set env [MkEnv $env]
    /define [S first] [car $tail] $env
    /define [S rest] [do-and [cdr $tail] \
        [car $tail] $env] $env
    set qq "`(if ,first ,rest #f)"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
reg macro del!

proc ::constcl::expand-del! {expr env} {
  set tail [cdr $expr]
  set env [MkEnv $env]
  ::if {[T [null? $tail]]} {
    ::error "too few arguments, 0 of 2"
  }
  /define [S listname] [car $tail] $env
  ::if {[T [null? [cdr $tail]]]} {
    ::error "too few arguments, 1 of 2"
  }
  /define [S key] [cadr $tail] $env
  set qq "`(set! ,listname
             (delete! ,listname ,key))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
reg macro for

proc ::constcl::expand-for {expr env} {
  set res [do-for [cdr $expr] $env]
  lappend res [parse "'()"]
  return [list [S begin] {*}$res]
}
proc ::constcl::for-seq {seq env} {
  ::if {[T [number? $seq]]} {
    set seq [in-range $seq]
  } else {
    set seq [eval $seq $env]
  }
  ::if {[T [list? $seq]]} {
    set seq [splitlist $seq]
  } elseif {[T [string? $seq]]} {
    set seq [lmap c [split [$seq value] {}] {
      switch $c {
        " "  { MkChar #\\space }
        "\n" { MkChar #\\newline }
        default {
          MkChar #\\$c
        }
      }
    }]
  } elseif {[T [vector? $seq]]} {
    set seq [$seq value]
  } else {
    ::error "unknown sequence type [$seq tstr]"
  }
  return $seq
}
proc ::constcl::do-for {tail env} {
  # make clauses a Tcl list
  set clauses [splitlist [car $tail]]
  set body [cdr $tail]
  set data [dict create]
  set length 0
  foreach clause $clauses {
    set id [car $clause]
    set sequence [for-seq [cadr $clause] $env]
    set length [llength $sequence]
    # save every id and step of the iteration
    for {set i 0} {$i < $length} {incr i} {
        dict set data $id $i [lindex $sequence $i]
    }
  }
  set res {}
  # for every step of the iteration...
  for {set i 0} {$i < $length} {incr i} {
    set decl {}
    # retrieve the ids
    foreach id [dict keys $data] {
      # list the id and the step
      lappend decl [
        list $id [dict get $data $id $i]]
    }
    # add to the structure of let constructs
    lappend res [list [S let] [
        list {*}$decl] {*}[splitlist $body]]
  }
  return $res
}
reg macro for/and

proc ::constcl::expand-for/and {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S and] {*}$res]
}
reg macro for/list

proc ::constcl::expand-for/list {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S list] {*}$res]
}
reg macro for/or

proc ::constcl::expand-for/or {expr env} {
  set tail [cdr $expr]
  set res [do-for $tail $env]
  return [list [S or] {*}$res]
}
reg macro or

proc ::constcl::expand-or {expr env} {
  set tail [cdr $expr]
  ::if {[[length $tail] numval] == 0} {
    return [list [S begin] ${::#f}]
  } elseif {[[length $tail] numval] == 1} {
    return [cons [S begin] $tail]
  } else {
    return [do-or $tail $env]
  }
}
proc ::constcl::do-or {tail env} {
  ::if {[T [null? $tail]]} {
    return ${::#f}
  } else {
    set env [MkEnv $env]
    /define [S first] [car $tail] $env
    /define [S rest] [do-or [cdr $tail] $env] $env
    set qq "`(let ((x ,first)) (if x x ,rest))"
    set expr [expand-quasiquote [parse $qq] $env]
    $env destroy
    return $expr
  }
}
reg macro pop!

proc ::constcl::expand-pop! {expr env} {
  set tail [cdr $expr]
  set env [MkEnv $env]
  ::if {[T [null? $tail]]} {
      ::error "too few arguments:\n(pop! listname)"
  }
  ::if {[symbol? [car $tail]] eq ${::#f}} {
      ::error "SYMBOL expected:\n(pop! listname)"
  }
  /define [S listname] [car $tail] $env
  set qq "`(set! ,listname (cdr ,listname))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
reg macro push!

proc ::constcl::expand-push! {expr env} {
  set tail [cdr $expr]
  set env [MkEnv $env]
  ::if {[T [null? $tail]]} {
    ::error \
      "too few arguments:\n(push! obj listname)"
  }
  /define [S obj] [car $tail] $env
  ::if {[T [null? [cdr $tail]]]} {
    ::error \
      "too few arguments:\n(push! obj listname)"
  }
  ::if {[symbol? [cadr $tail]] eq ${::#f}} {
    ::error \
      "SYMBOL expected:\n(push! obj listname)"
  }
  /define [S listname] [cadr $tail] $env
  set qq "`(set!
             ,listname
             (cons ,obj ,listname))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
reg macro put!

proc ::constcl::expand-put! {expr env} {
  set tail [cdr $expr]
  set env [::constcl::MkEnv $env]
  ::if {[T [null? $tail]]} {
      ::error "too few arguments, 0 of 3"
  }
  /define [S name] [car $tail] $env
  ::if {[T [null? [cdr $tail]]]} {
      ::error "too few arguments, 1 of 3"
  }
  /define [S key] [cadr $tail] $env
  ::if {[T [null? [cddr $tail]]]} {
      ::error "too few arguments, 2 of 3"
  }
  /define [S val] [caddr $tail] $env
  set qq "`(let ((idx (list-find-key ,name ,key)))
             (if (< idx 0)
               (set!
                 ,name
                 (append (list ,key ,val) ,name))
               (begin
                 (list-set! ,name (+ idx 1) ,val)
                 ,name)))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
reg macro quasiquote

proc ::constcl::expand-quasiquote {expr env} {
  set tail [cdr $expr]
  set qqlevel 0
  ::if {[T [list? [car $tail]]]} {
    set node [car $tail]
    return [qq-visit-child $node 0 $env]
  } elseif {[T [vector? [car $tail]]]} {
    set vect [car $tail]
    set res {}
    for {set i 0} {$i < [
        [vector-length $vect] numval]} {incr i} {
      set idx [MkNumber $i]
      set vecref [vector-ref $vect $idx]
      ::if {[T [pair? $vecref]] &&
          [T [eq? [car $vecref] [
            S unquote]]]} {
        ::if {$qqlevel == 0} {
          lappend res [eval [cadr $vecref] $env]
        }
      } elseif {[T [pair? $vecref]] &&
          [T [eq? [car $vecref] [
            S unquote-splicing]]]} {
        ::if {$qqlevel == 0} {
          lappend res {*}[splitlist [
            eval [cadr $vecref] $env]]
        }
      } elseif {[T [atom? $vecref]]} {
        lappend res $vecref
      } else {
      }
    }
    return [list [S "vector"] {*}$res]
  }
}
proc ::constcl::qq-visit-child {node qqlevel env} {
  ::if {$qqlevel < 0} {
    set qqlevel 0
  }
  ::if {[T [list? $node]]} {
    set res {}
    foreach child [splitlist $node] {
      ::if {[T [pair? $child]] &&
          [T [eq? [car $child] [S unquote]]]} {
        ::if {$qqlevel == 0} {
          lappend res [eval [cadr $child] $env]
        } else {
          lappend res [list [S unquote] [
            qq-visit-child [cadr $child] [
            expr {$qqlevel - 1}] $env]]
        }
      } elseif {[T [pair? $child]] &&
          [T [eq? [car $child] [
          S unquote-splicing]]]} {
        ::if {$qqlevel == 0} {
          lappend res {*}[splitlist [
            eval [cadr $child] $env]]
        }
      } elseif {[T [pair? $child]] &&
          [T [eq? [car $child] [S quasiquote]]]} {
        lappend res [list [S quasiquote] [car [
          qq-visit-child [cdr $child] [
            expr {$qqlevel + 1}] $env]]]
      } elseif {[T [atom? $child]]} {
        lappend res $child
      } else {
        lappend res [
          qq-visit-child $child $qqlevel $env]
      }
    }
  }
  return [list {*}$res]
}
reg macro unless

proc ::constcl::expand-unless {expr env} {
  set tail [cdr $expr]
  set env [MkEnv $env]
  /define [S tail] $tail $env
  set qq "`(if ,(car tail)
             '()
             (begin ,@(cdr tail)))"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
reg macro when

proc ::constcl::expand-when {expr env} {
  set tail [cdr $expr]
  set env [MkEnv $env]
  /define [S tail] $tail $env
  set qq "`(if ,(car tail)
             (begin ,@(cdr tail))
             '())"
  set expr [expand-quasiquote [parse $qq] $env]
  $env destroy
  return $expr
}
proc ::constcl::resolve-local-defines {expr} {
  set exps [cdr $expr]
  set rest [lassign [
    extract-from-defines $exps VALS] a error]
  ::if {[T $error]} {
    return ${::#NIL}
  }
  set rest [lassign [
    extract-from-defines $exps VARS] v error]
  ::if {[T $error]} {
    return ${::#NIL}
  }
  ::if {$rest eq ${::#NIL}} {
    set rest [cons #UNS ${::#NIL}]
  }
  return [make-lambdas $v $a $rest]
}
proc ::constcl::extract-from-defines {exps part} {
  set a ${::#NIL}
  while {$exps ne ${::#NIL}} {
    ::if {[T [atom? $exps]] ||
        [T [atom? [car $exps]]] ||
        ![T [eq? [caar $exps] [S define]]]} {
      break
    }
    set n [car $exps]
    set k [length $n]
    ::if {![T [list? $n]] ||
        [$k numval] < 3 ||
        ![T [argument-list? [cadr $n]]] ||
        ([T [symbol? [cadr $n]]] &&
        [$k numval] > 3)} {
        return [::list ${::#NIL} ${::#t} ${::#NIL}]
      }
      ::if {[T [pair? [cadr $n]]]} {
        ::if {$part eq "VARS"} {
          set a [cons [caadr $n] $a]
        } else {
          set a [cons ${::#NIL} $a]
          set new [cons [cdadr $n] [cddr $n]]
          set new [cons [S lambda] $new]
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
    return [::list $a ${::#f} $exps]
}
proc ::constcl::argument-list? {val} {
  ::if {$val eq ${::#NIL}} {
    return ${::#t}
  } elseif {[T [symbol? $val]]} {
    return ${::#t}
  } elseif {[T [atom? $val]]} {
    return ${::#f}
  }
  while {[T [pair? $val]]} {
    ::if {[symbol? [car $val]] eq ${::#f}} {
      return ${::#f}
    }
    set val [cdr $val]
  }
  ::if {$val eq ${::#NIL}} {
    return ${::#t}
  } elseif {[T [symbol? $val]]} {
    return ${::#t}
  }
}
proc ::constcl::make-lambdas {vars args body} {
  set tmps [make-temporaries $vars]
  set body [append-b [
    make-assignments $vars $tmps] $body]
  set body [cons $body ${::#NIL}]
  set n [cons $tmps $body]
  set n [cons [S lambda] $n]
  set n [cons $n $args]
  set n [cons $n ${::#NIL}]
  set n [cons $vars $n]
  set n [cons [S lambda] $n]
  set n [cons $n [make-undefineds $vars]]
  return $n
}
proc ::constcl::make-temporaries {vals} {
  set res ${::#NIL}
  while {$vals ne ${::#NIL}} {
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
    set s $prefix<[incr ::constcl::gensymnum]>
  }
  return [S $s]
}
proc ::constcl::append-b {a b} {
  ::if {$a eq ${::#NIL}} {
    return $b
  }
  set p $a
  while {$p ne ${::#NIL}} {
    ::if {[T [atom? $p]]} {
      ::error "append: improper list"
    }
    set last $p
    set p [cdr $p]
  }
  set-cdr! $last $b
  return $a
}
proc ::constcl::make-assignments {vars tmps} {
  set res ${::#NIL}
  while {$vars ne ${::#NIL}} {
    set asg [cons [car $tmps] ${::#NIL}]
    set asg [cons [car $vars] $asg]
    set asg [cons [S set!] $asg]
    set res [cons $asg $res]
    set vars [cdr $vars]
    set tmps [cdr $tmps]
  }
  return [cons [S begin] $res]
}
proc ::constcl::make-undefineds {vals} {
  set res ${::#NIL}
  while {$vals ne ${::#NIL}} {
    set res [cons [list [S quote] ${::#UND}] $res]
    set vals [cdr $vals]
  }
  return $res
}
reg write

proc ::constcl::write {val args} {
  ::if {$val ne ""} {
    set oldport $::constcl::Output_port
    ::if {[llength $args]} {
      lassign $args port
      set ::constcl::Output_port $port
    }
    $val write $::constcl::Output_port
    $::constcl::Output_port newline
    set ::constcl::Output_port $oldport
  }
  return
}
reg display

proc ::constcl::display {val args} {
  ::if {$val ne ""} {
    set oldport $::constcl::Output_port
    ::if {[llength $args]} {
      lassign $args port
      set ::constcl::Output_port $port
    }
    $val display $::constcl::Output_port
    $::constcl::Output_port flush
    set ::constcl::Output_port $oldport
  }
  return
}
proc ::constcl::write-pair {port pair} {
  # take an object and print the car
  # and the cdr of the stored value
  set a [car $pair]
  set d [cdr $pair]
  # print car
  $a write {} $port
  ::if {[T [pair? $d]]} {
    # cdr is a cons pair
    $port put " "
    write-pair $port $d
  } elseif {[T [null? $d]]} {
    # cdr is nil
    return
  } else {
    # it is an atom
    $port put " . "
    $d write $port
  }
  return
}
proc ::constcl::idcheckinit {init} {
  ::if {[::string is alpha -strict $init] ||
    $init in {! $ % & * / : < = > ? ^ _ ~}} {
    return true
  } else {
    return false
  }
}
proc ::constcl::idchecksubs {subs} {
  foreach c [split $subs {}] {
    ::if {!([::string is alnum -strict $c] ||
      $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
      return false
    }
  }
  return true
}
proc ::constcl::idcheck {sym} {
  ::if {$sym eq {}} {return $sym}
  ::if {(![idcheckinit [::string index $sym 0]] ||
    ![idchecksubs [::string range $sym 1 end]]) &&
    $sym ni {+ - ...}} {
    ::error "Identifier expected ($sym)"
  }
  set sym
}
proc ::constcl::varcheck {sym} {
  ::if {$sym in {
    else => define unquote unquote-splicing
    quote lambda if set! begin cond and or
    case let let* letrec do delay quasiquote
  }} {
    ::error "Variable name is reserved: $sym"
  }
  return $sym
}
oo::class create ::constcl::Environment {
  superclass ::constcl::Base
  variable bindings outer_env
  constructor {syms vals {outer {}}} {
    set bindings [dict create]
    ::if {[T [::constcl::null? $syms]]} {
      ::if {[llength $vals]} {
        ::error "too many arguments"
      }
    } elseif {[T [::constcl::list? $syms]]} {
      set syms [::constcl::splitlist $syms]
      set symsn [llength $syms]
      set valsn [llength $vals]
      ::if {$symsn != $valsn} {
        ::error [
          ::append --> "wrong # of arguments, " \
            "$valsn instead of $symsn"]
      }
      foreach sym $syms val $vals {
        my bind $sym [lindex $val 0] [lindex $val 1]
      }
    } elseif {[T [::constcl::symbol? $syms]]} {
      my bind $syms VARIABLE [
        ::constcl::list {*}[lmap v $vals {
          lindex $v 1
        }]]
    } else {
      while true {
        ::if {[llength $vals] < 1} {
          ::error "too few arguments"
        }
        my bind [::constcl::car $syms] \
          [lindex $vals 0 0] [lindex $vals 0 1]
        set vals [lrange $vals 1 end]
        ::if {[T [
          ::constcl::symbol? [
            ::constcl::cdr $syms]]]} {
          my bind [::constcl::cdr $syms] \
            VARIABLE [
              ::constcl::list {*}[lmap v $vals {
                lindex $v 1
              }]]
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
    ::constcl::check {::constcl::symbol? $sym} {
      "SYMBOL expected\nEnvironment find"
    }
    ::if {[dict exists $bindings $sym]} {
      self
    } else {
      $outer_env find $sym
    }
  }
  method get {sym} {
    ::constcl::check {::constcl::symbol? $sym} {
      "SYMBOL expected\nEnvironment get"
    }
    dict get $bindings $sym
  }
  method unbind {sym} {
    ::constcl::check {::constcl::symbol? $sym} {
      "SYMBOL expected\nEnvironment unbind"
    }
    dict unset bindings $sym
    return
  }
  method bind {sym type info} {
    ::constcl::check {::constcl::symbol? $sym} {
      "SYMBOL expected\nEnvironment bind"
    }
    ::if {[dict exists $bindings $sym]} {
      set bi [my get $sym]
      lassign $bi bt in
      ::if {$bt in {SPECIAL VARIABLE SYNTAX}} {
        ::error "[$sym name] is already bound"
      }
    }
    dict set bindings $sym [list $type $info]
    return
  }
  method assign {sym type info} {
    ::constcl::check {::constcl::symbol? $sym} {
      "SYMBOL expected\nEnvironment assign"
    }
    ::if {![dict exists $bindings $sym]} {
      ::error "[$sym name] is not bound"
    }
    set bi [my get $sym]
    lassign $bi bt in
    ::if {$bt ne "VARIABLE"} {
      ::error "[$sym name] is not assignable"
    }
    dict set bindings $sym [list $type $info]
    return
  }
  method parent {} {
    set outer_env
  }
  method names {} {
    dict keys $bindings
  }
  method values {} {
    dict values $bindings
  }
  method tstr {} {
    regexp {(\d+)} [self] -> num
    return "#<env-$num>"
  }
}
proc ::constcl::MkEnv {args} {
  ::if {[llength $args] == 1} {
    set parms ${::#NIL}
    set vals {}
    lassign $args env
  } elseif {[llength $args] == 3} {
    lassign $args parms vals env
  } else {
    ::error "wrong number of arguments"
  }
  Environment new $parms $vals $env
}
reg environment?

proc ::constcl::environment? {val} {
  typeof? $val Environment
}
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
  set cur_env [::constcl::MkEnv ::constcl::global_env]
  set str [::constcl::input $prompt]
  while {$str ne ""} {
    set expr [parse $str]
    set val [::constcl::eval $expr $cur_env]
    ::constcl::write $val
    set str [::constcl::input $prompt]
  }
  $cur_env destroy
}
reg eq?

proc ::constcl::eq? {expr1 expr2} {
  ::if {[teq boolean? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } elseif {[teq symbol? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } elseif {[teq number? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return ${::#t}
  } elseif {[teq char? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } elseif {[teq null? $expr1 $expr2]} {
    return ${::#t}
  } elseif {[teq pair? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } elseif {[teq string? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } elseif {[teq vector? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } elseif {[teq procedure? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
proc ::constcl::teq {typep expr1 expr2} {
    return [expr {[T [$typep $expr1]] &&
      [T [$typep $expr2]]}]
}
proc ::constcl::veq {expr1 expr2} {
    return [expr {[$expr1 value] eq [$expr2 value]}]
}
reg eqv?

proc ::constcl::eqv? {expr1 expr2} {
  ::if {[teq boolean? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } elseif {[teq symbol? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return ${::#t}
  } elseif {[teq number? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return ${::#t}
  } elseif {[teq char? $expr1 $expr2] &&
      [veq $expr1 eq $expr2]} {
    return ${::#t}
  } elseif {[teq null? $expr1 $expr2]} {
    return ${::#t}
  } elseif {[T [pair? $expr1]] &&
      [T [pair? $expr2]] &&
      [$expr1 car] eq [$expr2 car] &&
      [$expr1 cdr] eq [$expr2 cdr]} {
    return ${::#t}
  } elseif {[teq string? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return ${::#t}
  } elseif {[teq vector? $expr1 $expr2] &&
      [veq $expr1 $expr2]} {
    return ${::#t}
  } elseif {[teq procedure? $expr1 $expr2] &&
      $expr1 eq $expr2} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg equal?

proc ::constcl::equal? {expr1 expr2} {
  ::if {[$expr1 tstr] eq [$expr2 tstr]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
oo::class create ::constcl::Number {
  superclass ::constcl::Base
  variable value
  constructor {val} {
    ::if {[::string is double -strict $val]} {
      set value $val
    } else {
      ::error "NUMBER expected\n$val"
    }
  }
  method zero? {} {
    ::if {$value == 0} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method positive? {} {
    ::if {$value > 0} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method negative? {} {
    ::if {$value < 0} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method even? {} {
    ::if {$value % 2 == 0} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method odd? {} {
    ::if {$value % 2 == 1} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method value {} {
    set value
  }
  method numval {} {
    set value
  }
  method constant {} {
    return 1
  }
  method tstr {} {
    return $value
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
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(= [
      [lindex $args 0] tstr] ...)"
  }
  ::if {[::tcl::mathop::== {*}$nums]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg <

proc ::constcl::< {args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(< num ...)"
  }
  ::if {[::tcl::mathop::< {*}$nums]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg >

proc ::constcl::> {args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(> num ...)"
  }
  ::if {[::tcl::mathop::> {*}$nums]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg <=

proc ::constcl::<= {args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(<= num ...)"
  }
  ::if {[::tcl::mathop::<= {*}$nums]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg >=

proc ::constcl::>= {args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(>= num ...)"
  }
  ::if {[::tcl::mathop::>= {*}$nums]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg zero?

proc ::constcl::zero? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  return [$num zero?]
}
reg positive?

proc ::constcl::positive? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  return [$num positive?]
}
reg negative?

proc ::constcl::negative? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  return [$num negative?]
}
reg even?

proc ::constcl::even? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  return [$num even?]
}
reg odd?

proc ::constcl::odd? {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  return [$num odd?]
}
reg max

proc ::constcl::max {num args} {
  lappend args $num
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(max num...)"
  }
  N [::tcl::mathfunc::max {*}$nums]
}
reg min

proc ::constcl::min {num args} {
  lappend args $num
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(min num...)"
  }
  N [::tcl::mathfunc::min {*}$nums]
}
reg +

proc ::constcl::+ {args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(+ num ...)"
  }
  N [::tcl::mathop::+ {*}$nums]
}
reg *

proc ::constcl::* {args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(* num ...)"
  }
  N [::tcl::mathop::* {*}$nums]
}
reg -

proc ::constcl::- {num args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(- num ...)"
  }
  N [::tcl::mathop::- [$num numval] {*}$nums]
}
reg /

proc ::constcl::/ {num args} {
  try {
    set nums [lmap arg $args {$arg numval}]
  } on error {} {
    ::error "NUMBER expected\n(/ num ...)"
  }
  N [::tcl::mathop::/ [$num numval] {*}$nums]
}
reg abs

proc ::constcl::abs {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  ::if {[T [$num negative?]]} {
    return [N [expr {[$num numval] * -1}]]
  } else {
    return $num
  }
}
reg quotient

proc ::constcl::quotient {num1 num2} {
  set q [::tcl::mathop::/ [$num1 numval] \
    [$num2 numval]]
  ::if {$q > 0} {
    return [N [::tcl::mathfunc::floor $q]]
  } elseif {$q < 0} {
    return [N [::tcl::mathfunc::ceil $q]]
  } else {
    return [N 0]
  }
}
reg remainder

proc ::constcl::remainder {num1 num2} {
  set n [::tcl::mathop::% [[abs $num1] numval] \
    [[abs $num2] numval]]
  ::if {[T [$num1 negative?]]} {
    set n -$n
  }
  return [N $n]
}
reg modulo

proc ::constcl::modulo {num1 num2} {
  return [N [::tcl::mathop::% [$num1 numval] \
    [$num2 numval]]]
}
reg floor

proc ::constcl::floor {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::floor [$num numval]]
}
reg ceiling

proc ::constcl::ceiling {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::ceil [$num numval]]
}
reg truncate

proc ::constcl::truncate {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  ::if {[T [$num negative?]]} {
    N [::tcl::mathfunc::ceil [$num numval]]
  } else {
    N [::tcl::mathfunc::floor [$num numval]]
  }
}
reg round

proc ::constcl::round {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::round [$num numval]]
}
reg exp

proc ::constcl::exp {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::exp [$num numval]]
}
reg log

proc ::constcl::log {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::log [$num numval]]
}
reg sin

proc ::constcl::sin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::sin [$num numval]]
}
reg cos

proc ::constcl::cos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::cos [$num numval]]
}
reg tan

proc ::constcl::tan {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::tan [$num numval]]
}
reg asin

proc ::constcl::asin {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::asin [$num numval]]
}
reg acos

proc ::constcl::acos {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::acos [$num numval]]
}
reg atan

proc ::constcl::atan {args} {
  ::if {[llength $args] == 1} {
    set num [lindex $args 0]
    check {number? $num} {
        NUMBER expected\n([pn] [$num tstr])
    }
    N [::tcl::mathfunc::atan [$num numval]]
  } else {
    lassign $args num1 num2
    check {number? $num1} {
        NUMBER expected\n([pn] [$num1 tstr])
    }
    check {number? $num2} {
        NUMBER expected\n([pn] [$num2 tstr])
    }
    N [::tcl::mathfunc::atan2 \
      [$num1 numval] [$num2 numval]]
  }
}
reg sqrt

proc ::constcl::sqrt {num} {
  check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
  }
  N [::tcl::mathfunc::sqrt [$num numval]]
}
reg expt

proc ::constcl::expt {x y} {
  check {number? $x} {
      NUMBER expected\n([pn] [$x tstr] \
        [$y tstr])
  }
  check {number? $y} {
      NUMBER expected\n([pn] [$x tstr] \
        [$y tstr])
  }
  N [::tcl::mathfunc::pow [$x numval] \
    [$y numval]]
}
reg number->string

proc ::constcl::number->string {num args} {
  ::if {[llength $args] == 0} {
    check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
    }
    return [MkString [$num numval]]
  } else {
    lassign $args radix
    check {number? $num} {
      NUMBER expected\n([pn] [$num tstr])
    }
    check {number? $radix} {
      NUMBER expected\n([pn] [$num tstr] \
        [$radix tstr])
    }
    set radices [list [N 2] [N 8] [N 10] [N 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] \
        [$num tstr] [$radix tstr])
    }
    ::if {[$radix numval] == 10} {
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
  ::if $negative {set res -$res}
  set res
}
reg string->number

proc ::constcl::string->number {str args} {
  ::if {[llength $args] == 0} {
    check {string? $str} {
      STRING expected\n([pn] [$str tstr])
    }
    return [N [$str value]]
  } else {
    lassign $args radix
    check {string? $str} {
      STRING expected\n([pn] [$str tstr])
    }
    set radices [list [N 2] [N 8] [N 10] [N 16]]
    check {memv $radix $radices} {
      Radix not in 2, 8, 10, 16\n([pn] [$str tstr] \
        [$radix tstr])
    }
    ::if {[$radix numval] == 10} {
      return [N [$str value]]
    } else {
      return [N [
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
    ::if {$dv < 0 || $dv >= $base} {
      ::error "bad digit $dv for base $base"
    }
    set res [expr {$res * $base + $dv}]
  }
  ::if $negative {set res -$res}
  set res
}
$singletonClass create ::constcl::True {
  superclass ::constcl::Base
  method tstr {} {
    return "${::#t}"
  }
}
$singletonClass create ::constcl::False {
  superclass ::constcl::Base
  method tstr {} {
    return "${::#f}"
  }
}
proc ::constcl::MkBoolean {bool} {
  switch $bool {
    "#t" - ${::#t} { return ${::#t} }
    "#f" - ${::#f} { return ${::#f} }
    default { ::error "invalid boolean ($bool)" }
  }
}
reg boolean?

proc ::constcl::boolean? {val} {
  ::if {$val eq ${::#t} || $val eq ${::#f}} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg not

proc ::constcl::not {val} {
  ::if {$val eq ${::#f}} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
oo::class create ::constcl::Char {
  superclass ::constcl::Base
  variable value
  constructor {val} {
    switch -regexp $val {
      {(?i)#\\space} {
        set val " "
      }
      {(?i)#\\newline} {
        set val "\n"
      }
      {#\\[[:graph:]]} {
        set val [::string index $val 2]
      }
      default {
        ::error "CHAR expected\n$val"
      }
    }
    set value $val
  }
  method char {} {
    set value
  }
  method alphabetic? {} {
    ::if {[::string is alpha $value]} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method numeric? {} {
    ::if {[::string is digit $value]} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method whitespace? {} {
    ::if {[::string is space $value]} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method upper-case? {} {
    ::if {[::string is upper $value]} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method lower-case? {} {
    ::if {[::string is lower $value]} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
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
  method display {port} {
    $port put [my char]
  }
  method tstr {} {
    return [my external]
  }
}
proc ::constcl::MkChar {char} {
  ::if {[regexp -nocase {space|newline} $char]} {
      set char [::string tolower $char]
  }
  foreach instance [
    info class instances Char] {
    ::if {[$instance external] eq $char} {
      return $instance
    }
  }
  return [::constcl::Char new $char]
}
reg char?

proc ::constcl::char? {val} {
  return [typeof? $val Char]
}
reg char=?

proc ::constcl::char=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[$char1 char] eq [$char2 char]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char<?

proc ::constcl::char<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[$char1 char] < [$char2 char]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char>?

proc ::constcl::char>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[$char1 char] > [$char2 char]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char<=?

proc ::constcl::char<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[$char1 char] <= [$char2 char]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char>=?

proc ::constcl::char>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[$char1 char] >= [$char2 char]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[::string tolower [$char1 char]] eq
      [::string tolower [$char2 char]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char-ci<?

proc ::constcl::char-ci<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[::string tolower [$char1 char]] <
      [::string tolower [$char2 char]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char-ci>?

proc ::constcl::char-ci>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[::string tolower [$char1 char]] >
      [::string tolower [$char2 char]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char-ci<=?

proc ::constcl::char-ci<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[::string tolower [$char1 char]] <=
      [::string tolower [$char2 char]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char-ci>=?

proc ::constcl::char-ci>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 tstr] [$char2 tstr])
  }
  ::if {[::string tolower [$char1 char]] >=
      [::string tolower [$char2 char]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char tstr])
  }
  return [$char alphabetic?]
}
reg char-numeric?

proc ::constcl::char-numeric? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char tstr])
  }
  return [$char numeric?]
}
reg char-whitespace?

proc ::constcl::char-whitespace? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char tstr])
  }
  return [$char whitespace?]
}
reg char-upper-case?

proc ::constcl::char-upper-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char tstr])
  }
  return [$char upper-case?]
}
reg char-lower-case?

proc ::constcl::char-lower-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char tstr])
  }
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
reg char-upcase

proc ::constcl::char-upcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char tstr])
  }
  ::if {[$char char] in [::list " " "\n"]} {
    return $char
  } else {
    return [MkChar [
      ::string toupper [$char external]]]
  }
}
reg char-downcase

proc ::constcl::char-downcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char tstr])
  }
  ::if {[$char char] in [::list " " "\n"]} {
    return $char
  } else {
    return [MkChar [
      ::string tolower [$char external]]]
  }
}
catch { ::constcl::Procedure destroy }

oo::class create ::constcl::Procedure {
  superclass ::constcl::Base
  variable parms body env
  constructor {p b e} {
    set parms $p
    set body $b
    set env $e
  }
}
oo::define ::constcl::Procedure method call {args} {
  set vals [lmap a $args {::list VARIABLE $a}]
  ::constcl::eval $body [
    ::constcl::MkEnv $parms $vals $env]
}
oo::define ::constcl::Procedure method value {} {}
oo::define ::constcl::Procedure method tstr {} {
  regexp {(\d+)} [self] -> num
  return "#<proc-$num>"
}
interp alias {} ::constcl::MkProcedure \
  {} ::constcl::Procedure new
reg procedure?

proc ::constcl::procedure? {val} {
  ::if {[typeof? $val Procedure] eq ${::#t}} {
    return ${::#t}
  } elseif {[::string match "::constcl::*" $val]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg apply

proc ::constcl::apply {pr vals} {
  check {procedure? $pr} {
    PROCEDURE expected\n([pn] [$pr tstr] ...)
  }
  invoke $pr $vals
}
reg map

proc ::constcl::map {pr args} {
  check {procedure? $pr} {
    PROCEDURE expected\n([pn] [$pr tstr] ...)
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
    set actuals {}
    for {set arglist 0} \
      {$arglist < [llength $arglists]} \
      {incr arglist} {
      lappend actuals [
        lindex $arglists $arglist $item]
    }
    lappend res [invoke $pr [list {*}$actuals]]
  }
  return [list {*}$res]
}
reg for-each

proc ::constcl::for-each {proc args} {
  check {procedure? $proc} {
    PROCEDURE expected\n([pn] [$proc tstr] ...)
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
    set actuals {}
    for {set arglist 0} \
      {$arglist < [llength $arglists]} \
      {incr arglist} {
      lappend actuals [
        lindex $arglists $arglist $item]
    }
    invoke $proc [list {*}$actuals]
  }
  return ${::#NIL}
}
oo::abstract create ::constcl::Port {
  superclass ::constcl::Base
  variable handle
  constructor {args} {
    if {[llength $args]} {
      lassign $args handle
    } else {
      set handle ${::#NIL}
    }
  }
  method handle {} {
    set handle
  }
  method close {} {
    close $handle
    set handle ${::#NIL}
    return
  }
}
oo::class create ::constcl::InputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    try {
      set handle [open [$name value] "r"]
    } on error {} {
      set handle ${::#NIL}
    }
    return $handle
  }
  method get {} {
    chan read $handle 1
  }
  method eof {} {
    chan eof $handle
  }
  method copy {} {
    ::constcl::InputPort new $handle
  }
  method tstr {} {
    regexp {(\d+)} [self] -> num
    return "#<input-port-$num>"
  }
}
interp alias {} ::constcl::MkInputPort \
  {} ::constcl::InputPort new
oo::class create ::constcl::StringInputPort {
  superclass ::constcl::Port
  variable buffer read_eof
  constructor {str} {
    set buffer $str
    set read_eof 0
  }
  method open {name} {}
  method close {} {}
  method get {} {
    ::if {[::string length $buffer] == 0} {
      set read_eof 1
      return #EOF
    }
    set c [::string index $buffer 0]
    set buffer [::string range $buffer 1 end]
    return $c
  }
  method eof {} {
    return $read_eof
  }
  method copy {} {
    ::constcl::StringInputPort new $buffer
  }
  method tstr {} {
    regexp {(\d+)} [self] -> num
    return "#<string-input-port-$num>"
  }
}
interp alias {} ::constcl::MkStringInputPort \
  {} ::constcl::StringInputPort new
oo::class create ::constcl::OutputPort {
  superclass ::constcl::Port
  variable handle
  method open {name} {
    ::error "remove this line to use"
    try {
      set handle [open [$name value] "w"]
    } on error {} {
      set handle ${::#NIL}
    }
    return $handle
  }
  method put {str} {
    puts -nonewline $handle $str
  }
  method newline {} {
    puts $handle {}
  }
  method flush {} {
    flush $handle
  }
  method copy {} {
    ::constcl::OutputPort new $handle
  }
  method tstr {} {
    regexp {(\d+)} [self] -> num
    return "#<output-port-$num>"
  }
}
interp alias {} ::constcl::MkOutputPort \
  {} ::constcl::OutputPort new
oo::class create ::constcl::StringOutputPort {
  superclass ::constcl::Port
  variable buffer
  constructor {args} {
    if {[llength $args]} {
      lassign $args str
      set buffer $str
    } else {
      set buffer {}
    }
  }
  method open {name} {}
  method close {} {}
  method put {str} {
    append buffer $str
    return
  }
  method newline {} {
    append buffer \n
  }
  method flush {} {}
  method tostring {} {
    MkString $buffer
  }
  method copy {} {
    ::constcl::StringOutputPort new $buffer
  }
  method tstr {} {
    regexp {(\d+)} [self] -> num
    return "#<string-output-port-$num>"
  }
}
interp alias {} ::constcl::MkStringOutputPort \
  {} ::constcl::StringOutputPort new

set ::constcl::Input_port [
  ::constcl::MkInputPort stdin]
set ::constcl::Output_port [
  ::constcl::MkOutputPort stdout]
reg port?

proc ::constcl::port? {val} {
  typeof? $val Port
}
reg call-with-input-file

proc ::constcl::call-with-input-file {filename proc} {
  set port [open-input-file $filename]
  set res [invoke $proc [list $port]]
  close-input-port $port
  $port destroy
  return $res
}
reg call-with-output-file

proc ::constcl::call-with-output-file {filename proc} {
  set port [open-output-file $filename]
  set res [invoke $proc [list $port]]
  close-output-port $port
  $port destroy
  return $res
}
reg input-port?

proc ::constcl::input-port? {val} {
  ::if {[T typeof? $val InputPort]} {
    return ${::#t}
  } elseif {[T typeof? $val StringInputPort]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg output-port?

proc ::constcl::output-port? {val} {
  ::if {[T typeof? $val OutputPort]} {
    return ${::#t}
  } elseif {[T typeof? $val StringOutputPort]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg current-input-port

proc ::constcl::current-input-port {} {
  return [$::constcl::Input_port copy]
}
reg current-output-port

proc ::constcl::current-output-port {} {
  return [$::constcl::Output_port copy]
}
reg with-input-from-file

proc ::constcl::with-input-from-file {filename thunk} {
  set newport [open-input-file $filename]
  ::if {[$newport handle] ne ${::#NIL}} {
    set oldport $::constcl::Input_port
    set ::constcl::Input_port $newport
    $thunk call
    set ::constcl::Input_port $oldport
    close-input-port $newport
  }
  $newport destroy
}
reg with-output-to-file

proc ::constcl::with-output-to-file {filename thunk} {
  set newport [open-output-file $filename]
  ::if {[$newport handle] ne ${::#NIL}} {
    set oldport $::constcl::Output_port
    set ::constcl::Output_port $newport
    $thunk call
    set ::constcl::Output_port $oldport
    close-input-port $newport
  }
  $newport destroy
}
reg open-input-file

proc cnof {} {return "could not open file"}
proc fae {} {return "file already exists"}

proc ::constcl::open-input-file {filename} {
  set p [MkInputPort]
  $p open $filename
  ::if {[$p handle] eq ${::#NIL}} {
    set fn [$filename value]
    ::error "open-input-file: [cnof] $fn"
  }
  return $p
}
reg open-output-file

proc ::constcl::open-output-file {filename} {
  ::if {[file exists $filename]} {
    ::error "open-output-file: [fae] $filename"
  }
  set p [MkOutputPort]
  $p open $filename
  ::if {[$p handle] eq ${::#NIL}} {
    ::error "open-output-file: [cnof] $filename"
  }
  return $p
}
reg close-input-port

proc ::constcl::close-input-port {port} {
  ::if {[$port handle] eq "stdin"} {
    ::error "don't close the standard input port"
  }
  $port close
}
reg close-output-port

proc ::constcl::close-output-port {port} {
  ::if {[$port handle] eq "stdout"} {
    ::error "don't close the standard output port"
  }
  $port close
}
reg newline

proc ::constcl::newline {args} {
  ::if {[llength $args]} {
    lassign $args port
  } else {
    set port [current-output-port]
  }
  $port newline
}
reg load

proc ::constcl::load {filename} {
  try {
    open-input-file $filename
  } on ok port {
  } on error {} {
    return
  }
  ::if {[$port handle] ne ${::#NIL}} {
    set expr [read $port]
    while {$expr ne "#EOF"} {
      eval $expr
      set expr [read $port]
    }
    close-input-port $port
  }
  $port destroy
}
oo::class create ::constcl::Pair {
  superclass ::constcl::Base
  variable car cdr constant
  constructor {a d} {
    set car $a
    set cdr $d
    set constant 0
  }
  method value {} {
    my tstr
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
    return
  }
  method constant {} {
    return $constant
  }
  method mutable? {} {
    expr {$constant ? ${::#f} : ${::#t}}
  }
  method write {port} {
    $port put "("
    ::constcl::write-pair $port [self]
    $port put ")"
  }
  method tstr {} {
    format "(%s)" [::constcl::tstr-pair [self]]
  }
}
interp alias {} ::constcl::MkPair \
  {} ::constcl::Pair new
reg pair?

proc ::constcl::pair? {val} {
  typeof? $val Pair
}
proc ::constcl::tstr-pair {pair} {
  # take a pair and make a string of the car
  # and the cdr of the stored value
  set str {}
  set a [car $pair]
  set d [cdr $pair]
  ::append str [$a tstr]
  ::if {[T [pair? $d]]} {
    # cdr is a cons pair
    ::append str " "
    ::append str [tstr-pair $d]
  } elseif {[T [null? $d]]} {
    # cdr is nil
    return $str
  } else {
    # it is an atom
    ::append str " . "
    ::append str [$d tstr]
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

    proc ::constcl::c${ads}r {x} "
        foreach c \[lreverse \[split $ads {}\]\] {
            ::if {\$c eq \"a\"} {
                set x \[car \$x\]
            } else {
                set x \[cdr \$x\]
            }
        }
        return \$x
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
  ::if {[T [null? $val]]} {
      return ${::#t}
  } elseif {[T [pair? $val]]} {
      return [listp $val]
  } else {
      return ${::#f}
  }
}
proc ::constcl::listp {pair} {
  upvar visited visited
  ::if {$pair in $visited} {
    return ${::#f}
  }
  lappend visited $pair
  ::if {[T [null? $pair]]} {
    return ${::#t}
  } elseif {[T [pair? $pair]]} {
    return [listp [cdr $pair]]
  } else {
    return ${::#f}
  }
}
reg list

proc ::constcl::list {args} {
  ::if {[llength $args] == 0} {
    return ${::#NIL}
  } else {
    set prev ${::#NIL}
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
  ::if {[T [null? $pair]]} {
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
      LIST expected\n([pn] [$r tstr])
    }
    set prev [copy-list $r $prev]
  }
  set prev
}
proc ::constcl::copy-list {pair next} {
  ::if {[T [null? $pair]]} {
    set next
  } elseif {[T [null? [cdr $pair]]]} {
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
  ::if {[T [zero? $k]]} {
    return $vals
  } else {
    list-tail [cdr $vals] [- $k [N 1]]
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
    LIST expected\n($name [$val1 tstr] [$val2 tstr])
  }
  ::if {[T [null? $val2]]} {
    return ${::#f}
  } elseif {[T [pair? $val2]]} {
    ::if {[T [$epred $val1 [car $val2]]]} {
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
    LIST expected\n($name [$val1 tstr] [$val2 tstr])
  }
  ::if {[T [null? $val2]]} {
    return ${::#f}
  } elseif {[T [pair? $val2]]} {
    ::if {[T [pair? [car $val2]]] &&
      [T [$epred $val1 [caar $val2]]]} {
      return [car $val2]
    } else {
      return [assoc-proc $epred $val1 [cdr $val2]]
    }
  }
}
oo::class create ::constcl::String {
  superclass ::constcl::Base
  variable data constant
  constructor {val} {
    set val [::string map {\\\\ \\ \\\" \" \\n \n} $val]
    set len [::string length $val]
    # allocate vector space for the string's
    # characters
    set vsa [::constcl::vsAlloc $len]
    # store the characters in vector space, as
    # Char objects
    set idx $vsa
    foreach elt [split $val {}] {
      ::if {$elt eq " "} {
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
    # store the basic vector data: address of
    # first character and length
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
    # present the store as a string of printable characters
    join [lmap c [my store] {$c char}] {}
  }
  method set! {k c} {
    ::if {[my constant]} {
      ::error "string is constant"
    } else {
      set k [$k numval]
      ::if {$k < 0 ||
        $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[::constcl::car $data] numval]
      lset ::constcl::vectorSpace $base+$k $c
    }
    return [self]
  }
  method fill! {c} {
    ::if {[my constant]} {
      ::error "string is constant"
    } else {
      set base [[::constcl::car $data] numval]
      set len [[my length] numval]
      for {set idx $base} \
        {$idx < $base+$len} \
        {incr idx} {
        lset ::constcl::vectorSpace $idx $c
      }
    }
    return [self]
  }
  method substring {from to} {
    set f [$from numval]
    ::if {$f < 0 ||
      $f >= [[my length] numval]} {
      ::error "index out of range\n$f"
    }
    set t [$to numval]
    ::if {$t < 0 ||
      $t > [[my length] numval]} {
      ::error "index out of range\n$t"
    }
    ::if {$t < $f} {
      ::error "index out of range\n$t"
    }
    join [lmap c [
      lrange [my store] $f $t-1] {$c char}] {}
  }
  method mkconstant {} {
    set constant 1
    return
  }
  method constant {} {
    return $constant
  }
  method external {} {
    return "\"[
      string map {\\ \\\\ \" \\\" \n \\n} [my value]]\""
  }
  method display {port} {
    $port put [my value]
  }
  method tstr {} {
    return [my external]
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
  set i [$k numval]
  ::if {[llength $args] == 0} {
    set char " "
  } else {
    lassign $args c
    set char [$c char]
  }
  return [MkString [::string repeat $char $i]]
}
reg string

proc ::constcl::string {args} {
  set str {}
  foreach char $args {
    check {::constcl::char? $char} {
      CHAR expected\n([pn] [lmap c $args \
        {$c tstr}])
    }
    ::append str [$char char]
  }
  return [MkString $str]
}
reg string-length

proc ::constcl::string-length {str} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str tstr])
  }
  return [$str length]
}
reg string-ref

proc ::constcl::string-ref {str k} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str tstr] \
      [$k tstr])
  }
  check {::constcl::number? $k} {
    INTEGER expected\n([pn] [$str tstr] \
      [$k tstr])
  }
  return [$str ref $k]
}
reg string-set!

proc ::constcl::string-set! {str k char} {
  check {string? $str} {
    STRING expected\n([pn] [$str tstr] [$k tstr] \
      [$char tstr])
  }
  check {number? $k} {
    INTEGER expected\n([pn] [$str tstr] \
      [$k tstr] [$char tstr])
  }
  check {char? $char} {
    CHAR expected\n([pn] [$str tstr] [$k tstr] \
      [$char tstr])
  }
  $str set! $k $char
  return $str
}
reg string=?

proc ::constcl::string=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[$str1 value] eq [$str2 value]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string-ci=?

proc ::constcl::string-ci=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[::string tolower [$str1 value]] eq
      [::string tolower [$str2 value]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string<?

proc ::constcl::string<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[$str1 value] < [$str2 value]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string-ci<?

proc ::constcl::string-ci<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[::string tolower [$str1 value]] <
      [::string tolower [$str2 value]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string>?

proc ::constcl::string>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[$str1 value] > [$str2 value]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string-ci>?

proc ::constcl::string-ci>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[::string tolower [$str1 value]] >
      [::string tolower [$str2 value]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string<=?

proc ::constcl::string<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[$str1 value] <= [$str2 value]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string-ci<=?

proc ::constcl::string-ci<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[::string tolower [$str1 value]] <=
      [::string tolower [$str2 value]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string>=?

proc ::constcl::string>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[$str1 value] >= [$str2 value]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg string-ci>=?

proc ::constcl::string-ci>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 tstr] \
      [$str2 tstr])
  }
  ::if {[::string tolower [$str1 value]] >=
      [::string tolower [$str2 value]]} {
    return ${::#t}
  } else {
    return ${::#f}
  }
}
reg substring

proc ::constcl::substring {str start end} {
  check {string? $str} {
    STRING expected\n([pn] [$str tstr] \
      [$start tstr] [$end tstr])
  }
  check {number? $start} {
    NUMBER expected\n([pn] [$str tstr] \
      [$start tstr] [$end tstr])
  }
  check {number? $end} {
    NUMBER expected\n([pn] [$str tstr] \
      [$start tstr] [$end tstr])
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
    STRING expected\n([pn] [$str tstr])
  }
  return [MkString [$str value]]
}
reg string-fill!

proc ::constcl::string-fill! {str char} {
  check {string? $str} {
    STRING expected\n([pn] [$str tstr] \
      [$char tstr])
  }
  $str fill! $char
  return $str
}
oo::class create ::constcl::Symbol {
  superclass ::constcl::Base
  variable name caseconstant
  constructor {n} {
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
  method = {sym} {
    ::if {$name eq [$sym name]} {
      return ${::#t}
    } else {
      return ${::#f}
    }
  }
  method constant {} {
    return 1
  }
  method case-constant {} {
    set caseconstant
  }
  method make-case-constant {} {
    set caseconstant 1
  }
  method tstr {} {
    return $name
  }
}
proc ::constcl::MkSymbol {str} {
  ::if {[dict exists $::constcl::symbolTable $str]} {
    return [dict get $::constcl::symbolTable $str]
  } else {
    set sym [::constcl::Symbol new $str]
    dict set ::constcl::symbolTable $str $sym
    return $sym
  }
}
interp alias {} S {} ::constcl::MkSymbol
reg symbol?

proc ::constcl::symbol? {val} {
  typeof? $val Symbol
}
reg symbol->string

proc ::constcl::symbol->string {sym} {
  check {symbol? $sym} {
    SYMBOL expected\n([pn] [$sym tstr])
  }
  ::if {![$sym case-constant]} {
    set str [MkString [
      ::string tolower [$sym name]]]
  } else {
    set str [MkString [$sym name]]
  }
  $str mkconstant
  return $str
}
reg string->symbol

proc ::constcl::string->symbol {str} {
  check {string? $str} {
    STRING expected\n([pn] [$obj tstr])
  }
  set sym [MkSymbol [$str value]]
  $sym make-case-constant
  return $sym
}
oo::class create ::constcl::Vector {
  superclass ::constcl::Base
  variable data constant
  constructor {val} {
    ::if {[T [::constcl::list? $val]]} {
      # ::if val is provided in the form of a Lisp list
      set len [[::constcl::length $val] numval]
      # allocate vector space for the elements
      set vsa [::constcl::vsAlloc $len]
      # store the elements in vector space
      set idx $vsa
      while {![T [::constcl::null? $val]]} {
        set elt [::constcl::car $val]
        lset ::constcl::vectorSpace $idx $elt
        incr idx
        set val [::constcl::cdr $val]
      }
    } else {
      # ::if val is provided in the form of a Tcl list
      set len [llength $val]
      # allocate vector space for the elements
      set vsa [::constcl::vsAlloc $len]
      # store the elements in vector space
      set idx $vsa
      foreach elt $val {
        lset ::constcl::vectorSpace $idx $elt
        incr idx
      }
    }
    # store the basic vector data: address of
    # first element and length
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
    ::if {$k < 0 || $k >= [[my length] numval]} {
      ::error "index out of range\n$k"
    }
    lindex [my store] $k
  }
  method store {} {
    set base [[my baseadr] numval]
    set end [expr {[[my length] numval] + $base - 1}]
    lrange $::constcl::vectorSpace $base $end
  }
  method value {} {
    my store
  }
  method set! {k val} {
    ::if {[my constant]} {
      ::error "vector is constant"
    } else {
      set k [$k numval]
      ::if {$k < 0 || $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[my baseadr] numval]
      lset ::constcl::vectorSpace $k+$base $val
    }
    return [self]
  }
  method fill! {val} {
    ::if {[my constant]} {
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
  method tstr {} {
    return [format "#(%s)" [
      join [lmap val [my value] {$val tstr}]]]
  }
}
interp alias {} ::constcl::MkVector \
  {} ::constcl::Vector new
reg vector?

proc ::constcl::vector? {val} {
  typeof? $val Vector
}
reg make-vector

proc ::constcl::make-vector {k args} {
  ::if {[llength $args] == 0} {
    set val ${::#NIL}
  } else {
    lassign $args val
  }
  MkVector [lrepeat [$k numval] $val]
}
reg vector

proc ::constcl::vector {args} {
  MkVector $args
}
reg vector-length

proc ::constcl::vector-length {vec} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec tstr])
  }
  return [$vec length]
}
reg vector-ref

proc ::constcl::vector-ref {vec k} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec tstr] [$k tstr])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec tstr] [$k tstr])
  }
  return [$vec ref $k]
}
reg vector-set!

proc ::constcl::vector-set! {vec k val} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec tstr] [$k tstr])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec tstr] [$k tstr])
  }
  return [$vec set! $k $val]
}
reg vector->list

proc ::constcl::vector->list {vec} {
  list {*}[$vec value]
}
reg list->vector

proc ::constcl::list->vector {list} {
  vector {*}[splitlist $list]
}
reg vector-fill!

proc ::constcl::vector-fill! {vec fill} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec tstr] \
      [$fill tstr])
  }
  $vec fill! $fill
}
set ::constcl::vectorSpaceSize [expr {1 * 1024}]
set ::constcl::vectorSpace [
  lrepeat $::constcl::vectorSpaceSize [N 0]]

set ::constcl::vectorAssign 0
proc ::constcl::vsAlloc {num} {
  ::if {$::constcl::vectorSpaceSize -
    $::constcl::vectorAssign < $num} {
    ::error "not enough vector space left"
  }
  set va $::constcl::vectorAssign
  incr ::constcl::vectorAssign $num
  return $va
}
unset -nocomplain ::constcl::symbolTable
set ::constcl::symbolTable [dict create]

set ::constcl::gensymnum 0
interp recursionlimit {} 2000

proc singleton {objclass objname} {
    # a fake singleton, but it will do
    set obj [uplevel #0 [::list oo::object create $objname]]
    oo::objdefine $obj class ::constcl::$objclass
    oo::objdefine $obj {
        unexport destroy
    }
    uplevel #0 [::list set $objname $obj]
    return $obj
}

singleton NIL #NIL
singleton True #t
singleton False #f
singleton Unspecified #UNS
singleton Undefined #UND
singleton EndOfFile #EOF

regvar pi [N 3.1415926535897931]
regvar nil ${::#NIL}
::constcl::Environment create \
  ::constcl::null_env ${::#NIL} {}

oo::objdefine ::constcl::null_env {
  method find {sym} {
    self
  }
  method get {sym} {
    ::error "Unbound variable: [$sym name]"
  }
  method set {sym t_ i_} {
    ::error "Unbound variable: [$sym name]"
  }
}
namespace eval ::constcl {
  Environment create global_env ${::#NIL} {} \
    ::constcl::null_env
  foreach v [dict values $defreg] {
    lassign $v key val
    lassign $val bt in
    global_env bind [S $key] $bt $in
  }
}
pe {(load "schemebase.scm")}

