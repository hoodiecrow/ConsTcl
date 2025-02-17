
MD(
## Identifier validation

__idcheckinit__

__idchecksubs__

__idcheck__

__varcheck__

Some routines for checking if a string is a valid identifier. `idcheckinit`
checks the first character, `idchecksubs` checks the rest. `idcheck` calls the
others and raises errors if they fail. A valid symbol is still an invalid
identifier if has the name of some keyword, which `varcheck` checks, for a set
of keywords given in the standard.

MD)

CB
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
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
