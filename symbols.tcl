
MD(
### Symbols

Symbols are like little strings that are used to refer to things (variables, including
procedure names, etc) or for comparing against each other.

**Symbol** class
MD)

CB
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
CB

PR(
symbol? (public);val val -> bool
PR)

CB
reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {val} {
  typeof? $val Symbol
}
CB

TT(

::tcltest::test symbols-1.0 {try symbol?} -body {
    pew {(symbol? 'foo)}
    pew {(symbol? (car '(a b)))}
    pew {(symbol? "bar")}
    pew {(symbol? 'nil)}
    pew {(symbol? '())}
    pew {(symbol? #f)}
    puts [::constcl::symbol? [S quote]]
} -output "#t\n#t\n#f\n#t\n#f\n#f\n#t\n"

TT)

MD(
**symbol->string**

`symbol->string` yields a string consisting of the symbol name, usually
lower-cased.
MD)

PR(
symbol->string (public);sym sym -> str
PR)

CB
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
CB

MD(
Example:

```
(let ((sym 'Foobar))
  (symbol->string sym))   =>  "foobar"
```
MD)

TT(

::tcltest::test symbols-1.1 {try symbol->string (and string->symbol)} -body {
    pew {(symbol->string 'flying-fish)}
    pew {(symbol->string 'Martin)}
    pew {(symbol->string (string->symbol "Malvina"))}
} -output {"flying-fish"
"martin"
"Malvina"
}

::tcltest::test symbols-1.2 {try symbol->string} -constraints knownBug -body { ;# bug: don't know, hangs tkcon
    pew {(string-set! (symbol->string 'flying-fish) 3 #\A}
} -returnCodes error -result ""

TT)

MD(
**string->symbol**

`string->symbol` creates a symbol with the name given by the string. The symbol
is 'case-constant', i.e. it will not be lower-cased.
MD)

PR(
string->symbol (public);str str -> sym
PR)

MD(
Example:

```
(define sym (let ((str "Foobar"))
              (string->symbol str)))
sym                                    =>  Foobar
(symbol->string sym)                   =>  "Foobar"
```
MD)

CB
reg string->symbol ::constcl::string->symbol

proc ::constcl::string->symbol {str} {
  check {string? $str} {
    STRING expected\n([pn] [$obj show])
  }
  set sym [MkSymbol [$str value]]
  $sym make-case-constant
  return $sym
}
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et  
