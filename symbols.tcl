
MD(
### Symbols

Symbols are like little strings that are used to refer to things (variables, including
procedure names, etc) or for comparing against each other.
MD)

CB
oo::class create Symbol {
    superclass NIL
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
    foreach instance [info class instances Symbol] {
        if {[$instance name] eq $n} {
            return $instance
        }
    }
    return [Symbol new $n]
}
CB

CB
reg symbol? ::constcl::symbol?

proc ::constcl::symbol? {obj} {
    if {[info object isa typeof $obj Symbol]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $obj] Symbol]} {
        return #t
    } else {
        return #f
    }
}
CB

TT(

::tcltest::test symbols-1.0 {try symbol?} -body {
    pep {(symbol? 'foo)}
    pep {(symbol? (car '(a b)))}
    pep {(symbol? "bar")}
    pep {(symbol? 'nil)}
    pep {(symbol? '())}
    pep {(symbol? #f)}
    puts [::constcl::symbol? #Q]
} -output "#t\n#t\n#f\n#t\n#f\n#f\n#t\n"

TT)

MD(
`symbol->string` yields a string consisting of the symbol name, usually
lower-cased.
MD)

CB
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
CB

TT(

::tcltest::test symbols-1.1 {try symbol->string (and string->symbol)} -body {
    pep {(symbol->string 'flying-fish)}
    pep {(symbol->string 'Martin)}
    pep {(symbol->string (string->symbol "Malvina"))}
} -output {"flying-fish"
"martin"
"Malvina"
}

# hangs tkcon
::tcltest::test symbols-1.2 {try symbol->string} -constraints knownBug -body {
    pep {(string-set! (symbol->string 'flying-fish) 3 #\A}
} -returnCodes error -result ""

TT)

MD(
`string->symbol` creates a symbol with the name given by the string. The symbol
is 'case-constant', i.e. it will not be lower-cased.
MD)

CB
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
CB

