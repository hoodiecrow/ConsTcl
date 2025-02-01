# ConsTcl
A second try at a Lisp interpreter written in Tcl (the first one was [Thtcl](https://github.com/hoodiecrow/thtcl)),
this time with a real Lisp-like type system. It steps over and back over the border
between Tcl and Lisp a lot of times while working, and as a result is fairly slow.

#### Benchmark

On my cheap computer, the following code takes 0.024 seconds to run.

```
namespace eval ::constcl {
    eval [parse "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))"]
    time {eval [parse "(fact 100)"]} 10
}
```


Speed aside, it is an amusing piece of machinery. The types are implemented as TclOO
classes, and evaluation is to a large extent applying Lisp methods to Tcl data.

It is limited: as of 2025-01-30, it still doesn't handle input (but has an interactive
REPL). Quite a few standard procedures are missing. It doesn't come near to having
call/cc or tail recursion. It doesn't have ports or exact/inexact numbers, or most of
the numerical tower. Error reporting is spotty, and there is no error recovery.


## Initial declarations

First, I need to create the namespace that will be used for most identifiers:

```
namespace eval ::constcl {}
```

Next, some procedures that make my life as developer somewhat easier, but
don't really matter to the interpreter (except the first one, `reg`, which
registers built-in procedures in the definitions register). The other ones
will show up a lot in the test cases.

```
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
    ::constcl::expand-macro ::constcl::global_env
    ::constcl::write [::constcl::cons $op $args]
}
```

This one is a little bit of both, a utility function that is also among the
builtins in the library. It started out as a one-liner by Donal K. Fellows,
but has grown a bit since then to suit my needs.

```
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
```

The `NIL` class has one object: the empty list called `#NIL`. It is also base class for many other
type classes.

```
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
```

The `null?` standard predicate recognizes the empty list. Predicates
in ConsTcl return #t or #f for true or false, so some care is necessary
when calling them from Tcl code.

```
reg null? ::constcl::null?

proc ::constcl::null? {obj} {
    if {$obj eq "#NIL"} {
        return #t
    } else {
        return #f
    }
}
```

The `None` class serves but one purpose: to avoid printing a result after `define`.

```
catch { ::constcl::None destroy}

oo::class create ::constcl::None {}
```

The `Dot` class is a helper class for the parser.

```
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

```


## read

`read` represents the interpreter's input facility. Currently input is faked with input
strings.

A quick-and-dirty input simulator, using an input buffer object to hold characters to be
read. The `fill` method fills the buffer and sets the first character in the peek position.
The `advance` method consumes one character from the buffer. `first` peeks at the next
character to be read. `skip-ws` advances past whitespace and comments.  `unget` backs up
one position and sets a given character in the peek position. The `find` method looks past
whitespace and comments to find a given character. It returns Tcl truth if it is found.
Or it gets the hose again.


```
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
        if {[::string is space -strict $peekc]} {
            for {set cp 0} {$cp < [::string length $buffer]} {incr cp} {
                if {![::string is space -strict [::string index $buffer $cp]]} {
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
```

Given a string, `parse` fills the input buffer. It then reads and parses the input.

<table border=1><thead><tr><th colspan=2 align="left">parse (public)</th></tr></thead><tr><td>str</td><td>Lisp source text</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg parse

proc ::constcl::parse {str} {
    ib fill $str
    return [parse-value]
}
```

The standard builtin `read` consumes and parses input into a Lisp expression.

<table border=1><thead><tr><th colspan=2 align="left">read (public)</th></tr></thead><tr><td>args</td><td>-don't care-</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg read ::constcl::read

proc ::constcl::read {args} {
    return [parse-value]
}
```

The procedure `parse-value` reads a value of any kind.

<table border=1><thead><tr><th colspan=2 align="left">parse-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::parse-value {} {
    ib skip-ws
    switch -regexp [ib first] {
        {^$}          { return }
        {\"}          { return [parse-string-value] }
        {\#}          { return [parse-sharp] }
        {\'}          { return [parse-quoted-value] }
        {\(}          { return [parse-pair-value ")"] }
        {\+} - {\-}   { return [parse-plus-minus] }
        {\,}          { return [parse-unquoted-value] }
        {\.}          { ib advance ; return [Dot new] }
        {\[}          { return [parse-pair-value "\]"] }
        {\`}          { return [parse-quasiquoted-value] }
        {\d}          { return [parse-number-value] }
        {[[:space:]]} { ib advance }
        {[[:graph:]]} { return [parse-identifier-value] }
        default {
            error "unexpected char [ib first]"
        }
    }
}
```

`parse-string` reads a string value and returns a [String](https://github.com/hoodiecrow/ConsTcl#strings) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-string-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

```
proc ::constcl::parse-string-value {} {
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
```


`parse-sharp` reads the various kinds of values whose literal begins with
a sharp sign (#).

<table border=1><thead><tr><th colspan=2 align="left">parse-sharp (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a vector, boolean, or character value</td></tr></table>

```
proc ::constcl::parse-sharp {} {
    ib advance
    switch [ib first] {
        (    { return [parse-vector-value] }
        t    { ib advance ; ib skip-ws ; return #t }
        f    { ib advance ; ib skip-ws ; return #f }
        "\\" { return [parse-character-value] }
        default {
            error "Illegal #-literal"
        }
    }
}
```

The `make-constant` helper procedure is called to set values to
constants when read as a quoted literal.

```
proc ::constcl::make-constant {val} {
    if {[pair? $val] eq "#t"} {
        $val mkconstant
        make-constant [car $val]
        make-constant [cdr $val]
    } elseif {[null? $val] eq "#t"} {
        return #NIL
    } else {
        $val mkconstant
    }
}
```

`parse-quoted-value` reads a value and returns it wrapped in `quote`.

<table border=1><thead><tr><th colspan=2 align="left">parse-quoted-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a value wrapped in the quote symbol</td></tr></table>

```
proc ::constcl::parse-quoted-value {} {
    ib advance
    set val [parse-value]
    ib skip-ws
    make-constant $val
    return [list #Q $val]
}
```


The `parse-pair-value` procedure reads values and returns a structure of
[Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) objects.

<table border=1><thead><tr><th colspan=2 align="left">parse-pair-value (internal)</th></tr></thead><tr><td>char</td><td>the terminating paren or bracket</td></tr><tr><td><i>Returns:</i></td><td>a structure of pair values</td></tr></table>

```

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
```


`parse-plus-minus` reacts to a plus or minus in the input buffer, and either
returns a `#+` or `#-` symbol, or a number.

<table border=1><thead><tr><th colspan=2 align="left">parse-plus-minus (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>either the symbols + or - or a number</td></tr></table>

```
proc ::constcl::parse-plus-minus {} {
    set c [ib first]
    ib advance
    if {[::string is digit -strict [ib first]]} {
        ib unget $c
        return [::constcl::parse-number-value]
    } else {
        if {$c eq "+"} {
            ib skip-ws
            return [MkSymbol "+"]
        } else {
            ib skip-ws
            return [MkSymbol "-"]
        }
    }
}
```

`parse-unquoted-value` reads a value and returns it wrapped in `unquote`, or possibly
in `unquote-splicing`.

<table border=1><thead><tr><th colspan=2 align="left">parse-unquoted-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a value wrapped in the unquote/-splicing symbol</td></tr></table>

```
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
```


`parse-quasiquoted-value` reads a value and returns it wrapped in `quasiquote`.

<table border=1><thead><tr><th colspan=2 align="left">parse-quasiquoted-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a value wrapped in the quasiquote symbol</td></tr></table>

```
proc ::constcl::parse-quasiquoted-value {} {
    ib advance
    set val [parse-value]
    ib skip-ws
    make-constant $val
    return [list [MkSymbol "quasiquote"] $val]
}
```


`parse-number-value` reads a number and returns a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-number-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
proc ::constcl::parse-number-value {} {
    while {[ib first] ne {} && ![::string is space -strict [ib first]] && [ib first] ni {) \]}} {
        ::append num [ib first]
        ib advance
    }
    ib skip-ws
    if {[::string is double -strict $num]} {
        return [MkNumber $num]
    } else {
        error "Invalid numeric constant $num"
    }
}
```


`parse-identifier-value` reads an identifier value and returns a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-identifier-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a symbol</td></tr></table>

```
proc ::constcl::parse-identifier-value {} {
    while {[ib first] ne {} && ![::string is space -strict [ib first]] && [ib first] ni {) \]}} {
        ::append name [ib first]
        ib advance
    }
    ib skip-ws
    # idcheck throws error if invalid identifier
    return [MkSymbol [idcheck $name]]
}
```


The `character-check` helper procedure compares a potential
character constant to the valid kinds. Returns Tcl truth (1/0).

```
proc ::constcl::character-check {name} {
    regexp -nocase {^#\\([[:graph:]]|space|newline)$} $name
}
```

`parse-character-value` reads a character and returns a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-character-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a character</td></tr></table>

```
proc ::constcl::parse-character-value {} {
    set name "#"
    while {[ib first] ne {} && ![::string is space -strict [ib first]] && [ib first] ni {) ]}} {
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
```


`parse-vector-value` reads a vector value and returns a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.

<table border=1><thead><tr><th colspan=2 align="left">parse-vector-value (internal)</th></tr></thead><tr><td><i>Returns:</i></td><td>a vector</td></tr></table>

```
proc ::constcl::parse-vector-value {} {
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
```


## eval

The heart of the Lisp interpreter, `eval` takes a Lisp expression and processes it according to its form.

| Syntactic form | Syntax | Semantics |
|----------------|--------|-----------|
| [variable reference](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.1) | _variable_ | An expression consisting of a identifier is a variable reference. It evaluates to the value the identifier is bound to. An unbound identifier can't be evaluated. Example: `r` ⇒ 10 if _r_ is bound to 10 |
| [constant literal](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.2) | _number_ or _boolean_ | Numerical and boolean constants evaluate to themselves. Example: `99` ⇒ 99 |
| [quotation](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.2) | __quote__ _datum_ | (__quote__ _datum_) evaluates to _datum_, making it a constant. Example: `(quote r)` ⇒ r
| [sequence](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.3) | __begin__ _expression_... | The _expression_ s are evaluated sequentially, and the value of the last <expression> is returned. Example: `(begin (define r 10) (* r r))` ⇒ the square of 10 |
| [conditional](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.5) | __if__ _test_ _conseq_ _alt_ | An __if__ expression is evaluated like this: first, _test_ is evaluated. If it yields a true value, then _conseq_ is evaluated and its value is returned. Otherwise _alt_ is evaluated and its value is returned. Example: `(if (> 99 100) (* 2 2) (+ 2 4))` ⇒ 6 |
| [definition](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-8.html#%_sec_5.2) | __define__ _identifier_ _expression_ | A definition binds the _identifier_ to the value of the _expression_. A definition does not evaluate to anything. Example: `(define r 10)` ⇒ |
| [assignment](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.6) | __set!__ _variable_ _expression_ | _Expression_ is evaluated, and the resulting value is stored in the location to which _variable_ is bound. It is an error to assign to an unbound _identifier_. Example: `(set! r 20)` ⇒ 20 |
| [procedure definition](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.4) | __lambda__ _formals_ _body_ | _Formals_ is a list of identifiers. _Body_ is zero or more expressions. A __lambda__ expression evaluates to a Procedure object. Example: `(lambda (r) (* r r))` ⇒ ::oo::Obj3601 |
| [procedure call](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.1.3) | _operator_ _operand_... | If _operator_ is anything other than __quote__, __begin__, __if__, __define__, __set!__, or __lambda__, it is treated as a procedure. Evaluate _operator_ and all the _operands_, and then the resulting procedure is applied to the resulting list of argument values. Example: `(sqrt (+ 4 12))` ⇒ 4.0 |

The evaluator also does a simple form of macro expansion on `op` and `args` before processing them in the big `switch`. 
See the part about [macros](https://github.com/hoodiecrow/ConsTcl#macros) below.


<table border=1><thead><tr><th colspan=2 align="left">eval (public)</th></tr></thead><tr><td>e</td><td>an expression</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
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
        while {[$op name] in {
                and case cond for for/and for/list
                for/or let or define quasiquote}} {
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
```

Variable reference, or _lookup_, is handled by the helper `lookup`. It searches the
environment chain for the symbol's name, and returns the value it is bound to.

<table border=1><thead><tr><th colspan=2 align="left">lookup (internal)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::lookup {sym env} {
    [$env find $sym] get $sym
}
```

The _conditional_ form evaluates a Lisp list of three expressions. The first, the _condition_,
is evaluated first. If it evaluates to anything other than `#f`, the second expression (the
_consequent_) is evaluated and the value returned. Otherwise, the third expression (the 
_alternate_) is evaluated and the value returned.

The `eprogn` helper procedure takes a Lisp list of expressions and evaluates them in
_sequence_, returning the value of the last one.

<table border=1><thead><tr><th colspan=2 align="left">eprogn (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
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
```

The `declare` helper adds a variable to the current environment. It first checks that the
symbol name is a valid identifier, then it updates the environment with the new binding.

<table border=1><thead><tr><th colspan=2 align="left">declare (internal)</th></tr></thead><tr><td>sym</td><td>a symbol</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::declare {sym val env} {
    varcheck [idcheck [$sym name]]
    $env set $sym $val
    return #NONE
}
```

The `update!` helper does _assignment_: it modifies an existing variable that is bound
somewhere in the environment chain. It finds the variable's environment and updates the
binding. It returns the value, so calls to `set!` can be chained: `(set! foo (set! bar 99))`
sets both variables to 99.

<table border=1><thead><tr><th colspan=2 align="left">update! (internal)</th></tr></thead><tr><td>var</td><td>a bound symbol</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
proc ::constcl::update! {var val env} {
    [$env find $var] set $var $val
    set val
}
```

`make-function` makes a [Procedure](https://github.com/hoodiecrow/ConsTcl#control)
object. First it needs to convert the Lisp list `body`. It is packed inside a `begin`
if it has more than one expression, and taken out of its list if not. The Lisp list
`formals` is passed on as is.

A Scheme formals list is either:

* An _empty list_, `()`, meaning that no arguments are accepted,
* A _proper list_, `(a b c)`, meaning it accepts three arguments, one in each symbol,
* A _symbol_, `a`, meaning that all arguments go into `a`, or
* A _dotted list_, `(a b . c)`, meaning that two arguments go into `a` and `b`, and the rest into `c`.

<table border=1><thead><tr><th colspan=2 align="left">make-function (internal)</th></tr></thead><tr><td>formals</td><td>a Scheme formals list</td></tr><tr><td>body</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a procedure</td></tr></table>

```
proc ::constcl::make-function {formals body env} {
    if {[[length $body] value] > 1} {
        set body [cons #B $body]
    } else {
        set body [car $body]
    }
    return [MkProcedure $formals $body $env]
}
```

`invoke` arranges for a procedure to be called with each of the values in _vals_. It checks if
_pr_ really is a procedure, and determines whether to call _pr_ as an object or as a Tcl command.

<table border=1><thead><tr><th colspan=2 align="left">invoke (internal)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>what pr returns</td></tr></table>

```
proc ::constcl::invoke {pr vals} {
    if {[procedure? $pr] eq "#t"} {
        if {[info object isa object $pr]} {
            $pr call {*}[splitlist $vals]
        } else {
            $pr {*}[splitlist $vals]
        }
    } else {
        error "PROCEDURE expected\n([$pr show] val ...)" ;# [$vals show])
    }
}
```

`splitlist` converts a Lisp list to a Tcl list with Lisp objects.

<table border=1><thead><tr><th colspan=2 align="left">splitlist (internal)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of Lisp values</td></tr></table>

```
proc ::constcl::splitlist {vals} {
    set result {}
    while {[pair? $vals] eq "#t"} {
        lappend result [car $vals]
        set vals [cdr $vals]
    }
    return $result
}
```

`eval-list` successively evaluates the elements of a Lisp list and returns the results
as a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">eval-list (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
proc ::constcl::eval-list {exps env} {
    if {[pair? $exps] eq "#t"} {
        return [cons [eval [car $exps] $env] [eval-list [cdr $exps] $env]]
    } else {
        return #NIL
    }
}
```

### Macros

Macros that rewrite expressions into other, more concrete expressions is one of Lisp's strong
points. This interpreter does macro expansion, but the user can't define new macros--the ones
available are hardcoded in the code below.

<table border=1><thead><tr><th colspan=2 align="left">expand-macro (internal)</th></tr></thead><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>the empty list</td></tr></table>

```
proc ::constcl::expand-macro {env} {
    upvar op op args args
    if {[$op name] eq "define" && ([pair? [car $args]] eq "#f" || [[caar $args] name] eq "lambda")} {
        return -code break
    }
    switch [$op name] {
        and {
            set expr [expand-and $args]
        }
        case {
            set expr [do-case [car $args] [cdr $args]]
        }
        cond {
            set expr [do-cond $args]
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
        define {
            set expr [expand-define $args]
        }
        quasiquote {
            set expr [expand-quasiquote $args $env]
        }
    }
    set op [car $expr]
    set args [cdr $expr]
    return #NIL
}
```

`expand-and` expands the `and` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.

<table border=1><thead><tr><th colspan=2 align="left">expand-and (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-and {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return [list #B #t]
    } elseif {[eq? [length $exps] #1] eq "#t"} {
        return [cons #B $exps]
    } else {
        return [do-and $exps #NIL]
    }
}
```

<table border=1><thead><tr><th colspan=2 align="left">do-and (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>prev</td><td>an expression</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::do-and {exps prev} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return $prev
    } else {
        return [list #I [car $exps] [do-and [cdr $exps] [car $exps]] #f]
    }
}
```

The `case` macro is expanded by `do-case`. It returns `'()` if there are no clauses (left), 
and nested `if` constructs if there are some.

<table border=1><thead><tr><th colspan=2 align="left">do-case (internal)</th></tr></thead><tr><td>keyexpr</td><td>an expression</td></tr><tr><td>clauses</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
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
```

The `cond` macro is expanded by `do-cond`. It returns `'()` if there are no clauses (left), 
and nested `if` constructs if there are some.

<table border=1><thead><tr><th colspan=2 align="left">do-cond (internal)</th></tr></thead><tr><td>clauses</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
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
```

The `expand-for` procedure expands the `for` macro. It returns a `begin`
construct containing the iterations of each clause (multiple clauses
weren't implemented, but I brought up my strongest brain cells and they
did it).

<table border=1><thead><tr><th colspan=2 align="left">for-seq (internal)</th></tr></thead><tr><td>seq</td><td>a Lisp value</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of Lisp values</td></tr></table>

```
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
```

<table border=1><thead><tr><th colspan=2 align="left">do-for (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of expressions</td></tr></table>

```
proc ::constcl::do-for {exps env} {
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
```

<table border=1><thead><tr><th colspan=2 align="left">expand-for (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-for {exps env} {
    set res [do-for $exps $env]
    lappend res [list #Q #NIL]
    return [list #B {*}$res]
}
```

The `expand-for/and` procedure expands the `for/and` macro. It returns an `and`
construct containing the iterations of the clauses.

<table border=1><thead><tr><th colspan=2 align="left">expand-for/and (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-for/and {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "and"] {*}$res]
}
```

The `expand-for/list` procedure expands the `for/list` macro. It returns a `list`
construct containing the iterations of each clause.

<table border=1><thead><tr><th colspan=2 align="left">expand for/list (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-for/list {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "list"] {*}$res]
}
```

The `expand-for/or` procedure expands the `for/or` macro. It returns an `or`
construct containing the iterations of each clause.

<table border=1><thead><tr><th colspan=2 align="left">expand-for/or (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-for/or {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "or"] {*}$res]
}
```

`expand-let` expands the named `let` and 'regular' `let` macros. They ultimately
expand to `lambda` constructs.

<table border=1><thead><tr><th colspan=2 align="left">expand-let (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
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
            if {$var in [dict keys $vars]} {error "variable '$var' occurs more than once in let construct"}
            dict set vars $var $val
        }
        return [list [list #λ [list {*}[dict keys $vars]] {*}[splitlist $body]] {*}[dict values $vars]]
    }
}
```

`expand-or` expands the `or` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.

<table border=1><thead><tr><th colspan=2 align="left">expand-or (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-or {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return [list #B #f]
    } elseif {[eq? [length $exps] #1] eq "#t"} {
        return [cons #B $exps]
    } else {
        return [do-or $exps]
    }
}
```

<table border=1><thead><tr><th colspan=2 align="left">do-or (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::do-or {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return #f
    } else {
        return [list #L [list [list #x [car $exps]]] [list #I #x #x [do-or [cdr $exps]]]]
    }
}
```

`define` has two variants, one of which requires some rewriting. It's the one with an implied `lambda`
call, the one that defines a procedure.

<table border=1><thead><tr><th colspan=2 align="left">expand-define (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-define {exps} {
    set symbol [caar $exps]
    set formals [cdar $exps]
    set body [cdr $exps]
    return [list [MkSymbol "define"] $symbol [list #λ $formals {*}[splitlist $body]]]
}
```

A quasi-quote isn't a macro, but we'll deal with it in this section anyway. `expand-quasiquote`
traverses the quasi-quoted structure searching for `unquote` and `unquote-splicing`. This code is
brittle and sprawling and I barely understand it myself.

<table border=1><thead><tr><th colspan=2 align="left">qq-visit-child (internal)</th></tr></thead><tr><td>node</td><td>a Lisp list of expressions</td></tr><tr><td>qqlevel</td><td>a Tcl number</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>a Tcl list of expressions</td></tr></table>

```
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
```

<table border=1><thead><tr><th colspan=2 align="left">expand-quasiquote (internal)</th></tr></thead><tr><td>exps</td><td>a Lisp list of expressions</td></tr><tr><td>env</td><td>an environment</td></tr><tr><td><i>Returns:</i></td><td>an expression</td></tr></table>

```
proc ::constcl::expand-quasiquote {exps env} {
    set qqlevel 0
    if {[list? [car $exps]] eq "#t"} {
        set node [car $exps]
        return [qq-visit-child $node 0 $env]
    } elseif {[vector? [car $exps]] eq "#t"} {
        set vect [car $exps]
        set res {}
        for {set i 0} {$i < [[vector-length $vect] numval]} {incr i} {
            set idx [MkNumber $i]
            set vecref [vector-ref $vect $idx]
            if {[pair? $vecref] eq "#t" && [eq? [car $vecref] [MkSymbol "unquote"]] eq "#t"} {
                if {$qqlevel == 0} {
                    lappend res [eval [cadr $vecref] $env]
                }
            } elseif {[pair? $vecref] eq "#t" && [eq? [car $vecref] [MkSymbol "unquote-splicing"]] eq "#t"} {
                if {$qqlevel == 0} {
                    lappend res {*}[splitlist [eval [cadr $vecref] $env]]
                }
            } elseif {[atom? $vecref] eq "#t"} {
                lappend res $vecref
            } else {
            }
        }
        return [list [MkSymbol "vector"] {*}$res]
    }
}
```


```
proc ::constcl::scheme-report-environment {version} {
    # TODO
}
```

```
proc ::constcl::null-environment {version} {
    # TODO
}
```

```
proc ::constcl::interaction-environment {} {
    # TODO
}
```


## write

The third member in the great triad is `write`. As long as the object
given to it isn't `#NONE`, it passes it to `write-value` and prints
a newline.

<table border=1><thead><tr><th colspan=2 align="left">write (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td>args</td><td>-don't care-</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg write ::constcl::write

proc ::constcl::write {val args} {
    if {$val ne "#NONE"} {
        ::constcl::write-value $val
        puts {}
    }
}
```

`write-value` simply calls an object's `write` method, letting the object
write itself.

<table border=1><thead><tr><th colspan=2 align="left">write-value (internal)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::write-value {val} {
    $val write
}
```

The `display` procedure is like `write` but doesn't print a newline.

<table border=1><thead><tr><th colspan=2 align="left">display (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td>args</td><td>-don't care-</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg display ::constcl::display

proc ::constcl::display {val args} {
    ::constcl::write-value $val
    flush stdout
}
```

The `write-pair` procedure prints a Pair object.

<table border=1><thead><tr><th colspan=2 align="left">write-pair (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
proc ::constcl::write-pair {pair} {
    # take an object and print the car and the cdr of the stored value
    set a [car $pair]
    set d [cdr $pair]
    # print car
    write-value $a
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        puts -nonewline " "
        write-pair $d
    } elseif {[null? $d] eq "#t"} {
        # cdr is nil
        return
    } else {
        # it is an atom
        puts -nonewline " . "
        write-value $d
    }
    return #NONE
}
```


## Built-in procedures

### Equivalence predicates

Of the three equivalence predicates, `eq` generally tests for identity (with exceptions for numbers
and strings), `eqv` tests for value equality (except for booleans and procedures, where it tests for
identity), and `equal` tests for whether the output strings are equal.

<table border=1><thead><tr><th colspan=2 align="left">eq?, eqv?, equal? (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg eq? ::constcl::eq?

proc ::constcl::eq? {val1 val2} {
    if {[boolean? $val1] eq "#t" && [boolean? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] eq "#t" && [symbol? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[number? $val1] eq "#t" && [number? $val2] eq "#t" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[char? $val1] eq "#t" && [char? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[null? $val1] eq "#t" && [null? $val2] eq "#t"} {
        return #t
    } elseif {[pair? $val1] eq "#t" && [pair? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[string? $val1] eq "#t" && [string? $val2] eq "#t" && [$val1 index] eq [$val2 index]} {
        return #t
    } elseif {[vector? $val1] eq "#t" && [vector? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[procedure? $val1] eq "#t" && [procedure? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}
```

```
reg eqv? ::constcl::eqv?

proc ::constcl::eqv? {val1 val2} {
    if {[boolean? $val1] eq "#t" && [boolean? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } elseif {[symbol? $val1] eq "#t" && [symbol? $val2] eq "#t" && [$val1 name] eq [$val2 name]} {
        return #t
    } elseif {[number? $val1] eq "#t" && [number? $val2] eq "#t" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[char? $val1] eq "#t" && [char? $val2] eq "#t" && [$val1 char] eq [$val2 char]} {
        return #t
    } elseif {[null? $val1] eq "#t" && [null? $val2] eq "#t"} {
        return #t
    } elseif {[pair? $val1] eq "#t" && [pair? $val2] eq "#t" && [$val1 car] eq [$val2 car] && [$val1 cdr] eq [$val2 cdr]} {
        return #t
    } elseif {[string? $val1] eq "#t" && [string? $val2] eq "#t" && [$val1 index] eq [$val2 index]} {
        return #t
    } elseif {[vector? $val1] eq "#t" && [vector? $val2] eq "#t" && [$val1 value] eq [$val2 value]} {
        return #t
    } elseif {[procedure? $val1] eq "#t" && [procedure? $val2] eq "#t" && $val1 eq $val2} {
        return #t
    } else {
        return #f
    }
}
```

```
reg equal? ::constcl::equal?

proc ::constcl::equal? {val1 val2} {
    if {[$val1 show] eq [$val2 show]} {
        return #t
    } else {
        return #f
    }
    # TODO
}
```


### Numbers

I have only implemented a bare-bones version of Scheme's numerical
library. The following is a reasonably complete framework for operations
on integers and floating-point numbers. No rationals, no complex numbers,
no gcd or lcm.

```
oo::class create ::constcl::Number {
    superclass ::constcl::NIL
    variable value
    constructor {v} {
        if {[::string is double -strict $v]} {
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

```

`number?` recognizes a number by object type, not by content.

<table border=1><thead><tr><th colspan=2 align="left">number? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg number? ::constcl::number?

proc ::constcl::number? {val} {
    if {[info object isa typeof $val ::constcl::Number]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Number]} {
        return #t
    } else {
        return #f
    }
}
```


The operators `=`, `<`, `>`, `<=`, and `>=` are implemented. They return Lisp truth (#t / #f),
not Tcl truth.

<table border=1><thead><tr><th colspan=2 align="left">=, &lt;, &gt;, &lt;=, &gt;= (public)</th></tr></thead><tr><td>args</td><td>a Tcl list of numbers</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


The `zero?` predicate tests if a given number is equal to zero.

<table border=1><thead><tr><th colspan=2 align="left">zero? (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg zero? ::constcl::zero?

proc ::constcl::zero? {num} {
    if {[number? $num] eq "#t"} {
        return [$num zero?]
    } else {
        error "NUMBER expected\n(zero? [$num show])"
    }
}
```


The `positive?`/`negative?`/`even?`/`odd?` predicates test a number
for those traits.

<table border=1><thead><tr><th colspan=2 align="left">positive?, negative?, even?, odd? (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg positive? ::constcl::positive?

proc ::constcl::positive? {num} {
    if {[::constcl::number? $num] eq "#t"} {
        return [$num positive?]
    } else {
        error "NUMBER expected\n(positive? [$num show])"
    }
}
```


```
reg negative? ::constcl::negative?

proc ::constcl::negative? {num} {
    if {[::constcl::number? $num] eq "#t"} {
        return [$num negative?]
    } else {
        error "NUMBER expected\n(negative? [$num show])"
    }
}
```


```
reg even? ::constcl::even?

proc ::constcl::even? {num} {
    if {[::constcl::number? $num] eq "#t"} {
        return [$num even?]
    } else {
        error "NUMBER expected\n(even? [$num show])"
    }
}
```


```
reg odd? ::constcl::odd?

proc ::constcl::odd? {num} {
    if {[::constcl::number? $num] eq "#t"} {
        return [$num odd?]
    } else {
        error "NUMBER expected\n(odd? [$num show])"
    }
}
```


The `max` function selects the largest number, and the `min` function
selects the smallest number.

<table border=1><thead><tr><th colspan=2 align="left">max, min (public)</th></tr></thead><tr><td>args</td><td>a Tcl list of numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg max ::constcl::max

proc ::constcl::max {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(max num...)"
    }
    MkNumber [::tcl::mathfunc::max {*}$vals]
}
```


```
reg min ::constcl::min

proc ::constcl::min {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(min num...)"
    }
    MkNumber [::tcl::mathfunc::min {*}$vals]
}
```


The operators `+`, `*`, `-`, and `/` stand for the respective
mathematical operations. They take a number of operands, but
at least one for `-` and `/`.

<table border=1><thead><tr><th colspan=2 align="left">+, * (public)</th></tr></thead><tr><td>args</td><td>a Tcl list of numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

<table border=1><thead><tr><th colspan=2 align="left">-, / (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>args</td><td>a Tcl list of numbers</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg + ::constcl::+

proc ::constcl::+ {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(+ num ...)"
    }
    MkNumber [::tcl::mathop::+ {*}$vals]
}
```


```
reg * ::constcl::*

proc ::constcl::* {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(* num ...)"
    }
    MkNumber [::tcl::mathop::* {*}$vals]
}
```


```
reg - ::constcl::-

proc ::constcl::- {num args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(- num ...)"
    }
    MkNumber [::tcl::mathop::- [$num numval] {*}$vals]
}
```


```
reg / ::constcl::/

proc ::constcl::/ {num args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(/ num ...)"
    }
    MkNumber [::tcl::mathop::/ [$num numval] {*}$vals]
}
```


The `abs` function yields the absolute value of a number.

<table border=1><thead><tr><th colspan=2 align="left">abs (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg abs ::constcl::abs

proc ::constcl::abs {num} {
    if {[number? $num] eq "#t"} {
        if {[$num negative?] eq "#t"} {
            return [MkNumber [expr {[$num numval] * -1}]]
        } else {
            return $num
        }
    } else {
        error "NUMBER expected\n(abs [$num show])"
    }
}
```


`quotient` calculates the quotient between two numbers.

<table border=1><thead><tr><th colspan=2 align="left">quotient (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg quotient

proc ::constcl::quotient {num1 num2} {
    set q [::tcl::mathop::/ [$num1 numval] [$num2 numval]]
    if {$q > 0} {
        return [MkNumber [::tcl::mathfunc::floor $q]]
    } elseif {$q < 0} {
        return [MkNumber [::tcl::mathfunc::ceil $q]]
    } else {
        return #0
    }
}
```

`remainder` is a variant of the modulus function. (I'm a programmer, not
a mathematician!)

<table border=1><thead><tr><th colspan=2 align="left">remainder (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg remainder

proc ::constcl::remainder {num1 num2} {
    set n [::tcl::mathop::% [[abs $num1] numval] [[abs $num2] numval]]
    if {[$num1 negative?] eq "#t"} {
        set n -$n
    }
    return [MkNumber $n]
}
```

<table border=1><thead><tr><th colspan=2 align="left">modulo (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg modulo

proc ::constcl::modulo {num1 num2} {
    return [MkNumber [::tcl::mathop::% [$num1 numval] [$num2 numval]]]
}
```


```
proc ::constcl::gcd {args} {
    # TODO
}
```

```
proc ::constcl::lcm {args} {
    # TODO
}
```

```
proc ::constcl::numerator {q} {
    # TODO
}
```

```
proc ::constcl::denominator {q} {
    # TODO
}
```

`floor`, `ceiling`, `truncate`, and `round` are different methods for
converting a real number to an integer.

<table border=1><thead><tr><th colspan=2 align="left">floor, ceiling, truncate, round (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg floor ::constcl::floor

proc ::constcl::floor {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::floor [$num numval]]
    } else {
        error "NUMBER expected\n(floor [$num show])"
    }
}
```


```
reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::ceil [$num numval]]
    } else {
        error "NUMBER expected\n(ceiling [$num show])"
    }
}
```


```
reg truncate ::constcl::truncate

proc ::constcl::truncate {num} {
    if {[number? $num] eq "#t"} {
        if {[$num negative?] eq "#t"} {
            MkNumber [::tcl::mathfunc::ceil [$num numval]]
        } else {
            MkNumber [::tcl::mathfunc::floor [$num numval]]
        }
    } else {
        error "NUMBER expected\n(truncate [$num show])"
    }
}
```


```
reg round ::constcl::round

proc ::constcl::round {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::round [$num numval]]
    } else {
        error "NUMBER expected\n(round [$num show])"
    }
}
```


```
proc ::constcl::rationalize {x y} {
    # TODO
}
```

The mathematical functions _e<sup>x</sup>_, natural logarithm,
sine, cosine, tangent, arcsine, arccosine, and arctangent are
calculated by `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`,
and `atan`, respectively.

<table border=1><thead><tr><th colspan=2 align="left">exp, log, sin, cos, tan, asin, acos, atan (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

<table border=1><thead><tr><th colspan=2 align="left">(binary) atan (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg exp ::constcl::exp

proc ::constcl::exp {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::exp [$num numval]]
    } else {
        error "NUMBER expected\n(exp [$num show])"
    }
}
```


```
reg log ::constcl::log

proc ::constcl::log {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::log [$num numval]]
    } else {
        error "NUMBER expected\n(log [$num show])"
    }
}
```


```
reg sin ::constcl::sin

proc ::constcl::sin {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::sin [$num numval]]
    } else {
        error "NUMBER expected\n(sin [$num show])"
    }
}
```

```
reg cos ::constcl::cos

proc ::constcl::cos {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::cos [$num numval]]
    } else {
        error "NUMBER expected\n(cos [$num show])"
    }
}
```

```
reg tan ::constcl::tan

proc ::constcl::tan {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::tan [$num numval]]
    } else {
        error "NUMBER expected\n(tan [$num show])"
    }
}
```


```
reg asin ::constcl::asin

proc ::constcl::asin {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::asin [$num numval]]
    } else {
        error "NUMBER expected\n(asin [$num show])"
    }
}
```

```
reg acos ::constcl::acos

proc ::constcl::acos {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::acos [$num numval]]
    } else {
        error "NUMBER expected\n(acos [$num show])"
    }
}
```

```
reg atan ::constcl::atan

proc ::constcl::atan {args} {
    if {[llength $args] == 1} {
        set num [lindex $args 0]
        if {[number? $num] eq "#t"} {
            MkNumber [::tcl::mathfunc::atan [$num numval]]
        } else {
            error "NUMBER expected\n(atan [$num show])"
        }
    } else {
        lassign $args num1 num2
        if {[number? $num1] eq "#t" && [::constcl::number? $num2] eq "#t"} {
            MkNumber [::tcl::mathfunc::atan2 [$num1 numval] [$num2 numval]]
        } else {
            error "NUMBER expected\n(atan [$num1 show] [$num2 show])"
        }
    }
}
```


`sqrt` calculates the square root.

<table border=1><thead><tr><th colspan=2 align="left">sqrt (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {num} {
    if {[number? $num] eq "#t"} {
        MkNumber [::tcl::mathfunc::sqrt [$num numval]]
    } else {
        error "NUMBER expected\n(sqrt [$num show])"
    }
}
```


`expt` calculates the _x_ to the power of _y_.

<table border=1><thead><tr><th colspan=2 align="left">expt (public)</th></tr></thead><tr><td>num1</td><td>a number</td></tr><tr><td>num2</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg expt ::constcl::expt

proc ::constcl::expt {num1 num2} {
    if {[number? $num1] eq "#t" && [number? $num2] eq "#t"} {
        MkNumber [::tcl::mathfunc::pow [$num1 numval] [$num2 numval]]
    } else {
        error "NUMBER expected\n(expt [$num1 show] [$num2 show])"
    }
}
```


```
proc ::constcl::make-rectangular {x1 x2} {
    # TODO
}
```

```
proc ::constcl::make-polar {x3 x4} {
    # TODO
}
```

```
proc ::constcl::real-part {z} {
    # TODO
}
```

```
proc ::constcl::imag-part {z} {
    # TODO
}
```

```
proc ::constcl::magnitude {z} {
    # TODO
}
```

```
proc ::constcl::angle {z} {
    # TODO
}
```

```
proc ::constcl::exact->inexact {z} {
    # TODO
}
```

```
proc ::constcl::inexact->exact {z} {
    # TODO
}
```

The procedures `number->string` and `string->number` converts between
number and string with optional radix conversion.

<table border=1><thead><tr><th colspan=2 align="left">number-&gt;string (public)</th></tr></thead><tr><td>num</td><td>a number</td></tr><tr><td>?radix?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a string</td></tr></table>

```
reg number->string ::constcl::number->string

proc ::constcl::number->string {num args} {
    if {[llength $args] == 0} {
        if {[number? $num] eq "#t"} {
            return [MkString [$num numval]]
        } else {
            error "NUMBER expected\n(string->number [$num show])"
        }
    } else {
        lassign $args radix
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
```


<table border=1><thead><tr><th colspan=2 align="left">string-&gt;number (public)</th></tr></thead><tr><td>str</td><td>a string</td></tr><tr><td>?radix?</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg string->number ::constcl::string->number

proc ::constcl::string->number {str args} {
    if {[llength $args] == 0} {
        if {[string? $str] eq "#t"} {
            return [MkNumber [$str value]]
        } else {
            error "STRING expected\n(string->number [$str show])"
        }
    } else {
        lassign $args radix
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
```



### Booleans

Booleans are logic values, either true (`#t`) or false (`#f`).
All predicates (procedures whose name end with -?) return
boolean values. The conditional `if` operator considers all
values except for `#f` to be true.

```
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
```


The `boolean?` predicate recognizes a Boolean by type.

<table border=1><thead><tr><th colspan=2 align="left">boolean? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg boolean? ::constcl::boolean?

proc ::constcl::boolean? {val} {
    if {[info object isa typeof $val ::constcl::Boolean]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Boolean]} {
        return #t
    } else {
        return #f
    }
}
```


The only operation on booleans: `not`, or logical negation.

<table border=1><thead><tr><th colspan=2 align="left">not (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg not ::constcl::not

proc ::constcl::not {val} {
    if {[$val bvalue] eq "#f"} {
        return #t
    } else {
        return #f
    }
}
```



### Characters

Characters are any Unicode printing character, and also space and newline space characters.

```
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
```

`char?` recognizes Char values by type.


```
reg char? ::constcl::char?

proc ::constcl::char? {val} {
    if {[info object isa typeof $val ::constcl::Char]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Char]} {
        return #t
    } else {
        return #f
    }
}
```


`char=?`, `char<?`, `char>?`, `char<=?`, and `char>=?` compare character
values. They only compare two characters at a time.

<table border=1><thead><tr><th colspan=2 align="left">char=?, char&lt;?, char&gt;?, char&lt;=?, char&gt;=? (public)</th></tr></thead><tr><td>char1</td><td>a character</td></tr><tr><td>char2</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char=? ::constcl::char=?

proc ::constcl::char=? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {$char1 eq $char2} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$char1 show] [$char2 show])"
    }
}
```


```
reg char<? ::constcl::char<?

proc ::constcl::char<? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[$char1 char] < [$char2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$char1 show] [$char2 show])"
    }
}
```


```
reg char>? ::constcl::char>?

proc ::constcl::char>? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[$char1 char] > [$char2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$char1 show] [$char2 show])"
    }
}
```


```
reg char<=? ::constcl::char<=?

proc ::constcl::char<=? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[$char1 char] <= [$char2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$char1 show] [$char2 show])"
    }
}
```


```
reg char>=? ::constcl::char>=?

proc ::constcl::char>=? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[$char1 char] >= [$char2 char]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$char1 show] [$char2 show])"
    }
}
```


`char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, and `char-ci>=?` compare character
values in a case insensitive manner. They only compare two characters at a time.

<table border=1><thead><tr><th colspan=2 align="left">char-ci=?, char-ci&lt;?, char-ci&gt;?, char-ci&lt;=?, char-ci&gt;=? (public)</th></tr></thead><tr><td>char1</td><td>a character</td></tr><tr><td>char2</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char-ci=? ::constcl::char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[::string tolower [$char1 char]] eq [::string tolower [$char2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char=? [$char1 show] [$char2 show])"
    }
}
```


```
reg char-ci<? ::constcl::char-ci<?

proc ::constcl::char-ci<? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[::string tolower [$char1 char]] < [::string tolower [$char2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<? [$char1 show] [$char2 show])"
    }
}
```


```
reg char-ci>? ::constcl::char-ci>?

proc ::constcl::char-ci>? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[::string tolower [$char1 char]] > [::string tolower [$char2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>? [$char1 show] [$char2 show])"
    }
}
```


```
reg char-ci<=? ::constcl::char-ci<=?

proc ::constcl::char-ci<=? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[::string tolower [$char1 char]] <= [::string tolower [$char2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char<=? [$char1 show] [$char2 show])"
    }
}
```


```
reg char-ci>=? ::constcl::char-ci>=?

proc ::constcl::char-ci>=? {char1 char2} {
    if {[char? $char1] eq "#t" && [char? $char2] eq "#t"} {
        if {[::string tolower [$char1 char]] >= [::string tolower [$char2 char]]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "CHAR expected\n(char>=? [$char1 show] [$char2 show])"
    }
}
```


The predicates `char-alphabetic`, `char-numeric`, `char-whitespace`,
`char-upper-case`, and `char-lower-case` test a character for these
conditions.

<table border=1><thead><tr><th colspan=2 align="left">char-alphabetic?, char-numeric?, char-whitespace?, char-upper-case?, char-lower-case? (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg char-alphabetic? ::constcl::char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
    if {[char? $char] eq "#t"} {
        return [$char alphabetic?]
    } else {
        error "CHAR expected\n(char-alphabetic? [$char show])"
    }
}
```


```
reg char-numeric? ::constcl::char-numeric?

proc ::constcl::char-numeric? {char} {
    if {[char? $char] eq "#t"} {
        return [$char numeric?]
    } else {
        error "CHAR expected\n(char-numeric? [$char show])"
    }
}
```


```
reg char-whitespace? ::constcl::char-whitespace?

proc ::constcl::char-whitespace? {char} {
    if {[char? $char] eq "#t"} {
        return [$char whitespace?]
    } else {
        error "CHAR expected\n(char-whitespace? [$char show])"
    }
}
```


```
reg char-upper-case? ::constcl::char-upper-case?

proc ::constcl::char-upper-case? {char} {
    if {[char? $char] eq "#t"} {
        return [$char upper-case?]
    } else {
        error "CHAR expected\n(char-upper-case? [$char show])"
    }
}
```


```
reg char-lower-case? ::constcl::char-lower-case?

proc ::constcl::char-lower-case? {char} {
    if {[char? $char] eq "#t"} {
        return [$char lower-case?]
    } else {
        error "CHAR expected\n(char-lower-case? [$char show])"
    }
}
```


`char->integer` and `integer->char` convert between characters and their
16-bit numeric codes.

<table border=1><thead><tr><th colspan=2 align="left">char-&gt;integer (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>an integer</td></tr></table>

```
reg char->integer

proc ::constcl::char->integer {char} {
    return [MkNumber [scan [$char char] %c]]
}
```

<table border=1><thead><tr><th colspan=2 align="left">integer-&gt;char (public)</th></tr></thead><tr><td>int</td><td>an integer</td></tr><tr><td><i>Returns:</i></td><td>a character</td></tr></table>

```
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
```


`char-upcase` and `char-downcase` alter the case of a character.

<table border=1><thead><tr><th colspan=2 align="left">char-upcase, char-downcase (public)</th></tr></thead><tr><td>char</td><td>a character</td></tr><tr><td><i>Returns:</i></td><td>a character</td></tr></table>

```
reg char-upcase ::constcl::char-upcase

proc ::constcl::char-upcase {char} {
    if {[char? $char] eq "#t"} {
        if {[::string is alpha -strict [$char char]]} {
            return [MkChar [::string toupper [$char value]]]
        } else {
            return $char
        }
    } else {
        error "CHAR expected\n(char-upcase [$char show])"
    }
}
```



```
reg char-downcase ::constcl::char-downcase

proc ::constcl::char-downcase {char} {
    if {[char? $char] eq "#t"} {
        if {[::string is alpha -strict [$char char]]} {
            return [MkChar [::string tolower [$char value]]]
        } else {
            return $char
        }
    } else {
        error "CHAR expected\n(char-downcase [$char show])"
    }
}
```



### Control

This section concerns itself with procedures and the application of the same.

A `Procedure` object is a
[closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)),
storing the procedure's parameter list, the body, and the environment that is current
when the object is created (when the procedure is defined).

When a `Procedure` object is called, the body is evaluated in a new environment
where the parameters are given values from the argument list and the outer link
goes to the closure environment.


```
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
    method write {} { puts -nonewline [self] }
    method show {} { return [self] }
    method call {args} {
        ::constcl::eval $body [::constcl::Environment new $parms $args $env]
    }

}

interp alias {} ::constcl::MkProcedure {} ::constcl::Procedure new
```

<table border=1><thead><tr><th colspan=2 align="left">procedure? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg procedure? ::constcl::procedure?

proc ::constcl::procedure? {val} {
    if {[info object isa typeof $val ::constcl::Procedure]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Procedure]} {
        return #t
    } elseif {[::string match "::constcl::*" $val]} {
        return #t
    } else {
        return #f
    }
}
```


`apply` applies a procedure to a Lisp list of Lisp arguments.

<table border=1><thead><tr><th colspan=2 align="left">apply (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>what pr returns</td></tr></table>

```
reg apply ::constcl::apply

proc ::constcl::apply {pr vals} {
    if {[procedure? $pr] eq "#t"} {
        invoke $pr $vals
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}
```


`map` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The Lisp list of the results of the invocations is 
returned.

<table border=1><thead><tr><th colspan=2 align="left">map (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>args</td><td>a Tcl list of Lisp lists of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
reg map ::constcl::map

proc ::constcl::map {pr args} {
    if {[procedure? $pr] eq "#t"} {
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
    } else {
        error "PROCEDURE expected\n(apply [$pr show] ...)"
    }
}
```


`for-each` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The empty list is returned.

<table border=1><thead><tr><th colspan=2 align="left">for-each (public)</th></tr></thead><tr><td>pr</td><td>a procedure</td></tr><tr><td>args</td><td>a Tcl list of Lisp lists of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>the empty list</td></tr></table>

```
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
```


```
proc ::constcl::force {promise} {
    # TODO
}
```

```
proc ::constcl::call-with-current-continuation {proc} {
    # TODO
}
```

```
proc ::constcl::values {args} {
    # TODO
}
```

```
proc ::constcl::call-with-values {producer consumer} {
    # TODO
}
```

```
proc ::constcl::dynamic-wind {before thunk after} {
    # TODO
}
```


### Input and output

I may never get around to implementing these.

```
proc ::constcl::call-with-input-file {string proc} {
    # TODO
}
```

```
proc ::constcl::call-with-output-file {string proc} {
    # TODO
}
```

```
proc ::constcl::input-port? {obj} {
    # TODO
}
```

```
proc ::constcl::output-port? {obj} {
    # TODO
}
```

```
proc ::constcl::current-input-port {} {
    # TODO
}
```

```
proc ::constcl::current-output-port {} {
    # TODO
}
```

```
proc ::constcl::with-input-from-file {string thunk} {
    # TODO
}
```


```
proc ::constcl::with-output-to-file {string thunk} {
    # TODO
}
```

```
proc ::constcl::open-input-file {filename} {
    # TODO
}
```

```
proc ::constcl::open-output-file {filename} {
    # TODO
}
```

```
proc ::constcl::close-input-port {port} {
    # TODO
}
```

```
proc ::constcl::close-output-port {port} {
    # TODO
}
```

`read` implemented in [read](https://github.com/hoodiecrow/ConsTcl#read) section.

```
proc ::constcl::read-char {args} {
    # TODO
}
```

```
proc ::constcl::peek-char {args} {
    # TODO
}
```

```
proc ::constcl::char-ready? {args} {
    # TODO
}
```

`write` implemented in [write](https://github.com/hoodiecrow/ConsTcl#write) section.

`display` implemented in [write](https://github.com/hoodiecrow/ConsTcl#write) section.

```
reg newline ::constcl::newline

proc ::constcl::newline {args} {
    # TODO write newline
}
```

```
proc ::constcl::write-char {args} {
    # TODO
}
```

```
proc ::constcl::load {filename} {
    # TODO
}
```

```
proc ::constcl::transcript-on {filename} {
    # TODO
}
```

```
proc ::constcl::transcript-off {} {
    # TODO
}
```


### Pairs and lists

List processing is another of Lisp's great strengths.

```
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
```

<table border=1><thead><tr><th colspan=2 align="left">pair? (public)</th></tr></thead><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg pair? ::constcl::pair?

proc ::constcl::pair? {val} {
    if {[info object isa typeof $val ::constcl::Pair]} {
        return #t
    } elseif {[info object isa typeof [interp alias {} $val] ::constcl::Pair]} {
        return #t
    } else {
        return #f
    }
}
```

Helper procedure to make a string representation of a list.

<table border=1><thead><tr><th colspan=2 align="left">show-pair (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Tcl string</td></tr></table>

```
proc ::constcl::show-pair {pair} {
    # take an object and print the car and the cdr of the stored value
    set str {}
    set a [car $pair]
    set d [cdr $pair]
    # print car
    ::append str [$a show]
    if {[pair? $d] eq "#t"} {
        # cdr is a cons pair
        ::append str " "
        ::append str [show-pair $d]
    } elseif {[null? $d] eq "#t"} {
        # cdr is nil
        return $str
    } else {
        # it is an atom
        ::append str " . "
        ::append str [$d show]
    }
    return $str
}
```


`cons` joins two values in a pair; useful in many operations such as pushing
a new value onto a list.

<table border=1><thead><tr><th colspan=2 align="left">cons (public)</th></tr></thead><tr><td>car</td><td>a Lisp value</td></tr><tr><td>cdr</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>a pair</td></tr></table>

```
reg cons ::constcl::cons

proc ::constcl::cons {car cdr} {
    MkPair $car $cdr
}
```


`car` gets the contents of the first cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">car (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg car ::constcl::car

proc ::constcl::car {pair} {
    $pair car
}
```


`cdr` gets the contents of the second cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">cdr (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg cdr ::constcl::cdr

proc ::constcl::cdr {pair} {
    $pair cdr
}
```


`car` and `cdr` can be combined to form 28 composite access
operations.

```
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
```

`set-car!` sets the contents of the first cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">set-car! (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {pair val} {
    $pair set-car! $val
}
```


`set-cdr!` sets the contents of the second cell in a pair.

<table border=1><thead><tr><th colspan=2 align="left">set-cdr! (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>val</td><td>a Lisp value</td></tr><tr><td><i>Returns:</i></td><td>nothing</td></tr></table>

```
reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {pair val} {
    $pair set-cdr! $val
}
```


The `list?` predicate tests if a pair is part of a proper list, one that
ends with NIL.

<table border=1><thead><tr><th colspan=2 align="left">listp (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
proc ::constcl::listp {pair} {
    upvar visited visited
    if {$pair in $visited} {
        return #f
    }
    lappend visited $pair
    if {[null? $pair] eq "#t"} {
        return #t
    } elseif {[pair? $pair] eq "#t"} {
        return [listp [cdr $pair]]
    } else {
        return #f
    }
}
```

<table border=1><thead><tr><th colspan=2 align="left">list? (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a boolean</td></tr></table>

```
reg list? ::constcl::list?

proc ::constcl::list? {pair} {
    set visited {}
    return [listp $pair]
}
```


`list` constructs a Lisp list from a Tcl list of items.

<table border=1><thead><tr><th colspan=2 align="left">list (public)</th></tr></thead><tr><td>args</td><td>a Tcl list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
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
```


`length` reports the length of a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">length-helper (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a Tcl number</td></tr></table>

```
proc ::constcl::length-helper {pair} {
    if {[null? $pair] eq "#t"} {
        return 0
    } else {
        return [expr {1 + [length-helper [cdr $pair]]}]
    }
}
```

<table border=1><thead><tr><th colspan=2 align="left">length (public)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td><i>Returns:</i></td><td>a number</td></tr></table>

```
reg length ::constcl::length

proc ::constcl::length {pair} {
    if {[list? $pair] eq "#t"} {
        MkNumber [length-helper $pair]
    } else {
        error "LIST expected\n(list lst)"
    }
}
```


`append` joins lists together.

<table border=1><thead><tr><th colspan=2 align="left">copy-list (internal)</th></tr></thead><tr><td>pair</td><td>a pair</td></tr><tr><td>next</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
proc ::constcl::copy-list {pair next} {
    # TODO only fresh conses in the direct chain to NIL
    if {[null? $pair] eq "#t"} {
        set next
    } elseif {[null? [cdr $pair]] eq "#t"} {
        cons [car $pair] $next
    } else {
        cons [car $pair] [copy-list [cdr $pair] $next]
    }
}
```

<table border=1><thead><tr><th colspan=2 align="left">append (public)</th></tr></thead><tr><td>args</td><td>a Tcl list of Lisp lists of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
reg append ::constcl::append

proc ::constcl::append {args} {
    set prev [lindex $args end]
    foreach r [lreverse [lrange $args 0 end-1]] {
        set prev [copy-list $r $prev]
    }
    set prev
}
```


`reverse` produces a reversed copy of a Lisp list.

<table border=1><thead><tr><th colspan=2 align="left">reverse (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
reg reverse ::constcl::reverse

proc ::constcl::reverse {vals} {
    list {*}[lreverse [splitlist $vals]]
}
```


Given a list index, `list-tail` yields the sublist starting from that index.

<table border=1><thead><tr><th colspan=2 align="left">list-tail (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of Lisp values</td></tr></table>

```
reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {vals k} {
    if {[zero? $k] eq "#t"} {
        return $vals
    } else {
        list-tail [cdr $vals] [- $k #1]
    }
}
```


`list-ref` yields the list item at a given index.

<table border=1><thead><tr><th colspan=2 align="left">list-ref (public)</th></tr></thead><tr><td>vals</td><td>a Lisp list of Lisp values</td></tr><tr><td>k</td><td>a number</td></tr><tr><td><i>Returns:</i></td><td>a Lisp value</td></tr></table>

```
reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {vals k} {
    car [list-tail $vals $k]
}
```


`memq`, `memv`, and `member` return the sublist starting with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison.

<table border=1><thead><tr><th colspan=2 align="left">member-proc (internal)</th></tr></thead><tr><td>epred</td><td>an equivalence predicate</td></tr><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```

proc ::constcl::member-proc {epred val1 val2} {
    if {[list? $val2] eq "#t"} {
        if {[null? $val2] eq "#t"} {
            return #f
        } elseif {[pair? $val2] eq "#t"} {
            if {[$epred $val1 [car $val2]] eq "#t"} {
                return $val2
            } else {
                return [member-proc $epred $val1 [cdr $val2]]
            }
        }
    } else {
        switch $epred {
            eq? { set name "memq" }
            eqv? { set name "memv" }
            equal? { set name "member" }
        }
        error "LIST expected\n($name [$val1 show] [$val2 show])"
    }
}
```

<table border=1><thead><tr><th colspan=2 align="left">memq (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg memq ::constcl::memq

proc ::constcl::memq {val1 val2} {
    return [member-proc eq? $val1 $val2]
}
```


<table border=1><thead><tr><th colspan=2 align="left">memv (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg memv ::constcl::memv

proc ::constcl::memv {val1 val2} {
    return [member-proc eqv? $val1 $val2]
}
```

<table border=1><thead><tr><th colspan=2 align="left">member (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg member ::constcl::member

proc ::constcl::member {val1 val2} {
    return [member-proc equal? $val1 $val2]
}
```

`assq`, `assv`, and `assoc` return the associative item marked with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison. They implement lookup in the form of lookup
table known as an association list, or _alist_.

Example:

```
    (define e '((a 1) (b 2) (c 3)))
    (assq 'a e)
                                   ⇒ (a 1)
```

<table border=1><thead><tr><th colspan=2 align="left">assoc-proc (internal)</th></tr></thead><tr><td>epred</td><td>an equivalence predicate</td></tr><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td></td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
proc ::constcl::assoc-proc {epred val1 val2} {
    if {[list? $val2] eq "#t"} {
        if {[null? $val2] eq "#t"} {
            return #f
        } elseif {[pair? $val2] eq "#t"} {
            if {[pair? [car $val2]] eq "#t" && [$epred $val1 [caar $val2]] eq "#t"} {
                return [car $val2]
            } else {
                return [assoc-proc $epred $val1 [cdr $val2]]
            }
        }
    } else {
        switch $epred {
            eq? { set name "assq" }
            eqv? { set name "assv" }
            equal? { set name "assoc" }
        }
        error "LIST expected\n($name [$val1 show] [$val2 show])"
    }
}
```

<table border=1><thead><tr><th colspan=2 align="left">assq (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>

```
reg assq

proc ::constcl::assq {val1 val2} {
    return [assoc-proc eq? $val1 $val2]
}
```

<table border=1><thead><tr><th colspan=2 align="left">assv (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>


```
reg assv

proc ::constcl::assv {val1 val2} {
    return [assoc-proc eqv? $val1 $val2]
}
```

<table border=1><thead><tr><th colspan=2 align="left">assoc (public)</th></tr></thead><tr><td>val1</td><td>a Lisp value</td></tr><tr><td>val2</td><td>a Lisp list of Lisp values</td></tr><tr><td><i>Returns:</i></td><td>a Lisp list of values OR #f</td></tr></table>


```
reg assoc

proc ::constcl::assoc {val1 val2} {
    return [assoc-proc equal? $val1 $val2]
}
```



### Strings

Procedures for dealing with strings of characters.

```
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
```

Helper function for finding a string in the string store.

```
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
```


`make-string` _k_ _?c?_ creates a string of _k_ characters, optionally
filled with _c_ characters.

```
reg make-string ::constcl::make-string

proc ::constcl::make-string {k args} {
    if {[llength $args] == 0} {
        return [MkString [::string repeat " " [$k numval]]]
    } else {
        lassign $args c
        return [MkString [::string repeat [$c char] [$k numval]]]
    }
}
```


`string` constructs a string from a Tcl list of Lisp characters.

```
reg string ::constcl::string

proc ::constcl::string {args} {
    set str {}
    foreach char $args {
        if {[::constcl::char? $char] eq "#t"} {
            ::append str [$char char]
        } else {
            error "CHAR expected\n(string [lmap c $args {$c show}])"
        }
    }
    return [MkString $str]
}
```


`string-length` reports a string's length.

```
reg string-length ::constcl::string-length

proc ::constcl::string-length {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkNumber [$str length]]
    } else {
        error "STRING expected\n(string-length [$str show])"
    }
}
```


`string-ref` _str_ _k_ yields the _k_-th character (0-based) in _str_.

```
reg string-ref ::constcl::string-ref

proc ::constcl::string-ref {str k} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k numval]
        } else {
            error "Exact INTEGER expected\n(string-ref [$str show] [$k show])"
        }
        return [MkChar "#\\[$str ref $i]"]
    } else {
        error "STRING expected\n(string-ref [$str show] [$k show])"
    }
}
```


`string-set!` _str_ _k_ _char_ replaces the character at _k_ with _char_.

```
reg string-set! ::constcl::string-set!

proc ::constcl::string-set! {str k char} {
    if {[::constcl::string? $str] eq "#t"} {
        if {[::constcl::number? $k] eq "#t"} {
            set i [$k numval]
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
```


`string=?`, `string<?`, `string>?`, `string<=?`, `string>=?` and their
case insensitive variants `string-ci=?`, `string-ci<?`, `string-ci>?`,
`string-ci<=?`, `string-ci>=?` compare strings.

```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


`substring` _str_ _start_ _end_ yields the substring of _str_ that starts at _start_
and ends at _end_.

```
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
```


`string-append` joins strings together.

```
reg string-append ::constcl::string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {$arg value}]]
}
```


`string->list` converts a string to a Lisp list of characters.

```
reg string->list ::constcl::string->list

proc ::constcl::string->list {str} {
    list {*}[lmap c [split [$str value] {}] {MkChar "#\\$c"}]
}
```


`list->string` converts a Lisp list of characters to a string.

```
reg list->string ::constcl::list->string

proc ::constcl::list->string {list} {
    MkString [::append --> {*}[lmap c [splitlist $list] {$c char}]]
}
```


`string-copy` makes a copy of a string.

```
reg string-copy ::constcl::string-copy

proc ::constcl::string-copy {str} {
    if {[::constcl::string? $str] eq "#t"} {
        return [MkString [$str value]]
    } else {
        error "STRING expected\n(string-copy [$str show])"
    }
}
```


`string-fill!` _str_ _char_ fills a non-constant string with _char_.

```
reg string-fill! ::constcl::string-fill!

proc ::constcl::string-fill! {str char} {
    if {[::constcl::string? $str] eq "#t"} {
        $str fill! [$char char]
        return $str
    } else {
        error "STRING expected\n(string-fill [$str show] [$char show])"
    }
}
```



### Symbols

Symbols are like little strings that are used to refer to things (variables, including
procedure names, etc) or for comparing against each other.

```
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
```


`symbol->string` yields a string consisting of the symbol name, usually
lower-cased.

```
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
```


`string->symbol` creates a symbol with the name given by the string. The symbol
is 'case-constant', i.e. it will not be lower-cased.

```
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
```


### Vectors

Vectors are heterogenous structures of fixed length whose elements are indexed by integers. 
They are implemented as Tcl lists of Lisp values.

The number of elements that a vector contains (the _length_) is set when the vector is created.
Elements can be indexed by integers from zero to length minus one.

```
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
```


`make-vector` creates a vector with a given length and optionally a fill value.

```
reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {k args} {
    if {[llength $args] == 0} {
        lassign $args k
        set fill #NIL
    } else {
        lassign $args fill
    }
    MkVector [lrepeat [$k numval] $fill]
}
```

Given a number of Lisp values, `vector` creates a vector containing them.

```
reg vector ::constcl::vector

proc ::constcl::vector {args} {
    MkVector $args
}
```


`vector-length` returns the length of a vector.

```
reg vector-length ::constcl::vector-length

proc ::constcl::vector-length {vec} {
    if {[vector? $vec] eq "#t"} {
        return [MkNumber [$vec length]]
    } else {
        error "VECTOR expected\n(vector-length [$vec show])"
    }
}
```


`vector-ref` _vector_ _k_ returns the element of _vector_ at index _k_.

```
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
    if {[vector? $vec] eq "#t"} {
        if {[number? $k] eq "#t"} {
            return [$vec ref [$k numval]]
        } else {
            error "NUMBER expected\n(vector-ref [$vec show] [$k show])"
        }
    } else {
        error "VECTOR expected\n(vector-ref [$vec show] [$k show])"
    }
}
```


`vector-set!` _vector_ _k_ _obj_, for a non-constant vector, sets the element at
index _k_ to _obj_.

```
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k obj} {
    if {[vector? $vec] eq "#t"} {
        if {[number? $k] eq "#t"} {
            return [$vec set! [$k numval] $obj]
        } else {
            error "NUMBER expected\n(vector-set! [$vec show] [$k show] [$obj show])"
        }
    } else {
        error "VECTOR expected\n(vector-set! [$vec show] [$k show] [$obj show])"
    }
}
```


`vector->list` converts a vector value to a Lisp list.

```
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
    list {*}[$vec value]
}
```


`list->vector` converts a Lisp list value to a vector.

```
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
    vector {*}[splitlist $list]
}
```


`vector-fill!` fills a non-constant vector with a given value.

```
reg vector-fill! ::constcl::vector-fill!

proc ::constcl::vector-fill! {vec fill} {
    if {[vector? $vec] eq "#t"} {
        $vec fill! $fill
    } else {
        error "VECTOR expected\n(vector-fill [$vec show] [$fill show])"
    }
}
```



## Identifier validation

Some routines for checking if a string is a valid identifier. `idcheckinit` checks the
first character, `idchecksubs` checks the rest. `idcheck` calls the others and raises
errors if they fail. A valid symbol is still an invalid identifier if has the name of
some keyword, which varcheck checks, for a set of keywords given in the standard.

```
proc ::constcl::idcheckinit {init} {
    if {[::string is alpha -strict $init] || $init in {! $ % & * / : < = > ? ^ _ ~}} {
        return true
    } else {
        return false
    }
}

proc ::constcl::idchecksubs {subs} {
    foreach c [split $subs {}] {
        if {!([::string is alnum -strict $c] || $c in {! $ % & * / : < = > ? ^ _ ~ + - . @})} {
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
```

## Initialization

Initialize the string store with the running index `S` and the
storage variable `StrSto`.

```
unset -nocomplain ::constcl::S ;# string store number
set ::constcl::S 0

unset -nocomplain ::constcl::StrSto
set ::constcl::StrSto [list]
```

Pre-make a set of constants (mostly symbols but also e.g. #NIL, #t, and #f)
and give them aliases for use in source text.

```
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

```

Initialize the definition register with the queen of numbers (or at least
a double floating point approximation).

```
dict set ::constcl::defreg pi [::constcl::MkNumber 3.1415926535897931]
```

`atom?` recognizes an atom by checking for membership in one of the atomic types.

```
reg atom? ::constcl::atom?

proc ::constcl::atom? {obj} {
    if {[symbol? $obj] eq "#t" || [number? $obj] eq "#t" || [string? $obj] eq "#t" || [char? $obj] eq "#t" || [boolean? $obj] eq "#t" || [vector? $obj] eq "#t"} {
        return #t
    } else {
        return #f
    }
}
```





## The REPL

The REPL ([read-eval-print loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop))
is a loop that repeatedly _reads_ a Scheme source string from the user through the command
`::constcl::input` (breaking the loop if given an empty line) and `::constcl::parse`, _evaluates_ it using
`::constcl::eval`, and _prints_ using `::constcl::write`.

`input` is modelled after the Python 3 function. It displays a prompt and reads a string.

```
proc ::constcl::input {prompt} {
    puts -nonewline $prompt
    flush stdout
    gets stdin
}
```

`repl` puts the loop in the read-eval-print loop. It repeats prompting for a string until given
a blank input. Given non-blank input, it parses and evaluates the string, printing the resulting value.

```
proc ::constcl::repl {{prompt "ConsTcl> "}} {
    set str [input $prompt]
    while {$str ne ""} {
        write [eval [parse $str]]
        set str [input $prompt]
    }
}
```

## Environment class and objects

The class for environments is called `Environment`. It is mostly a wrapper around a dictionary,
with the added finesse of keeping a link to the outer environment (starting a chain that goes all
the way to the global environment and then stops at the null environment) which can be traversed
by the find method to find which innermost environment a given symbol is bound in.

The long and complex constructor is to accommodate the variations of Scheme parameter lists, which 
can be empty, a proper list, a symbol, or an improper list.

```
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
```

# vim: set filetype=tcl:

On startup, two `Environment` objects called `null_env` (the null environment, not the same
as `null-environment` in Scheme) and `global_env` (the global environment) are created. 

Make `null_env` empty and unresponsive: this is where searches for unbound symbols end up.

```
::constcl::Environment create ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
    method find {sym} {self}
    method get {sym} {error "Unbound variable: [$sym name]"}
    method set {sym val} {error "Unbound variable: [$sym name]"}
}
```

Meanwhile, `global_env` is populated with all the definitions from the definitions register,
`defreg`. This is where top level evaluation happens.

```
namespace eval ::constcl {
    set keys [list {*}[lmap k [dict keys $defreg] {MkSymbol $k}]]
    set vals [dict values $defreg]
    Environment create global_env $keys $vals ::constcl::null_env
}
```

Thereafter, each time a user-defined procedure is called, a new `Environment` object is
created to hold the bindings introduced by the call, and also a link to the outer environment
(the one closed over when the procedure was defined).

#### Lexical scoping


Example:

```
ConsTcl> (define circle-area (lambda (r) (* pi (* r r))))
ConsTcl> (circle-area 10)
314.1592653589793
```

During a call to the procedure `circle-area`, the symbol `r` is bound to the
value 10. But we don't want the binding to go into the global environment,
possibly clobbering an earlier definition of `r`. The solution is to use
separate (but linked) environments, making `r`'s binding a
_[local variable](https://en.wikipedia.org/wiki/Local_variable)_
in its own environment, which the procedure will be evaluated in. The symbols
`*` and `pi` will still be available through the local environment's link
to the outer global environment. This is all part of
_[lexical scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope)_.

In the first image, we see the global environment before we call `circle-area`
(and also the empty null environment which the global environment links to):

![A global environment](/images/env1.png)

During the call. Note how the global `r` is shadowed by the local one, and how
the local environment links to the global one to find `*` and `pi`. 

![A local environment shadows the global](/images/env2.png)

After the call, we are back to the first state again.

![A global environment](/images/env1.png)




