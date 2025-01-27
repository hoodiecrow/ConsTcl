# ConsTcl
A second try at a Lisp interpreter written in Tcl, this time with a real Lisp-like type system.


#### Benchmark

On my cheap computer, the following code takes 0.025 seconds to run.

```
namespace eval ::constcl {
    eval [parse "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"]
    time {eval [parse "(fact 100)"]} 10
}
```

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
    ::constcl::expand-macro op args ::constcl::global_env
    set val [::constcl::cons $op $args]
    ::constcl::write $val
}
```

This one is a little bit of both, a utility function that is also among the
builtins in the library. It started out as a one-liner by Donal K. Fellows,
but has grown a bit since then to suit my needs.

```
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
```

The `NIL` class has one object: the empty list called `#NIL`. It is also base class for many other
type classes.

```
catch { ::constcl::NIL destroy }

oo::class create ::constcl::NIL {
    constructor {} {}
    method bvalue {} {return #t}
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

A quick-and-dirty input simulator, using an input buffer variable to hold characters
to be read. The `advance` command consumes one character from the buffer. The `first`
command peeks at the first (next) character in the buffer; `second` peeks at the 
second. `skip-whitespace` advances past whitespace and comments.


```
set inputbuffer {}

proc ::constcl::advance {args} {
    if {[llength $args] == 1} {
        incr args -1
        set ::inputbuffer [::string range $::inputbuffer 1+$args end]
    } else {
        set ::inputbuffer [::string range $::inputbuffer 1 end]
    }
}

proc ::constcl::first {} {
    ::string index $::inputbuffer 0
}

proc ::constcl::second {} {
    ::string index $::inputbuffer 1
}

proc ::constcl::skip-whitespace {} {
    # move the source pointer past whitespace and comments
    # adapted from Robert Nystrom, 'Crafting Interpreters'
    while true {
        set c [first]
        switch $c {
            " " - "\r" - "\t" -
            "\n" {
                advance
            }
            ";" {
                # a comment goes on until the end of the line
                while {[first] != "\n" && $::inputbuffer ne {}} {
                    advance
                }
            }
            default {
                return
            }
        }
    }
}
```

`parse` fills the input buffer and then reads and parses the input.

```
proc ::constcl::parse {str} {
    set ::inputbuffer $str
    return [read]
}
```

The standard builtin `read` consumes and parses input into a Lisp expression.

```
reg read ::constcl::read

proc ::constcl::read {args} {
    ::constcl::read-value
}
```

The helper procedure `read-value` reads a value of any kind.

```
proc ::constcl::read-value {} {
    skip-whitespace
    if {$::inputbuffer eq {}} {set ::inputbuffer [gets stdin]}
    switch -regexp [first] {
        {^$}          { return }
        {\"}          { return [read-string] }
        {\#}          { return [read-sharp] }
        {\'}          { return [read-quoted-value] }
        {\(}          { return [read-pair-value ")"] }
        {\+} - {\-}   { return [read-plus-minus] }
        {\.}          { advance ; return [Dot new] }
        {\[}          { return [read-pair-value "\]"] }
        {\d}          { return [read-number] }
        {[[:space:]]} { advance }
        {[[:graph:]]} { return [read-identifier] }
        default {
            error "unexpected char [first]"
        }
    }
}
```

`read-string` reads a string value and returns a [String](https://github.com/hoodiecrow/ConsTcl#strings) object.

```
proc ::constcl::read-string {} {
    set str {}
    advance
    while {[first] ne {"}} {
        set c [first]
        if {$c eq "\\"} {
            advance
            ::append str [first]
        } else {
            ::append str $c
        }
        advance
    }
    advance
    set str [MkString $str]
    $str mkconstant
    return $str
}
```


`read-sharp` reads the various kinds of values whose literal begins with
a sharp sign (#).

```
proc ::constcl::read-sharp {} {
    advance
    switch [first] {
        (    { return [read-vector] }
        t    { advance ; return #t }
        f    { advance ; return #f }
        "\\" { return [read-character] }
        default {
            error "Illegal #-literal"
        }
    }
}
```

The `make-constant` helper procedure is called to set objects to
constants when read as a quoted literal.

```
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
```

`read-quoted-value` reads a value and returns it wrapped in `quote`.

```
proc ::constcl::read-quoted-value {} {
    advance
    set val [read]
    make-constant $val
    return [::constcl::list #Q $val]
}
```


The `find-char` helper procedure reads past whitespace to find a given character.
Returns Tcl truth if it is found.

```
proc ::constcl::find-char {c} {
    set cp 0
    while {[::string is space [::string index $::inputbuffer $cp]]} {
        incr cp
    }
    return [expr {[::string index $::inputbuffer $cp] eq $c}]
}
```

The `read-pair-value` procedure reads values and returns a [Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) object.

```

proc ::constcl::read-pair {c} {
    skip-whitespace
    if {[find-char $c]} {
        return #NIL
    }
    set a [read]
    skip-whitespace
    set res $a
    set prev #NIL
    while {![find-char $c]} {
        set x [read]
        skip-whitespace
        if {[dot? $x] eq "#t"} {
            set prev [read]
            skip-whitespace
        } else {
            lappend res $x
        }
        if {[llength $res] > 99} break
    }
    foreach r [lreverse $res] {
        set prev [cons $r $prev]
    }
    return $prev
}

proc ::constcl::read-pair-value {char} {
    advance
    skip-whitespace
    set val [read-pair $char]
    skip-whitespace
    if {[first] ne $char} {
        if {$char eq ")"} {
            error "Missing right parenthesis (first=[first])."
        } else {
            error "Missing right bracket (first=[first])."
        }
    }
    advance
    return $val
}
```


`read-plus-minus` reacts to a plus or minus in the input buffer, and either
returns a `#+` or `#-` symbol, or a number.

```
proc ::constcl::read-plus-minus {} {
    if {![::string is digit [second]]} {
        if {[first] eq "+"} {
            advance
            return #+
        } else {
            advance
            return #-
        }
    } else {
        return [::constcl::read-number]
    }
}
```

`read-number` reads a number and returns a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.

```
proc ::constcl::read-number {} {
    while {$::inputbuffer ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append num [first]
        advance
    }
    if {[::string is double $num]} {
        return [MkNumber $num]
    } else {
        error "Invalid numeric constant $num"
    }
}
```


`read-identifier` reads an identifier value and returns a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.

```
proc ::constcl::read-identifier {} {
    ::append name [first]
    advance
    while {$::inputbuffer ne {} && ![::string is space [first]] && [first] ni {) \]}} {
        ::append name [first]
        advance
    }
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

`read-character` reads a character and returns a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.

```
proc ::constcl::read-character {} {
    set name "#"
    while {$::inputbuffer ne {} && ![::string is space [first]] && [first] ni {) ]}} {
        ::append name [first]
        advance
    }
    if {[::constcl::character-check $name]} {
        return [MkChar $name]
    } else {
        error "Invalid character constant $name"
    }
}
```


`read-vector` reads a vector value and returns a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.

```
proc ::constcl::read-vector {} {
    advance
    skip-whitespace
    set res {}
    while {$::inputbuffer ne {} && [first] ne ")"} {
        lappend res [read]
        skip-whitespace
    }
    set vec [MkVector $res]
    $vec mkconstant
    if {[first] ne ")"} {
        error "Missing right parenthesis (first=[first])."
    }
    advance
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


```
reg eval ::constcl::eval

proc ::constcl::eval {e {env ::constcl::global_env}} {
    # TODO
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
        while {[$op name] in {and case cond for for/and for/list for/or let or}} {
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
```

Variable reference, or _lookup_, is handled by the helper `lookup`. It searches the
environment chain for the symbol's name, and returns the value it is bound to.

```
proc ::constcl::lookup {sym env} {
    set name [$sym name]
    [$env find $name] get $name
}
```

The _conditional_ form evaluates a Lisp list of three expressions. The first, the _condition_,
is evaluated first. If it evaluates to anything other than `#f`, the second expression (the
_consequent_) is evaluated and the value returned. Otherwise, the third expression (the 
_alternate_) is evaluated and the value returned.

The `eprogn` helper procedure takes a Lisp list of expressions and evaluates them in
_sequence_, returning the value of the last one.

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

```
proc ::constcl::declare {sym val env} {
    set var [varcheck [idcheck [$sym name]]]
    $env set $var $val
    return #NONE
}
```

The `update!` helper modifies an existing variable that is bound somewhere in the 
environment chain. It finds the variable's environment and updates the binding. It
returns the expression, so calls to `set!` can be chained: `(set! foo (set! bar 99))`
sets both variables to 99.

```
proc ::constcl::update! {var expr env} {
    [$env find [$var name]] set [$var name] $expr
    set expr
}
```

`make-function` makes a [Procedure](https://github.com/hoodiecrow/ConsTcl#control)
object. First it needs to convert the Lisp list `body`. It is packed inside a `begin`
if it has more than one expression, and taken out of its list if not. The Lisp list
`formals` is passed on as is.

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

`invoke` _pr_ _vals_ where _pr_ is a procedure and _vals_ is a Lisp list of Lisp values. It 
arranges for a procedure to be called with each of the values in _vals. It checks if
`pr`really is a procedure, and determines whether to call `pr` as an object or as a Tcl command.

```
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
```

`splitlist` converts a Lisp list to a Tcl list with Lisp objects.

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

`evlis` successively evaluates the elements of a Lisp list and returns the results
as a Lisp list.

```
proc ::constcl::evlis {exps env} {
    if {[pair? $exps] eq "#t"} {
        return [cons [eval [car $exps] $env] [evlis [cdr $exps] $env]]
    } else {
        return #NIL
    }
}
```

### Macros

Macros that rewrite expressions into other, more concrete expressions is one of Lisp's strong
points. This interpreter does macro expansion, but the user can't define new macros--the ones
available are hardcoded in the code below.

```
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
    }
    set op [car $val]
    set args [cdr $val]
}
```

`expand-and` expands the `and` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.

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

proc ::constcl::do-and {exps prev} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return $prev
    } else {
        return [list #I [car $exps] [do-and [cdr $exps] [car $exps]] #f]
    }
}
```

The `case` macro is expanded by `do-case`. It returns `'()` if there are no clauses, 
and nested `if` constructs if there are some.

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

The `cond` macro is expanded by `do-cond`. It returns `'()` if there are no clauses, 
and nested `if` constructs if there are some.

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
```

The `expand-for/and` procedure expands the `for/and` macro. It returns an `and`
construct containing the iterations of the first clause (multiple clauses
isn't implemented yet).

```
proc ::constcl::expand-for/and {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "and"] {*}$res]
}
```

The `expand-for/list` procedure expands the `for/list` macro. It returns a `list`
construct containing the iterations of the first clause (multiple clauses
isn't implemented yet).

```
proc ::constcl::expand-for/list {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "list"] {*}$res]
}
```

The `expand-for/or` procedure expands the `for/or` macro. It returns an `or`
construct containing the iterations of the first clause (multiple clauses
isn't implemented yet).

```
proc ::constcl::expand-for/or {exps env} {
    set res [do-for $exps $env]
    return [list [MkSymbol "or"] {*}$res]
}
```

`expand-let` expands the named `let` and 'regular' `let` macros. They ultimately
expand to `lambda` constructs.

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
```

`expand-or` expands the `or` macro. It returns a `begin`-expression if the macro
has 0 or 1 elements, and a nested `if` construct otherwise.

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

proc ::constcl::do-or {exps} {
    if {[eq? [length $exps] #0] eq "#t"} {
        return #f
    } else {
        return [list #L [list [list #x [car $exps]]] [list #I #x #x [do-or [cdr $exps]]]]
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

```
reg write ::constcl::write

proc ::constcl::write {obj args} {
    if {$obj ne "#NONE"} {
        ::constcl::write-value $obj
        puts {}
    }
}
```

`write-value` simply calls an object's `write` method, letting the object
write itself.

```
proc ::constcl::write-value {obj} {
    $obj write
}
```

The `display` procedure is like `write` but doesn't print a newline.

```
reg display ::constcl::display

proc ::constcl::display {obj args} {
    ::constcl::write-value $obj
    flush stdout
}
```

The `write-pair` procedure prints a Pair object.

```
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
```


## Built-in procedures

### Equivalence predicates

Of the three equivalence predicates, `eq` generally tests for identity (with exceptions for numbers
and strings), `eqv` tests for value equality (except for booleans and procedures, where it tests for
identity), and `equal` tests for whether the output strings are equal.

```
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
```

```
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
```

```
reg equal? ::constcl::equal?

proc ::constcl::equal? {obj1 obj2} {
    if {[$obj1 show] eq [$obj2 show]} {
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
        if {[::string is double $v]} {
            set value $v
        } else {
            error "NUMBER expected\n$v"
        }
    }
    method positive {} {expr {$value > 0}}
    method negative {} {expr {$value < 0}}
    method even {} {expr {$value % 2 == 0}}
    method odd {} {expr {$value % 2 == 1}}
    method incr {val} {incr value $val}
    method mult {val} {set value [expr {$value * $val}]}
    method decr {val} {incr value -$val}
    method div {val} {set value [expr {$value / $val}]}
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
```
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
```


The operators `=`, `<`, `>`, `<=`, and `>=` are implemented.

```
reg = ::constcl::=

proc ::constcl::= {args} {
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(= num...)"
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
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(< num...)"
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
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(> num...)"
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
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(<= num...)"
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
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(>= num...)"
    }
    if {[::tcl::mathop::>= {*}$vals]} {
        return #t
    } else {
        return #f
    }
}
```


The `zero?` predicate tests if a given number is equal to zero.

```
reg zero? ::constcl::zero?

proc ::constcl::zero? {obj} {
    if {[number? $obj] eq "#t"} {
        if {[$obj numval] == 0} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(zero? [$obj show])"
    }
}
```


The `positive?`/`negative?`/`even?`/`odd?` predicates test a number
for those traits.

```
reg positive? ::constcl::positive?

proc ::constcl::positive? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj positive]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(positive? [$obj show])"
    }
}
```


```
reg negative? ::constcl::negative?

proc ::constcl::negative? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj negative]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(negative? [$obj show])"
    }
}
```


```
reg even? ::constcl::even?

proc ::constcl::even? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj even]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(even? [$obj show])"
    }
}
```


```
reg odd? ::constcl::odd?

proc ::constcl::odd? {obj} {
    if {[::constcl::number? $obj] eq "#t"} {
        if {[$obj odd]} {
            return #t
        } else {
            return #f
        }
    } else {
        error "NUMBER expected\n(odd? [$obj show])"
    }
}
```


The `max` function selects the largest number, and the `min` function
selects the smallest number.

```
reg max ::constcl::max

proc ::constcl::max {args} {
    # TODO type-check
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
    # TODO type-check
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(min num...)"
    }
    MkNumber [::tcl::mathfunc::min {*}$vals]
}
```


The operators `+`, `*`, `-`, and `/` stand for the respective
mathematical operations.

```
reg + ::constcl::+

proc ::constcl::+ {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(+ num...)"
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
        error "NUMBER expected\n(* num...)"
    }
    MkNumber [::tcl::mathop::* {*}$vals]
}
```


```
reg - ::constcl::-

proc ::constcl::- {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(- num...)"
    }
    MkNumber [::tcl::mathop::- {*}$vals]
}
```


```
reg / ::constcl::/

proc ::constcl::/ {args} {
    try {
        set vals [lmap arg $args {$arg numval}]
    } on error {} {
        error "NUMBER expected\n(/ num...)"
    }
    MkNumber [::tcl::mathop::/ {*}$vals]
}
```


The `abs` function yields the absolute value of a number.

```
reg abs ::constcl::abs

proc ::constcl::abs {x} {
    if {[::constcl::number? $x] eq "#t"} {
        if {[$x negative]} {
            return [MkNumber [expr {[$x numval] * -1}]]
        } else {
            return $x
        }
    } else {
        error "NUMBER expected\n(abs [$x show])"
    }
}
```


```
proc ::constcl::quotient {n1 n2} {
    # TODO
}
```

```
proc ::constcl::remainder {n1 n2} {
    # TODO
}
```

```
proc ::constcl::modulo {n1 n2} {
    # TODO
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

```
reg floor ::constcl::floor

proc ::constcl::floor {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::floor [$x numval]]
    } else {
        error "NUMBER expected\n(floor [$x show])"
    }
}
```


```
reg ceiling ::constcl::ceiling

proc ::constcl::ceiling {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::ceil [$x numval]]
    } else {
        error "NUMBER expected\n(ceiling [$x show])"
    }
}
```


```
reg truncate ::constcl::truncate

proc ::constcl::truncate {x} {
    if {[::constcl::number? $x] eq "#t"} {
        if {[$x negative]} {
            MkNumber [::tcl::mathfunc::ceil [$x numval]]
        } else {
            MkNumber [::tcl::mathfunc::floor [$x numval]]
        }
    } else {
        error "NUMBER expected\n(truncate [$x show])"
    }
}
```


```
reg round ::constcl::round

proc ::constcl::round {x} {
    if {[::constcl::number? $x] eq "#t"} {
        MkNumber [::tcl::mathfunc::round [$x numval]]
    } else {
        error "NUMBER expected\n(round [$x show])"
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

```
reg exp ::constcl::exp

proc ::constcl::exp {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::exp [$z numval]]
    } else {
        error "NUMBER expected\n(exp [$z show])"
    }
}
```


```
reg log ::constcl::log

proc ::constcl::log {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::log [$z numval]]
    } else {
        error "NUMBER expected\n(log [$z show])"
    }
}
```


```
reg sin ::constcl::sin

proc ::constcl::sin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sin [$z numval]]
    } else {
        error "NUMBER expected\n(sin [$z show])"
    }
}
```

```
reg cos ::constcl::cos

proc ::constcl::cos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::cos [$z numval]]
    } else {
        error "NUMBER expected\n(cos [$z show])"
    }
}
```

```
reg tan ::constcl::tan

proc ::constcl::tan {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::tan [$z numval]]
    } else {
        error "NUMBER expected\n(tan [$z show])"
    }
}
```


```
reg asin ::constcl::asin

proc ::constcl::asin {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::asin [$z numval]]
    } else {
        error "NUMBER expected\n(asin [$z show])"
    }
}
```

```
reg acos ::constcl::acos

proc ::constcl::acos {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::acos [$z numval]]
    } else {
        error "NUMBER expected\n(acos [$z show])"
    }
}
```

```
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
```


`sqrt` calculates the square root.

```
reg sqrt ::constcl::sqrt

proc ::constcl::sqrt {z} {
    if {[::constcl::number? $z] eq "#t"} {
        MkNumber [::tcl::mathfunc::sqrt [$z numval]]
    } else {
        error "NUMBER expected\n(sqrt [$z show])"
    }
}
```


`expt` calculates the _x_ to the power of _y_.

```
reg expt ::constcl::expt

proc ::constcl::expt {z1 z2} {
    if {[::constcl::number? $z1] eq "#t" && [::constcl::number? $z2] eq "#t"} {
        MkNumber [::tcl::mathfunc::pow [$z1 numval] [$z2 numval]]
    } else {
        error "NUMBER expected\n(expt [$z1 show] [$z2 show])"
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

```
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
```


```
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

```
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
```


The only operation on booleans: `not`, or logical negation.

```
reg not ::constcl::not

proc ::constcl::not {obj} {
    if {[$obj bvalue] eq "#f"} {
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
```

`char?` recognizes Char objects by type.

```
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
```


`char=?`, `char<?`, `char>?`, `char<=?`, and `char>=?` compare character
values. They only compare two characters at a time.

```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


`char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, and `char-ci>=?` compare character
values in a case insensitive manner. They only compare two characters at a time.

```
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
```


```
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
```


```
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
```


```
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
```


```
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
```


The predicates `char-alphabetic`, `char-numeric`, `char-whitespace`,
`char-upper-case`, and `char-lower-case` test a character for these
conditions.

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


```
proc ::constcl::char->integer {char} {
    # TODO
}
```

```
proc ::constcl::integer->char {n} {
    # TODO
}
```

`char-upcase` and `char-downcase` alter the case of a character.

```
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
```



```
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
```



### Control

This section concerns itself with procedures and the application of the same.

```
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
```


`apply` applies a procedure to a Tcl list of Lisp arguments.

```
reg apply ::constcl::apply

proc ::constcl::apply {proc args} {
    if {[procedure? $proc] eq "#t"} {
        invoke $proc $args 
    } else {
        error "PROCEDURE expected\n(apply [$proc show] ...)"
    }
}
```


`map` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The Lisp list of the results of the invocations is 
returned.

```
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
```


`for-each` iterates over one or more lists, taking an element from each list to pass to
a procedure as an argument. The empty list is returned.

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

```
if no {
    # defined in read.tcl
proc ::constcl::read {args} {
    # TODO
}
}
```

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

```
if no {
proc ::constcl::write {obj args} {
    # TODO write [$obj write]
}
}
```

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
    method bvalue {} {return #t}
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
```

Helper procedure to make a string representation of a list.

```
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
```


`cons` adds a pair to a list.

```
reg cons ::constcl::cons

proc ::constcl::cons {car cdr} {
    MkPair $car $cdr
}
```


`car` gets the contents of the first cell in a pair.

```
reg car ::constcl::car

proc ::constcl::car {obj} {
    $obj car
}
```


`cdr` gets the contents of the second cell in a pair.

```
reg cdr ::constcl::cdr

proc ::constcl::cdr {obj} {
    $obj cdr
}
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

`set-car!` sets the contents of the first cell in a pair.

```
reg set-car! ::constcl::set-car!

proc ::constcl::set-car! {obj val} {
    $obj set-car! $val
}
```


`set-cdr!` sets the contents of the second cell in a pair.

```
reg set-cdr! ::constcl::set-cdr!

proc ::constcl::set-cdr! {obj val} {
    $obj set-cdr! $val
}
```


The `list?` predicate tests if a pair is part of a proper list, one that
ends with NIL.

```
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
```


`list` constructs a Lisp list from a Tcl list of items.

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

```
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
```


`append` joins lists together.

```
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
```


`reverse` produces a reversed copy of a Lisp list.

```
reg reverse ::constcl::reverse

proc ::constcl::reverse {obj} {
    list {*}[lreverse [splitlist $obj]]
}
```


Given a list index, `list-tail` yields the sublist starting from that index.

```
reg list-tail ::constcl::list-tail

proc ::constcl::list-tail {obj k} {
    if {[zero? $k] eq "#t"} {
        return $obj
    } else {
        list-tail [cdr $obj] [- $k #1]
    }
}
```


`list-ref` yields the list item at a given index.

```
reg list-ref ::constcl::list-ref

proc ::constcl::list-ref {obj k} {
    car [list-tail $obj $k]
}
```


`memq`, `memv`, and `member` return the sublist starting with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison.

```
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
```


```
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
```

```
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
```

`assq`, `assv`, and `assoc` return the associative item marked with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison.

```
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
```


```
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
```

```
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

proc ::constcl::make-string {args} {
    if {[llength $args] == 1} {
        lassign $args k
        return [MkString [::string repeat " " [$k value]]]
    } else {
        lassign $args k c
        return [MkString [::string repeat [$c char] [$k value]]]
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
            error "CHAR expected\n(string [$char show])"
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
            set i [$k value]
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
```

```
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

Vectors are heterogenous structures whose elements are indexed by integers. They are implemented
as Tcl lists of Lisp values.

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

proc ::constcl::make-vector {args} {
    if {[llength $args] == 1} {
        lassign $args k
        set fill #NIL
    } else {
        lassign $args k fill
    }
    MkVector [lrepeat [$k value] $fill]
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
            return [$vec ref [$k value]]
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
            return [$vec set! [$k value] $obj]
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

The class for environments is called __Environment__. It is mostly a wrapper around a dictionary,
with the added finesse of keeping a link to the outer environment (starting a chain that goes all
the way to the global environment and then stops at the null environment) which can be traversed
by the find method to find which innermost environment a given symbol is bound in.

```
catch { ::constcl::Environment destroy }

oo::class create ::constcl::Environment {
    variable bindings outer_env
    constructor {syms vals {outer {}}} {
        set bindings [dict create]
        if {$syms eq "#NIL"} {
            if {[llength $vals]} { error "too many arguments" }
        } elseif {[::constcl::list? $syms] eq "#t"} {
            set syms [lmap sym [::constcl::splitlist $syms] {$sym name}]
            foreach sym $syms val $vals {
                my set $sym $val
            }
        } elseif {[::constcl::symbol? $syms] eq "#t"} {
            my set [$syms name] [::constcl::list {*}$vals]
        } else {
            while {[::constcl::null? $syms] ne "#t"} {
                if {[::constcl::symbol? [::constcl::cdr $syms]] eq "#t"} {
                    my set [[::constcl::car $syms] name] [lindex $vals 0]
                    set vals [lrange $vals 1 end]
                    my set [[::constcl::cdr $syms] name] [::constcl::list {*}$vals]
                    set vals {}
                    break
                } else {
                    my set [[::constcl::car $syms] name] [lindex $vals 0]
                    set vals [lrange $vals 1 end]
                    set syms [::constcl::cdr $syms]
                }
                #if {[llength $vals] < 1} { error "too few arguments" }
            }
            if {[llength $vals] > 0} { error "too many arguments $vals" }
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


On startup, two __Environment__ objects called __null_env__ (the null environment, not the same
as __null-environment__ in Scheme) and __global_env__ (the global environment) are created. 

Make __null_env__ empty and unresponsive: this is where searches for unbound symbols end up.

```
::constcl::Environment create ::constcl::null_env #NIL {}

oo::objdefine ::constcl::null_env {
    method find {sym} {self}
    method get {sym} {error "Unbound variable: $sym"}
    method set {sym val} {error "Unbound variable: $sym"}
}
```

Meanwhile, __global_env__ is populated with all the definitions from the definitions register,
__defreg__. This is where top level evaluation happens.

```
namespace eval ::constcl {
    set keys [list {*}[lmap k [dict keys $defreg] {MkSymbol $k}]]
    set vals [dict values $defreg]
    Environment create global_env $keys $vals ::constcl::null_env
}
```

Thereafter, each time a user-defined procedure is called, a new __Environment__ object is
created to hold the bindings introduced by the call, and also a link to the outer environment
(the one closed over when the procedure was defined).

#### Lexical scoping

A procedure definition form creates a new procedure. Example:

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

In the first image, we see the global environment before we call __circle-area__
(and also the empty null environment which the global environment links to):

![A global environment](/images/env1.png)

During the call. Note how the global `r` is shadowed by the local one, and how
the local environment links to the global one to find `*` and `pi`. 

![A local environment shadows the global](/images/env2.png)

After the call, we are back to the first state again.

![A global environment](/images/env1.png)




