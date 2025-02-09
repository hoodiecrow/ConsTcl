
MD(
## read

`read` represents the interpreter's input facility. Currently input is faked with input
strings.
MD)

MD(
**IB** class

A quick-and-dirty input simulator, using an input buffer object to hold
characters to be read. The `fill` method fills the buffer and sets the first
character in the peek position.  The `advance` method consumes one character
from the buffer. `first` peeks at the next character to be read. `skip-ws`
advances past whitespace and comments.  `unget` backs up one position and sets a
given character in the peek position. The `find` method looks past whitespace
and comments to find a given character. It returns Tcl truth if it is found.  Or
it gets the hose again.

MD)

CB
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

CB

MD(

The parsing procedure translates an expression from external representation to
internal representation. The external representation is a 'recipe' for an
expression that expresses it in a unique way. For example, the external
representation for a vector is a sharp sign (#), a left parenthesis ((), the
external representation for some values, and a right parenthesis ()). The parser
takes in the input buffer character by character, matching each character
against a fitting external representation. When done, it creates an object,
which is the internal representation of an expression.  The object can then be
passed to the evaluator.

MD)

MD(
### parse

**parse**

Given a string, `parse` fills the input buffer. It then parses the input and
produces an expression.

Example:

```
% ::constcl::parse "(+ 2 3)"
::oo::Obj491
```

Here, `parse` parsed the external representation of a list with three elements,
+, 2, and 3. It produced the expression that has the internal representation
`::oo::Obj491`. We will later meet procedures like `eval`, which transforms an
expression into a value, and `write`, which prints a printed representation of
expressions and values. Putting them together: we can see

```
% ::constcl::write ::oo::Obj491
(+ 2 3)
% ::constcl::write [::constcl::eval ::oo::Obj491]
5
```

Fortunately, we don't have to work at such a low level. We can use the `repl`
instead:

```
ConsTcl> (+ 2 3)
5
```

Then, parsing and evaluation and writing goes on in the background and the
internal representations of expressions and values are hidden.

Anyway, here is how it really looks like. `::oo::Obj491` was just the head of
the list.

![intreplist](/images/intreplist.png)

MD)

PR(
parse (public);inp strinpbuf -> expr
PR)

CB
reg parse

proc ::constcl::parse {inp} {
    ::if {[info object isa object $inp]} {
        set ib $inp
    } else {
        set ib [IB new $inp]
    }
    return [parse-expression]
}
CB

MD(
**parse-expression**

The procedure `parse-expression` parses input by peeking at the first available
character and delegating to one of the more detailed parsing procedures based on
that, producing an expression of any kind.
MD)

PR(
parse-expression (internal);-> expr
PR)

CB
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
CB

MD(
**parse-string-expression**

`parse-string-expression` parses input starting with a double quote and collects
characters until it reaches another (unescaped) double quote. It then returns a
string expression (a [String](https://github.com/hoodiecrow/ConsTcl#strings) object).
MD)

PR(
parse-string-expression (internal);-> str
PR)

CB
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
CB

TT(

::tcltest::test parse-4.0 {try reading a string} {
    set expr [::constcl::parse {"foo bar"}]
    $expr value
} "foo bar"

::tcltest::test parse-4.1 {try reading a string} {
    set expr [::constcl::parse {"\"foo\" \\ bar"}]
    $expr value
} {"foo" \ bar}

TT)

MD(
**parse-sharp**

`parse-sharp` parses input starting with a sharp sign (#) and produces the various kinds of
expressions whose external representation begins with a sharp sign.
MD)

PR(
parse-sharp (internal);-> sharp
PR)

CB
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
CB

MD(
**make-constant**

The `make-constant` helper procedure is called to set components of expressions to
constants when read as a quoted literal.
MD)

CB
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
CB

MD(
**parse-quoted-expression**

`parse-quoted-expression` parses input starting with a "'", and then parses an entire
expression beyond that, returning it wrapped in a list with `quote`.
MD)

PR(
parse-quoted-expression (internal);-> quote
PR)

CB
proc ::constcl::parse-quoted-expression {} {
    upvar ib ib
    $ib advance
    set expr [parse-expression]
    $ib skip-ws
    make-constant $expr
    return [list #Q $expr]
}
CB

TT(

::tcltest::test parse-1.0 {try reading quoted symbol} -body {
    pp "'foo"
} -output "(quote foo)\n"

TT)

MD(
**parse-pair-expression**

The `parse-pair-expression` procedure parses input and produces a structure of
[Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists)s expression.
MD)

PR(
parse-pair-expression (internal);char pterm -> pstr
PR)

CB

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
CB

TT(
::tcltest::test parse-6.0 {try reading an improper list} -body {
    pp "(a . b)"
} -output "(a . b)\n"

::tcltest::test parse-6.1 {try reading an improper list} -body {
    pp "(a b . c)"
} -output "(a b . c)\n"

::tcltest::test parse-1.1 {try reading a list} -body {
    namespace eval ::constcl {
        set expr [::constcl::parse "(a (b))"]
        [caadr $expr] name
    }
} -result "b"

::tcltest::test parse-1.2 {try reading a list} -body {
    pp "(a)"
} -output "(a)\n"

::tcltest::test parse-1.3 {try reading a list} -body {
    pp "(a b)"
} -output "(a b)\n"

::tcltest::test parse-1.4 {try reading a list} -body {
    pp "(a b c)"
} -output "(a b c)\n"

::tcltest::test parse-1.5 {try reading a list} -body {
    pp "(a b c d)"
} -output "(a b c d)\n"

::tcltest::test parse-1.6 {try reading a list} -body {
    pp "(a b c d e)"
} -output "(a b c d e)\n"

::tcltest::test parse-1.7 {try reading a list} -body {
    pp "(a (b) )"
} -output "(a (b))\n"

::tcltest::test parse-1.8 {try reading a list} -body {
    pp "(a (b))"
} -output "(a (b))\n"

TT)

MD(
**parse-plus-minus**

`parse-plus-minus` reacts to a plus or minus in the input buffer, and either
returns a `+` or `-` symbol, or a number.
MD)

PR(
parse-plus-minus (internal);-> pm
PR)

CB
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
CB

MD(
**parse-unquoted-expression**

`parse-unquoted-expression` parses input, producing an expression and returning
it wrapped in `unquote`, or in `unquote-splicing` if an @-sign is present in
the input stream.
MD)

PR(
parse-unquoted-expression (internal);-> unquote
PR)

CB
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
CB

TT(

::tcltest::test parse-1.9 {try reading unquoted symbol} -body {
    pp ",foo"
} -output "(unquote foo)\n"

TT)

MD(
**parse-quasiquoted-expression**

`parse-quasiquoted-expression` parses input, producing an expression and returning it wrapped in `quasiquote`.
MD)

PR(
parse-quasiquoted-expression (internal);-> qquote
PR)

CB
proc ::constcl::parse-quasiquoted-expression {} {
    upvar ib ib
    $ib advance
    set expr [parse-expression]
    $ib skip-ws
    make-constant $expr
    return [list [MkSymbol "quasiquote"] $expr]
}
CB

TT(

::tcltest::test parse-1.10 {try reading unquoted symbol} -body {
    pp "`(list 1 2 ,@foo)"
} -output "(quasiquote (list 1 2 (unquote-splicing foo)))\n"

TT)

MD(
**interspace**

The `interspace` helper procedure recognizes whitespace or comments between
value representations.
MD)

CB
proc ::constcl::interspace {c} {
    # don't add #EOF: parse-* uses this one too
    ::if {$c eq {} || [::string is space -strict $c] || $c eq ";"} {
        return #t
    } else {
        return #f
    }
}
CB

MD(
**parse-number-expression**

`parse-number-expression` parses input, producing a number and returning a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

PR(
parse-number-expression (internal);-> num
PR)

CB
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
CB

TT(
::tcltest::test parse-2.0 {try reading a number} {
    set obj [::constcl::parse "99.99"]
    $obj value
} "99.99"

::tcltest::test parse-2.1 {try reading a number} {
    set obj [::constcl::parse "     99.99"]
    $obj value
} "99.99"

::tcltest::test parse-2.2 {try reading a number} {
    set obj [::constcl::parse "     9"]
    $obj value
} "9"

::tcltest::test parse-2.3 {try reading a number} {
    set obj [::constcl::parse "     +9"]
    $obj value
} "+9"

::tcltest::test parse-2.4 {try reading a number} {
    set obj [::constcl::parse "     -9"]
    $obj value
} "-9"

::tcltest::test parse-2.5 {try reading a number} {
    set obj [::constcl::parse "     - "]
    $obj name
} "-"

::tcltest::test parse-2.6 {try reading a number} {
    set obj [::constcl::parse "     + "]
    $obj name
} "+"

TT)

MD(
**parse-identifier-expression**

`parse-identifier-expression` parses input, producing an identifier expression and returning a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

PR(
parse-identifier-expression (internal);-> sym
PR)

CB
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
CB

TT(

::tcltest::test parse-5.0 {try reading an identifier} {
    set expr [::constcl::parse [::constcl::IB new "foo"]]
    $expr name
} "foo"

::tcltest::test parse-5.1 {try reading an identifier} -body {
    set ib [::constcl::IB new "+foo"]
    set expr [::constcl::parse-identifier-expression]
    $expr name
} -returnCodes error -result "Identifier expected (+foo)"

::tcltest::test parse-5.2 {try reading an identifier} -body {
    ::constcl::IB create ib-read-5.2 "let"
    set expr [::constcl::parse ib-read-5.2]
    ::constcl::varcheck [$expr name]
} -returnCodes error -result "Macro name can't be used as a variable: let"

TT)

MD(
**character-check**

The `character-check` helper procedure compares a potential
character constant to the valid kinds. Returns Tcl truth (1/0).
MD)

CB
proc ::constcl::character-check {name} {
    ::if {[regexp -nocase {^#\\([[:graph:]]|space|newline)$} $name]} {
        return #t
    } else {
        return #f
    }
}
CB

MD(
**parse-character-expression**

`parse-character-expression` parses input, producing a character and returning
a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

PR(
parse-character-expression (internal);-> char
PR)

CB
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
CB

TT(

::tcltest::test parse-3.0 {try reading a character} {
    set expr [::constcl::parse [::constcl::IB new {#\A}]]
    $expr char
} "A"

::tcltest::test parse-3.1 {try reading a character} {
    set expr [::constcl::parse [::constcl::IB new "#\\space"]]
    $expr char
} " "

::tcltest::test parse-3.2 {try reading a character} {
    set expr [::constcl::parse [::constcl::IB new "#\\newline"]]
    $expr char
} "\n"

::tcltest::test parse-3.3 {try reading a character} -body {
    set expr [::constcl::parse [::constcl::IB new "#\\foobar"]]
    $expr char
} -returnCodes error -result "Invalid character constant #\\foobar"

TT)

MD(
**parse-vector-expression**

`parse-vector-expression` parses input, producing a vector expression and returning a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

PR(
parse-vector-expression (internal);-> vec
PR)

CB
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
CB

TT(

::tcltest::test parse-6.0 {try reading a vector} -body {
    pp "#(1 2 3)"
} -output "#(1 2 3)\n"

TT)

MD(
### read

**read**

The standard builtin `read` reads and parses input into a Lisp expression in a
similar manner to how `parse` parses a string buffer.
MD)

PR(
read (public);?port? port -> expr
PR)

CB
reg read ::constcl::read

set ::constcl::global_unget {}

proc ::constcl::read {args} {
    set c {}
    set unget $::constcl::global_unget
    set ::constcl::global_unget {}
    ::if {[llength $args]} {
        lassign $args port
    } else {
        set port $::constcl::Input_port
    }
    set oldport $::constcl::Input_port
    set ::constcl::Input_port $port
    set expr [read-expression]
    set ::constcl::Input_port $oldport
    ::if {$unget ne "#EOF"} {
        set ::constcl::global_unget $unget
    }
    return $expr
}
CB

MD(
**read-expression**

The procedure `read-expression` parses input by reading the first available
character and delegating to one of the more detailed reading procedures based on
that, producing an expression of any kind. A Tcl character value can be passed
to it, that character will be used first before reading from the input stream.
MD)

PR(
read-expression (internal);?char? tchar -> expr
PR)

CB
proc ::constcl::read-expression {args} {
    upvar c c unget unget
    ::if {[llength $args]} {
        lassign $args c
    } else {
        set c [readc]
    }
    read-eof $c
    ::if {[::string is space $c] || $c eq ";"} {
        set c [skip-ws $c]
        read-eof $c
    }
    switch -regexp $c {
        {^$}          { return #NONE}
        {\"}          { set n [read-string-expression]       ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\#}          { set n [read-sharp]                   ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\'}          { set n [read-quoted-expression]       ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\(}          { set n [read-pair-expression ")"]     ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\+} - {\-}   { set n [read-plus-minus $c]           ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\,}          { set n [read-unquoted-expression]     ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\.}          { set n [Dot new]; set c [readc]       ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\[}          { set n [read-pair-expression "\]"]    ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\`}          { set n [read-quasiquoted-expression]  ; read-eof $n; set unget [skip-ws $c]; return $n }
        {\d}          { set n [read-number-expression $c]    ; read-eof $n; set unget [skip-ws $c]; return $n }
        {[[:graph:]]} { set n [read-identifier-expression $c]; read-eof $n; set unget [skip-ws $c]; return $n }
        default {
            read-eof $c
            ::error "unexpected character ($c)"
        }
    }
}
CB

TT(
::tcltest::test read-1.0 {try read-expression on a string} -setup {
    ::tcltest::makeFile {"foo bar"  } testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "\"foo bar\"\n"

::tcltest::test read-1.1 {try read-expression on a string/eof} -setup {
    ::tcltest::makeFile {"foo } testrr.lsp ; #"
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -returnCodes error -result {malformed string (no ending double quote)}

::tcltest::test read-1.2 {try read-expression on a couple of vectors} -setup {
    ::tcltest::makeFile {  #(1 2 3)  #(11 22 33)} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#(1 2 3)\n#(11 22 33)\n"

::tcltest::test read-1.3 {try read-expression on booleans} -setup {
    ::tcltest::makeFile {  #t  #f} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#t\n#f\n"

::tcltest::test read-1.4 {try read-expression on characters} -setup {
    ::tcltest::makeFile {  #\A  #\space} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#\\A\n#\\space\n"

::tcltest::test read-1.5 {try read-expression on quoted expr} -setup {
    ::tcltest::makeFile {  'foo } testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(quote foo)\n"

::tcltest::test read-1.6 {try read-expression on pair expr} -setup {
    ::tcltest::makeFile {  (a b c)  ((a b) c)} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(a b c)\n"

::tcltest::test read-1.7 {try read-expression on pair expr} -setup {
    ::tcltest::makeFile {  ([d e] f)} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "((d e) f)\n"

::tcltest::test read-1.8 {try read-expression on pair expr} -setup {
    ::tcltest::makeFile {  (def ghi (jkl mno))} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(def ghi (jkl mno))\n"

::tcltest::test read-1.9 {try read-expression on plus/minus} -setup {
    ::tcltest::makeFile {  +  -  -99} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "+\n-\n-99\n"

::tcltest::test read-1.10 {try read-expression on unquoted expr} -setup {
    ::tcltest::makeFile {  ,foo ,@bar} testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(unquote foo)\n(unquote-splicing bar)\n"

::tcltest::test read-1.11 {try read-expression on dot expr} -setup {
    ::tcltest::makeFile {  a . b } testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "a\n.\nb\n"

::tcltest::test read-1.12 {try read-expression on quasiquoted expr} -setup {
    ::tcltest::makeFile {  `(a b) } testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(quasiquote (a b))\n"

::tcltest::test read-1.13 {try read-expression on numeric expr} -setup {
    ::tcltest::makeFile {  99 } testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "99\n"

::tcltest::test read-1.14 {try read-expression on identifiers} -setup {
    ::tcltest::makeFile {  foo    bar } testrr.lsp
    set p [::constcl::open-input-file testrr.lsp]
} -body {
    ::constcl::write [::constcl::read $p]
    ::constcl::write [::constcl::read $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "foo\nbar\n"
TT)

MD(
`readc` reads one character either from the unget store or from the input
stream. If the input stream is at end-of-file, an eof object is returned.
MD)

PR(
readc (internal);-> tchareof
PR)

CB
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
CB

MD(
`read-find` reads ahead through whitespace to find a given character. Returns 1
if it has found the character, and 0 if it has stopped at some other character.
MD)

PR(
read-find (internal);char tchar -> tbool
PR)

CB
proc read-find {char} {
    upvar c c unget unget
    while {[::string is space -strict $c]} {
        set c [readc]
        read-eof $c
    }
    set unget $c
    return [expr {$c eq $char}]
}
CB

MD(
`skip-ws` skips whitespace and comments (the ; to end of line kind). Expects a
character argument to start the skipping with.
MD)

PR(
skip-ws (internal);char tchar -> tchareof
PR)

CB
proc skip-ws {char} {
    upvar unget unget
    read-eof $char
    while true {
        switch -regexp $char {
            {[[:space:]]} {
                set char [readc]
            }
            {;} {
                while {$char ne "\n" && $char ne "#EOF"}  {
                    set char [readc]
                }
            }
            default {
                read-eof $char
                #set unget $char
                return $char
            }
        }
    }
}
CB

MD(
`read-eof` checks a number of characters for possible end-of-file objects. If it
finds one, it returns _from its caller_ with the EOF value.
MD)

PR(
read-eof (internal);args chars
PR)

CB
proc read-eof {args} {
    foreach val $args {
        ::if {$val eq "#EOF"} {
            return -level 1 -code return #EOF
        }
    }
}
CB

MD(
**read-string-expression**

`read-string-expression` parses input starting with a double quote and collects
characters until it reaches another (unescaped) double quote. It then returns a
string expression (an immutable
[String](https://github.com/hoodiecrow/ConsTcl#strings) object).
MD)

PR(
read-string-expression (internal);-> str
PR)

CB
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
CB

MD(
**read-sharp**

`read-sharp` parses input starting with a sharp sign (#) and produces the various kinds of
expressions whose external representation begins with a sharp sign.
MD)

PR(
read-sharp (internal);-> sharp
PR)

CB
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
CB

MD(
**read-vector-expression**

`read-vector-expression` parses input, producing a vector expression and returning a [Vector](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

PR(
read-vector-expression (internal);-> expr
PR)

CB
proc ::constcl::read-vector-expression {} {
    upvar c c unget unget
    set res {}
    set c [readc]
    while {$c ne "#EOF" && $c ne ")"} {
        lappend res [read-expression $c]
        set c [skip-ws $c]
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
CB

MD(
**read-character-expression**

`read-character-expression` parses input, producing a character and returning
a [Char](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

PR(
read-character-expression (internal);-> char
PR)

CB
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
CB

MD(
**read-quoted-expression**

`read-quoted-expression` parses input starting with a "'", and then parses an entire
expression beyond that, returning it wrapped in a list with `quote`.
MD)

PR(
read-quoted-expression (internal);-> quote
PR)

CB
proc ::constcl::read-quoted-expression {} {
    upvar c c unget unget
    set expr [read-expression]
    read-eof $expr
    make-constant $expr
    return [list #Q $expr]
}
CB

MD(
**read-pair-expression**

The `read-pair-expression` procedure parses input and produces a structure of
[Pair](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists)s expression.
MD)

PR(
read-pair-expression (internal);char pterm -> pstr
PR)

CB
proc ::constcl::read-pair-expression {char} {
    upvar c c unget unget
    set expr [read-pair $char]
    set c [skip-ws $c]
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
    set a [read-expression]
    set res $a
    set c [skip-ws $c]
    set prev #NIL
    while {![read-find $char]} {
        set x [read-expression]
        set c [skip-ws $c]
        read-eof $c
        ::if {[dot? $x] ne "#f"} {
            set prev [read-expression]
            set c [skip-ws $c]
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
CB

MD(
**read-plus-minus**

`read-plus-minus` reacts to a plus or minus in the input stream, and either
returns a `+` or `-` symbol, or a number.
MD)

PR(
read-plus-minus (internal);-> pm
PR)

CB
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
CB

MD(
**read-number-expression**

`read-number-expression` parses input, producing a number and returning a [Number](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

PR(
read-number-expression (internal);?char? tchar -> num
PR)

CB
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
CB

MD(
**read-unquoted-expression**

`read-unquoted-expression` parses input, producing an expression and returning
it wrapped in `unquote`, or in `unquote-splicing` if an @-sign is present in
the input stream.
MD)

PR(
read-unquoted-expression (internal);-> unquote
PR)

CB
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
CB

MD(
**read-quasiquoted-expression**

`read-quasiquoted-expression` parses input, producing an expression and returning it wrapped in `quasiquote`.
MD)

PR(
read-quasiquoted-expression (internal);-> qquote
PR)

CB
proc ::constcl::read-quasiquoted-expression {} {
    upvar c c unget unget
    set expr [read-expression]
    set c [skip-ws $c]
    read-eof $expr
    make-constant $expr
    return [list [MkSymbol "quasiquote"] $expr]
}
CB

MD(
**read-identifier-expression**

`read-identifier-expression` parses input, producing an identifier expression and returning a [Symbol](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

PR(
read-identifier-expression (internal);?char? tchar -> sym
PR)

CB
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
CB

# vim: ft=tcl tw=80
