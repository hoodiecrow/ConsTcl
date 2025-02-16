
MD(
## Input

The first thing an interpreter must be able to do is to take in the user's code
and data input, whether from the keyboard or from a source file.  `read`
represents the interpreter's main input facility. As a complement, a similar set
of procedures that read input from an input buffer exists (the `parse-`
procedures). The main set (the `read-` procedures) read from standard input,
or--if a port is provided--from the port's channel.
MD)

MD(
### The IB class

A quick-and-dirty input simulator, using an input buffer object to hold
characters to be parsed by the `parse-` procedures.
MD)

CB
catch { ::constcl::IB destroy }

oo::class create ::constcl::IB {
  variable peekc buffer
  constructor {str} {
    set peekc {}
    my fill $str
  }
}
CB

MD(
The `fill` method fills the buffer and sets the first character in the peek position.  
MD)

CB
oo::define ::constcl::IB method fill {str} {
  set buffer $str
  my advance
}
CB

MD(
The `advance` method consumes one character from the buffer. 
MD)

CB
oo::define ::constcl::IB method advance {} {
  if {$buffer eq {}} {
    set peekc {}
  } else {
    set peekc [::string index $buffer 0]
    set buffer [::string range $buffer 1 end]
  }
}
CB

MD(
`peek` peeks at the next character to be read. 
MD)

CB
oo::define ::constcl::IB method peek {} {
  return $peekc
}
CB

MD(
`unget` backs up one position and sets a given character in the peek position. 
MD)

CB
oo::define ::constcl::IB method unget {char} {
  set buffer $peekc$buffer
  set peekc $char
}
CB

MD(
The `find` method looks past whitespace to find a given character. It returns
Tcl truth if it is found.  Or it gets the hose again. 
MD)

CB
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
CB

MD(
`skip-ws` skips past whitespace and comments.  
MD)

CB
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
CB

MD(
### Parsing

#### The parsing process

Parsing[#](https://en.wikipedia.org/wiki/Parsing), or syntactic analysis, is
analyzing a sequence of letters, digits, and other characters, conforming to the
rules of **external representation**. The result of parsing is an **expression**
in internal form.

The parsing process translates an expression from external representation to
internal representation. The external representation is a 'recipe' for an
expression that expresses it in a unique way. 

For example, the external representation for a vector is a sharp sign (#), a
left parenthesis ((), the external representation for some values, and a right
parenthesis ()). When the reader or parser is working through input, a `#(`
symbol signals that a vector structure is being read. A number of subexpressions
for the elements of the vector follow, and then a closing parenthesis `)`
signals that the vector is done. The elements are saved in vector memory and the
vector gets the address to the first element and the number of elements.

![vrep](/images/vector-representation)

The `parse` procedure takes in the input buffer character by character, matching
each character against a fitting external representation. When done, it creates
a ConsTcl object, which is the internal representation of an expression.  The
object can then be passed to the evaluator.

Given a string, `parse` fills the input buffer. It then parses the input and
produces the internal representation of an expression.

Example:

```
% ::constcl::parse "(+ 2 3)"
::oo::Obj491
```

Here, `parse` parsed the external representation of a list with three elements,
+, 2, and 3. It produced the expression that has an internal representation
labeled `::oo::Obj491`. We will later meet procedures like `eval`, which
transforms an expression into a value, and `write`, which prints a printed
representation of expressions and values. Putting them together: we can see

```
% ::constcl::write ::oo::Obj491
(+ 2 3)
% ::constcl::eval ::oo::Obj491
::oo::Obj494
% ::constcl::write ::oo::Obj494
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

#### The parsing library

__parse__

`parse` is the entry point into the `parse-` set of commands. It can be called
with either a Tcl or ConsTcl string, or an input buffer (instance of IB). Once
the input buffer is established, `parse` leaves control to `parse-expr`.
MD)

PR(
parse (internal);inp tstrinpbuf -> expr
PR)

CB
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
CB

TT(
::tcltest::test parse-1.0 {try parse in ConsTcl} -body {
  pep {(parse "42")}
} -output "42\n"

::tcltest::test parse-1.1 {try parse with IB buffer} -body {
  pp [::constcl::IB new "42"]
} -output "42\n"

::tcltest::test parse-1.1 {try parse with string} -body {
  pp "42"
} -output "42\n"
TT)

MD(
__parse-expr__

The procedure `parse-expr` parses input by peeking at the first available
character and delegating to one of the more detailed parsing procedures based on
that, producing an expression of any kind.
MD)

PR(
parse-expr (internal);-> expr
PR)

CB
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
CB

MD(
__parse-string-expr__

`parse-string-expr` parses input starting with a double quote and collects
characters until it reaches another (unescaped) double quote. It then returns a
string expression--an immutable
String[#](https://github.com/hoodiecrow/ConsTcl#strings) object.
MD)

PR(
parse-string-expr (internal);-> str
PR)

CB
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
CB

TT(

::tcltest::test parse-2.0 {try reading a string} {
    set expr [p {"foo bar"}]
    $expr value
} "foo bar"

::tcltest::test parse-2.1 {try reading a string} {
    set expr [p {"\"foo\" \\ bar"}]
    $expr value
} {"foo" \ bar}

TT)

MD(
__parse-sharp__

`parse-sharp` parses input starting with a sharp sign (#) and either produces
the boolean literals, or delegates to the vector parser (if the next character is
a left parenthesis) or the character parser (if it is a backslash). 
MD)

PR(
parse-sharp (internal);-> sharp
PR)

CB
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
CB

MD(
__make-constant__

The `make-constant` helper procedure is called to set components of expressions to
constants when read as a quoted literal.
MD)

CB
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
CB

MD(
__parse-quoted-expr__

`parse-quoted-expr` parses input starting with a single quote ('), and then
parses an entire expression beyond that, returning it wrapped in a list with
`quote`. The quoted expression is made constant.
MD)

PR(
parse-quoted-expr (internal);-> quote
PR)

CB
proc ::constcl::parse-quoted-expr {} {
  upvar ib ib
  $ib advance
  set expr [parse-expr]
  $ib skip-ws
  make-constant $expr
  return [list [S quote] $expr]
}
CB

TT(

::tcltest::test parse-3.0 {try reading quoted symbol} -body {
    pp "'foo"
} -output "(quote foo)\n"

TT)

MD(
__parse-pair-expr__

The `parse-pair-expr` procedure parses everything between two matching
parentheses, or, as the case might be, brackets. It produces a possibly
recursive structure of
Pair[#](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) objects, either a
proper list, i.e. one that ends in #NIL, or an improper one. i.e. one that has an atom as
its last member, or in some cases an empty list.
MD)

PR(
parse-pair-expr (internal);char pterm -> pstr
PR)

CB
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
CB

MD(
`parse-pair` is a helper procedure that does the heavy lifting in parsing a pair
structure. First it checks if the list is empty, returning #NIL in that case.
Otherwise it parses the first element in the list and then repeatedly the rest of
them. If it parses a Dot object, the following element to be parsed is the tail
end of an improper list. When `parse-pair` has reached the ending parenthesis or
bracket, it conses up the elements starting from the last, and returns the head
of the list.
MD)

CB
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
CB

TT(
::tcltest::test parse-4.0 {try reading an improper list} -body {
    pp "(a . b)"
} -output "(a . b)\n"

::tcltest::test parse-4.1 {try reading an improper list} -body {
    pp "(a b . c)"
} -output "(a b . c)\n"

::tcltest::test parse-4.2 {try reading a list} -body {
    set expr [p "(a (b))"]
    [::constcl::caadr $expr] name
} -result "b"

::tcltest::test parse-4.3 {try reading a list} -body {
    pp "(a)"
} -output "(a)\n"

::tcltest::test parse-4.4 {try reading a list} -body {
    pp "(a b)"
} -output "(a b)\n"

::tcltest::test parse-4.5 {try reading a list} -body {
    pp "(a b c)"
} -output "(a b c)\n"

::tcltest::test parse-4.6 {try reading a list} -body {
    pp "(a b c d)"
} -output "(a b c d)\n"

::tcltest::test parse-4.7 {try reading a list} -body {
    pp "(a b c d e)"
} -output "(a b c d e)\n"

::tcltest::test parse-4.8 {try reading a list} -body {
    pp "(a (b) )"
} -output "(a (b))\n"

::tcltest::test parse-4.9 {try reading a list} -body {
    pp "(a (b))"
} -output "(a (b))\n"

TT)

MD(
__parse-plus-minus__

`parse-plus-minus` is called when a plus or minus is found in the input buffer.
If the next character is a digit, it delegates to the number parser. Otherwise,
it returns a `+` or `-` symbol.
MD)

PR(
parse-plus-minus (internal);-> pm
PR)

CB
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
CB

MD(
__parse-unquoted-expr__

When a comma is found in the input buffer, `parse-unquoted-expr` is activated.
If it reads an at-sign (@) it selects the symbol `unquote-splicing`, otherwise
it selects the symbol `unquote`. Then it parses an entire expression and returns
it wrapped in the selected symbol. Both of these expressions are only suppposed
to occur inside a quasiquoted expression.
MD)

PR(
parse-unquoted-expr (internal);-> unquote
PR)

CB
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
CB

TT(

::tcltest::test parse-5.0 {try reading unquoted symbol} -body {
    pp ",foo"
} -output "(unquote foo)\n"

TT)

MD(
__parse-quasiquoted-expr__

`parse-quasiquoted-expr` is activated when there is a backquote (&grave;) in the
input buffer. It parses an entire expression and returns it wrapped in
`quasiquote`.
MD)

PR(
parse-quasiquoted-expr (internal);-> qquote
PR)

CB
proc ::constcl::parse-quasiquoted-expr {} {
  upvar ib ib
  $ib advance
  set expr [parse-expr]
  $ib skip-ws
  # TODO make semi-constant
  make-constant $expr
  return [list [S "quasiquote"] $expr]
}
CB

TT(

::tcltest::test parse-6.0 {try reading unquoted symbol} -body {
    pp "`(list 1 2 ,@foo)"
} -output "(quasiquote (list 1 2 (unquote-splicing foo)))\n"

TT)

MD(
__interspace__

The `interspace` helper procedure recognizes whitespace between value
representations.
MD)

CB
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
CB

MD(
__parse-number-expr__

`parse-number-expr` parses numerical input, both integers and floating point
numbers. It actually takes in anything that starts out like a number and stops
at whitespace or an ending parenthesis or bracket, and then it accepts or
rejects the input by comparing it to a Tcl double. It returns a
Number[#](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

PR(
parse-number-expr (internal);-> num
PR)

CB
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
CB

TT(
::tcltest::test parse-7.0 {try reading a number} {
    set obj [::constcl::parse "99.99"]
    $obj value
} "99.99"

::tcltest::test parse-7.1 {try reading a number} {
    set obj [::constcl::parse "     99.99"]
    $obj value
} "99.99"

::tcltest::test parse-7.2 {try reading a number} {
    set obj [::constcl::parse "     9"]
    $obj value
} "9"

::tcltest::test parse-7.3 {try reading a number} {
    set obj [::constcl::parse "     +9"]
    $obj value
} "+9"

::tcltest::test parse-7.4 {try reading a number} {
    set obj [::constcl::parse "     -9"]
    $obj value
} "-9"

::tcltest::test parse-7.5 {try reading a number} {
    set obj [::constcl::parse "     - "]
    $obj name
} "-"

::tcltest::test parse-7.6 {try reading a number} {
    set obj [::constcl::parse "     + "]
    $obj name
} "+"

TT)

MD(
__parse-identifier-expr__

`parse-identifier-expr` is activated for "anything else", and takes in
characters until it finds whitespace or an ending parenthesis or bracket. It
checks the input against the rules for identifiers, accepting or rejecting it
with an error message. It returns a
Symbol[#](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

PR(
parse-identifier-expr (internal);-> sym
PR)

CB
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
CB

TT(

::tcltest::test parse-8.0 {try reading an identifier} {
    set expr [::constcl::parse [::constcl::IB new "foo"]]
    $expr name
} "foo"

::tcltest::test parse-8.1 {try reading an identifier} -body {
    set ib [::constcl::IB new "+foo"]
    set expr [::constcl::parse-identifier-expr]
    $expr name
} -returnCodes error -result "Identifier expected (+foo)"

::tcltest::test parse-8.2 {try reading an identifier} -body {
    ::constcl::IB create ib-read-5.2 "let"
    set expr [::constcl::parse ib-read-5.2]
    ::constcl::varcheck [$expr name]
} -returnCodes error -result "Variable name is reserved: let"

TT)

MD(
__character-check__

The `character-check` helper procedure compares a potential
character constant to the valid kinds.
MD)

PR(
character-check (internal);name tstr -> tbool
PR)

CB
proc ::constcl::character-check {name} {
  if {[regexp {(?i)^#\\([[:graph:]]|space|newline)$} \
      $name]} {
    return #t
  } else {
    return #f
  }
}
CB

MD(
__parse-character-expr__

`parse-character-expr` is activated from `parse-sharp` and parses a character or
character name from input, producing and returning a
Char[#](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

PR(
parse-character-expr (internal);-> char
PR)

CB
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
CB

TT(

::tcltest::test parse-9.0 {try reading a character} {
    set expr [::constcl::parse [::constcl::IB new {#\A}]]
    $expr char
} "A"

::tcltest::test parse-9.1 {try reading a character} {
    set expr [::constcl::parse [::constcl::IB new "#\\space"]]
    $expr char
} " "

::tcltest::test parse-9.2 {try reading a character} {
    set expr [::constcl::parse [::constcl::IB new "#\\newline"]]
    $expr char
} "\n"

::tcltest::test parse-9.3 {try reading a character} -body {
    set expr [::constcl::parse [::constcl::IB new "#\\foobar"]]
    $expr char
} -returnCodes error -result "Invalid character constant #\\foobar"

TT)

MD(
__parse-vector-expr__

`parse-vector-expr` is also activated from `parse-sharp`. It parses a number of
expressions until it encounters an ending parenthesis. It produces a vector with
the expressions parsed as elements and returns a
Vector[#](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

PR(
parse-vector-expr (internal);-> vec
PR)

CB
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
CB

TT(

::tcltest::test parse-10.0 {try reading a vector} -body {
    pp "#(1 2 3)"
} -output "#(1 2 3)\n"

::tcltest::test parse-10.1 {try reading a vector, with non-normal expression} -body {
    pp "#(1 2 (+ 1 2))"
} -output "#(1 2 (+ 1 2))\n"

TT)

MD(
__parse-object-expr__

A non-standard extension, `parse-object-expr` parses a ConsTcl object of any
kind and passes its name along.
MD)

PR(
parse-object-expr (internal);-> obj
PR)

CB
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
CB

MD(
### read

__read__

The standard builtin `read` reads an input port approximately the same way that
`parse` takes in an input buffer. Like the `parse-` procedures, the `read-`
procedures also parse their input (with small differences) and produce ConsTcl
objects just like them.

One can pass a port to `read`, in which case `read` sets the standard input port
temporarily to the provided port. If not, `read` uses the standard input port
(usually the keyboard).
MD)

PR(
read (public);?port? port -> expr
PR)

CB
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
CB

MD(
__read-expr__

The procedure `read-expr` parses input by reading the first available
character and delegating to one of the more detailed reading procedures based on
that, producing an expression of any kind. A Tcl character value can be passed
to it, that character will be used first before reading from the input stream.
If the end of file is encountered before an expression can be read in full, the
procedure returns end of file.
MD)

PR(
read-expr (internal);?char? tchar -> expreof
PR)

CB
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
CB

TT(
::tcltest::test read-1.0 {try read-expr on a string} -setup {
    ::tcltest::makeFile {"foo bar"  } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "\"foo bar\"\n"

::tcltest::test read-1.1 {try read-expr on a string/eof} -setup {
    ::tcltest::makeFile {"foo } testrr.lsp ; #"
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -returnCodes error -result {bad string (no ending double quote)}

::tcltest::test read-1.2 {try read-expr on a couple of vectors} -setup {
    ::tcltest::makeFile {  #(1 2 3)  #(11 22 33)} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#(1 2 3)\n#(11 22 33)\n"

::tcltest::test read-1.3 {try read-expr on booleans} -setup {
    ::tcltest::makeFile {  #t  #f} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#t\n#f\n"

::tcltest::test read-1.4 {try read-expr on characters} -setup {
    ::tcltest::makeFile {  #\A  #\space} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#\\A\n#\\space\n"

::tcltest::test read-1.5 {try read-expr on quoted expr} -setup {
    ::tcltest::makeFile {  'foo } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(quote foo)\n"

::tcltest::test read-1.6 {try read-expr on pair expr} -setup {
    ::tcltest::makeFile {  (a b c)  ((a b) c)} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(a b c)\n"

::tcltest::test read-1.7 {try read-expr on pair expr} -setup {
    ::tcltest::makeFile {  ([d e] f)} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "((d e) f)\n"

::tcltest::test read-1.8 {try read-expr on pair expr} -setup {
    ::tcltest::makeFile {  (def ghi (jkl mno))} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(def ghi (jkl mno))\n"

::tcltest::test read-1.9 {try read-expr on plus/minus} -setup {
    ::tcltest::makeFile {  +  -  -99} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
    w [r $p]
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "+\n-\n-99\n"

::tcltest::test read-1.10 {try read-expr on unquoted expr} -setup {
    ::tcltest::makeFile {  ,foo ,@bar} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(unquote foo)\n(unquote-splicing bar)\n"

::tcltest::test read-1.11 {try read-expr on dot expr} -setup {
    ::tcltest::makeFile {  a . b } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
    w [r $p]
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "a\n.\nb\n"

::tcltest::test read-1.12 {try read-expr on quasiquoted expr} -setup {
    ::tcltest::makeFile {  `(a b) } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(quasiquote (a b))\n"

::tcltest::test read-1.13 {try read-expr on numeric expr} -setup {
    ::tcltest::makeFile {  99 } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "99\n"

::tcltest::test read-1.14 {try read-expr on identifiers} -setup {
    ::tcltest::makeFile {  foo    bar } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    w [r $p]
    w [r $p]
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "foo\nbar\n"
TT)

MD(
`readc` reads one character from the unget store if it isn't empty or else from
the input stream. If the input stream is at end-of-file, an eof object is
returned.
MD)

PR(
readc (internal);-> tchareof
PR)

CB
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
CB

MD(
`read-find` reads ahead through whitespace to find a given character. Returns 1
if it has found the character, and 0 if it has stopped at some other character.
Returns end of file if eof is encountered.
MD)

PR(
read-find (internal);char tchar -> tbooleof
PR)

CB
proc ::constcl::read-find {char} {
  upvar c c unget unget
  while {[::string is space -strict $c]} {
    set c [readc]
    read-eof $c
    set unget $c
  }
  return [expr {$c eq $char}]
}
CB

MD(
`read-end` reads one character and returns 1 if it is an interspace character or
an ending parenthesis or bracket. Otherwise it returns 0 or end-of-file if
applicable. It ungets the character before returning.
MD)

PR(
read-end (internal);->tbooleof
PR)

CB
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
CB

MD(
`skip-ws` skips whitespace and comments (the ; to end of line kind). Uses the
shared **c** character. It leaves the first character not to be skipped in **c**.
MD)

PR(
skip-ws (internal);-> none
PR)

CB
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
CB

MD(
`read-eof` checks a number of objects for possible end-of-file objects. If it
finds one, it returns **from its caller** with the EOF value.
MD)

PR(
read-eof (internal);args chars
PR)

CB
proc ::constcl::read-eof {args} {
  foreach val $args {
    if {$val eq "#EOF"} {
      return -level 1 -code return #EOF
    }
  }
}
CB

MD(
__read-string-expr__

`read-string-expr` is activated by `read-expr` when it reads a double quote. It
collects characters until it reaches another (unescaped) double quote. It then
returns a string expression--an immutable
String[#](https://github.com/hoodiecrow/ConsTcl#strings) object.
MD)

PR(
read-string-expr (internal);-> streof
PR)

CB
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
  read-eof $expr
  $expr mkconstant
  return $expr
}
CB

MD(
__read-sharp__

`read-sharp` is activated by `read-expr` when it reads a sharp sign (#). It in
turn either delegates to the vector reader or the character reader, or returns
boolean literals.
MD)

PR(
read-sharp (internal);-> sharpeof
PR)

CB
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
CB

MD(
__read-vector-expr__

`read-vector-expr` is activated by `read-sharp` and reads a number of expressions until it finds an ending parenthesis.  It produces a vector expression and returns a Vector[#](https://github.com/hoodiecrow/ConsTcl#vectors) object.
MD)

PR(
read-vector-expr (internal);-> veceof
PR)

CB
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
  read-eof $expr
  $expr mkconstant
  return $expr
}
CB

MD(
__read-character-expr__

`read-character-expr` parses input, producing a character and returning
a Char[#](https://github.com/hoodiecrow/ConsTcl#characters) object.
MD)

PR(
read-character-expr (internal);-> chareof
PR)

CB
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
CB

MD(
__read-quoted-expr__

`read-quoted-expr` is activated by `read-expr` when reading a single quote (').
It then reads an entire expression beyond that, returning it wrapped in a list
with `quote`. The quoted expression is made constant.
MD)

PR(
read-quoted-expr (internal);-> quoteeof
PR)

CB
proc ::constcl::read-quoted-expr {} {
  upvar c c unget unget
  set expr [read-expr]
  read-eof $expr
  make-constant $expr
  return [list [S quote] $expr]
}
CB

MD(
__read-pair-expr__

The `read-pair-expr` procedure reads everything between two matching
parentheses, or, as the case might be, brackets. It produces a possibly
recursive structure of
Pair[#](https://github.com/hoodiecrow/ConsTcl#pairs-and-lists) objects, either
an empty list, a proper list, i.e. one that ends in #NIL, or an improper one.
i.e. one that has an atom as its last member.
MD)

PR(
read-pair-expr (internal);char pterm -> pstreof
PR)

CB
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
CB

MD(
`read-pair` is a helper procedure that does the heavy lifting in reading a pair
structure. First it checks if the list is empty, returning #NIL in that case.
Otherwise it reads the first element in the list and then repeatedly the rest of
them. If it reads a Dot object, the following element to be read is the tail
end of an improper list. When `read-pair` has reached the ending parenthesis or
bracket, it conses up the elements starting from the last, and returns the head
of the list.
MD)

CB
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
CB

MD(
__read-plus-minus__

`read-plus-minus` is called when a plus or minus is found in the input stream.
If the next character is a digit, it delegates to the number reader. Otherwise,
it returns a `+` or `-` symbol.
MD)

PR(
read-plus-minus (internal);-> pmeof
PR)

CB
proc ::constcl::read-plus-minus {char} {
  upvar c c unget unget
  set c [readc]
  read-eof $c
  if {[::string is digit -strict $c]} {
    set n [read-number-expr $c]
    read-eof $n
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
CB

MD(
__read-number-expr__

`read-number-expr` reads numerical input, both integers and floating point
numbers. It actually takes in anything that starts out like a number and stops
at whitespace or an ending parenthesis or bracket, and then it accepts or
rejects the input by comparing it to a Tcl double. It returns a
Number[#](https://github.com/hoodiecrow/ConsTcl#numbers) object.
MD)

PR(
read-number-expr (internal);?char? tchar -> numeof
PR)

CB
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
CB

MD(
__read-unquoted-expr__

When a comma is found in the input stream, `parse-unquoted-expr` is activated.
If it reads an at-sign (@) it selects the symbol `unquote-splicing`, otherwise
it selects the symbol `unquote`. Then it reads an entire expression and returns
it wrapped in the selected symbol. Both of these expressions are only suppposed
to occur inside a quasiquoted expression.
MD)

PR(
read-unquoted-expr (internal);-> unquoteeof
PR)

CB
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
CB

MD(
__read-object-expr__

A non-standard extension, `read-object-expr` reads a ConsTcl object of any kind
and passes its name along.
MD)

PR(
read-object-expr (internal);-> objeof
PR)

CB
proc ::constcl::read-object-expr {} {
  upvar c c unget unget
  # first colon has already been read
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
CB

MD(
__read-quasiquoted-expr__

`read-quasiquoted-expr` is activated when there is a backquote (&grave;) in the
input stream. It reads an entire expression and returns it wrapped in
`quasiquote`.
MD)

PR(
read-quasiquoted-expr (internal);-> qquoteeof
PR)

CB
proc ::constcl::read-quasiquoted-expr {} {
  upvar c c unget unget
  set expr [read-expr]
  skip-ws
  read-eof $expr
  make-constant $expr
  return [list [S "quasiquote"] $expr]
}
CB

MD(
__read-identifier-expr__

`read-identifier-expr` is activated for "anything else", and takes in
characters until it finds whitespace or an ending parenthesis or bracket. It
checks the input against the rules for identifiers, accepting or rejecting it
with an error message. It returns a
Symbol[#](https://github.com/hoodiecrow/ConsTcl#symbols) object.
MD)

PR(
read-identifier-expr (internal);?char? tchar -> symeof
PR)

CB
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
CB

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
