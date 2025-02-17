
MD(
## Input

The first thing an interpreter must be able to do is to take in the user's code
and data input, whether from the keyboard or from a source file.  `read`
represents the interpreter's main input facility. The `read-` procedures read
from standard input, or--if a port is provided--from the port's channel.
MD)

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

`parse` can be called with either a Tcl or ConsTcl string, or a string input
port. Once the input port is established, `parse` leaves control to `read-expr`.
MD)

PR(
parse (internal);inp tstrinpbuf -> expr
PR)

CB
reg parse

proc ::constcl::parse {inp} {
  set c {}
  set unget {}
  if {[info object isa object $inp]} {
    if {[typeof? $inp IB] ne "#f"} {
      error "IB used"
    } elseif {[typeof? $inp StringInputPort] ne "#f"} {
      set port $inp
    } elseif {[typeof? $inp String] ne "#f"} {
      set port [::constcl::StringInputPort new [$inp value]]
    } else {
      ::error "Unknown object [$inp show]"
    }
  } else {
    # It's a Tcl string, we hope
    set port [StringInputPort new $inp]
  }
  set oldport $::constcl::Input_port
  set ::constcl::Input_port $port
  set expr [read-expr]
  set ::constcl::Input_port $oldport
  return $expr
}
CB

TT(
::tcltest::test read-1.0 {try parse in ConsTcl} -body {
  pew {(parse "42")}
} -output "42\n"

::tcltest::test read-1.1 {try parse with StringInputPort buffer} -body {
  pw [::constcl::StringInputPort new "42"]
} -output "42\n"

::tcltest::test read-1.1 {try parse with string} -body {
  pw "42"
} -output "42\n"
TT)

TT(

::tcltest::test read-2.0 {try reading a string} {
    set expr [p {"foo bar"}]
    $expr value
} "foo bar"

::tcltest::test read-2.1 {try reading a string} {
    set expr [p {"\"foo\" \\ bar"}]
    $expr value
} {"foo" \ bar}

TT)

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

TT(

::tcltest::test read-3.0 {try reading quoted symbol} -body {
    pw "'foo"
} -output "(quote foo)\n"

TT)

TT(
::tcltest::test read-4.0 {try reading an improper list} -body {
    pw "(a . b)"
} -output "(a . b)\n"

::tcltest::test read-4.1 {try reading an improper list} -body {
    pw "(a b . c)"
} -output "(a b . c)\n"

::tcltest::test read-4.2 {try reading a list} -body {
    set expr [p "(a (b))"]
    [::constcl::caadr $expr] name
} -result "b"

::tcltest::test read-4.3 {try reading a list} -body {
    pw "(a)"
} -output "(a)\n"

::tcltest::test read-4.4 {try reading a list} -body {
    pw "(a b)"
} -output "(a b)\n"

::tcltest::test read-4.5 {try reading a list} -body {
    pw "(a b c)"
} -output "(a b c)\n"

::tcltest::test read-4.6 {try reading a list} -body {
    pw "(a b c d)"
} -output "(a b c d)\n"

::tcltest::test read-4.7 {try reading a list} -body {
    pw "(a b c d e)"
} -output "(a b c d e)\n"

::tcltest::test read-4.8 {try reading a list} -body {
    pw "(a (b) )"
} -output "(a (b))\n"

::tcltest::test read-4.9 {try reading a list} -body {
    pw "(a (b))"
} -output "(a (b))\n"

TT)

TT(

::tcltest::test read-5.0 {try reading unquoted symbol} -body {
    pw ",foo"
} -output "(unquote foo)\n"

TT)

TT(

::tcltest::test read-6.0 {try reading quasiquoted expression} -body {
    pw "`(list 1 2 ,@foo)"
} -output "(quasiquote (list 1 2 (unquote-splicing foo)))\n"

TT)

MD(
__interspace__

The `interspace` helper procedure recognizes whitespace between value
representations.
MD)

CB
proc ::constcl::interspace {c} {
  if {$c eq {} ||
    [::string is space $c] ||
    $c eq ";"} {
      return #t
    } else {
      return #f
    }
}
CB

TT(
::tcltest::test read-7.0 {try reading a number} {
    set obj [::constcl::parse "99.99"]
    $obj value
} "99.99"

::tcltest::test read-7.1 {try reading a number} {
    set obj [::constcl::parse "     99.99"]
    $obj value
} "99.99"

::tcltest::test read-7.2 {try reading a number} {
    set obj [::constcl::parse "     9"]
    $obj value
} "9"

::tcltest::test read-7.3 {try reading a number} {
    set obj [::constcl::parse "     +9"]
    $obj value
} "9"

::tcltest::test read-7.4 {try reading a number} {
    set obj [::constcl::parse "     -9"]
    $obj value
} "-9"

::tcltest::test read-7.5 {try reading a number} {
    set obj [::constcl::parse "     - "]
    $obj name
} "-"

::tcltest::test read-7.6 {try reading a number} {
    set obj [::constcl::parse "     + "]
    $obj name
} "+"

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

TT(

::tcltest::test read-9.0 {try reading a character} {
    set expr [p {#\A}]
    $expr char
} "A"

::tcltest::test read-9.1 {try reading a character} {
    set expr [p "#\\space"]
    $expr char
} " "

::tcltest::test read-9.2 {try reading a character} {
    set expr [p "#\\newline"]
    $expr char
} "\n"

::tcltest::test read-9.3 {try reading a character} -body {
    set expr [p "#\\foobar"]
    $expr char
} -returnCodes error -result "Invalid character constant #\\foobar"

TT)

TT(

::tcltest::test read-10.0 {try reading a vector} -body {
    pew "#(1 2 3)"
} -output "#(1 2 3)\n"

::tcltest::test read-10.1 {try reading a vector, with non-normal expression} -body {
    pew "#(1 2 (+ 1 2))"
} -output "#(1 2 (+ 1 2))\n"

TT)

TT(

::tcltest::test read-8.0 {try reading an identifier} {
    set expr [p "foo"]
    $expr name
} "foo"

::tcltest::test read-8.1 {try reading an identifier} -body {
    set expr [p "let"]
    ::constcl::varcheck [$expr name]
} -returnCodes error -result "Variable name is reserved: let"

TT)

MD(
### read

__read__

The standard builtin `read` reads an input port approximately the same way that
`parse` takes in an input buffer. The `read-` procedures parse their input and
produce ConsTcl objects just like them.

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

The procedure `read-expr` reads input by reading the first available
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
  set unget {}
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
    {\.} {
        set x [Dot new]; set c [readc]; set x
    }
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
::tcltest::test read-11.0 {try read-expr on a string} -setup {
    ::tcltest::makeFile {"foo bar"  } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "\"foo bar\"\n"

::tcltest::test read-11.1 {try read-expr on a string/eof} -setup {
    ::tcltest::makeFile {"foo } testrr.lsp ; #"
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -returnCodes error -result {bad string (no ending double quote)}

::tcltest::test read-11.2 {try read-expr on a couple of vectors} -setup {
    ::tcltest::makeFile {  #(1 2 3)  #(11 22 33)} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#(1 2 3)\n#(11 22 33)\n"

::tcltest::test read-11.3 {try read-expr on booleans} -setup {
    ::tcltest::makeFile {  #t  #f} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#t\n#f\n"

::tcltest::test read-11.4 {try read-expr on characters} -setup {
    ::tcltest::makeFile {  #\A  #\space} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "#\\A\n#\\space\n"

::tcltest::test read-11.5 {try read-expr on quoted expr} -setup {
    ::tcltest::makeFile {  'foo } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(quote foo)\n"

::tcltest::test read-11.6 {try read-expr on pair expr} -setup {
    ::tcltest::makeFile {  (a b c)  ((a b) c)} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(a b c)\n"

::tcltest::test read-11.7 {try read-expr on pair expr} -setup {
    ::tcltest::makeFile {  ([d e] f)} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "((d e) f)\n"

::tcltest::test read-11.8 {try read-expr on pair expr} -setup {
    ::tcltest::makeFile {  (def ghi (jkl mno))} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(def ghi (jkl mno))\n"

::tcltest::test read-11.9 {try read-expr on plus/minus} -setup {
    ::tcltest::makeFile {  +  -  -99} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
    rw $p
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "+\n-\n-99\n"

::tcltest::test read-11.10 {try read-expr on unquoted expr} -setup {
    ::tcltest::makeFile {  ,foo ,@bar} testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(unquote foo)\n(unquote-splicing bar)\n"

::tcltest::test read-11.11 {try read-expr on dot expr} -setup {
    ::tcltest::makeFile {  a . b } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
    rw $p
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "a\n.\nb\n"

::tcltest::test read-11.12 {try read-expr on quasiquoted expr} -setup {
    ::tcltest::makeFile {  `(a b) } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "(quasiquote (a b))\n"

::tcltest::test read-11.13 {try read-expr on numeric expr} -setup {
    ::tcltest::makeFile {  99 } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
} -cleanup {
    ::constcl::close-input-port $p
    ::tcltest::removeFile testrr.lsp
} -output "99\n"

::tcltest::test read-11.14 {try read-expr on identifiers} -setup {
    ::tcltest::makeFile {  foo    bar } testrr.lsp
    set p [pe {(open-input-file "testrr.lsp")}]
} -body {
    rw $p
    rw $p
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
    set c [$::constcl::Input_port get]
    if {[$::constcl::Input_port eof]} {
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
  if {[interspace $c] ne "#f" || $c in {) ]} || $c eq "#EOF"} {
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
        set unget $c
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
      ::append str $c
      set c [readc]
    }
    ::append str $c
    set c [readc]
  }
  if {$c eq "#EOF"} {
    error "bad string (no ending double quote)"
  }
  set c [readc]
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
  set unget {}
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

`read-character-expr` reads input, producing a character and returning
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
  while {$c ni {) ]} && [::string is graph $c] && $c ne "#EOF"} {
    ::append name $c
    set c [readc]
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
  set unget {}
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
  set unget {}
  set expr [read-pair $char]
  read-eof $expr
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
  set c [readc]
  read-eof $c
  if {[read-find $char]} {
    # read right paren/brack
    #set c [readc]
    return #NIL
  }
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
      skip-ws
      read-eof $c
    } else {
      lappend res $x
    }
    if {[llength $res] > 99} break
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
  set unget {}
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
  set unget {}
  if {[llength $args]} {
    lassign $args c
  } else {
    set c [readc]
  }
  read-eof $c
  while {[interspace $c] ne "#t" && $c ne "#EOF" &&
      $c ni {) ]}} {
    ::append num $c
    set c [readc]
  }
  set unget $c
  check {::string is double -strict $num} {
      Invalid numeric constant $num
  }
  set expr [N $num]
  return $expr
}
CB

MD(
__read-unquoted-expr__

When a comma is found in the input stream, `read-unquoted-expr` is activated.
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
  set unget {}
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
  set unget {}
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
  set unget {}
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

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
