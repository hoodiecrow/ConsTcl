
proc ::constcl::__eval {expr {env ::constcl::global_env}} {
    if {[atom? $expr] ne "#f"} {
        if {[symbol? $expr] ne "#f"} {
            return [lookup $expr $env]
        } elseif {[null? $expr] ne "#f" || [atom? $expr] ne "#f"} {
            return $expr
        } else {
            error "cannot evaluate $expr"
        }
    } else {
        set op [car $expr]
        set args [cdr $expr]
        while {[$op name] in {
                and case cond define for for/and
                for/list for/or let or quasiquote}} {
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


CB
proc ::constcl::expand-put! {exps} {
    ::if {[null? $exps] ne "#f"} {::error "too few arguments, 3 expected, got 0"}
    set listname [car $exps]
    ::if {[null? [cdr $exps]] ne "#f"} {::error "too few arguments, 3 expected, got 1"}
    set key [cadr $exps]
    ::if {[null? [cddr $exps]] ne "#f"} {::error "too few arguments, 3 expected, got 2"}
    set val [caddr $exps]
    set keypresent [list #B [list [MkSymbol "list-set!"] $listname [list [MkSymbol "+"] [MkSymbol "idx"] #1] $val] $listname]
    set keynotpresent [list #S $listname [list [MkSymbol "append"] [list [MkSymbol "list"] $key $val] $listname]]
    set conditional [list #I [list [MkSymbol "<"] [MkSymbol "idx"] #0] $keynotpresent $keypresent]
    set let [list #L [list [list [MkSymbol "idx"] [list [MkSymbol "list-find-key"] $listname $key]]] $conditional]
    #return $let
    set qq "`(let ((idx (list-find-key ,listname ,key))) (if (< idx 0) (set! ,listname (append (list ,key ,val) ,listname)) (begin (list-set! plist (+ idx 1) ,val) ,listname)))"
    set env [::constcl::Environment new #NIL {} ::global_env]
    $env set [MkSymbol "listname"] $listname
    $env set [MkSymbol "key"] $key
    $env set [MkSymbol "val"] $val
    return [expand-quasiquote [cdr [parse $qq]] $env]
}
CB


MD(
`--load` is a raw port of the S9fES implementation. `----load` is my original
straight-Tcl version.
MD)

CB
proc ::constcl::--load {filename} {
  set new_port [MkInputPort]
  $new_port open $filename
  if {[$new_port handle] eq "#NIL"} {
    return -1
  }
  set ::constcl::File_list [cons [MkString $filename] $::constcl::File_list]
  set save_env $env
  set env ::constcl::global_env
  set outer_loading [$::constcl::S_loading cdr]
  set-cdr! ::constcl::S_loading #t
  set old_port $::constcl::Input_port
  set outer_lno $::constcl::Line_no
  set ::constcl::Line_no 1
  while true {
    set ::constcl::Input_port $new_port
    set n [xread]
    set ::constcl::Input_port $old_port
    if {$n == $::constcl::END_OF_FILE} {
      break
    }
    set n [eval $n $env]
  }
  $new_port close
  set $::constcl::Line_no $outer_lno
  set-cdr! ::constcl::S_loading $outer_loading
  set ::constcl::File_list [cdr $::constcl::File_list]
  set env $save_env
  return 0
}

proc ::constcl::----load {filename} {
  set f [open $filename]
  set src [::read $f]
  close $f
  set ib [::constcl::IB new $src]
  while {[$ib first] ne {}} {
    eval [parse $ib]
  }
}
CB

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

