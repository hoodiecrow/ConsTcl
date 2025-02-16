
MD(
### Characters

Characters are any Unicode printing character, and also space and newline space
characters. External representation is '#\A' (change A to relevant character)
or #\space or #\newline. Internal representation is simply a Tcl character.

__Char__ class
MD)

CB
oo::class create ::constcl::Char {
  superclass ::constcl::NIL
  variable value
  constructor {v} {
    switch -regexp $v {
      {(?i)#\\space} {
        set v " "
      }
      {(?i)#\\newline} {
        set v "\n"
      }
      {#\\[[:graph:]]} {
        set v [::string index $v 2]
      }
    }
    set value $v
  }
  method char {} {
    set value
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
  method write {handle} {
    puts -nonewline $handle [my external]
  }
  method display {handle} {
    puts -nonewline $handle [my char]
  }
  method show {} {
    my external
  }
}

proc ::constcl::MkChar {v} {
  if {[regexp -nocase {space|newline} $v]} {
      set v [::string tolower $v]
  }
  foreach instance [
    info class instances Char] {
    if {[$instance external] eq $v} {
      return $instance
    }
  }
  return [::constcl::Char new $v]
}
CB

MD(
__char?__

`char?` recognizes Char values by type.
MD)

PR(
char? (public);val val -> bool
PR)

CB
reg char?

proc ::constcl::char? {val} {
  return [typeof? $val Char]
}
CB

TT(

::tcltest::test characters-1.0 {try char?} -body {
    pew {(char? #\A)}
} -output "#t\n"

TT)

MD(
__char=?__

__char<?__

__char>?__

__char<=?__

__char>=?__

`char=?`, `char<?`, `char>?`, `char<=?`, and `char>=?` compare character
values. They only compare two characters at a time.
MD)

PR(
char=?, char<?, char>?, char<=?, char>=? (public);char1 char char2 char -> bool
PR)

CB
reg char=?

proc ::constcl::char=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {$char1 eq $char2} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.1 {try char=?} -body {
    pew {(char=? #\A #\A)}
    pew {(char=? #\A #\a)}
    pew {(char=? #\Space #\space)}
} -output "#t\n#f\n#t\n"

::tcltest::test characters-check-1.0 {try triggering a check} -body {
    pew {(char=? 99 #\A)}
} -returnCodes error -result "CHAR expected\n(char=? 99 #\\A)"

TT)

CB
reg char<?

proc ::constcl::char<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] < [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.2 {try char<?} -body {
    pew {(char<? #\A #\A)}
    pew {(char<? #\A #\B)}
    pew {(char<? #\B #\A)}
} -output "#f\n#t\n#f\n"

TT)

CB
reg char>?

proc ::constcl::char>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] > [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.3 {try char>?} -body {
    pew {(char>? #\A #\A)}
    pew {(char>? #\A #\B)}
    pew {(char>? #\B #\A)}
} -output "#f\n#f\n#t\n"

TT)

CB
reg char<=?

proc ::constcl::char<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] <= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.4 {try char<=?} -body {
    pew {(char<=? #\A #\A)}
    pew {(char<=? #\A #\B)}
    pew {(char<=? #\B #\A)}
} -output "#t\n#t\n#f\n"

TT)

CB
reg char>=?

proc ::constcl::char>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[$char1 char] >= [$char2 char]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.5 {try char>=?} -body {
    pew {(char>=? #\A #\A)}
    pew {(char>=? #\A #\B)}
    pew {(char>=? #\B #\A)}
} -output "#t\n#f\n#t\n"

TT)

MD(
__char-ci=?__

__char-ci<?__

__char-ci>?__

__char-ci<=?__

__char-ci>=?__

`char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, and `char-ci>=?` compare character
values in a case insensitive manner. They only compare two characters at a time.
MD)

PR(
char-ci=?, char-ci<?, char-ci>?, char-ci<=?, char-ci>=? (public);char1 char char2 char -> bool
PR)

CB
reg char-ci=?

proc ::constcl::char-ci=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] eq
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.6 {try char-ci=?} -body {
    pew {(char-ci=? #\A #\a)}
    pew {(char-ci=? #\A #\b)}
    pew {(char-ci=? #\B #\a)}
} -output "#t\n#f\n#f\n"

TT)

CB
reg char-ci<?

proc ::constcl::char-ci<? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] <
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.7 {try char-ci<?} -body {
    pew {(char-ci<? #\A #\a)}
    pew {(char-ci<? #\A #\b)}
    pew {(char-ci<? #\B #\a)}
} -output "#f\n#t\n#f\n"

TT)

CB
reg char-ci>?

proc ::constcl::char-ci>? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] >
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.8 {try char-ci>?} -body {
    pew {(char-ci>? #\A #\a)}
    pew {(char-ci>? #\A #\b)}
    pew {(char-ci>? #\B #\a)}
} -output "#f\n#f\n#t\n"

TT)

CB
reg char-ci<=?

proc ::constcl::char-ci<=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] <=
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.9 {try char-ci<=?} -body {
    pew {(char-ci<=? #\A #\a)}
    pew {(char-ci<=? #\A #\b)}
    pew {(char-ci<=? #\B #\a)}
} -output "#t\n#t\n#f\n"

TT)

CB
reg char-ci>=?

proc ::constcl::char-ci>=? {char1 char2} {
  check {char? $char1} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  check {char? $char2} {
    CHAR expected\n([pn] [$char1 show] [$char2 show])
  }
  if {[::string tolower [$char1 char]] >=
      [::string tolower [$char2 char]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test characters-1.10 {try char-ci>=?} -body {
    pew {(char-ci>=? #\A #\a)}
    pew {(char-ci>=? #\A #\b)}
    pew {(char-ci>=? #\B #\a)}
    pew {(char-ci>=? #\A #\Space)}
} -output "#t\n#f\n#t\n#t\n"

TT)

MD(
__char-alphabetic__

__char-numeric__

__char-whitespace__

__char-upper-case__

__char-lower-case__

The predicates `char-alphabetic`, `char-numeric`, `char-whitespace`,
`char-upper-case`, and `char-lower-case` test a character for these
conditions.
MD)

PR(
char-alphabetic?, char-numeric?, char-whitespace? (public);char char -> bool
PR)

PR(
char-upper-case?, char-lower-case? (public);char char -> bool
PR)

CB
reg char-alphabetic?

proc ::constcl::char-alphabetic? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char alphabetic?]
}
CB

TT(

::tcltest::test characters-1.11 {try char-alphabetic?} -body {
    pew {(char-alphabetic? #\A)}
    pew {(char-alphabetic? #\9)}
    pew {(char-alphabetic? #\space)}
    pew {(char-alphabetic? #\A)}
    pew {(char-alphabetic? #\a)}
    pew {(char-alphabetic? #\%)}
} -output "#t\n#f\n#f\n#t\n#t\n#f\n"

TT)

CB
reg char-numeric?

proc ::constcl::char-numeric? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char numeric?]
}
CB

TT(

::tcltest::test characters-1.12 {try char-numeric?} -body {
    pew {(char-numeric? #\A)}
    pew {(char-numeric? #\9)}
    pew {(char-numeric? #\space)}
    pew {(char-numeric? #\A)}
    pew {(char-numeric? #\a)}
    pew {(char-numeric? #\%)}
} -output "#f\n#t\n#f\n#f\n#f\n#f\n"

TT)

CB
reg char-whitespace?

proc ::constcl::char-whitespace? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char whitespace?]
}
CB

TT(

::tcltest::test characters-1.13 {try char-whitespace?} -body {
    pew {(char-whitespace? #\A)}
    pew {(char-whitespace? #\9)}
    pew {(char-whitespace? #\space)}
    pew {(char-whitespace? #\A)}
    pew {(char-whitespace? #\a)}
    pew {(char-whitespace? #\%)}
} -output "#f\n#f\n#t\n#f\n#f\n#f\n"

TT)

CB
reg char-upper-case?

proc ::constcl::char-upper-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char upper-case?]
}
CB

TT(

::tcltest::test characters-1.14 {try char-upper-case?} -body {
    pew {(char-upper-case? #\A)}
    pew {(char-upper-case? #\9)}
    pew {(char-upper-case? #\space)}
    pew {(char-upper-case? #\A)}
    pew {(char-upper-case? #\a)}
    pew {(char-upper-case? #\%)}
} -output "#t\n#f\n#f\n#t\n#f\n#f\n"

TT)

CB
reg char-lower-case?

proc ::constcl::char-lower-case? {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [$char lower-case?]
}
CB

TT(

::tcltest::test characters-1.15 {try char-lower-case?} -body {
    pew {(char-lower-case? #\A)}
    pew {(char-lower-case? #\9)}
    pew {(char-lower-case? #\space)}
    pew {(char-lower-case? #\A)}
    pew {(char-lower-case? #\a)}
    pew {(char-lower-case? #\%)}
} -output "#f\n#f\n#f\n#f\n#t\n#f\n"

TT)

MD(
__char->integer__

__integer->char__

`char->integer` and `integer->char` convert between characters and their
16-bit numeric codes.
MD)

PR(
char->integer (public);char char -> int
PR)

MD(
Example:

```
(char->integer #\A)   =>  65
```
MD)

CB
reg char->integer

proc ::constcl::char->integer {char} {
  return [MkNumber [scan [$char char] %c]]
}
CB

PR(
integer->char (public);int int -> char
PR)

MD(
Example:

```
(integer->char 97)   =>  #\a
```
MD)

CB
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
CB

TT(

::tcltest::test characters-1.16 {try char-upcase?} -body {
    pew {(char->integer #\A)}
    pew {(integer->char 97)}
} -output "65\n#\\a\n"

TT)

MD(
__char-upcase__

__char-downcase__

`char-upcase` and `char-downcase` alter the case of a character.
MD)

PR(
char-upcase, char-downcase (public);char char -> char
PR)

CB
reg char-upcase

proc ::constcl::char-upcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [MkChar [
    ::string toupper [$char value]]]
}
CB

TT(

::tcltest::test characters-1.17 {try char-upcase} -body {
    pew {(char-upcase #\A)}
    pew {(char-upcase #\a)}
    pew {(char-upcase #\space)}
} -output "#\\A\n#\\A\n#\\space\n"

TT)


CB
reg char-downcase

proc ::constcl::char-downcase {char} {
  check {char? $char} {
    CHAR expected\n([pn] [$char show])
  }
  return [MkChar [
    ::string tolower [$char value]]]
}
CB

TT(

::tcltest::test characters-1.18 {try char-downcase?} -body {
    pew {(char-downcase #\A)}
    pew {(char-downcase #\a)}
    pew {(char-downcase #\space)}
} -output "#\\a\n#\\a\n#\\space\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
