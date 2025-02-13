
MD(
### Strings

Procedures for dealing with strings of characters.

__String__ class
MD)

CB
oo::class create ::constcl::String {
  superclass ::constcl::NIL
  variable data constant
  constructor {v} {
    set len [::string length $v]
    set vsa [::constcl::vsAlloc $len]
    set idx $vsa
    foreach elt [split $v {}] {
      if {$elt eq " "} {
        set c #\\space
      } elseif {$elt eq "\n"} {
        set c #\\newline
      } else {
        set c #\\$elt
      }
      lset ::constcl::vectorSpace $idx [::constcl::MkChar $c]
      incr idx
    }
    set data [
      ::constcl::cons [
        ::constcl::MkNumber $vsa] [::constcl::MkNumber $len]]
    set constant 0
  }
  method = {str} {
    ::string equal [my value] [$str value]
  }
  method cmp {str} {
    ::string compare [my value] [$str value]
  }
  method length {} {
    ::constcl::cdr $data
  }
  method ref {k} {
    set k [$k numval]
    if {$k < 0 || $k >= [[my length] numval]} {
      ::error "index out of range\n$k"
    }
    lindex [my store] $k
  }
  method store {} {
    set base [[::constcl::car $data] numval]
    set end [expr {[[my length] numval] + $base - 1}]
    lrange $::constcl::vectorSpace $base $end
  }
  method value {} {
    join [lmap c [my store] {$c char}] {}
  }
  method set! {k c} {
    if {[my constant]} {
      ::error "string is constant"
    } else {
      set k [$k numval]
      if {$k < 0 || $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[::constcl::car $data] numval]
      lset ::constcl::vectorSpace $k+$base $c
    }
    return [self]
  }
  method fill! {c} {
    if {[my constant]} {
      ::error "string is constant"
    } else {
      set base [[::constcl::car $data] numval]
      set len [[my length] numval]
      for {set idx $base} {$idx < $len+$base} {incr idx} {
        lset ::constcl::vectorSpace $idx $c
      }
    }
    return [self]
  }
  method substring {from to} {
    join [lmap c [lrange [my store] [$from numval] [$to numval]] {$c char}] {}
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method write {handle} {
    puts -nonewline $handle "\"[my value]\""
  }
  method display {handle} {
    puts -nonewline $handle [my value]
  }
  method show {} {
    format "\"[my value]\""
  }
}

interp alias {} MkString {} ::constcl::String new
CB

PR(
string? (public);val val -> bool
PR)

CB
reg string? ::constcl::string?

proc ::constcl::string? {val} {
  if {[info object isa typeof $val ::constcl::String]} {
    return #t
  } elseif {[info object isa typeof [interp alias {} $val] ::constcl::String]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.0 {try string?} -body {
    pep {(string? "foo bar")}
    pep {(string? 'foo-bar)}
} -output "#t\n#f\n"

TT)

MD(
__make-string__

`make-string` creates a string of _k_ characters, optionally filled with _char_
characters. If _char_ is omitted, the string will be filled with space characters.
MD)

PR(
make-string (public);k num ?char? char -> str
PR)

MD(
Example:

```
(let ((k 5)) (make-string k))                   =>  "     "
(let ((k 5) (char #\A)) (make-string k char))   =>  "AAAAA"
```
MD)

CB
reg make-string ::constcl::make-string

proc ::constcl::make-string {k args} {
  if {[llength $args] == 0} {
    return [MkString [::string repeat " " [$k numval]]]
  } else {
    lassign $args char
    return [MkString [::string repeat [$char char] [$k numval]]]
  }
}
CB

TT(

::tcltest::test strings-1.1 {try make-string} -body {
    pep {(make-string 5 #\x)}
} -output "\"xxxxx\"\n"

TT)

MD(
__string__

`string` constructs a string from a number of Lisp characters.
MD)

PR(
string (public);args chars -> str
PR)

MD(
Example:

```
(string #\f #\o #\o)   =>  "foo"
```
MD)

CB
reg string ::constcl::string

proc ::constcl::string {args} {
  set str {}
  foreach char $args {
    check {::constcl::char? $char} {
      CHAR expected\n([pn] [lmap c $args {$c show}])
    }
    ::append str [$char char]
  }
  return [MkString $str]
}
CB

TT(

::tcltest::test strings-1.2 {try string} -body {
    pep {(string #\f #\o #\o)}
} -output "\"foo\"\n"

::tcltest::test strings-1.3 {try string} -body {
    pep {(string #\f #\o 'a #\o)}
} -returnCodes error -result "CHAR expected\n(string {#\\f} {#\\o} a {#\\o})"

TT)

MD(
__string-length__

`string-length` reports a string's length.
MD)

PR(
string-length (public);str str -> num
PR)

MD(
Example:

```
(string-length "foobar")   => 6
```
MD)

CB
reg string-length ::constcl::string-length

proc ::constcl::string-length {str} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkNumber [[$str length] numval]]
}
CB

TT(

::tcltest::test strings-1.4 {try string-length} -body {
    pep {(string-length "foo bar")}
} -output "7\n"

TT)

MD(
__string-ref__

`string-ref` yields the _k_-th character (0-based) in _str_.
MD)

PR(
string-ref (public);str str k num -> char
PR)

MD(
Example:

```
(string-ref "foobar" 3)   => #\b
```
MD)

CB
reg string-ref ::constcl::string-ref

proc ::constcl::string-ref {str k} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show] [$k show])
  }
  check {::constcl::number? $k} {
    Exact INTEGER expected\n([pn] [$str show] [$k show])
  }
  return [$str ref $k]
}
CB

TT(

::tcltest::test strings-1.5 {try string-ref} -body {
    pep {(string-ref "foo bar" 4)}
} -output "#\\b\n"

TT)

MD(
__string-set!__

`string-set!` replaces the character at _k_ with _char_ in a non-constant string.
MD)

PR(
string-set! (public);str str k num char char -> str
PR)

MD(
Example:

```
(let ((str (string #\f #\o #\o)) (k 2) (char #\x)) (string-set! str k char))   =>  "fox"
```
MD)

CB
reg string-set!

proc ::constcl::string-set! {str k char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$k show] [$char show])
  }
  check {number? $k} {
    Exact INTEGER expected\n([pn] [$str show] [$k show] [$char show])
  }
  check {char? $char} {
    CHAR expected\n([pn] [$str show] [$k show] [$char show])
  }
  $str set! $k $char
  return $str
}
CB

TT(

::tcltest::test strings-1.6 {try string-set!} -body {
    pep {(string-set! (string #\f #\o #\o) 0 #\x)}
} -output "\"xoo\"\n"

::tcltest::test strings-1.7 {try string-set!} -body {
    pep {(define f (lambda () (make-string 3 #\*)))}
    pep {(define g (lambda () "***"))}
    pep {(string-set! (f) 0 #\?)}
} -output "\"?**\"\n"

::tcltest::test strings-1.8 {try string-set!} -body {
    pep {(string-set! (g) 0 #\?)}
} -returnCodes error -result "string is constant"

TT)

MD(
__string=?__, __string-ci=?__

__string<?__, __string-ci<?__

__string>?__, __string-ci>?__

__string<=?__, __string-ci<=?__

__string>=?__, __string-ci>=?__

`string=?`, `string<?`, `string>?`, `string<=?`, `string>=?` and their
case insensitive variants `string-ci=?`, `string-ci<?`, `string-ci>?`,
`string-ci<=?`, `string-ci>=?` compare strings.
MD)

PR(
string=?, string<?, string>?, string<=?, string>=? (public);str1 str str2 str -> bool
PR)

PR(
string-ci=?, string-ci<?, string-ci>?, string-ci<=?, string-ci>=? (public);str1 str str2 str -> bool
PR)

CB
reg string=? ::constcl::string=?

proc ::constcl::string=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] eq [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.9 {try string=?} -body {
    pep {(string=? "foo bar" "faa bor")}
    pep {(string=? "foo bar" "foo bar")}
    pep {(string=? "foo bar" "Foo bar")}
} -output "#f\n#t\n#f\n"

TT)

CB
reg string-ci=? ::constcl::string-ci=?

proc ::constcl::string-ci=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] eq [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.10 {try string-ci=?} -body {
    pep {(string-ci=? "foo bar" "faa bor")}
    pep {(string-ci=? "foo bar" "foo bar")}
    pep {(string-ci=? "foo bar" "Foo bar")}
} -output "#f\n#t\n#t\n"

TT)

CB
reg string<? ::constcl::string<?

proc ::constcl::string<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] < [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.11 {try string<?} -body {
    pep {(string<? "bar" "car")}
    pep {(string<? "bar" "bar")}
    pep {(string<? "bar" "aar")}
} -output "#t\n#f\n#f\n"

TT)

CB
reg string-ci<? ::constcl::string-ci<?

proc ::constcl::string-ci<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] < [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.12 {try string-ci<?} -body {
    pep {(string-ci<? "bar" "Car")}
    pep {(string-ci<? "bar" "Bar")}
    pep {(string-ci<? "bar" "Aar")}
} -output "#t\n#f\n#f\n"

TT)

CB
reg string>? ::constcl::string>?

proc ::constcl::string>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] > [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.13 {try string>?} -body {
    pep {(string>? "bar" "car")}
    pep {(string>? "bar" "bar")}
    pep {(string>? "bar" "aar")}
} -output "#f\n#f\n#t\n"

TT)

CB
reg string-ci>? ::constcl::string-ci>?

proc ::constcl::string-ci>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] > [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.14 {try string-ci>?} -body {
    pep {(string-ci>? "bar" "Car")}
    pep {(string-ci>? "bar" "Bar")}
    pep {(string-ci>? "bar" "Aar")}
} -output "#f\n#f\n#t\n"

TT)

CB
reg string<=? ::constcl::string<=?

proc ::constcl::string<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] <= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.15 {try string<=?} -body {
    pep {(string<=? "bar" "car")}
    pep {(string<=? "bar" "bar")}
    pep {(string<=? "bar" "aar")}
} -output "#t\n#t\n#f\n"

TT)

CB
reg string-ci<=? ::constcl::string-ci<=?

proc ::constcl::string-ci<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] <= [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.16 {try string-ci<=?} -body {
    pep {(string-ci<=? "bar" "Car")}
    pep {(string-ci<=? "bar" "Bar")}
    pep {(string-ci<=? "bar" "Aar")}
} -output "#t\n#t\n#f\n"

TT)

CB
reg string>=? ::constcl::string>=?

proc ::constcl::string>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[$str1 value] >= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.17 {try string>=?} -body {
    pep {(string>=? "bar" "car")}
    pep {(string>=? "bar" "bar")}
    pep {(string>=? "bar" "aar")}
} -output "#f\n#t\n#t\n"

TT)

CB
reg string-ci>=? ::constcl::string-ci>=?

proc ::constcl::string-ci>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] [$str2 show])
  }
  if {[::string tolower [$str1 value]] >= [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-1.18 {try string-ci>=?} -body {
    pep {(string-ci>=? "bar" "Car")}
    pep {(string-ci>=? "bar" "Bar")}
    pep {(string-ci>=? "bar" "Aar")}
} -output "#f\n#t\n#t\n"

TT)

MD(
__substring__

`substring` yields the substring of _str_ that starts at _start_ and ends at _end_.
MD)

PR(
substring (public);str str start num end num -> str
PR)

MD(
Example:

```
(substring "foobar" 2 4)   => "oba"
```
MD)

CB
reg substring ::constcl::substring

proc ::constcl::substring {str start end} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$start show] [$end show])
  }
  check {number? $start} {
    NUMBER expected\n([pn] [$str show] [$start show] [$end show])
  }
  check {number? $end} {
    NUMBER expected\n([pn] [$str show] [$start show] [$end show])
  }
  return [MkString [$str substring $start $end]]
}
CB

TT(

::tcltest::test strings-1.19 {try substring} -body {
    pep {(substring "foo bar" 0 2)}
} -output "\"foo\"\n"

TT)

MD(
__string-append__

`string-append` joins strings together.
MD)

PR(
string-append (public);args strs -> str
PR)

MD(
Example:

```
(string-append "foo" "bar")   =>  "foobar"
```
MD)

CB
reg string-append ::constcl::string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {$arg value}]]
}
CB

TT(

::tcltest::test strings-1.20 {try string-append} -body {
    pep {(string-append "foo" " bar")}
} -output "\"foo bar\"\n"

TT)

MD(
__string->list__

`string->list` converts a string to a Lisp list of characters.
MD)

PR(
string->list (public);str str -> lchars
PR)

MD(
Example:

```
(string->list "foo")   =>  (#\f #\o #\o)
```
MD)

CB
reg string->list ::constcl::string->list

proc ::constcl::string->list {str} {
  list {*}[$str store]
}
CB

TT(

::tcltest::test strings-1.21 {try string->list} -body {
    pep {(string->list "foo")}
} -output "(#\\f #\\o #\\o)\n"

TT)

MD(
__list->string__

`list->string` converts a Lisp list of characters to a string.
MD)

PR(
list->string (public);list lchars -> str
PR)

MD(
Example:

```
(list->string '(#\1 #\2 #\3))   => "123"
```
MD)

CB
reg list->string ::constcl::list->string

proc ::constcl::list->string {list} {
  MkString [::append --> {*}[lmap c [splitlist $list] {$c char}]]
}
CB

TT(

::tcltest::test strings-1.22 {try list->string} -body {
    pep {(list->string '(#\f #\o #\o))}
} -output "\"foo\"\n"

TT)

MD(
__string-copy__

`string-copy` makes a copy of a string.
MD)

PR(
string-copy (public);str str -> str
PR)

MD(
Example:

```
(let ((str (string-copy "abc")) (k 0) (char #\x)) (string-set! str k char))            =>  "xbc"
```
MD)

CB
reg string-copy ::constcl::string-copy

proc ::constcl::string-copy {str} {
  check {string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkString [$str value]]
}
CB

TT(

::tcltest::test strings-1.23 {try string-copy} -body {
    pep {(define x (string-copy "foo"))}
    pep {(string-set! x 0 #\x)}
    pep {(define y "foobar")}
    pep {(define z (string-copy y))}
    pep {(eq? y z)}
    pep {(equal? y z)}
} -output "\"xoo\"\n#f\n#t\n"

TT)

MD(
__string-fill!__

`string-fill!` _str_ _char_ fills a non-constant string with _char_.
MD)

PR(
string-fill! (public);str str char char -> str
PR)

MD(
Example:

```
(let ((str (string-copy "foobar")) (char #\X)) (string-fill! str char))   =>  "XXXXXX"
```
MD)

CB
reg string-fill! ::constcl::string-fill!

proc ::constcl::string-fill! {str char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$char show])
  }
  $str fill! $char
  return $str
}
CB

TT(

::tcltest::test strings-1.24 {try string-fill!} -body {
    pep {(define x (string-copy "foo"))}
    pep {(string-fill! x #\x)}
} -output "\"xxx\"\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
