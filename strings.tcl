
MD(
### Strings

Procedures for dealing with strings of characters.

__String__ class

Strings have the internal representation of a vector of character objects, with
the data elements of the vector address of the first element, and the length of
the vector. External representation is surrounded by double quotes, with double
quotes and backslashes within the string escaped with a backslash.

As a ConsTcl extension, a backslash+n pair in the external representation is
stored as a newline character. It is restored to backslash+n on write.
MD)

CB
oo::class create ::constcl::String {
  superclass ::constcl::NIL
  variable data constant
  constructor {v} {
    set v [string map {\\\\ \\ \\\" \" \\n \n} $v]
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
      lset ::constcl::vectorSpace $idx \
        [::constcl::MkChar $c]
      incr idx
    }
    set data [
      ::constcl::cons [N $vsa] [N $len]]
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
    set end [expr {[[my length] numval] +
      $base - 1}]
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
      if {$k < 0 ||
        $k >= [[my length] numval]} {
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
      for {set idx $base} \
        {$idx < $len+$base} \
        {incr idx} {
        lset ::constcl::vectorSpace $idx $c
      }
    }
    return [self]
  }
  method substring {from to} {
    join [lmap c [lrange [my store] \
      [$from numval] [$to numval]] {$c char}] {}
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method external {} {
    return "\"[
      string map {\\ \\\\ \" \\\" \n \\n} [my value]]\""
  }
  method write {handle} {
    puts -nonewline $handle [my external]
  }
  method display {handle} {
    puts -nonewline $handle [my value]
  }
  method show {} {
    my external
  }
}

interp alias {} ::constcl::MkString \
  {} ::constcl::String new
CB

TT(
::tcltest::test strings-1.0 {try reading a string} -body {
    set o [p {"foo bar baz"}]
    puts [$o value]
} -output {foo bar baz
}

::tcltest::test strings-1.1 {try reading a string} -body {
    set o [p {"foo \"bar\" baz\nqux\\bod"}]
    puts [$o value]
} -output {foo "bar" baz
qux\bod
}

::tcltest::test strings-1.2 {try printing a read string} -body {
    pw {"foo \"bar\" baz\nqux\\bod"}
} -output {"foo \"bar\" baz\nqux\\bod"
}
TT)

PR(
string? (public);val val -> bool
PR)

CB
reg string?

proc ::constcl::string? {val} {
  typeof? $val String
}
CB

TT(

::tcltest::test strings-2.0 {try string?} -body {
    pew {(string? "foo bar")}
    pew {(string? 'foo-bar)}
} -output "#t\n#f\n"

TT)

MD(
__make-string__

`make-string` creates a string of **k** characters, optionally filled with **char**
characters. If **char** is omitted, the string will be filled with space characters.
MD)

PR(
make-string (public);k num ?char? char -> str
PR)

MD(
Example:

```
(let ((k 5))
  (make-string k))        =>  "     "
(let ((k 5) (char #\A))
  (make-string k char))   =>  "AAAAA"
```
MD)

CB
reg make-string

proc ::constcl::make-string {k args} {
  if {[llength $args] == 0} {
    return [MkString [::string repeat " " \
      [$k numval]]]
  } else {
    lassign $args char
    return [MkString [::string repeat \
      [$char char] [$k numval]]]
  }
}
CB

TT(

::tcltest::test strings-3.0 {try make-string} -body {
    pew {(make-string 5 #\x)}
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
reg string

proc ::constcl::string {args} {
  set str {}
  foreach char $args {
    check {::constcl::char? $char} {
      CHAR expected\n([pn] [lmap c $args \
        {$c show}])
    }
    ::append str [$char char]
  }
  return [MkString $str]
}
CB

TT(

::tcltest::test strings-4.0 {try string} -body {
    pew {(string #\f #\o #\o)}
} -output "\"foo\"\n"

::tcltest::test strings-4.1 {try string} -body {
    pew {(string #\f #\o 'a #\o)}
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
reg string-length

proc ::constcl::string-length {str} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkNumber [[$str length] numval]]
}
CB

TT(

::tcltest::test strings-5.0 {try string-length} -body {
    pew {(string-length "foo bar")}
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
reg string-ref

proc ::constcl::string-ref {str k} {
  check {::constcl::string? $str} {
    STRING expected\n([pn] [$str show] \
      [$k show])
  }
  check {::constcl::number? $k} {
    Exact INTEGER expected\n([pn] [$str show] \
      [$k show])
  }
  return [$str ref $k]
}
CB

TT(

::tcltest::test strings-6.0 {try string-ref} -body {
    pew {(string-ref "foo bar" 4)}
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
(let ((str (string #\f #\o #\o))
      (k 2)
      (char #\x))
  (string-set! str k char))         =>  "fox"
```
MD)

CB
reg string-set!

proc ::constcl::string-set! {str k char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] [$k show] \
      [$char show])
  }
  check {number? $k} {
    Exact INTEGER expected\n([pn] [$str show] \
      [$k show] [$char show])
  }
  check {char? $char} {
    CHAR expected\n([pn] [$str show] [$k show] \
      [$char show])
  }
  $str set! $k $char
  return $str
}
CB

TT(

::tcltest::test strings-7.0 {try string-set!} -body {
    pew {(string-set! (string #\f #\o #\o) 0 #\x)}
} -output "\"xoo\"\n"

::tcltest::test strings-7.1 {try string-set!} -body {
    pew {(define f (lambda () (make-string 3 #\*)))}
    pew {(define g (lambda () "***"))}
    pew {(string-set! (f) 0 #\?)}
} -output "\"?**\"\n"

::tcltest::test strings-7.2 {try string-set!} -body {
    pew {(string-set! (g) 0 #\?)}
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
string-ci=?, string-ci<?, string-ci>? (public);str1 str str2 str -> bool
PR)

PR(
string-ci<=?, string-ci>=? (public);str1 str str2 str -> bool
PR)

CB
reg string=?

proc ::constcl::string=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] eq [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-8.0 {try string=?} -body {
    pew {(string=? "foo bar" "faa bor")}
    pew {(string=? "foo bar" "foo bar")}
    pew {(string=? "foo bar" "Foo bar")}
} -output "#f\n#t\n#f\n"

TT)

CB
reg string-ci=?

proc ::constcl::string-ci=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] eq
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-9.0 {try string-ci=?} -body {
    pew {(string-ci=? "foo bar" "faa bor")}
    pew {(string-ci=? "foo bar" "foo bar")}
    pew {(string-ci=? "foo bar" "Foo bar")}
} -output "#f\n#t\n#t\n"

TT)

CB
reg string<?

proc ::constcl::string<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] < [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-10.0 {try string<?} -body {
    pew {(string<? "bar" "car")}
    pew {(string<? "bar" "bar")}
    pew {(string<? "bar" "aar")}
} -output "#t\n#f\n#f\n"

TT)

CB
reg string-ci<?

proc ::constcl::string-ci<? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] <
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-11.0 {try string-ci<?} -body {
    pew {(string-ci<? "bar" "Car")}
    pew {(string-ci<? "bar" "Bar")}
    pew {(string-ci<? "bar" "Aar")}
} -output "#t\n#f\n#f\n"

TT)

CB
reg string>?

proc ::constcl::string>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] > [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-12.0 {try string>?} -body {
    pew {(string>? "bar" "car")}
    pew {(string>? "bar" "bar")}
    pew {(string>? "bar" "aar")}
} -output "#f\n#f\n#t\n"

TT)

CB
reg string-ci>?

proc ::constcl::string-ci>? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] >
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-13.0 {try string-ci>?} -body {
    pew {(string-ci>? "bar" "Car")}
    pew {(string-ci>? "bar" "Bar")}
    pew {(string-ci>? "bar" "Aar")}
} -output "#f\n#f\n#t\n"

TT)

CB
reg string<=?

proc ::constcl::string<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] <= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-14.0 {try string<=?} -body {
    pew {(string<=? "bar" "car")}
    pew {(string<=? "bar" "bar")}
    pew {(string<=? "bar" "aar")}
} -output "#t\n#t\n#f\n"

TT)

CB
reg string-ci<=?

proc ::constcl::string-ci<=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] <=
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-15.0 {try string-ci<=?} -body {
    pew {(string-ci<=? "bar" "Car")}
    pew {(string-ci<=? "bar" "Bar")}
    pew {(string-ci<=? "bar" "Aar")}
} -output "#t\n#t\n#f\n"

TT)

CB
reg string>=?

proc ::constcl::string>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[$str1 value] >= [$str2 value]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-16.0 {try string>=?} -body {
    pew {(string>=? "bar" "car")}
    pew {(string>=? "bar" "bar")}
    pew {(string>=? "bar" "aar")}
} -output "#f\n#t\n#t\n"

TT)

CB
reg string-ci>=?

proc ::constcl::string-ci>=? {str1 str2} {
  check {string? $str1} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  check {string? $str2} {
    STRING expected\n([pn] [$str1 show] \
      [$str2 show])
  }
  if {[::string tolower [$str1 value]] >=
      [::string tolower [$str2 value]]} {
    return #t
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test strings-17.0 {try string-ci>=?} -body {
    pew {(string-ci>=? "bar" "Car")}
    pew {(string-ci>=? "bar" "Bar")}
    pew {(string-ci>=? "bar" "Aar")}
} -output "#f\n#t\n#t\n"

TT)

MD(
__substring__

`substring` yields the substring of **str** that starts at **start** and ends at **end**.
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
reg substring

proc ::constcl::substring {str start end} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  check {number? $start} {
    NUMBER expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  check {number? $end} {
    NUMBER expected\n([pn] [$str show] \
      [$start show] [$end show])
  }
  return [MkString [$str substring $start $end]]
}
CB

TT(

::tcltest::test strings-18.0 {try substring} -body {
    pew {(substring "foo bar" 0 2)}
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
reg string-append

proc ::constcl::string-append {args} {
    MkString [::append --> {*}[lmap arg $args {
      $arg value
    }]]
}
CB

TT(

::tcltest::test strings-19.0 {try string-append} -body {
    pew {(string-append "foo" " bar")}
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
reg string->list

proc ::constcl::string->list {str} {
  list {*}[$str store]
}
CB

TT(

::tcltest::test strings-20.0 {try string->list} -body {
    pew {(string->list "foo")}
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
reg list->string

proc ::constcl::list->string {list} {
  MkString [::append --> {*}[
    lmap c [splitlist $list] {$c char}]]
}
CB

TT(

::tcltest::test strings-21.0 {try list->string} -body {
    pew {(list->string '(#\f #\o #\o))}
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
(let ((str (string-copy "abc"))
      (k 0)
      (char #\x))
  (string-set! str k char))       =>  "xbc"
```
MD)

CB
reg string-copy

proc ::constcl::string-copy {str} {
  check {string? $str} {
    STRING expected\n([pn] [$str show])
  }
  return [MkString [$str value]]
}
CB

TT(

::tcltest::test strings-22.0 {try string-copy} -body {
    pew {(define x (string-copy "foo"))}
    pew {(string-set! x 0 #\x)}
    pew {(define y "foobar")}
    pew {(define z (string-copy y))}
    pew {(eq? y z)}
    pew {(equal? y z)}
} -output "\"xoo\"\n#f\n#t\n"

TT)

MD(
__string-fill!__

`string-fill!` **str** **char** fills a non-constant string with **char**.
MD)

PR(
string-fill! (public);str str char char -> str
PR)

MD(
Example:

```
(let ((str (string-copy "foobar"))
      (char #\X))
  (string-fill! str char))           =>  "XXXXXX"
```
MD)

CB
reg string-fill!

proc ::constcl::string-fill! {str char} {
  check {string? $str} {
    STRING expected\n([pn] [$str show] \
      [$char show])
  }
  $str fill! $char
  return $str
}
CB

TT(

::tcltest::test strings-23.0 {try string-fill!} -body {
    pew {(define x (string-copy "foo"))}
    pew {(string-fill! x #\x)}
} -output "\"xxx\"\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
