
MD(
### Pairs and lists

List processing is another of Lisp's great strengths.

__Pair__ class
MD)

CB
catch { ::constcl::Pair destroy }

oo::class create ::constcl::Pair {
  superclass ::constcl::NIL
  variable car cdr constant
  constructor {a d} {
    set car $a
    set cdr $d
    set constant 0
  }
  method name {} {}
  method value {} {
    my show
  }
  method car {} {
    set car
  }
  method cdr {} {
    set cdr
  }
  method set-car! {val} {
    ::constcl::check {my mutable?} {
      Can't modify a constant pair
    }
    set car $val
    self
  }
  method set-cdr! {val} {
    ::constcl::check {my mutable?} {
      Can't modify a constant pair
    }
    set cdr $val
    self
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    return $constant
  }
  method mutable? {} {
    expr {$constant ? "#f" : "#t"}
  }
  method write {handle} {
    puts -nonewline $handle "("
    ::constcl::write-pair $handle [self]
    puts -nonewline $handle ")"
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "(%s)" [::constcl::show-pair [self]]
  }
}


interp alias {} ::constcl::MkPair \
  {} ::constcl::Pair new
CB

TT(

::tcltest::test pairslists-1.0 {playing with lists} -body {
    pew {(define x (list 'a 'b 'c))}
    pew {(define y x)}
    pew {y}
    pew {(list? y)}
} -output "(a b c)\n#t\n"

::tcltest::test pairslists-1.2 {playing with lists} -body {
    pew {(set-cdr! x 4)}
    pew {x}
} -output "(a . 4)\n(a . 4)\n"

::tcltest::test pairslists-1.3 {playing with lists} -body {
    pew {(eqv? x y)}
    pew {y}
} -output "#t\n(a . 4)\n"

::tcltest::test pairslists-1.4 {playing with lists} -body {
    pew {(eqv? x y)}
    pew {y}
    pew {(list? y)}
} -output "#t\n(a . 4)\n#f\n"

TT)

MD(
__pair?__
MD)

PR(
pair? (public);val val -> bool
PR)

CB
reg pair?

proc ::constcl::pair? {val} {
  typeof? $val Pair
}
CB

TT(

::tcltest::test pairslists-2.0 {try pair?} -body {
    pew {(pair? '(a . b))}
    pew {(pair? '(a b c))}
    pew {(pair? '())}
} -output "#t\n#t\n#f\n"

TT)

MD(
__show-pair__

Helper procedure to make a string representation of a list.
MD)

PR(
show-pair (internal);pair pair -> tstr
PR)

CB
proc ::constcl::show-pair {pair} {
  # take an object and print the car
  # and the cdr of the stored value
  set str {}
  set a [car $pair]
  set d [cdr $pair]
  # print car
  ::append str [$a show]
  if {[pair? $d] ne "#f"} {
    # cdr is a cons pair
    ::append str " "
    ::append str [show-pair $d]
  } elseif {[null? $d] ne "#f"} {
    # cdr is nil
    return $str
  } else {
    # it is an atom
    ::append str " . "
    ::append str [$d show]
  }
  return $str
}
CB

TT(

::tcltest::test pairslists-3.0 {try show-pair} -body {
    puts [::constcl::show-pair [p {(a . b)}]]
    puts [::constcl::show-pair [p {(a b c)}]]
    puts [::constcl::show-pair [p {(a b . c)}]]
    puts [::constcl::show-pair [p {(a)}]]
} -output "a . b\na b c\na b . c\na\n"

TT)

MD(
__cons__

`cons` joins two values in a pair; useful in many operations such as pushing
a new value onto a list.
MD)

PR(
cons (public);car val cdr val -> pair
PR)

MD(
Example:

```
(cons 'a 'b)              =>  (a . b)
(cons 'a nil)             =>  (a)
(cons 'a (cons 'b nil))   =>  (a b)
```

![a small schematic to make it clearer](/images/consing.png)

MD)

CB
reg cons

proc ::constcl::cons {car cdr} {
  MkPair $car $cdr
}
CB

TT(

::tcltest::test pairslists-4.0 {try cons} -body {
    pew {(cons 'a '())}
    pew {(cons '(a) '(b c d))}
    pew {(cons "a" '(b c))}
    pew {(cons 'a 3)}
    pew {(cons '(a b) 'c)}
} -output "(a)\n((a) b c d)\n(\"a\" b c)\n(a . 3)\n((a b) . c)\n"

TT)

MD(
__car__

`car` gets the contents of the first cell in a pair.
MD)

PR(
car (public);pair pair -> val
PR)

MD(
Example:

```
(car '(a b))   =>  a
```
MD)

CB
reg car

proc ::constcl::car {pair} {
  $pair car
}
CB

TT(

::tcltest::test pairslists-5.0 {try car} -body {
    pew {(car '(a b c))}
    pew {(car '((a) b c d))}
    pew {(car '(1 . 2))}
} -output "a\n(a)\n1\n"

::tcltest::test pairslists-5.1 {try car} -body {
    pew {(car '())}
} -returnCodes error -result "PAIR expected"

TT)

MD(
__cdr__

`cdr` gets the contents of the second cell in a pair.
MD)

PR(
cdr (public);pair pair -> val
PR)

MD(
Example:

```
(cdr '(a b))   =>  (b)
```
MD)

CB
reg cdr

proc ::constcl::cdr {pair} {
  $pair cdr
}
CB

TT(

::tcltest::test pairslists-6.0 {try cdr} -body {
    pew {(cdr '((a) b c d))}
    pew {(cdr '(1 . 2))}
} -output "(b c d)\n2\n"

::tcltest::test pairslists-6.1 {try cdr} -body {
    pew {(cdr '())}
} -returnCodes error -result "PAIR expected"

TT)

MD(
__caar__ to __cddddr__

`car` and `cdr` can be combined to form 28 composite access
operations.
MD)

CB
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
    reg c${ads}r

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
CB

MD(
__set-car!__

`set-car!` sets the contents of the first cell in a pair.
MD)

PR(
set-car! (public);pair pair val val -> pair
PR)

MD(
Example:

```
(let ((pair (cons 'a 'b)) (val 'x))
  (set-car! pair val))                =>  (x . b)
```
MD)

CB
reg set-car!

proc ::constcl::set-car! {pair val} {
  $pair set-car! $val
}
CB

TT(

::tcltest::test pairslists-7.0 {try set-car!} -body {
    pew {(define (f) (list 'not-a-constant-list)))}
    pew {(define (g) '(constant-list)))}
    pew {(set-car! (f) 3)}
} -output "(3)\n"

::tcltest::test pairslists-7.1 {try set-car!} -body {
    pew {(set-car! (g) 3)}
} -returnCodes error -result "Can't modify a constant pair"

TT)

MD(
__set-cdr!__

`set-cdr!` sets the contents of the second cell in a pair.
MD)

PR(
set-cdr! (public);pair pair val val -> pair
PR)

MD(
Example:

```
(let ((pair (cons 'a 'b)) (val 'x))
  (set-cdr! pair val))                =>  (a . x)
```
MD)

CB
reg set-cdr!

proc ::constcl::set-cdr! {pair val} {
  $pair set-cdr! $val
}
CB

TT(

::tcltest::test pairslists-8.0 {try set-cdr!} -body {
    pew {(define (f) (list 'not-a-constant-list)))}
    pew {(define (g) '(constant-list)))}
    pew {(set-cdr! (f) 3)}
} -output "(not-a-constant-list . 3)\n"

::tcltest::test pairslists-8.1 {try set-cdr!} -body {
    pew {(set-cdr! (g) 3)}
} -returnCodes error -result "Can't modify a constant pair"

TT)

MD(
__list?__

The `list?` predicate tests if a pair is part of a proper list, one that
ends with NIL.
MD)

PR(
list? (public);val val -> bool
PR)

CB
reg list?

proc ::constcl::list? {val} {
  set visited {}
  if {[null? $val] ne "#f"} {
      return #t
  } elseif {[pair? $val] ne "#f"} {
      return [listp $val]
  } else {
      return #f
  }
}
CB

PR(
listp (internal);pair pair -> bool
PR)

CB
proc ::constcl::listp {pair} {
  upvar visited visited
  if {$pair in $visited} {
    return #f
  }
  lappend visited $pair
  if {[null? $pair] ne "#f"} {
    return #t
  } elseif {[pair? $pair] ne "#f"} {
    return [listp [cdr $pair]]
  } else {
    return #f
  }
}
CB

TT(

::tcltest::test pairslists-9.0 {try list?} -body {
    pew {(list? '(a b c))}
    pew {(list? '())}
    pew {(list? '(a . b))}
    pew {(list? #\A)}
} -output "#t\n#t\n#f\n#f\n"

::tcltest::test pairslists-9.1 {try list?} -body {
    # "bug": list is infinite and list? must detect that
    pew {(let ((x (list 'a)))
          (set-cdr! x x)
          (list? x))}
} -output "#f\n"

TT)

MD(
__list__

`list` constructs a Lisp list from a number of values.
MD)

PR(
list (public);args vals -> lvals
PR)

MD(
Example:

```
(list 1 2 3)   =>  (1 2 3)
```
MD)

CB
reg list

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
CB

TT(

::tcltest::test pairslists-10.0 {try list} -body {
    pew {(list 'a (+ 3 4) 'c)}
    pew {(list)}
} -output "(a 7 c)\n()\n"

TT)

MD(
__length__

`length` reports the length of a Lisp list.
MD)

PR(
length (public);pair pair -> num
PR)

MD(
Example:

```
(length '(a b c d))   =>  4
```
MD)

CB
reg length

proc ::constcl::length {pair} {
  check {list? $pair} {
    LIST expected\n([pn] lst)
  }
  MkNumber [length-helper $pair]
}
CB

PR(
length-helper (internal);pair pair -> tnum
PR)

CB
proc ::constcl::length-helper {pair} {
  if {[null? $pair] ne "#f"} {
    return 0
  } else {
    return [expr {1 +
      [length-helper [cdr $pair]]}]
  }
}
CB

TT(

::tcltest::test pairslists-11.0 {try length} -body {
    pew {(length '(a b c))}
    pew {(length '(a (b) (c d e)))}
    pew {(length '())}
} -output "3\n3\n0\n"

TT)

MD(
__append__

`append` joins lists together.
MD)

MD(
Example:

```
(append '(a b) '(c d))   =>  (a b c d)
```
MD)

PR(
append (public);args lists -> lvals
PR)

CB
reg append

proc ::constcl::append {args} {
  set prev [lindex $args end]
  foreach r [lreverse [lrange $args 0 end-1]] {
    check {list? $r} {
      LIST expected\n([pn] [$r show])
    }
    set prev [copy-list $r $prev]
  }
  set prev
}
CB

PR(
copy-list (internal);pair pair next lvals -> lvals
PR)

CB
proc ::constcl::copy-list {pair next} {
  # TODO only fresh conses in the direct chain to NIL
  if {[null? $pair] ne "#f"} {
    set next
  } elseif {[null? [cdr $pair]] ne "#f"} {
    cons [car $pair] $next
  } else {
    cons [car $pair] [copy-list [cdr $pair] $next]
  }
}
CB

TT(
::tcltest::test pairslists-12.0 {try append} -body {
    pew "(append '(a b) '(c) '(d e))"
    pew {(append '(x) '(y))}
    pew {(append '(a) '(b c d))}
    pew {(append '(a (b)) '((c)))}
    pew {(append '(a b) '(c . d))}
    pew {(append '() 'a)}
} -output "(a b c d e)\n(x y)\n(a b c d)\n(a (b) (c))\n(a b c . d)\na\n"

::tcltest::test pairslists-12.1 {try append} -body {
    pew "(append '(a b) 'c '(d e))"
} -returnCodes error -result "LIST expected\n(append c)"

TT)

MD(
__reverse__

`reverse` produces a reversed copy of a Lisp list.
MD)

PR(
reverse (public);vals lvals -> lvals
PR)

MD(
Example:

```
(reverse '(a b c))   =>  (c b a)
```
MD)

CB
reg reverse

proc ::constcl::reverse {vals} {
  list {*}[lreverse [splitlist $vals]]
}
CB

TT(

::tcltest::test pairslists-13.0 {try reverse} -body {
    pew {(reverse '(a b c))}
    pew {(reverse '(a (b c) d (e (f))))}
} -output "(c b a)\n((e (f)) d (b c) a)\n"

TT)

MD(
__list-tail__

Given a list index, `list-tail` yields the sublist starting from that index.
MD)

PR(
list-tail (public);vals lvals k num -> lvals
PR)

MD(
Example:

```
(let ((lst '(a b c d e f)) (k 3))
  (list-tail lst k))                =>  (d e f)
```
MD)

CB
reg list-tail

proc ::constcl::list-tail {vals k} {
  if {[zero? $k] ne "#f"} {
    return $vals
  } else {
    list-tail [cdr $vals] [- $k #1]
  }
}
CB

TT(

::tcltest::test pairslists-14.0 {try list-tail} -body {
    pew {(list-tail '(a b c d) 2)}
} -output "(c d)\n"

TT)

MD(
__list-ref__

`list-ref` yields the list item at a given index.
MD)

PR(
list-ref (public);vals lvals k num -> val
PR)

MD(
Example:

```
(let ((lst '(a b c d e f)) (k 3))
  (list-ref lst k))                 =>  d
```
MD)

CB
reg list-ref

proc ::constcl::list-ref {vals k} {
  car [list-tail $vals $k]
}
CB

TT(

::tcltest::test pairslists-15.0 {try list-ref} -body {
    pew {(list-ref '(a b c d) 2)}
} -output "c\n"

TT)

MD(
__memq__

__memv__

__member__

`memq`, `memv`, and `member` return the sublist starting with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison.
MD)

PR(
memq (public);val1 val val2 lvals -> lvfalse
PR)

MD(
Example:

```
(let ((lst '(a b c d e f)) (val 'd))
  (memq val lst))                      =>  (d e f)
```
MD)

CB
reg memq

proc ::constcl::memq {val1 val2} {
  return [member-proc eq? $val1 $val2]
}
CB

TT(

::tcltest::test pairslists-16.0 {try memq, memv} -body {
    pew {(memq 'a '(a b c))}
    pew {(memq 'b '(a b c))}
    pew {(memq 'a '(b c d))}
    pew {(memq (list 'a) '(b (a) c))}
    pew {(memq 101 '(100 101 102))}
    pew {(memv 101 '(100 101 102))}
} -output "(a b c)\n(b c)\n#f\n#f\n(101 102)\n(101 102)\n"

::tcltest::test pairslists-16.1 {try member} -body {
    pew {(member (list 'a) '(b (a) c))}
} -output "((a) c)\n"

TT)

PR(
memv (public);val1 val val2 lvals -> lvfalse
PR)

CB
reg memv

proc ::constcl::memv {val1 val2} {
  return [member-proc eqv? $val1 $val2]
}
CB

PR(
member (public);val1 val val2 lvals -> lvfalse
PR)

CB
reg member

proc ::constcl::member {val1 val2} {
  return [member-proc equal? $val1 $val2]
}
CB

PR(
member-proc (internal);epred epred val1 val val2 lvals -> lvfalse
PR)

CB

proc ::constcl::member-proc {epred val1 val2} {
  switch $epred {
    eq? { set name "memq" }
    eqv? { set name "memv" }
    equal? { set name "member" }
  }
  check {list? $val2} {
    LIST expected\n($name [$val1 show] [$val2 show])
  }
  if {[null? $val2] ne "#f"} {
    return #f
  } elseif {[pair? $val2] ne "#f"} {
    if {[$epred $val1 [car $val2]] ne "#f"} {
      return $val2
    } else {
      return [member-proc $epred $val1 [cdr $val2]]
    }
  }
}
CB

MD(
__assq__

__assv__

__assoc__

`assq`, `assv`, and `assoc` return the associative item marked with a given
item, or `#f` if there is none. They use `eq?`, `eqv?`, and `equal?`, 
respectively, for the comparison. They implement lookup in the form of lookup
table known as an association list, or _alist_.

Example:

```
(define e '((a 1) (b 2) (c 3)))
(assq 'a e)                       => (a 1)
```
MD)

PR(
assq (public);val1 val val2 alist -> lvfalse
PR)

CB
reg assq

proc ::constcl::assq {val1 val2} {
  return [assoc-proc eq? $val1 $val2]
}
CB

PR(
assv (public);val1 val val2 alist -> lvfalse
PR)


CB
reg assv

proc ::constcl::assv {val1 val2} {
  return [assoc-proc eqv? $val1 $val2]
}
CB

PR(
assoc (public);val1 val val2 alist -> lvfalse
PR)


CB
reg assoc

proc ::constcl::assoc {val1 val2} {
  return [assoc-proc equal? $val1 $val2]
}
CB

PR(
assoc-proc (internal);epred epred val1 val val2 alist -> lvfalse
PR)

CB
proc ::constcl::assoc-proc {epred val1 val2} {
  switch $epred {
    eq? { set name "assq" }
    eqv? { set name "assv" }
    equal? { set name "assoc" }
  }
  check {list? $val2} {
    LIST expected\n($name [$val1 show] [$val2 show])
  }
  if {[null? $val2] ne "#f"} {
    return #f
  } elseif {[pair? $val2] ne "#f"} {
    if {[pair? [car $val2]] ne "#f" && 
      [$epred $val1 [caar $val2]] ne "#f"} {
      return [car $val2]
    } else {
      return [assoc-proc $epred $val1 [cdr $val2]]
    }
  }
}
CB

TT(
::tcltest::test pairslists-17.0 {try member} -body {
    pew {(define e '((a 1) (b 2) (c 3)))}
    pew {(assq 'a e)}
    pew {(assq 'b e)}
    pew {(assq 'd e)}
    pew {(assq (list 'a) '(((a)) ((b)) ((c))))}
    pew {(assoc (list 'a) '(((a)) ((b)) ((c))))}
    pew {(assq 5 '((2 3) (5 7) (11 13)))}
    pew {(assv 5 '((2 3) (5 7) (11 13)))}
} -output "(a 1)\n(b 2)\n#f\n#f\n((a))\n(5 7)\n(5 7)\n"
TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
