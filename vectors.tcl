
MD(
### Vectors

Vectors are heterogenous structures of fixed length whose elements are indexed by integers. 
They are implemented as Tcl lists of Lisp values.

The number of elements that a vector contains (the _length_) is set when the vector is created.
Elements can be indexed by integers from zero to length minus one.

__Vector__ class
MD)

CB
oo::class create ::constcl::Vector {
  superclass ::constcl::NIL
  variable data constant
  constructor {v} {
    set len [llength $v]
    set vsa [::constcl::vsAlloc $len]
    set idx $vsa
    foreach elt $v {
      lset ::constcl::vectorSpace $idx $elt
      incr idx
    }
    set data [::constcl::cons [N $vsa] [N $len]]
    set constant 0
  }
  method baseadr {} {
    ::constcl::car $data
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
    set base [[my baseadr] numval]
    set end [expr {[[my length] numval] +
      $base - 1}]
    lrange $::constcl::vectorSpace $base $end
  }
  method value {} {
    my store
  }
  method set! {k obj} {
    if {[my constant]} {
      ::error "vector is constant"
    } else {
      set k [$k numval]
      if {$k < 0 || $k >= [[my length] numval]} {
        ::error "index out of range\n$k"
      }
      set base [[my baseadr] numval]
      lset ::constcl::vectorSpace $k+$base $obj
    }
    return [self]
  }
  method fill! {val} {
    if {[my constant]} {
      ::error "vector is constant"
    } else {
      set base [[my baseadr] numval]
      set len [[my length] numval]
      for {set idx $base} \
        {$idx < $len+$base} \
        {incr idx} {
        lset ::constcl::vectorSpace $idx $val
      }
    }
    return [self]
  }
  method mkconstant {} {
    set constant 1
  }
  method constant {} {
    set constant
  }
  method write {handle} {
    puts -nonewline $handle [my show]
  }
  method display {handle} {
    my write $handle
  }
  method show {} {
    format "#(%s)" [
      join [lmap val [my value] {$val show}]]
  }
}

interp alias {} ::constcl::MkVector \
  {} ::constcl::Vector new
CB

MD(
__vector?__
MD)

PR(
vector? (public);val val -> bool
PR)

CB
reg vector? ::constcl::vector?

proc ::constcl::vector? {val} {
  typeof? $val Vector
}
CB

TT(

::tcltest::test vectors-1.0 {try vector? (and make-vector, vector)} -body {
    pew {(vector? #(0 (2 2 2 2) "Anna"))}
    pew {(vector? (make-vector 3 #\X))}
    pew {(vector? (vector 'a 'b 'c))}
} -output "#t\n#t\n#t\n"

TT)

MD(
__make-vector__

`make-vector` creates a vector with a given length and optionally a fill value.
If a fill value isn't given, the empty list will be used.
MD)

PR(
make-vector? (public);k num ?fill? val -> vec
PR)

MD(
Example:

```
(let ((k 3))
  (make-vector k))        =>  #(() () ())
(let ((k 3) (fill #\A))
  (make-vector k fill))   =>  #(#\A #\A #\A)
```
MD)

CB
reg make-vector ::constcl::make-vector

proc ::constcl::make-vector {k args} {
  if {[llength $args] == 0} {
    set fill #NIL
  } else {
    lassign $args fill
  }
  MkVector [lrepeat [$k numval] $fill]
}
CB

MD(
__vector__

Given a number of Lisp values, `vector` creates a vector containing them.
MD)

PR(
vector (public);args vals -> vec
PR)

MD(
Example:

```
(vector 'a 'b 'c)   =>  #(a b c)
```
MD)

CB
reg vector ::constcl::vector

proc ::constcl::vector {args} {
  MkVector $args
}
CB

TT(

::tcltest::test vectors-1.1 {try vector} -body {
    pew {(vector 'a 'b 'c)}
    pew {(vector 0 '(2 2 2 2) "Anna")}
} -output "#(a b c)\n#(0 (2 2 2 2) \"Anna\")\n"

TT)

MD(
__vector-length__

`vector-length` returns the length of a vector.
MD)

PR(
vector-length (public);vec vec -> num
PR)

MD(
Example:

```
(vector-length #(a b c))   =>  3
```
MD)

CB
reg vector-length

proc ::constcl::vector-length {vec} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show])
  }
  return [$vec length]
}
CB

TT(

::tcltest::test vectors-1.2 {try vector-length} -body {
    pew {(vector-length (vector 'a 'b 'c))}
} -output "3\n"

TT)

MD(
__vector-ref__

`vector-ref` returns the element of **vec** at index **k** (0-based).
MD)

PR(
vector-ref (public);vec vec k num -> val
PR)

MD(
Example:

```
(let ((vec #(a b c)) (k 1))
  (vector-ref vec k))          =>  b
```
MD)

CB
reg vector-ref ::constcl::vector-ref

proc ::constcl::vector-ref {vec k} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] [$k show])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec show] [$k show])
  }
  return [$vec ref $k]
}
CB

TT(

::tcltest::test vectors-1.3 {try vector-ref} -body {
    pew {(vector-ref (vector 'a 'b 'c) 1)}
} -output "b\n"

TT)

MD(
__vector-set!__

`vector-set!`, for a non-constant vector, sets the element at index _k_ to _val_.
MD)

PR(
vector-set! (public);vec vec k num val val -> vec
PR)

MD(
Example:

```
(let ((vec #(a b c))
      (k 1)
      (val 'x))
  (vector-set! vec k val))      =>  *error*
(let ((vec (vector 'a 'b 'c))
      (k 1)
      (val 'x))
  (vector-set! vec k val))      =>  #(a x c)
```
MD)

CB
reg vector-set! ::constcl::vector-set!

proc ::constcl::vector-set! {vec k val} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] [$k show])
  }
  check {number? $k} {
    NUMBER expected\n([pn] [$vec show] [$k show])
  }
  return [$vec set! $k $val]
}
CB

TT(

::tcltest::test vectors-1.4 {try vector-set!} -body {
    pew {(define x (lambda () (vector 0 '(2 2 2 2) "Anna")))}
    pew {(vector-set! (x) 1 '(foo bar))}
} -output "#(0 (foo bar) \"Anna\")\n"

TT)

MD(
__vector->list__

`vector->list` converts a vector value to a Lisp list.
MD)

PR(
vector->list (public);vec vec -> lvals
PR)

MD(
Example:

```
(vector->list #(a b c))   =>  (a b c)
```
MD)

CB
reg vector->list ::constcl::vector->list

proc ::constcl::vector->list {vec} {
  list {*}[$vec value]
}
CB

TT(

::tcltest::test vectors-1.5 {try vector->list} -body {
    pew {(vector->list (vector 'a 'b 'c))}
} -output "(a b c)\n"

TT)

MD(
__list->vector__

`list->vector` converts a Lisp list value to a vector.
MD)

PR(
list->vector (public);list lvals -> vec
PR)

MD(
Example:

```
(list->vector '(1 2 3))   =>  #(1 2 3)
```
MD)

CB
reg list->vector ::constcl::list->vector

proc ::constcl::list->vector {list} {
  vector {*}[splitlist $list]
}
CB

TT(

::tcltest::test vectors-1.6 {try list->vector} -body {
    pew {(list->vector '(a b c))}
} -output "#(a b c)\n"

TT)

MD(
__vector-fill!__

`vector-fill!` fills a non-constant vector with a given value.
MD)

PR(
vector-fill! (public);vec vec fill val -> vec
PR)

MD(
Example:

```
(define vec (vector 'a 'b 'c))
(vector-fill! vec 'x)             =>  #(x x x)
vec                               =>  #(x x x)
```
MD)

CB
reg vector-fill! ::constcl::vector-fill!

proc ::constcl::vector-fill! {vec fill} {
  check {vector? $vec} {
    VECTOR expected\n([pn] [$vec show] \
      [$fill show])
  }
  $vec fill! $fill
}
CB

TT(

::tcltest::test vectors-1.7 {try vector-fill!} -body {
    pew {(vector-fill! (vector 'a 'b 'c) 'x)}
} -output "#(x x x)\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
