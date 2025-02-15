MD(
## Initialization

Initialize the memory space for vector contents.
MD)

CB
set ::constcl::vectorSpace [lrepeat 1024 #NIL]

set ::constcl::vectorAssign 0

proc ::constcl::vsAlloc {num} {
  # TODO calculate free space
  set va $::constcl::vectorAssign
  incr ::constcl::vectorAssign $num
  return $va
}
CB

CB
set ::constcl::gensymnum 0
CB

MD(
Pre-make a set of constants (e.g. #NIL, #t, and #f)
and give them aliases for use in source text.
MD)

CB
interp alias {} #NIL {} [::constcl::NIL new]

interp alias {} #t {} [::constcl::MkBoolean #t]

interp alias {} #f {} [::constcl::MkBoolean #f]

interp alias {} #-1 {} [N -1]

interp alias {} #0 {} [N 0]

interp alias {} #1 {} [N 1]

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #UNS {} [::constcl::Unspecified new]

interp alias {} #UND {} [::constcl::Undefined new]

interp alias {} #EOF {} [::constcl::EndOfFile new]

CB

MD(
Initialize the definition register with the queen of numbers (or at least
a double-precision floating point approximation).
MD)

CB
dict set ::constcl::defreg pi [N 3.1415926535897931]
CB

MD(
In this interpreter, `nil` does refer to the empty list.
MD)

CB
reg nil #NIL
CB

TT(
::tcltest::test cons-1.0 {calculate circle area} -body {
    pep "(define (circle-area r) (* pi (* r r)))"
    pep "(circle-area 3)"
} -output 28.274333882308138\n

::tcltest::test cons-2.0 {calculate factorial} -body {
    pep "(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))"
    pep "(fact 10)"
} -output 3628800\n

::tcltest::test cons-2.1 {calculate factorial 100} -body {
    pep "(fact 100)"
} -output 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000\n

::tcltest::test cons-2.2 {calculate circle-area factorial 10} -body {
    pep "(circle-area (fact 10))"
} -output 41369087205782.695\n

::tcltest::test cons-3.0 {count} -body {
    pep "(define (truthtoint val) (if val 1 0))"
    pep "(define (count item L) (if (not (null? L)) (+ (truthtoint (equal? item (car L))) (count item (cdr L))) 0))"
} -output ""

::tcltest::test cons-3.0 {count} -body {
    pep "(count 0 (list 0 1 2 3 0 0))"
} -output 3\n

::tcltest::test cons-3.1 {count} -body {
    pep "(count 'the '(the more the merrier the bigger the better))"
} -output 4\n

::tcltest::test cons-4.0 {twice} -body {
    pep "(define (twice x) (* 2 x))"
    pep "(twice 5)"
} -output 10\n

::tcltest::test cons-4.1 {twice} -body {
    pep "(define (repeat f) (lambda (x) (f (f x))))"
    pep "((repeat twice) 10)"
} -output 40\n

::tcltest::test cons-4.2 {twice} -body {
    pep "((repeat (repeat twice)) 10)"
} -output 160\n

::tcltest::test cons-4.3 {twice} -body {
    pep "((repeat (repeat (repeat twice))) 10)"
} -output 2560\n

::tcltest::test cons-4.4 {twice} -body {
    pep "((repeat (repeat (repeat (repeat twice)))) 10)"
} -output 655360\n

::tcltest::test cons-5.0 {fib-range} -body {
    pep "(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))"
    pep "(define (range a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b))))"
    pep "(range 0 10)"
} -output "(0 1 2 3 4 5 6 7 8 9)\n"

::tcltest::test cons-5.1 {fib-range} -body {
    pep "(map fib (range 0 10))"
} -output "(1 1 2 3 5 8 13 21 34 55)\n"

::tcltest::test cons-5.2 {fib-range} -body {
    pep "(map fib (range 0 20))"
} -output "(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)\n"

::tcltest::test cons-6.0 {procedure call with a list operator} -body {
    pep "((if #t + *) 2 3)"
} -output "5\n"

::tcltest::test cons-7.0 {assignment} -body {
    pep "(begin (define r 10) (set! r 20) r)"
} -output "20\n"

::tcltest::test cons-7.1 {assignment returns a value} -body {
    pep "(begin (define r 10) (set! r 20))"
} -output "20\n"

::tcltest::test cons-7.2 {assignment to an unbound symbol} -body {
    pep "(begin (set! XX 20))"
} -returnCodes error -result "Unbound variable: XX"

::tcltest::test cons-8.0 {procedure definition} -body {
    pep "(lambda (r) (* r r))"
} -match regexp -output "#<proc-\\d+>\n"

::tcltest::test cons-8.1 {procedure with two expressions} -body {
    pep "(define (f) (define r 20) (* r r))"
    pep "(f)"
} -output "400\n"

::tcltest::test cons-8.2 {resolve procedure with two expressions} -body {
    pxp "(define (f) (define r 20) (* r r))"
} -output "(define f (lambda () (define r 20) (* r r)))\n"

::tcltest::test cons-9.0 {symbol?} -body {
    pep "(symbol? (quote foo99))"
} -output "#t\n"

::tcltest::test cons-10.0 {shadowing} -body {
    pep "(begin (define r 10) (define (f r) (set! r 20)) (f 30) r)"
} -output "10\n"

::tcltest::test cons-13.0 {expandquotes} -body {
    pep "''foo"
} -output "(quote foo)\n"

::tcltest::test cons-14.0 {Scheme cookbook, due to Jakub T. Jankiewicz} -body {
    pep "(define (every? fn list)
  (or (null? list)
      (and (fn (car list)) (every? fn (cdr list)))))"
    pep "(every? number? '(1 2 3 4))"
} -output "#t\n"

::tcltest::test cons-14.1 {Scheme cookbook, due to Jakub T. Jankiewicz} -body {
    pep "(define (adjoin x a)
  (if (member x a)
      a
      (cons x a)))"
    pep "(adjoin 'x '(a b c))"
} -output "(x a b c)\n"

::tcltest::test cons-14.2 {Scheme cookbook, due to Nils M Holm} -body {
    pep "(adjoin 'c '(a b c))"
} -output "(a b c)\n"

::tcltest::test cons-14.3 {Scheme cookbook, due to Jakub T. Jankiewicz} -body {
    pep "(define (list-index fn list)
  (let iter ((list list) (index 0))
    (if (null? list)
        -1
        (let ((item (car list)))
          (if (fn item)
              index
              (iter (cdr list) (+ index 1)))))))"
    pep "(define (>10 x) (> x 10))"
    pep "(list-index >10 '(1 2 3 4 10 11 12 13 14))"
} -output "5\n"

::tcltest::test cons-14.4 {Scheme cookbook, due to Jakub T. Jankiewicz} -body {
    pep "(define (take lst n)
  (let loop ((result '()) (i n) (lst lst))
    (if (or (null? lst) (<= i 0))
        (reverse result)
        (loop (cons (car lst) result) (- i 1) (cdr lst)))))"
    pep "(define (sublist-map n fn lst)
  (let loop ((lst lst) (result '()))
    (if (< (length lst) n)
        (reverse result)
        (let ((next-list (take lst n)))
          (loop (cdr lst) (cons (apply fn next-list) result))))))"
    pep "(sublist-map 2 < '(1 2 3 4))"
} -output "(#t #t #t)\n"

::tcltest::test cons-14.5 {Scheme cookbook, due to Jakub T. Jankiewicz} -body {
    pep "(define (remove fn lst)
  (let loop ((lst lst) (result '()))
    (if (null? lst)
        (reverse result)
        (let ((item (car lst)))
          (loop (cdr lst)
                (if (fn item) result (cons item result)))))))"
    pep "(remove >10 '(1 2 3 4 10 11 12 13 14))"
} -output "(1 2 3 4 10)\n"

::tcltest::test cons-14.6 {Scheme cookbook, due to Lassi Kortela} -body {
    pep {(define (group n lst)
  (if (< n 1)
      (error "group: n must be positive")
      (let loop ((lst lst) (m n) (g '()) (gs '()))
        (cond ((and (null? lst) (null? g))
               (reverse gs))
              ((or (null? lst) (zero? m))
               (loop lst n '() (cons (reverse g) gs)))
              (else
               (loop (cdr lst) (- m 1) (cons (car lst) g) gs))))))}
    pep "(group 3 (in-range 10))"
} -output "((0 1 2) (3 4 5) (6 7 8) (9))\n"

::tcltest::test cons-14.7 {Scheme cookbook, due to Lassi Kortela} -body {
    pep {(define (group-by f lst)
  (if (null? lst) '()
      (let ((first (car lst)))
        (let loop ((lst (cdr lst))
                   (key (f first))
                   (group (list first))
                   (groups '()))
          (if (null? lst)
              (reverse (cons (reverse group) groups))
              (let ((newkey (f (car lst))))
                (if (equal? key newkey)
                    (loop (cdr lst) key
                          (cons (car lst) group)
                          groups)
                    (loop (cdr lst) newkey
                          (list (car lst))
                          (cons (reverse group) groups)))))))))}
    pep "(group-by odd? '(1 3 5 2 1 6 4 1 7))"
} -output "((1 3 5) (2) (1) (6 4) (1 7))\n"

TT)

# vim: ft=tcl tw=80 ts=2 sw=2 sts=2 et 
