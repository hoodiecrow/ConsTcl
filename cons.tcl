MD(
## Initialization

Initialize the memory space for vector contents.
MD)

CB
unset -nocomplain ::constcl::vectorSpace
set ::constcl::vectorSpace [lrepeat 1024 #NIL]

unset -nocomplain ::constcl::vectorAssign
set ::constcl::vectorAssign 0
CB

MD(
Pre-make a set of constants (mostly symbols but also e.g. #NIL, #t, and #f)
and give them aliases for use in source text.
MD)

CB
interp alias {} #NIL {} [::constcl::NIL new]

interp alias {} #t {} [::constcl::MkBoolean #t]

interp alias {} #f {} [::constcl::MkBoolean #f]

interp alias {} #-1 {} [::constcl::MkNumber -1]

interp alias {} #0 {} [::constcl::MkNumber 0]

interp alias {} #1 {} [::constcl::MkNumber 1]

interp alias {} #B {} [::constcl::MkSymbol begin]

interp alias {} #I {} [::constcl::MkSymbol if]

interp alias {} #L {} [::constcl::MkSymbol let]

interp alias {} #Q {} [::constcl::MkSymbol quote]

interp alias {} #U {} [::constcl::MkSymbol unquote]

interp alias {} #S {} [::constcl::MkSymbol set!]

interp alias {} #x {} [::constcl::MkSymbol x]

interp alias {} #y {} [::constcl::MkSymbol y]

interp alias {} #λ {} [::constcl::MkSymbol lambda]

interp alias {} #+ {} [::constcl::MkSymbol +]

interp alias {} #- {} [::constcl::MkSymbol -]

interp alias {} #NONE {} [::constcl::None new]

CB

MD(
Initialize the definition register with the queen of numbers (or at least
a double floating point approximation).
MD)

CB
dict set ::constcl::defreg pi [::constcl::MkNumber 3.1415926535897931]
CB

MD(
**atom?**

`atom?` recognizes an atom by checking for membership in one of the atomic types.
MD)

PR(
atom? (public);val val -> bool
PR)

CB
reg atom? ::constcl::atom?

proc ::constcl::atom? {val} {
    if {[symbol? $val] ne "#f" || [number? $val] ne "#f" || [string? $val] ne "#f" || [char? $val] ne "#f" || [boolean? $val] ne "#f" || [vector? $val] ne "#f"} {
        return #t
    } else {
        return #f
    }
}
CB

TT(
::tcltest::test cons-1.0 {calculate circle area} -body {
    pep "(define circle-area (lambda (r) (* pi (* r r))))"
    pep "(circle-area 3)"
} -output 28.274333882308138\n

::tcltest::test cons-2.0 {calculate factorial} -body {
    pep "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"
    pep "(fact 10)"
} -output 3628800\n

::tcltest::test cons-2.1 {calculate factorial} -body {
    pep "(fact 100)"
} -output 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000\n

::tcltest::test cons-2.2 {calculate factorial} -body {
    pep "(circle-area (fact 10))"
} -output 41369087205782.695\n

::tcltest::test cons-3.0 {count} -body {
    pep "(define first car)"
    pep "(define rest cdr)"
    pep "(define truthtoint (lambda (val) (if val 1 0)))"
    pep "(define count (lambda (item L) (if (not (eqv? L '())) (+ (truthtoint (equal? item (first L))) (count item (rest L))) 0)))"
} -output ""

::tcltest::test cons-3.0 {count} -body {
    pep "(count 0 (list 0 1 2 3 0 0))"
} -output 3\n

::tcltest::test cons-3.1 {count} -body {
    pep "(count (quote the) (quote (the more the merrier the bigger the better)))"
} -output 4\n

::tcltest::test cons-4.0 {twice} -body {
    pep "(define twice (lambda (x) (* 2 x)))"
    pep "(twice 5)"
} -output 10\n

::tcltest::test cons-4.1 {twice} -body {
    pep "(define repeat (lambda (f) (lambda (x) (f (f x)))))"
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

::tcltest::test cons-9.0 {symbol?} -body {
    pep "(symbol? (quote foo99))"
} -output "#t\n"

::tcltest::test cons-10.0 {shadowing} -body {
    pep "(begin (define r 10) (define (f r) (set! r 20)) (f 30) r)"
} -output "10\n"

#-constraints knownBug 
::tcltest::test cons-11.0 {and} -body {
    pep "(and (= 2 2) (> 2 1))"
} -output "#t\n"

::tcltest::test cons-11.1 {and} -body {
    pep "(and (= 2 2) (< 2 1))"
} -output "#f\n"

::tcltest::test cons-11.2 {and :( } -body {
    pep "(and)"
} -output "#t\n"

::tcltest::test cons-11.3 {and} -body {
    pep "(and 1 2 (quote c) (quote (f g)))"
} -output "(f g)\n"

::tcltest::test cons-12.0 {or} -body {
    pep "(or (= 2 2) (> 2 1))"
} -output "#t\n"

::tcltest::test cons-12.1 {or} -body {
    pep "(or (= 2 2) (< 2 1))"
} -output "#t\n"

::tcltest::test cons-12.2 {or} -body {
    pep "(or #f #f #f)"
} -output "#f\n"

::tcltest::test cons-12.3 {or} -body {
    pep "(or)"
} -output "#f\n"

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

# vim: ft=tcl tw=80
