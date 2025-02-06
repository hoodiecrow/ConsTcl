if no {

MD(
## Lookup tables

Lisp languages have two simple variants of key/value lookup tables: property
lists (plists) and association lists (alists).

A property list is simply a list where every odd-numbered item (starting from 1)
is a key and every even-numbered item is a value. Example:
MD)

CB
'(a 1 b 2 c 3 d 4 e 5)
CB

MD(
Values can be retrieved in a two-step process:
MD)

CB
> (define plist (list 'a 1 'b 2 'c 3 'd 4 'e 5))
> (define v '())
> (set! v (memq 'c plist))
(c 3 d 4 e 5)
> (set! v (cadr v))
3
CB

MD(
If a key doesn't occur in the plist, `memq` returns `#f`.

Alternatively, ConsTcl users can use `get` to access the value in one step. 
MD)

CB
> (get plist 'c)
3
CB

MD(
`get` returns `#f` if the key isn't present in the plist.
MD)

TT(
::tcltest::test lutables-1.0 {test get procedure} -body {
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (get plist 'c))"
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (get plist 'x))"
} -output "3\n#f\n"
TT)

MD(
Values can be added with a single statement:
MD)

CB
> (set! plist (append '(f 6) plist))
(f 6 a 1 b 2 c 3 d 4 e 5)
CB

MD(
or with the `put!` macro, which can both update existing values and add new ones:
MD)

CB
> (put! plist 'c 9)
(f 6 a 1 b 2 c 9 d 4 e 5)
> (put! plist 'g 7)
(g 7 f 6 a 1 b 2 c 9 d 4 e 5)
CB

TT(
::tcltest::test lutables-2.0 {test put! macro} -body {
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (put! plist 'd 4) plist)"
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (put! plist 'c 4) plist)"
} -output "(d 4 a 1 b 2 c 3)\n(a 1 b 2 c 4)\n"
TT)

MD(
To get rid of a key/value pair, the simplest way is to add a masking pair:
MD)

CB
> (set! plist (append '(d #f) plist))
(d #f g 7 f 6 a 1 b 2 c 3 d 4 e 5)
CB

MD(
But instead, one can use the `del!` macro:
MD)

CB
> plist
(g 7 f 6 a 1 b 2 c 9 d 4 e 5)
> (del! plist 'd)
(g 7 f 6 a 1 b 2 c 9 e 5)
CB

TT(
::tcltest::test lutables-3.0 {test del! macro} -body {
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (del! plist 'c) plist)"
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (del! plist 'a) plist)"
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (del! plist 'b) plist)"
    pep "(let ((plist (list 'a 1 'b 2 'c 3))) (del! plist 'x) plist)"
} -output "(a 1 b 2)\n(b 2 c 3)\n(a 1 c 3)\n(a 1 b 2 c 3)\n"
TT)

MD(
An alist is a list where the items are pairs, with the key as the `car` and the
value as the `cdr`. Example:
MD)

CB
> (define alist (list (cons 'a 1) (cons 'b 2) (cons 'c 3) (cons 'd 4)))
> alist
((a . 1) (b . 2) (c . 3) (d . 4))
CB

MD(
The procedure `assq` retrieves one pair based on the key:
MD)

CB
> (assq 'a alist)
(a . 1)
> (cdr (assq 'a alist))
1
> (assq 'x alist)
#f
CB

MD(
As an alternative, the `get-alist` procedure fetches the value directly, or #f
for a missing item:
MD)

CB
> (get-alist 'a)
1
> (get-alist 'x)
#f
CB

}

# vim: ft=tcl tw=80
