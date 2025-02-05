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
> (define plist '(a 1 b 2 c 3 d 4 e 5))
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

MD(
To get rid of a key/value pair, the simplest way is to add an erasing pair:
MD)

CB
> (set! plist (append '(d #f) plist))
(d #f g 7 f 6 a 1 b 2 c 3 d 4 e 5)
CB

MD(
But instead, one can use the `del` macro:
MD)

}

# vim: ft=tcl tw=80
