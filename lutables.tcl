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
MD)

MD(
Values can be added with a single statement:
MD)

CB
> (set! plist (append '(f 6) plist))
(f 6 a 1 b 2 c 3 d 4 e 5)
CB

MD(
To get rid of a key/value pair, the simplest way is to add an erasing pair:
MD)

CB
> (set! plist (append '(d #f) plist))
(d #f f 6 a 1 b 2 c 3 d 4 e 5)
CB

}

# vim: ft=tcl tw=80
