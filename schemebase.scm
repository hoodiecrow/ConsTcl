
; An assortment of procedures to supplement the builtins.

(define (get plist key)
  (let ((v (memq key plist)))
    (if v
      (cadr v)
      #f)))

(define (list-find-key lst key)
  (lfk lst key 0))

(define (lfk lst key count)
  (if (null? lst)
    -1
    (if (eq? (car lst) key)
      count
      (lfk (cddr lst) key (+ count 2)))))

(define (list-set! lst idx val)
  (if (zero? idx)
    (set-car! lst val)
    (list-set! (cdr lst) (- idx 1) val)))

(define (delete! lst key)
  (let ((idx (list-find-key lst key)))
    (if (< idx 0)
      lst
      (if (= idx 0)
        (set! lst (cddr lst))
        (let ((bef (del-seek lst (- idx 1)))
              (aft (del-seek lst (+ idx 2))))
          (set-cdr! bef aft))))
    lst))

(define (del-seek lst idx)
  (if (zero? idx)
    lst
    (del-seek (cdr lst) (- idx 1))))

(define (get-alist lst key)
  (let ((item (assq key lst)))
    (if item
      (cdr item)
      #f)))

(define (pairlis a b)
  (if (null? a)
    ()
    (cons
      (cons (car a) (car b))
      (pairlis (cdr a) (cdr b)))))

(define (set-alist! lst key val)
  (let ((item (assq key lst)))
    (if item
      (begin (set-cdr! item val) lst)
      lst)))

(define (fact n)
  (if (<= n 1)
    1
    (* n (fact (- n 1)))))

(define (list-copy list)
  (if (null? list)
    '()
    (cons (car list)
      (list-copy (cdr list)))))

