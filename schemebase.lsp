
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
        (let ((node-before (delete-seek lst (- idx 1)))
              (node-after (delete-seek lst (+ idx 2))))
          (set-cdr! node-before node-after))))
    lst))

(define (delete-seek lst idx)
  (if (zero? idx)
    lst
    (delete-seek (cdr lst) (- idx 1))))

(define (get-alist lst key)
  (let ((item (assq key lst)))
    (if item
      (cdr item)
      #f)))

(define (pairlis a b)
  (if (null? a)
    ()
    (cons (cons (car a) (car b)) (pairlis (cdr a) (cdr b)))))

(define (set-alist! lst key val)
  (let ((item (assq key lst)))
    (if item
      (begin (set-cdr! item val) lst)
      lst)))


; vim: ft=lisp tw=80
