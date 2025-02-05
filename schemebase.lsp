(define (get plist key)
  (let ((v (memq key plist)))
    (if v
      (set! v (cadr v))
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

; vim: ft=lisp tw=80
