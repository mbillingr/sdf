(import (sunny arity)
        (sunny hash-table))

; -----------------------------------

; utils thas should eventually be moved to another file...

(define-syntax assert
  (simple-macro (condition)
    (if condition 'ok
        (error (cons "assertion failed" (quote condition))))))

(define (list . x) x)

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (list-tail lst n)
  (if (= n 0)
      lst
      (list-tail (cdr lst) (- n 1))))

(define (list-head lst n)
  (if (= n 0)
      '()
      (cons (car lst)
            (list-head (cdr lst) (- n 1)))))

; -----------------------------------

(define (compose f g)
  (assert (= (get-arity f) 1))
  (let ((n (get-arity g)))
    (define (the-composition . args)
      (assert (= (length args) n))
      (f (apply g args)))
    (restrict-arity the-composition n)))

(define (iterate n)
  (define (the-iterator f)
    (if (= n 0)
        identity
        (compose f ((iterate (- n 1)) f))))
  the-iterator)

(define (identity x) x)

(define (parallel-combine h f g)
  (assert (= (get-arity h) 2))
  (let ((n (get-arity f))
        (m (get-arity g)))
    (assert (= n m))
    (define (the-combination . args)
      (assert (= (length args) n))
      (h (apply f args)
         (apply g args)))
    (restrict-arity the-combination n)))

(define (spread-combine h f g)
  (let ((n (get-arity f))
        (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (let ((entry (hash-table-ref/default arity-table proc #f)))
    (if entry
        entry
        (let ((a (procedure-arity proc)))
          (assert (eqv? (procedure-arity-min a)
                        (procedure-arity-max a)))
          (procedure-arity-min a)))))

(define arity-table (make-key-weak-eq-hash-table))


((spread-combine list (lambda (a b) (list 'foo a b)) (lambda (c d e) (list 'bar c d e))) 1 2 3 4 5)
