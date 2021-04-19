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

(define (or a b)
  (if a a b))

; -----------------------------------

(define (compose f g)
  (check-arity (get-arity f) 1)
  (let ((n (get-arity g)))
    (define (the-composition . args)
      (check-arity n (length args))
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
  (check-arity (get-arity h) 2)
  (let ((n (get-arity f))
        (m (get-arity g)))
    (assert (equal? n m))
    (define (the-combination . args)
      (check-arity n (length args))
      (h (apply f args)
         (apply g args)))
    (restrict-arity the-combination n)))

(define (spread-combine h f g)
  (check-arity (get-arity h) 2)
  (let ((arity-f (get-arity f))
        (arity-g (get-arity g)))
    (assert (or (fixed-arity? arity-f)
                (fixed-arity? arity-g)))
    (if (fixed-arity? arity-f)
        (let ((n (procedure-arity-min arity-f)))
          (let ((t (arity-add-fixed arity-g n)))
            (define (the-combination . args)
              (check-arity t (length args))
              (h (apply f (list-head args n))
                 (apply g (list-tail args n))))
            (restrict-arity the-combination t)))
        (let ((m (procedure-arity-min arity-g)))
          (let ((t (arity-add-fixed arity-f m)))
            (define (the-combination . args)
              (check-arity t (length args))
              (let ((n (- (length args) m)))
                (h (apply f (list-head args n))
                   (apply g (list-tail args n)))))
            (restrict-arity the-combination t))))))

(define (restrict-arity proc arity)
  (hash-table-set! arity-table proc arity)
  proc)

(define (get-arity proc)
  (let ((entry (hash-table-ref/default arity-table proc #f)))
    (if entry
        entry
        (procedure-arity proc))))

(define arity-table (make-key-weak-eq-hash-table))

(define (check-arity arity n-args)
  (assert (>= n-args (procedure-arity-min arity)))
  (if (procedure-arity-max arity)
      (assert (<= n-args (procedure-arity-max arity)))))

(define (fixed-arity? arity)
  (eqv? (procedure-arity-min arity)
        (procedure-arity-max arity)))

(define (arity-add-fixed arity fixed-n)
  (cons (+ fixed-n (procedure-arity-min arity))
        (if (procedure-arity-max arity)
            (+ fixed-n (procedure-arity-max arity))
            #f)))

((spread-combine list (lambda (a b) (list 'foo a b)) (lambda (c d e) (list 'bar c d e))) 1 2 3 4 5)
