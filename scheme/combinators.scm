(import (sunny arity)
        (sunny hash-table))

; -----------------------------------

; utils thas should eventually be moved to another file...

(define-syntax assert
  (simple-macro (condition)
    (if condition 'ok
        (error (list "assertion failed" (quote condition))))))

(define (list . x) x)

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(define (list-tail lst n)
  (if (= n 0)
      lst
      (list-tail (cdr lst) (- n 1))))

(define (list-head lst n)
  (if (= n 0)
      '()
      (cons (car lst)
            (list-head (cdr lst) (- n 1)))))

(define (list-remove lst n)
  (if (= n 0)
      (cdr lst)
      (cons (car lst)
            (list-remove (cdr lst) (- n 1)))))

(define (list-insert lst index value)
  (if (= index 0)
      (cons value lst)
      (cons (car lst)
            (list-insert (cdr lst) (- index 1) value))))

(define (map f lst)
  (if (null? lst)
      lst
      (cons (f (car lst))
            (map f (cdr lst)))))

(define (exact-nonnegative-integer? i)
  #t)  ; our interpreter does not have this function yet

(define (or a b)
  (if a a b))

; -----------------------------------

(define (compose f g)
  (let ((n (get-arity g)))
    (define (the-composition . args)
      (check-arity n (length args))
      (call-with-values
        (lambda () (apply g args))
        f))
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
  (compose h (spread-apply f g)))

(define (spread-apply f g)
  (let ((arity-f (get-arity f))
        (arity-g (get-arity g)))
    (assert (or (fixed-arity? arity-f)
                (fixed-arity? arity-g)))
    (if (fixed-arity? arity-f)
        (let ((n (procedure-arity-min arity-f)))
          (let ((t (arity-add-fixed arity-g n)))
            (define (the-combination . args)
              (check-arity t (length args))
              (let ((fv (call-with-values
                          (lambda () (apply f (list-head args n)))
                          list))
                    (gv (call-with-values
                          (lambda () (apply g (list-tail args n)))
                          list)))
                (apply values (append fv gv))))
            (restrict-arity the-combination t)))
        (let ((m (procedure-arity-min arity-g)))
          (let ((t (arity-add-fixed arity-f m)))
            (define (the-combination . args)
              (check-arity t (length args))
              (let ((fv (call-with-values
                          (lambda () (apply f (list-head args n)))
                          list))
                    (gv (call-with-values
                          (lambda () (apply g (list-tail args n)))
                          list)))
                (apply values (append fv gv))))
            (restrict-arity the-combination t))))))

(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (compose f (lambda args
                 (apply values (list-remove args i))))))

(define (curry-argument i)
  (lambda args
    (lambda (f)
      (assert (= (length args) (- (get-arity f) 1)))
      (lambda (x)
        (apply f (list-insert args i x))))))

(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      (define (the-combination . args)
        (apply f (permute args)))
      (let ((n (get-arity f)))
        (check-arity n (length permspec))
        (restrict-arity the-combination n)))))

(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p))
         permspec))
  the-permuter)

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
(display ((spread-combine list (lambda (a b) (list 'foo a b)) (lambda (c d e) (list 'bar c d e))) 1 2 3 4 5))
(newline)

(display ((spread-combine list
                          (lambda (x y) (values x y))
                          (lambda (u v w) (values w v u)))
          'a 'b 'c 'd 'e))
(newline)

(display (((permute-arguments 1 2 0 3) (lambda (x y z w) (list 'foo x y z w))) 'a 'b 'c 'd))
(newline)
