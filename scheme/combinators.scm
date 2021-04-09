
(define (compose f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)

(define (iterate n)
  (define (the-iterator f)
    (if (= n 0)
        identity
        (compose f ((iterate (- n 1)) f))))
  the-iterator)

(define (identity x) x)

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args)
       (apply g args)))
  the-combination)
