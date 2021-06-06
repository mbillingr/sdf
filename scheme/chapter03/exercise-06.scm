#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'generic-procedures)

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#

(define (matrix? obj)
  (or (eq? obj ZERO-MATRIX)
      (eq? obj IDENTITY-MATRIX)
      (and (vector? obj)
           (and (> (vector-length obj) 0)
                (vector? (vector-ref obj 0))))))
(register-predicate! matrix? 'matrix)

(define (matrix-rows mat)
  (vector-length mat))

(define (matrix-cols mat)
  (vector-length (vector-ref mat 0)))

(define ZERO-MATRIX (vector 'zero-matrix))
(define IDENTITY-MATRIX (vector 'identity-matrix))

(define (matrix-neg mat)
  (matrix-map (lambda (x) (- x)) mat))

(define (matrix-sub mat-a mat-b)
  (matrix-map (lambda (a b) (- a b)) mat-a mat-b))

(define (matrix-add mat-a mat-b)
  (matrix-map (lambda (a b) (+ a b)) mat-a mat-b))

(define (matrix-map element-procedure . matrices)
  (ensure-matrix-dims-match matrices)
  (apply vector-map
         (lambda rows
           (apply vector-map
                  element-procedure
                  rows))
         matrices))

(define (ensure-matrix-dims-match matrices)
  (let ((n-rows (matrix-rows (car matrices)))
        (n-cols (matrix-cols (car matrices))))
    (if (any (lambda (m)
               (not (and (n:= (vector-length m) n-rows)
                         (n:= (vector-length (vector-ref m 0)) n-cols))))
             matrices)
        (error "Matrix dimension mismatch:" matrices))))

(define (matrix-mul mat-a mat-b)
  (if (not (n:= (matrix-cols mat-a)
                (matrix-rows mat-b)))
      (error "Matrix dimension mismatch:" mat-a mat-b))
  (vector-map (lambda (row-a)
                (vector-matrix-mul row-a mat-b))
              mat-a))

(define (vector-matrix-mul vec mat)
  (apply vector
        (matrix-column-map (lambda (col)
                             (list-dot-product (vector->list vec)
                                               (vector->list col)))
                           mat)))

(define (matrix-column-map proc mat)
  (define (loop j acc)
    (if (n:< j 0)
        acc
        (loop (- j 1)
              (cons (proc (matrix-column-ref mat j))
                    acc))))
  (loop (n:- (matrix-cols mat) 1) '()))


(define (matrix-column-ref mat j)
  (vector-map (lambda (row) (vector-ref row j))
              mat))


(define (list-dot-product list1 list2)
  (if (null? list1)
      0
      (+ (* (car list1) (car list2))
         (list-dot-product (cdr list1) (cdr list2)))))


(matrix-mul (vector (vector 2)) (vector (vector 3)))


(define (matrix-extender base-arithmetic)
  (make-arithmetic 'matrix matrix? (list base-arithmetic)
    (lambda (name base-constant)
      (case name
        ((additive-identity) ZERO-MATRIX)
        ((multiplicative-identity) IDENTITY-MATRIX)
        (else base-constant)))
    (lambda (operator base-operation)
      (let ((procedure
             (case operator
               ((+) matrix-add)
               ((*) matrix-mul)
               ((-) matrix-sub)
               ((negate) matrix-neg)
               (else (lambda args
                       (error "Operator undefined in Matrix"
                              operator args))))))
        (and procedure
             (simple-operation operator matrix? procedure))))))

(let ((g (make-generic-arithmetic make-simple-dispatch-store)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g matrix-extender)
  (extend-generic-arithmetic! g symbolic-extender)
  (install-arithmetic! g))

; b) Why may symbolic matrix inversion take space factorial in the dimension?
;    -> The inverse is a combination of the matrix determinant and the adjugate
;       matrix. The determinant in the general case involves a sum over all
;       permutations of dimensions, and the number of permutations is the
;       factorial.
