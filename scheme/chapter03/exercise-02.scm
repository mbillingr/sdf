#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'combining-arithmetics)

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#

(register-predicate! vector? 'vector)

(define (vector-element-wise element-procedure)
  (lambda vecs
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
        (error "Vector dimension mismatch:" vecs))))

(define vector-arithmetic
  (make-arithmetic 'vector vector? '()
    (lambda (name)
      (case name
        ((additive-identity) #f)
        ((multiplicative-identity) #t)
        (else (default-object))))
    (lambda (operator)
      (let ((procedure
              (case operator
                ((+) (vector-element-wise +))
                ((-) (vector-element-wise -))
                ((negate) (vector-element-wise -))
                (else
                  (lambda args
                    (error "Operator undefined in Vector" operator))))))
        (simple-operation operator vector? procedure)))))

(install-arithmetic! (add-arithmetics combined-arithmetic vector-arithmetic))
