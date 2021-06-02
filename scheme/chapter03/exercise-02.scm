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

(define (vector-extender base-arithmetic)
  (make-arithmetic 'vector vector? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
            (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
        (simple-operation operator
                          vector?
                          (case operator
                            ; alternative: implement vector operations in terms of the base operation
                            ;              this has the drawback that it does not work on vectors of vectors.
                            ;((+) (vector-element-wise (lambda args (apply-operation base-operation args))))
                            ((+) (vector-element-wise +))
                            ((-) (vector-element-wise -))
                            ((negate) (vector-element-wise -))
                            ((*) (lambda (v1 v2)
                                   (reduce
                                     +
                                     (arithmetic-constant 'additive-identity
                                                          base-arithmetic)
                                     (vector->list ((vector-element-wise *) v1 v2)))))
                            (else
                              (lambda args
                                (error "Operator undefined in Vector" operator)))))))))

(install-arithmetic! (extend-arithmetic vector-extender combined-arithmetic))
