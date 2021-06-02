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

(define (vector-scalar-operation proc)
  (lambda (vec s)
    (vector-map (lambda (x) (proc x s)) vec)))

(define (vector-dot-product identity)
  (let ((mul (vector-element-wise *)))
    (lambda (v1 v2)
      (reduce + identity (vector->list (mul v1 v2))))))

(define (vector-extender base-arithmetic)
  (make-arithmetic 'vector vector? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
            (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
        (case operator
          ((*) (operation-union *
                 (make-operation operator
                                 (all-args (operator-arity operator)
                                           vector?)
                                 (vector-dot-product
                                   (arithmetic-constant 'additive-identity
                                                        base-arithmetic)))
                 (make-operation operator
                                 (list (list vector? base-predicate))
                                 (let ((mul (vector-scalar-operation *)))
                                   (lambda (vec s) (mul vec s))))
                 (make-operation operator
                                 (list (list base-predicate vector?))
                                 (let ((mul (vector-scalar-operation *)))
                                   (lambda (s vec) (mul vec s))))))
          (else
            (simple-operation operator
                              vector?
                              (case operator
                                ; alternative: implement vector operations in terms of the base operation
                                ;              this has the drawback that it does not work on vectors of vectors.
                                ;((+) (vector-element-wise (lambda args (apply-operation base-operation args))))
                                ((+) (vector-element-wise +))
                                ((-) (vector-element-wise -))
                                ((negate) (vector-element-wise -))
                                ((magnitude) (lambda (v) (sqrt (* v v))))
                                (else
                                  (lambda args
                                    (error "Operator undefined in Vector" operator)))))))))))

(install-arithmetic! (extend-arithmetic vector-extender combined-arithmetic))
