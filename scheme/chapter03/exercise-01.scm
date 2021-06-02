#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'combining-arithmetics)

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#

(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
    (lambda (name)
      (case name
        ((additive-identity) #f)
        ((multiplicative-identity) #t)
        (else (default-object))))
    (lambda (operator)
      (let ((procedure
              (case operator
                ((+) (lambda (a b) (or a b)))
                ((-) (lambda (a b) (or a (not b))))
                ((*) (lambda (a b) (and a b)))
                ((negate) not)
                (else
                  (lambda args
                    (error "Operator undefined in Boolean" operator))))))
        (simple-operation operator boolean? procedure)))))

(define boolean-symbolic-arithmetic
  (extend-arithmetic symbolic-extender boolean-arithmetic))
