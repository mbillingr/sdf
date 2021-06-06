#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'automatic-differentiation)

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#


(define diff:tan
  (diff:unary-proc tan (lambda (x) (/ (square (cos x))))))

(define diff:atan1
  (diff:unary-proc atan (lambda (x) (/ (+ 1 (square x))))))

(assign-handler! tan    diff:tan    differential?)
(assign-handler! atan1  diff:atan1  differential?)
