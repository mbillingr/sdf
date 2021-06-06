#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'combinators 'automatic-differentiation)
(load "/sdf/combinators/more-combinators.scm")

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#

(define ((partial i) f)
  (let ((func-currier ((curry-arguments i) f)))
    (define (the-derivative . args)
      (let ((d-arg (list-ref args i))
            (constant-args (append (list-head args i)
                                   (list-tail args (+ i 1)))))
        ((derivative (apply func-currier constant-args)
                     d-arg))))
    the-derivative))
