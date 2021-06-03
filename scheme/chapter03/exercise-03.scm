#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'combining-arithmetics)
(load "/mystuff/scheme/chapter03/exercise-02.scm")

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#

(define vec-before-func
  (extend-arithmetic
    function-extender
    (extend-arithmetic vector-extender
                       combined-arithmetic)))

(define func-before-vec
  (extend-arithmetic
    vector-extender
    (extend-arithmetic function-extender
                       combined-arithmetic)))

(define (unit-circle x)
  (vector (sin x) (cos x)))


;((magnitude unit-circle) 'a)
;((magnitude (vector sin cos)) 'a)
