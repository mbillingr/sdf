#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'generic-procedures)
(load "/mystuff/scheme/chapter03/exercise-02.scm")

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#

(let ((g (make-generic-arithmetic make-simple-dispatch-store)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g vector-extender)
  (extend-generic-arithmetic! g symbolic-extender)
  (extend-generic-arithmetic! g function-extender)
  (install-arithmetic! g))

(((* 3
     (lambda (x) (lambda (y) (+ x y)))
     (lambda (x) (lambda (y) (vector y x))))
  'a)
 4)

(define (unit-circle x)
  (vector (sin x) (cos x)))

((magnitude unit-circle) 'a)
((magnitude (vector sin cos)) 'a)


; b) Both expressions work when the arithmetic is first extended by vector
;    and then by function. It does not work when extending by vector last.
;    I guess we have some ambiguity in choice of rules here.

; c) (vector cos sin) creates a vector of functions. If we wanted to apply this
;    to some arguments we would have to define what it means to apply a vector.
;    This can't be done with generic procedures because application is intrinsic
;    to the interpreter, and does not go through a procudere.
;    On the other hand, it might be possible to redefine the vector constructor.
;    It could return a vector-generating function. So if we made vector a
;    generic function that normally just creates a vector, but if any of its
;    arguments is a function it would create a function... I think this system
;    would be hard to generalize further.
