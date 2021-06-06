#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'automatic-differentiation)

Unfortunately, it seems to be impossible to perform the preparations within
the script.

Use the defined arithmetics by passing them to (install-arithmetic!).
|#

(define (root-newton f initial-guess tolerance)
  (let ((Df (derivative f)))
    (define (improve-guess xn)
      (- xn (/ (f xn) (Df xn))))
    (let loop ((xn initial-guess))
      (let ((xn+1 (improve-guess xn)))
        (if (close-enuf? xn xn+1 tolerance)
            xn+1
            (loop xn+1))))))


(define (cs theta)
  (- (cos theta) (sin theta)))

(root-newton cs 0.5 1e-8)
