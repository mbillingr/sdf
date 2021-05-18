(import (sunny derived-syntax)
        (sunny hash-table))

(include "combinators.scm")

; ======================================
;  unit impls
; ======================================

(define UNIT-TABLE (make-key-weak-eq-hash-table))

(define (make-unit-conversion conv inv)
  (hash-table-set! UNIT-TABLE conv inv)
  conv)

(define (unit:invert conv)
  (hash-table-ref/default UNIT-TABLE conv #f))

; ======================================
; chapter code
; ======================================

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)     ; J/(K*mol)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))


(define fahrenheit-to-celsius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ (* c 9/5) 32))))


(define celsius-to-kelvin
  (let ((zero-celsius 273.15)) ; kelvins
    (make-unit-conversion (lambda (c) (+ c zero-celsius))
                          (lambda (k) (- k zero-celsius)))))

(define inch-to-meter
  (let ((scaling 39.37)) ; inches per meter
    (make-unit-conversion (lambda (i) (/ i scaling))
                          (lambda (m) (* m scaling)))))

(define pound-to-newton
  (let ((scaling 0.22480894387096)) ; pounds per newton
    (make-unit-conversion (lambda (p) (/ p scaling))
                          (lambda (n) (* n scaling)))))

(define psi-to-nsm
  (compose pound-to-newton
           (compose (unit:invert inch-to-meter)
                    (unit:invert inch-to-meter))))

; ======================================
;  pre-helpers
; ======================================

(define (println . args)
  (if (null? args)
      (newline)
      (begin (display (car args))
             (display " ")
             (apply println (cdr args)))))

; ======================================

(println (fahrenheit-to-celsius -40))
(println (fahrenheit-to-celsius 32))
(println ((unit:invert fahrenheit-to-celsius) 20))
(println ((compose celsius-to-kelvin fahrenheit-to-celsius) 80))
(println ((unit:invert inch-to-meter)
          (sphere-radius
            (gas-law-volume
              (psi-to-nsm 14.7)
              ((compose celsius-to-kelvin fahrenheit-to-celsius) 68)
              1))))
