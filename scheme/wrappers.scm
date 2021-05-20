(import (sunny derived-syntax)
        (sunny hash-table))

(include "combinators.scm")

; ======================================
;  helpers
; ======================================

(define (println . args)
  (if (null? args)
      (newline)
      (begin (display (car args))
             (display " ")
             (apply println (cdr args)))))

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

; unit conversions
; --------------------------------------

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
           (unit:invert inch-to-meter)
           (unit:invert inch-to-meter)))

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



; specialization wrappers
; --------------------------------------

(define (unit-specializer procedure implicit-output-unit . implicit-input-units)
  (define (specializer specific-output-unit . specific-input-units)
    (let ((output-converter
            (make-converter implicit-output-units
                            specific-output-units))
          (input-converters
            (map make-converter
                 specific-input-units
                 implicit-input-units)))
      (define (specialized-procedure . arguments)
        (output-converter
          (apply procedure
                 (map (lambda (converter argument) (converter argument))
                      input-converters
                      arguments))))
      specialized-procedure))
  specializer)

(define (unit:* u1 u2)
  (make-unit-conversion (compose u2 u1)
                        (compose (unit::invert u1)
                                 (unit::invert u2))))

(register-unit-conversion 'fahrenheit 'celsius fahrenheit-to-celsius)
(register-unit-cenversion 'celsius 'kelvin celsius-to-kelvin)

(register-unit-conversion 'fahrenheit 'kelvin
  (unit:* fahrenheit-to-celsius celsius-to-kelvin))

(register-unit-conversion '(/ pound (expt inch 2))
                          '(/ newton (expt meter 2))
  (unit:/ pound-to-newton
          (unit:expt inch-to-meter 2)))

(register-unit-conversion '(expt inch 3) '(expt meter 3)
  (unit:expt inch-to-meter 3))

(define make-specialized-gas-law-volume
  (unit-specializer
    gas-law-volume
    '(expt meter 3)             ; output (volume)
    '(/ newton (expt meter 2))  ; pressure
    'kelvin                     ; temperature
    'mole))                     ; amount

(define conventional-gas-law-volume
  (make-specialized-gas-law-volume
    '(expt inch 3)            ; output (volume)
    '(/ pound (expt inch 2))  ; pressure
    'fahrenheit               ; temperature
    'mole))                   ; amount

(sphere-radius (conventional-gas-law-volume 14.7 68 1))
