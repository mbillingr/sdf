#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'design-of-the-matcher)

Unfortunately, it seems to be impossible to perform the preparations within
the script.
|#

(define match:var-types (cons '?:choice match:var-types))

(define (match:compile-pattern pattern)
  (cond ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ((??) (match:segment pattern))
           ((?:choice) (match:choice (map match:compile-pattern (cdr pattern))))
           (else (error "Unknown var type:" pattern))))
        ((list? pattern)
         (match:list (map match:compile-pattern pattern)))
        (else
         (match:eqv pattern))))

(define (match:choice matchers)
  (define (choice-match data dictionary succeed)
    (let lp ((matchers matchers))
      (if (pair? matchers)
          (or ((car matchers)
               data
               dictionary
               succeed)
              (lp (cdr matchers)))
          #f)))
  choice-match)
