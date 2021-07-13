#|
This script is expected to be loaded into a REPL session prepared with
the following steps

(load "/sdf/manager/load.scm")
(manage 'new-environment 'unification)

Unfortunately, it seems to be impossible to perform the preparations within
the script.
|#

(define (unify:vector-terms terms1 terms2)
  (let ((first1 (car terms1)) (rest1 (cdr terms1))
        (first2 (car terms2)) (rest2 (cdr terms2)))
    (cond ((and (= (vector-length first1) 0)
                (= (vector-length first2) 0))
           (lambda (dict succeed fail) (succeed dict fail rest1 rest2)))
          ((not (= (vector-length first1)
                   (vector-length first2)))
           (lambda (dict succeed fail) (fail)))
          (else
            (lambda (dict succeed fail)
              (let lp ((k 0) (dict dict) (fail fail))
                (if (= k (vector-length first1))
                    (succeed dict fail rest1 rest1)
                    ((unify:dispatch (list (vector-ref first1 k))
                                     (list (vector-ref first2 k)))
                     dict
                     (lambda (dict* fail* null1 null2)
                       (lp (+ k 1) dict* fail*))
                     fail))))))))


(define (vector-term? term)
  (vector? term))

(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies vector-term?)
              (car-satisfies vector-term?))
  unify:vector-terms)


;; need to override this function to replace variables in vectors
(define (match:map-vars get-value pattern)
  (let loop ((pattern pattern))
    (cond ((match:element-var? pattern)
           (get-value pattern (lambda () pattern)))
          ((match:segment-var? pattern)
           (if (get-value pattern (lambda () #f))
               (error "Ill-formed pattern:" pattern))
           pattern)
          ((list? pattern)
           (append-map (lambda (sub)
                         (if (match:segment-var? sub)
                             (get-value sub
                                        (lambda () (list sub)))
                             (list (loop sub))))
                       pattern))
          ((vector? pattern)
           (vector-map (lambda (sub)
                         (if (match:segment-var? sub)
                             (error "substituting segment-vars in vectors is not implemented")
                             (loop sub)))
                       pattern))
          (else pattern))))
