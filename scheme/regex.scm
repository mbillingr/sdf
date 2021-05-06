(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(define (r:quote string)
  (r:seq
    (list->string
      (append-map (lambda (char)
                    (if (memv char chars-needing-quoting)
                        (list #\\ char)
                        (list char)))
                  (string->list string)))))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))


;; helper functions

(define (list . x) x)

(define (append-map func list)
  (if (null? list)
      '()
      (append (func (car list))
              (append-map func (cdr list)))))


(define (list->string list-of-chars)
  (apply string list-of-chars))

(define (memv obj list)
  (if (null? list)
      #f
      (if (eqv? obj (car list))
          (cdr list)
          (memv obj (cdr list)))))
