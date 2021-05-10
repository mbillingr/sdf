(import (sunny derived-syntax))

; overriden functions

(define primitive-append append)
(define (append left . more)
  (if (null? more)
      left
      (primitive-append
        left
        (apply append more))))

; ------------------------------

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

(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq
             (cons (car exprs)
                   (append-map (lambda (expr) (list "\\|" expr))
                               (cdr exprs))))
      (r:seq)))

(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max) (list expr "*"))
                       ((= max min) '())
                       (else (make-list (- max min)
                                        (r:alt expr "")))))))

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
      (bracket string
               (lambda (members)
                 (if (lset= eqv? '(#\- #\^) members)
                     '(#\- #\^)
                     (quote-bracketed-contents members)))))))

(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
    (append '(#\[)
            (procedure (string->list string))
            '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove
            (lambda (c)
              (memv c chars-needing-quoting-in-brackets))
            members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))


(define (r:* expr) (r:repeat 0 #f expr))
(define (r:+ expr) (r:repeat 1 #f expr))


(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e"
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (bourne-shell-quote-string string)
  (list->string
    (append (list #\')
            (append-map (lambda (char)
                          (if (char=? char #\')
                              (list #\' #\\ char #\')
                              (list char)))
                        (string->list string))
            (list #\'))))

;; helper functions

(define (not x) (if x #f #t))

(define (list . x) x)

(define (make-list n x)
  (if (<= n 0)
      '()
      (cons x (make-list (- n 1) x))))

(define (append-map func list)
  (if (null? list)
      '()
      (append (func (car list))
              (append-map func (cdr list)))))


(define (list->string list-of-chars)
  (apply string list-of-chars))

(define (remove pred? list)
  (cond ((null? list))
        ((pred? (car list))
         (remove pred? (cdr list)))
        (else
          (cons (car list)
                (remove pred? (cdr list))))))

(define (lset= pred set-a set-b)
  (and (subset? pred set-a set-b)
       (subset? pred set-b set-a)))

(define (subset? pred set-a set-b)
  (if (null? set-a)
      #t
      (if (contains? pred (car set-a) set-b)
          (subset? pred (cdr set-a) set-b)
          #f)))

(define (contains? pred item set)
  (cond ((null? set) #f)
        ((pred item (car set)) #t)
        (else (contains? pred item (cdr set)))))
