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

(define (r->string/level precedence-level)
  (lambda (expr)
    (let ((p (precedence expr)))
      (let ((str-expr (cond ((seq? expr)
                             (apply string-append (map (r->string/level p) (cdr expr))))
                            ((alt? expr)
                             (join "\\|" (map (r->string/level p) (cdr expr))))
                            ((*? expr)
                             (string-append ((r->string/level p) (cadr expr)) "*"))
                            ((set? expr)
                             (string-append "\\[" (cadr expr) "\\]"))
                            (else expr))))
        (if (< p precedence-level)
            (group str-expr)
            str-expr)))))

(define r->string (r->string/level 0))

(define (group expr)
  (string-append "\\(" expr "\\)"))

(define (join separator sequence)
  (apply string-append
         (cons (car sequence)
               (append-map (lambda (expr) (list separator expr))
                           (cdr sequence)))))

(define (precedence expr)
  (cond ((alt? expr) 1)
        ((seq? expr) 2)
        ((*? expr) 3)
        ((set? expr) 4)
        (else 5)))

(define (r:seq . exprs)
  (cons 'sequence exprs))

(define (seq? expr)
  (and (pair? expr)
       (eq? (car expr) 'sequence)))

(define (r:alt . exprs)
  (cons 'alternative exprs))

(define (alt? expr)
  (and (pair? expr)
       (eq? (car expr) 'alternative)))

(define (r:* expr)
  (list '* expr))

(define (*? expr)
  (and (pair? expr)
       (eq? (car expr) '*)))

(define (r:set expr)
  (list 'set expr))

(define (set? expr)
  (and (pair? expr)
       (eq? (car expr) 'set)))

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

(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max) (list (r:* expr)))
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
  (r:set
    (list->string
      (procedure (string->list string)))))

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

(define (cadr x) (car (cdr x)))

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

(define (map func list)
  (map-reduce func cons '() list))

(define (map-reduce mapper reducer init list)
  (if (null? list)
      init
      (reducer (mapper (car list))
               (map-reduce mapper reducer init (cdr list)))))


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
