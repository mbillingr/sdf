
(define (solve)
  (define n-strong 0)
  (define n-weak 0)

  (set! positions (cons 1 (cons 4 (permutation '(2 3 5 6)))))

  (set! scores (permutation '(1 2 3 4 5 6)))

  (require (= (position ben) 1))
  (require (= (position eva) 4))

  (require (stronger-hand (the-man (right-of alyssa)) lem))
  (require (stronger-hand (the-man (right-of eva)) ben))
  (require (stronger-hand (the-man (right-of ben)) eva))
  (require (stronger-hand (the-woman (right-of lem)) cy))
  (require (stronger-hand (the-woman (right-of cy)) luis))

  (require (not (= (the-man (right-of ben)) cy)))

  (set! n-weak (+ n-weak 1))
  (display "  weak solution: ")
  (display positions)
  (display scores)
  (newline)

  (require (stronger-hand (the-man (right-of ben)) cy))

  (set! n-strong (+ n-strong 1))
  (display "strong solution: ")
  (display positions)
  (display scores)
  (newline)

  (amb))


(define ben 0)
(define eva 1)
(define alyssa 2)
(define luis 3)
(define cy 4)
(define lem 5)

(define positions 'uninitialized)
(define scores 'uninitialized)


(define (stronger-hand person1 person2)
  (> (hand-score person1)
     (hand-score person2)))

(define (the-man person)
  (require (eq? (gender person) 'man))
  person)

(define (the-woman person)
  (require (eq? (gender person) 'woman))
  person)

(define (right-of person)
  (let ((right_pos (if (= (position person) 6)
                       1
                       (+ (position person) 1))))
    (cond ((= right_pos (position ben)) ben)
          ((= right_pos (position eva)) eva)
          ((= right_pos (position alyssa)) alyssa)
          ((= right_pos (position luis)) luis)
          ((= right_pos (position cy)) cy)
          ((= right_pos (position lem)) lem)
          (else (error "invalid position" right_pos)))))

(define (gender person)
  (cond ((= person ben) 'man)
        ((= person eva) 'woman)
        ((= person alyssa) 'woman)
        ((= person luis) 'man)
        ((= person cy) 'man)
        ((= person lem) 'man)
        (else #f)))

(define (position person)
  (list-ref person positions))

(define (hand-score person)
  (list-ref person scores))


(define (permutation seq)
  (define (loop n acc)
    (if (pair? acc)
        (require (not (memq (car acc) (cdr acc))))
        'ok)
    (if (= n 0)
        acc
        (loop (- n 1)
              (cons (amb* seq) acc))))
  (loop (length seq) '()))

(define (amb* seq)
  (if (null? seq)
      (amb)
      (amb (car seq) (amb* (cdr seq)))))

(define (require p)
  (if (not p) (amb) 'ok))

(define (deny p)
  (if p (amb) 'ok))

(define (memq obj sequence)
  (cond ((null? sequence)
         #f)
        ((eq? (car sequence) obj)
         sequence)
        (else (memq obj (cdr sequence)))))

(define (list-ref n seq)
  (if (= n 0)
      (car seq)
      (list-ref (- n 1) (cdr seq))))

(define (not p)
  (if p #f #t))


(solve)
