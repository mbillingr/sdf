
(define (for-each proc sequence)
  (if (null? sequence)
      'ok
      (begin (proc (car sequence))
             (for-each proc (cdr sequence)))))

(define (assoc key sequence)
  (cond ((null? sequence)
         #f)
        ((eq? (car (car sequence)) key)
         (car sequence))
        (else (assoc key (cdr sequence)))))

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

(define (require p)
  (if (not p) (amb) 'ok))

(define (deny p)
  (if p (amb) 'ok))

(define (not p)
  (if p #f #t))

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


(define (perm) (permutation 2 '(a b) '()))

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


(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (req a b) (require (= a b)))


(begin
  (define ben 0)
  (define eva 1)
  (define alyssa 2)
  (define luis 3)
  (define cy 4)
  (define lem 5)

  (define people 'uninitialized)

  (define n-solutions 0)

  (define positions (cons 1 (cons 4 (permutation '(2 3 5 6)))))

  (require (= (position ben) 1))
  (require (= (position eva) 4))

  (define scores (permutation '(1 2 3 4 5 6)))

  (require (stronger-hand (the-man (right-of alyssa)) lem))
  (require (stronger-hand (the-man (right-of eva)) ben))
  (require (stronger-hand (the-man (right-of ben)) cy))
  (require (stronger-hand (the-man (right-of ben)) eva))
  (require (stronger-hand (the-woman (right-of lem)) cy))
  (require (stronger-hand (the-woman (right-of cy)) luis))

  (set! n-solutions (+ n-solutions 1))
  (display positions)
  (display scores)
  (newline)
  (amb)

  (display n-solutions))
