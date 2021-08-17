
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

(define (person name)
  (assoc name people))

(define (name person)
  (car person))

(define (gender person)
  (car (cdr person)))

(define (position person)
  (car (cdr (cdr person))))

(define (hand-score person)
  (car (cdr (cdr (cdr person)))))


(define (perm) (permutation 2 '(a b) '()))

(define (permutation (seq lazy memo))
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
  (define ben 'uninitialized)
  (define eva 'uninitialized)
  (define alyssa 'uninitialized)
  (define luis 'uninitialized)
  (define cy 'uninitialized)
  (define lem 'uninitialized)

  (define people 'uninitialized)

  (define counter 0)

  (maybe-set! people (list (list 'ben 'man 1 20)
                           (list 'cy 'man (amb 2 3 ) (amb 10 20 30))
                           (list 'lem 'man (amb 2 3) (amb 10 20 30))))

  (if (= (% counter 10000) 0)
      (begin (display counter) (newline))
      'ok)
  (set! counter (+ counter 1))

  (display "Solution") (newline)
  (for-each (lambda (person)
              (display person)
              (newline))
            people)
  (newline)

  (require (not (= (hand-score (person 'ben)) (hand-score (person 'lem)))))
  (require (not (= (hand-score (person 'ben)) (hand-score (person 'cy)))))
  (require (not (= (hand-score (person 'cy)) (hand-score (person 'lem))))))
