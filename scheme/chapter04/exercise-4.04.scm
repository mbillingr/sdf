(define algebra-3
  (rule-simplifier
   (list

    ;; Sums

    (rule `(+ (? a)) a)

    (rule `(+ (?? a) (+ (?? b)) (?? c))
          `(+ ,@a ,@b ,@c))

    (rule `(+ (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
               `(+ ,@a ,x ,y ,@b)))


    ;; Products

    (rule `(* (? a)) a)

    (rule `(* (?? a) (* (?? b)) (?? c))
          `(* ,@a ,@b ,@c))

    (rule `(* (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
               `(* ,@a ,x ,y ,@b)))


    ;; Distributive law

    (rule `(* (?? a) (+ (?? b)) (?? c))
          `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))


    ;; Numerical simplifications below

    (rule `(+ 0 (?? x)) `(+ ,@x))

    (rule `(+ (? x ,number?) (? y ,number?) (?? z))
          `(+ ,(+ x y) ,@z))


    (rule `(* 0 (?? x)) 0)

    (rule `(* 1 (?? x)) `(* ,@x))

    (rule `(* (? x ,number?) (? y ,number?) (?? z))
          `(* ,(* x y) ,@z))

    ;; Term collecting

    (rule `(+ (?? x) (* (? a ,number?) (?? c)) (* (? b ,number?) (?? c)) (?? z))
          `(+ ,@x (* ,(+ a b) ,@c) ,@z))

    (rule `(+ (?? x) (?? c) (* (? b ,number?) (?? c)) (?? z))
          `(+ ,@x (* ,(+ 1 b) ,@c) ,@z))

    (rule `(+ (?? x) (? c) (? c) (?? z))
          `(+ ,@x (* 2 ,c) ,@z)))))

; given: (algebra-3 `(+ y (* x -2 w) (* x 4 y) (* w x) z (* 5 z) (* x w) (* x y 3)))
; then: (+ y (* 6 z) (* 7 x y))
