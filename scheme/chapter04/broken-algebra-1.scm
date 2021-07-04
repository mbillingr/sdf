
; Without (expr<? a b) the commutative law is always applied and the solution
; never reaches a fixed point.
; With the condition, the commutative law is only applied if two expressions do
; not follow the order implied by expr<?.

(define algebra-1
  (rule-simplifier
   (list
    ;; Associative law of addition
    (rule '(+ (? a) (+ (? b) (? c)))
          `(+ (+ ,a ,b) ,c))

    ;; Commutative law of multiplication
    (rule '(* (? b) (? a))
          `(* ,a ,b))

    ;; Distributive law of multiplication over addition
    (rule '(* (? a) (+ (? b) (? c)))
          `(+ (* ,a ,b) (* ,a ,c))))))
