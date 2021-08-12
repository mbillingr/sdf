from generic_interpreter import symbol
from generic_interpreter import initialize_repl, eval_str


def test_symbol_equality():
    a1 = symbol("a")
    a2 = symbol("a")
    b = symbol("b")

    assert a1 == a2
    assert a1 is a2
    assert a1 is not b
    assert a1 != b


def test_lazy_argument():
    initialize_repl()
    assert not eval_str(
        """
        (define f-called #f)
        (define (f) (set! f-called #t))

        (define (ignore (arg lazy)) 'ok)

        (ignore (f))
        f-called"""
    )


def test_lazy_memoization():
    initialize_repl()
    assert eval_str(
        """
        (define (kons (x lazy memo) (y lazy memo)) 
            (define (the_pair m) 
                (cond ((eq? m 'kar) x) 
                      ((eq? m 'kdr) y) 
                      (else (error "Unknown message -- kons" m x y)))) 
            the_pair)
        (eq? ((kons 1 2) 'kdr) 2)
        """
    ) is True


def test_stream():
    initialize_repl()
    assert eval_str(
        """
        (define (kons (x lazy memo) (y lazy memo)) 
            (define (the_pair m) 
                (cond ((eq? m 'kar) x) 
                      ((eq? m 'kdr) y) 
                      (else (error "Unknown message -- kons" m x y))))
            (global-hash-set! (cons 'kons-registration the_pair) #t) 
            the_pair)
        (define (kar x) (x 'kar))
        (define (kdr x) (x 'kdr))
        (define (kons? object)
            (if (global-hash-get (cons 'kons-registration object)) #t #f))
        
        (define the-empty-stream '())
        (define (empty-stream? thing) (null? thing))
        
        (define (add-streams s1 s2)
            (cond ((empty-stream? s1) s2)
                  ((empty-stream? s2) s1)
                  (else (kons (+ (kar s1) (kar s2))
                              (add-streams (kdr s1) (kdr s2))))))
                              
        (define (ref-stream stream n)
            (if (= n 0)
                (kar stream)
                (ref-stream (kdr stream) (- n 1))))
                
        (define (map-stream proc (items lazy memo))
            (if (empty-stream? items)
                items
                (kons (proc (kar items))
                      (map-stream proc (kdr items)))))
                      
        (define (scale-stream items factor)
            (map-stream (lambda (x) (* x factor))
                        items))
                 
        (define (integral (integrand lazy memo) initial-value dt)
            (define int
                (kons initial-value
                      (add-streams (scale-stream integrand dt)
                                   int)))
            int)  
            
        (define (solve f y0 dt)
            (define y (integral dy y0 dt))
            (define dy (map-stream f y))
            y)
            
        (ref-stream (solve (lambda (x) x) 1 0.001) 500)
        """
        # solution to exercise 5.8: make integrand lazy (and memoized)
    ) == 354224848179261915075
