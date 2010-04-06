(define (sum-of-squares x y) (+
                                (* x x)
                                (* y y)))

(define (sos-of-greatest a b c) (+
                                    (if (or (a > b) (a > c))  (* a a) 0)
                                    (if (or (b > a) (b > c)) (* b b) 0)
                                    (if (or (c > a) (c > b)) (* c c) 0)
                                ))


(sos-of-greatest 3 4 5)