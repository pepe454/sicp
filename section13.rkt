#lang sicp


(define (even? x)
  (= (remainder x 2) 0))
(define (cube x)
  (* x x x))
(define (square x)
  (* x x))

(define (sum2 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum2 term (next a) next b))))

; EX 1.30

(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b)
        result
        (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

;(sum-cubes 1 10)


(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

; EX 1.31

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


;(* 8 (pi-sum 1 1000000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


; EX 1.29

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (simps-mult x)
    (if (even? x)
        (* 2 (y x))
        (* 4 (y x))))
  (* (/ h 3)
     (+ (sum simps-mult 0 inc (- n 1))
        (y 0)
        (y n))))

;(simpsons-rule cube 0 1 1000)


(define (product-recurse term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recurse term (next a) next b))))

(define (product term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* result (term a)))))
  (product-iter a 1))

(define (pi-product b)
  (define (pi-term x)
    (/ (* (* 2 x) (+ (* 2 x) 2))
       (square (+ (* 2 x) 1))))
  (product-recurse pi-term 1 inc b))

(define (factorial n)
  (product identity 1 inc n))


; EX 1.32

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a b result)
    (if (> a b)
        result
        (accumulate-iter (next a) b (combiner result (term a)))))
  (accumulate-iter a b null-value))

(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate2 combiner null-value term (next a) next b))))

(define (sum-accum term a next b)
  (accumulate + 0 term a next b))

(define (product-accum term a next b)
  (accumulate * 1 term a next b))

(define (sum-integers-accum a b)
  (sum-accum identity a inc b))

;(sum-integers-accum 1 10)

(define (pi-sum-accum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-accum pi-term a pi-next b))

;(* 8 (pi-sum-accum 1 1000000))

(define (pi-product-accum b)
  (define (pi-term x)
    (/ (* (* 2 x) (+ (* 2 x) 2))
       (square (+ (* 2 x) 1))))
  (product-accum pi-term 1 inc b))


; EX 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (accumulate-iter a result)
    (cond ((> a b) result)
          ((filter a) (accumulate-iter (next a) (combiner result (term a))))
          (else (accumulate-iter (next a) result))))
  (accumulate-iter a null-value))

(define (next-div test-divisor)
  (if (= 2 test-divisor) 3 (+ 2 test-divisor)))

(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-div test-divisor)))))

(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (sum-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

;(sum-primes 1 5)


; EX 1.34
; let's the applicative order evaluation to see:
; (f f) -> (f 2) -> (2 2). since 2 is not a procedure this would cause an error.

(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;(half-interval-method sin 2.0 4.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; EX 1.35

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))
;golden-ratio
; golden ratio = x s.t. x^2 = x + 1
; show that it is a fixed point of x -> 1 + 1/x; divide both side by x. 
;

; EX 1.36

(define (damped-136 x)
  (* (/ 1 2)
     (+ (/ (log 1000)
           (log x))
        x)))

(define ex136
  (fixed-point (lambda (x) (damped-136 x))
               3.0))


; EX 1.37

; compute value of k-term finite continued fraction
; start n, d at 1, 1
(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (let ((numerator (n i))
          (denominator (d i)))
      (if (= i k)
          (/ numerator denominator)
          (/ numerator (+ denominator
                          (cont-frac-iter (+ i 1)))))))
  (cont-frac-iter 1))

(define (cont-frac-recurse n d k)
  (define (cont-frac-recurse-accum i accum)
   (let ((numerator (n i))
         (denominator (d i)))
     (if (= i 1)
         (/ numerator (+ denominator accum))
         (cont-frac-recurse-accum (- i 1)
                                  (/ numerator (+ denominator accum))))))
  (cont-frac-recurse-accum k (/ (n k) (d k))))
                   
(/ 1 golden-ratio)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

(cont-frac-recurse (lambda (i) 1.0)
                   (lambda (i) 1.0)
                   100)

; EX 1.38

;approximate e
(define (eulers-denoms i)
  (cond ((= i 1) 1)
        ((= (modulo i 3) 2)
         (* 2 (/ (+ i 1) 3)))
        (else 1)))

(cont-frac (lambda (i) 1.0)
           eulers-denoms
           100)


; EX 1.39

(define (tan-cf x k)
  (cont-frac (lambda (x)
               (if (= x 1) x (- (* x x))))
             (lambda (x) (- (* 2 x) 1))
             k))


(define (average-damp f)
  (lambda (x) (average x (f x))))


; dx is the change in x
(define dx 0.000001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt-new x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(sqrt-new 25)
(sqrt-new 100)
(sqrt-new 2)


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


; EX 1.40 
; allow for expression like
; (newtons-method (cubic a b c) 1) -> x^3 + ax^2 + bx + c
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(newtons-method (cubic 1 2 1) 1)

;cubic(-1)  = -1 + 1 - 1 + 1 = 0
;


; EX 1.41
(define (double proc)
  (lambda (x) (proc (proc x))))

;((double (double inc)) 5)

; EX 1.42
(define (compose f g)
  (lambda (x) (f (g x))))


;((compose square inc) 6)


; EX 1.43
(define (repeated f k)
  (define (repeated-lambda x)
    (define (repeated-recurse i x)
      (if (= i 1)
          (f x)
          (f (repeated-recurse (- i 1) x))))
    (repeated-recurse k x))
  repeated-lambda)


;((repeated square 2) 8)

; EX 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (- x dx))
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((n-fold-smooth square 10) 2)


(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(cube-root 27)
(cube-root 100)
(cube-root 1000)

; EX 1.45
(define (fourth-root x)
  (fixed-point (average-damp
                (average-damp (lambda (y) (/ x (cube y)))))
               1.0))

(define (seventh-root x)
  (fixed-point (average-damp
                (average-damp (lambda (y) (/ x (* (cube y) (cube y))))))
               1.0))

(seventh-root 100)


; sqrt need 1 average damp. cube need 1 average damp
; fourth need 2 average damp, fifth need 2 avg damp
; sixth need 2 average damp and its pretty good, 


(define (iterative-improve good? improve-guess)
  (define (improve-until-good guess)
    (if (good? guess)
        guess
        (improve-until-good (improve-guess guess))))
  improve-until-good)

; tolerance is (define tolerance 0.00001)
(define (fixed-point-iterative f initial-guess)
  (define (close-enough guess)
    ;(display guess)
    ;(newline)
    (< (abs (- guess (f guess)))
       tolerance))
  ((iterative-improve close-enough f) initial-guess))

(define golden-ratio-2
  (fixed-point-iterative (lambda (x) (+ 1 (/ 1 x)))
                         1.0))

golden-ratio-2

; EX 1.46

(define (sqrt-2 x)
  (fixed-point-iterative (average-damp (lambda (y) (/ x y)))
                         1.0))
