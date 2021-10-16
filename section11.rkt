#lang sicp
486
(+ 40 86)
(* 45 89)

(define size 2)
size

(define (power base exp)
  (cond ((= exp 0) 1)
        (else
         (* base (power base (- exp 1))))))
(power 2 5)
(power 3 3)
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference
  (* 2 pi radius))
circumference
(define area
  (* pi (expt radius 2)))

area

(define (square x)
  (* x x))

(define (sum-squares x y)
  (+ (square x) (square y)))

(sum-squares 20 20)


(define (even x)
  (= (modulo x 2) 0))

(define (even-only x)
  (cond ((even x) "hooray it is even!")
        (else "aww it is false.")))

(even-only 2)
(even-only 3)


; EX 1.2

(/ (+ 5 4 (- 2
             (- 3
                (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


; EX 1.3

(define (sum-squares-largest a b c)
  (cond ((and (<= a b) (<= a c)) (sum-squares b c))
        ((and (<= b a) (<= b c)) (sum-squares a c))
        (else (sum-squares a b))))

(sum-squares-largest 1 2 3)
(sum-squares-largest 4 4 2)
(sum-squares-largest 3 2 1)
(sum-squares-largest 1 1 1)
(sum-squares-largest 3 2 1)


; EX 1.5

(define (p)
  (p))

(define (test x y)
  (if (= x 0) 0 y))


; EX 1.7

(define (abs x)
  (if (< x 0) (- x) x))


; test if x is approx a good root for y with bound 
(define (root-test-approx guess x bound)
  (<= (abs (- x (* guess guess))) (abs bound)))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (newtons-method guess x)
  (if (root-test-approx guess x .00001)
      guess
      (newtons-method (improve guess x) x)))

(define (sqrt x)
  (newtons-method 1 x))

(sqrt 4)
(sqrt 3)
(sqrt 2)


; EX 1.8

; new guess test
(define (root-test-approx2 old-guess guess bound)
  (<= (/ (abs (- guess old-guess))
         guess)
      bound))

(define (improve2 guess x)
  (/ (+ guess (/ x guess)) 2))


(define (newtons-method2 old-guess guess x)
  (if (root-test-approx2 old-guess guess .00001)
      guess
      (newtons-method2 guess (improve2 guess x) x)))

(define (sqrt2 x)
  (newtons-method2 0 1 x))

(sqrt2 4)
(sqrt2 3)
(sqrt2 2)
(sqrt2 .0006)
(sqrt2 100000)
