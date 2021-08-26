#lang sicp

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (factorial-iter i n product)
    (cond ((< n 0) 0)
          ((= i n)(* i product))
          (else (factorial-iter (+ i 1) n (* product i)))))
  (factorial-iter 1 n 1))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib2 n)
  (define (fib-iter i n current-fib sum)
    (if (>= i n)
        current-fib
        (fib-iter (+ i 1) n sum (+ current-fib sum))))
  (fib-iter 0 n 0 1))

(define (func n)
  (if (< n 3)
      n
      (+ (func (- n 1))
         (* 2 (func (- n 2)))
         (* 3 (func (- n 3))))))

; get pascal element in row, col
(define (pascal row col)
  (cond ((= row col) 1)
        ((= col 1) 1)
        (else (+ (pascal (- row 1)
                         (- col 1))
                 (pascal (- row 1)
                         col)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

;successive squaring using an iterative method
(define (expt b n)
  (define (expt-iter a b n)
    (cond ((<= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (expt-iter 1 b n))

(define (double n) (+ n n))

(define (halve n) (/ n 2))

(define (mult2 a b)
  (cond ((= b 1) a)
        ((= b -1) (- a))
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult  a (- b 1))))))

(define (mult a b)
  (define (mult-iter sum a b)
    (cond ((= b 0) sum)
          ((even? b) (mult-iter sum (double a) (halve b)))
          (else (mult-iter (+ sum a) a (- b 1)))))
  (mult-iter 0 a b))

(define (smallest-divisor n) (find-divisor n 2))
(define (next test-divisor)
  (if (= 2 test-divisor) 3 (+ 2 test-divisor)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
         
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      #f))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes start primes)
  (define (search-primes-iter found current-test)
    (cond ((= found primes)
           (display "\nDone!"))
          ((even? current-test)
           (search-primes-iter found (+ 1 current-test)))
          ((timed-prime-test current-test)
           (search-primes-iter (+ found 1) (+ 1 current-test)))
          (else (search-primes-iter found (+ 1 current-test)))))
  (search-primes-iter 0 start))


(define (carmichael-test n)
  (define (carmichael-iter a n)
    (cond ((= a n) #t)
          ((=(expmod a n n) a)
           (carmichael-iter (+ a 1) n))
          (else #f)))
  (carmichael-iter 2 n))

(define (expmod-miller-rabin base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))


