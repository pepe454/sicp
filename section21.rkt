#lang sicp

; procedure can be a pattern for evolution of a process
; patterns for process: summation, accumulation, iterative improvement
; higher order procedures give much more expressive power.

; compound data is necessary for most programs.
; data abstraction: isolate compound data representation
;     from the places where data is used.

; the main takeaway is to use abstraction to deal with complexity.
; construct abstraction BARRIERS between parts of programs.

; closure: ability to combine compound data objects together.
; conventional interfaces to combine program modules

; data with elementary parts that are symbols, not numbers
; representing sets
; data struccture implementations

; data directed programming = design data representations isolation,
;     then combine data without modification.



; 2.1 start - data abstraction

; simplify the fractions
(define (make-rat n d)
  (define (sign x)
    (/ x (abs x)))
  (let ((g (gcd n d))
        (rat-sign (* (sign n) (sign d))))
    (cons (* rat-sign (abs (/ n g)))
          (abs (/ d g)))))

; ya basic
;(define (make-rat x y)
;  (cons x y))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


; (add-rat (make-rat 1 2) (make-rat 1 4))
; we have all these nice ops. so how do we make use of them?

; pairs
;(define x (cons 1 2))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
;(print-rat (make-rat -2 2))
;(print-rat (make-rat -4 2))
;(print-rat (make-rat 2 -5))
;(print-rat (make-rat -6 -8))


;2.2
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(define point1 (make-point 1 2))
;(x-point point1)
;(y-point point1)
;(print-point point1)

(define (axis-distance axis a b)
  (- (axis a) (axis b)))

(define (x-distance a b)
  (axis-distance x-point a b))

(define (y-distance a b)
  (axis-distance y-point a b))

(define (square x)
  (* x x))

(define (distance a b)
  (sqrt (+ (square (x-distance a b))
           (square (y-distance a b)))))

;(distance (make-point 1 4)
;          (make-point 1 10))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

;(define seg1 (make-segment 1 2))
;(start-segment seg1)
;(end-segment seg1)

(define (distance-segment segment)
  (distance (start-segment segment)
            (end-segment segment)))

;(length-segment (make-segment (make-point 1 4)
;                              (make-point 1 10)))


; take 2 points and a direction
; in order to see their distance

;(define test-segment
;  (make-segment (make-point 2 4)
;                (make-point 4 8)))

; output should be (3,6)
; (print-point (midpoint-segment test-segment))

;excercise 2.2
;make a rectangle that looks like
;a---------------b
;|               |
;|               |
;c---------------*
;such that a,b,c,* are points
;and segment a,b is length segment
;and segment a,c is the width segment

; rectangle implementation 1: make it with two segments!
(define (make-rectangle length-segment width-segment)
  (cons length-segment width-segment))

(define (length-segment rectangle)
  (car rectangle))

;(define (length rectangle)
;  (distance-segment (length-segment rectangle)))

(define (width-segment rectangle)
  (cdr rectangle))

;(define (width rectangle)
;  (distance-segment (width-segment rectangle)))

(define test-rectangle-1
  (make-rectangle (make-segment (make-point 10 10)
                                (make-point 20 10))
                  (make-segment (make-point 10 10)
                                (make-point 10 15))))
;length should be 20-10 = 10
;(length test-rectangle-1)
;width should be 15-10 = 5
;(width test-rectangle-1)                                

(define (perimeter rectangle)
  (+ (* 2 (length rectangle))
     (* 2 (width rectangle))))

(define (area rectangle)
  (* (length rectangle)
     (width rectangle)))

;perimeter should be 10 + 10 + 5 + 5 = 20
;(perimeter test-rectangle-1)
;area should be 10*5 = 50
;(area test-rectangle-1)


;second rectangle implementation!!!
; define a rectangle using only two diagonal corners like so:
;a------------------*
;|                  |
;|                  |
;*------------------b
;or
;*------------------a
;|                  |
;|                  |
;b------------------*
 
(define (make-rectangle-2 corner diagonal-corner)
  (cons corner diagonal-corner))

(define (length rectangle2)
  (abs (x-distance (car rectangle2)
                   (cdr rectangle2))))

(define (width rectangle2)
  (abs (y-distance (car rectangle2)
                   (cdr rectangle2))))

(define test-rectangle-2
  (make-rectangle-2 (make-point 0 5)
                    (make-point 10 0)))

;should be 10.
(length test-rectangle-2)
;should be 5.
(width test-rectangle-2)

;should be 10+10+5+5 = 30
(perimeter test-rectangle-2)
;should be 10*5 = 30
(area test-rectangle-2)
                                
