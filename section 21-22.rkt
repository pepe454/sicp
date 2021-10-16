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



; EX 2.1 start - data abstraction

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


; EX 2.2
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

; EX 2.2
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



; EX 2.3
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
;(length test-rectangle-2)
;should be 5.
;(width test-rectangle-2)

;should be 10+10+5+5 = 30
;(perimeter test-rectangle-2)
;should be 10*5 = 30
;(area test-rectangle-2)

; thoughts of data:
; collection of selectors and constructors with specified cnds
; that the procedures of selection + construction must fulfill.

; having first-class procedures + functions make it
; possible to represent compound data, dare i say objects,
; 


; EX 2.4
(define (cons-ex x y)
  (lambda (m) (m x y)))
(define (car-ex z)
  (z (lambda (p q) p)))
(define (cdr-ex z)
  (z (lambda (p q) q)))

; should be 7
; (cdr-ex (cons-ex 5 7))
; why? here is the substitution method in action
; (cdr-ex (lambda (m) (m 5 7)))
; ((lambda (m) (m 5 7)) (lambda (p q) q))
; ((lambda (p q) q) 5 7)
; ((lambda (5 7) 7)
; 7

; ex 2.5
(define (cons-int a b)
  (if (and (> a -1)
           (> b -1))
      (* (expt 2 a)
         (expt 3 b))
      (error "Argument must be >= 0: " a)))

(define (get-pow base divisions remaining)
  (if (not (= (modulo remaining base) 0))
      divisions
      (get-pow base (+ divisions 1) (/ remaining base))))

; log base 2 operation
(define (car-int int)
  (get-pow 2 0 int))

; log base 3 operation
(define (cdr-int int)
  (get-pow 3 0 int))

; since the number is 2^a * 3^b
; you want to get a. how?

; heres an idea. we could try keep dividing by 3 or 2.
; no way to know what is a or b.
; log is just repeated division though, yeah?
  
; should be 2^2*3^3 = 4*27 = 108
;(cons-int 2 3)

; should be 2
;(car-int (cons-int 2 3))
; should be 3
;(cdr-int (cons-int 2 3))


; EX 2.6

(define zero
  (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; zero
; (lambda (f) (lambda (x) x))
; after this you anything you pass will return a function that returns anything
; for example, (zero 0) just returns (lambda (x) x)
; and ((zero 0) 0) just returns 0 it essentially does nothing.
; should be 0
;((zero 0) 0)
; (add-1 zero)
; (lambda (f) (lambda (x)(f ((zero f) x))))
; (lambda (f) (lambda (x) (f x)))
(define one
  (lambda (f) (lambda (x) (f x))))

;(add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

; EX 2.7
(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))


; EX 2.8

(define (sub-interval a b)
  (make-interval (- (lower-bound a)
                    (lower-bound b))
                 (- (upper-bound a)
                    (upper-bound b))))

(define (print-interval interval)
  (display "[")
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display "]")
  (newline))

;(print-interval (sub-interval (make-interval 10 15)
;                              (make-interval 20 30)))

; EX 2.9

(define (width-interval interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

;(width-interval (make-interval 10 12))


; assume interval [a,b] and interval [c,d]
; their widths are (b/2-a/2), w1 and (d/2-c/2), w2
; their sum is [a+c, b+d],
; whose width is b+d/2 - (a+c)/2
; further: b/2 + d/2 - a/2 - c/2
; notice this can be grouped as (b/2-a/2) + (d/2-c/2)
; this can now be substituted: w1 + w2

; however, this isn't true for mult. and division
; assume interval [a,b] and interval [c,d] and a,b,c,d nonnegative integers
; their widths are (b/2-a/2), w1 and (d/2-c/2), w2
; after multiplication, the new interval is
; [ac, bd], whose width is (bd-ac)/2 = bd/2 - ac/2
; this is not the same as b/2+d/2 - a/2-c/2.
; for example, assume a,b..d = 1,2..4
; b/2+d/2 - a/2-c/2 = 1+2-1/2-1.5 = 1
; bd/2-ac/2, however = 4 - 1.5 = 2.5

(define (spans-zero interval)
  (and (<= (lower-bound interval) 0)
       (>= (upper-bound interval) 0)))

;true
;(spans-zero (make-interval -5 5))
;true
;(spans-zero (make-interval -5 0))
;true
;(spans-zero (make-interval 0 5))
;false
;(spans-zero (make-interval -4 -1))
;false
;(spans-zero (make-interval 1 4))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; new division procedure
(define (div-interval x y)
  (if (spans-zero y)
      (error "Result is ambiguous when intervals span 0")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))


; should raise an Error
;(div-interval (make-interval 1 2)
;              (make-interval 0 5))
; should not raise error

; 4 0, 0 4, 2 2, 3 1, 1 3, 

; EX 2.11
; signs of the endpoints; well, there are 4 endpoints and 2 possible signs per endpoint.
; say mul interval [a,b] x [c,d]. since this also equals [c,d] x [a,b], some cases can be combined.
; suppose all negative, i.e. [-5,-4] x [-4,-3]. then the new interval is [12, 20] = [bd, ac]
; suppose all positive, i.e. [4,5] x [6,7]. then the new interval is [24, 35] = [ac, bd]
; suppose 2 neg and 2 positive i.e. [4,5] x [-7,-6]. then the interval is [-30, -28] = [bc, ad]
; supp. 3 neg, 1 positive i.e. [4,5] x [-5,6]. new interval is [-25, 30] = [bc, bd]
; supp. 2 neg, 2 positive again [-2, 2] x [-3, 3].

; EX 2.12
(define (make-center-percent c p)
  (let ((tolerance (* c p)))
    (if (> c 0)
        (make-interval (- c tolerance)
                       (+ c tolerance))
        (make-interval (+ c tolerance)
                       (- c tolerance)))))

; should be [25,75]
;(make-center-percent 50 0.5)
; should be [6,10]
;(make-center-percent 8 0.25)
; should be [-75,-25]
;(make-center-percent -50 0.50)

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

; should be 100
;(center (make-center-percent 100 0.5))
; should be 50
;(width-interval(make-center-percent 100 0.5))

(define (percent i)
  (abs (/ (width-interval i)
          (center i))))

; should be 0.5
;(percent (make-center-percent 100 0.5))
; should be 0.5
;(percent (make-center-percent -100 0.5))

; assume two intervals with all positive ints: [a,b], [c,d]
; if you wanna multiply them, what are the tolerances?
; well, we can assume the resulting intervals are [ac, bd] since a,b,c,d are positive.
; suppose the center and percent of interval 1 is c1,p1
;   and for the other, c2 and p2.
; then [a,b] = [c1-p1,c2+p2] and [c,d] = [c2-p2,c2+p2]
; their product is [c1c2-c1p2-c2p1+p1p2,... this is a bit of mess :]

; how about this:
; say interval 1 = [9, 11], c=10, p=10%. interval 2 = [9, 11], c=10, p=10%
; their product is [81, 121], with center = 101, p ~= 20%. it doubles.
; lets try again, so we have [18, 22], c=20,p=10% and [38,42], c=40, p=5%
; their product is [684, 924], c=804, p=15%, c=roughly 20x40


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


; closure: applying an operation to elements in the set produces
; elements that are again in the set.
; this is powerful, allowing us to represent hierarchichal strcutures;
; for exmaple a heap of heaps.

; since we are working with the list processing language, lisp,
;   of course we are going to be working with lists of lists!

; this is a nice little linked list
(define list1 (cons 1 (cons 2 (cons 3 (cons 4 nil)))))
(define list2 (list 1 2 3 4))

(define (lists-equal list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
        ((and (null? list1) (not (null? list2))) #f)
        ((and (not (null? list1)) (null? list2)) #f)
        ((not (= (car list1) (car list2))) #f)
        (else (lists-equal (cdr list1) (cdr list2)))))
; should be true
;(lists-equal list1 list2)


; ex 2.17
(define (last-pair list)
  (cond ((null? list) nil)
        ((null? (cdr list)) list)
        (else (last-pair (cdr list)))))

;(last-pair (list 23 74 149 34))

(define (reverse list)
  (define (reverse-iter reversed remaining)
    (if (null? remaining)
        reversed
        (reverse-iter (cons (car remaining) reversed)
                      (cdr remaining))))                            
  (cond ((null? list) list)
        ((not (pair? list)) list)
        (else (reverse-iter (cons (car list) nil)
                            (cdr list)))))

;(reverse (list 1 4 9 16 25))

(define (no-more? coins)
  (null? coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

;(count-change 100)
(define us-coins (list 50 25 10 5 1))
; should be 292
;(cc 100 us-coins)

;(define uk-coins (list 100 50 20 10 5 2 1 0.5))
;(cc 10 uk-coins)


; procedure with arbitrary number of args
(define (filter list predicate)
  (cond ((null? list) nil)
        ((predicate (car list))
         (cons (car list)
               (filter (cdr list) predicate)))
        (else (filter (cdr list) predicate))))

(define (even? x)
  (= (modulo x 2) 0))

(define (odd? x)
  (not (even? x)))

; should be 2,4,6,8
;(filter (list 1 2 3 4 5 6 7 8) even?)
; should be 1,3,5,7,9
;(filter (list 1 2 3 4 5 6 7 8) odd?)


(define (same-parity x . z)
  (let ((parity-x (if (odd? x) odd? even?)))
    (cons x (filter z parity-x))))

; should be 1,3,5,7,9
;(same-parity 1 2 3 4 5 6 7)
; should be 2,4,6
;(same-parity 2 3 4 5 6 7)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

; (scale-list (list 1 2 3 4 5 6) 10)

; EX 2.21
(define (square-list items)
  (map (lambda (x) (* x x)) items))

; (square-list (list 1 2 3 4))

; EX 2.22

; it seems that louis reasoner needs to use some eval apply!
; the first implementation is wrong because he successively
; appends the results of (square item) to the front of answer,
; meaning that each iteration of iter things answer
; will place the next item in things at the front, essentially reversing the list

; the next implementation also shows some difficulties.
; that is because answer starts out as nil, meaning the first element is nil.

(define (square-list-louis items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
      (square (car things))))))
  (iter items nil))

; (square-list-louis (list 1 2 3 4 5 6 7 8 9 10))


; EX 2.23
(define (for-each proc list)
  (if (not (null? list))
      (and (proc (car list)) (for-each proc (cdr list)))))

;(for-each (lambda (x) (newline) (display x))
;          (list 57 321 88))


; this is trees! count the number of leaves in a tree
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define tree-1
  (cons (cons (cons 1 nil)
              (cons 2 nil))
        (cons 3 nil)))

; should be 3
; (count-leaves tree-1)

; EX 2.24
; (list 1 (list 2 (list 3 4)))
; should be: (1 (2 (3 4)))
; box notation should be

; (1 (2 (3 4)) -> [ a, b] s.t.
; a = [c, d], where c = 1, d = nil
; and b = (2 (3 4)) -> [e, f]
; where e = [g, h] where g=2, h=nil
; finally, f = (3 4) -> [i, j], where
; i = 3 and j = (4, ) -> [4 ,nil]


; lastly, expressed as a tree, this looks like:
;   (1 (2 (3 4)))
;       / \
;      1   (2 (3 4))
;           /   \
;          2    (3 4)
;                /  \
;                3  4

; EX 2.25
(define list3 (list 1 3 (list 5 7) 9))
; list3
; this is a little gnarly :)
; (car (cdr (car (cdr (cdr list3)))))

(define list4 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;list4
;(cadr (cadr (cadr (cadr (cadr (cadr list4))))))

; EX 2.27

(define (deep-reverse list)
  (define (reverse-iter reversed remaining)
    (if (null? remaining)
        reversed
        (reverse-iter (cons (deep-reverse (car remaining))
                            reversed)
                      (cdr remaining))))                            
  (cond ((null? list) list)
        ((not (pair? list)) list)
        (else (reverse-iter (cons (car list) nil)
                            (cdr list)))))

;(deep-reverse (list (list 1 2) (list 3 4)))


;EX 2.28

; this is something like a flatten procedure... 
(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))
; (fringe x)


; EX 2.29

;(define (make-mobile left right)
;  (list left right))

(define (make-mobile left right) (cons left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

;(define (make-branch length structure)
;  (list length structure))

(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))


(define branch1 (make-branch 10 12))
(branch-length branch1)
(branch-structure branch1)
(define branch2 (make-branch 11 15))
(define mobile1 (make-mobile branch1 branch2))
(define branch3 (make-branch 12 mobile1))
(define branch4 (make-branch 30 32))
(define mobile2 (make-mobile branch3 branch4))

;mobile1
;mobile2
(left-branch mobile1)
(right-branch mobile1)

; can get the total weight of a branch OR a mobile named x
(define (total-weight x)
  (cond ((null? x) 0)
        ; this is a weight
        ((not (pair? x)) x)
        ; this is a branch
        ((not (pair? (left-branch x)))
         (total-weight (branch-structure x)))
        (else (+ (total-weight (left-branch x))
                 (total-weight (right-branch x))))))

;(total-weight mobile1)
;(total-weight mobile2)

(define (torque branch)
  (* (branch-length branch)
     (total-weight branch)))

;(torque branch1)
;(torque branch2)
;(torque branch3)
;(torque branch4)

(define (balanced? x)
  (cond ((null? x) #t)
        ; weight
        ((not (pair? x)) #t)
        ; this is a branch
        ((not (pair? (left-branch x)))
         (balanced? (branch-structure x)))
        ; this is actually a mobile!
        (else (and (= (torque (left-branch x))
                      (torque (right-branch x)))
                   (balanced? (left-branch x))
                   (balanced? (right-branch x))))))


(define mobile4 (make-mobile branch1 branch1))
; should be true
;(balanced? mobile4)
(define branch5 (make-branch 10 mobile4))
(define mobile5 (make-mobile branch5 branch5))
; should be true
;(balanced? mobile5)
(define branch6 (make-branch 12 mobile4))
; should be false
;(balanced? (make-mobile branch5 branch6))

; HOORAY! It works when we change the underlying repr of mobile!
; it does not break encapsulation by accessing internals!
; perhaps the only faux pas here is the use of pair?
; but the rest is history :))))


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; EX 2.30

; i can do u one better. what if i generalize the mapping to map tree?

; this is literally the solution to 2.31 :)

(define (map-tree mapping tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree mapping sub-tree)
             (mapping sub-tree)))
       tree))


(define (scale-tree-2 tree factor)
  (map-tree (lambda (x) (* x factor)) tree))

;(scale-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)


(define (square-tree tree)
  (map-tree (lambda (x) (* x x)) tree))

;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; EX 2.32
; general algorithm:
; given a set, generate all subsets of a set; this is the powerset.
; for every element of the set, pop it, then get all subsets of the remaining set.
; once you have all subsets of remaining set, prepend the element
; to each subset in this temporary subset list.
; do this until you get to an empty list. 

(define (prepend-all item lists)
  (map (lambda (x) (append (list item) x)) lists))

; should be ((1), (1,1,2), (1,3,4), (1,5,6), (1,7,8))
;(prepend-all 1 (list (list nil) (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
                   
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (prepend-all (lambda (x) (append (list (car s)) x))))                    
        (append rest (map prepend-all rest)))))

; (subsets (list 1 2 3))

; sequences as conventional interfaces
; previous: the use of abstraction to allow for different
; implementations with the same common interface s.t. the user
; of the library does not care!

; 1.3 - use higher-order procedures to capture general common patterns.

; one crazy idea
; produce a "signal" by enumerating over a list, tree, or any kind of sequence.
; pass it through some kind of filter
; pass the filtered elems through a map which transduces/transforms
; accumulate results; sum, cons, what ever.


; what if we can organize signal flow??
; first how do we do this? organize it as lists. this way you can use list ops
;     to process each stage. wait. is this the ListProcessing language???

(define (accumulate op partial-result sequence)
  (if (null? sequence)
      partial-result
      (op (car sequence)
          (accumulate op partial-result (cdr sequence)))))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

; this is really pretty. very modular.
; think of it like this. you want to be able to compose your
; program with these esssential building blocks.

; EX 2.33
(define (map-custom p sequence)
  (accumulate (lambda (x y) (append (list (p x)) y)) nil sequence))
;(map-custom square (list 1 2 3 4))

(define (append-custom seq1 seq2)
  (accumulate cons seq2 seq1))
;(append-custom (list 1 2 3) (list 4 5 6))

(define (length-custom sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))
;(length-custom (list 1 2 3 4 5))


; EX 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; should be 1 + 2 + 4 + 8 = 15
;(horner-eval 2 (list 1 1 1 1))
; should be 1 + 6 + 40 + 32 = 79
;(horner-eval 2 (list 1 3 0 5 0 1))

; EX 2.35
(define (count-leaves-2 t)
  (length-custom (enumerate-tree t)))

(define tree-2
  (cons (cons (cons 1 nil)
              (cons (cons 2 nil)
                    (cons 4 nil)))
        (cons 3 nil)))

; should be 4
;(count-leaves-2 tree-2)


; EX 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define seqs (list (list 1 2 3) (list 1 2 3) (list 1 2 3)))
; should be (1*1*1) , (2*2*2) , (3*3*3) = (1 8 27)
; (accumulate-n * 1 seqs)


; EX 2.37
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

; should be 18 + 28 + 40 = 86
;(dot-product (list 3 4 5) (list 6 7 8))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define matrix-1
  (list (list 1 2 3 4)
        (list 4 5 6 6)
        (list 6 7 8 9)
        (list 9 8 7 6)))

; this is like an identity vector
; applying it to matrix-1 will result in a vector of the sum of each row.
; result should be (list (+ 1 2 3 4) (+ 4 5 6 6) (+ 6 7 8 9) (+ 9 8 7 6))
;        comes out to: (list 10 21 30 30)
(matrix-*-vector matrix-1 (list 1 1 1 1))

(define matrix-2
  (list (list 1 2 3)
        (list 4 5 6)
        (list 7 8 9)))

; transpose of
; (1 2 3)      (1 4 7)
; (4 5 6)      (2 5 8)
; (7 8 9)  is  (3 6 9)
;
(define (transpose mat)
  (accumulate-n (lambda (x y) (append (list x) y))
                nil mat))


; right-rotate of
; (1 2 3)      (7 4 1)
; (4 5 6)      (8 5 2)
; (7 8 9)  is  (9 6 3)
(define (right-rotate mat)
  (map reverse (transpose mat)))

;(transpose matrix-1)
;(transpose matrix-2)


(define matrix-3
  (list (list 1 2)
        (list 3 4)))

(define matrix-4
  (list (list 5 6)
        (list 7 8)))

; matrix-3:   matrix-4:
;  ( 1 2 )     (5 6)
;  ( 3 4 )     (7 8)

; matrix-3 X matrix-4 =
;  ( (1 2) * (5 7), (1 2) * (6 8))
;  ( (3 4) * (5 7), (3 4) * (6 8))
;   = 
;  (5+14,   6+16) 
;  (15+28, 18+32)
;   =
;  (19, 22)
;  (43, 50)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(matrix-*-matrix matrix-3 matrix-4)


; right or left folding indicates the direction
; the operations are applied in.
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
; should be 1 (4 / 2) = 1/2 = 0.5
(fold-right / 1 (list 2 4))
; should be (1/2) / 4 = 1/8 = 0.125
(fold-left / 1 (list 2 4))
; should be (1 (2 (3 ())
(fold-right list nil (list 1 2 3))
; should be ((() 1) 2 3)
(fold-left list nil (list 1 2 3))

; given a binary op and 2 arguments a, b
; fold-right and fold-left will produce the same
; values if (op a b) = (op b a).
; this is the associative property i believe.

; for example:

(fold-right * 1 (list 1 2 3 4))
(fold-left * 1 (list 1 2 3 4))

(fold-right + 0 (list 1 2 3 4))
(fold-right + 0 (list 1 2 3 4))


; EX 2.39
; since you are folding right, you actually want to cons
; the temp result on the right to the current item. 
(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
; should be 6 5 4 3 2 1
;(reverse-fold-right (list 1 2 3 4 5 6))

; since you are folding to the left, you want to
; append the partial result on the left to the right of the current item
(define (reverse-fold-left sequence)
  (fold-left (lambda (partial item) (append (list item) partial)) nil sequence))
; should be (6 4 5 ...)
;(reverse-fold-left (list 1 2 3 4 5 6))


; nested mappings:
; i.e. loop over all pairs of numbers i,j less than n.
; well, you can generate a sequence of all these pairs, then use filter.

; generating the pairs:
;(accumulate
; append nil (map (lambda (i)
;                   (map (lambda (j) (list i j))
;                        (enumerate-interval 1 (- i 1))))
;                 (enumerate-interval 1 n)))

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

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter-noo pred list)
  (filter list pred))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter-noo prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s) ; empty set?
      (list nil) ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;(define (enumerate-interval low high)
;  (if (> low high)
;      nil
;      (cons low (enumerate-interval (+ low 1) high))))

; EX 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs-new n)
  (map make-pair-sum
       (filter-noo prime-sum? (unique-pairs n))))

; time for a refactor yet? 
(define (unique-triples n)
  (flatmap
   (lambda (unique-pair)
     (map (lambda (j) (append unique-pair (list j)))
          (enumerate-interval 1 (- (cadr unique-pair) 1))))
   (unique-pairs n)))

; EX 2.41
(define (triple-sum n s)
  (define (sums-up? triple)
    (= (accumulate + 0 triple) s))
  (filter (unique-triples n) sums-up?))

;(triple-sum 10 10)

; EX 2.42 and 2.43- this is a really tricky problem.
; question 1: how do I represent an empty-board?
(define empty-board (list nil))
; question 2: what is a position?
; a position is a list of coordinates representing the places of the queens
; question 3: how do I represent a queen-place?
(define (make-queen-place row column)
  (list row column))
(define (row queen-place)
  (car queen-place))
(define (column queen-place)
  (cadr queen-place))

; question 4: how do I add a queen-place to a list of positions?
; append it to the front, this makes it easier for searching later on
; but there's a catch; 1. if list of position is nil, then make a new position.
;                      2. if the position already has new-col,
;                         make way for a new position to arise. 
(define (adjoin-position new-row new-col rest-of-queens)
  (if (null? (car rest-of-queens))
      ; make a new list of positions with only new-qp
      (list (list (make-queen-place new-row new-col)))
      (let ((new-qp (make-queen-place new-row new-col))
            (position (car rest-of-queens)))
        (if (not (= (column (car position)) new-col))
            ; add new-qp to the front of existing position.
            (cons (append (list new-qp) position) (cdr rest-of-queens))
            ; make a list of len 2 with postiion w/ new-qp and old-qp
            (list (append (list new-qp) (cdr position))
                  position
                  (cdr rest-of-queens))))))

(define rq1 (adjoin-position 1 1 empty-board))
; diff column
(define rq2 (adjoin-position 1 2 rq1))
; another diff column
;(define rq3 (adjoin-position 5 6 rq2))
; same column as 6: should now have 2 positions!
;(define rq4 (adjoin-position 7 6 rq3))
; another column: we should now have 3 positions!
;(define rq5 (adjoin-position 8 6 rq4))

; diag means that the difference between
; columns and diff between rows is the same!

; e.g. [1,1] is diagonal to [2,2], [3,3], [4,4] ...
(define (diag? new-row new-col old-row old-col)
  (= (abs (- new-row old-row))
     (abs (- new-col old-col))))

(define (safe? new-col position)
  (define (test-places new-row queen-places)
    (if (null? queen-places)
        #t
        (let ((queen-place (car queen-places)))
          (cond ((= new-row (row queen-place)) #f)
                ((diag? new-row new-col (row queen-place) (column queen-place)) #f)
                (else (test-places new-row (cdr queen-places)))))))  
  (let ((new-place (caar position)))
    (if (null? new-place)
        #t
        (test-places (row new-place)                     
                     (cdar position)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter-noo
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (display-position position)
  (define (display-row row)
    (for-each display row)
    (newline))
  (define (display-qp num-rows queen-place)
    (list (map
           (lambda (r) (if (= (row queen-place) r) "X " "0 "))
           (enumerate-interval 1 num-rows))))
  ; number of rows is the same as number of columns
  ; the first qp has the largest number of columns.
  (let ((num-rows (column (car (car position)))))
    (for-each display-row
              (right-rotate
               (flatmap (lambda (qp) (display-qp num-rows qp))
                        (car position)))))
    (newline))


;(for-each display-position (queens 8))


; EX 2.44
;(define wave2 (beside wave (flip-vert wave)))
;(define wave4 (below wave2 wave2))
;(define (flipped-pairs painter)
;  (let ((painter2 (beside painter (flip-vert painter))))
;    (below painter2 painter2)))
;(define wave4-2 (flipped-pairs wave))
;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))
;
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1))))
;        (below painter (beside up up)))))
;
;(define (corner-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1)))
;            (right (right-split painter (- n 1))))
;        (let ((top-left (beside up up))
;              (bottom-right (below right right))
;              (corner (corner-split painter (- n 1))))
;          (beside (below painter top-left)
;                  (below bottom-right corner))))))
;
;(define (square-limit painter n)
;  (let ((quarter (corner-split painter n)))
;    (let ((half (beside (flip-horiz quarter) quarter)))
;      (below (flip-vert half) half))))
;
; goal is to make higher order procedures
; with the intent to take in painter operations and
; return new, modified painter operations.

;(define (square-of-four tl tr bl br)
;  (lambda (painter)
;    (let ((top (beside (tl painter) (tr painter)))
;          (bottom (beside (bl painter) (br painter))))
;      (below bottom top))))


;(define (flipped-pairs-2 painter)
;  (let ((combine4 (square-of-four identity flip-vert
;                                  identity flip-vert)))
;    (combine4 painter)))


;(define (square-limit-2 painter n)
;  (let ((combine4 (square-of-four flip-horiz identity
;                                  rotate180 flip-vert)))))


; 2.45
;(define (split combine-1 combine-2)
;  (define (split-recurse painter n)
;    (if (= n 0)
;        painter
;        (let ((smaller (split-recurse painter (- n 1))))
;          (combine-1 painter (combine-2 smaller smaller)))))
;  split-recurse)

; 2.46
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))


(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

; paint a series of lines in segment-list
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame)
;         (start-segment segment))
;        ((frame-coord-map frame)
;         (end-segment segment))))
;     segment-list)))


;(define (make-segment v1 v2)
;  (list v1 v2))

;(define (start-segment segment)
;  (car segment))

;(define (end-segment segment)
;  (cadr segment))
 
