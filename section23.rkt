#lang sicp

; Symbolic data: work with arbitrary symbols as data

(define a 1)
(define b 2)
;(list a b)
;(list 'a 'b)
(define false #f)
(define true #t)
(define abc (list '(a b c)))
;(list '123)
;(list nil)
;(list '())

(define (memq symbol symbols)
  (cond ((null? symbols) false)
        ((eq? symbol (car symbols)) symbols)
        (else (memq symbol (cdr symbols)))))

;(memq 'apple '(x (apple sauce) y apple pear))

; EX 2.53
; (a b c)
; ((george))
; ((y1 y2))
; (y1 y2)
; #f
; #f
; (red shoes blue socks)

(define (list-equal? list1 list2)
  (cond ((and (null? list1)
              (null? list2)) true)
        ((or (null? list1)
             (null? list2)) false)
        ((not (eq? (car list1)
                   (car list2))) false)
        (else (list-equal? (cdr list1)
                           (cdr list2)))))

;(list-equal? '(this is a list) '(this is a list))
;(list-equal? '(this is a list) '(this (is a) list))

; EX 2.55
; when you quote a quote, "quote" is itself now part of the symbol.
; the first character in a quoted quote is '.
; thus, the first element, the car, is '


; taking some derivatives
; ax2 + bx + c should be derived to 2ax + b
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-expr-prefix op v1 v2)
  (list op v1 v2))
(define (numbers? a b)
  (and (number? a) (number? b)))
(define (get-next exp)
  (let ((op (car exp)))
    (cond ((null? (cdddr exp))
           ; there are only two arguments in the expression
           (caddr exp))
          ; there are only three arguments in the expression 
          ((null? (cddddr exp)) (make-expr-prefix op (caddr exp) (cadddr exp)))
          ; there are more than three args
          (else (make-expr-prefix op (caddr exp) (cdddr exp))))))


; EX 2.57

; sums, addition
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((numbers? a1 a2)
         (+ a1 a2))
        (else (make-expr-prefix '+ a1 a2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (get-next s))


; products multiplication
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((numbers? m1 m2) (* m1 m2))
        (else (make-expr-prefix '* m1 m2))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (get-next p))


; EX 2.56

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        ((numbers? base exponent)
         (expt base exponent))
        (else (make-expr-prefix '** base exponent))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

;(make-exponentiation 1 1)
;(make-exponentiation 2 1)
;(make-exponentiation 5 0)
;(make-exponentiation 4 3)
;(make-exponentiation 'b 1)
;(make-exponentiation 'b 2)
;(exponentiation? '(** x 1))
;(make-exponentiation 'a 3)
;(exponentiation? (make-exponentiation 'a 3))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((old-e (exponent exp))
               (old-b (base exp)))
           (make-product
            (make-product old-e (make-exponentiation old-b (make-sum old-e -1)))
            (deriv old-b var))))
        (else
         (error "unknown expression type: DERIV" exp))))

;(deriv '(* (* x y) (+ x 3)) 'x)
;(deriv '(+ x 3) 'x)
;(deriv '(* x y) 'x)
;(deriv '(** x 4) 'x)
;(deriv '(** 2 4) 'x)


; representing sets 2.3.3

; sets have a bunch of different potential representations
; distinct objects collection. ops: union-set, intersection, element of ? adjoin
; ..

; adjoin-set: add an object to a set
; union-set: union of two sets

; Possible representations:
; list: do a linear search for element-of-set?
; adjoin-set: simple cons the next set
; intersection-set: recursive algo.
; if car(S1) is element of S2, (cons (car S1) (intersetion-set S1 S2))

; O(n) search, linnear search. not great.
; O(n^2) linear search for each elem of S1 -> no bueno.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; EX 2.59

; make the assumption that S1 and S2 are both sets. 
(define (union-set s1 s2)
  (define (union-accum accum remaining)
    (display s1)
    (cond ((null? remaining) accum)
          ((not (element-of-set? (car remaining) s1))
           (union-accum (cons (car remaining) accum) (cdr remaining)))
          (else (union-accum accum (cdr remaining)))))
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (union-accum s1 s2))))

;(union-set (list 1 2 3 4) (list 7 6 3 2))


; EX 2.60

; first of all, element-of-set? does not change implementation

; with dups, it doesn't matter if x is already a part of the set
(define (adjoin-set-dup x set)
  (cons x set))

; allow dups. doesn't matter if any elems of s2 are elements of s1
(define (union-set-dup s1 s2)
  (append s1 s2))

; intersection set doesn't change. but you will end up repeating
; a few element searches for duplicated elements.

; when would you prefer this to non-duplicate?
; in many ways this is inferior to the previous.
; it is slower for intersections, since you have to do
; duplicate lookups.
; it can be slower for searches, since the overall size of sets
; with duplicates is going to be larger than unique-element sets.
; finally, set implementations are used to represent unique collections.
; depending on the use case, these use cases want a unique representation
; of elements, for example, de-duplicating lists.
; that use case won't be supported by this implementation.

; how is it better? inserts and unions are extremely fast, owing to the fact
; that one doesn't need to search before insertion.
; when might this be preferred to non-duplicate? probably when you expect
; a very uniform distribution of to-be-inserted elemtents; i.e. you don't expect
; to get many duplicates. moreover, if you expect to see many more unions and
; insertions than intersections/searches.

; ordered implementation:
(define (element-of-set-ordered? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; much quicker: O(n) where n is the sum of S1 and S2
; you don't need to search through the lists. can immediately
; insert the smaller element into the intersection set. 
(define (intersection-set-ord set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ord (cdr set1)
                                              (cdr set2))))
              ((< x1 x2)
               (intersection-set-ord (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ord set1 (cdr set2)))))))


; EX 2.61

(define (adjoin-set-ordered x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set))
         ;(display "i guess that ")
         ;(display x)
         ;(display " is less than ")
         ;(display (car set))
         ;(newline)
         (cons x set))
        (else (cons (car set) (adjoin-set-ordered x (cdr set))))))

;(define s1 (adjoin-set-ordered 1 '()))
;(define s2 (adjoin-set-ordered 2 s1))
;(define s3 (adjoin-set-ordered -1 s2))
;(define s4 (adjoin-set-ordered 8 s3))
;(define s5 (adjoin-set-ordered 10 s4))
;(define s6 (adjoin-set-ordered 9 s5))
;(adjoin-set-ordered 0 s6)



; EX 2.62
; luckily for us, the union-set-ordered operation is also n + n = O(n)
(define (union-set-ordered s1 s2)
  (define (union-helper set1 set2)
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (union-set-ordered (cdr set1)
                                         (cdr set2))))
            ((< x1 x2)
             (cons x1 (union-set-ordered (cdr set1) set2)))
            ((< x2 x1)
             (cons x2 (union-set-ordered  set1 (cdr set2)))))))
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (union-helper s1 s2))))

;(union-set-ordered (list 2 4 6 7 9 10) (list 5 6 7 8 10 12))



; binary trees. let's have at it

(define (entry tree) (car tree))
;(define (left-branch tree) (cadr tree))
;(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; search takes O(logn)
(define (element? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element? x (left-branch set)))
        ((> x (entry set))
         (element? x (right-branch set)))))

; also takes o(logn)
(define (add x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (add x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (add x (right-branch set))))))

; both these claims require the tree to be balanced.
; why? the claims rest on the fact that you are cutting
; the search space down by half each time.

; but wait. if you are NOT cutting down by half each time,
; it isn't O(logn).

; B-trees and red-black trees. why don't you describe them here.
; please? pretty - please? with a cherry on top ...


; EX 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;(define t1 (add 10 (add 20 (add 5 (add 30 (add 50 (add 25 '())))))))
;(tree->list-2 t1)
;(define t2 (add 1 (add 11 (add 5 (add 3 (add 9 (add 7 '())))))))
;(tree->list-1 t2)
;(tree->list-2 t2)
;(define t3 (add 11 (add 9 (add 5 (add 7 (add 1 (add 3 '())))))))
;(tree->list-1 t3)
;(tree->list-2 t3)
;(define t4 (add 11 (add 7 (add 1 (add 9 (add 3 (add 5 '())))))))
;(tree->list-1 t4)
;(tree->list-2 t4)

; these procedures both produce the same result for each tree.
; supposing the tree is a BST, the procedures produce an ordered list.
; the order of growth is tied to the number of elements in the tree.
; they are both linear in growth to the number of elements i the tree.


; EX 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;(partial-tree (list 9 8 7 6 5 3 45 46 37 26 20) 10)

; partial-tree works in the following way:
; if there is an empty list of elements, it returns '()
; otherwise, partial-tree does a bit of a divide and conquer
; it does partial-tree on the left side,
; then partial-tree in the right side, then takes the elemtent
; in the middle and make-tree with middle, left, right.

; the order of growth is O(n)

; EX 2.65

(define (fast-union s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         ; convert both sets to lists in O(n) time
         (let ((list-set-1 (tree->list-1 s1))
               (list-set-2 (tree->list-2 s2)))
           ; perform union-set-ordered in O(n) time
           (let ((union-list
                  (union-set-ordered list-set-1 list-set-2)))
             ; convert partial-tree in O(n) time
             (let ((n (length union-list)))
               (list->tree union-list)))))))

; hooray! it mostly works!
;(define t1 (add 10 (add 20 (add 5 (add 30 (add 50 (add 25 '())))))))
;(define t2 (add 1 (add 50 (add 5 (add 3 (add 9 (add 7 '())))))))
;(tree->list-1 (fast-union t1 t2))
;(define

(define (make-entry key value)
  (list key value))
(define (key entry)
  (car entry))
(define (value entry)
  (cadr entry))

; EX 2.66

(define (lookup-set given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((entry-key (key (entry set-of-records))))
        (cond ((equal? given-key entry-key)
               (entry set-of-records))
              ((< given-key entry-key)
               (lookup-set given-key (left-branch set-of-records)))
              (else
               (lookup-set given-key (right-branch set-of-records)))))))     
                      

; huffman encoding trees
; the idea is to compress textual data by encoding it with
; less information than 1 byte per characer.
; you can also use variable length prefix codes to use less
; information for more frequent letters in the text.
; huffman tree use the direction you take in the tree to
; signify 1 or 0. go left if the bit is 0; otherwise, go right.
; once you hit a terminal node, a character, start back at the root.


; generate a tree s.t. lowest frequency symbols are the farthest
; from the root node.
; 1. collect frequency data.
; 2. find 2 leaves with lowest weights (sorted order here?)
;    and merge into one node with both leaves as children.
;    set the weight of the parent node at w(child1) + w(child2).

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))


; symbols and weight are generic procedures, because
; they can both work on leaves and trees. 
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree))) ; this is a full on tree.

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; recursively traverse the tree,
; making the decision of which way to go based on the bit
; at the beginning of bits list, and moving to the next
; bit in the bits list after deciding.
; at the end of a symbol decoding, the traversal returns
; to the root of the tree to start with the remaining bits. 
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (adjoin-set-huff x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-huff x (cdr set))))))

(define (member-set-huff x set)
  (cond ((null? set) #f)
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-huff x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set-huff (make-leaf (car pair) ; symbol
                                    (cadr pair)) ; frequency
                         (make-leaf-set (cdr pairs))))))

; EX 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display sample-tree)
;(decode sample-message sample-tree)
; result is A D A B B C A

; EX 2.68

(define (member? symbol symbol-list)
  (if (null? symbol-list)
    #f

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((or (null? tree) (not (member? symbol 
	((
