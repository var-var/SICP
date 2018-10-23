#lang racket

"Flying first-class"
;; Write Scheme expressions with the following names and behaviors:
;; 1. divide-by: Given a number, return a procedure that accepts a number and divides it by this
;;               first number
(define divide-by
  (lambda (n)
    (lambda (x)
      (/ x n))))

;; 2. square-and-add: Given a number, return a procedure that accepts a number, squares it, and
;;                    adds the first number
(define square-and-add
  (lambda (n)
    (lambda (x)
      (+ (* x x) n))))
  

;; 3. compose: Given two procedures, return a procedure which, given an input, applies the second
;;             function then the first. Thus ((compose f g) 5) is equivalent to (f (g 5))
(define compose  
  (lambda (f g)
    (lambda (input)
      (f (g input)))))

;; Test case:
((compose (square-and-add 42) (divide-by 2)) 20)    ;should produce 142


"consider this"
"Ex.2:"
;; 2. Write expressions whose values will print out like the following.
;;      (1 2 3)
;;      (1 2 . 3)
;;      ((1 2) (3 4) (5 6))
(cons 1 (cons 2 (cons 3 null)))
(cons 1 (cons 2 3))
(list (list 1 2) (list 3 4) (list 5 6))
"Ex.3:"
;; 3. Write expressions using car and cdr that will return 4 when the name lst is bound to the
;; following values:

;; (7 6 5 4 3 2 1):
(car (cdr (cdr (cdr (list 7 6 5 4 3 2 1)))))
;; ((7) (6 5 4) (3 2) 1)
(car (cdr (cdr (car (cdr  (list 7 (list 6 5 4) (list 3 2) 1))))))
;; (7 (6 (5 (4 (3 (2 (1)))))))
(car
 (car (cdr
       (car (cdr
             (car (cdr (list 7 (list 6 (list 5 (list 4 (list 3 (list 2 (list 1))))))))))))))
;; (7 ((6 5 ((4)) 3) 2) 1)
(car
 (car
  (cdr
   (cdr
    (car
     (car
      (cdr (list 7 (list (list 6 5 (list 4) 3) 2) 1))))))))

"Down for the Count"
;; Write a procedure, list-ref, with type List<A>, non-negative integer -> A, which will return
;;       the Nth element of a list. Start counting from 0 like any good computer scientist.

;; (listof X), Natural -> X
(define list-ref
  (lambda (L n)
    (cond ((zero? n) (car L))          
          (else
           (if (null? (cdr L))
               "Error: out of range"
               (list-ref (cdr L) (- n 1)))))))

;; Test cases:
(list-ref (list "a" "b" "c" "d") 3)   ;should produce "d"
(list-ref (list "a" "b" "c" "d") 4)   ;should produce "Error: out of range"
(list-ref (list null) 0)              ;should produce null

"Copy cat"
;; Produce a copy of the given list
(define copy
  (lambda (L)
    (cond ((null? L) null)
          (else
           (cons (car L)
                 (copy (cdr L)))))))

;; Test cases:
(define L1 (list 1 5 (list 8 9) 'foo (quote bar)))
(eq? L1 (copy L1))                                 ;should produce "false"
(eq? (copy L1) (copy L1))                          ;should produce "false"
(equal? L1 (copy L1))                              ;should produce "false"

  
"Got it backwards"
;; Write a procedure reverse which, given a list L, returns a new list where the elements appear in
;;       the reverse order.

;; (listof X) -> (listof X)
;; Consume list and produce a new list where the elements appear in the reverse order.
(define (reverse L)
  ;; (listof X), Natural -> Natural
  ;; Produce the length of the list
  (define (reverse-helper-lengthL L acc)
    (cond ((null? L) acc) 
          (else
           (reverse-helper-lengthL (cdr L) (+ 1 acc)))))
  ;; (listof X), Natural -> (listof X)
  ;; Produce a new list with items in reverse order (using list-ref).
  (define (reverse-helper-consL L index)
    (cond ((< index 0) null)
          (else
           (cons (list-ref L index)
                 (reverse-helper-consL L (- index 1))))))
  (reverse-helper-consL L (- (reverse-helper-lengthL L 0) 1)))

;; Test cases:
(reverse (list 1 2 3 4 5))                   ;should produce '(5 4 3 2 1)
(reverse (list (list 1 2) (list 3 4) 5))     ;should produce '(5 (3 4) (1 2))     





