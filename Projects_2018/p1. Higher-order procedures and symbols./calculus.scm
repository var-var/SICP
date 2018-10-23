#lang racket


"Part 1: Numerical integration"

"Problem 1: Integrating any function"

(define (integral func num-steps x1 x2)
  (define (func-rect func x1 x2)
    (* (- x2 x1) (func x1)))
  (cond((equal? num-steps 1) (func-rect func x1 x2))
       (else
        (+ (func-rect func x1 (+ x1 (/ (- x2 x1) num-steps)))
           (integral func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2)))))

;; Test cases:

;; With only one step, the integral of y = x^2 from 3 to 5
;; should be 3^2 * 2 = 18
(integral (lambda (x) (expt x 2)) 1 3 5)
;; With two steps, we should get 3^2 + 4^2 = 25
(integral (lambda (x) (expt x 2)) 2 3 5)

"Problem 2: Area of a unit circle"

(define (approx-pi num-steps)
  (* 4
     (integral (lambda (x)  (sqrt (- 1 (expt x 2)))) num-steps 0 1))) 

(approx-pi 1)   ;; Should be 4
(approx-pi 2)   ;; Hopefully lower than 4
(approx-pi 600) ;; Right to the first two decimal places?

"Problem 3: Integrating with pieces of any shape"

(define (rectangle func x1 x2)
  (* (- x2 x1) (func x1)))

(define (trapezoid func x1 x2)
  (* (/ (+ (func x1) (func x2))
        2)
     (- x2 x1)))

(define (integral-with piece func num-steps x1 x2)
  (cond[(equal? num-steps 1) (piece func x1 x2)]
       [else
        (+ (piece func x1 (+ x1 (/ (- x2 x1) num-steps)))
           (integral-with piece func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))]))
 

;; Write your own test cases.  Start with checking that calling
;; (integral-with rectangle ...) is the same as calling (integral ...)
;; Then check that (integral-with trapezoid ...) produces better answers
;; for a given num-steps than the same (integral-with rectangle ...)

;; Test cases:
;; (integral-with rectangle ...) should be the same as calling (integral ...)
(integral-with rectangle (lambda (x) (expt x 2)) 1 3 5) ;; Should be 18
(integral-with rectangle (lambda (x) (expt x 2)) 2 3 5) ;; Should be 25
;; x1 = x2 should give 0
(integral-with rectangle (lambda (x) (expt x 2)) 10 3 3)
(integral-with trapezoid (lambda (x) (expt x 2)) 10 3 3)

(integral-with trapezoid (lambda (x) (expt x 2)) 1 3 5) ;; Should be larger than 18, smaller than 47.334
(integral-with trapezoid (lambda (x) (expt x 2)) 2 3 5) ;; Should be larger than 25, smaller than 40.334


"Problem 4: Better approximation of pi"

(define (better-pi num-steps)
  (* 4
     (integral-with trapezoid (lambda (x)  (sqrt (- 1 (expt x 2)))) num-steps 0 1)))

;; How many digits does (better-pi 600) get correct, compared to
;; the earlier (approx-pi 600) ?
(better-pi 1)
(better-pi 2)
(better-pi 600)

"Part 2: Symbolic differentiation"

(define (deriv-constant wrt constant)
  0)


"Problem 5: Derivative of a variable"

(define (deriv-variable wrt var)
  (cond ((equal? wrt var) 1)
        (else 0)))

;; Test cases:
(deriv-variable 'x 'x) ;; Should be 1
(deriv-variable 'x 'X) ;; Should be 0
(deriv-variable 'x 'y) ;; Should be 0

"Problem 6: Calling the right function"

(define (derivative wrt expr)
  (cond ((symbol? expr) (deriv-variable wrt expr))
        ((number? expr) (deriv-constant wrt expr))
        ((and (list? expr)
              (= 3 (length expr))
              (eq? `+ (first expr)))
         (deriv-sum wrt expr))        
        (else (error "Don't know how to differentiate" expr))))

;; Test cases:
(derivative 'x 3)  ;; Should be 0
(derivative 'x '3) ;; Should be 0
(derivative 'x 'y) ;; Should be 0
(derivative 'x 'x) ;; Should be 1


"Problem 7: Derivative of a sum"

(define (deriv-sum wrt expr)
  (list `+ (derivative wrt (second expr)) (derivative wrt (third expr))))

;; Test cases:
(derivative 'x '(+ 1 2)) ;; Should be (+ 0 0)
(derivative 'x '(+ x 2)) ;; Should be (+ 1 0)
(derivative 'x '(+ x x)) ;; Should be (+ 1 1)


"Problem 8: Derivative of a product"

(define (deriv-product wrt expr)
  (list `+
        (list `* (second expr) (derivative wrt (third expr)))
        (list `* (derivative wrt (second expr)) (third expr))))


"Problem 9: Additional testing"

; Additional test cases for 'derivative' go here.
