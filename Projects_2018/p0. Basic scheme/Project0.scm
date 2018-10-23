#lang racket

"Problem 1"
(define (bitfunc x)
  (+ (- (expt x 4) (* 5 x x)) 4))

;; Test cases:
(bitfunc 0)  ;; Should be 4
(bitfunc 1)  ;; Should be 0, or very close
(bitfunc 2)  ;; Should also be very close to 0
(bitfunc -1) ;; Should also also be very close to 0
(bitfunc 10) ;; Should be pretty big, and positive


"Problem 2"
(define (bitfunc-rect x1 x2)
  (* (- x2 x1) (bitfunc x1)))

;; Test cases:
(bitfunc-rect 0 1)   ;; Should be 4
(bitfunc-rect 0 0.5) ;; Should be 2
(bitfunc-rect 1.5 2) ;; Should be negative

"Problem 3"
(define (bitfunc-integral-recur num-steps x1 x2)      ;recursive algorithm
  (cond((equal? num-steps 1) (bitfunc-rect x1 x2))
       (else
        (+ (bitfunc-rect (/ (- x2 x1) num-steps) x2)
           (bitfunc-integral-recur (- num-steps 1) x1 x2)))))

(define (bitfunc-integral-iter num-steps x1 x2)       ;iterative algorithm:
  (define (bitfunc-integral-helper num-steps answer)
    (cond ((equal? num-steps 1) answer)
          (else
           (bitfunc-integral-helper (- num-steps 1)
                                    (+ (bitfunc-rect (/ (- x2 x1) num-steps) x2) answer)))))
  (bitfunc-integral-helper num-steps (bitfunc-rect x1 x2)))


;; Test cases:
(bitfunc-integral-recur 1 0 1)   ;; Should be 4
(bitfunc-integral-recur 1 0 0.5) ;; Should be 2
(bitfunc-integral-recur 1 1.5 2) ;; Should be negative
(bitfunc-integral-recur 1 10 10) ;; Should be 0

(bitfunc-integral-iter 1 0 1)   ;; Should be 4
(bitfunc-integral-iter 1 0 0.5) ;; Should be 2
(bitfunc-integral-iter 1 1.5 2) ;; Should be negative
(bitfunc-integral-iter 1 10 10) ;; Should be 0


"Problem 4"
(define (bitfunc-integral-difference num-steps x1 x2)
  (abs (- (bitfunc-integral-recur num-steps x1 x2)
          (bitfunc-integral-iter num-steps x1 x2))))

;; Test cases:
(bitfunc-integral-difference 10 10 10) ;; Should be 0
(bitfunc-integral-difference 1 5 10)   ;; Should be 0
(bitfunc-integral-difference 10 5 10)  ;; Should be 0
          
            
           
