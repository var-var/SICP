#lang racket

(define-syntax cons-stream 
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))
(define stream-car car)
(define stream-cdr (lambda (s) (force (cdr s))))

(define (first-n s n)
  (if (= n 0)
      '()
      (cons (stream-car s)
            (first-n (stream-cdr s) (- n 1)))))
(define (head s) (first-n s 10))

(define zeros (cons-stream 0 zeros))

(define (add-streams* s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
               (add-streams* (stream-cdr s1) (stream-cdr s2))))

(define ones (cons-stream 1 ones))
(define ints (cons-stream 1 (add-streams ones ints)))


(define (map2-stream op s1 s2)
  (cons-stream (op (stream-car s1) (stream-car s2))
               (map2-stream op (stream-cdr s1) (stream-cdr s2))))

(define (add-streams s1 s2) (map2-stream + s1 s2))
(define (mul-streams s1 s2) (map2-stream * s1 s2))
(define (div-streams s1 s2) (map2-stream / s1 s2))

(define (scale-stream x s)
  (cons-stream (* x (stream-car s))
               (scale-stream x (stream-cdr s))))

(define facts (cons-stream 1 (mul-streams ints facts)))

(define (powers x)
  (cons-stream 1 (scale-stream x (powers x))))

(define (sum-series s x n)
  (define (sum-helper s sum n)
    (if (= n 0)
        sum
        (sum-helper (stream-cdr s) (+ sum (stream-car s)) (- n 1))))
  (sum-helper (mul-streams s (powers x)) 0 n))

(define two-x-plus-five (cons-stream 5 (cons-stream 2 zeros)))

(define e-to-the-x (div-streams ones facts))

(define (integrate-series s)
  (div-streams s ints))

;(define e-to-the-x (cons-stream 1 (integrate-series e-to-the-x)))

(define sine (cons-stream 0 (integrate-series cosine)))
(define cosine (cons-stream 1 (scale-stream -1 (integrate-series sine))))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
     (add-streams (scale-stream (stream-car s1)
                                (stream-cdr s2))
                  (mul-series (stream-cdr s1) s2))))

(define mystery (add-streams (mul-series sine sine)
                             (mul-series cosine cosine)))

(require plot)
(plot-new-window? #t)
(define (plot-stream stream n min max)
  (define (func x)
    (sum-series stream x n))
  (plot (function func min max)))


; Try these out.. (may take several seconds to plot)
; (plot-stream sine 50 0 (* 4 pi))
; (plot-stream cosine 50 0 (* 4 pi))
; (plot-stream mystery 50 0 (* 4 pi))
