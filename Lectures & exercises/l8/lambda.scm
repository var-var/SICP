
#|

This file includes all of the definitions I gave in class, as well as
'if' and boolean logic. For additional masochism (or "purity", if you
like), I have not used any procedures of multiple definitions, instead
using "currying" to implement multiple arguments by writing functions
that return functions that accept the further arguments.

Loading this file will seriously screw with your scheme, as it
replaces many of the language's primitives with ones that have been
lambda-ified.

To run in Racket, use the Language menu, Choose Language... and pick
R5RS and then uncheck the box that says "disallow redefition of initial 
bindings.

|#

(define cons
  (lambda (a)
    (lambda (b)
      (lambda (c)
        ((c a) b)))))

(define car
  (lambda (p)
    (p (lambda (a)
         (lambda (b) a)))))

(define cdr
  (lambda (p)
    (p (lambda (a)
         (lambda (b) b)))))

(define true
  (lambda (a)
    (lambda (b)
      a)))

(define false
  (lambda (a)
    (lambda (b)
      b)))

; Because our scheme is strict, we need to pass closures to (if),
; rather than values, to prevent both operands from being
; evaluated. That's why there's an extra pair of parentheses, in order
; to force the result.
(define church-if
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (((a b) c))))))

(define and
  (lambda (a)
    (lambda (b)
      ((a b) false))))

(define or
  (lambda (a)
    (lambda (b)
      ((a true) b))))

(define not
  (lambda (a)
    ((a false) true)))

(define church-0
  (lambda (f)
    (lambda (x)
      x)))

(define church-1
  (lambda (f)
    (lambda (x)
      (f x))))

(define church-2
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define church-inc
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define church-add
  (lambda (a)
    (lambda (b)
      ((a church-inc) b))))

(define church-mul
  (lambda (a)
    (lambda (b)
      ((a (b church-inc)) church-0))))

(define church-expt
  (lambda (a)
    (lambda (b)
      (((b a) church-inc) church-0))))

(define church-zero?
  (lambda (x)
    ((x (lambda (p) false)) true)))

(define church-dec
  (lambda (n)
    (car ((n (lambda (p)
               ((cons (cdr p))
                (church-inc (cdr p)))))
          ((cons church-0) church-0)))))

(define church-sub
  (lambda (a)
    (lambda (b)
      ((b church-dec) a))))

(define church<=
  (lambda (a)
    (lambda (b)
      (church-zero? ((church-sub a) b)))))

(define church>=
  (lambda (a)
    (lambda (b)
      ((church<= b) a))))

(define church=
  (lambda (a)
    (lambda (b)
      ((and ((church>= a) b))
       ((church<= a) b)))))

(define scm-- -)
(define scm-+ +)
(define scm-zero? zero?)

(define + church-add)
(define * church-mul)
(define expt church-expt)
(define zero? church-zero?)
(define - church-sub)
(define <= church<=)
(define >= church>=)
(define = church=)

;; These procedures allow us to convert between scheme numbers and
;; church numerals, so that we can translate numbers that we type, and
;; translate numbers back for display.

(define church->scm
  (lambda (n)
    ((n (lambda (x) (scm-+ x 1))) 0)))

(define (scm->church n)
  (cond ((scm-zero? n) church-0)
        (else (church-inc (scm->church (scm-- n 1))))))

#|

;; This is the Y combinator I showed in class

;; It will diverge (loop forever) in a normal scheme, although it
;; would fine in a lazy evaluator. The one below works properly in
;; scheme.

(define Y
  (lambda (f)
    ((lambda (g) (f (g g)))
     (lambda (g) (f (g g))))))

|#

(define Y
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define fact
  (Y (lambda (f)
       (lambda (n)
         (((church-if (zero? n))
           (lambda () church-1))
          (lambda () ((* n) (f (church-dec n)))))))))

;; Some examples

(begin
  (display (church->scm ((* (scm->church 4)) (scm->church 3))))
  (newline)

  (display (church->scm (fact (scm->church 6))))
  (newline))

#|

;; Here's the derivation I gave in class for why (Y f) = (f (Y f))

let Y = (lambda (f)
          ((lambda (g) (f (g g)))
           (lambda (g) (f (g g)))))
let H = (lambda (g) (f (g g))) for convenience

let fact = (Y fact-h)

   fact
=> (Y fact-h)
=> (H H)      ; with f = fact-h
=> (fact-h (g g))
=> (fact-h (H H))
=  (fact-h (Y fact-h))
=  (fact-h fact)

;; => indicates a substitution step, while = indicates an equivalence
;; which is not actually a substitution

|#

