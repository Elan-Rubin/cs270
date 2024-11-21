#lang racket


(define (countOp exp)
     (cond
       [(not (list? exp)) 0]
       [(null? exp) 0]
       [else (+ 1 
                (countOp (first (rest exp)))
             (countOp (first (rest (rest exp)))))]))

(countOp 'x) ; Returns 0
(countOp 7) ; Returns 0
(countOp '(+ x 7)) ; Returns 1
(countOp '(- x y)) ; Returns 1
(countOp '(+ (* 2 x) (- y 2))) ; Returns 3
(countOp '(+ x (- y (* 2 (+ z x))))) ; Returns 4
 

; '(root left right)


(define (countTerms exp)
  (if (list? exp)
      (+ (countTerms (first (rest exp))) (countTerms (first (rest (rest exp)))))
      1))

(countTerms 'x) ; Returns 1
(countTerms 7) ; Returns 1
(countTerms '(+ x 7)) ; Returns 2
(countTerms '(- x y)) ; Returns 2
(countTerms '(+ (* 2 x) (- y 2))) ; Returns 4
(countTerms '(+ x (- y (* 2 (+ z x))))) ; Returns 5


(define (setVar exp var val)
 (if (list? exp)
     (cons (first exp) (cons (setVar (first (rest exp)) var val) (cons (setVar (first (rest (rest exp))) var val) null)))
     (if (equal? exp var)
         val 
         exp)))

(setVar 'x 'x 2) ; Returns 2
(setVar 7 'x 2) ; Returns 7
(setVar '(+ 9 7) 'x 9) ; Returns '(+ 9 7)
(setVar '(- x y) 'y 3) ; Returns '(- x 3)
(setVar '(+ (* 2 x) (- y 2)) 'y 9) ; Returns '(+ (* 2 x) (- 9 2))



