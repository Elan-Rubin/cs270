#lang racket

;(Question 1)
(define (F a b)
  (if (= b 0)
      a
      (+ 1 (F a (- b 1)))))

;(F 5 1) premise
;(if (= 1 0) 5 (+ 1 (F 5 (- 1 1)))) expand definition
;(if #f 5 (+ 1 (F 5 (- 1 1)))) evaluate condition
;(+ 1 (F 5 (- 1 1))) take false branch
;(+ 1 (F 5 0)) evaluate subtraction
;(+ 1 (if (= 0 0) 5 (+ 1 (F 5 (- 0 1))))) expand f
;(+ 1 (if #t 5 (+ 1 (F 5 (- 0 1))))) evaluate condition
;(+ 1 5) take true branch
;6 final result

;(Question 2)
(define (H a b)
  (if (< a b)
      0
      (+ 1 (H (- a b) b))))

;(H 3 2) premise
;(if (< 3 2) 0 (+ 1 (H (- 3 2) 2))) expand definition
;(if #f 0 (+ 1 (H (- 3 2) 2))) evaluate condition
;(+ 1 (H (- 3 2) 2)) take false branch
;(+ 1 (H 1 2)) evaluate subtraction
;(+ 1 (if (< 1 2) 0 (+ 1 (H (- 1 2) 2)))) expand h
;(+ 1 (if #t 0 (+ 1 (H (- 1 2) 2)))) evaluate condition
;(+ 1 0) take true branch
;1 final result

;(Question 3)
(define (Q a b)
  (if (= b 1)
      a
      (+ a (Q a (- b 1)))))

;(Q 3 2) premise
;(if (= 2 1) 3 (+ 3 (Q 3 (- 2 1)))) expand definition
;(if #f 3 (+ 3 (Q 3 (- 2 1)))) evaluate condition
;(+ 3 (Q 3 (- 2 1))) take false branch
;(+ 3 (Q 3 1)) evaluate subtraction
;(+ 3 (if (= 1 1) 3 (+ 3 (Q 3 (- 1 1))))) expand q
;(+ 3 (if #t 3 (+ 3 (Q 3 (- 1 1))))) evaluate condition
;(+ 3 3) take true branch
;6 final result

;(Question 4)
(define (M x L)
  (if (= x 0)
      (first L)
      (M (- x 1) (rest L))))

;(M 1 '(1 2 3)) premise
;(if (= 1 0) (first '(1 2 3)) (M (- 1 1) (rest '(1 2 3)))) expand definition
;(if #f (first '(1 2 3)) (M (- 1 1) (rest '(1 2 3)))) evaluate condition
;(M (- 1 1) (rest '(1 2 3))) take false branch
;(M 0 '(2 3)) evaluate subtraction and rest
;(if (= 0 0) (first '(2 3)) (M (- 0 1) (rest '(2 3)))) expand m
;(if #t (first '(2 3)) (M (- 0 1) (rest '(2 3)))) evaluate condition
;(first '(2 3)) take true branch
;2 final result