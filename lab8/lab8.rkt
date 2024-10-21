#lang racket

(define (countdown n)
  (if (zero? n) null (cons n (countdown (- n 1)))))

(define (countup n [L null])
  (if (zero? n) L (countup (- n 1) (cons n L))))

;not the best way to do this in racket
(define (f x y)
  (if (< x 5) (+ x y) (* x y)))

;a better, more functional way
(define (f2 x y)
  ((if (< x 5) + *) x y))