#lang racket

(define (seq n)
  (if (< n 0)
      null
  (cons n (seq(- n 1)))))

(define (mult5 n k)
  (if (equal? n 0)
      null
  (cons k (mult5 (- n 1) (+ k 5)))))

(define (largest L)
  (cond
    [(null? L) 0]  
    [(null? (rest L)) (first L)]  
    [else
     (if (> (first L) (largest (rest L)))
         (first L)
         (largest (rest L)))])) 