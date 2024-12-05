#lang racket


#| (define (zip L M)
     (if null? L)
     M
     (if null? M)
     L
     cons(first L)
     cons(first M)
  (zip (rest L) (rest M))) |#

(define (zip L M)
  (if (null? L)
      M
      (if (null? M)
          L
          (cons (first L) 
                (cons (first M) 
                      (zip (rest L) (rest M)))))))


(define (snoc x L)
  (if (null? L)
      (cons x null)
      (cons (first L) (snoc x (rest L)))))



