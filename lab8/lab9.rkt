#lang racket

;question 1
(define (star f n b)
  (if (< n b)
      0
      ;
      (+ 1 (star f (f n) b))
   )
)

;question 2
;a)
(map (lambda (x) (* x 3)) '(1 2 3 4))
;b)
(map (lambda (x) (- 0 x)) '(1 2 3 4))
;c)
(map (lambda (x) (- 10 x)) '(1 2 3 4))

;question 3
;a)
(foldr * 1 '(1 2 3 4))
;b)
(foldr / 1 '(1 2 3 4))
;c)
(foldl / 1 '(1 2 3 4))

;question 4
;a)
(define (neg? x)
  (if (< x 0)
  1
  0
  ))
;b)

(map (lambda (x) (neg? x)) '(1 2 3 4))

(map (lambda (x) (neg? x)) '(1 -3 -4 5 9))
;simplified -->
(map neg? '(1 -3 -4 5 9))
;c)
(foldr + 0 (map (lambda (x) (if (< x 0) 1 0)) '(1 -3 -4 5 9)))

