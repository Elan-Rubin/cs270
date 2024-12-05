#lang racket

; input-contract: num is an integer x in pnum
; output-contract: (succ num) is the pnum for x + 1
(define (succ num)
  (cons 's num))

; input-contract: num is an integer x in pnum
; output-contract: (pred num) is the pnum for x - 1
(define (pred num)
  (if (null? num)
      null
      (rest num)))

(pred (succ '(s s))) ; premise
(pred (cons 's '(s s))) ; apply definition of succ
(pred '(s s s)) ;evaluate cons
(if(null? '(s s s)) null (rest '(s s s))) ;apply definition of pred
(if #f null (rest '(s s s)))    ;evaluate null
(rest '(s s s)) ;evaluate if 
'(s s) ;evaluate rest

#|  |#

; input-contract: A and B are the pnum for the integers a and b.
; output-contract: (subtract A B) is the pnum for a - b

#| (define (subtract A B)
     (cond
       [(null? A) '()]            
       [(null? B) A]        
    [else (subtract (rest A) (rest B))])) |#

(define (subtract A B)
  (cond
    [(null? A) '()]         
    [(null? B) A]          
    [else (subtract (pred A) (pred B))]))

;keep subtracting from both
;if A empty return ()
;if B empty return A

(subtract '(s) '())         ; Returns '(s)
(subtract '(s) '(s))        ; Returns '()
(subtract '(s s s) '(s s))  ; Returns '(s)
(subtract '(s s) '(s s s))  ; Returns '()

#|  |#

#| 
   ; input-contract: a non-negative integer n
   ; output-contract: (toPeano n) is the pnum of the input integer n
   (define (toPeano n)
     (if (= n 0)
         '()
         (succ (toPeano (- n 1)
     ))))
   
   (toPeano 1) ; Returns '(s)
   (toPeano 2) ; Returns '(s s)
   (toPeano 5) ; Returns '(s s s s s)
   (toPeano 7) ; Returns '(s s s s s s s)
   
   #|  |#
   
   Base:
   LHS:
   (length (toPeano 0)) premise
   (length (if (= 0 0) '() (cons 's (toPeano (- 0 1))))) apply def to toPeano
   (length '()) eval if
   0 by lemma 1
   RHS:
   0 Premise
   leap:
   IH = (length (toPeano x)) = x
   LHS:
   (length (toPeano (+ k 1))) premise
   (length (if (= (+ k 1) 0) '() (cons 's (toPeano (- (+ k 1) 1))))) apply def of toPeano
   (length (if #f '() (cons 's (toPeano (- (+ k 1) 1))))) eval =
   (length (cons 's (toPeano (- (+ k 1) 1)))) eval if
   (length (cons 's (toPeano k))) eval -
   (+ 1 (length (toPeano k))) by lemma 2
   (+ 1 k) by IH
   k + 1 eval +
   RHS:
k + 1 premise |#








