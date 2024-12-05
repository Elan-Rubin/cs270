#lang racket

;
;left: (first (rest T))

;right: (first (rest (rest T)))

(define (left T) (first (rest T)))
(define (right T) (first (rest (rest T))))
(define (value T) (first T))
(define isEmpty? null?)

(define (countVals T)
  (if (isEmpty? T) 
      0
      (+ 1
         (countVals (left T))     ; left
         (countVals (right T))))) ; right

(define test1 '(5 (3 (1 () (2 () ())) ()) (7 () (9 () ()))))
(define test2 '(6 (3 (1 () ()) (4 () ())) (9 () (10 () (12 () ())))))
(countVals test1) ; Returns 6
(countVals test2) ; Returns 7

(define (search v T)
  (if (isEmpty? T)
      #f
      ;check current, search left, search right
      (or (= v (value T))
          (search v(right T))
          (search v (left T)))))

(define (binSearch v T)
  (if (isEmpty? T)
      #f
      (cond 
        [(= v (value T)) #t]
        [(< v (value T)) (binSearch v (left T))]
        [else (binSearch v (right T))])))  

(define test3 '(5 (3 (1 () (2 () ())) ()) (7 () (9 () ()))))
(define test4 '(6 (3 (1 () ()) (4 () ())) (9 () (10 () (12 () ())))))
    
(search 1 test3) ; #t
(search 7 test3) ; #t
(search 11 test3) ; #f
(search 6 test4) ; #t
(search 12 test4) ; #t
(search 2 test4) ; #f

    
(binSearch 1 test3) ; #t
(binSearch 7 test3) ; #t
(binSearch 11 test3) ; #f
(binSearch 6 test4) ; #t
(binSearch 12 test4) ; #t
(binSearch 2 test4) ; #f


#|

LHS:
(integer? (countVals K)) ; Premise LHS
(integer? (if (null? K) 0 (+ 1 (countVals (first (rest K))) (countVals (first (rest (rest K))))))) ; application of definition of countVals
(integer? (if #f 0 (+ 1 (countVals (first (rest K))) (countVals (first (rest (rest K))))))) ; evaluate null?
(integer? (+ 1 (countVals (first (rest K))) (countVals (first (rest (rest K)))))) ; evaluate if
(and (integer? 1) (integer? (countVals (first (rest K)))) (integer? (countVals (first (rest (rest K))))))) ; lemma 1
(and #t (integer? (countVals (first (rest K)))) (integer? (countVals (first (rest (rest K))))))) ; evaluate integer?
(and #t #t (integer? (countVals (first (rest (rest K))))))) ; invoke IH
(and #t #t #t) ; invoke IH
#t ; evaluate and and

RHS:
#t ; Premise RHS
because LHS = RHS, this completes the leap step

Since both the leap case and the base case have been established, this demonstrates by PORI that
∀x ∈ B ((integer? (countVals x) = #t)
|#
