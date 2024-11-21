#lang racket
(require rackunit)
(require rackunit/text-ui)

;CS 270
;Homework 7
;Professor B. Char, M. Boady,  J. Johnson, and G. Long

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;Each of the below questions has two parts.
;First, you will be asked to write a Racket function to solve a problem.
;Secondly, you will be asked to prove by induction that your
;Racket code has some property.


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 1a (10 points)
;Write a recursive function to compute
;the sum( 6*x^2 , x = 1..n) for a given n
;You must write a recursive function.
;If you use any iterative commands (for/loop/sum/etc you will receive a 0)

; Computes sum( 6*x^2 , x = 1..n)
; Input:  n an integer >= 1
; Output: an integer, the result of the summation
;Question 1
(define (spec_sum n)
  (cond 
    [(= n 1) 6]
    [else (+ (* 6 (* n n)) (spec_sum (- n 1)))]))
;end

;Test Bed
(display "Question 1a spec_sum Tests (5 points)\n")
(define-test-suite test_spec_sum
  (check-equal? (spec_sum 1) 6)
  (check-equal? (spec_sum 2) 30)
  (check-equal? (spec_sum 3) 84)
  (check-equal? (spec_sum 4) 180)
  (check-equal? (spec_sum 5) 330)
  (check-equal? (spec_sum 6) 546)
  (check-equal? (spec_sum 7) 840)
  (check-equal? (spec_sum 8) 1224)
  (check-equal? (spec_sum 9) 1710)
  (check-equal? (spec_sum 10) 2310)
)
(define q1a_score (- 10 (run-tests test_spec_sum 'verbose)))

;Question 1b (10 points)
;Prove by induction that
;for all integers n >= 1 -> (spec_sum n) = 2n^3+3n^2+n

;Give your proof below in comments

#|
  anchor case: n = 1
  base case: (spec_sum 1) = (6*1^2 + 6*0^2)

  proof of base case:
  LHS:
  (spec_sum 1) ;premise of LHS
  (if (= 1 0) 0 (+ (* 6 (* 1 1)) (spec_sum (- 1 1)))) ;application of definition of spec_sum
  (if #f 6 (+ 6 (spec_sum 0))) ;application of definition of equals
  (+ 6 (spec_sum 0)) ;application of definition of if
  (+ 6 (if (= 0 0) 0 (+ (* 6 (* 0 0)) (spec_sum (- 0 1))))) ;application of definition of spec_sum
  (+ 6 (if #t 0 (+ 0 (spec_sum (- 0 1))))) ;application of definition of equals
  (+ 6 0) ;application of definition of if
  6 ;evaluate plus

  RHS:
  6*1^2 + 6*0^2 ;premise of RHS
  6 ;evaluate algebra

  inductive hypothesis: (spec_sum k) = 2k^3 + 3k^2 + k
  leap step: (spec_sum (+ k 1)) = 2(k+1)^3 + 3(+ k 1)^2 + (+ k 1)

  proof of leap step:
  LHS:
  (spec_sum (+ k 1)) ;premise of LHS
  (if (= (+ k 1) 0) 0 (+ (* 6 (* (+ k 1) (+ k 1))) (spec_sum (- (+ k 1) 1)))) ;application of definition of spec_sum
  (if #f 0 (+ (* 6 (* (+ k 1 ) (+ k 1)) (spec_sum (- (+ k 1) 1))))) ;evaluate equals
  (+ (* 6 (* (+ k 1) (+ k 1))) (spec_sum (- (+ k 1) 1))) ;evaluate if
  (+ (* 6 (* (+ k 1) (+ k 1))) (spec_sum k)) ;evaluate plus and minus
  6(k+1)^2 + 2k^3 + 3k^2 + k ;invoke IH
  2k^3 + 9k^2 + 13k + 6 ;evaluate algebra

  RHS:
  2(k+1)^3 + 3(k+1)^2 + (k+1) ;premise of RHS
  2k^3 + 9k^2 + 13k + 6 ;evaluation of algebra  

  LHS = RHS
  Since the LHS = RHS, this completes the leap step.
  PORI -> (spec_sum n) = 2n^3 + 3n^2 + n for all n >= 1.
|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 2 (10 points)
; Write a recursive function evenzeros to check if a list of integers
; contains an even number of zeros.

; Input:  L is a list of integers.
; Output: a boolean value which is true when an even number of the elements
;          in L is equal to zero and false otherwise.
; Requirements: if you are actually counting the number of zeroes, you are
; implementing this in a bad way.  There should be NO arithmetic involved.

;Question 2
(define (evenzeros X)
  (cond
    [(empty? X) #t]
    [(= (first X) 0) (not (evenzeros (rest X)))]
    [else (evenzeros (rest X))]))
;end
;Test Bed
(display "Question 2a evenzeros Tests (10 points)\n")
(define-test-suite test_even_zeros
  (check-equal? (evenzeros '()) #t)
  (check-equal? (evenzeros '(1)) #t)
  (check-equal? (evenzeros '(0)) #f)
  (check-equal? (evenzeros '(0 0)) #t)
  (check-equal? (evenzeros '(7 0)) #f)
  (check-equal? (evenzeros '(1 -2)) #t)
  (check-equal? (evenzeros '(0 0 1)) #t)
  (check-equal? (evenzeros '(4 0 1)) #f)
  (check-equal? (evenzeros '(1 0 8)) #f)
  (check-equal? (evenzeros '(0 11 0 -9)) #t)
)
(define q2a_score (- 10 (run-tests test_even_zeros 'verbose)))
;Question 2b (10 points)
;Prove by induction, algebra, and equational reasoning that
;If L contains an even number of zeros then (equal? (evenzeros L) #t)
;If L contains an odd number of zeros then (equal? (evenzeros L) #f)
;Hint:
;Assume two different list E with an even number and O with an odd number
;You need 4 cases (cons 0 E), (cons x E), (cons 0 O), (cons x O)
;Where, x!=0, E is a list with an even number of zeros and O is a list
;with an odd number of zeros.

#|
  anchor case: x = '()
  base case: (evenzeros '()) = #t

  proof of base case:
  LHS:
  (evenzeros '()) ;premise of LHS
  (if (null? '()) #t (if (= (first '()) 0) (not (evenzeros (rest '())) (evenzeros (rest '()))))) ;apply definition of evenzeros
  (if #t #t (if (= (first '()) 0) (not (evenzeros (rest '())) (evenzeros (rest '()))))) ;eval null?
  #t ;eval if

  RHS:
  #t ;premise of RHS

  inductive hypothesis: (evenzeros E) = #t and (evenzeros O) = #f
  leap steps: (evenzeros (cons 0 E)) = #f, (evenzeros (cons x E)) = #t, (evenzeros (cons 0 O)) = #t, (evenzeros (cons x O)) = #f

  proof for (evenzeros (cons 0 E)) = #f
  LHS:
  (evenzeros (cons 0 E)) ;premise of LHS
  (if (null? (cons 0 E)) #t (if (= (first (cons 0 E)) 0) (not (evenzeros (rest (cons 0 E))) (evenzeros (rest (cons 0 E)))))) ;apply definition of evenzeros
  (if #f #t (if (= (first (cons 0 E)) 0) (not (evenzeros (rest (cons 0 E))) (evenzeros (rest (cons 0 E)))))) ;apply definition of null? [must be #f due to IH]
  (if (= (first (cons 0 E)) 0) (not (evenzeros (rest (cons 0 E))) (evenzeros (rest (cons 0 E))))) ;eval if
  (if (= 0 0) (not (evenzeros (rest (cons 0 E))) (evenzeros (rest (cons 0 E))))) ;eval first-cons elim
  (if #t (not (evenzeros (rest (cons 0 E))) (evenzeros (rest (cons 0 E))))) ;eval =
  (not (evenzeros (rest (cons 0 E)))) ;eval if
  (not (evenzeros E)) ;eval rest-cons elim
  not #t ;invoke IH
  #f ;eval not

  RHS:
  #f ;premise of RHS

  proof for (evenzeros (cons x E)) = #t
  LHS:
  (evenzeros (cons x E)) ;premise of LHS
  (if (null? (cons x E)) #t (if (= (first (cons x E)) 0) (not (evenzeros (rest (cons x E))) (evenzeros (rest (cons x E)))))) ;apply definition of evenzeros
  (if #f #t (if (= (first (cons x E)) 0) (not (evenzeros (rest (cons x E))) (evenzeros (rest (cons x E)))))) ;apply definition of null? [must be #f due to IH]
  (if (= (first (cons x E)) 0) (not (evenzeros (rest (cons x E))) (evenzeros (rest (cons x E))))) ;eval if
  (if (= x 0) (not (evenzeros (rest (cons x E))) (evenzeros (rest (cons x E))))) ;eval first-cons elim
  (if #f (not (evenzeros (rest (cons x E))) (evenzeros (rest (cons x E))))) ;eval = [must be false due to x != 0]
  (evenzeros (rest (cons x E))) ;eval if
  (evenzeros E) ;eval rest-cons elim
  #t ;invoke IH

  RHS:
  #t ;premise of RHS

  proof for (evenzeros (cons 0 O)) = #t
  LHS:
  (evenzeros (cons 0 O)) ;premise of LHS
  (if (null? (cons 0 O)) #t (if (= (first (cons 0 O)) 0) (not (evenzeros (rest (cons 0 O))) (evenzeros (rest (cons 0 O)))))) ;apply definition of evenzeros
  (if #f #t (if (= (first (cons 0 O)) 0) (not (evenzeros (rest (cons 0 O))) (evenzeros (rest (cons 0 O)))))) ;apply definition of null? [must be #f due to IH]
  (if (= (first (cons 0 O)) 0) (not (evenzeros (rest (cons 0 O))) (evenzeros (rest (cons 0 O))))) ;eval if
  (if (= 0 0) (not (evenzeros (rest (cons 0 O))) (evenzeros (rest (cons 0 O))))) ;eval first-cons elim
  (if #t (not (evenzeros (rest (cons 0 O))) (evenzeros (rest (cons 0 O))))) ;eval =
  (not (evenzeros (rest (cons 0 O)))) ;eval if
  (not (evenzeros O)) ;eval rest-cons elim
  not #f ;invoke IH
  #t ;eval not

  RHS:
  #t ;premise of RHS

  proof for (evenzeros (cons x O)) = #f
  LHS:
  (evenzeros (cons x O)) ;premise of LHS
  (if (null? (cons x O)) #t (if (= (first (cons x O)) 0) (not (evenzeros (rest (cons x O))) (evenzeros (rest (cons x O)))))) ;apply definition of evenzeros
  (if #f #t (if (= (first (cons x O)) 0) (not (evenzeros (rest (cons x O))) (evenzeros (rest (cons x O)))))) ;apply definition of null? [must be #f due to IH]
  (if (= (first (cons x O)) 0) (not (evenzeros (rest (cons x O))) (evenzeros (rest (cons x O))))) ;eval if
  (if (= x 0) (not (evenzeros (rest (cons x O))) (evenzeros (rest (cons x O))))) ;eval first-cons elim
  (if #f (not (evenzeros (rest (cons x O))) (evenzeros (rest (cons x O))))) ;eval = [must be false due to x != 0]
  (evenzeros (rest (cons x O))) ;eval if
  (evenzeros O) ;eval rest-cons elim
  #f ;invoke IH

  RHS:
  #f ;premise of RHS

  Since the LHS = RHS for all the respective leap cases, this completes the leap step.
  Since both the leap cases and the base case has been proven, this demonstrates by PORI that (evenzeros E) = #t for the list E with an even number of 0s 
  and (evenzeros O) = #f for the list O with an odd number of 0s.
|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Q3a (10 Points)
;Write a recursive function duplicate that takes every element in a list
;and makes a second copy of the item.
;For example if we started with (1 2 3)
;then the duplicated list would be (1 1 2 2 3 3)


; Duplicates Elements in a list
; Input:  X a list
; Output: A new list with two copies of every value in X
;Question 3
(define (duplicate X)
  (cond
    [(empty? X) '()]
    [else (cons (first X) 
                (cons (first X) 
                      (duplicate (rest X))))]))
;end
(display "Question 3a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '()) '())
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5))
                '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5))
)
(define q3a_score (- 10 (run-tests test_duplicate 'verbose)))

;Q3b (10 Points)
;Prove By Induction
;(length (duplicate L)) = 2x where x is the length of list L
;You may use the following properties of length
;Length Property 1: (length null) = 0 
;Length Property 2: If a is any object and B is any list
;(length (cons a B)) = (+ 1 (length B))
;You may Justify lines by saying [By Length Property 1]
;Hint: An equals can be used both ways.

#|
  anchor case: L = '()
  base case: (length (duplicate '())) = 0

  proof of base case:
  LHS:
  (length (duplicate '())) ;premise of LHS
  (length (if (null? '()) '() (cons (first '()) (cons (first '()) (duplicate (rest '())))))) ;apply definition of duplicate
  (length (if #t '() (cons (first '()) (cons (first '()) (duplicate (rest '())))))) ;apply definition of null?
  (length '()) ;eval if
  0 ;length lemma 1

  RHS:
  0 ;premise of RHS

  inductive hypothesis: (length (duplicate L)) = 2k
  leap step: (length (duplicate (cons a L))) = 2(k + 1)

  proof of leap step:
  LHS:
  (length (duplicate (cons a L))) ;premise of LHS
  (length (if (null? (cons a L)) '() (cons (first (cons a L)) (cons (first (cons a L)) (duplicate (rest (cons a L))))))) ;apply definition of duplicate
  (length (if #f '() (cons (first (cons a L)) (cons (first (cons a L)) (duplicate (rest (cons a L))))))) ;apply definition of null? [must be #f because a is not null by IH and input contract]
  (length (cons (first (cons a L)) (cons (first (cons a L)) (duplicate (rest (cons a L)))))) ;eval if
  (length (cons a (cons a (duplicate (rest (cons a L)))))) ;eval first-cons elim
  (length (cons a (cons a (duplicate L)))) ;eval rest-cons elim 
  (+ 1 (length (cons a (duplicate L)))) ;length lemma 2
  (+ 1 (+ 1 (length (duplicate L)))) ;length lemma 2
  (+ 1 (+ 1 2k)) ;invoke IH
  2k + 2 ;algebra

  RHS:
  2(k + 1) ;premise of RHS
  2k + 2 ;algebra

  Since the LHS = RHS, this completes the leap step.
  Since both the leap case and the base case has been proven, this demonstrates by PORI that for all lists L we have (length (duplicate L)) = 2x.
|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 4a (10pts)
;Write a recursive function (cut_end L) that removes the last element from the list

; Removes the last element in a list
; Input:  X non-empty a list
; Output: A new list with the last element removed
;Question 4
(define (cut_end L)
  (cond
    [(empty? (rest L)) '()]
    [else (cons (first L) 
                (cut_end (rest L)))]))
;end

(display "Question 4a cut_end Tests (10 points)\n")
(define-test-suite test_cut_end
  (check-equal? (cut_end '(1)) '())
  (check-equal? (cut_end '(1 2)) '(1))
  (check-equal? (cut_end '(3 4 5)) '(3 4))
  (check-equal? (cut_end '( (1) (2) (3) )) '( (1) (2) ))
  (check-equal? (cut_end '((1 2 3 4))) '())
  (check-equal? (cut_end '((1 2) (3 4))) '((1 2)))
  (check-equal? (cut_end '(9 9 8)) '(9 9))
  (check-equal? (cut_end '(AND A B)) '(AND A))
  (check-equal? (cut_end '(NOT X)) '(NOT))
)
(define q4a_score (- 10 (run-tests test_cut_end 'verbose)))

;Question 4b
;Prove by Induction that (length (cut_end L)) = x-1, where x is the output of (length L)
;for all L where (length L) >= 1
;You may use the properties of length from Question 3

#|
  anchor case: L = '(a)
  base case: (length (cut_end '(a))) = 0

  proof of base case:
  LHS:
  (length (cut_end '(a))) ;premise of LHS
  (length (if (null? (rest '(a))) '() (cons (first '(a)) (cut_end (rest '(a)))))) ;apply definition of cut_end
  (length (if (null? '()) '() (cons (first '(a)) (cut_end (rest '(a)))))) ;eval rest
  (length (if #t '() (cons (first '(a)) (cut_end (rest '(a)))))) ;eval null?
  (length '()) ;eval if
  0 ;length lemma 1

  RHS:
  0 ;premise of RHS

  inductive hypothesis: (length (cut_end L)) = k - 1
  leap step: (length (cut_end (cons a L))) = (k + 1) - 1

  proof of leap step:
  LHS:
  (length (cut_end (cons a L))) ;premise of LHS
  (length (if (null? (rest (cons a L))) '() (cons (first (cons a L)) (cut_end (rest (cons a L)))))) ;apply definition of cut_end
  (length (if (null? L) '() (cons (first (cons a L)) (cut_end (rest (cons a L)))))) ;eval rest-cons elim
  (length (if #f '() (cons (first (cons a L)) (cut_end (rest (cons a L)))))) ;eval null? [must be false because L is not null by IH and input contract]
  (length (cons (first (cons a L)) (cut_end (rest (cons a L))))) ;eval if
  (length (cons a (cut_end (rest (cons a L))))) ;eval first-cons elim
  (+ 1 (length (cut_end L))) ;eval rest-cons elim
  (k - 1) + 1 ;invoke IH
  k ;algebra

  RHS:
  (k + 1) - 1 ;premise of RHS
  k ;algebra

  Since the LHS = RHS, this completes the leap step.
  Since both the leap case and the base case has been proven, this demonstrates by PORI that for all lists L with a length >= 1 (length (cut_end L)) = x - 1 where x is the length of the list.
|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 5a (10pts)
;Write a recursive function (add_pairs L)
;that adds pairs of numbers.
;You may assume the length of L will always be even.

; Adds pairs of numbers
; Input:  L a list (the list must have even length)
; Output: A new list with pairs of elements added together.
;Question 5
(define (add_pairs L)
  (cond
    [(empty? L) '()]
    [else (cons (+ (first L) (first (rest L)))
                (add_pairs (rest (rest L))))]))
;end

(display "Question 5a add_pairs Tests (10 points)\n")
(define-test-suite test_add_pairs
  (check-equal? (add_pairs '()) '())
  (check-equal? (add_pairs '(1 2)) '(3))
  (check-equal? (add_pairs '(1 2 3 4)) '(3 7))
  (check-equal? (add_pairs '(2 2 2 2)) '(4 4))
  (check-equal? (add_pairs '(0 -1 -2 3)) '(-1 1))
  (check-equal? (add_pairs '(1 1 1 1)) '(2 2))
  (check-equal? (add_pairs '(1 2 3 4 5 6 7 8)) '(3 7 11 15))
  (check-equal? (add_pairs '(9 9 9 9 9 9)) '(18 18 18))
  (check-equal? (add_pairs '(7 3 4 6 5 5)) '(10 10 10))
  (check-equal? (add_pairs '(-9 9 -8 8)) '(0 0))
  
)
(define q5a_score (- 10 (run-tests test_add_pairs 'verbose)))

;Question 5b
;Prove by Induction that (length (add_pairs L)) = x/2, where x is the length of L
;for all L where (even? (length L)) and x = (length L) >= 0
;You may use the properties of length from Question 3

#|
  anchor case: L = '()
  base case: (length (add_pairs '())) = 0

  proof of base case:
  LHS:
  (length (add_pairs '())) ;premise of LHS
  (length (if (null? '()) '() (cons (+ (first '()) (first (rest '()))) (add_pairs (rest (rest '())))))) ;apply definition of add_pairs
  (length (if #t '() (cons (+ (first '()) (first (rest '()))) (add_pairs (rest (rest '())))))) ;eval null?
  (length '()) ;eval if
  0 ;length lemma 1

  RHS:
  0 ;premise of RHS

  inductive hypothesis: (length (add_pairs L)) = k/2
  leap step: (length (add_pairs (cons a (cons b L)))) = (k + 2)/2

  proof of leap step:
  LHS:
  (length (add_pairs (cons a (cons b L)))) ;premise of LHS
  (length (if (null? (cons a (cons b L))) '() (cons (+ (first (cons a (cons b L))) (first (rest (cons a (cons b L))))) (add_pairs (rest (rest (cons a (cons b L)))))))) ;apply definition of add_pairs
  (length (if #f '() (cons (+ ((first (cons a (cons b L))) (first (rest (cons a (cons b L))))) (add_pairs (rest (rest (cons a (cons b L))))))))) ;eval null? [must be false because L is not null by IH and input contract]
  (length (cons (+ (first (cons a (cons b L))) (first (rest (cons a (cons b L)))) (add_pairs (rest (rest (cons a (cons b L))))))) ;eval if
  (length (cons (+ a (first (rest (cons a (cons b L)))) (add_pairs (rest (rest (cons a (cons b L))))))) ;eval first-cons elim
  (length (cons (+ a (first (cons b L)) (add_pairs (rest (rest (cons a (cons b L)))))))) ;eval first-rest elim
  (length (cons (+ a b) (add_pairs (rest (rest (cons a (cons b L))))))) ;eval first-cons elim
  (length (cons (+ a b) (add_pairs (rest (cons b L))))) ;eval rest-cons elim
  (length (cons (+ a b) (add_pairs L))) ;eval rest-cons elim
  (+ 1 (length (add_pairs L))) ;length lemma 2
  1 + k/2 ;invoke IH
  (k + 2)/2 ;algebra

  RHS:
  (k + 2)/2 ;premise of RHS

  Since the LHS = RHS, this completes the leap step.
  Since both the leap case and the base case has been proven, this demonstrates by PORI that for all lists L with an even length that (length (add_pairs L)) = n/2 where n is the length of all elements in L.
|#

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")


(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")