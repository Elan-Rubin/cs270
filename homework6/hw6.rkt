#lang racket
;Put Your Name Here: Elan Rubin edr53

#|
CS 270 Math Foundations of CS
Homework 6
Created by Professor Bruce Char, Professor Mark Boady, Professor Jeremy Johnson, and Steve Earth

Submit in BBLearn.

Once you write a function, you may use it in later questions.

Important Rules:
1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
    Recursive helper functions are allowed (the main function not being recursive).
3.) You may not use the set! command. If used, your answer will get a zero.
4.) Using If/Cond to explicitly pass tests instead of following the instructions
    will always result in a zero for that question.

  Peano arithmetic

    In words:  A peano number (aka "pnum" for short) is either zero, or, recursively, the symbol s cons-ed 
    to a peano number.

    Formally:  Pnum := null | (cons s [pnum])
|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

#|

Peano arithmetic.  Note that for these problems, uppercase designates a pnum and lowercase is the integer it represents.
For instance, we might have M = '(s s s) in which case m = 3.

|#

;The pnum representation for 0 will be the null list.
;similar to how zero? works on the actual integers, we want a corresponding predicate for the pnums.
;input-contract: (list? N)
;output-contract: (boolean? (zero? N)). that is, pzero? is the predicate for whether the pnum N represents 0
(define pzero? null?)
(define zero? null?)

;Checks if the input is a list representing a peano number
;input-contract: (list? N)
;output-contract: (boolean? (nat? N)). i.e. nat? is the predicate checking whether N is a pnum.
(define (nat? N)
  (cond
    [(pzero? N) #t]
    [(cons? N) (and (equal? (first N) 's) (nat? (rest N)))]
    [else #f]))

;Increment a peano number by adding 1
;input-contract: (nat? N)
;output-contract: (nat? (succ N)). i.e. (succ N) is the pnum representing n+1
(define (succ N) (cons 's N))

;Decrement a peano number by subtracting 1
;input-contract: (nat? N)
;output-contract: (nat? (pred N)).  i.e. (pred N) is the pnum representing n-1 provided n>0
(define (pred N) (if (pzero? N) null (rest N)))

;Define a collection of common numbers
(define zero null)
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine (succ eight))
(define ten (succ nine))

; Addition of Peano numbers
; input-contract: (and (nat? M) (nat? N)). i.e. M and N are pnums representing the integers m and n.
; output-contract: (nat? (plus M N)).   i.e. (plus M N) is the pnum representing m+n.
(define (plus M N) (if (pzero? M) N (succ (plus (pred M) N))))

; Multiplication of Peano numbers
; input-contract: (and (nat? M) (nat? N)). M,N are the pnum representations of the integers m,n.
; output-contract: (nat? (mult M N)). i.e. (mult M N) is the pnum representing m*n.
(define (mult M N) (if (pzero? M) M (plus N (mult (pred M) N))))

; Comparison of Peano numbers
; input-contract: (and (nat? M) (nat? N)). M,N are the pnum representations of the integers m,n.
; output-contract: (boolean? (ltnat? M N)). that is, (ltnat? M N) is the predicate for m<n.
(define (ltnat? M N)  ;Note: nested if's were used here rather than cond in order to make the proofs easier
  (if(pzero? N) #f (if (pzero? M) #t (ltnat? (pred M) (pred N)))))

;--------------- Question 1.  Implement subtraction of Peano numbers. --------------
;See specification below.

; Subtraction of Peano numbers
; input-contract: (and (nat? M) (nat? N))
; output-contract: (nat? (sub M N)). (sub M N) is the pnum of m-n if m >= n; otherwise return zero
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (sub M N)
  (cond
    [(pzero? N) M] 
    [(pzero? M) zero] 
    [else (sub (pred M) (pred N))]))

;Test Bed
(display "Question 1 - Subtraction (10 points)\n")
(define-test-suite peano-subtract
  (check-equal? (sub ten ten) zero)
  (check-equal? (sub ten two) eight)
  (check-equal? (sub nine nine) zero)
  (check-equal? (sub nine one) eight)
  (check-equal? (sub eight six) two)
  (check-equal? (sub eight five) three)
  (check-equal? (sub seven one) six)
  (check-equal? (sub seven five) two)
  (check-equal? (sub six six) zero)
  (check-equal? (sub six two) four)
)
(define q1_score (- 10 (run-tests peano-subtract 'verbose)))

;--------------- Question 2.  Implement Quotient of Peano numbers. --------------

; Division of Peano numbers
; input-contract: (and (nat? M) (nat? N) (not (zero? N)))
; output-contract: (nat? (ltnat? M N))
; Returns a Peano number whose value q is the quotient of m divided by n.
; Remember if x/y then quotient q and remainder r meet the following requirement:
; x = q*y + r with 0 <= r < x. Return floor(m/n) otherwise
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (div M N)
  (if (ltnat? M N)
    zero
    (succ (div (sub M N) N))))

;Test Bed
(display "Question 2 - Division (10 points)\n")
(define-test-suite peano-div
  (check-equal? (div ten ten) one)
  (check-equal? (div ten two) five)
  (check-equal? (div nine three) three)
  (check-equal? (div nine one) nine)
  (check-equal? (div eight six) one)
  (check-equal? (div eight four) two)
  (check-equal? (div one seven) zero)
  (check-equal? (div seven five) one)
  (check-equal? (div six six) one)
  (check-equal? (div six two) three)
)
(define q2_score  (- 10 (run-tests peano-div 'verbose)))


;--------------- Question 3.  Implement Remainder of Peano numbers. --------------
; Remainder of Peano numbers
; input-contract: (and (nat? M) (nat? N) (not (zero? N)))
; output-contract: (nat? (rem M N)), that is (rem M N) is the pnum representation of m%n
; Returns a Peano number representing the remainder r of m divided by n.
; Remember for x/y, the quotient q and remainder r meet the following requirement: x = q*y + r with 0 <= r < x.
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (rem M N)
  (cond
    [(pzero? N) zero]  
    [(ltnat? M N) M]
    [else (rem (sub M N) N)]))

;Test Bed
(display "Question 3 - Remainder (10 points)\n")
(define-test-suite peano-rem
  (check-equal? (rem ten three) one)
  (check-equal? (rem ten two) zero)
  (check-equal? (rem nine three) zero)
  (check-equal? (rem nine one) zero)
  (check-equal? (rem eight five) three)
  (check-equal? (rem eight four) zero)
  (check-equal? (rem one seven) one)
  (check-equal? (rem seven five) two)
  (check-equal? (rem six six) zero)
  (check-equal? (rem six two) zero)
)
(define q3_score  (- 10 (run-tests peano-rem 'verbose)))

;--------------- Question 4.  Implement Not Equal of Peano numbers. --------------
; Not Equal of Peano numbers
; input-contract: (and (nat? M) (nat? N))
; output-contract: (boolean? (neq M N)). (neq M N) is the predicate for m ≠ n
;                  in other words, returns true when the numbers are not equal and false when they are equal
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
(define (neq M N)
  (not (and (zero? (sub M N)) (zero? (sub N M)))))

;Test Bed
(display "Question 4 - Not Equal (10 points)\n")
(define-test-suite peano-neq
  (check-equal? (neq ten ten) #f)
  (check-equal? (neq six six) #f)
  (check-equal? (neq five five) #f)
  (check-equal? (neq four four) #f)
  (check-equal? (neq three three) #f)
  (check-equal? (neq two two) #f)
  (check-equal? (neq one one) #f)
  (check-equal? (neq seven five) #t)
  (check-equal? (neq six nine) #t)
  (check-equal? (neq six two) #t)
)
(define q4_score  (- 10 (run-tests peano-neq 'verbose)))

;--------------- Question 5.  Implement GCD of Peano numbers. --------------
#|
             Implement a function to compute the greatest common divisor
             of the Peano numbers m and n.  g = gcd(m,n) satisfies
             1)  g is a common divisor of m and n.
                 g divides m and g divides n.  I.E. the remainder when
                 dividing m and n by g is 0.
             2)  g is the greatest common divisor.
                 If e divides m and e divides n then e must divide g.

             The gcd(m,n) can be computed recursively.
             1)  gcd(m,0) = m
             2)  gcd(m,n) = gcd(n,remainder of m divided by n).
|#

; Greatest common divisor of Peano numbers
; input-contract: (and (nat? M) (nat? N)))
; output-contract: (nat? (gcd M N)). i.e. (gcd M N) is the pnum representation for gcd(m,n)
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
; HINT:  See algorithm in comments above
(define (gcd M N)
  (if (zero? N)
    M
    (gcd N (rem M N))))

(display "Question 5 - GCD (20 points)\n")
(define-test-suite peano-gcd
  (check-equal? (gcd two ten) two)
  (check-equal? (gcd two four) two)
  (check-equal? (gcd three zero) three)
  (check-equal? (gcd three two) one)
  (check-equal? (gcd three three) three)
  (check-equal? (gcd three five) one)
  (check-equal? (gcd three six) three)
  (check-equal? (gcd three nine) three)
  (check-equal? (gcd three ten) one)
  (check-equal? (gcd four two) two)
  (check-equal? (gcd five ten) five)
  (check-equal? (gcd six one) one)
  (check-equal? (gcd six seven) one)
  (check-equal? (gcd seven six) one)
  (check-equal? (gcd eight two) two)
  (check-equal? (gcd eight four) four)
  (check-equal? (gcd eight eight) eight)
  (check-equal? (gcd nine one) one)
  (check-equal? (gcd nine ten) one)
  (check-equal? (gcd ten three) one)

)
(define q5_score  (- 20 (run-tests peano-gcd 'verbose)))

;--------------- Question 6.  Implement Mod Power of Peano numbers. --------------
#|
            The mod power is commonly used in cryptography.
            We want to compute (x^y) % b
            When we compute exponents, the number normally get very large.
            If we only need the remainder, we can take it at each stage
            This keeps our numbers smaller

            The algorithm is described below
            (x^y)%b = 1 if y==0
            (x^y)%b = (x*(x^(y-1)%b)%b otherwise
|#

; Mod Power of Peano numbers
; input-contract: (and (nat? X) (nat? Y) (nat? B))
; output-contract: (nat? (modpow X Y B)), where (modpow X Y B) is the pnum representation for (x^y)% b
; Requirements: You may not convert to or from traditional integers, nor use length. Your function must be recursive.
; HINT:  See algorithm in comments above
(define (modpow X Y B)
  (cond
    [(pzero? Y) one] 
    [else (rem (mult X (modpow X (pred Y) B)) B)]))

(display "Question 6 - Mod Power (20 points)\n")
(define-test-suite peano-modpow
  (check-equal? (modpow two zero three) one)
  (check-equal? (modpow two one three) two)
  (check-equal? (modpow two two three) one)
  (check-equal? (modpow two three three) two)
  (check-equal? (modpow three zero four) one)
  (check-equal? (modpow three one four) three)
  (check-equal? (modpow three two four) one)
  (check-equal? (modpow three three four) three)
  (check-equal? (modpow two zero four) one)
  (check-equal? (modpow two one four) two)
  (check-equal? (modpow two two four) zero)
  (check-equal? (modpow two three four) zero)
  (check-equal? (modpow three zero five) one)
  (check-equal? (modpow three one five) three)
  (check-equal? (modpow three two five) four)
  (check-equal? (modpow three three five) two)
  (check-equal? (modpow three four five) one)
  (check-equal? (modpow three five five) three)
  (check-equal? (modpow three four two) one)
  (check-equal? (modpow three five two) one)
)
(define q6_score  (- 20 (run-tests peano-modpow 'verbose)))

;--------------- Question 7.  Inductive Proof --------------

;This question will be manually graded by the course assistants, and uses the following function:

; Input contract: n is a nonnegative integer
; Output contract: (toPeano n)=N, the pnum representation of n
(define (toPeano n) (if (zero? n) null (cons 's (toPeano (- n 1)))))

;Prove the following claim:
;let m be any nonnegative integer and,n be any positive integer.
;Prove the claim that (ltnat? (toPeano m) (toPeano (+ m n))) = #t

;You may use the following in your proof [for a bonus 10pts to this assignment, you can prove the lemmas.
;Note: proofs of the lemmas are short and do NOT require induction; a standard equational reasoning argument suffices]
;Property of Addition: (+ (+ x y) 1) = (+ (+ x 1) y)
;Lemma 1: (pzero? (toPeano (+ x y))) = #f if x is nonnegative and y is positive
;Lemma 2: (pred (toPeano (+ x 1))) = (toPeano x)

;Provide a Proof by Induction on m assuming n is a constant.
;This question is worth 20 points. To get full credit, you must show all the steps and their justifications.

#| Enter proof here; you may create extra space within this comment block as needed.
; 1 point: first, explain what the claim is saying in plain english (you should only need a single sentence;
do not refer to any details of any code. imagine you are talking to someone that is not a programmer).
the claim is saying for any positive integers m and n, m will always be less than m + n

;anchor identified: 1 point
> m = 1 and n = 1

;base case lhs: 4 points
(ltnat? (toPeano 1) (toPeano (+ 1 n))) ; premise of lhs
(ltnat? (if (= 1 0) null (cons 's (toPeano (- 1 1)))) (toPeano (+ 1 n))) ; application of definition of topeano
(ltnat? (if #f null (cons 's (toPeano (- 1 1)))) (toPeano (+ 1 n))) ; evaluate =
(ltnat? (cons 's (toPeano (- 1 1))) (toPeano (+ 1 n))) ; evaluate if
(ltnat? (cons 's (toPeano 0)) (toPeano (+ 1 n))) ; evaluate -
(ltnat? (cons 's (if (= 0 0) null (cons 's (toPeano (- 0 1))))) (toPeano (+ 1 n))) ; application of definition of topeano
(ltnat? (cons 's (if #t null (cons 's (toPeano (- n 1))))) (toPeano (+ 1 n))) ; evaluate =
(ltnat? (cons 's null) (toPeano (+ 1 n))) ; evaluate if
(ltnat? '(s) (toPeano (+ 1 n))) ; evaluate cons
(if (zero? (toPeano (+ 1 n))) #f (if (zero? '(s)) #t (ltnat? (pred '(s)) (pred (toPeano (+ 1 n)))))) ; application of definition of ltnat?
(if #f #f (if (zero? '(s)) #t (ltnat? (pred '(s)) (pred (toPeano (+ 1 n)))))) ; lemma 2 [n cannot be <= 0 because of input contract]
(if (zero? '(s)) #t (ltnat? (pred '(s)) (pred (toPeano (+ 1 n))))) ; evaluate if
(if (null? '(s)) #t (ltnat? (pred '(s)) (pred (toPeano (+ 1 n))))) ; application of definition of zero?
(if #f #t (ltnat? (pred '(s)) (pred (toPeano (+ 1 n))))) ; evaluate null?
(ltnat? (pred '(s)) (pred (toPeano (+ 1 n)))) ; evaluate if
(ltnat? (if (zero? '(s)) null (rest '(s))) (pred (toPeano (+ 1 n)))) ; application of definition of pred
(ltnat? (if (null? '(s)) null (rest '(s))) (pred (toPeano (+ 1 n)))) ; application of definition of zero?
(ltnat? (if #f null (rest '(s))) (pred (toPeano (+ 1 n)))) ; evaluate null?
(ltnat? (rest '(s)) (pred (toPeano (+ 1 n)))) ; evaluate if
(ltnat? '() (pred (toPeano (+ 1 n)))) ; evaluate rest
(ltnat? '() (toPeano n)) ; lemma 1
(if (zero? (toPeano n)) #f (if (zero? '()) #t (ltnat? (pred '()) (pred (toPeano n))))) ; application of definition of ltnat?
(if (null? (toPeano n)) #f (if (zero? '()) #t (ltnat? (pred '()) (pred (toPeano n))))) ; application of definition of zero?
(if #f #f (if (zero? '()) #t (ltnat? (pred '()) (pred (toPeano n))))) ; evaluate null? [must be #f because of input contract]
(if (zero? '()) #t (ltnat? (pred '()) (pred (toPeano n)))) ; evaluate if
(if (null? '()) #t (ltnat? (pred '()) (pred (toPeano n)))) ; application of definition of zero?
(if #t #t (ltnat? (pred '()) (pred (toPeano n)))) ; evaluate null?
#t ; evaluate if

;base case rhs: 1 points
#t ; premise of rhs

;inductive hypothesis: 2 points
(ltnat? (toPeano k) (toPeano (+ k n))) = #t

;leap case lhs: 10 points
(ltnat? (toPeano (+ k 1)) (toPeano (+ (+ k 1) n))) ; premise lhs
(if (zero? (toPeano (+ (+ k 1) n))) #f (if (zero? (toPeano (+ k 1))) #t (ltnat? (pred (toPeano (+ k 1))) (pred (toPeano (+ (+ k 1) n)))))) ; application of definition of ltnat?
(if #f #f (if (zero? (toPeano (+ k 1))) #t (ltnat? (pred (toPeano (+ k 1))) (pred (toPeano (+ (+ k 1) n)))))) ; lemma 1 [k and n are both nonzero values]
(if (zero? (toPeano (+ k 1))) #t (ltnat? (pred (toPeano (+ k 1))) (pred (toPeano (+ (+ k 1) n))))) ; evaluate if
(if #f #t (ltnat? (pred (toPeano (+ k 1))) (pred (toPeano (+ (+ k 1) n))))) ; lemma 1 [k is a nonzero value]
(ltnat? (pred (toPeano (+ k 1))) (pred (toPeano (+ (+ k 1) n)))) ; evaluate if
(ltnat? (toPeano k) (pred (toPeano (+ (+ k 1) n)))) ; lemma 2
(ltnat? (toPeano k) (pred (toPeano (+ (+ k n) 1)))) ; algebra
(ltnat? (toPeano k) (toPeano (+ k n))) ; lemma 2
#t ; invoke ih

;leap case rhs: 1 points
#t ; premise of rhs
|#
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1 Scored: ")(display q1_score)(display "/10\n")
(display "Q2 Scored: ")(display q2_score)(display "/10\n")
(display "Q3 Scored: ")(display q3_score)(display "/10\n")
(display "Q4 Scored: ")(display q4_score)(display "/10\n")
(display "Q5 Scored: ")(display q5_score)(display "/20\n")
(display "Q6 Scored: ")(display q6_score)(display "/20\n")
(display "Q7 Scored: ")(display 0)(display "/20\n")
(define grand_total (+ q1_score q2_score q3_score q4_score q5_score q6_score))
(display "\n")(display "Total: ")(display grand_total)(display "/100\n")