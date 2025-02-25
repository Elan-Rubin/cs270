#lang racket

(require rackunit)
(require rackunit/text-ui)


;CS 270 Math Foundations of CS
;Create By Professor Bruce Char, Professor Mark Boady, and Professor Jeremy Johnson
;Drexel University
;Homework 8

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;In this exercise, we will directly relate integers and boolean values.
;We will use this ability to generate partial truth tables.
;Specifically, we will generate the inputs to the function.
;Next week, we will create the truth table outputs.
;Afterwards, will will combine these parts to solve a problems.

;Question 1: Convert an Integer to a List of true/false values.
;Implement the function int_to_bool with the following specs
; Input contract: n is a non-negative integer
; output contract: (int_to_bool n) is a list of booleans corresponding
;      to the integers reprentation in binary, with #f representing 0 and #t being 1
; Example: (int_to_bool 6) would be (#t #t #f) since 6 in binary is 110 = 1*2^2 + 1*2^1 + 0*2^0
; Requirements: when done optimally, there's on need for nested if/cond, helpers, reverse, length, or expt.
;     append is permitted, and for ease of later implementations, 0 will be represented as null rather than '(#f)

(define (int_to_bool n)
  (int_to_bool_helper n '())
)

(define (int_to_bool_helper n L)
  (if (zero? n)
    L
    (int_to_bool_helper (quotient n 2) (cons (odd? n) L))
))


;Test to see if you function works correctly
(define-test-suite test_int_to_bool
  (check-equal? (int_to_bool 1) '(#t))
  (check-equal? (int_to_bool 2) '(#t #f))
  (check-equal? (int_to_bool 3) '(#t #t))
  (check-equal? (int_to_bool 4) '(#t #f #f))
  (check-equal? (int_to_bool 5) '(#t #f #t))
  (check-equal? (int_to_bool 6) '(#t #t #f))
  (check-equal? (int_to_bool 7) '(#t #t #t))
  (check-equal? (int_to_bool 8) '(#t #f #f #f))
  (check-equal? (int_to_bool 9) '(#t #f #f #t))
  (check-equal? (int_to_bool 10) '(#t #f #t #f))
  (check-equal? (int_to_bool 11) '(#t #f #t #t))
  (check-equal? (int_to_bool 12) '(#t #t #f #f))
  (check-equal? (int_to_bool 13) '(#t #t #f #t))
  (check-equal? (int_to_bool 14) '(#t #t #t #f))
  (check-equal? (int_to_bool 15) '(#t #t #t #t))
  (check-equal? (int_to_bool 16) '(#t #f #f #f #f))
)
(display "Question 1.) int_to_bool Results (8 points)\n")
(define q1_score (* (/ 1.0 2.0) (- 16 (run-tests test_int_to_bool 'verbose))))


;Question 2
;Only significant binary digits are stored by the above function.
;In reality, we would want every number to have the same bit length.
; input contract: num_bits is a nonnegative integer, bit_list is a list of booleans
; output contract: (pad num_bits bit_list) is the same list a bit_list except that it has been "padded"
;      with enough #f in front to make the list have a length of num_bits.  if the bit_list is already
;      at that length or longer, than the original list is just returned
; Example: (pad 5 '(#t #t)) would be (#f #f #f #t #t), and (pad 2 '(#t #f #t)) would be '(#t #f #t)
; Requirements: when done optimally, there's on need for nested if/cond, helpers, reverse, append, or expt.
;     For ease of implementation, the length function is permitted

(define (pad num_bits bit_list)
  (define (list-len lst)
    (if (null? lst)
        0
        (+ 1 (list-len (rest lst)))))
  
  (if (= num_bits (list-len bit_list))
      bit_list
      (pad num_bits (cons #f bit_list))))

;Check your function with the below tests
(define-test-suite test_pad
  (check-equal? (pad 5 (int_to_bool 0))  '(#f #f #f #f #f))
  (check-equal? (pad 5 (int_to_bool 1))  '(#f #f #f #f #t))
  (check-equal? (pad 5 (int_to_bool 2))  '(#f #f #f #t #f))
  (check-equal? (pad 5 (int_to_bool 3))  '(#f #f #f #t #t))
  (check-equal? (pad 5 (int_to_bool 4))  '(#f #f #t #f #f))
  (check-equal? (pad 5 (int_to_bool 5))  '(#f #f #t #f #t))
  (check-equal? (pad 5 (int_to_bool 6))  '(#f #f #t #t #f))
  (check-equal? (pad 5 (int_to_bool 7))  '(#f #f #t #t #t))
  (check-equal? (pad 5 (int_to_bool 8))  '(#f #t #f #f #f))
  (check-equal? (pad 5 (int_to_bool 9))  '(#f #t #f #f #t))
  (check-equal? (pad 5 (int_to_bool 10)) '(#f #t #f #t #f))
  (check-equal? (pad 5 (int_to_bool 11)) '(#f #t #f #t #t))
  (check-equal? (pad 5 (int_to_bool 12)) '(#f #t #t #f #f))
  (check-equal? (pad 5 (int_to_bool 13)) '(#f #t #t #f #t))
  (check-equal? (pad 5 (int_to_bool 14)) '(#f #t #t #t #f))
  (check-equal? (pad 5 (int_to_bool 15)) '(#f #t #t #t #t))
)
(display "Question 2.) pad Results (8 points)\n")
(define q2_score (* (/ 1.0 2.0) (- 16 (run-tests test_pad 'verbose))))

;Question 3: Generate a Truth Table
; input contract: n is a nonnegative integer
; output contract: (tt_inputs n) is a list of boolean lists representing the input rows of a truth table
;      with n variables, starting with all Trues and ending with all Falses. The answer should contain
;      2^n lists, and each of those sublists should have exactly n members.
; Example: (tt_inputs 2) would be '( (#t #t) (#t #f) (#f #t) (#f #f) )
; Requirements:  There are two extremely different approaches to solving this, you can choose either you
; find easiest.  Method One (which would require a helper function) is to utilize together the prior two questions,
;      essentially converting the list of integers from 1 to 2^n-1 into the padded boolean versions.
; Method Two is to bypass the integer conversion and just directly append two recursive calls (one that begins with
;      #t and the other with #f).  If you *successfully* do both methods, you can earn +10 bonus points (no bonus
;      bonus points if you try both but one doesn't work; in which case your lowest scoring one will count, so only
;      do both if you've testing both and they both work!)

(define (tt_inputs n)
  (tt_inputs_helper n (- (expt 2 n) 1))
)

(define (tt_inputs_helper bits row_val)
  (if (zero? row_val)
    (list (pad bits (int_to_bool 0)))
    (cons (pad bits (int_to_bool row_val)) (tt_inputs_helper bits (- row_val 1))))
)

;Check your function with the following tests
(define-test-suite test_tt

  (check-equal? (tt_inputs 1)
                '( (#t) (#f) )
  )
  (check-equal? (tt_inputs 2)
                '( (#t #t) (#t #f) (#f #t) (#f #f))  )

  (check-equal? (tt_inputs 3)
                '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)))

   (check-equal? (tt_inputs 4)
                '( (#t #t #t #t)
                   (#t #t #t #f)
                   (#t #t #f #t)
                   (#t #t #f #f)
                   (#t #f #t #t)
                   (#t #f #t #f)
                   (#t #f #f #t)
                   (#t #f #f #f)
                   (#f #t #t #t)
                   (#f #t #t #f)
                   (#f #t #f #t)
                   (#f #t #f #f)
                   (#f #f #t #t)
                   (#f #f #t #f)
                   (#f #f #f #t)
                   (#f #f #f #f)))
  (check-equal? (tt_inputs 5)
                '((#t #t #t #t #t)
                  (#t #t #t #t #f)
                  (#t #t #t #f #t)
                  (#t #t #t #f #f)
                  (#t #t #f #t #t)
                  (#t #t #f #t #f)
                  (#t #t #f #f #t)
                  (#t #t #f #f #f)
                  (#t #f #t #t #t)
                  (#t #f #t #t #f)
                  (#t #f #t #f #t)
                  (#t #f #t #f #f)
                  (#t #f #f #t #t)
                  (#t #f #f #t #f)
                  (#t #f #f #f #t)
                  (#t #f #f #f #f)
                  (#f #t #t #t #t)
                  (#f #t #t #t #f)
                  (#f #t #t #f #t)
                  (#f #t #t #f #f)
                  (#f #t #f #t #t)
                  (#f #t #f #t #f)
                  (#f #t #f #f #t)
                  (#f #t #f #f #f)
                  (#f #f #t #t #t)
                  (#f #f #t #t #f)
                  (#f #f #t #f #t)
                  (#f #f #t #f #f)
                  (#f #f #f #t #t)
                  (#f #f #f #t #f)
                  (#f #f #f #f #t)
                  (#f #f #f #f #f))))
(display "Question 3.) tt_inputs Results (10 points)\n")
(define q3_score (- 10 (* 2 (run-tests test_tt 'verbose))))

;Question 4
;The inputs we made above have the format '(#t #f #f #t).
;We need boolean expressions that work with this format.
;We will make function that take a list as input
;This function implements (A->B). Note that for ease of readability,
;    for questions 4abc only, the let function is permitted
(define (implies_example boolean_vars)
  (let (;start of name list
        (a (list-ref boolean_vars 0));pairs - name value
        (b (list-ref boolean_vars 1))
      );end of name list
    (or (not a) b)
 );end of let
)

;Test Implies Def
(define-test-suite test_implies
  (check-equal? (implies_example '(#t #t)) #t)
  (check-equal? (implies_example '(#t #f)) #f)
  (check-equal? (implies_example '(#f #t)) #t)
  (check-equal? (implies_example '(#f #f)) #t)
)
(display "Example.) Results of Implies Example\n")
;(run-tests test_implies)


;Write the following three simple boolean expressions as functions. 
;Question 4a
;a.) Example Function 1
;implement ¬(¬a ∨ (b ∨ ¬a) )
(define (example_expr1 bool_vars)
  (not (or (not (first bool_vars)) (or (second bool_vars) (not (first bool_vars)))))
)
;end
;Test Implies Def
(define-test-suite test_ex1
  (check-equal? (example_expr1 '(#t #t)) #f)
  (check-equal? (example_expr1 '(#t #f)) #t)
  (check-equal? (example_expr1 '(#f #t)) #f)
  (check-equal? (example_expr1 '(#f #f)) #f)
)
(display "4a.) Results of Example Function 1 (8 points)\n")
(define q4a_score (- 8 (* 2 (run-tests test_ex1 'verbose))))

;Question 4b
;b.) Example Function 2
;implement (a ∧ b) ∨ (¬a ∧ ¬b)
(define (example_expr2 bool_vars)
  (or (and (first bool_vars) (second bool_vars)) (and (not (first bool_vars)) (not (second bool_vars))))
)
;end
;Test Implies Def
(define-test-suite test_ex2
  (check-equal? (example_expr2 '(#t #t)) #t)
  (check-equal? (example_expr2 '(#t #f)) #f)
  (check-equal? (example_expr2 '(#f #t)) #f)
  (check-equal? (example_expr2 '(#f #f)) #t)
)
(display "4b.) Results of Example Function 2 (8 points)\n")
(define q4b_score (- 8 (* 2 (run-tests test_ex2 'verbose))))

;Question 4c
;c.) Example Function 3
;implement (a ∧ ¬b ∧ c)
(define (example_expr3 bool_vars)
  (and (first bool_vars) (not (second bool_vars)) (third bool_vars))
)
;end
;Test Implies Def
(define-test-suite test_ex3
  (check-equal? (example_expr3 '(#t #t #t)) #f)
  (check-equal? (example_expr3 '(#t #t #f)) #f)
  (check-equal? (example_expr3 '(#t #f #t)) #t)
  (check-equal? (example_expr3 '(#t #f #f)) #f)
  (check-equal? (example_expr3 '(#f #t #t)) #f)
  (check-equal? (example_expr3 '(#f #t #f)) #f)
  (check-equal? (example_expr3 '(#f #f #t)) #f)
  (check-equal? (example_expr3 '(#f #f #f)) #f)
)
(display "4c.) Results of Example Function 3 (16 points)\n")
(define q4c_score (- 16 (* 2 (run-tests test_ex3 'verbose))))

;Question 5
; input contract: fun is a function representing a boolean expression such as in Q4
;    (in other words,  a function that takes a list of boolean values and returns a boolean)
;    and tt is a truth table (i.e. a list of lists of booleans)
; output contract: (evaluate_tt fun tt) is the list of booleans representing the result
;    of plugging in each row of the truth table inputs into the provided function
; Example: suppose fun represents the function (not p), and tt represents the one variable truth table of ((#t)(#f))
;    Then the output of (evaluate_tt fun tt) should be (#f #t)
; Requirements: this problem is optimally done NONrecursively

;Question 5
(define (evaluate_tt fun tt)
  (cond
    ((null? tt) '())
    (else (cons (fun (first tt))
                (evaluate_tt fun (rest tt))))
  ))

;Test your function
(define-test-suite test_eval_tt
  (check-equal?
   (evaluate_tt implies_example '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #f #t #t))
  (check-equal?
   (evaluate_tt example_expr1 '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#f #t #f #f))
  (check-equal?
   (evaluate_tt example_expr2 '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #f #f #t))
  (check-equal?
   (evaluate_tt example_expr3
                '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)))
   '(#f #f #t #f #f #f #f #f)))
(display "5.) Results of Evaluate on Truth Table (12 points)\n")
(define q5_score (- 12 (* 3 (run-tests test_eval_tt 'verbose))))

;Question 6: implement an inverse of Q1
; input contract: bools is a list of booleans
; output contract: (bool_to_int bools) is the integer representation of the booleans in binary
; Example: (bool_to_int '(#t #t #f)) would be 6, since 6 in binary is 110 = 1*2^2 + 1*2^1 + 0*2^0
; Requirements: when done optimally, there's no need for nested if/cond, helpers, append, or reverse.
;     For ease of later implementations, length and append are permitted.

(define (bool_to_int values)
  (bool_to_int_helper (reverse values) 0)
)

(define (bool_to_int_helper values exp)
  (if (null? values)
    0
    (+ (* (expt 2 exp) (if (first values) 1 0)) (bool_to_int_helper (rest values) (+ exp 1))))
)
;Test your function
(define-test-suite test_b2i
  (check-equal? (bool_to_int '(#f #f #f #f)) 0)
  (check-equal? (bool_to_int '(#f #f #f #t)) 1)
  (check-equal? (bool_to_int '(#f #f #t #f)) 2)
  (check-equal? (bool_to_int '(#f #f #t #t)) 3)
  (check-equal? (bool_to_int '(#f #t #f #f)) 4)
  (check-equal? (bool_to_int '(#f #t #f #t)) 5)
  (check-equal? (bool_to_int '(#f #t #t #f)) 6)
  (check-equal? (bool_to_int '(#f #t #t #t)) 7)
  (check-equal? (bool_to_int '(#t #f #f #f)) 8)
  (check-equal? (bool_to_int null) 0)
  (check-equal? (bool_to_int '(#t)) 1)
  (check-equal? (bool_to_int '(#t #f)) 2)
  (check-equal? (bool_to_int '(#t #t)) 3)
  (check-equal? (bool_to_int '(#t #f #f)) 4)
  (check-equal? (bool_to_int '(#t #f #t)) 5)
  (check-equal? (bool_to_int '(#t #t #f)) 6)
  (check-equal? (bool_to_int '(#t #t #t)) 7)
  (check-equal? (bool_to_int '(#t #t #t #t)) 15)
)
(display "6.) Results of Bool to Int (18 points)\n")
(define q6_score (- 18 (run-tests test_b2i 'verbose)))

;Question 7: Satisfiability Solver
; input contract: func is a function representing a logic expression and n is the number of variables it has
; output contract (sat_solve func n) is the list of inputs that make the expression true, where those inputs
;      are represented as integers.
; Example: (or a b) is true when '(#t #t) '(#t #f) '(#f #t), therefore the answer would be '(3 2 1)
; Requirements: do not use let, list-ref, match, reverse, length, append, expt, or nested ifs.
;      a single cond is permitted, and a single helper function is recommended. you may utilize functions
;      created in previous questions. 

(define (sat_solve func n)
  (map bool_to_int (filter func (tt_inputs n)))
)

;Test your function
(define-test-suite test_sat_solve
  (check-equal? (sat_solve implies_example 2) '(3 1 0))
  (check-equal? (sat_solve example_expr1 2) '(2))
  (check-equal? (sat_solve example_expr2 2) '(3 0))
  (check-equal? (sat_solve example_expr3 3) '(5))
  (check-equal? (sat_solve (lambda (X) (and (first X) (second X))) 2) '(3))
  (check-equal? (sat_solve (lambda (X) (or (first X) (second X))) 2) '(3 2 1))
)
(display "7.) Results of SAT Solver Question (12 points)\n")
(define q7_score (- 12 (* 2 (run-tests test_sat_solve 'verbose))))


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1 Scored: ")
(display q1_score)
(display "/8\n")
(display "Q2 Scored: ")
(display q2_score)
(display "/8\n")
(display "Q3 Scored: ")
(display q3_score)
(display "/10\n")
(display "Q4 Scored: ")
(display (+ q4a_score q4b_score q4c_score))
(display "/32\n")
(display "Q5 Scored: ")
(display q5_score)
(display "/12\n")
(display "Q6 Scored: ")
(display q6_score)
(display "/18\n")
(display "Q7 Scored: ")
(display q7_score)
(display "/12\n")

(define grand_total (+ q1_score q2_score q3_score q4a_score q4b_score q4c_score q5_score q6_score q7_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")