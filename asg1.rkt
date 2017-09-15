;TEAM MEMBERS: Srinivasa Reddy Duggempudi(U00830362)
;Assignment-1



;(Q1)Define functions odd and even, which takes a list of symbols L, and produces a sublist of L containing symbols located at odd and even positions repectively.

;Depending on the function called, It produces either the elements at odd positions or even positions in the list.

;CONTRACT: even,odd:Functions

;PURPOSE: even,odd functions take a list as an input and produces the sublist containing elements that are in even and odd positions respectively based on the fuction called.

;CODE
#lang racket
(define (odd li )
(if (null? li) '()
    (cons (car li)
          (if (null? (cdr li)) '() (odd (cddr li)))
)
)
)
(define (even li)
(if (null? li) '() (odd (cdr li))
)
)

;TEST CASES AND EXPECTED OUTPUTS
; 1. check  (odd (list 3 5 1 6 8))  -->'(3 1 8)
; 2. check  (even (list 3 5 1 6 8))  -->'(5 6)
; 3. check  (odd '(3 5 1 6 8))  -->'(3 1 8)
; 4. check   (even '(3 5 1 6 8))  -->'(5 6)
; 5. check  (odd '(lists even odd function positions called))  -->'(lists odd positions)
; 6. check  (even '(lists even odd function positions called))  -->'(even function called)





;(Q2)Develop a function duplicate that takes a nested list containing symbols and numbers as input and produces the nested list of symbols and numbers with immediately duplicating every element.

;CONTRACT: duplicate:nested list

;PURPOSE: duplicate function takes a nested list containing both symbols and numbers as input and gives a nested list with same symbols and numbers with immediate duplicationof every element as output.

;CODE:
(define (duplicate li)
(cond ((null? li) '())
      ((symbol? (car li)) (append (list (car li) (car li)) (duplicate (cdr li))))
      ((number? (car li)) (append (list (car li) (car li)) (duplicate (cdr li))))
      (else (cons (duplicate (car li)) (duplicate (cdr li))))
)
)

;TEST CASES AND EXPECTED OUTCOMES
; 1. check (duplicate '(1 3 a (r t)))  -->'(1 1 3 3 a a (r r t t))
; 2. check  (duplicate '(2 r (s) 23 ((e))))  -->'(2 2 r r (s s) 23 23 ((e e)))





;(Q3)Develop a function dotproduct that takes input as two lists and produces a number its dot product.

;Based on the size of the lists the function produces either a number which is the dot product of the input lists or "incompatible".

;CONTRACT: dotproduct:lists -->number(dotproduct) or string(incompatible)

;PURPOSE:dotproduct function takes two lists as input and gives their dot product if they are of same size or else says incompatible as output.

;CODE:
(define (product li si)
(cond ((null? li) 0)
      (else (+ (* (car li) (car si)) (dotproduct (cdr li) (cdr si))))
)
)

(define (check si li)
(cond ((null? li) (if (null? si) 1 0))
      ((null? si) (if (null? li) 1 0))
      (else (* 1 (check (cdr li) (cdr si))))
)
)

(define (dotproduct si li)
(if (= 1 (check li si)) (product li si) "Incompatible")
)

;TEST CASES AND EXPECTED OUTPUTS
; (dotproduct '(2 5 6) '(5 6))  "Incompatible"
; (dotproduct '(2 5) '(5 9))  55
; (dotproduct '(1 3 5 2) '(4 6 8 1))  64





;(Q4)Develop a function lastless that takes a nested list with symbols and a symbol and produces an output as the same nested list with eleminating the last occurance of that symbol.

;CONTRACT: lastless:nested list

;PURPOSE: For the given nested list of symbols and a symbol as input to the function lastless it produces the same nested list with the symbols by eleminating the last occurance of the given symbol in the nested list.

;CODE:
(define (lastless li a)
(if (member? (cdr li) a) (cons (car li) (lastless (cdr li) a)) 
 
	(if (or (symbol? (car li)) (number? (car li))) (if (equal? (car li) a) (cdr li) li)
		(cons (lastless (car li) a) (cdr li))
	)
)
)  






(define (member? li a)
(cond ((null? li) #f)
((symbol? (car li)) (if (equal? (car li) a) (or #t (member? (cdr li) a))
					    (or #f (member? (cdr li) a))
			)
			)      
((number? (car li)) (if (equal? (car li) a) (or #t (member? (cdr li) a))
					    (or #f (member? (cdr li) a))
			)
			)
(else (or (member? (car li) a) (member? (cdr li) a) ) )
)
)

;TEST CASES AND EXPECTED OUTPUTS
; 1. check   (lastless '(a r e ( t e) s) 'e)  -->'(a r e (t) s)
; 2. check   (lastless '(a r e (t e) e) 'e)  -->'(a r e (t e))
; 3. check   (lastless '((s f) e g f) 's)  -->'((f) e g f)





;(Q5)Develop a function ptyper such that it replaces all the numbers with n and symbols with s in a nested list.

;CONTRACT: ptyper:list(nested list)

;PURPOSE: function ptyper takes a nested list as input and produces the same nested list with all the symbols and numbers replaced with s and n respectively as output.

;CODE:
(define (ptyper li)
(cond ((null? li) '())
      ((symbol? (car li)) (cons 's (ptyper (cdr li))))
      ((number? (car li)) (cons 'n (ptyper (cdr li))))
      ((list? (car li)) (cons (ptyper (car li)) (ptyper (cdr li))))
      (else (cons (car li) (ptyper (cdr li))))
)
)

;TEST CASES AND EXPECTED OUTPUT
; 1. check   (ptyper '(a f 1 (4  5)))  -->'(s s n (n n))
; 2. check   (ptyper '(#f 3 (! a) 4 f))  -->'(#f n (s s) n s)





;(Q6.1)(EOPL 3rd ed. Text. Pages 28. Exercises 1.29)
;Develop a function sort that takes a list of numbers as input and outputs a list with the list elements sorted in ascending order.

;CONTRACT: sort:list, insert:list

;PURPOSE: Function sort takes a list of numbers as input and produces the list with the numbers sorted in ascending order as output.

;CODE:
(define (insert a li)
(if (null? li) (list a)
	(if(< a (car li)) (cons a li) (cons (car li) (insert a (cdr li)))
	)
)
)  

(define (sort li)
(if (null? li) '() (insert (car li) (sort (cdr li)))
)
)


;TEST CASES AND EXPECTED OUTPUTS
; 1. check  (sort '(4 7 9 1 3 0))  -->'(0 1 3 4 7 9)
; 2. check  (sort '(2 4 1 -2 -5))  -->'(-5 -2 1 2 4)
; 3. check  (sort '(3 3 7 8 1 -9))  -->'(-9 1 3 3 7 8)





  ;(Q6.2)(EOPL 3rd ed. Text. Pages 28. Exercises 1.30)
;Develop a function predicatesort that takes a list of numbers and a predicate as input and outputs a list with the list elemnts sorted in the given predicate (< or >) order.

;CONTRACT: predicatesort:list, insert:list

;PURPOSE: Function predicatesort takes a list of numbers and a predicate as an input and produces the list with the numbers sorted in an order as output based on the predicate.

;CODE:
(define (newinsert k a li)
(if (null? li) (list a)
        (if (k a (car li)) (cons a li)
                        (cons (car li) (insert k a (cdr li)))
        )
)
)

(define (predicatesort k li)
(if (null? li) '()
        (newinsert k (car li) (predicatesort k (cdr li)))
)
)

;TEST CASES AND EXPECTED OUTPUTS
; 1. check  (predicatesort < '(3 8 91 0 2))  -->'(0 2 3 8 91)
; 2. check  (predicatesort > '(3 8 91 0 2))  -->'(91 8 3 2 0)
; 3. check  (predicatesort > '(-2 5 1 7 0))  -->'(7 5 1 0 -2)







  ;(Q6.3)(EOPL 3rd ed. Text. Pages 28. Exercises 1.34)
;Develop a procedure path that takes input as an integer and a binary search tree best containing the integer and returns a list of lefts and rights showing how to find the integer and returns empty list if integer is found at the root.

;CONTRACT: path:list

;PURPOSE: To find out the integer in a list containg binary search tree bst and show the path to find that integer in it.

;CODE
(define (path li a)
(cond ((equal? a (car li)) '())
	((member? (cadr li) a) (cons 'left (path (cadr li) a)))
	(else (cons 'right (path (caddr li) a)))
)
)

;TEST CASES AND EXPECTED OUTPUT
; check   (path '(10 (10 () (17 () ())) ())  17)  -->'(left right)
; check    (path '(7 (4 () (12 () () )) (64 (30 (5 () ()) ()) (6 () () )))  5)  -->'(right left left)   

  
