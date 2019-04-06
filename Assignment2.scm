;Shandilya, Anshul Kumar
;Instructor, Scott Gordon
;CSc 135, #2
;Programming Assignment #2 (Scheme Programming)

;------------------------------------------------------------------------
;Problem A
;Function appentInt to append two positive integers

(define (appendInt X Y)
	(+ (* (countDigit Y) X) Y)	;Takes 10^(digit nos), then multiplies the other number and adds the original number to it
)

;Counts the digits of given number and returns 10 to the power number of digits
(define (countDigit Y)
	(if (< Y 10)		;if Y is less than 10, ie its come down to single digit
		10
		(* 10 (countDigit (/ Y 10)))
	)
		
)
	
;------------------------------------------------------------------------
;Problem B
;"listsMax" - takes two lists of integers of the same length, and returns a list
;   where the elements are the max value of the two elements in the same position
;    of each list.

(define (listsMax l1 l2)
	(if (null? l1)
		'()						;Return null list if current list is empty
		(cons (compare (car l1) (car l2)) (listsMax (cdr l1) (cdr l2)))	;Compare each element individually and put winner in the list
	)
)

(define (compare X Y)
	(cond 
		((> X Y) X)	;If x > y
		((< X Y) Y)	;If X < Y
		(else X)	;else equal, return any element
	)
)

;------------------------------------------------------------------------
;Problem C
;"cycler" - takes a list L and an integer N, and returns an integer which is the
;    result of "cycling" N through the list L.  "Cycling" is done by successively
;    applying the operations addition (+), subtraction (-) and multiplication (*) using
;    each of the integers in L until they are exhausted.  If the length of L is longer
;    than 3, then the +, -, and * operations repeat.





;------------------------------------------------------------------------
;Problem D 
;"functionWinner" - takes as input a Boolean function F, and two lists L and M.
;   It returns:
;       the number 1 if list L is the "winner"
;       the number 2 if list M is the "winner"
;       the number 0 if the two lists are tied
;
;    The "winning" list is the one for which the function F has the most "true" answers
;    among the values in the two lists.

(define (functionWinner f l1 l2)
	(cond ((= (countTrue l1 f)(countTrue l2 f)) 0)		;Uses the countTrue function of problem E. return 0 if both equal
		((< (countTrue l1 f)(countTrue l2 f)) 2)		;return 2 if right list wins
		(else 1)										;return 1 if left list wins
	)
	
)

;------------------------------------------------------------------------
;Problem E
;"countTrue" - takes a list L (which may include sublists) and a Boolean function F,
;    and returns a count of the number of elements in the list for which F returns true.
;    Elements nested in sublists are included.  You only need to handle functions F defined
;    for scalar inputs. 

	
(define (countTrue lst f)
	(if (and (null? lst) (list? lst))	;return 0 if current list is null and is a list
		0
		(+ 0 (countList lst f))			;else call countList
	)
)


(define (countList lst f)
	(if (list? (car lst))				;If current element in the list is a list (sublist)
		(+ 0 (countTrue (car lst) f))	;Call countTrue with the sub list as a parameter
		(if (f (car lst))				;else check with function f, the first element of the list
			(+ 1 (countTrue (cdr lst) f))	;if function returns true, then recursively call countTrue and add 1
			(+ 0 (countTrue (cdr lst) f))	; else call without adding
		)
	
	)
)
	
;------------------------------------------------------------------------
;Problem F
;"badNumberRemover" - takes a list L that contains only numbers.
;    It then builds a "remover" function based on L.
;
;    The resulting "remover" function can take a list of numbers, and return an identical
;    list except that the "bad" numbers have been removed.  The "bad" numbers are any of
;    those that were specified in the original list L.
;    For example, if badNumberRemover was called as follows:
;
;         (define B (badNumberRemover '(42 13 4)));
;
;    then the produced function B would behave as follows:;
;
;         (B '(83 2 17 42 9 100 42 13 17 29))  would return (83 2 17 9 100 17 29);
;
;    Your task is to write badNumberRemover, not P.
;    You will probably find it useful to write one or more utility functions.
;    Of course, badNumberRemover should work for any reasonable input list L,
;    not just the one shown above.

(define (badNumberRemover lst)
	(lambda (L) (aux L lst))
)

(define (aux L lst)	;defined auxillary function
	(if (null? lst)
		L			;if bad list is null, return main list
		(aux (removeAll (car lst) L) (cdr lst))	;else call removeAll to remove all occurances of top element of bad list, then call itself with cdt lst
	)
)

(define removeAll	;function to remove all occurances of given element from list
    (lambda (x ls)
        (if (equal? (removeF x ls) ls)   	;call removeF, if lists equal, then all occurances removed
            ls                              
            (removeAll x (removeF x ls))))	 ;else call removeF function to one by one remove first occurance of element
)

(define removeF
    (lambda (x ls)
        (if (null? ls)                   ;return empty list if list is null
            '()                           
            (if (equal? x (car ls))       ;else check for equality
                (cdr ls)                  ;if equal, return cdr ls
                (cons (car ls) (removeF x (cdr ls))))))		;else call function again to remove first occurance	
)			
			
;--------------------------------------------------------------------------



;;test boolean functions to test the above problems

(define (isEven X)
	(if (= (modulo x 2) 0)
		#t
		#f
	)	
)

(define (isOdd X)
	(if (not (isEven X))
		#t
		#f
	)
)