#lang racket
(provide (all-defined-out))


; From TA amit slide
(define LAMBDA (string->symbol "\u03BB"))



; From TA Xinyu hint code
(define (lambda? x)
  (member x '(lambda λ)))



; Structure and code from TA Xinyu hint code
(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        
	; if one of them is not list - which means that not function
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]
         
	; and below here it is your work to figure out how to judge every case
	[#t (check-list x y)]
))



; Check the function (list) from the beginning
(define (check-list x y)
  (cond [(not (equal? (length x) (length y)))
	 (list 'if '% x y)]

	[(and (equal? (car x) 'quote) (equal? (car y) 'quote))
	 (list 'if '% x y)]

	[(and (lambda? (car x)) (lambda? (car y)))
	 (check-lambda x y)]

	[(or (lambda? (car x)) (lambda? (car y)))
	 (list 'if '% x y)]

	[(and (equal? (car x) 'if) (equal? (car y) 'if))
	 (cons 'if (check-rest (cdr x) (cdr y)))]

	[(or (equal? (car x) 'if) (equal? (car y) 'if))
	 (list 'if '% x y)]

	[(equal? (car x) (car y))
	 (cons (car x) (check-rest (cdr x) (cdr y)))]

	[(and (list? (car x)) (list? (car y)))
         (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]

	[(not (equal? (car x) (car y)))
	 (cons (list 'if '% (car x) (car y)) (check-rest (cdr x) (cdr y)))]

	[#t (list 'if '% x y)]
))



; Check the function (list) from the second argument, case where first argument is not lambda
(define (check-rest x y)
  (cond [(null? x) x]

	[(equal? (car x) (car y))
	 (cons (car x) (check-rest (cdr x) (cdr y)))]

	[(and (boolean? (car x)) (boolean? (car y))) 
	 (if (car x) 
	   (cons '% (check-rest (cdr x) (cdr y))) 
	   (cons '(not %) (check-rest (cdr x) (cdr y))))]

        [(and (list? (car x)) (list? (car y)))
         (cons (expr-compare (car x) (car y)) (check-rest (cdr x) (cdr y)))]

        [(not (equal? (car x) (car y)))
	 (cons (list 'if '% (car x) (car y)) (check-rest (cdr x) (cdr y)))]
))



; Check the function from the beginning, case where first argument is lambda
(define (check-lambda x y)
  (let ([correct-lambda (get-lambda-symbol (car x) (car y))]
	[x-vari (cadr x)][y-vari (cadr y)][x-func (caddr x)][y-func (caddr y)])
    (cond [(not (equal? (length x-vari) (length y-vari)))
	   (list 'if '% x y)]

	  [(and (not (list? x-func)) (not (list? y-func)))
	   (let ([combined-var (combine-lambda-variables x-vari y-vari)])
		(let ([x-combined-func-1 (combine-lambda-function-1 x-func x-vari combined-var)]
		      [y-combined-func-1 (combine-lambda-function-1 y-func y-vari combined-var)])
	   (cons correct-lambda (list combined-var (expr-compare x-combined-func-1 y-combined-func-1)))))]


	  [(not (equal? (length x-func) (length y-func)))
	   (list 'if '% x y)]

	  [#t (let ([combined-var (combine-lambda-variables x-vari y-vari)])
		(let ([x-combined-func (combine-lambda-function x-func x-vari combined-var 1)]
		      [y-combined-func (combine-lambda-function y-func y-vari combined-var 1)])
	   (cons correct-lambda (list combined-var (expr-compare x-combined-func y-combined-func)))))]
    )
  )
)



; Combine the variables in lambda expressions in the form of a!b
(define (combine-lambda-variables x y)
  (cond [(null? x) x]

	[(equal? (car x) (car y))
	 (cons (car x) (combine-lambda-variables (cdr x) (cdr y)))]

	[(not (equal? (car x) (car y)))
	 (cons (combine-help (car x) (car y)) (combine-lambda-variables (cdr x) (cdr y)))]
  )
)



; Helper function that helps the above function to have the a!b structure
(define (combine-help x y)
  (string->symbol (string-append (symbol->string x) 
				 "!" 
				 (symbol->string y)))
)



; Determine whether the symbol should be lambda or λ
(define (get-lambda-symbol x y)
  (cond [(and (equal? x 'lambda) (equal? y 'lambda))
	 'lambda]
	[#t 'λ]
  )
)



; Convert the variable in lambda function according to the change in the variable section
(define (combine-lambda-function func ori-var comb-var cont)
  (cond [(null? func) func]

	[(and (and (= cont 1) (list? (car func))) (not (lambda? (caar func))))
	 (cons (combine-lambda-function (car func) ori-var comb-var 0) 
	       (combine-lambda-function (cdr func) ori-var comb-var 0))]

	[(and (and (= cont 1) (list? (car func))) (lambda? (caar func)))
	 (let ([new-comb-var (update-comb-var (cadar func) ori-var comb-var)])
	 (cons (combine-lambda-function (car func) ori-var new-comb-var 0) 
	       (combine-lambda-function (cdr func) ori-var comb-var 1)))]

        [(= -1 (get-position (car func) ori-var 1))
 	 (cons (car func) (combine-lambda-function (cdr func) ori-var comb-var cont))]

        [#t (let ([index (get-position (car func) ori-var 1)])
	 (cons (get-item index comb-var) (combine-lambda-function (cdr func) ori-var comb-var cont)))]
  )   
)



; Update the mapping of variation and combined variable to a list of new variables
(define (update-comb-var new-var ori-var ori-comb-var)
  (cond [(null? new-var) ori-comb-var]

	[(> (get-position (car new-var) ori-var 1) -1)
	 (let ([updated-comb-var (set-item (get-position (car new-var) ori-var 1) (car new-var) ori-comb-var)])
	   (update-comb-var (cdr new-var) ori-var updated-comb-var))]

	[#t (update-comb-var (cdr new-var) ori-var ori-comb-var)]
  )
)



; Convert the variable in lambda function according to the change in the variable section, function is of form 'a
(define (combine-lambda-function-1 func ori-var comb-var)
  (cond [(= -1 (get-position func ori-var 1)) func]

        [#t (let ([index (get-position func ori-var 1)])
	 (get-item index comb-var))]
  )
)



; get the position of x in a list, -1 if doesnt exist
(define (get-position x var-list i)
  (cond [(null? var-list) -1]

	[(equal? x (car var-list))
	 i]

	[(not (equal? x (car var-list)))
	 (get-position x (cdr var-list) (+ 1 i))]
  )
)



; get the member at index i from a list
(define (get-item index var-list)
  (cond [(= 1 index)
	 (car var-list)]

	[#t (get-item (- index 1) (cdr var-list))]
  )
)



; set the member at index i to be x in a list
(define (set-item index new-var var-list)
  (cond [(= 1 index)
	 (cons new-var (cdr var-list))]

	[#t (cons (car var-list) (set-item (- index 1) new-var (cdr var-list)))]
  )
)



; Taken from TA Xinyu hint code
; compare and see if the (expr-compare x y) result is the same with x when % = #t
;                                                 and the same with y when % = #f
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))



; Referred to TA Xinyu hint code
; You need to cover all grammars including:
;     constant literals, variables, procedure calls, quote, lambda, if
(define test-expr-x
  '(1 'a (a b) (cons a lambda)
    (+ 1 2) (quote x y) (if 'a 1 4)
    (if 'a 3 5) (rambda (a b c) c)
    (λ (a b c) a)
   )
)



(define test-expr-y
  '(2 'b (a c) (cons a happy)
    (+ 3 5) (quote y z) (quoth 'a 1 4)
    (if 'a 3 4) (lambda (a b c) c)
    (λ (x y z) x)
   )
)


