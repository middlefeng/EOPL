

(define (zero? n)
	(eq? n 0))

(define (>= a b)
	(or (eq? a b) (> a b)))

(define #t true)
(define #f false)


(define (eqv? a b)
	(eq? a b))







(define in-S?
    (lambda (n)
		(if (zero? n) #t
			(if (>= (- n 3) 0)
				(begin (in-S? (- n 3)))
				(begin #f)))))


(define nth-element
	(lambda (lst n)
		(if (null? lst)
			(display 'list too short')
			(if (zero? n)
				(car lst)
				(nth-element (cdr lst) (- n 1)) ) ) ) )



(define remove-first
    (lambda (s los)
		(if (null? los)
			'()
			(if (eqv? (car los) s)
				(cdr los)
			 	(cons (car los) (remove-first s (cdr los)))))))


(define (occurs-free? sym exp)
	(cond ((atom? exp) (eqv? sym exp))
		  ((eqv? (car exp) 'lambda)
		  		(and (not eqv? sym (car (car (cdr exp))))
		  			 (occurs-free? sym (car (cdr (cdr exp))))))
		  (else (or (occurs-free? sym (car exp))
		  			(occurs-free? sym (car (cdr exp))))) ) )







(define subst
	(lambda (new old exp)
		(if (null? exp) '()
			(cons (subst-in-s-exp new old (car exp))
				  (subst new old (cdr exp)))) ) )


(define subst-in-s-exp
	(lambda (new old sexp)
		(if (atom? sexp)
			(if (eqv? old sexp) new sexp)
			(subst new old sexp)) ) )





(define number-elements-from
	(lambda (lst n)
		(if (null? lst) '()
						(cons (list n (car lst))
						 	  (number-elements-from (cdr lst)
						 	   						(+ 1 n)))) ) )


(define (list-sum lst)
	(if (null? lst) 0
					(+ (car lst)
					   (list-sum (cdr lst)))))












