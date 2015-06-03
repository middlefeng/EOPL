

(define (zero? n)
	(eq? n 0))

(define (>= a b)
	(or (eq? a b) (> a b)))

(define #t true)
(define #f false)


(define (eqv? a b)
	(eq? a b))

(define (append lst x)
	(if (null? lst)
		(list x)
		(cons (car lst) (append (cdr lst) x))))







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





(define (duple n x)
	(cond ((eq? 0 n) '())
		  ((eq? 1 n) (list x))
		  (else (cons x (duple (- n 1) x)))))


(define (invert-elements lst)
	(if (null? lst)
		lst
		(append (invert-elements (cdr lst)) (car lst))))

(define (invert lst)
	(if (null? lst)
		lst
		(cons (invert-elements (car lst)) (invert (cdr lst)))))



(define (swapper-sym s1 s2 s)
	(cond ((eq? s1 s) s2)
		  ((eq? s2 s) s1)
		  (else s)))

(define (swapper s1 s2 lst)
	(if (null? lst)
		lst
		(if (atom? (car lst))
			(cons (swapper-sym s1 s2 (car lst))
			  	  (swapper s1 s2 (cdr lst)))
			(cons (swapper s1 s2 (car lst))
				  (swapper s1 s2 (cdr lst))) ) ) )




		















