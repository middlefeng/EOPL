

(define (zero? n)
	(eq? n 0))

(define (>= a b)
	(or (eq? a b) (> a b)))

(define #t true)
(define #f false)

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