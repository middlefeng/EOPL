

(define (plus x y)
	(if (is-zero? x)
		y
		(successor (plus (predecessor x) y)) ))



; Procedural Representation of Enviornment

(define empty-env
	(lambda () 
		(lambda (search-var)
			(display "can not find") )))


(define (extend-env saved-var saved-val saved-env)
	(lambda (search-var)
		(if (eq? search-var saved-var)
			saved-val
			(saved-env search-var) )))


(define (apply-env env search-var)
	(env search-var))




; Procedural Representation of Stack


(define empty-stack
	(lambda () 
		(lambda ()
			(display "stack empty!") )))


(define (push stack item)
	(lambda ()
		(list item stack)))


(define (pop stack)
	(stack))

