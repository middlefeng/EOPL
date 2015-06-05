

(define (plus x y)
	(if (is-zero? x)
		y
		(successor (plus (predecessor x) y)) ))


