(define (repeat func numtimes)
		(cond
			((= numtimes 1) func)
			(else func (repeat func (- numtimes 1)))
		)
	)


(setpos )