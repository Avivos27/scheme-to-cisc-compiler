(define (empty? lst)
	(eq? lst '()))
(define (rest-iter rest var)
	(if (empty? rest)
		#f
		(let* ((f-inst (car rest))
			  (vars-read (cadr f-inst))
			  (vars-writen-to (caddr f-inst)))

			  (cond 
			  	((member var vars-read) #f)
			  	((member var vars-writen-to) #t)
			  	(else (rest-iter (cdr rest) var))))))

(define (remove-inst? first rest)
	(let ((vars-writen-to (caddr first)))
			(if (empty? vars-writen-to)
				#t
				(andmap (lambda (var) (rest-iter rest var)) vars-writen-to))))




(define (build-true-list instuctions)
	(if (empty? instuctions)
		'()
		(let* ((first (car instuctions))
			    (rest (cdr instuctions))
		   	    (vars-writen-to (caddr first)))

		(cons (remove-inst? first rest) (build-true-list rest)))))
(define (filter-inst inst bool)
	(cond ((empty? inst) `())
		  ((not (car bool)) (cons (car inst) (filter-inst (cdr inst) (cdr bool))))
		  (else (filter-inst (cdr inst) (cdr bool)))
	)
)

(define (false? x)
 (eq? x #f)
)

(define (remww instuctions)
	(let* ((true-lst (build-true-list instuctions))
		 (updated-inst (filter-inst instuctions true-lst)))
	(if (andmap false? true-lst)
		updated-inst
	(remww updated-inst))))

