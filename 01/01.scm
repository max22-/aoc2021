(define (read-number port)
  (let ((l (read-line port)))
    (if (eof-object? l)
	'()
	(cons (string->number l) (read-number port)))))

(define (zip a b)
  (if (or (null? a) (null? b))
      '()
      (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

(define (solution input)
  (apply +
	 (map (lambda (e)
		(if (< (car e) (cadr e))
		    1
		    0))
	      (zip input (cdr input)))))

(display
 (solution (call-with-input-file "input.txt" read-number)))
