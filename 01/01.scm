(define (read-numbers)
  (let ((l (read-line)))
    (if (eof-object? l)
	'()
	(cons (string->number l) (read-numbers)))))

(define numbers (read-numbers))

(define (zip a b)
  (if (or (null? a) (null? b))
      '()
      (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

(define (solution1 input)
  (apply +
	 (map (lambda (e)
		(if (< (car e) (cadr e))
		    1
		    0))
	      (zip input (cdr input)))))

(display "Solution 1: ")
(display (solution1 numbers))
(newline)

(define (zipWith3 f a b c)
  (if (or (null? a) (null? b) (null? c))
      '()
      (cons (f (car a) (car b) (car c))
	    (zipWith3 f (cdr a) (cdr b) (cdr c)))))

(define (apply-window input)
  (zipWith3 + input (cdr input) (cddr input)))

(define (solution2 input)
  (solution1 (apply-window input)))

(display "Solution2: ")
(display (solution2 numbers))
(newline)
