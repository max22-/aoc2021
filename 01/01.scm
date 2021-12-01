(define (read-number port)
  (let ((l (read-line port)))
    (if (eof-object? l)
	'()
	(cons (string->number l) (read-number port)))))

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
(display
 (solution1 (call-with-input-file "input.txt" read-number)))
(newline)

(define (zipWith3 f a b c)
  (if (or (null? a) (null? b) (null? c))
      '()
      (cons (f (car a) (car b) (car c))
	    (zipWith3 f (cdr a) (cdr b) (cdr c)))))

(define example (call-with-input-file "example.txt" read-number))

(define (apply-window input)
  (zipWith3 + input (cdr input) (cddr input)))

(define (solution2 input)
  (solution1 (apply-window input)))

(display "Solution2: ")
(display (solution2 (call-with-input-file "input.txt" read-number)))
(newline)
