(define (read-input)
  (let ((l (read-line)))
    (if (eof-object? l)
	'()
	(cons (parse l) (read-input)))))

(define (parse l)
  (map
   (lambda (c)
     (cond ((char=? c #\0) 0)
	   ((char=? c #\1) 1)
	   (else (begin (error "Invalid input") (quit)))))
   (string->list l)))

(define input (read-input))

(define (zipWith f a b)
  (if (or (null? a) (null? b))
      '()
      (cons (f (car a) (car b))
	    (zipWith f (cdr a) (cdr b)))))

(define (decode base x)
  (fold-left (lambda (a b) (+ (* base a) b)) 0 x))

(define (gamma acc)
  (decode 2
	  (map (lambda (x) (if (< 0 x) 1 0)) acc)))

(define (epsilon acc)
  (decode 2
	  (map (lambda (x) (if (> 0 x) 1 0)) acc)))

(define (repeat n x)
  (if (> n 0)
      (cons x (repeat (- n 1) x))
      '()))

(define accumulator
  (fold-left
   (lambda (a b)
     (begin
       ;(format #t "a: ~A" a)
       ;(newline)
       ;(format #t "b: ~A" b)
       ;(newline)
       (zipWith +
		(map (lambda (x) (- (* 2 x) 1)) b)
		a)))
   (repeat (length (car input)) 0)
   input))

(display "Accumulator: ")
(display accumulator)
(newline)

(display "Gamma: ")
(display (gamma accumulator))
(newline)

(display "Espilon: ")
(display (epsilon accumulator))
(newline)

(display "Solution 1: ")
(display (* (gamma accumulator) (epsilon accumulator)))
(newline)

(define (accumulate l column)
  (apply +
	 (map
	  (lambda (x)
	    (- (* 2 (list-ref x column)) 1))
	  l)))

(define (keep p column l)
  (begin
    ;(format #t "(keep p ~A ~A)\n" column l)
    (cond ((<= (length l) 1) l)
	  ((>= column (length (car l))) l)
	  (else  
	   (let* ((acc (accumulate l column))
		  (bit (if (p acc) 1 0)))
	     (keep p
		   (+ column 1)
		   (filter (lambda (x) (= bit (list-ref x column))) l)))))))


(define oxygen
  (decode 2 (car (keep (lambda (acc) (>= acc 0)) 0 input))))

(define CO2
  (decode 2 (car (keep (lambda (acc) (< acc 0)) 0 input))))


(format #t "Oxygen: ~A\n" oxygen)
(format #t "CO2: ~A\n" CO2)
(format #t "Solution 2: ~A\n" (* oxygen CO2))

