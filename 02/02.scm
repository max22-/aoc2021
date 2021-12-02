(define (parse l)
  (apply (lambda (s n) (list s (string->number n)))
       (string-split #\space l)))

(define (read-commands)
  (let ((l (read-line)))
    (if (eof-object? l)
	'()
	(cons (parse l) (read-commands)))))

(define (update-position l pos)
  (let ((horiz (car pos))
	(depth (cadr pos))
	(command (car l))
	(n (cadr l)))
    (cond
     ((string=? "forward" command) (list (+ horiz n) depth))
     ((string=? "down" command) (list horiz (+ depth n)))
     ((string=? "up" command) (list horiz (- depth n)))
     (else (begin
	     (error "Invalid input")
	     (quit))))))

(define (solution input)
  (apply *
	 (fold-right update-position '(0 0) input)))

(display (solution (read-commands)))
(newline)
