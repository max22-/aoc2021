(define (parse l)
  (apply (lambda (s n) (list s (string->number n)))
       (string-split #\space l)))

(define (read-commands)
  (let ((l (read-line)))
    (if (eof-object? l)
	'()
	(cons (parse l) (read-commands)))))

(define (update-state state l)
  (let ((horiz (car state))
	(depth (cadr state))
	(aim (caddr state))
	(command (car l))
	(n (cadr l)))
    (cond
     ((string=? "forward" command)
      (list (+ horiz n)
	    (+ depth (* aim n))
	    aim))
     ((string=? "down" command)
      (list horiz
	    depth
	    (+ aim n)))
     ((string=? "up" command)
      (list horiz
	    depth
	    (- aim n)))
     (else (begin
	     (error "Invalid input")
	     (quit))))))

(define (solution input)
  (let ((final-state (fold-left update-state '(0 0 0) input)))
    (* (car final-state) (cadr final-state))))

(display (solution (read-commands)))
(newline)
