#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice9-format:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

;;;#############################################################
;;;#############################################################
;;;### expects 2 julian days (plain numbers)
;;;### differences between 2 julian days is in days (or a fraction of a day)
(define-public (julian-day-difference-to-string dend dstart)
  (define (local-process-sub-day day-fraction)
    (let ((nsecs (* day-fraction 24.0 60.0 60.0))
	  (nmins (truncate (* day-fraction 24.0 60.0)))
	  (nhours (truncate (* day-fraction 24.0))))
      (let ((nminutes
	     (* 0.0010 (truncate (* 1000.0 (- nmins (* nhours 60.0)))))))
	(let ((nseconds
	       (* 0.0010
		  (truncate
		   (* 1000.0 (- nsecs (+ (* nhours 60.0 60.0) (* nminutes 60.0))))))))
	  (begin
	    (if (<= nhours 0.0)
		(if (<= nminutes 0.0)
		    (format #f "~a seconds" nsecs)
		    (format #f "~a minutes, ~a seconds" nminutes nseconds))
		(if (<= nminutes 0.0)
		    (format #f "~a hours, ~a seconds" nhours nseconds)
		    (format #f "~a hours, ~a minutes, ~a seconds" nhours nminutes nseconds))
		))))))
  (if (and (number? dend) (number? dstart))
      (begin
	(let ((jd-diff (exact->inexact (- dend dstart))))
	  (if (< jd-diff 1.0)
	      (begin
		(let ((tstring (local-process-sub-day jd-diff)))
		  tstring
		  ))
	      (begin
		(let ((ndays (truncate jd-diff)))
		  (let ((dfract-diff (- jd-diff ndays)))
		    (let ((tstring (local-process-sub-day dfract-diff)))
		      (let ((ttstring (format #f "~a days, ~a" ndays tstring)))
			ttstring
			))))))))
      #f))

;;;#############################################################
;;;#############################################################
(define-public (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
;;; spiral anti-clockwise
(define (make-reverse-spiral-array! this-array max-row max-col)
  (let ((center-row (euclidean/ max-row 2))
	(center-col (euclidean/ max-col 2))
	(arm-length 1)
	(arm-count 0)
	(current-direction 1)
	(num-counter 1)
	(end-flag #f))
    (let ((this-row center-row)
	  (this-col center-col))
      (begin
	(array-set! this-array num-counter this-row this-col)
	(set! num-counter (1+ num-counter))

	(while
	 (and (equal? end-flag #f)
	      (<= arm-length max-row)
	      (<= arm-length max-col))
	 (begin
	   (cond
	    ((or (> this-row max-row)
		 (> this-col max-col))
	     (begin
	       (set! end-flag #t)))
	    ((= current-direction 1)
	     (begin
	        ;;; then we are incrementing columns
	       (do ((ii 0 (+ ii 1)))
		   ((or (>= ii arm-length)
			(>= (+ this-col 1) max-col)))
		 (begin
		   (let ((next-col (+ this-col 1)))
		     (begin
		       (if (zero? (array-ref this-array this-row next-col))
			   (begin
			     (array-set! this-array num-counter this-row next-col)
			     (set! num-counter (1+ num-counter))
			     (set! this-col next-col)
			     ))))))
	       (set! current-direction -2)
	       (set! arm-count (1+ arm-count))
	       (if (>= arm-count 2)
		   (begin
		     (set! arm-count 0)
		     (set! arm-length (1+ arm-length))
		     ))
	       ))
	    ((= current-direction -1)
	     (begin
	        ;;; then we are decrementing columns
	       (do ((ii 0 (+ ii 1)))
		   ((or (>= ii arm-length)
			(< this-col 0)))
		 (begin
		   (let ((next-col (- this-col 1)))
		     (begin
		       (if (zero? (array-ref this-array this-row next-col))
			   (begin
			     (array-set! this-array num-counter this-row next-col)
			     (set! num-counter (1+ num-counter))
			     (set! this-col next-col))
			   )))))
	       (set! current-direction +2)
	       (set! arm-count (1+ arm-count))
	       (if (>= arm-count 2)
		   (begin
		     (set! arm-count 0)
		     (set! arm-length (1+ arm-length))
		     ))
	       ))
	    ((= current-direction 2)
	     (begin
	        ;;; then we are incrementing rows
	       (do ((ii 0 (+ ii 1)))
		   ((or (>= ii arm-length)
			(>= (+ this-row 1) max-row)))
		 (begin
		   (let ((next-row (+ this-row 1)))
		     (begin
		       (if (zero? (array-ref this-array next-row this-col))
			   (begin
			     (array-set! this-array num-counter next-row this-col)
			     (set! num-counter (1+ num-counter))
			     (set! this-row next-row))
			   )))))

	       (set! current-direction 1)
	       (set! arm-count (1+ arm-count))
	       (if (>= arm-count 2)
		   (begin
		     (set! arm-count 0)
		     (set! arm-length (1+ arm-length))
		     ))
	       ))
	    ((= current-direction -2)
	     (begin
	        ;;; then we are decrementing rows
	       (do ((ii 0 (+ ii 1)))
		   ((or (>= ii arm-length)
			(< this-row 0)))
		 (begin
		   (let ((next-row (- this-row 1)))
		     (begin
		       (if (zero? (array-ref this-array next-row this-col))
			   (begin
			     (array-set! this-array num-counter next-row this-col)
			     (set! num-counter (1+ num-counter))
			     (set! this-row next-row))
			   )))))

	       (set! current-direction -1)
	       (set! arm-count (1+ arm-count))
	       (if (>= arm-count 2)
		   (begin
		     (set! arm-count 0)
		     (set! arm-length (1+ arm-length))
		     ))
	       ))
	    (else
	     (begin
	       (display (format #f "make-spiral-array! error: invalid current-direction=~a~%" current-direction))
	       (quit)
	       ))
	    )))))
    ))

;;;#############################################################
;;;#############################################################
(define (test-make-reverse-spiral-array-3-1)
  (let ((sub-name "test-make-reverse-spiral-array-3-1")
	(test-array (make-array 0 3 3))
	(shouldbe-list-list (list
			     (list 5 4 3)
			     (list 6 1 2)
			     (list 7 8 9)))
	(max-row 3)
	(max-col 3)
	(test-label-index 0))
    (let ((shouldbe-array (list->array 2 shouldbe-list-list)))
      (begin
	(make-reverse-spiral-array! test-array max-row max-col)

	(do ((ii-row 0 (+ ii-row 1)))
	    ((>= ii-row max-row))
	  (begin
	    (do ((ii-col 0 (+ ii-col 1)))
		((>= ii-col max-col))
	      (begin
		(let ((result-num (array-ref test-array ii-row ii-col))
		      (shouldbe-num (array-ref shouldbe-array ii-row ii-col)))
		  (begin
		    (if (not (equal? shouldbe-num result-num))
			(begin
			  (display (format #f "~a : error (~a) : shouldbe array=~a, test-array=~a, inccrrect at row=~a, col=~a, shouldbe=~a, result=~a~%"
					   sub-name test-label-index shouldbe-array test-array
					   ii-row ii-col shouldbe-num result-num))
			  (quit)
			  ))
		    ))
		))
	    ))
	))))

;;;#############################################################
;;;#############################################################
(define (test-make-reverse-spiral-array-5-1)
  (let ((sub-name "test-make-reverse-spiral-array-5-1")
	(test-array (make-array 0 5 5))
	(shouldbe-list-list (list
			     (list 17 16 15 14 13)
			     (list 18  5  4  3 12)
			     (list 19  6  1  2 11)
			     (list 20  7  8  9 10)
			     (list 21 22 23 24 25)
			     ))
	(max-row 5)
	(max-col 5)
	(test-label-index 0))
    (let ((shouldbe-array (list->array 2 shouldbe-list-list)))
      (begin
	(make-reverse-spiral-array! test-array max-row max-col)

	(do ((ii-row 0 (+ ii-row 1)))
	    ((>= ii-row max-row))
	  (begin
	    (do ((ii-col 0 (+ ii-col 1)))
		((>= ii-col max-col))
	      (begin
		(let ((result-num (array-ref test-array ii-row ii-col))
		      (shouldbe-num (array-ref shouldbe-array ii-row ii-col)))
		  (begin
		    (if (not (equal? shouldbe-num result-num))
			(begin
			  (display (format #f "~a : error (~a) : shouldbe array=~a, test-array=~a, inccrrect at row=~a, col=~a, shouldbe=~a, result=~a~%"
					   sub-name test-label-index shouldbe-array test-array
					   ii-row ii-col shouldbe-num result-num))
			  (quit)
			  ))
		    ))
		))
	    ))
	))))

;;;#############################################################
;;;#############################################################
(define (prime? nn)
  (define (smallest-divisor nn test-divisor max-divisor)
    (cond
     ((> test-divisor max-divisor) nn)
     ((zero? (modulo nn test-divisor)) test-divisor)
     (else
      (smallest-divisor nn (+ test-divisor 2) max-divisor)
      )))
  (begin
    (cond
     ((<= nn 1) #f)
     ((= nn 2) #t)
     ((zero? (modulo nn 2)) #f)
     (else
      (let ((max-divisor
	     (+ (exact-integer-sqrt nn) 1)))
	(= nn (smallest-divisor nn 3 max-divisor)))
      ))))

;;;#############################################################
;;;#############################################################
(define (test-prime-1)
  (let ((sub-name "test-prime-1")
	(test-list
	 (list
	  (list 0 #f) (list 1 #f) (list 2 #t) (list 3 #t)
	  (list 4 #f) (list 5 #t) (list 6 #f) (list 7 #t)
	  (list 8 #f) (list 9 #f) (list 10 #f) (list 11 #t)
	  (list 12 #f) (list 13 #t) (list 14 #f) (list 15 #f)
	  (list 16 #f) (list 17 #t) (list 18 #f) (list 19 #t)
	  (list 20 #f) (list 21 #f) (list 22 #f) (list 23 #t)
	  (list 24 #f) (list 25 #f) (list 26 #f) (list 27 #f)
	  (list 28 #f) (list 29 #t) (list 30 #f) (list 31 #t)
	  (list 32 #f) (list 33 #f) (list 34 #f) (list 35 #f)
	  (list 36 #f) (list 37 #t) (list 38 #f) (list 39 #f)
	  (list 40 #f) (list 41 #t) (list 42 #f) (list 43 #t)
	  (list 44 #f) (list 45 #f) (list 46 #f) (list 47 #t)
	  (list 48 #f) (list 49 #f) (list 50 #f) (list 51 #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (prime? test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, prime? shouldbe=~a, result=~a~%"
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (display-array this-array max-row max-col)
  (cond
   ((not (array? this-array))
    (begin
      (display (format #f "display-array error: expecting array, instead received ~a~%" this-array))
      (quit)))
   (else
    (begin
      (do ((ii-row 0 (+ ii-row 1)))
	  ((>= ii-row max-row))
	(begin
	  (do ((ii-col 0 (+ ii-col 1)))
	      ((>= ii-col max-col))
	    (begin
	      (display
	       (ice9-format:format #f "~4:d "
				   (array-ref this-array ii-row ii-col)))
	      ))
	  (newline)
	  ))
      ))))

;;;#############################################################
;;;#############################################################
(define (main-loop end-num floor-percent debug-flag)
  (let ((break-flag #f)
	(total 0)
	(prime-count 0)
	(side-length 0)
	(ratio 0.0))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((or (> ii end-num)
	       (equal? break-flag #t)))
	(begin
	  ;;; for-each square ii
	  (let ((ii-side-length (+ (* 2 ii) 1))
		(ii-diagonal-count (+ (* 4 ii) 1))
		(ii-prime-count 0))
	    (let ((se-value (* ii-side-length ii-side-length))
		  (delta (* 2 ii)))
	      (let ((sw-value (- se-value delta)))
		(let ((nw-value (- sw-value delta)))
		  (let ((ne-value (- nw-value delta)))
		    (begin
		      (if (prime? sw-value)
			  (begin
			    (set! ii-prime-count (1+ ii-prime-count))
			    ))
		      (if (prime? nw-value)
			  (begin
			    (set! ii-prime-count (1+ ii-prime-count))
			    ))
		      (if (prime? ne-value)
			  (begin
			    (set! ii-prime-count (1+ ii-prime-count))
			    ))
		      (set! total ii-diagonal-count)
		      (set! side-length ii-side-length)
		      (set! prime-count (+ prime-count ii-prime-count))
		      (set! ratio (* 0.00010
				     (truncate
				      (* 10000.0
					 (exact->inexact (/ prime-count total))))))

		      (if (equal? debug-flag #t)
			  (begin
			    (display (format #f "for ~:d x ~:d : ~:d / ~:d = ~a%~%"
					     side-length side-length prime-count total
					     (* 100.0 ratio)))
			    (force-output)
			    ))

		      (if (< ratio floor-percent)
			  (begin
			    (set! break-flag #t)
			    (display (ice9-format:format
				      #f "square spiral of dimensions ~:d x ~:d has a ratio of primes along both diagonals less than ~a%, ratio = ~a% = ~:d / ~:d~%"
				      side-length side-length
				      (* 100.0 floor-percent) (* 100.0 ratio)
				      prime-count total))
			    (display (ice9-format:format #f "for square dimensions between ~:d and ~:d~%" 1 end-num))
			    (force-output)
			    ))
		      ))
		  ))
	      ))
	  ))

      (if (equal? break-flag #f)
	  (begin
	    (display (ice9-format:format #f "no square spiral with ratio less than ~a found between dimensions ~:d and ~:d~%" floor-percent 3 (+ (* 2 end-num) 1)))
	    (force-output)
	    ))
      )))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify code
(define-syntax run-test
  (syntax-rules ()
    ((run-test test-function counter)
     (begin
       (test-function)
       (set! counter (1+ counter))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax time-code
  (syntax-rules ()
    ((time-code body)
     (begin
       (let ((start-jday (srfi-19:current-julian-day)))
	 (begin
	   body

	   (let ((end-jday (srfi-19:current-julian-day)))
	     (begin
	       (display (format #f "elapsed time = ~a : ~a~%"
				(julian-day-difference-to-string end-jday start-jday)
				(date-time-to-string (srfi-19:current-date))))
	       (force-output)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main args)
  (begin
    (display (format #f "Problem 058 - Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.~%"))
    (newline)
    (display (format #f " 37 36 35 34 33 32 31~%"))
    (display (format #f " 38 17 16 15 14 13 30~%"))
    (display (format #f " 39 18  5  4  3 12 29~%"))
    (display (format #f " 40 19  6  1  2 11 28~%"))
    (display (format #f " 41 20  7  8  9 10 27~%"))
    (display (format #f " 42 21 22 23 24 25 26~%"))
    (display (format #f " 43 44 45 46 47 48 49~%"))
    (newline)
    (display (format #f "It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13  62%.~%"))
    (newline)
    (display (format #f "If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?~%"))
    (newline)
    (display (format #f "The solution was found at http://blog.mycila.com/2009/08/project-euler-problem-58.html~%"))
    (newline)
    (display (format #f "Instead of generating the spiral manually, each diagonal element can be calculated.  For block i, which has a side length = (2i+1), the south-east diagonal (SE), has elements (2*i+1)^2 (always a square, since the i'th square is a square).  The southwest diagonal has elements SE(i) - 2i, the northwest diagonal has elements SE(i)-4i, and the northeast diagonal elements are SE(i)-6i.  For square i, the total number on each diagonal is 4i+1.~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((end-num 10)
	  (floor-percent 0.10)
	  (debug-flag #t))
      (begin
	(main-loop end-num floor-percent debug-flag)
	))

    (newline)
    (force-output)

    (let ((end-num 100000)
	  (floor-percent 0.10)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop end-num floor-percent debug-flag)
	   ))
	))

    (newline)
    ))
