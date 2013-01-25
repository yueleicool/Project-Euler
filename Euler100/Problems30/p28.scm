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
(define (make-spiral-array! this-array max-row max-col)
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
	       (set! current-direction 2)
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
	       (set! current-direction -2)
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

	       (set! current-direction -1)
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

	       (set! current-direction 1)
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
(define (test-make-spiral-array-3-1)
  (let ((sub-name "test-make-spiral-array-3-1")
	(test-array (make-array 0 3 3))
	(shouldbe-list-list (list
			     (list 7 8 9)
			     (list 6 1 2)
			     (list 5 4 3)))
	(max-row 3)
	(max-col 3))
    (let ((shouldbe-array (list->array 2 shouldbe-list-list)))
      (begin
	(make-spiral-array! test-array max-row max-col)

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
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-make-spiral-array-5-1)
  (let ((sub-name "test-make-spiral-array-5-1")
	(test-array (make-array 0 5 5))
	(shouldbe-list-list (list
			     (list 21 22 23 24 25)
			     (list 20  7  8  9 10)
			     (list 19  6  1  2 11)
			     (list 18  5  4  3 12)
			     (list 17 16 15 14 13)
			     ))
	(max-row 5)
	(max-col 5))
    (let ((shouldbe-array (list->array 2 shouldbe-list-list)))
      (begin
	(make-spiral-array! test-array max-row max-col)

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
	))
    ))

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
      ))
   ))

;;;#############################################################
;;;#############################################################
;;; assume square array
(define (diag-sum this-array max-row max-col)
  (let ((sum 0)
	(center-row (euclidean/ max-row 2))
	(center-col (euclidean/ max-col 2))
	(max-index (- max-row 1)))
    (begin
      (do ((ii-row 0 (+ ii-row 1)))
	  ((>= ii-row max-row))
	(begin
	  (let ((jj-row (- max-index ii-row)))
	    (let ((elem-1 (array-ref this-array ii-row ii-row))
		  (elem-2 (array-ref this-array ii-row jj-row)))
	      (begin
		;;; don't double count the center row
		(if (equal? ii-row center-row)
		    (set! sum (+ sum elem-1))
		    (set! sum (+ sum elem-1 elem-2)))
		)))))
      sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-diag-sum-3-1)
  (let ((sub-name "test-diag-sum-3-1")
	(test-array (make-array 0 3 3))
	(shouldbe-list-list (list
			     (list 7 8 9)
			     (list 6 1 2)
			     (list 5 4 3)))
	(max-row 3)
	(max-col 3)
	(shouldbe-num 25))
    (let ((shouldbe-array (list->array 2 shouldbe-list-list)))
      (begin
	(make-spiral-array! test-array max-row max-col)

	(let ((result-num (diag-sum test-array max-row max-col)))
	  (begin
	    (if (not (equal? shouldbe-num result-num))
		(begin
		  (display (format #f "~a : error (~a) : test-array=~a, shouldbe=~a, result=~a~%"
				   sub-name test-label-index test-array
				   shouldbe-num result-num))
		  (quit)
		  ))
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-diag-sum-5-1)
  (let ((sub-name "test-diag-sum-5-1")
	(test-array (make-array 0 5 5))
	(shouldbe-list-list (list
			     (list 21 22 23 24 25)
			     (list 20  7  8  9 10)
			     (list 19  6  1  2 11)
			     (list 18  5  4  3 12)
			     (list 17 16 15 14 13)
			     ))
	(max-row 5)
	(max-col 5)
	(shouldbe-num 101))
    (let ((shouldbe-array (list->array 2 shouldbe-list-list)))
      (begin
	(make-spiral-array! test-array max-row max-col)

	(let ((result-num (diag-sum test-array max-row max-col)))
	  (begin
	    (if (not (equal? shouldbe-num result-num))
		(begin
		  (display (format #f "~a : error (~a) : test-array=~a, shouldbe=~a, result=~a~%"
				   sub-name test-label-index test-array
				   shouldbe-num result-num))
		  (quit)
		  ))
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-row max-col debug-flag)
  (let ((this-array (make-array 0 max-row max-col)))
    (begin
      (make-spiral-array! this-array max-row max-col)

      (let ((this-sum (diag-sum this-array max-row max-col)))
	(begin
	  (display (ice9-format:format #f "max-row=~:d, max-col=~:d~%"
				       max-row max-col))
	  (if (equal? debug-flag #t)
	      (begin
		(display-array this-array max-row max-col)
		))

	  (display (ice9-format:format #f "the sum of the numbers on the diagonals is ~:d~%"
				       this-sum))
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
    (display (format #f "Problem 028 - Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:~%"))
    (newline)
    (display (format #f " 21 22 23 24 25~%"))
    (display (format #f " 20  7  8  9 10~%"))
    (display (format #f " 19  6  1  2 11~%"))
    (display (format #f " 18  5  4  3 12~%"))
    (display (format #f " 17 16 15 14 13~%"))
    (newline)
    (display (format #f "It can be verified that the sum of the numbers on the diagonals is 101.~%"))
    (newline)
    (display (format #f "What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-spiral-array-3-1 counter)
	   (run-test test-make-spiral-array-5-1 counter)
	   (run-test test-diag-sum-3-1 counter)
	   (run-test test-diag-sum-5-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-row 5)
	  (max-col 5)
	  (debug-flag #t))
      (begin
	(main-loop max-row max-col debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-row 1001)
	  (max-col 1001)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-row max-col debug-flag)
	   ))
	))
    (newline)
    ))
