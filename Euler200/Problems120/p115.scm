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

;;;### ice-9 receive for receive function
(use-modules ((ice-9 receive)
	      :renamer (symbol-prefix-proc 'ice9-receive:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

;;;#############################################################
;;;#############################################################
;;;### expects 2 julian days (plain numbers)
;;;### differences between 2 julian days is in days (or a fraction of a day)
(define (julian-day-difference-to-string dend dstart)
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
(define (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))


;;;#############################################################
;;;#############################################################
(define (count-rb-fixed-length max-array-length min-red-len)
  (define (count-loop gap-len min-red-len gap-htable)
    (let ((hcount (hash-ref gap-htable gap-len #f)))
      (begin
	(cond
	 ((< gap-len min-red-len) 0)
	 ((= gap-len min-red-len) 1)
	 ((not (equal? hcount #f)) hcount)
	 (else
	  (begin
	    (let ((count 0))
	      (begin
		(do ((ii min-red-len (1+ ii)))
		    ((> ii gap-len))
		  (begin
		    (let ((this-count (1+ (- gap-len ii))))
		      (let ((remaining-gap (- gap-len ii 1))
			    (sub-count 0))
			(begin
			  (do ((jj 0 (1+ jj)))
			      ((> jj remaining-gap))
			    (begin
			      (let ((next-gap (- remaining-gap jj)))
				(let ((next-count (count-loop next-gap min-red-len
							      gap-htable)))
				  (begin
				    (set! sub-count (+ sub-count next-count))
				    )))
			      ))

			  (set! count (+ count this-count sub-count))
			  )))
		    ))

		(hash-set! gap-htable gap-len count)

		count
		))
	    )))
	)))
  (let ((gap-htable (make-hash-table)))
    (let ((count (count-loop max-array-length min-red-len gap-htable)))
      (begin
	;;; add 1 for the case where there are no red-tiles
	(1+ count)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-rb-fixed-length-1)
  (let ((sub-name "test-count-rb-fixed-length-1")
	(test-list
	 (list
	  (list 3 3 2) (list 4 3 4) (list 5 3 7)
	  (list 7 3 17)
	  (list 29 3 673135) (list 30 3 1089155)
	  (list 56 10 880711) (list 57 10 1148904)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-len (list-ref this-list 0))
		 (min-len (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (count-rb-fixed-length a-len min-len)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : array-len = ~a, min-len = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index a-len min-len
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (reproduce-problem-statement)
  (let ((a-list-list (list (list (list 29 3) (list 30 3) 1000000)
			   (list (list 56 10) (list 57 10) 1000000))))
    (begin
      (for-each
       (lambda (a-pair-list)
	 (begin
	   (let ((pair-1 (list-ref a-pair-list 0))
		 (pair-2 (list-ref a-pair-list 1))
		 (threshold (list-ref a-pair-list 2)))
	     (let ((max-1 (list-ref pair-1 0))
		   (min-1 (list-ref pair-1 1))
		   (max-2 (list-ref pair-2 0))
		   (min-2 (list-ref pair-2 1)))
	       (let ((count-1 (count-rb-fixed-length max-1 min-1))
		     (count-2 (count-rb-fixed-length max-2 min-2)))
		 (begin
		   (display (ice9-format:format
			     #f "F(~:d, ~:d) = ~:d : F(~:d, ~:d) = ~:d : n = ~:d is the least value for which the fill-count function first exceeds ~:d~%"
			     min-1 max-1 count-1
			     min-2 max-2 count-2 max-2 threshold))
		   (force-output)
		   ))
	       ))
	   )) a-list-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop min-length max-length threshold)
  (let ((prev-nn -1)
	(prev-count -1)
	(first-nn -1)
	(first-count -1)
	(continue-loop-flag #t))
    (begin
      (do ((ii min-length (1+ ii)))
	  ((or (> ii max-length)
	       (equal? continue-loop-flag #f)))
	(begin
	  (let ((curr-count (count-rb-fixed-length ii min-length)))
	    (begin
	      (if (>= curr-count threshold)
		  (begin
		    (set! continue-loop-flag #f)
		    (set! first-nn ii)
		    (set! first-count curr-count))
		  (begin
		    (set! prev-nn ii)
		    (set! prev-count curr-count)
		    ))
	      ))
	  ))

      (if (equal? continue-loop-flag #f)
	  (begin
	    (display (ice9-format:format
		      #f "F(~:d, ~:d) = ~:d : F(~:d, ~:d) = ~:d : n = ~:d is the least value for which the fill-count function first exceeds ~:d~%"
		      min-length prev-nn prev-count
		      min-length first-nn first-count first-nn threshold))
	    (force-output))
	  (begin
	    (display (ice9-format:format
		      #f "no results found : F(~:d, ~:d) = ~:d : n = ~:d is the largest value found so far, with n <= ~:d~%"
		      min-length prev-nn prev-count prev-nn max-length))
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
    (display (format #f "Project Euler 115 - NOTE: This is a more difficult version of problem 114.~%"))
    (newline)
    (display (format #f "A row measuring n units in length has red blocks with a minimum length of m units placed on it, such that any two red blocks (which are allowed to be different lengths) are separated by at least one black square.~%"))
    (newline)
    (display (format #f "Let the fill-count function, F(m, n), represent the number of ways that a row can be filled.~%"))
    (newline)
    (display (format #f "For example, F(3, 29) = 673135 and F(3, 30) = 1089155.~%"))
    (newline)
    (display (format #f "That is, for m = 3, it can be seen that n = 30 is the smallest value for which the fill-count function first exceeds one million.~%"))
    (newline)
    (display (format #f "In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count function first exceeds one million.~%"))
    (newline)
    (display (format #f "For m = 50, find the least value of n for which the fill-count function first exceeds one million.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-count-rb-fixed-length-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (time-code
     (begin
       (reproduce-problem-statement)
       ))

    (newline)
    (force-output)

    (let ((min-length 50)
	  (max-length 1000)
	  (threshold 1000000))
      (begin
	(time-code
	 (begin
	   (main-loop min-length max-length threshold)
	   ))
	))

    (newline)
    ))
