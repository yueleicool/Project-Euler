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

;;;### srfi-1 for let-values (multiple value binding)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

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
		   (* 1000.0
		      (- nsecs (+ (* nhours 60.0 60.0) (* nminutes 60.0))))))))
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
(define (populate-partition-function-hash! results-htable max-num)
  (let ((p-array (make-array 0 (+ max-num 1))))
    (begin
      (hash-clear! results-htable)
      (array-set! p-array 1 0)

      (do ((ii 1 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (let ((jj 1)
		(kk 1)
		(ss 0))
	    (begin
	      (while
	       (> jj 0)
	       (begin
		 (let ((kk-tmp (* 3 kk kk)))
		   (let ((next-jj (- ii (euclidean/
					 (+ kk-tmp kk) 2)))
			 (sign-factor -1))
		     (begin
		       (set! jj next-jj)
		       (if (>= jj 0)
			   (begin
			     (if (even? kk)
				 (begin
				   (set! sign-factor 1)
				   ))
			     (set! ss (- ss (* sign-factor (array-ref p-array jj))))
			     ))
		       (set! jj (- ii (euclidean/
				       (- kk-tmp kk) 2)))
		       (if (>= jj 0)
			   (begin
			     (if (even? kk)
				 (begin
				   (set! sign-factor 1))
				 (begin
				   (set! sign-factor -1)
				   ))
			     (set! ss (- ss (* sign-factor (array-ref p-array jj))))
			     ))

		       (set! kk (1+ kk))
		       )))
		 ))
	      (array-set! p-array ss ii)
	      (hash-set! results-htable ii ss)
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-partition-function-hash-1)
  (let ((sub-name "test-populate-partition-function-hash-1")
	(test-list
	 (list
	  (list 2 2) (list 3 3)
	  (list 4 5) (list 5 7)
	  (list 6 11) (list 7 15)
	  (list 8 22) (list 9 30)
	  (list 10 42) (list 11 56)
	  (list 12 77) (list 13 101)
	  (list 14 135) (list 15 176)
	  (list 16 231) (list 17 297)
	  (list 18 385) (list 19 490)
	  ))
	(max-num 20)
	(results-htable (make-hash-table 20))
	(mod-num 10000)
	(debug-flag #f)
	(test-label-index 0))
    (begin
      (populate-partition-function-hash! results-htable max-num)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (hash-ref results-htable num -1)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num min-ways)
  (let ((loop-continue-flag #t)
	(results-htable (make-hash-table max-num))
	(start-num 1)
	(max-iterations max-num)
	(ii-result 0)
	(last-num-ways 0)
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (while
       (equal? loop-continue-flag #t)
       (begin
	 (populate-partition-function-hash! results-htable max-iterations)

	 (do ((ii start-num (1+ ii)))
	     ((or (> ii max-iterations)
		  (equal? loop-continue-flag #f)))
	   (begin
	     (let ((rnum (hash-ref results-htable ii -1)))
	       (begin
		 (if (zero? (modulo rnum min-ways))
		     (begin
		       (set! loop-continue-flag #f)
		       (set! ii-result ii)
		       (display
			(ice9-format:format
			 #f "~:d is the least value for which p(n) = ~:d (~1,5e) is divisible by ~:d~%"
			 ii-result rnum (exact->inexact rnum) min-ways))
		       (force-output)
		       ))
		 (set! last-num-ways rnum)
		 ))
	     ))

	 (if (equal? loop-continue-flag #t)
	     (begin
	       (let ((end-jday (srfi-19:current-julian-day)))
		 (begin
		   (display (ice9-format:format #f "completed ~:d : last partition count = ~:d mod ~:d : (~1,5e) : "
						max-iterations last-num-ways min-ways
						(exact->inexact last-num-ways)))
		   (display (format #f "elapsed time = ~a : ~a~%"
				    (julian-day-difference-to-string end-jday start-jday)
				    (date-time-to-string (srfi-19:current-date))))
		   (force-output)
		   (set! start-jday end-jday)
		   ))
	       ))

	 (set! start-num max-iterations)
	 (set! max-iterations (+ max-iterations max-num))
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
    (display (format #f "Problem 078 - Let p(n) represent the number of different ways in which n coins can be separated into piles. For example, five coins can separated into piles in exactly seven different ways, so p(5)=7.~%"))
    (newline)
    (display (format #f "  5~%"))
    (display (format #f "  4 & 1~%"))
    (display (format #f "  3 & 2~%"))
    (display (format #f "  3 & 1 & 1~%"))
    (display (format #f "  2 & 2 & 1~%"))
    (display (format #f "  2 & 1 & 1 & 1~%"))
    (display (format #f "  1 & 1 & 1 & 1 & 1~%"))
    (newline)
    (display (format #f "Find the least value of n for which p(n) is divisible by one million.~%"))
    (newline)
    (display (format #f "A fast algorithm was found at http://www.numericana.com/answer/numbers.htm#partitions~%"))
    (display (format #f "The guile program takes about 12 minutes, but was re-written in c++, and completed in 15 seconds.  So the algorithm seems to be ok.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-populate-partition-function-hash-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 10)
	  (min-ways 5)
	  (debug-flag #t))
      (begin
	(main-loop max-num min-ways)
	))

    (newline)
    (force-output)

    (let ((max-num 10000)
	  (min-ways 1000000))
      (begin
	(time-code
	 (begin
	   (main-loop max-num min-ways)
	   ))
	))

    (newline)
    ))
