#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

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
(define (even-fib-sum max-num)
  (let ((f0 0)
	(f1 1)
	(isum 0))
    (begin
      (do ((f2 1 (+ f0 f1)))
	  ((> f2 max-num))
	(begin
	  (if (zero? (modulo f2 2))
	      (begin
		(set! isum (+ isum f2))
		))

	  (set! f0 f1)
	  (set! f1 f2)
	  ))
      isum
      )))

;;;#############################################################
;;;#############################################################
(define (test-even-fib-sum-1)
  (let ((sub-name "test-even-fib-sum-1")
	(test-list
	 (list
	  (list 3 2) (list 5 2) (list 6 2)
	  (list 8 10) (list 12 10) (list 20 10)
	  (list 30 10) (list 33 10) (list 34 44)
	  (list 35 44) (list 40 44)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-max (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (even-fib-sum test-max)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : max=~a, fib-sum shouldbe=~a, result=~a~%"
					sub-name test-label-index test-max
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
(define (main-loop max-num)
  (let ((sum (even-fib-sum max-num)))
    (begin
      (display (ice9-format:format #f "sum of even fibonacci numbers from 1 through ~:d is ~:d~%" (- max-num 1) sum))
      (force-output)
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
    (display (format #f "Project Euler 2: Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:~%"))
    (newline)
    (display (format #f "1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...~%"))
    (newline)
    (display (format #f "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.~%"))
    (newline)
    (display (format #f "see also http://en.wikipedia.org/wiki/Fibonacci_number~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-even-fib-sum-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 10))
      (begin
	(main-loop max-num)
	))

    (newline)
    (force-output)

    (let ((max-num 50))
      (begin
	(main-loop max-num)
	))

    (newline)
    (force-output)

    (let ((max-num 4000000))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))
    (newline)
    ))
