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
(define (calc-sum max-num debug-flag)
  (let ((isum 0))
    (begin
      (do ((ii 1 (+ ii 1)))
	  ((>= ii max-num))
	(begin
	  (if (or (zero? (modulo ii 3))
		  (zero? (modulo ii 5)))
	      (begin
		(set! isum (+ isum ii))
		(if (equal? debug-flag #t)
		    (display (format #f "debug ii=~a, isum=~a~%" ii isum)))
		))
	  ))
      isum
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-sum-1)
  (let ((sub-name "test-calc-sum-1")
	(test-list
	 (list
	  (list 1 0) (list 2 0) (list 3 0)
	  (list 4 3) (list 5 3) (list 6 8)
	  (list 7 14) (list 8 14) (list 9 14)
	  (list 10 23) (list 11 33) (list 12 33)
	  (list 13 45) (list 14 45) (list 15 45)
	  ))
	(debug-flag #f)
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (calc-sum test-num debug-flag)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
					shouldbe-num result-num))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((sum (calc-sum max-num debug-flag)))
    (begin
      (display (ice9-format:format #f "sum from 1 through ~:d is ~:d~%" (- max-num 1) sum))
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
    (display (format #f "Project Euler 1: If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.~%"))
    (newline)
    (display (format #f "Find the sum of all the multiples of 3 or 5 below 1000.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-calc-sum-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))


    (display (format #f "Output:~%"))
    (force-output)

    (time-code
     (begin
       (let ((max-num 10)
	     (debug-flag #f))
	 (begin
	   (main-loop max-num debug-flag)
	   ))
       ))
    (newline)
    (force-output)

    (time-code
     (begin
       (let ((max-num 15)
	     (debug-flag #f))
	 (begin
	   (main-loop max-num debug-flag)
	   ))
       ))
    (newline)
    (force-output)

    (time-code
     (begin
       (let ((max-num 1000)
	     (debug-flag #f))
	 (begin
	   (main-loop max-num debug-flag)
	   ))
       ))
    (newline)
    (force-output)
    ))
