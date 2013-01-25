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
(define (sum-of-squares max-nn)
  (begin
    (cond
     ((<= max-nn 0) -1)
     (else
      (let ((isum 0))
	(begin
	  (do ((ii 1 (+ ii 1)))
	      ((> ii max-nn))
	    (begin
	      (set! isum (+ isum (* ii ii)))
	      ))
	  isum
	  )))
     )))

;;;#############################################################
;;;#############################################################
(define (test-sum-of-squares-1)
  (let ((sub-name "test-sum-of-squares-1")
	(test-list
	 (list
	  (list 0 -1) (list 1 1) (list 2 5) (list 3 14)
	  (list 4 30) (list 5 55) (list 6 91)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (sum-of-squares test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, sum-of-squares shouldbe=~a, result=~a~%"
					sub-name test-label-index test-num
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
(define (sum-1-to-n-squared max-nn)
  (begin
    (cond
     ((<= max-nn 0) -1)
     (else
      (let ((sum (euclidean-quotient (* (+ max-nn 1) max-nn) 2)))
	(let ((sum2 (* sum sum)))
	  (begin
	    sum2
	    )))))))

;;;#############################################################
;;;#############################################################
(define (test-sum-1-to-n-squared-1)
  (let ((sub-name "test-sum-1-to-n-squared-1")
	(test-list
	 (list
	  (list 1 1) (list 2 9) (list 3 36)
	  (list 4 100) (list 5 225)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (sum-1-to-n-squared test-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : test num = ~a, shouldbe-num = ~a, result-num = ~a~%"
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
(define (squares-difference max-nn)
  (begin
    (let ((ssq (sum-of-squares max-nn))
	  (ssum (sum-1-to-n-squared max-nn)))
      (let ((diff (- ssum ssq)))
	diff
	))))

;;;#############################################################
;;;#############################################################
(define (test-squares-difference-1)
  (let ((sub-name "test-squares-difference-1")
	(test-list
	 (list
	  (list 10 2640)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (squares-difference test-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (main-loop max-num)
  (let ((diff (squares-difference max-num))
	(sum-sq (sum-1-to-n-squared max-num))
	(sq-sum (sum-of-squares max-num)))
    (begin
      (display (ice9-format:format #f "difference between squared sum and sum of squares from 1 to ~:d is ~:d - ~:d = ~:d~%"
		       max-num sum-sq sq-sum diff))
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
    (display (format #f "Project Euler 6: The sum of the squares of the first ten natural numbers is,~%"))
    (display (format #f "1^2 + 2^2 + ... + 10^2 = 385~%"))
    (display (format #f "The square of the sum of the first ten natural numbers is,~%"))
    (display (format #f "(1 + 2 + ... + 10)^2 = 55^2 = 3025~%"))
    (display (format #f "Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.~%"))
    (display (format #f "Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-sum-of-squares-1 counter)
	   (run-test test-sum-1-to-n-squared-1 counter)
	   (run-test test-squares-difference-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 10))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))
    (newline)
    (force-output)

    (let ((max-num 100))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))
    (newline)
    ))
