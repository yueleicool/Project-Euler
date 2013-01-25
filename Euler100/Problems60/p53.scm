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
(define (factorial nn)
  (cond
   ((< nn 0) #f)
   ((= nn 0) 1)
   (else
    (let ((result 1))
      (begin
	(do ((ii 1 (1+ ii)))
	    ((> ii nn))
	  (begin
	    (set! result (* ii result))
	    ))
	result
	))
    )))

;;;#############################################################
;;;#############################################################
(define (test-factorial-1)
  (let ((sub-name "test-factorial-1")
	(test-list
	 (list
	  (list 0 1) (list 1 1) (list 2 2)
	  (list 3 6) (list 4 24) (list 5 120)
	  (list 6 720)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (factorial test-num)))
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
(define (combinatorial nn rr)
  (define (local-semi-fac max-num min-num)
    (let ((prod 1)
	  (diff (- max-num min-num)))
      (begin
	(do ((ii (+ diff 1) (+ ii 1)))
	    ((> ii max-num))
	  (begin
	    (set! prod (* prod ii))
	    ))
	prod
	)))
  (cond
   ((< nn 0) #f)
   ((= nn 0) 1)
   (else
    (let ((numerator (local-semi-fac nn rr))
	  (denominator (factorial rr)))
      (if (and (number? numerator)
	       (number? denominator))
	  (begin
	    (let ((cnr (euclidean/ numerator denominator)))
	      cnr
	      ))
	  (begin
	    #f
	    ))
      ))))

;;;#############################################################
;;;#############################################################
(define (test-combinatorial-1)
  (let ((sub-name "test-combinatorial-1")
	(test-list
	 (list
	  (list 1 1 1) (list 1 0 1)
	  (list 2 0 1) (list 2 1 2) (list 2 2 1)
	  (list 3 0 1) (list 3 1 3) (list 3 2 3) (list 3 3 1)
	  (list 4 0 1) (list 4 1 4) (list 4 2 6) (list 4 3 4) (list 4 4 1)
	  (list 5 0 1) (list 5 1 5) (list 5 2 10) (list 5 3 10)
	  (list 5 4 5) (list 5 5 1)
	  (list 6 0 1) (list 6 1 6) (list 6 2 15) (list 6 3 20)
	  (list 6 4 15) (list 6 5 6) (list 6 6 1)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-nn (list-ref alist 0))
		 (test-rr (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (let ((result-num (combinatorial test-nn test-rr)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, rr = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-nn test-rr
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
(define (main-loop max-num min-value debug-flag)
  (let ((count 0)
	(comb-list (list)))
    (begin
      (do ((nn 1 (+ nn 1)))
	  ((> nn max-num))
	(begin
	  (do ((rr 1 (+ rr 1)))
	      ((>= rr nn))
	    (begin
	      (let ((this-value (combinatorial nn rr)))
		(if (>= this-value min-value)
		    (begin
		      (set! count (+ count 1))
		      (let ((this-list (list nn rr this-value)))
			(begin
			  (if (equal? debug-flag #t)
			      (set! comb-list (cons this-list comb-list)))
			  ))
		      )))
	      ))
	  ))

      (if (and (equal? debug-flag #t)
	       (or (not (list? comb-list)) (< (length comb-list) 1)))
	  (begin
	    (display (ice9-format:format #f "no binomial factors greater than ~:d found for n <= ~:d~%" min-value max-num)))
	  (begin
	    (display (ice9-format:format #f "found ~:d binomial factors greater than ~:d, for n <= ~:d~%"
					 count min-value max-num))
	    (if (equal? debug-flag #t)
		(begin
		  (let ((counter 0))
		    (begin
		      (for-each
		       (lambda(this-list)
			 (let ((nn (list-ref this-list 0))
			       (rr (list-ref this-list 1))
			       (value (list-ref this-list 2)))
			   (begin
			     (set! counter (+ counter 1))
			     (display (ice9-format:format #f "  (~:d) : C(~:d, ~:d) = ~:d~%"
							  counter nn rr value))
			     )))
		       (reverse comb-list)
		       )))))
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
    (display (format #f "Problem 053 - There are exactly ten ways of selecting three from five, 12345:~%"))
    (display (format #f "    123, 124, 125, 134, 135, 145, 234, 235, 245, and 345~%"))
    (newline)
    (display (format #f "In combinatorics, we use the notation, 5C3 = 10.~%"))
    (newline)
    (display (format #f "In general,~%"))
    (display (format #f "nCr =	n!/r!(nr)!, where r <= n, n! = nx(n1)x...3x2x1, and 0! = 1.
~%"))
    (newline)
    (display (format #f "It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.~%"))
    (newline)
    (display (format #f "How many, not necessarily distinct, values of  nCr, for 1 <= n <= 100, are greater than one-million?~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-factorial-1 counter)
	   (run-test test-combinatorial-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((end-num 23)
	  (min-value 1000000)
	  (debug-flag #t))
      (begin
	(main-loop end-num min-value debug-flag)
	))

    (newline)
    (force-output)

    (let ((end-num 100)
	  (min-value 1000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop end-num min-value debug-flag)
	   ))
	))

    (newline)
    ))
