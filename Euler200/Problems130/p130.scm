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

;;; srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

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
(define (current-date-time-string)
  (let ((this-datetime (srfi-19:current-date)))
    (let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	  (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (make-prime-array max-num)
  (let ((intermediate-array (make-array 0 (1+ max-num)))
	(result-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! intermediate-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (do ((jj ii (+ jj ii)))
	      ((> jj max-num))
	    (begin
	      (let ((this-num (array-ref intermediate-array jj)))
		(begin
		  (if (= this-num ii)
		      (begin
			(set! result-list (cons ii result-list)))
		      (begin
			(array-set! intermediate-array -1 jj)
			))
		  ))
	      ))
	  ))
      (list->array 1 (reverse result-list))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-array-1)
  (let ((sub-name "test-make-prime-array-1")
	(test-list
	 (list
	  (list 2 (list 2)) (list 3 (list 2 3)) (list 4 (list 2 3))
	  (list 5 (list 2 3 5)) (list 6 (list 2 3 5))
	  (list 7 (list 2 3 5 7)) (list 8 (list 2 3 5 7))
	  (list 9 (list 2 3 5 7)) (list 10 (list 2 3 5 7))
	  (list 11 (list 2 3 5 7 11)) (list 13 (list 2 3 5 7 11 13))
	  (list 17 (list 2 3 5 7 11 13 17))
	  (list 19 (list 2 3 5 7 11 13 17 19))
	  (list 23 (list 2 3 5 7 11 13 17 19 23))
	  (list 31 (list 2 3 5 7 11 13 17 19 23 29 31))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-array (make-prime-array test-num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (car (array-dimensions result-array))))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, lengths not equal, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-num
					  shouldbe slen rlen))
			 (quit)
			 ))
		   (do ((ii 0 (1+ ii)))
		       ((>= ii slen))
		     (begin
		       (let ((s-elem (list-ref shouldbe-list ii))
			     (r-elem (array-ref result-array ii)))
			 (begin
			   (if (not (equal? s-elem r-elem))
			       (begin
				 (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a, discrepancy at ii=~a, shouldbe=~a, result=~a~%"
						  sub-name test-label-index test-num
						  shouldbe result ii s-elem r-elem))
				 (quit)
				 ))
			   ))
		       ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (is-array-prime? nn prime-array)
  (begin
    (cond
     ((<= nn 1) #f)
     ((= nn 2) #t)
     ((even? nn) #f)
     (else
      (let ((max-divisor
	     (+ (exact-integer-sqrt nn) 1))
	    (asize (car (array-dimensions prime-array)))
	    (aprime 2)
	    (is-prime-flag #t)
	    (continue-loop-flag #t))
	(begin
	  (do ((ii 0 (1+ ii)))
	      ((or (>= ii asize)
		   (>= aprime max-divisor)
		   (equal? continue-loop-flag #f)))
	    (begin
	      (set! aprime (array-ref prime-array ii))

	      (if (zero? (modulo nn aprime))
		  (begin
		    (set! is-prime-flag #f)
		    (set! continue-loop-flag #f)
		    ))
	      ))
	  is-prime-flag
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-is-array-prime-1)
  (let ((sub-name "test-is-array-prime-1")
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
	  (list 52 #f) (list 53 #t) (list 54 #f) (list 55 #f)
	  (list 56 #f) (list 57 #f) (list 58 #f) (list 59 #t)
	  (list 60 #f) (list 61 #t) (list 62 #f) (list 63 #f)
	  (list 64 #f) (list 65 #f) (list 66 #f) (list 67 #t)
	  (list 68 #f) (list 69 #f) (list 70 #f) (list 71 #t)
	  (list 72 #f) (list 73 #t) (list 74 #f) (list 75 #f)
	  (list 76 #f) (list 77 #f) (list 78 #f) (list 79 #t)
	  (list 80 #f) (list 81 #f) (list 82 #f) (list 83 #t)
	  (list 84 #f) (list 85 #f) (list 86 #f) (list 87 #f)
	  (list 88 #f) (list 89 #t) (list 90 #f) (list 91 #f)
	  (list 92 #f) (list 93 #f) (list 94 #f) (list 95 #f)
	  (list 96 #f) (list 97 #t) (list 98 #f) (list 99 #f)
	  ))
	(prime-array (make-prime-array 20))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (is-array-prime? test-num prime-array)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
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
(define (brute-force-aa-calc nn)
  (let ((min-kk -1)
	(kk 1)
	(rr-kk 1)
	(continue-loop-flag #t))
    (begin
      (while
       (equal? continue-loop-flag #t)
       (begin
	 (if (zero? rr-kk)
	     (begin
	       (set! min-kk kk)
	       (set! continue-loop-flag #f)
	       ))
	 (set! rr-kk (modulo (1+ (* 10 rr-kk)) nn))
	 (set! kk (1+ kk))
	 ))

      min-kk
      )))

;;;#############################################################
;;;#############################################################
(define (test-brute-force-aa-calc-1)
  (let ((sub-name "test-brute-force-aa-calc-1")
	(test-list
	 (list
	  (list 7 6) (list 41 5)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((nn (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (brute-force-aa-calc nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : nn=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index nn
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
(define (main-loop max-num max-composites max-prime status-num debug-flag)
  (let ((sum-composites 0)
	(composites-count 0)
	(prime-array (make-prime-array max-prime))
	(start-jday (srfi-19:current-julian-day))
	(continue-loop-flag #t))
    (begin
      (do ((nn 9 (+ nn 2)))
	  ((or (> nn max-num)
	       (equal? continue-loop-flag #f)))
	(begin
	  (if (and (not (zero? (modulo nn 5)))
		   (equal? (is-array-prime? nn prime-array) #f))
	      (begin
		(let ((aa-kk (brute-force-aa-calc nn))
		      (nn-minus-1 (1- nn)))
		  (begin
		    (if (zero? (modulo nn-minus-1 aa-kk))
			(begin
			  (set! sum-composites (+ sum-composites nn))
			  (set! composites-count (1+ composites-count))
			  (if (>= composites-count max-composites)
			      (begin
				(set! continue-loop-flag #f)
				))

			  (if (equal? debug-flag #t)
			      (begin
				(display (format #f "  nn=~a, nn-minus-1=~a, aa-kk=~a, modulo=~a, sum-composites=~a, count=~a~%"
						 nn nn-minus-1 aa-kk (modulo nn-minus-1 aa-kk)
						 sum-composites composites-count))
				(force-output)
				))
			  ))
		    ))
		))

	  (if (zero? (modulo nn status-num))
	      (begin
		(display (ice9-format:format #f "completed ~:d / ~:d : ~%"
					     nn max-num))
		(let ((end-jday (srfi-19:current-julian-day)))
		  (begin
		    (display (format #f "elapsed time = ~a : ~a~%"
				     (julian-day-difference-to-string end-jday start-jday)
				     (current-date-time-string)))
		    ))
		(force-output)
		))
	  ))

      (if (equal? continue-loop-flag #f)
	  (begin
	    (display (ice9-format:format #f "The sum of the first ~:d composite values of A(n) is ~:d.~%"
					 max-composites sum-composites)))
	  (begin
	    (display (ice9-format:format #f "Insufficient composites found in the range 9 <= n <= ~:d.  Number found = ~:d, sum so far = ~:d.~%"
					 max-num composites-count sum-composites))
	    ))

      (force-output)
      )))

;;;#############################################################
;;;#############################################################
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
				(current-date-time-string)))
	       (force-output)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main args)
  (begin
    (display (format #f "Project Euler 130 - A number consisting entirely of ones is called a repunit. We shall define R(k) to be a repunit of length k; for example, R(6) = 111111.~%"))
    (newline)
    (display (format #f "Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that there always exists a value, k, for which R(k) is divisible by n, and let A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.~%"))
    (newline)
    (display (format #f "You are given that for all primes, p > 5, that p - 1 is divisible by A(p). For example, when p = 41, A(41) = 5, and 40 is divisible by 5.~%"))
    (newline)
    (display (format #f "However, there are rare composite values for which this is also true; the first five examples being 91, 259, 451, 481, and 703.~%"))
    (newline)
    (display (format #f "Find the sum of the first twenty-five composite values of n for which GCD(n, 10) = 1 and n - 1 is divisible by A(n).~%"))
    (newline)
    (display (format #f "For more on A(n) see http://oeis.org/A099679~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-prime-array-1 counter)
	   (run-test test-is-array-prime-1 counter)
	   (run-test test-brute-force-aa-calc-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 1000)
	  (max-composites 5)
	  (max-prime 1000)
	  (status-num 10000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num max-composites max-prime
		      status-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-num 100000)
	  (max-composites 25)
	  (max-prime 100000)
	  (status-num 10000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num max-composites max-prime
		      status-num debug-flag)
	   ))
	))

    (newline)
    ))
