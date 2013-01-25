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
;;; 1/x + 1/y = 1/n which is equivalent to n * (x + y) = x * y
;;; with d = x - n, or x = d + n, and n < x <= 2n
;;; (if x = 3n say, then y < 0)
;;; y = n + (n^2)/d, must find all divisors d of n^2 such that
;;; y an integer and x <= y
(define (calc-x-y-solutions nn)
  (let ((solutions-list (list))
	(nsquared (* nn nn))
	(max-dd (+ nn 1)))
    (begin
      (do ((dd 1 (1+ dd)))
	  ((> dd max-dd))
	(begin
	  (ice9-receive:receive
	   (ee remain)
	   (euclidean/ nsquared dd)
	   (begin
	     (if (zero? remain)
		 (begin
		   (let ((xx (+ dd nn))
			 (yy (+ nn ee)))
		     (begin
		       (if (>= yy xx)
			   (begin
			     (let ((elem (list xx yy)))
			       (begin
				 (set! solutions-list
				       (cons (list xx yy) solutions-list))
				 ))
			     ))
		       ))
		   ))
	     ))
	  ))

      (reverse solutions-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-x-y-solutions-1)
  (let ((sub-name "test-calc-x-y-solutions-1")
	(test-list
	 (list
	  (list 2 (list (list 3 6) (list 4 4)))
	  (list 3 (list (list 4 12) (list 6 6)))
	  (list	4 (list (list 5 20) (list 6 12) (list 8 8)))
	  (list 5 (list (list 6 30) (list 10 10)))
	  (list 6 (list (list 7 42) (list 8 24) (list 9 18)
			(list 10 15) (list 12 12)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((nn (list-ref a-list 0))
		 (shouldbe (list-ref a-list 1)))
	     (let ((result (calc-x-y-solutions nn)))
	       (let ((slen (length shouldbe))
		     (rlen (length result)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : nn=~a, shouldbe = ~a, result = ~a, lengths not equal, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index nn
					  shouldbe result slen rlen))
			 (quit)
			 ))
		   (for-each
		    (lambda (s-list)
		      (begin
			(if (equal? (member s-list result) #f)
			    (begin
			      (display (format #f "~a : error (~a) : nn=~a, shouldbe = ~a, result = ~a, discrepancy at ~a~%"
					       sub-name test-label-index nn
					       shouldbe result s-list))
			      (quit)
			      ))
			)) shouldbe)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (display-entire-list results-list nn)
  (begin
    (for-each
     (lambda (a-list)
       (begin
	 (display (ice9-format:format #f "    1/~:d + 1/~:d = 1/~:d~%"
				      (list-ref a-list 0)
				      (list-ref a-list 1) nn))
	 )) results-list)
    ))

;;;#############################################################
;;;#############################################################
(define (display-partial-list results-list top-num nn)
  (let ((rlen (length results-list))
	(counter 0)
	(loop-continue-flag #t))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((or (>= ii rlen)
	       (equal? loop-continue-flag #f)))
	(begin
	  (let ((a-list (list-ref results-list ii)))
	    (begin
	      (display (ice9-format:format #f "    1/~:d + 1/~:d = 1/~:d~%"
					   (list-ref a-list 0)
					   (list-ref a-list 1) nn))
	      (set! counter (1+ counter))
	      (if (> counter top-num)
		  (begin
		    (set! loop-continue-flag #f)
		    ))
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-nn nn-base min-number-solutions debug-flag)
  (let ((loop-continue-flag #t)
	(min-result-list (list))
	(min-nn -1))
    (begin
      (do ((nn 1 (1+ nn)))
	  ((or (>= nn max-nn)
	       (equal? loop-continue-flag #f)))
	(begin
	  (let ((nn-sum (* nn nn-base)))
	    (let ((results-list (calc-x-y-solutions nn-sum)))
	      (if (and (list? results-list) (> (length results-list) 0))
		  (begin
		    (let ((num-results (length results-list)))
		      (begin
			(if (> num-results min-number-solutions)
			    (begin
			      (set! loop-continue-flag #f)

			      (display (ice9-format:format
					#f "nn = ~:d, number of solutions = ~:d (exceeds ~:d solutions)~%"
					nn-sum num-results min-number-solutions))
			      (if (equal? debug-flag #t)
				  (begin
				    (display-entire-list results-list nn-sum))
				  (begin
				    (let ((top-results 4))
				      (begin
					(display (ice9-format:format #f "first ~:d results out of ~:d~%" top-results num-results))
					(display-partial-list results-list top-results nn-sum)
					(force-output)
					))
				    ))
			      (force-output))
			    (begin
			      (if (equal? debug-flag #t)
				  (begin
				    (display-entire-list results-list nn)
				    (newline)
				    (force-output)
				    ))

			      (set! min-result-list results-list)
			      (set! min-nn nn)
			      ))
			))
		    ))
	      ))
	  ))

      (if (equal? loop-continue-flag #t)
	  (begin
	    (display (ice9-format:format #f "did not find any results where the number of solutions exceeded ~:d, for n < ~:d~%" min-number-solutions max-nn))
	    (let ((top-results 4)
		  (num-results (length min-result-list)))
	      (begin
		(display (ice9-format:format #f "first ~:d results out of ~:d~%" top-results num-results))
		(display-partial-list min-result-list top-results min-nn)
		))
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
    (display (format #f "Project Euler 108 - In the following equation x, y, and n are positive integers.~%"))
    (newline)
    (display (format #f "1/x + 1/y = 1/n~%"))
    (newline)
    (display (format #f "For n = 4 there are exactly three distinct solutions:~%"))
    (display (format #f "1/5 + 1/20 = 1/4~%"))
    (display (format #f "1/6 + 1/12 = 1/4~%"))
    (display (format #f "1/8 + 1/8 = 1/4~%"))
    (newline)
    (display (format #f "What is the least value of n for which the number of distinct solutions exceeds one-thousand?~%"))
    (newline)
    (display (format #f "NOTE: This problem is an easier version of problem 110; it is strongly advised that you solve this one first.~%"))
    (newline)
    (display (format #f "Since using rationals are slow, this algorithm tries to find solutions using the equation (for fixed n), 1/n = 1/x + 1/y, then y is a solution if y = (n * x) / (x - n)~%"))
    (newline)
    (display (format #f "The solution was found at http://keyzero.wordpress.com/2010/06/05/project-euler-problem-108/. This is an extremely demanding program, since one must iterate over n and iterate over x to find y.  Some important limits to reduce the amount of work needed, n < x <= 2n, and setting d=(n-x), then realizing that y = n+(n^2)/d, so we just need to look for n < x <= 2n, and d a divisor of n^2, where n^2 has a large number of divisors, which will give a large number of solutions.~%"))
    (newline)
    (display (format #f "The number of divisors of a number n, is given by d(n) = Product(ai + 1), where i ranges over each divisor of n, and ai is the number of times each divisor divides n. For example, 24 = 2^3 x 3^1, so d(24) = (3+1)x(1+1)=8, and the divisors of 24 is 1, 2, 4, 8, 3, 6, 12, 24.  See the wikipedia article for more details http://en.wikipedia.org/wiki/Divisor_function~%"))
    (display (format #f "We can simply examine those n that have a high number of small prime factors, the article mentioned above shows that one only needs to consider those values of n that are multiples of 2x3x5x7x11x13=30,030, since n^2 will have (2+1)^6=729 divisors or more. Since 30,030x17=510,510, and 510,510^2 has (2+1)^7=2187 divisors, then y=n+n^2/d will have 2,187 possible solutions, so we can stop our iteration after n = 510,510.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-calc-x-y-solutions-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-nn 10)
	  (nn-base 1)
	  (min-number-solutions 4)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-nn nn-base min-number-solutions debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-nn 17)
	  (nn-base (* 2 3 5 7 11 13))
	  (min-number-solutions 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-nn nn-base min-number-solutions debug-flag)
	   ))
	))

    (newline)
    ))
