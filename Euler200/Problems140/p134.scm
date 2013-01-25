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
(define (extended-gcd aa bb)
  (let ((xx 0)
	(yy 1)
	(last-xx 1)
	(last-yy 0)
	(local-aa aa)
	(local-bb bb))
    (begin
      (while
       (> local-bb 0)
       (begin
	 (let ((qq (quotient local-aa local-bb)))
	   (begin
	     (let ((tmp-aa local-aa))
	       (begin
		 (set! local-aa local-bb)
		 (set! local-bb (modulo tmp-aa local-bb))
		 ))
	     (let ((tmpxx (- last-xx (* qq xx)))
		   (tmpyy (- last-yy (* qq yy))))
	       (begin
		 (set! last-xx xx)
		 (set! xx tmpxx)
		 (set! last-yy yy)
		 (set! yy tmpyy)
		 ))
	     ))
	 ))
      (list last-xx last-yy local-aa)
      )))

;;;#############################################################
;;;#############################################################
;;; (extended-gcd 5 7) = (list xx yy gcd) = (list 3 -2 1) where 3*5 - 2*7 = 1
;;; (extended-gcd 7 11) = (list -3 2 1) where -3*7 + 2*11 = 1
;;; (extended-gcd 11 13) = (list 6 -5 1) where 6*11 - 5*13 = 1
(define (test-extended-gcd-1)
  (let ((sub-name "test-extended-gcd-1")
	(test-list
	 (list
	  (list 5 7 (list 3 -2 1))
	  (list 7 11 (list -3 2 1))
	  (list 11 13 (list 6 -5 1))
	  (list 19 23 (list -6 5 1))
	  (list 120 23 (list -9 47 1))
	  (list 23 120 (list 47 -9 1))
	  (list 100 23 (list 3 -13 1))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((aa (list-ref this-list 0))
		 (bb (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (extended-gcd aa bb)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : aa=~a, bb=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index aa bb
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
(define (factors-of-ten a-num)
  (let ((factor 1)
	(b-num a-num))
    (begin
      (while
       (> b-num 0)
       (begin
	 (set! factor (* factor 10))
	 (set! b-num (euclidean/ b-num 10))
	 ))

      factor
      )))

;;;#############################################################
;;;#############################################################
(define (test-factors-of-ten-1)
  (let ((sub-name "test-factors-of-ten-1")
	(test-list
	 (list
	  (list 1 10) (list 2 10) (list 9 10)
	  (list 10 100) (list 11 100) (list 99 100)
	  (list 100 1000) (list 999 1000)
	  (list 1000 10000) (list 9999 10000)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (factors-of-ten num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : num=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index num
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
(define (form-number pp-1 pp-2 max-num)
  (let ((pfactor (factors-of-ten pp-1)))
    (let ((rlist (extended-gcd pfactor pp-2)))
      (let ((rr (car rlist))
	    (diff (- pp-2 pp-1)))
	(let ((xx (modulo (* rr diff) pp-2)))
	  (let ((result (+ (* pfactor xx) pp-1)))
	    (begin
	      result
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-form-number-1)
  (let ((sub-name "test-form-number-1")
	(test-list
	 (list
	  (list 5 7 100 35)
	  (list 7 11 100 77)
	  (list 11 13 100 611)
	  (list 13 17 100 1513)
	  (list 17 19 100 817)
	  (list 19 23 100 1219)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((p1 (list-ref this-list 0))
		 (p2 (list-ref this-list 1))
		 (max-num (list-ref this-list 2))
		 (shouldbe (list-ref this-list 3)))
	     (let ((result (form-number p1 p2 max-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : p1=~a, p2=~a, max-num=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index p1 p2 max-num
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
(define-syntax handle-loop
  (syntax-rules ()
    ((handle-loop ii prime-array p1-limit max-nn debug-flag
		  smallest-sum smallest-count)
     (begin
       (let ((pp-1 (array-ref prime-array ii))
	     (pp-2 (array-ref prime-array (1+ ii))))
	 (begin
	   (if (<= pp-1 p1-limit)
	       (begin
		 (let ((ss (form-number pp-1 pp-2 max-nn)))
		   (begin
		     (if (> ss 0)
			 (begin
			   (set! smallest-sum (+ smallest-sum ss))
			   (set! smallest-count (1+ smallest-count))

			   (if (equal? debug-flag #t)
			       (begin
				 (display (ice9-format:format #f "  prime-1=~:d, prime-2=~:d, smallest=~:d : sum so far=~:d  count=~:d~%"
							      pp-1 pp-2 ss smallest-sum smallest-count))
				 (force-output)
				 )))
			 (begin
			   (display (ice9-format:format #f "error : for prime-1=~:d, prime-2=~:d, smallest sum not found for max-nn=~:d~%"
							pp-1 pp-2 max-nn))
			   (force-output)
			   ))
		     ))
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-prime p1-limit max-nn debug-flag)
  (let ((smallest-sum 0)
	(smallest-count 0)
	(prime-array (make-prime-array max-prime)))
    (let ((plen (1- (car (array-dimensions prime-array)))))
      (begin
	(do ((ii 2 (1+ ii)))
	    ((>= ii plen))
	  (begin
	    (handle-loop ii prime-array p1-limit max-nn debug-flag
			 smallest-sum smallest-count)

	    ))

	(display (ice9-format:format #f "Sum(smallest) = ~:d, there were ~:d consecutive prime pairs found, (for primes less than ~:d).~%"
				     smallest-sum smallest-count p1-limit))
	(force-output)
	))
    ))

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
    (display (format #f "Project Euler 134 - Consider the consecutive primes p1 = 19 and p2 = 23. It can be verified that 1219 is the smallest number such that the last digits are formed by p1 whilst also being divisible by p2.~%"))
    (newline)
    (display (format #f "In fact, with the exception of p1 = 3 and p2 = 5, for every pair of consecutive primes, p2 > p1, there exist values of n for which the last digits are formed by p1 and n is divisible by p2. Let S be the smallest of these values of n.~%"))
    (newline)
    (display (format #f "Find Sum(S) for every pair of consecutive primes with 5 <= p1 <= 1000000.~%"))
    (newline)
    (display (format #f "The solution, with a very clear explanation, was found at http://problematicsets.com/project-euler-134-investigating-consecutive-primes-and-the-extended-euclidean-algorithm/, and it makes use of the extended Euclidean algorithm http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm, (see also http://www-math.ucdenver.edu/~~wcherowi/courses/m5410/exeucalg.html)~%"))
    (newline)
    (display (format #f "To see how the extended Euclidean algorithm is used, here are some examples:~%"))
    (display (format #f "(extended-gcd 5 7) -> (list xx yy gcd) = (list 3 -2 1) where 3*5 - 2*7 = 1~%"))
    (display (format #f "(extended-gcd 7 11) -> (list -3 2 1) where -3*7 + 2*11 = 1~%"))
    (display (format #f "(extended-gcd 11 13) -> (list 6 -5 1) where 6*11 - 5*13 = 1~%"))
    (display (format #f "(extended-gcd 19 23) -> (list -6 5 1) where -6*19 + 5*23 = 1~%"))
    (display (format #f "(extended-gcd 100 23) -> (list 3 -13 1) where 3*100 - 13*23 = 1~%"))
    (newline)
    (display (format #f "For example, when p1=19, and p2=23, we need to find 100*x + p1 = k*p2, or 100*x = -p1 mod p2 = (p2 - p1) mod p2. Here the extended-gcd(100, 23) = (list 3 -13 1). We want x = (3*(p2 - p1)) mod p2 = (3*4 mod 23) = 12. So 100*12+19 = 1219 = 53*23, or 1219 has 19 as it's last two digits, and is divisible by 23..~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-prime-array-1 counter)
	   (run-test test-extended-gcd-1 counter)
	   (run-test test-factors-of-ten-1 counter)
	   (run-test test-form-number-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-prime 40)
	  (p1-limit 30)
	  (max-nn 500)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-prime p1-limit max-nn debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-prime 1100000)
	  (p1-limit 1000000)
	  (max-nn 1000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-prime p1-limit max-nn debug-flag)
	   ))
	))

    (newline)
    ))
