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

;;;### ice-9 receive for multiple value receive
(use-modules ((ice-9 receive)
	      :renamer (symbol-prefix-proc 'ice9-receive:)))

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
(define (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
;;; note: number of solutions given by y = n + (n^2)/d
;;; and number of divisors of n^2 is Prod(ak+1), where ak is the
;;; number of powers of prime(k), that divides n^2.
;;; min-num/count - largest number with num divisors less than min-num-solutions
;;; max-num/count - smallest number with num divisors greater than min-num-solutions
(define (make-list-of-divisor-counts prime-list max-powers
				     min-num-solutions maximum-number
				     status-num)
  (define (local-loop depth max-depth max-powers min-num-solutions
		      maximum-number sorted-prime-list
		      status-num start-jday counter
		      current-product current-list
		      min-num min-count max-num max-count
		      acc-max-digit-list)
    (begin
      (cond
       ((and (>= depth max-depth) (<= current-product maximum-number))
	(begin
	  (let ((num-divisors (srfi-1:fold
			       (lambda (aa prev)
				 (* (1+ (* aa 2)) prev))
                               1 (reverse current-list))))
	    (begin
	      (if (and (<= num-divisors min-num-solutions)
		       (> num-divisors min-count)
		       (> num-divisors 0)
		       (> current-product min-num))
		  (begin
		    (set! min-num current-product)
		    (set! min-count num-divisors)
		    ))

	      (if (>= num-divisors min-num-solutions)
		  (begin
		    (if (or (< max-num 0)
			    (and (<= num-divisors max-count)
				 (< current-product max-num)
				 ))
			(begin
			  (set! max-num current-product)
			  (set! max-count num-divisors)
			  (set! acc-max-digit-list
                                (reverse current-list))
			  ))
		    ))

	      (set! counter (1+ counter))
	      (if (zero? (modulo counter status-num))
		  (begin
		    (let ((end-jday (srfi-19:current-julian-day)))
		      (begin
			(display
                         (ice9-format:format
                          #f "(~:d) : current-list = ~a : min num/count = ~:d / ~:d : max num/count = ~:d / ~:d, digit-list = ~a : "
                          counter (reverse current-list) min-num
                          (euclidean/ min-count 2)
                          max-num
                          (euclidean/ max-count 2)
                          acc-max-digit-list))
			(display
                         (format #f "elapsed time = ~a : ~a~%"
                                 (julian-day-difference-to-string end-jday start-jday)
                                 (date-time-to-string (srfi-19:current-date))))
			(force-output)
			(set! start-jday end-jday)
			))
		    ))

	      (list start-jday counter
		    min-num min-count max-num max-count
		    acc-max-digit-list)
	      ))
	  ))
       (else
	(begin
	  (let ((c-prime (list-ref sorted-prime-list depth)))
	    (let ((c-prod 1))
	      (begin
		(do ((ii 0 (1+ ii)))
		    ((> ii max-powers))
		  (begin
		    (let ((next-product (* current-product c-prod))
			  (next-current-list (cons ii current-list)))
		      (begin
			(if (<= next-product maximum-number)
			    (begin
			      (let ((next-list-list
				     (local-loop (1+ depth) max-depth
						 ii min-num-solutions
						 maximum-number sorted-prime-list
						 status-num start-jday counter
						 next-product next-current-list
						 min-num min-count max-num max-count
						 acc-max-digit-list)))
				(let ((next-start-jday (list-ref next-list-list 0))
				      (next-counter (list-ref next-list-list 1))
				      (next-min-num (list-ref next-list-list 2))
				      (next-min-count (list-ref next-list-list 3))
				      (next-max-num (list-ref next-list-list 4))
				      (next-max-count (list-ref next-list-list 5))
				      (next-acc-max-list (list-ref next-list-list 6)))
				  (begin
				    (set! start-jday next-start-jday)
				    (set! counter next-counter)
				    (set! min-num next-min-num)
				    (set! min-count next-min-count)
				    (set! max-num next-max-num)
				    (set! max-count next-max-count)
				    (set! acc-max-digit-list next-acc-max-list)
				    )))
			      ))
			))
		    (set! c-prod (* c-prod c-prime))
		    ))
		)))

	  (list start-jday counter
		min-num min-count max-num max-count
		acc-max-digit-list)
	  )))
      ))
  (begin
    (let ((max-depth (length prime-list))
	  (start-jday (srfi-19:current-julian-day))
	  (counter 0)
	  (min-square (* 2 min-num-solutions))
	  (sprime-list (sort prime-list <)))
      (let ((final-list (local-loop 0 max-depth max-powers
				    min-square maximum-number sprime-list
				    status-num start-jday counter
				    1 (list) -1 -1 -1 -1 (list))))
	(let ((next-start-jday (list-ref final-list 0))
	      (next-counter (list-ref final-list 1))
	      (next-min-num (list-ref final-list 2))
	      (next-min-count (list-ref final-list 3))
	      (next-max-num (list-ref final-list 4))
	      (next-max-count (list-ref final-list 5))
	      (next-acc-max-list (list-ref final-list 6)))
	  (let ((ff-list (list next-min-num next-min-count
			       next-max-num next-max-count
			       next-acc-max-list)))
	    (begin
	      ff-list
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-list-of-divisor-counts-1)
  (let ((sub-name "test-make-list-of-divisor-counts-1")
	(test-list
	 (list
	  (list (list 2) 2 4 (list 4 5 -1 -1 (list)))
	  (list (list 2 3) 4 2 (list 2 3 4 5 (list 2 0)))
	  (list (list 2 3 5 7) 3 4 (list 8 7 6 9 (list 1 1 0 0)))
	  ))
	(status-num 1000)
	(maximum-number 1000000)
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((prime-list (list-ref a-list 0))
		 (max-powers (list-ref a-list 1))
		 (min-num-solutions (list-ref a-list 2))
		 (shouldbe-list (list-ref a-list 3)))
	     (let ((result-list
		    (make-list-of-divisor-counts
                     prime-list max-powers min-num-solutions
                     maximum-number status-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : prime-list = ~a, max-powers = ~a, num-solutions = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index prime-list max-powers
					min-num-solutions shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (prime-powers-to-string prime-list power-list)
  (let ((p-len (length prime-list))
	(pow-len (length power-list))
	(pstr-list (list)))
    (begin
      (if (equal? p-len pow-len)
	  (begin
	    (do ((ii 0 (1+ ii)))
		((>= ii p-len))
	      (begin
		(let ((a-prime (list-ref prime-list ii))
		      (a-exp (list-ref power-list ii)))
		  (begin
		    (cond
		     ((= a-exp 0)
		      (begin
			(display (format #f "debug prime-powers-to-string prime-list=~a, power-list=~a, ii=~a, a-prime=~a, a-exp=~a~%" prime-list power-list ii a-prime a-exp))
			(force-output)
			))
		     ((= a-exp 1)
		      (begin
			(let ((t-string (ice9-format:format
					 #f "~:d"
					 (list-ref prime-list ii)
					 (list-ref power-list ii))))
			  (begin
			    (set! pstr-list (cons t-string pstr-list))
			    ))
			))
		     ((> a-exp 1)
		      (begin
			(let ((t-string (ice9-format:format
					 #f "~:d^~:d"
					 (list-ref prime-list ii)
					 (list-ref power-list ii))))
			  (begin
			    (set! pstr-list (cons t-string pstr-list))
			    ))
			)))
		    ))
		))

	    (let ((fstr (string-join (reverse pstr-list) "x")))
	      (begin
		fstr
		)))
	  (begin
	    #f
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop prime-list max-powers min-number-solutions
		   maximum-number status-num)
  (let ((results-list-list
	 (make-list-of-divisor-counts prime-list max-powers
				      min-number-solutions
				      maximum-number status-num)))
    (let ((min-num (list-ref results-list-list 0))
	  (min-count (list-ref results-list-list 1))
	  (max-num (list-ref results-list-list 2))
	  (max-count (list-ref results-list-list 3))
	  (acc-max-digit-list (list-ref results-list-list 4)))
      (let ((factor-string (prime-powers-to-string prime-list
						   acc-max-digit-list)))
	(begin
	  (newline)
	  (display (ice9-format:format
		    #f "n = ~:d = ~a, number of solutions = ~:d (exceeds ~:d solutions)~%"
		    max-num factor-string (euclidean/ max-count 2)
		    min-number-solutions))
	  (display (ice9-format:format
		    #f "previous n = ~:d, number of solutions = ~:d (less than ~:d solutions)~%"
		    min-num (euclidean/ min-count 2) min-number-solutions))
	  (newline)
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
    (display (format #f "Project Euler 110 - In the following equation x, y, and n are positive integers.~%"))
    (newline)
    (display (format #f "1/x + 1/y = 1/n~%"))
    (newline)
    (display (format #f "It can be verified that when n = 1260 there are 113 distinct solutions and this is the least value of n for which the total number of distinct solutions exceeds one hundred.~%"))
    (newline)
    (display (format #f "What is the least value of n for which the number of distinct solutions exceeds four million?~%"))
    (newline)
    (display (format #f "NOTE: This problem is a much more difficult version of problem 108 and as it is well beyond the limitations of a brute force approach it requires a clever implementation.~%"))
    (newline)
    (display (format #f "The solution was found at http://www.mathblog.dk/project-euler-110-efficient-diophantine-equation/.~%"))
    (newline)
    (display (format #f "Just like problem 108, 1/x + 1/y = 1/n which is equivalent to n * (x + y) = x * y.~%"))
    (display (format #f "with d = x - n, or x = d + n, and n < x <= 2n~%"))
    (newline)
    (display (format #f "y = n + (n^2)/d, must find all divisors d of n^2, so the key is to find an n such that n^2 has 4 million divisors~%"))
    (newline)
    (display (format #f "The key idea is to construct the smallest composite number that are made up of primes, p1^(a1)*p2^(a2)*...*pn^(an) where p1>p2>p3..., and a1>=a2>=a3...  If ai<aj when i<j, p1^(a1)*p2^(a2)*...*pai^(ai)*...*paj^(aj)*...*pn^(an) is bigger than p1^(a1)*p2^(a2)*...*pai^(aj)*...*paj^(ai)*...*pn^(an), but still has the same number of divisors. Once a configuration of exponents was found, the divisor function can be used to calculate the sum of divisors, (see the wikipedia article for more details http://en.wikipedia.org/wiki/Divisor_function)~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-list-of-divisor-counts-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((prime-list (list 2 3))
	  (max-power 2)
	  (min-number-solutions 4)
	  (maximum-number (inexact->exact 10e18))
	  (status-num 1000))
      (begin
	(time-code
	 (begin
	   (main-loop prime-list max-power min-number-solutions
		      maximum-number status-num)
	   ))
	))

    (newline)
    (force-output)

    ;;; 614,889,782,588,491,410 = Product(list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
    (let ((prime-list (list 2 3 5 7 11 13 17 19 23 29 31 37))
	  (max-powers 3)
	  (min-number-solutions 4000000)
	  (maximum-number (inexact->exact 10e18))
	  (status-num 30000000))
      (begin
	(time-code
	 (begin
	   (main-loop prime-list max-powers min-number-solutions
		      maximum-number status-num)
	   ))
	))

    (newline)
    ))
