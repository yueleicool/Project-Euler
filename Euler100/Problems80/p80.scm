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
;;; returns a rational number
(define (sqrt-herons-method input-number epsilon)
  (let ((closest-int-sqrt (exact-integer-sqrt input-number))
	(delta 1.0))
    (let ((xn closest-int-sqrt))
      (begin
	(while
	 (> delta epsilon)
	 (begin
	   (let ((xnp1 (/ (+ xn (/ input-number xn)) 2)))
	     (let ((next-delta (abs (exact->inexact (- xnp1 xn)))))
	       (begin
		 (set! xn xnp1)
		 (set! delta next-delta)
		 )))
	   ))
	xn
	))))

;;;#############################################################
;;;#############################################################
(define (test-sqrt-herons-method-1)
  (let ((sub-name "test-sqrt-herons-method-1")
	(test-list
	 (list
	  (list 2 1.0e-4 1.4142)
	  (list 2 1.0e-10 1.4142135624)
	  (list 3 1.0e-4 1.7321)
	  (list 4 1.0e-4 2.0000)
	  (list 5 1.0e-4 2.2361)
	  (list 6 1.0e-4 2.4495)
	  (list 6 1.0e-5 2.44949)
	  (list 6 1.0e-6 2.449490)
	  (list 7 1.0e-4 2.6458)
	  (list 8 1.0e-4 2.8284)
	  (list 9 1.0e-4 3.0000)
	  (list 10 1.0e-4 3.1622)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (delta (list-ref alist 1))
		 (shouldbe-decimal (list-ref alist 2)))
	     (let ((result-rational (sqrt-herons-method test-num delta)))
	       (let ((result-decimal (exact->inexact result-rational)))
		 (begin
		   (if (> (abs (- shouldbe-decimal result-decimal)) delta)
		       (begin
			 (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index test-num
					  shouldbe-decimal result-decimal))
			 (quit)
			 ))
		   ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (calc-decimal-digits-list this-rational num-digits)
  (cond
   ((integer? this-rational) (list))
   (else
    (begin
      ;;; first take care of the integer part
      (let ((integral-part (truncate this-rational))
	    (num-decimals num-digits)
	    (results-list (list)))
	(begin
	  (while
	   (>= integral-part 1.0)
	   (begin
	     (let ((num (inexact->exact
			 (* 10.0 (truncate (* 0.10 integral-part))))))
	       (let ((first-digit (- integral-part num)))
		 (let ((next-num (truncate (* 0.10 num))))
		   (begin
		     (set! results-list (cons first-digit results-list))
		     (set! integral-part next-num)
		     (set! num-decimals (1- num-decimals))
		     ))
		 ))
	     ))

	  ;;; next, take care of the decimal part
	  (let ((integral-part (truncate this-rational)))
	    (let ((fractional-part (- this-rational integral-part))
		  (dresults-list (list)))
	      (begin
		(do ((ii 0 (1+ ii)))
		    ((>= ii num-decimals))
		  (begin
		    (let ((next-int (inexact->exact
				     (truncate
				      (* 10.0 fractional-part)))))
		      (let ((ten-decimals (* 10 fractional-part)))
			(begin
			  (set! dresults-list (cons next-int dresults-list))
			  (set! fractional-part (- ten-decimals next-int))
			  )))
		    ))
		(set! results-list (append results-list (reverse dresults-list)))
		)))

	  results-list
	  )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-decimal-digits-list-1)
  (let ((sub-name "test-calc-decimal-digits-list-1")
	(test-list
	 (list
	  (list 2 1.0e-4 1.4142 4 (list 1 4 1 4))
	  (list 3 1.0e-4 1.7320 4 (list 1 7 3 2))
	  (list 4 1.0e-4 2.0000 4 (list))
	  (list 5 1.0e-4 2.2360 4 (list 2 2 3 6))
	  (list 6 1.0e-4 2.4494 4 (list 2 4 4 9))
	  (list 6 1.0e-5 2.44948 5 (list 2 4 4 9 4))
	  (list 6 1.0e-6 2.449489 6 (list 2 4 4 9 4 8))
	  (list 7 1.0e-4 2.6457 4 (list 2 6 4 5))
	  (list 8 1.0e-4 2.8284 4 (list 2 8 2 8))
	  (list 9 1.0e-4 3.0000 4 (list) )
	  (list 10 1.0e-4 3.1622 4 (list 3 1 6 2))
	  (list 100 1.0e-4 10.0 4 (list))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (delta (list-ref alist 1))
		 (sqrt-decimal (list-ref alist 2))
		 (num-digits (list-ref alist 3))
		 (shouldbe-list (list-ref alist 4)))
	     (let ((result-rational (sqrt-herons-method test-num delta)))
	       (let ((result-list (calc-decimal-digits-list result-rational num-digits)))
		 (begin
		   (if (not (equal? shouldbe-list result-list))
		       (begin
			 (display (format #f "~a : (~a) : error : number = ~a, num-digits = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index test-num num-digits
					  shouldbe-list result-list))
			 (quit)
			 ))
		   ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num end-num delta num-digits debug-flag)
  (let ((result-sum 0))
    (begin
      (do ((ii start-num (1+ ii)))
	  ((> ii end-num))
	(begin
	  (let ((sqrt-rational (sqrt-herons-method ii delta)))
	    (let ((decimals-list (calc-decimal-digits-list sqrt-rational num-digits)))
	      (let ((this-sum (srfi-1:fold + 0 decimals-list)))
		(begin
		  (set! result-sum (+ result-sum this-sum))

		  (if (equal? debug-flag #t)
		      (begin
			(display (ice9-format:format
				  #f "  sqrt(~:d) = ~a, decimals-list = ~a, decimals sum = ~a~%"
				  ii (exact->inexact sqrt-rational)
				  decimals-list this-sum))
			(force-output)
			))
		  ))
	      ))
	  ))

      (display (ice9-format:format
		#f "the sum of the first ~:d decimals of the square root "
		num-digits))
      (display (ice9-format:format
		#f "between ~:d and ~:d is ~:d~%"
		start-num end-num result-sum))
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
    (display (format #f "Problem 080 - It is well known that if the square root of a natural number is not an integer, then it is irrational. The decimal expansion of such square roots is infinite without any repeating pattern at all.~%"))
    (newline)
    (display (format #f "The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.~%"))
    (newline)
    (display (format #f "For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.~%"))
    (newline)
    (display (format #f "Used Heron's method, see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots for more details~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-sqrt-herons-method-1 counter)
	   (run-test test-calc-decimal-digits-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 2)
	  (end-num 2)
	  (delta 1.0e-120)
	  (num-digits 100)
	  (debug-flag #t))
      (begin
	(main-loop start-num end-num delta num-digits debug-flag)
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (end-num 100)
	  (delta 1.0e-120)
	  (num-digits 100)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num delta num-digits debug-flag)
	   ))
	))

    (newline)
    ))
