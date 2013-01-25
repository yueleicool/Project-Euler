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
;;; initial solutions for Pell's equation y^2-5x^2 = -1
(define (find-initial-solutions)
  (let ((nn 5)
	(kk -1)
	(max-xx 1000)
	(max-yy 1000))
    (begin
      (do ((xx 1 (1+ xx)))
	  ((> xx max-xx))
	(begin
	  (let ((xx-2 (* xx xx)))
	    (begin
	      (do ((yy 1 (1+ yy)))
		  ((> yy max-yy))
		(begin
		  (let ((yy-2 (* nn yy yy)))
		    (begin
		      (if (= (- xx-2 yy-2) kk)
			  (begin
			    (display (ice9-format:format #f "xx=~:d, yy=~:d, kk=~a~%"
							 xx yy kk))
			    (force-output)
			    ))
		      ))
		  ))
	      ))
	  ))
      )))


;;;#############################################################
;;;#############################################################
(define-syntax cons-solution
  (syntax-rules ()
    ((cons-solution ll bb hh results-list)
     (begin
       (if (and (integer? bb) (> bb 0))
	   (begin
	     (set! results-list (cons (list ll bb hh) results-list))
	     ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (calc-bb ll nn)
  (let ((ll-2 (1- (* nn ll ll)))
	(results-list (list)))
    (let ((ll-sqrt (exact-integer-sqrt ll-2)))
      (begin
	(if (equal? ll-2 (* ll-sqrt ll-sqrt))
	    (begin
	      (let ((bb-1 (/ (+ -4 (* 2 ll-sqrt)) 5))
		    (bb-2 (/ (+ 4 (* 2 ll-sqrt)) 5))
		    (bb-3 (/ (- 4 (* 2 ll-sqrt)) 5)))
		(let ((hh-1 (1+ bb-1))
		      (hh-2 (1- bb-2))
		      (hh-3 (1- bb-3)))
		  (begin
		    (cons-solution ll bb-1 hh-1 results-list)
		    (cons-solution ll bb-2 hh-2 results-list)
		    (cons-solution ll bb-3 hh-3 results-list)
		    )))

	      (if (> (length results-list) 0)
		  (begin
		    results-list)
		  (begin
		    #f
		    )))
	    (begin
	      #f
	      ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-calc-bb-1)
  (let ((sub-name "test-calc-bb-1")
	(test-list
	 (list
	  (list 17 5 (list (list 17 16 15)))
	  (list 305 5 (list (list 305 272 273)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((ll (list-ref this-list 0))
		 (nn (list-ref this-list 1))
		 (shouldbe-list (list-ref this-list 2)))
	     (let ((result-list (calc-bb ll nn)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : ll=~a, nn=~a, shouldbe=~a, result=~a, lengths not equal, shouldbe=~a, result=~a~%"
					  sub-name test-label-index
					  ll nn shouldbe-list result-list
					  slen rlen))
			 (quit)
			 ))
		   (for-each
		    (lambda (s-elem)
		      (begin
			(if (equal? (member s-elem result-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : ll=~a, nn=~a, shouldbe=~a, result=~a, discrepancy at shouldbe element =~a~%"
						  sub-name test-label-index
						  ll nn shouldbe-list result-list
						  s-elem))
			      (quit)
			      ))
			)) shouldbe-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; cc and dd solve the Pell's equation c^2 - 5d^2 = 1
;;; xx and yy solve the equation x^2 - 5y^2 = -1
(define (loop-over-positives xx-0 yy-0 cc-0 dd-0 nn max-num)
  (let ((result-list (list))
	(xx-1 xx-0)
	(yy-1 yy-0)
	(cc-1 cc-0)
	(dd-1 dd-0)
	(count 0)
	(continue-loop-flag #t))
    (begin
      (while
       (equal? continue-loop-flag #t)
       (begin
	 (let ((next-cc (+ (* cc-0 cc-1) (* nn dd-0 dd-1)))
	       (next-dd (+ (* cc-0 dd-1) (* dd-0 cc-1)))
	       (next-xx (+ (* xx-0 cc-1) (* nn yy-0 dd-1)))
	       (next-yy (+ (* xx-0 dd-1) (* cc-1 yy-0))))
	   (begin
	     (if (and (integer? yy-1) (> yy-1 0))
		 (begin
		   (let ((rlist-list (calc-bb yy-1 nn)))
		     (begin
		       (if (and (list? rlist-list) (> (length rlist-list) 0))
			   (begin
			     (for-each
			      (lambda (a-list)
				(begin
				  (let ((ll (list-ref a-list 0))
					(bb (list-ref a-list 1))
					(hh (list-ref a-list 2)))
				    (begin
				      (set! result-list (cons (list ll bb hh) result-list))
				      (set! count (1+ count))
				      ))
				  )) rlist-list)
			     ))
		       ))
		   ))

	     (set! cc-1 next-cc)
	     (set! dd-1 next-dd)
	     (set! xx-1 next-xx)
	     (set! yy-1 next-yy)

	     (if (>= count max-num)
		 (begin
		   (set! continue-loop-flag #f)
		   ))
	     ))
	 ))

      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-loop-over-positives-1)
  (let ((sub-name "test-loop-over-positives-1")
	(test-list
	 (list
	  (list 2 1 9 4 5 2 (list (list 17 16 15) (list 305 272 273)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((xx-0 (list-ref this-list 0))
		 (yy-0 (list-ref this-list 1))
		 (cc-0 (list-ref this-list 2))
		 (dd-0 (list-ref this-list 3))
		 (nn (list-ref this-list 4))
		 (max-num (list-ref this-list 5))
		 (shouldbe-list (list-ref this-list 6)))
	     (let ((result-list (loop-over-positives xx-0 yy-0 cc-0 dd-0 nn max-num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : xx=~a, yy=~a, cc=~a, dd=~a, nn=~a, max-num=~a, shouldbe=~a, result=~a, lengths not equal, shouldbe=~a, result=~a~%"
					  sub-name test-label-index
					  xx-0 yy-0 cc-0 dd-0 nn max-num
					  shouldbe-list result-list slen rlen))
			 (quit)
			 ))
		   (for-each
		    (lambda (s-elem)
		      (begin
			(if (equal? (member s-elem result-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : xx=~a, yy=~a, cc=~a, dd=~a, nn=~a, max-num=~a, shouldbe=~a, result=~a, discrepancy at shouldbe element =~a~%"
						  sub-name test-label-index
						  xx-0 yy-0 cc-0 dd-0 nn max-num
						  shouldbe-list result-list s-elem))
			      (quit)
			      ))
			)) shouldbe-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; cc and dd solve the Pell's equation cc^2 - 5d^2 = 1
;;; xx and yy solve the equation xx^2 - 5yy^2 = -1
(define (main-loop target-count debug-flag)
  (let ((count 0)
	(xx-0 2)
	(yy-0 1)
	(cc-0 9)
	(dd-0 4)
	(nn 5)
	(sum-ll 0)
	(results-list (list))
	(continue-loop-flag #t))
    (begin
      (let ((rlist1 (loop-over-positives xx-0 yy-0 cc-0 dd-0
					 nn target-count)))
	(begin
	  (set! results-list (sort rlist1
				   (lambda (a b)
				     (< (car a) (car b)))))

	  (let ((rlen (length results-list)))
	    (begin
	      (do ((ii 0 (1+ ii)))
		  ((or (>= ii rlen)
		       (>= ii target-count)))
		(begin
		  (let ((rlist (list-ref results-list ii)))
		    (let ((ll (list-ref rlist 0))
			  (bb (list-ref rlist 1))
			  (hh (list-ref rlist 2)))
		      (begin
			(set! sum-ll (+ sum-ll ll))

			(if (equal? debug-flag #t)
			    (begin
			      (display (ice9-format:format
					#f "  (~:d)  L = ~:d, b = ~:d, h = ~:d, sum so far = ~:d~%"
					(1+ ii) ll bb hh sum-ll))
			      (force-output)
			      ))
			)))
		  ))
	      ))

	  (display (ice9-format:format #f "The sum(L) = ~:d for the ~:d smallest isosceles triangles.~%"
				       sum-ll target-count))
	  (force-output)
	  ))
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
    (display (format #f "Project Euler 138 - Consider the isosceles triangle with base length, b = 16, and legs, L = 17.~%"))
    (newline)
    (display (format #f "By using the Pythagorean theorem it can be seen that the height of the triangle, h = sqrt(17^2 - 8^2) = 15, which is one less than the base length.~%"))
    (newline)
    (display (format #f "With b = 272 and L = 305, we get h = 273, which is one more than the base length, and this is the second smallest isosceles triangle with the property that h = b +/- 1.~%"))
    (newline)
    (display (format #f "Find Sum(L) for the twelve smallest isosceles triangles for which h = b +/- 1 and b, L are positive integers.~%"))
    (newline)
    (display (format #f "From the Pythagorean theorem, L^2 = (b/2)^2 + h^2, and h = b +/- 1, we have L^2 = b^2/4 + (b +/- 1)^2 = b^2/4 + b^2 +/- 2b + 1 = 5b^2/4 +/- 2b + 1.~%"))
    (newline)
    (display (format #f "b^2 +/- 8b/5 - 4(L^2 - 1)/5 = 0~%"))
    (newline)
    (display (format #f "Using the quadratic formula, when 8b/5 is positive (h=b+1), b = -4/5 +/- sqrt(64/25+16(L^2-1)/5)/2 = -4/5 +/- 2sqrt(4+5L^2-5)/5 = -4/5 +/- 2sqrt(5L^2-1)/5.  When 8/5 is negative (h=b-1), b = +4/5 +/- 2sqrt(5L^2-1)/5.~%"))
    (display (format #f "We can use Pell's equation to find those values of L for which 5L^2 - 1 is square.  Let 5y^2 - 1 = x^2, where x an integer and y=L, then x^2 - 5y^2 = -1. See http://mathworld.wolfram.com/PellEquation.html.~%"))
    (newline)
    (display (format #f "Initial solutions found (x=2, y=1), (x=38, y=17), (x=682, y=305).~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-calc-bb-1 counter)
	   (run-test test-loop-over-positives-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)
;;;    (find-initial-solutions)

    (let ((target-count 2)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop target-count debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-count 12)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop target-count debug-flag)
	   ))
	))

    (newline)
    ))
