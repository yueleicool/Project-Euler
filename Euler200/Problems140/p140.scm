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
;;; initial solutions for Pell's equation a^2-5b^2 = 44
(define (find-initial-solutions)
  (let ((nn 5)
	(diff 44)
	(max-aa 1000)
	(max-bb 1000))
    (begin
      (do ((aa 1 (1+ aa)))
	  ((> aa max-aa))
	(begin
	  (let ((aa-2 (* aa aa)))
	    (begin
	      (do ((bb 1 (1+ bb)))
		  ((> bb max-bb))
		(begin
		  (let ((bb-2 (* nn bb bb)))
		    (begin
		      (if (= (- aa-2 bb-2) diff)
			  (begin
			    (let ((kk (/ (- aa 7) nn)))
			      (begin
				(if (integer? kk)
				    (begin
				      (display (ice9-format:format #f "(*****) aa=~:d, bb=~:d, kk=~a~%"
								   aa bb kk)))
				    (begin
				      (display (ice9-format:format #f "  aa=~:d, bb=~:d, kk=~a~%"
								   aa bb kk))
				      ))
				(force-output)
				))
			    ))
		      ))
		  ))
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
;;; rational solutions to x = -(k+1)/(2(k+3)) +/- sqrt((k+1)^2+4k(k+3))/(2(k+3))
(define (quadratic-solution kk)
  (let ((kp1 (1+ kk))
	(kp3 (+ kk 3)))
    (let ((kk-2 (* kp1 kp1))
	  (fkkp3 (* 4 kk kp3)))
      (let ((square-term (+ kk-2 fkkp3)))
	(let ((sqrt-term (exact-integer-sqrt square-term)))
	  (begin
	    (if (= square-term (* sqrt-term sqrt-term))
		(begin
		  (let ((aterm (- sqrt-term kp1)))
		    (let ((bterm (/ aterm (* 2 kp3))))
		      (begin
			bterm
			))
		    ))
		(begin
		  #f
		  ))
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-quadratic-solution-1)
  (let ((sub-name "test-quadratic-solution-1")
	(test-list
	 (list
	  (list 1 #f) (list 2 2/5) (list 3 #f)
	  (list 4 #f) (list 5 1/2)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((kk (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (quadratic-solution kk)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : kk=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index kk
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
;;; cc and dd solve the Pell's equation cc^2 - 5d^2 = 1
;;; aa and bb solve the equation aa^2 - 5bb^2 = 45
(define (loop-over-positives aa-0 bb-0 cc-0 dd-0 nn max-count)
  (let ((result-list (list))
	(aa-1 aa-0)
	(bb-1 bb-0)
	(cc-1 cc-0)
	(dd-1 dd-0)
	(count 0)
	(continue-loop-flag #t))
    (begin
      (while
       (equal? continue-loop-flag #t)
       (begin
	 (let ((kk (/ (- aa-1 7) nn))
	       (next-cc-p (+ (* cc-0 cc-1) (* nn dd-0 dd-1)))
	       (next-dd-p (+ (* cc-0 dd-1) (* dd-0 cc-1)))
	       (next-aa-p (+ (* aa-0 cc-1) (* nn bb-0 dd-1)))
	       (next-bb-p (+ (* aa-0 dd-1) (* cc-1 bb-0))))
	   (begin
	     (if (and (integer? kk) (> kk 0))
		 (begin
		   (set! result-list (cons kk result-list))
		   (set! count (1+ count))
		   ))

	     (set! cc-1 next-cc-p)
	     (set! dd-1 next-dd-p)

	     (set! aa-1 next-aa-p)
	     (set! bb-1 next-bb-p)

	     (if (>= count max-count)
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
	  (list 13 5 9 4 5 2 (list 13970 42))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((aa-0 (list-ref this-list 0))
		 (bb-0 (list-ref this-list 1))
		 (cc-0 (list-ref this-list 2))
		 (dd-0 (list-ref this-list 3))
		 (nn (list-ref this-list 4))
		 (max-num (list-ref this-list 5))
		 (shouldbe-list (list-ref this-list 6)))
	     (let ((result-list (loop-over-positives aa-0 bb-0 cc-0 dd-0 nn max-num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : aa=~a, bb=~a, cc=~a, dd=~a, nn=~a, max-num=~a, shouldbe=~a, result=~a, lengths not equal, shouldbe=~a, result=~a~%"
					  sub-name test-label-index
					  aa-0 bb-0 cc-0 dd-0 nn max-num
					  shouldbe-list result-list slen rlen))
			 (quit)
			 ))
		   (for-each
		    (lambda (s-elem)
		      (begin
			(if (equal? (member s-elem result-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : aa=~a, bb=~a, cc=~a, dd=~a, nn=~a, max-num=~a, shouldbe=~a, result=~a, discrepancy at shouldbe element =~a~%"
					       sub-name test-label-index
					       aa-0 bb-0 cc-0 dd-0 nn max-num
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
(define (main-loop target-nugget debug-flag)
  (let ((count 0)
	(aa-init-list
	 (list (list 7 1) (list 8 2) (list 13 5)
	       (list 17 7) (list 32 14) (list 43 19)))
	(cc-0 9)
	(dd-0 4)
	(nn 5)
	(nugget-sum 0)
	(kk 44)
	(result-list (list))
	(continue-loop-flag #t))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((aa (list-ref a-list 0))
		 (bb (list-ref a-list 1)))
	     (begin
	       (let ((rlist (loop-over-positives aa bb cc-0 dd-0
						 nn target-nugget)))
		 (begin
		   (set! result-list (sort
				      (srfi-1:delete-duplicates
				       (append rlist result-list)) <))
		   ))
	       ))
	   )) aa-init-list)

      (let ((rlen (length result-list)))
	(begin
	  (do ((ii 0 (1+ ii)))
	      ((or (>= ii rlen)
		   (>= ii target-nugget)))
	    (begin
	      (let ((kk (list-ref result-list ii)))
		(begin
		  (set! nugget-sum (+ nugget-sum kk))
		  (set! count (1+ count))

		  (if (equal? debug-flag #t)
		      (begin
			(let ((xx (quadratic-solution kk)))
			  (begin
			    (display (ice9-format:format #f "  (~:d)  AG(~a) = ~:d, sum so far = ~:d~%"
							 (1+ ii) xx kk nugget-sum))
			    (force-output)
			    ))
			))
		  ))
	      ))
	  ))

      (display (ice9-format:format
		#f "The sum of the AG(x) gold nuggets = ~:d (first ~:d nuggets)~%"
		nugget-sum count))
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
    (display (format #f "Project Euler 140 - Consider the infinite polynomial series AG(x) = xG1 + x^2G2 + x^3G3 + ..., where Gk is the kth term of the second order recurrence relation Gk = Gk-1 + Gk-2, G1 = 1 and G2 = 4; that is, 1, 4, 5, 9, 14, 23, ... .~%"))
    (newline)
    (display (format #f "For this problem we shall be concerned with values of x for which AG(x) is a positive integer.~%"))
    (newline)
    (display (format #f "The corresponding values of x for the first five natural numbers are shown below.~%"))
    (newline)
    (display (format #f "    x             AG(x)~%"))
    (display (format #f "(sqrt(5)-1)/4      1~%"))
    (display (format #f "    2/5            2~%"))
    (display (format #f "(sqrt(22)-2)/6     3~%"))
    (display (format #f "(sqrt(137)-5)/14   4~%"))
    (display (format #f "    1/2            5~%"))
    (newline)
    (display (format #f "We shall call AG(x) a golden nugget if x is rational, because they become increasingly rarer; for example, the 20th golden nugget is 211345365.~%"))
    (newline)
    (display (format #f "Find the sum of the first thirty golden nuggets.~%"))
    (newline)
    (display (format #f "The solution uses the same method as http://ulcar.uml.edu/~~iag/CS/Fibonacci.html, which describes a closed form expression for the Fibonacci generating function.~%"))
    (newline)
    (display (format #f "Using the same derivation technique: AG(x) = xG(1) + x^2G(2) + x^3G(3)+.... = Sum(x^k * G(k))_(k=1->infinity). The recurrance relation G(k)=G(k-1)+G(k-2) leads to xG(1) + x^2G(2) + Sum((G(k-1) + G(k-2))*x^k)_(k=3->infinity) = xG(1) + x^2G(2) + xSum(G(k)x^k)_(k=2->infinity) + x^2Sum(G(k)x^(k))_(k=1->infinity)~%"))
    (newline)
    (display (format #f "Now the term xSum(G(k)x^k)_(k=2->infinity) is a little tricky, to convert iit into a sum like AG(x), we need to add and subtract a term, so xSum(G(k)x^k)_(k=2->infinity) = xSum(G(k)x^k)_(k=2->infinity) + G(1)x^2 - G(1)x^2 = xSum(G(k)x^k)_(k=1->infinity) - G(1)x^2.~%"))
    (newline)
    (display (format #f "Finally, AG(x) = xG(1) + x^2G(2) + x^3G(3) + ... = xG(1) + x^2G(2) + xAG(x) - G(1)x^2 + x^2AG(x) = xG(1) + (G(2) - G(1))x^2 + (x + x^2)AG(x).  Rearranging, AG(x) = (xG(1) + (G(2) - G(1))x^2)/(1 - x - x^2)~%"))
    (newline)
    (display (format #f "AG(x) = (x + 3x^2)/(1 - x - x^2), since G(1)=1 and G(2)=4.~%"))
    (newline)
    (display (format #f "Here we are interested in rational values of x which make AG(x) an integer, so AG(x) = k = (x + 3x^2)/(1 - x - x^2).~%"))
    (newline)
    (display (format #f "Re-arranging we get (k+3)x^2 + (k+1)x - k = 0.~%"))
    (newline)
    (display (format #f "The quadratic formula gives: x = -(k+1)/(2(k+3)) + sqrt((k+1)^2+4k(k+3))/(2(k+3)), positive solutions only since x>0.~%"))
    (newline)
    (display (format #f "When k=1, x=-1/4+sqrt(4+16)/8=(sqrt(5)-1)/4.~%"))
    (display (format #f "When k=2, x=-3/10+sqrt(9+40)/10=-3/10 + 7/10=2/5.~%"))
    (display (format #f "When k=3, x=-1/3+sqrt(16+72)/12=(sqrt(22)-2)/6.~%"))
    (display (format #f "When k=4, x=-5/14+sqrt(25+112)/14=(sqrt(137)-5)/14.~%"))
    (display (format #f "When k=5, x=-3/8+sqrt(36+160)/10=-3/8+14/16=1/2.~%"))
    (newline)
    (display (format #f "Look for those solutions of x where the term in the square root is a square, or (k+1)^2+4k(k+3)=b^2.  k^2+2k+1+4k^2+12k = 5k^2+14k+1 = b^2, then multiply through by 5, 25k^2+70k+5 = 5b^2.  Complete the square (5k+7)^2 - 49 + 5 = 5b^2, and a^2 - 5b^2 = 44, where a=5k+7.  This is a Pell's type equation, and we find an initial solutions (a=7, b=1), (a=8, b=2), (a=13, b=5), (a=17, b=7), ..., which satisfies a^2-5b^2=44, and the initial solution (c=9, b=4) satisfies c^2-5b^2=1 (Pell's equation), from which we can find all solutions of a^2-5b^2=44.  Finally transform back to find k = (a-7)/5.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-loop-over-positives-1 counter)
	   (run-test test-quadratic-solution-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)
;;;    (find-initial-solutions)

    (let ((target-nugget 20)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop target-nugget debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-nugget 30)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop target-nugget debug-flag)
	   ))
	))

    (newline)
    ))
