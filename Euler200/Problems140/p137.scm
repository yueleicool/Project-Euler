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
;;; initial solutions for Pell's equation a^2-5b^2 = -4
(define (find-initial-solutions)
  (let ((nn 5)
	(kk -4)
	(max-aa 2000)
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
		      (if (= (- aa-2 bb-2) kk)
			  (begin
			    (display (ice9-format:format #f "aa=~:d, bb=~:d, kk=~a~%"
							 aa bb kk))
			    (force-output)
			    ))
		      ))
		  ))
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
;;; rational solutions to x = -(k+1)/(2k) +/- sqrt((k+1)^2+4k^2)/2k
(define (quadratic-solution kk)
  (let ((kp1 (1+ kk)))
    (let ((kp1-2 (* kp1 kp1))
	  (kk-2 (* 4 kk kk)))
      (let ((square-term (+ kp1-2 kk-2)))
	(let ((sqrt-term (exact-integer-sqrt square-term)))
	  (begin
	    (if (= square-term (* sqrt-term sqrt-term))
		(begin
		  (let ((aterm (- sqrt-term kp1)))
		    (let ((bterm (/ aterm (* 2 kk))))
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
	  (list 1 #f) (list 2 1/2) (list 3 #f)
	  (list 4 #f) (list 5 #f)
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
;;; aa and bb solve the equation aa^2 - 5bb^2 = -4
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
	 (let ((kk (/ (1- aa-1) nn))
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
	  (list 1 1 9 4 5 2 (list 104 33552))
	  (list 4 2 9 4 5 2 (list 15 4895))
	  (list 11 5 9 4 5 2 (list 2 714))
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
	(aa-0 1)
	(bb-0 1)
	(aa-1 4)
	(bb-1 2)
	(aa-2 11)
	(bb-2 5)
	(cc-0 9)
	(dd-0 4)
	(nn 5)
	(result-list (list)))
    (begin
      (let ((rlist1 (loop-over-positives aa-0 bb-0 cc-0 dd-0
					 nn target-nugget))
	    (rlist2 (loop-over-positives aa-1 bb-1 cc-0 dd-0
					 nn target-nugget))
	    (rlist3 (loop-over-positives aa-2 bb-2 cc-0 dd-0
					 nn target-nugget)))
	(begin
	  (set! result-list (sort (srfi-1:delete-duplicates
				   (append rlist1 rlist2 rlist3)) <))

	  (if (equal? debug-flag #t)
	      (begin
		(let ((rlen (length result-list)))
		  (begin
		    (do ((ii 0 (1+ ii)))
			((or (>= ii rlen)
			     (>= ii target-nugget)))
		      (begin
			(let ((kk (list-ref result-list ii)))
			  (let ((xx (quadratic-solution kk)))
			    (begin
			      (display (ice9-format:format #f "  (~:d)  AF(~a) = ~:d~%"
							   (1+ ii) xx kk))
			      (force-output)
			      )))
			))
		    ))
		))

	  (let ((result-kk (list-ref result-list (1- target-nugget))))
	    (begin
	      (display (ice9-format:format #f "The ~:d-th gold nugget = ~:d~%"
					   target-nugget result-kk))
	      (force-output)
	      ))
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
    (display (format #f "Project Euler 137 - Consider the infinite polynomial series AF(x) = xF1 + x^2F2 + x^3F3 + ..., where Fk is the kth term in the Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ; that is, Fk = Fk-1 + Fk-2, F1 = 1 and F2 = 1.~%"))
    (newline)
    (display (format #f "For this problem we shall be interested in values of x for which AF(x) is a positive integer.~%"))
    (newline)
    (display (format #f "Surprisingly AF(1/2) = (1/2)*1 + (1/2)^2*1 + (1/2)^3*2 + (1/2)^4*3 + (1/2)^5*5 + ... = 1/2 + 1/4 + 2/8 + 3/16 + 5/32 + ... = 2~%"))
    (newline)
    (display (format #f "The corresponding values of x for the first five natural numbers are shown below.~%"))
    (newline)
    (display (format #f "    x          AF(x)~%"))
    (display (format #f "  sqrt(2)-1      1~%"))
    (display (format #f "    1/2          2~%"))
    (display (format #f "(sqrt(13)-2)/3   3~%"))
    (display (format #f "(sqrt(89)-5)/8   4~%"))
    (display (format #f "(sqrt(34)-3)/5   5~%"))
    (newline)
    (display (format #f "We shall call AF(x) a golden nugget if x is rational, because they become increasingly rarer; for example, the 10th golden nugget is 74049690.~%"))
    (newline)
    (display (format #f "Find the 15th golden nugget.~%"))
    (newline)
    (display (format #f "The solution was found at http://ulcar.uml.edu/~~iag/CS/Fibonacci.html, which describes a closed form expression for the Fibonacci generating function.~%"))
    (newline)
    (display (format #f "AF(x) = x/(1 - x - x^2)~%"))
    (newline)
    (display (format #f "Here we are interested in values of x which make AF(x) an integer, so AF(x) = k = x/(1 - x - x^2).~%"))
    (newline)
    (display (format #f "Re-arranging we get kx^2 + (k+1)x - k = 0.~%"))
    (newline)
    (display (format #f "The quadratic equation gives: x = -(k+1)/(2k) +/- sqrt((k+1)^2+4k^2)/2k~%"))
    (newline)
    (display (format #f "When k=1, x=-1+sqrt(4+4)/2=sqrt(2)-1.~%"))
    (display (format #f "When k=2, x=-3/4+sqrt(9+16)/4=-3/4 + 5/4=1/2.~%"))
    (display (format #f "When k=3, x=-2/3+sqrt(16+36)/6=(sqrt(13)-2)/3.~%"))
    (display (format #f "When k=4, x=-5/8+sqrt(25+64)/8=(sqrt(89)-5)/8.~%"))
    (display (format #f "When k=5, x=-3/5+sqrt(36+100)/10=(sqrt(34)-3)/5.~%"))
    (newline)
    (display (format #f "Iterating over k is time consuming, so it's better to look for possible solutions when the sqrt((k+1)^2+4k^2) is an integer.  Let b an integer such that b^2 = (k+1)^2+4k^2 = k^2+2k+1+4k^2 = 5k^2+2k+1.  Multiplying through by 5, and rewriting, 25k^2 + 10k + 1 + 4 = 5b^2.  Then (5k + 1)^2 - 5b^2 = -4.  Set a=(5k+1), then the equation becomes a^2 - 5b^2 = -4, where k=(a-1)/5, and k and b are integers, and will make the expression in the square root (k+1)^2+4k^2 a perfect square.~%"))
    (newline)
    (display (format #f "To solve the Pell's equation, we take the base equation a^2 - 5b^2 = -4, with initial solution (a0=11, b0=5), and generate all solutions of the ordinary Pell equation c^2-5d^2=1, (with initial solution (c=9, d=4), and (a0^2-5b0^2)*(c^2-5d^2) = -4. (see http://mathworld.wolfram.com/PellEquation.html).~%"))
    (newline)
    (display (format #f "One last caveat, given an (a, b) pair, one can generate subsequent solutions of Pell's equation, however you don't generate all possible solutions.  The first three solutions needed to be identified (a=1, b=1), (a=4, b=2), and (a=11, b=5) in order to generate all solutions of AF(x)=integer.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-quadratic-solution-1 counter)
	   (run-test test-loop-over-positives-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)
;;;    (find-initial-solutions)

    (let ((target-nugget 10)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop target-nugget debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-nugget 15)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop target-nugget debug-flag)
	   ))
	))

    (newline)
    ))
