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
  (let ((datetime (srfi-19:current-date)))
    (let ((s1 (srfi-19:date->string datetime "~A, ~B ~d, ~Y"))
	  (s2 (string-downcase (srfi-19:date->string datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (is-num-in-list? a-num a-list-list)
  (let ((in-list-flag #f)
	(a-len (length a-list-list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((or (>= ii a-len)
	       (equal? in-list-flag #t)))
	(begin
	  (let ((b-list (list-ref a-list-list ii)))
	    (begin
	      (if (not (equal? (member a-num b-list) #f))
		  (begin
		    (set! in-list-flag #t)
		    ))
	      ))
	  ))

      in-list-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-num-in-list-1)
  (let ((sub-name "test-is-num-in-list-1")
	(test-list
	 (list
	  (list 2 (list (list 1 1 2)) #t)
	  (list 3 (list (list 1 1 2)) #f)
	  (list 3 (list (list 2 1 3) (list 1 1 2)) #t)
	  (list 5 (list (list 2 1 3) (list 1 1 2)) #f)
	  (list 4 (list (list 2 2 4) (list 2 1 3) (list 1 1 2)) #t)
	  (list 5 (list (list 2 2 4) (list 2 1 3) (list 1 1 2)) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((a-num (list-ref alist 0))
		 (llist (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (is-num-in-list? a-num llist)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : a-num = ~a, llist = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index a-num llist
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
(define (dynamic-addition-chains max-num)
  (let ((sum-array (make-array (list) (1+ max-num))))
    (begin
      (array-set! sum-array (list (list (list 1 1 2))) 2)
      (array-set! sum-array (list (list (list 2 1 3) (list 1 1 2))) 3)
      (array-set! sum-array (list (list (list 2 2 4) (list 1 1 2))) 4)

      (do ((nn 5 (1+ nn)))
	  ((> nn max-num))
	(begin
	  (let ((end-mm (euclidean/ nn 2))
		(nn-list (list))
		(nn-len -1))
	    (begin
	      (do ((mm-1 (1- nn) (1- mm-1)))
		  ((< mm-1 end-mm))
		(begin
		  (let ((mm-2 (- nn mm-1)))
		    (begin
		      (if (and (>= mm-1 mm-2)
			       (>= mm-1 2))
			  (begin
			    (let ((this-list (array-ref sum-array mm-1))
				  (result-list (list))
				  (result-len -1)
				  (tlist (list mm-1 mm-2 nn)))
			      (begin
				(for-each
				 (lambda (a-list)
				   (begin
				     (if (is-num-in-list? mm-2 a-list)
					 (begin
					   (let ((next-list (cons tlist a-list)))
					     (begin
					       (if (equal? (member next-list result-list) #f)
						   (begin
						     (set! result-list (cons next-list result-list))
						     (set! result-len (length next-list))
						     ))
					       ))
					   ))
				     )) this-list)

				(if (or (< nn-len 0)
					(and (> result-len 0)
					     (< result-len nn-len)))
				    (begin
				      (set! nn-list result-list)
				      (set! nn-len result-len))
				    (begin
				      (if (= result-len nn-len)
					  (begin
					    (set! nn-list (append nn-list result-list))
					    ))
				      ))
				))
			    ))
		      ))
		  ))

	      (array-set! sum-array nn-list nn)
	      ))
	  ))

      sum-array
      )))

;;;#############################################################
;;;#############################################################
(define (test-dynamic-addition-chains-1)
  (let ((sub-name "test-dynamic-addition-chains-1")
	(test-list
	 (list
	  (list 2 (list (list 1 1 2)))
	  (list 3 (list (list 2 1 3) (list 1 1 2)))
	  (list 4 (list (list 2 2 4) (list 1 1 2)))
	  (list 5 (list (list 3 2 5) (list 2 1 3) (list 1 1 2)))
	  (list 5 (list (list 4 1 5) (list 2 2 4) (list 1 1 2)))
	  (list 6 (list (list 3 3 6) (list 2 1 3) (list 1 1 2)))
	  (list 6 (list (list 4 2 6) (list 2 2 4) (list 1 1 2)))
	  (list 7 (list (list 5 2 7) (list 3 2 5) (list 2 1 3)
			(list 1 1 2)))
	  (list 7 (list (list 5 2 7) (list 4 1 5) (list 2 2 4)
			(list 1 1 2)))
	  (list 7 (list (list 6 1 7) (list 3 3 6) (list 2 1 3)
			(list 1 1 2)))
	  (list 7 (list (list 6 1 7) (list 4 2 6) (list 2 2 4)
			(list 1 1 2)))
	  (list 8 (list (list 4 4 8) (list 2 2 4) (list 1 1 2)))
	  (list 9 (list (list 5 4 9) (list 4 1 5) (list 2 2 4)
			(list 1 1 2)))
	  (list 9 (list (list 6 3 9) (list 3 3 6) (list 2 1 3)
			(list 1 1 2)))
	  (list 9 (list (list 8 1 9) (list 4 4 8) (list 2 2 4)
			(list 1 1 2)))
	  (list 10 (list (list 5 5 10) (list 3 2 5) (list 2 1 3)
			 (list 1 1 2)))
	  (list 10 (list (list 5 5 10) (list 4 1 5) (list 2 2 4)
			 (list 1 1 2)))
	  (list 10 (list (list 6 4 10) (list 4 2 6) (list 2 2 4)
			 (list 1 1 2)))
	  (list 10 (list (list 8 2 10) (list 4 4 8) (list 2 2 4)
			 (list 1 1 2)))
	  (list 11 (list (list 7 4 11) (list 6 1 7) (list 4 2 6)
			 (list 2 2 4) (list 1 1 2)))
	  (list 11 (list (list 7 4 11) (list 5 2 7) (list 4 1 5)
			 (list 2 2 4) (list 1 1 2)))
	  (list 11 (list (list 10 1 11) (list 5 5 10) (list 3 2 5)
			 (list 2 1 3) (list 1 1 2)))
	  (list 11 (list (list 10 1 11) (list 5 5 10) (list 4 1 5)
			 (list 2 2 4) (list 1 1 2)))
	  (list 11 (list (list 10 1 11) (list 6 4 10) (list 4 2 6)
			 (list 2 2 4) (list 1 1 2)))
	  (list 11 (list (list 10 1 11) (list 8 2 10) (list 4 4 8)
			 (list 2 2 4) (list 1 1 2)))
	  (list 12 (list (list 6 6 12) (list 3 3 6) (list 2 1 3) (list 1 1 2)))
	  (list 12 (list (list 6 6 12) (list 4 2 6) (list 2 2 4) (list 1 1 2)))
	  (list 12 (list (list 8 4 12) (list 4 4 8) (list 2 2 4) (list 1 1 2)))
	  (list 13 (list (list 12 1 13) (list 6 6 12) (list 3 3 6) (list 2 1 3)
			 (list 1 1 2)))
	  (list 13 (list (list 12 1 13) (list 6 6 12) (list 4 2 6) (list 2 2 4)
			 (list 1 1 2)))
	  (list 13 (list (list 10 3 13) (list 5 5 10) (list 3 2 5) (list 2 1 3)
			 (list 1 1 2)))
	  (list 14 (list (list 12 2 14) (list 6 6 12) (list 3 3 6) (list 2 1 3)
			 (list 1 1 2)))
	  (list 14 (list (list 12 2 14) (list 6 6 12) (list 4 2 6) (list 2 2 4)
			 (list 1 1 2)))
	  (list 14 (list (list 7 7 14) (list 5 2 7) (list 3 2 5) (list 2 1 3)
			 (list 1 1 2)))
	  (list 14 (list (list 7 7 14) (list 6 1 7) (list 3 3 6) (list 2 1 3)
			 (list 1 1 2)))
	  (list 15 (list (list 10 5 15) (list 5 5 10) (list 3 2 5)
			 (list 2 1 3) (list 1 1 2)))
	  (list 15 (list (list 10 5 15) (list 5 5 10) (list 4 1 5)
			 (list 2 2 4) (list 1 1 2)))
	  (list 15 (list (list 12 3 15) (list 6 6 12) (list 3 3 6)
			 (list 2 1 3) (list 1 1 2)))
	  (list 16 (list (list 8 8 16) (list 4 4 8) (list 2 2 4) (list 1 1 2)))
	  (list 17 (list (list 9 8 17) (list 8 1 9) (list 4 4 8) (list 2 2 4)
			 (list 1 1 2)))
	  (list 17 (list (list 16 1 17) (list 8 8 16) (list 4 4 8) (list 2 2 4)
			 (list 1 1 2)))
	  (list 18 (list (list 9 9 18) (list 5 4 9) (list 4 1 5)
			 (list 2 2 4) (list 1 1 2)))
	  (list 18 (list (list 12 6 18) (list 6 6 12) (list 3 3 6)
			 (list 2 1 3) (list 1 1 2)))
	  (list 18 (list (list 16 2 18) (list 8 8 16) (list 4 4 8)
			 (list 2 2 4) (list 1 1 2)))
	  (list 19 (list (list 11 8 19) (list 9 2 11) (list 8 1 9)
			 (list 4 4 8) (list 2 2 4) (list 1 1 2)))
	  (list 19 (list (list 13 6 19) (list 7 6 13) (list 6 1 7)
			 (list 3 3 6) (list 2 1 3) (list 1 1 2)))
	  (list 19 (list (list 18 1 19) (list 9 9 18) (list 8 1 9)
			 (list 4 4 8) (list 2 2 4) (list 1 1 2)))
	  (list 20 (list (list 10 10 20) (list 5 5 10) (list 3 2 5)
			 (list 2 1 3) (list 1 1 2)))
	  (list 20 (list (list 10 10 20) (list 6 4 10) (list 4 2 6)
			 (list 2 2 4) (list 1 1 2)))
	  (list 20 (list (list 12 8 20) (list 8 4 12) (list 4 4 8)
			 (list 2 2 4) (list 1 1 2)))
	  ))
	(max-nn 20)
	(test-label-index 0))
    (begin
      (let ((result-array (dynamic-addition-chains max-nn)))
	(begin
	  (for-each
	   (lambda (alist)
	     (begin
	       (let ((nn (list-ref alist 0))
		     (shouldbe-list-list (list-ref alist 1)))
		 (let ((result-list-list (array-ref result-array nn)))
		   (let ((slen (length shouldbe-list-list))
			 (rlen (length (car result-list-list))))
		     (begin
		       (if (not (equal? slen rlen))
			   (begin
			     (display (format #f "~a : (~a) : error : nn = ~a, shouldbe = ~a, result = ~a, lengths not equal, shouldbe = ~a, result = ~a~%"
					      sub-name test-label-index nn
					      shouldbe-list-list result-list-list
					      slen rlen))
			     ))

		       (for-each
			(lambda (s-list)
			  (begin
			    (let ((rlen (length result-list-list))
				  (ok-flag #f))
			      (begin
				(do ((ii 0 (1+ ii)))
				    ((or (>= ii rlen)
					 (equal? ok-flag #t)))
				  (begin
				    (let ((rr-list (list-ref result-list-list ii)))
				      (begin
					(if (not (equal? (member s-list rr-list) #f))
					    (begin
					      (set! ok-flag #t)
					      ))
					))
				    ))

				(if (equal? ok-flag #f)
				    (begin
				      (display (format #f "~a : (~a) : error : nn = ~a, shouldbe = ~a, result = ~a, discrepancy at ~a~%"
						       sub-name test-label-index nn
						       shouldbe-list-list result-list-list
						       s-list))
				      (quit)
				      ))
				))
			    )) shouldbe-list-list)
		       ))
		   ))

	       (set! test-label-index (1+ test-label-index))
	       ))
	   test-list)
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((sum-array (dynamic-addition-chains max-num))
	(sum 0))
    (begin
      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (let ((min-sum-list (array-ref sum-array ii)))
	    (let ((first-list (car min-sum-list)))
	      (let ((min-len (length first-list)))
		(begin
		  (set! sum (+ sum min-len))

		  (if (equal? debug-flag #t)
		      (begin
			(display (ice9-format:format #f "    (~:d) min length = ~:d : sum so far = ~:d : ~a~%"
						     ii min-len sum (current-date-time-string)))
			(force-output)

			(for-each
			 (lambda (a-list)
			   (begin
			     (let ((exp-1 (list-ref a-list 0))
				   (exp-2 (list-ref a-list 1))
				   (exp-3 (list-ref a-list 2)))
			       (begin
				 (display (ice9-format:format #f "    n^~:d x n^~:d = n^~:d~%"
							      exp-1 exp-2 exp-3))
				 ))
			     )) (reverse first-list))
			(newline)
			(force-output)
			))
		  ))
	      ))
	  ))

      (display (ice9-format:format #f "Sum(m(k)) = ~:d, for 1 <= k <= ~:d~%"
				   sum max-num))
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
    (display (format #f "Project Euler 122 - The most naive way of computing n^15 requires fourteen multiplications:~%"))
    (newline)
    (display (format #f "    n x n x ... x n = n^15~%"))
    (newline)
    (display (format #f "But using a 'binary' method you can compute it in six multiplications:~%"))
    (newline)
    (display (format #f "    n x n = n^2~%"))
    (display (format #f "    n^2 x n^2 = n^4~%"))
    (display (format #f "    n^4 x n^4 = n^8~%"))
    (display (format #f "    n^8 x n^4 = n^12~%"))
    (display (format #f "    n^12 x n^2 = n^14~%"))
    (display (format #f "    n^14 x n = n^15~%"))
    (newline)
    (display (format #f "However it is yet possible to compute it in only five multiplications:~%"))
    (newline)
    (display (format #f "    n x n = n^2~%"))
    (display (format #f "    n^2 x n = n^3~%"))
    (display (format #f "    n^3 x n^3 = n^6~%"))
    (display (format #f "    n^6 x n^6 = n^12~%"))
    (display (format #f "    n^12 x n^3 = n^15~%"))
    (newline)
    (display (format #f "We shall define m(k) to be the minimum number of multiplications to compute nk; for example m(15) = 5.~%"))
    (newline)
    (display (format #f "For 1 <= k <= 200, find Sum(m(k)).~%"))
    (newline)
    (display (format #f "To understand this problem, see the following article on the shortest addition chains, http://wwwhomes.uni-bielefeld.de/achim/addition_chain.html~%"))
    (newline)
    (display (format #f "The number of possible additions (starting from 1 + 1 = 2), are too large as k gets big. The key to making this program go fast, is to work backwards, (starting from 10 + 10 = 20). For example, 9 = 5+4 = 6+3 = 7+2 = 8+1, then you need to check a few possibilities.  The next key is to use a dynamic programming approach, and finally, to make sure that if 9=5+4, that the sequence of 5 contains 4, so that you don't have to add extra additions to make 4.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-is-num-in-list-1 counter)
	   (run-test test-dynamic-addition-chains-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 20)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-num 200)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
