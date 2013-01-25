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

;;;### srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

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
;;; assume points-list = (p0 p1 p2 p3 ...)
;;; and p0 = f(1), p1 = f(2), p2 = f(3), p3 = f(4) ...
(define (nevilles-algorithm points-list nn)
  (let ((points-array (list->array 1 points-list))
	(max-loops (- (length points-list) 1))
	(max-points (- (length points-list) 1)))
    (begin
      (do ((loop 0 (1+ loop)))
	  ((>= loop max-loops))
	(begin
	  (let ((arr-index (- loop 1)))
	    (begin
	      (do ((ii 0 (1+ ii)))
		  ((>= ii max-points))
		(begin
		  (let ((t0 ii)
			(t1 (+ ii 1 loop)))
		    (let ((p0 (array-ref points-array ii))
			  (p1 (array-ref points-array (+ ii 1))))
		      (let ((denom (- t1 t0)))
			(let ((factor1 (- t1 nn))
			      (factor2 (- nn t0)))
			  (let ((p0prime (/ (+
					     (* p0 factor1)
					     (* p1 factor2))
					    denom)))
			    (begin
			      (array-set! points-array p0prime ii)
			      )))
			)))
		  ))
	      ))
	  ))
      (array-ref points-array 0)
      )))

;;;#############################################################
;;;#############################################################
(define (test-nevilles-algorithm-1)
  (let ((sub-name "test-nevilles-algorithm-1")
	(test-list
	 (list
	  (list (list 1) 1 1)
	  (list (list 1 8) 2 15)
	  (list (list 1 8 27) 3 58)
	  (list (list 1 8 27 64) 4 125)
	  (list (list 1 8 27 64 125) 5 216)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((points-list (list-ref this-list 0))
		 (nn (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (nevilles-algorithm points-list nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : points list = ~a, nn = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index points-list nn shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (third-degree-polynomial nn)
  (let ((sum (* nn nn nn)))
    (begin
      sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-third-degree-polynomial-1)
  (let ((sub-name "test-third-degree-polynomial-1")
	(test-list
	 (list
	  (list 1 1)
	  (list 2 8)
	  (list 3 27)
	  (list 4 64)
	  (list 5 125)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((nn (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (third-degree-polynomial nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error for test num = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index nn shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (tenth-degree-polynomial nn)
  (let ((coefficents (list 1 -1 1 -1 1
			   -1 1 -1 1 -1 1))
	(sum 0))
    (begin
      (do ((jj 0 (1+ jj)))
	  ((> jj 10))
	(begin
	  (let ((this-coeff (list-ref coefficents jj)))
	    (begin
	      (set! sum (+ (* nn sum) this-coeff))
	      ))
	  ))
      sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-tenth-degree-polynomial-1)
  (let ((sub-name "test-tenth-degree-polynomial-1")
	(test-list
	 (list
	  (list 1 1)
	  (list 2 683)
	  (list 3 44287)
	  (list 4 838861)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((nn (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (tenth-degree-polynomial nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error for test num = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index nn shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; using the tenth-degree-polynomial function compute a list from n = 1 to kk
(define (make-function-list func kk)
  (let ((klist (list)))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((> ii kk))
	(begin
	  (let ((fresult (func ii)))
	    (begin
	      (set! klist (cons fresult klist))
	      ))
	  ))

      (reverse klist)
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-function-list-1)
  (let ((sub-name "test-make-function-list-1")
	(test-list
	 (list
	  (list third-degree-polynomial 1 (list 1))
	  (list third-degree-polynomial 2 (list 1 8))
	  (list third-degree-polynomial 3 (list 1 8 27))
	  (list third-degree-polynomial 4 (list 1 8 27 64))
	  (list tenth-degree-polynomial 1 (list 1))
	  (list tenth-degree-polynomial 2 (list 1 683))
	  (list tenth-degree-polynomial 3 (list 1 683 44287))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((func (list-ref alist 0))
		 (kk (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (make-function-list func kk)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : function = ~a, kk = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index func kk
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
(define (main-loop func end-num)
  (let ((sum 0)
	(sum-list (list)))
    (begin
      (do ((kk 1 (1+ kk)))
	  ((> kk end-num))
	(begin
	  (let ((points-list (make-function-list func kk)))
	    (let ((fit (nevilles-algorithm points-list kk))
		  (good-op (func (+ kk 1))))
	      (begin
		(if (not (= fit good-op))
		    (begin
		      (set! sum (+ sum fit))
		      (set! sum-list (cons fit sum-list))
		      ))
		)))
	  ))

      (let ((slist (reverse sum-list)))
	(let ((sum-string
	       (string-join
		(map
		 (lambda (num)
		   (ice9-format:format #f "~:d" num))
		 slist) " + ")))
	  (begin
	    (display (ice9-format:format #f "sum of the FITs generated by the BOPs ~a = ~:d~%"
					 sum-string sum))
	    (force-output)
	    )))
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
    (display (format #f "Project Euler 101 - If we are presented with the first k terms of a sequence it is impossible to say with certainty the value of the next term, as there are infinitely many polynomial functions that can model the sequence.~%"))
    (newline)
    (display (format #f "As an example, let us consider the sequence of cube numbers. This is defined by the generating function, u(n) = n^3: 1, 8, 27, 64, 125, 216, ...~%"))
    (newline)
    (display (format #f "Suppose we were only given the first two terms of this sequence. Working on the principle that 'simple is best' we should assume a linear relationship and predict the next term to be 15 (common difference 7). Even if we were presented with the first three terms, by the same principle of simplicity, a quadratic relationship should be assumed.~%"))
    (newline)
    (display (format #f "We shall define OP(k, n) to be the nth term of the optimum polynomial generating function for the first k terms of a sequence. It should be clear that OP(k, n) will accurately generate the terms of the sequence for n <= k, and potentially the first incorrect term (FIT) will be OP(k, k+1); in which case we shall call it a bad OP (BOP).~%"))
    (newline)
    (display (format #f "As a basis, if we were only given the first term of sequence, it would be most sensible to assume constancy; that is, for n >= 2, OP(1, n) = u1.~%"))
    (newline)
    (display (format #f "Hence we obtain the following OPs for the cubic sequence:~%"))
    (newline)
    (display (format #f "OP(1, n) = 1   1, 1, 1, 1, ...~%"))
    (display (format #f "OP(2, n) = 7n-6   1, 8, 15, ...~%"))
    (display (format #f "OP(3, n) = 6n^2-11n+6   1, 8, 27, 58, ...~%"))
    (display (format #f "OP(4, n) = n^3   1, 8, 27, 64, 125, ...~%"))
    (newline)
    (display (format #f "Clearly no BOPs exist for k >= 4.~%"))
    (newline)
    (display (format #f "By considering the sum of FITs generated by the BOPs (indicated in red above), we obtain 1 + 15 + 58 = 74.~%"))
    (newline)
    (display (format #f "Consider the following tenth degree polynomial generating function:~%"))
    (newline)
    (display (format #f "u(n) = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10~%"))
    (newline)
    (display (format #f "Find the sum of FITs for the BOPs.~%"))
    (newline)
    (display (format #f "the solution to this problem uses Neville's algorithm, http://en.wikipedia.org/wiki/Neville's_algorithm~%"))
    (display (format #f "for more details of the derivation of Neville's algorithm, see http://www.clear.rice.edu/comp360/lectures/LagrangeText.pdf~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-nevilles-algorithm-1 counter)
	   (run-test test-third-degree-polynomial-1 counter)
	   (run-test test-tenth-degree-polynomial-1 counter)
	   (run-test test-make-function-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((kth-term 4))
      (begin
	(time-code
	 (begin
	   (display (format #f "function = n^3~%"))
	   (force-output)
	   (main-loop third-degree-polynomial kth-term)
	   ))
	))

    (newline)
    (force-output)

    (let ((kth-term 10))
      (begin
	(time-code
	 (begin
	   (display (format #f "function = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10~%"))
	   (force-output)
	   (main-loop tenth-degree-polynomial kth-term)
	   ))
	))

    (newline)
    ))
