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
(define (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
(define (find-rmax aa-num max-nn)
  (let ((rmax (* 2 aa-num))
	(rexp 1)
	(aa-sqr (* aa-num aa-num)))
    (let ((two-aa (modulo (* 2 aa-num) aa-sqr)))
      (begin
	(do ((ii 2 (1+ ii)))
	    ((> ii max-nn))
	  (begin
	    (cond
	     ((even? ii)
	      (begin
		(if (< rmax two-aa)
		    (begin
		      (set! rmax two-aa)
		      (set! rexp ii)
		      ))
		))
	     (else
	      (begin
		(let ((remain (modulo (* 2 aa-num ii) aa-sqr)))
		  (if (< rmax remain)
		      (begin
			(set! rmax remain)
			(set! rexp ii)
			))
		  ))
	      ))
	    ))
	(list rmax rexp)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-find-rmax-1)
  (let ((sub-name "test-find-rmax-1")
	(test-list
	 (list
	  (list 3 10 (list 6 1))
	  (list 7 10 (list 42 3))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((aa-num (list-ref alist 0))
		 (max-nn (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (find-rmax aa-num max-nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, max-nn = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index aa-num max-nn
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
(define (integer-power this-number this-exponent)
  (cond
   ((= this-exponent 0) 1)
   ((= this-exponent 1) this-number)
   ((< this-exponent 0) -1)
   (else
    (let ((result-num this-number)
	  (max-iter (- this-exponent 1)))
      (begin
	(do ((ii 0 (+ ii 1)))
	    ((>= ii max-iter))
	  (begin
	    (set! result-num (* result-num this-number))
	    ))
	result-num
	)))))

;;;#############################################################
;;;#############################################################
(define (test-integer-power-1)
  (let ((sub-name "test-integer-power-1")
	(test-list
	 (list
	  (list 10 0 1) (list 11 0 1) (list 12 0 1)
	  (list 10 1 10) (list 11 1 11) (list 12 1 12)
	  (list 10 2 100) (list 11 2 121) (list 12 2 144)
	  (list 2 2 4) (list 2 3 8) (list 2 4 16) (list 2 5 32)
	  (list 2 6 64) (list 2 7 128) (list 2 8 256) (list 2 9 512)
	  (list 2 10 1024)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (test-exp (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (let ((result-num (integer-power test-num test-exp)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, exponent = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num test-exp
					shouldbe-num result-num))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num end-num max-exponent debug-flag)
  (let ((sum-rmax 0))
    (begin
      (do ((ii start-num (1+ ii)))
	  ((> ii end-num))
	(begin
	  (let ((result-list (find-rmax ii max-exponent)))
	    (let ((rmax (list-ref result-list 0))
		  (rexp (list-ref result-list 1)))
	      (begin
		(set! sum-rmax (+ sum-rmax rmax))

		(if (equal? debug-flag #t)
		    (begin
		      (let ((aa-sqr (* ii ii))
			    (aa-m1 (1- ii))
			    (aa-p1 (1+ ii)))
			(let ((aa-m1-exp (integer-power aa-m1 rexp))
			      (aa-p1-exp (integer-power aa-p1 rexp)))
			  (let ((aa-plus (+ aa-m1-exp aa-p1-exp)))
			    (let ((remain (modulo aa-plus aa-sqr)))
			      (begin
				(display (ice9-format:format #f "  a = ~:d, n = ~:d : ~:d^~:d + ~:d^~:d = ~:d == ~:d mod ~:d : rmax(~:d) = ~:d~%"
							     ii rexp aa-m1 rexp aa-p1 rexp aa-plus
							     remain aa-sqr ii rmax))
				(force-output)
				))
			    )))
		      ))
		)))
	  ))

      (display (ice9-format:format #f "sum of the rmax = ~:d  (~:d <= n <= ~:d)~%"
				   sum-rmax start-num end-num))
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
    (display (format #f "Project Euler 120 - Let r be the remainder when (a-1)^n + (a+1)^n is divided by a^2.~%"))
    (newline)
    (display (format #f "For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728 == 42 mod 49. And as n varies, so too will r, but for a = 7 it turns out that rmax = 42.~%"))
    (newline)
    (display (format #f "For 3 <= a <= 1000, find Sum(rmax).~%"))
    (newline)
    (display (format #f "The solution was found at http://www.lousycoder.com/index.php?/archives/151-Project-Euler-Problem-120.html~%"))
    (newline)
    (display (format #f "Using the binomial theorem, http://en.wikipedia.org/wiki/Binomial_theorem one can rewrite (a-1)^n + (a+1)^n=Sum(Choose(n,k) * a^k * ((-1)^(n-k) + 1)).~%"))
    (newline)
    (display (format #f "From the article above there are three cases to examine: n=1, n even, n odd.  When n=1, (a-1)+(a+1)=2a.  When n is even, then (n-k) is odd when k is odd, so only the even powers a^k remain (-1+1 = 0), then a^2t = 0 mod a^2.  (a-1)^n + (a+1)^n = 2 mod a^2.  When n is odd, then (n-k) is even when k is odd, so only the odd powers of a^k remain.  Every term in the binomial expansion contains a power of a^2 (each a^k is equal to a^2 times a constant), except for the k=1 term, so (a-1)^n + (a+1)^n = 2na mod a^2.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-find-rmax-1 counter)
	   (run-test test-integer-power-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 3)
	  (end-num 7)
	  (max-exp 10)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num max-exp debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((start-num 3)
	  (end-num 1000)
	  (max-exp 2000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num max-exp debug-flag)
	   ))
	))

    (newline)
    ))
