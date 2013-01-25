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
(define (brute-force-aa-calc nn)
  (begin
    (cond
     ((<= nn 0) -1)
     ((even? nn) -1)
     ((zero? (modulo nn 5)) -1)
     ((= nn 1) 1)
     (else
      (let ((min-kk -1)
	    (kk 1)
	    (rr-kk 1)
	    (continue-loop-flag #t))
	(begin
	  (while
	   (equal? continue-loop-flag #t)
	   (begin
	     (if (zero? rr-kk)
		 (begin
		   (set! min-kk kk)
		   (set! continue-loop-flag #f)
		   ))
	     (set! rr-kk (modulo (1+ (* 10 rr-kk)) nn))
	     (set! kk (1+ kk))
	     ))

	  min-kk
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-brute-force-aa-calc-1)
  (let ((sub-name "test-brute-force-aa-calc-1")
	(test-list
	 (list
	  (list 7 6) (list 41 5)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((nn (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (brute-force-aa-calc nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : nn=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index nn
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
(define-syntax brute-force-check
  (syntax-rules ()
    ((brute-force-check nn min-target continue-loop-flag min-nn min-aa-kk)
     (begin
       (let ((aa-kk (brute-force-aa-calc nn)))
	 (begin
	   (if (> aa-kk min-target)
	       (begin
		 (set! min-aa-kk aa-kk)
		 (set! min-nn nn)
		 (set! continue-loop-flag #f)
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop min-target max-num max-num-factors max-prime status-num)
  (let ((min-nn -1)
	(min-aa-kk -1)
	(counter 0)
	(start-num min-target)
	(start-jday (srfi-19:current-julian-day))
	(continue-loop-flag #t))
    (begin
      (do ((ii start-num (1+ ii)))
	  ((or (>= ii max-num)
	       (equal? continue-loop-flag #f)))
	(begin
	  (if (= (gcd ii 10) 1)
	      (begin
		(brute-force-check ii min-target
				   continue-loop-flag min-nn min-aa-kk)
		))

	  (set! counter (1+ counter))
	  (if (zero? (modulo counter status-num))
	      (begin
		(display (ice9-format:format #f "completed ~:d in (~:d - ~:d) : so far A(~:d) = ~:d : "
					     ii start-num max-num min-nn min-aa-kk))
		(let ((end-jday (srfi-19:current-julian-day)))
		  (begin
		    (display (format #f "elapsed time = ~a : ~a~%"
				     (julian-day-difference-to-string end-jday start-jday)
				     (current-date-time-string)))
		    (set! start-jday end-jday)
		    ))
		(force-output)
		))
	  ))

      (if (> min-nn 0)
	  (begin
	    (display (ice9-format:format #f "The least value of n for which A(n) first exceeds ~:d is ~:d, A(~:d) = ~:d~%"
					 min-target min-nn min-nn min-aa-kk)))
	  (begin
	    (display (ice9-format:format #f "Least value n for which A(n) exceeds ~:d not found (for nn <= ~:d)~%"
					 min-target max-num))
	    ))

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
    (display (format #f "Project Euler 129 - A number consisting entirely of ones is called a repunit. We shall define R(k) to be a repunit of length k; for example, R(6) = 111111.~%"))
    (newline)
    (display (format #f "Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that there always exists a value, k, for which R(k) is divisible by n, and let A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.~%"))
    (newline)
    (display (format #f "The least value of n for which A(n) first exceeds ten is 17.~%"))
    (newline)
    (display (format #f "Find the least value of n for which A(n) first exceeds one-million.~%"))
    (newline)
    (display (format #f "For more information on A(n) see the Online Encyclopedia of Integer Sequences http://oeis.org/A099679.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-brute-force-aa-calc-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((min-target 10)
	  (max-num 100)
	  (max-num-factors 10)
	  (max-prime 100)
	  (status-num 1000))
      (begin
	(time-code
	 (begin
	   (main-loop min-target max-num max-num-factors max-prime
		      status-num)
	   ))
	))

    (newline)
    (force-output)

    (let ((min-target 1000000)
	  (max-num 2000000)
	  (max-num-factors 20)
	  (max-prime 100000)
	  (status-num 200000))
      (begin
	(time-code
	 (begin
	   (main-loop min-target max-num max-num-factors max-prime
		      status-num)
	   ))
	))

    (newline)
    ))
