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
(define (is-list-1-9-pandigital? digit-list)
  (let ((max-digit 9)
	(ok-flag #t))
    (let ((llen (length digit-list)))
      (begin
	(if (not (= llen 9))
	    (begin
	      #f)
	    (begin
	      (do ((ii 1 (+ ii 1)))
		  ((or (> ii max-digit)
		       (equal? ok-flag #f)))
		(begin
		  (if (equal? (member ii digit-list) #f)
		      (begin
			(set! ok-flag #f)
			#f)
		      )))
	      ok-flag
	      ))))))

;;;#############################################################
;;;#############################################################
(define (test-is-list-1-9-pandigital-1)
  (let ((sub-name "test-is-list-1-9-pandigital-1")
	(test-list
	 (list
	  (list (list 1 2 3 4 5 6 7 8 9) #t)
	  (list (list 9 2 3 4 5 6 7 8 1) #t)
	  (list (list 9 2 3 5 4 6 7 8 1) #t)
	  (list (list 8 2 3 4 5 6 7 8 9) #f)
	  (list (list 8 2 3 4 5 1 7 8 9) #f)
	  (list (list 1 2 3 4 5 6 7 8 9 3) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-bool (list-ref alist 1)))
	     (let ((result-bool (is-list-1-9-pandigital? test-list)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display (format #f "~a : (~a) : error : list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-list
					shouldbe-bool result-bool))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (cond
     ((< this-num 0) acc-list)
     ((< this-num 10) (cons this-num acc-list))
     (else
      (srfi-11:let-values
       (((next-num this-digit) (euclidean/ this-num 10)))
       (begin
	 (local-loop next-num (cons this-digit acc-list))
	 )))))
  (let ((result-list (local-loop this-num (list))))
    result-list
    ))

;;;#############################################################
;;;#############################################################
(define (test-split-digits-list-1)
  (let ((sub-name "test-split-digits-list-1")
	(test-list
	 (list
	  (list 3 (list 3)) (list 4 (list 4)) (list 5 (list 5))
	  (list 13 (list 1 3)) (list 14 (list 1 4)) (list 15 (list 1 5))
	  (list 23 (list 2 3)) (list 24 (list 2 4)) (list 25 (list 2 5))
	  (list 123 (list 1 2 3)) (list 1234 (list 1 2 3 4)) (list 98765 (list 9 8 7 6 5))
	  (list 341608987 (list 3 4 1 6 0 8 9 8 7))
	  (list 116696699999166169 (list 1 1 6 6 9 6 6 9 9 9 9 9 1 6 6 1 6 9))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (split-digits-list test-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, list shouldbe = ~a, result list = ~a~%"
					sub-name test-label-index test-num
					shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop end-num min-num status-num debug-flag)
  (begin
    (let ((f1 1)
	  (f2 1)
	  (f3 0)
	  (loop-continue-flag #t)
	  (first-num 0)
	  (first-fib 0)
	  (start-jday (srfi-19:current-julian-day)))
      (begin
	(do ((ii 3 (1+ ii)))
	    ((or (> ii end-num)
		 (equal? loop-continue-flag #f)))
	  (begin
	    (set! f3 (+ f1 f2))
	    (set! f1 f2)
	    (set! f2 f3)

	    (let ((dlist (split-digits-list (modulo f3 min-num))))
	      (let ((dlen (length dlist)))
		(begin
		  (if (>= dlen 9)
		      (begin
			(let ((bottom-digits (list-head (reverse dlist) 9)))
			  (begin
			    (if (is-list-1-9-pandigital? bottom-digits)
				(begin
				  (let ((top-digits (list-head (split-digits-list f3) 9)))
				    (begin
				      (if (is-list-1-9-pandigital? top-digits)
					  (begin
					    (set! loop-continue-flag #f)
					    (set! first-num ii)
					    (set! first-fib f3)
					    ))
				      ))
				  ))
			    )))
		      ))
		))

	    (if (zero? (modulo ii status-num))
		(begin
		  (let ((end-jday (srfi-19:current-julian-day))
			(dlist (split-digits-list f3)))
		    (let ((dlen (length dlist)))
		      (begin
			(display (ice9-format:format #f "~:d / ~:d : fib(~:d) digit length = ~:d : elapsed time ~a : ~a~%"
						     ii end-num ii dlen
						     (julian-day-difference-to-string end-jday start-jday)
						     (date-time-to-string (srfi-19:current-date))))
			(force-output)
			(set! start-jday end-jday)
			)))
		  ))

	    (if (equal? debug-flag #t)
		(begin
		  (display (ice9-format:format #f "  F(~:d) = ~:d,  " ii f3))
		  (if (zero? (modulo ii 5))
		      (begin
			(newline)
			))
		  (force-output)
		  ))
	    ))

	(if (> first-num 0)
	    (begin
	      (display (ice9-format:format #f "found a fibonacci number such that the first and last 9 digits are pandigital~%"))
	      (let ((dlist (split-digits-list f3)))
		(let ((dlen (length dlist))
		      (top-digits (list-head dlist 9)))
		  (let ((bottom-digits (list-tail dlist (- dlen 9))))
		    (begin
		      (display (ice9-format:format #f "for n = ~:d, fib(~:d) has ~:d digits, the top 9 digits are ~a, the last 9 digits are ~a~%" first-num first-num dlen top-digits bottom-digits))
		      ))
		  ))

	      (if (equal? debug-flag #t)
		  (begin
		    (display (ice9-format:format #f "fib(~:d) = ~:d~%" first-num first-fib))
		    ))
	      (force-output))
	    (begin
	      (newline)
	      (force-output)
	      ))
	))
    ))

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
    (display (format #f "Project Euler 104 - The Fibonacci sequence is defined by the recurrence relation:~%"))
    (newline)
    (display (format #f "F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.~%"))
    (newline)
    (display (format #f "It turns out that F(541), which contains 113 digits, is the first Fibonacci number for which the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order). And F(2749), which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.~%"))
    (newline)
    (display (format #f "Given that Fk is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-is-list-1-9-pandigital-1 counter)
	   (run-test test-split-digits-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (time-code
     (begin
       (let ((end-num 10)
	     (min-num 1000000000)
	     (status-num 100)
	     (debug-flag #t))
	 (begin
	   (main-loop end-num min-num status-num debug-flag)
	   ))
       ))

    (newline)
    (force-output)

    (time-code
     (begin
       (let ((end-num 20000000)
	     (min-num 1000000000)
	     (status-num 1000000)
	     (debug-flag #f))
	 (begin
	   (main-loop end-num min-num status-num debug-flag)
	   ))
       ))

    (newline)
    ))
