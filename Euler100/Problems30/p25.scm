#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format function
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
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (cond
     ((< this-num 0) acc-list)
     ((< this-num 10) (cons this-num acc-list))
     (else
      (let ((next-num 0)
	    (this-digit 0))
	(begin
	  (call-with-values (lambda() (euclidean/ this-num 10))
	    (lambda (a b)
	      (begin
		(set! next-num a)
		(set! this-digit b))))
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
		       (display (format #f "~a : error (~a) : number = ~a, list shouldbe = ~a, result list = ~a~%"
					sub-name test-label-index test-num shouldbe-list result-list))
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (number-of-digits this-num)
  (let ((dlist (split-digits-list this-num)))
    (let ((ndigits (length dlist)))
      (begin
	ndigits
	))))

;;;#############################################################
;;;#############################################################
(define (test-number-of-digits-1)
  (let ((sub-name "test-number-of-digits-1")
	(test-list
	 (list
	  (list 1 1) (list 10 2) (list 100 3) (list 101 3)
	  (list 1000 4) (list 10045 5) (list 120111 6)
	  (list 1234567 7)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (number-of-digits test-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-num shouldbe-num result-num))
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-digits debug-flag)
  (let ((f1 1)
	(f2 1)
	(fcounter 3)
	(loop-completed #f)
	(final-result 0)
	(final-index 0)
	(final-ndigits 0))
    (begin
      (if (equal? debug-flag #t)
	  (begin
	    (display (ice9-format:format #f "F1 = 1~%F2 = 1~%"))))

      (while
       (equal? loop-completed #f)
       (begin
	 (let ((fn (+ f1 f2)))
	   (let ((ndigits (number-of-digits fn)))
	     (begin
	       (if (equal? debug-flag #t)
		   (begin
		     (display (ice9-format:format #f "F~:d = ~:d~%" fcounter fn))))
	       (if (>= ndigits max-digits)
		   (begin
		     (set! loop-completed #t)
		     (set! final-result fn)
		     (set! final-index fcounter)
		     (set! final-ndigits ndigits)
		     (break)
		     ))
	       (set! f1 f2)
	       (set! f2 fn)
	       (set! fcounter (1+ fcounter))
	       )))))

      (display (ice9-format:format #f "the ~:dth term, F~:d = ~:d, is the first term to contain ~:d digits~%"
				   final-index final-index final-result final-ndigits))
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
    (display (format #f "Problem 025 - The Fibonacci sequence is defined by the recurrence relation:~%"))
    (display (format #f "Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.~%"))
    (display (format #f "Hence the first 12 terms will be:~%"))
    (display (format #f "F1 = 1, F2 = 1, F3 = 2, F4 = 3, F5 = 5, F6 = 8~%"))
    (display (format #f "F7 = 13, F8 = 21, F9 = 34, F10 = 55, F11 = 89, F12 = 144~%"))
    (newline)
    (display (format #f "The 12th term, F12, is the first term to contain three digits.~%"))
    (newline)
    (display (format #f "What is the first term in the Fibonacci sequence to contain 1000 digits?~%"))
    (newline)
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-number-of-digits-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-digits 3)
	  (debug-flag #t))
      (begin
	(main-loop max-digits debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-digits 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-digits debug-flag)
	   ))
	))

    (newline)
    ))
