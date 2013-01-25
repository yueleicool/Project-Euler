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
(define (calc-nth-iter nn)
  (let ((rvalue 1)
	(aa 0))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((> ii nn))
	(begin
	  (set! aa (/ 1 (+ 2 aa)))
	  ))
      (let ((result (+ 1 aa)))
	(let ((numer (numerator result))
	      (denom (denominator result)))
	  (list numer denom)
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-calc-nth-iter-1)
  (let ((sub-name "test-calc-nth-iter-1")
	(test-list
	 (list
	  (list 0 (list 3 2))
	  (list 1 (list 7 5))
	  (list 2 (list 17 12))
	  (list 3 (list 41 29))
	  (list 4 (list 99 70))
	  (list 5 (list 239 169))
	  (list 6 (list 577 408))
	  (list 7 (list 1393 985))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (calc-nth-iter test-num)))
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
(define (main-loop start-num max-num debug-flag)
  (begin
    (let ((counter 0))
      (begin
	(do ((nn start-num (+ nn 1)))
	    ((> nn max-num))
	  (begin
	    (let ((this-list (calc-nth-iter nn)))
	      (let ((numer (list-ref this-list 0))
		    (denom (list-ref this-list 1)))
		(let ((numer-ndigits
		       (length (split-digits-list numer)))
		      (denom-ndigits
		       (length (split-digits-list denom))))
		  (begin
		    (if (> numer-ndigits denom-ndigits)
			(begin
			  (set! counter (1+ counter))
			  (if (equal? debug-flag #t)
			      (begin
				(display
				 (ice9-format:format
				  #f "~:dth expansion has ~:d digits in the numerator and ~:d digits in the denominator, ~:d / ~:d~%"
				  (+ nn 1) numer-ndigits denom-ndigits numer denom))
				))
			  ))
		    ))))
	    ))

	(newline)
	(display (ice9-format:format #f "The number of expansions that contain a numerator with more digits than the denominator = ~:d :  where the expansions range between ~:d and ~:d~%"
				     counter start-num max-num))
	(force-output)
	))))

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
    (display (format #f "Problem 057 - It is possible to show that the square root of two can be expressed as an infinite continued fraction.~%"))
    (newline)
    (display (format #f " sqrt(2) = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...~%"))
    (newline)
    (display (format #f "By expanding this for the first four iterations, we get:~%"))
    (display (format #f "1 + 1/2 = 3/2 = 1.5~%"))
    (display (format #f "1 + 1/(2 + 1/2) = 7/5 = 1.4~%"))
    (display (format #f "1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...~%"))
    (display (format #f "1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...~%"))
    (newline)
    (display (format #f "The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.~%"))
    (newline)
    (display (format #f "In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?~%"))

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-calc-nth-iter-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 1)
	  (max-num 10)
	  (debug-flag #t))
      (begin
	(main-loop start-num max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (max-num 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop start-num max-num debug-flag)
	   ))
	))

    (newline)
    ))
