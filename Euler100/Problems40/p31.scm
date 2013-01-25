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
(define (dynamic-change-list target-amount coin-list debug-flag)
  (let ((dynamic-array (make-array 0 (+ target-amount 1) (length coin-list)))
	(coin-array (list->array 1 coin-list))
	(coin-length (length coin-list)))
    (begin
      ;;; populate the first row with 1's
      (do ((ii 0 (1+ ii)))
	  ((>= ii coin-length))
	(begin
	  (array-set! dynamic-array 1 0 ii)
	  ))

      (do ((ii 0 (1+ ii)))
	  ((> ii target-amount))
	(begin
	  (array-set! dynamic-array 1 ii 0)

	  (if (equal? debug-flag #t)
	      (begin
		(display (ice9-format:format #f "(~:d) : 1 " ii))
		(force-output)
		))

	  (do ((jj 1 (1+ jj)))
	      ((>= jj coin-length))
	    (begin
	      (if (>= ii (array-ref coin-array jj))
		  (begin
		    ;;; then add previous value
		    (let ((prev-sum (array-ref dynamic-array ii (- jj 1)))
			  (sub-coin (- ii (array-ref coin-array jj))))
		      (let ((sub-target (array-ref dynamic-array sub-coin jj)))
			(begin
			  (array-set! dynamic-array (+ prev-sum sub-target) ii jj)
			  ))))
		  (begin
		    ;;; can't make change with this coin, so just copy previous result
		    (array-set! dynamic-array
				(array-ref dynamic-array ii (- jj 1))
				ii jj)
		    ))

	      (if (equal? debug-flag #t)
		  (begin
		    (if (= jj 1)
			(begin
			  (display (ice9-format:format #f "~:d" (array-ref dynamic-array ii jj))))
			(begin
			  (display (ice9-format:format #f " ~:d" (array-ref dynamic-array ii jj)))
			  ))
		    (force-output)
		    ))
	      ))

	  (if (equal? debug-flag #t)
	      (begin
		(newline)
		(force-output)
		))
	  ))
      (let ((final-value
	     (array-ref dynamic-array target-amount (- coin-length 1))))
	final-value
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-dynamic-change-list-1)
  (let ((sub-name "test-dynamic-change-list-1")
	(test-list
	 (list
	  (list 1 (list 1 2 5 10 20 50 100 200) 1)
	  (list 2 (list 1 2 5 10 20 50 100 200) 2)
	  (list 3 (list 1 2 5 10 20 50 100 200) 2)
	  (list 4 (list 1 2 5 10 20 50 100 200) 3)
	  (list 5 (list 1 2 5 10 20 50 100 200) 4)
	  (list 6 (list 1 2 5 10 20 50 100 200) 5)
	  (list 7 (list 1 2 5 10 20 50 100 200) 6)
	  (list 8 (list 1 2 5 10 20 50 100 200) 7)
	  (list 9 (list 1 2 5 10 20 50 100 200) 8)
	  (list 10 (list 1 2 5 10 20 50 100 200) 11)
	  (list 11 (list 1 2 5 10 20 50 100 200) 12)
	  (list 195 (list 1 2 5 10 20 50 100 200) 65934)
	  (list 196 (list 1 2 5 10 20 50 100 200) 67425)
	  (list 197 (list 1 2 5 10 20 50 100 200) 68916)
	  (list 198 (list 1 2 5 10 20 50 100 200) 70407)
	  (list 199 (list 1 2 5 10 20 50 100 200) 71898)
	  (list 200 (list 1 2 5 10 20 50 100 200) 73682)
	  ))
	(test-label-index 0)
	(debug-flag #f))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((target-num (list-ref alist 0))
		 (coin-list (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (dynamic-change-list target-num coin-list debug-flag)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : target number = ~a, coin-list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num coin-list
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
(define (dynamic-main-loop target-amount coin-list debug-flag)
  (let ((num-ways (dynamic-change-list target-amount coin-list debug-flag)))
    (begin
      (display
       (ice9-format:format
        #f "~:d = number of ways to make change for ~:d : "
        num-ways target-amount))
      (display
       (ice9-format:format
        #f "with coin list ~a (dynamic method)~%"
        coin-list))
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
    (display (format #f "Problem 031 - In England the currency is made up of pound, and pence, p, and there are eight coins in general circulation:~%"))
    (newline)
    (display (format #f "1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).~%"))
    (display (format #f "It is possible to make £2 in the following way:~%"))
    (display (format #f "1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p~%"))
    (newline)
    (display (format #f "How many different ways can 200p be made using any number of coins?~%"))
    (newline)
    (display (format #f "The solution can be found from http://ttsiodras.github.com/euler31.html~%"))
    (newline)
    (display (format #f "This program uses the dynamic programming method to find the number of ways to make change.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-dynamic-change-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((target-amount 10)
	  (coin-list (list 1 2 5 10))
	  (debug-flag #t))
      (begin
	(dynamic-main-loop target-amount coin-list debug-flag)
	))

    (newline)
    (force-output)

    (let ((target-amount 200)
	  (coin-list (list 1 2 5 10 20 50 100 200))
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (dynamic-main-loop target-amount coin-list debug-flag)
	   ))
	))

    (newline)
    ))
