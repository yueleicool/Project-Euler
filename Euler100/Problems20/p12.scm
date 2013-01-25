#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

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
(define (triangular-number nn)
  (euclidean/ (* nn (+ nn 1)) 2))

;;;#############################################################
;;;#############################################################
(define (test-triangular-number-1)
  (let ((sub-name "test-triangular-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 3) (list 3 6) (list 4 10)
	  (list 5 15) (list 6 21) (list 7 28)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (triangular-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (divisor-list this-num)
  (let ((div-list (list 1 this-num))
	(max-iter (+ (exact-integer-sqrt this-num) 1)))
    (begin
      (if (equal? this-num 1)
	  (set! div-list (list 1)))

      (do ((ii 2 (+ ii 1)))
	  ((> ii max-iter))
	(begin
	  (if (zero? (modulo this-num ii))
	      (begin
		(let ((other-div (euclidean/ this-num ii)))
		  (begin
		    (if (< ii other-div)
			(begin
			  (set! div-list (cons ii div-list ))
                          (set! div-list (cons other-div div-list)))
			(begin
			  (if (= ii other-div)
                              (begin
                                (set! div-list (cons ii div-list))
                                ))
			  ))
		    ))
                ))
	  ))

      div-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-divisor-list-1)
  (let ((sub-name "test-divisor-list-1")
	(test-list
	 (list
	  (list 1 (list 1)) (list 3 (list 1 3))
	  (list 6 (list 1 2 3 6)) (list 10 (list 1 2 5 10))
	  (list 15 (list 1 3 5 15)) (list 21 (list 1 3 7 21))
	  (list 28 (list 1 2 4 7 14 28))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result
                    (sort (divisor-list test-num) <)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-digits max-ii)
  (let ((max-found-flag #f)
	(found-ii 0)
	(found-num 0)
	(found-div-list (list))
	(found-div-len 0))
    (begin
      (do ((ii 1 (+ ii 1)))
	  ((or (> ii max-ii)
	       (equal? max-found-flag #t)))
	(begin
	  (let ((tnum (triangular-number ii)))
	    (let ((divisors (divisor-list tnum)))
	      (let ((llen (length divisors)))
		(begin
		  (if (> llen max-digits)
		      (begin
			(set! found-ii ii)
			(set! found-num tnum)
			(set! found-div-list
                              (sort divisors <))
			(set! found-div-len llen)
			(set! max-found-flag #t)
			))
		  ))
              ))
	  ))

      (if (equal? max-found-flag #t)
	  (begin
	    (display (ice9-format:format #f "(~:d) ~:d : number of divisors = ~a : divisor list = ~a~%"
					 found-ii found-num found-div-len found-div-list)))
	  (begin
	    (display (ice9-format:format #f "no results found for max-digits = ~:d, max-ii = ~:d~%"
					 max-digits max-ii))
	    ))

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
    (display (format #f "Project Euler 12: The sequence of triangle numbers is generated by adding the natural numbers.~%So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:~%"))
    (display (format #f "1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...~%"))
    (display (format #f "Let us list the factors of the first seven triangle numbers:~%"))
    (display (format #f "1: 1~%"))
    (display (format #f "3: 1,3~%"))
    (display (format #f "6: 1,2,3,6~%"))
    (display (format #f "10: 1,2,5,10~%"))
    (display (format #f "15: 1,3,5,15~%"))
    (display (format #f "21: 1,3,7,21~%"))
    (display (format #f "28: 1,2,4,7,14,28~%"))
    (display (format #f "We can see that 28 is the first triangle number to have over five divisors.~%"))
    (display (format #f "What is the value of the first triangle number to have over five hundred divisors?~%"))

    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-triangular-number-1 counter)
	   (run-test test-divisor-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (do ((ii 1 (+ ii 1)))
	((> ii 8))
      (begin
	(let ((tnum (triangular-number ii)))
	  (let ((divisors (divisor-list tnum)))
	    (begin
	      (display (format #f "(~a) ~a : ~a~%" ii tnum divisors))
	      )))
	))

    (newline)
    (force-output)

    (let ((max-digits 500)
	  (max-ii 1000000))
      (begin
	(time-code
	 (begin
	   (main-loop max-digits max-ii)
	   ))
	))

    (newline)
    ))
