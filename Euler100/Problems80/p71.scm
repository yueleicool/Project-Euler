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

;;;### srfi-11 for let-values function
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

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
(define (main-loop max-num target-numer target-denom)
  (let ((left-numer 0)
	(left-denom 1)
	(right-numer 1)
	(right-denom 1)
	(mid-numer 0)
	(mid-denom 0))
    (begin
      (while
       (<= (+ left-denom right-denom) max-num)
       (begin
	 ;;; compute the mediant
	 (set! mid-numer (+ left-numer right-numer))
	 (set! mid-denom (+ left-denom right-denom))

	 ;;; if mid ratio is greater than 3/7, then mid-numer*7 > mid-denom*3
	 (if (or
	      (and (= mid-numer target-numer)
		   (= mid-denom target-denom))
	      (>= (* mid-numer target-denom)
		  (* mid-denom target-numer)))
	     (begin
	       (set! right-numer mid-numer)
	       (set! right-denom mid-denom))
	     (begin
	       (set! left-numer mid-numer)
	       (set! left-denom mid-denom)
	       ))
	 ))

      (display
       (ice9-format:format
	#f "~:d/~:d is the fraction that's immediately to the left of ~:d/~:d, (for denominators <= ~:d)~%"
	left-numer left-denom target-numer target-denom max-num))
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
    (display (format #f "Problem 071 - Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.~%"))
    (newline)
    (display (format #f "If we list the set of reduced proper fractions for d <= 8 in ascending order of size, we get:~%"))
    (newline)
    (display (format #f "  1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8~%"))
    (newline)
    (display (format #f "It can be seen that 2/5 is the fraction immediately to the left of 3/7.~%"))
    (newline)
    (display (format #f "By listing the set of reduced proper fractions for d <= 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.~%"))
    (newline)
    (display (format #f "Several solutions can be found at http://www.mathblog.dk/project-euler-71-proper-fractions-ascending-order/~%"))
    (newline)
    (display (format #f "This program binary search of the Farey sequence to find the fraction less than 3/7.  See the wikipedia for a description of the Farey sequence and it's generation http://en.wikipedia.org/wiki/Farey_sequence~%"))
    (newline)

    (newline)
    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 8)
	  (target-numer 3)
	  (target-denom 7))
      (begin
	(main-loop max-num target-numer target-denom)
	))

    (newline)
    (force-output)

    (let ((max-num 1000000)
	  (target-numer 3)
	  (target-denom 7))
      (begin
	(time-code
	 (begin
	   (main-loop max-num target-numer target-denom)
	   ))
	))

    (newline)
    ))
