#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-11 for let-values
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

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
(define (pythagorean-triplet aa bb)
  (let ((a2 (* aa aa))
	(b2 (* bb bb)))
    (let ((a2b2sum (+ a2 b2)))
      (srfi-11:let-values
       (((cc cc-remainder)
	 (exact-integer-sqrt a2b2sum)))
       (begin
	 (if (and
	      (zero? cc-remainder)
	      (> cc aa)
	      (> cc bb))
	     cc
	     #f
	     )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-pythagorean-triplet-1)
  (let ((sub-name "test-pythagorean-triplet-1")
	(test-list
	 (list
	  (list 1 2 #f) (list 1 3 #f) (list 3 4 5)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-aa (list-ref alist 0))
		 (test-bb (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (let ((result-num (pythagorean-triplet test-aa test-bb)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : aa=~a, bb=~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-aa test-bb
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
(define (find-pythagorean-triplet ii jj sum-to-find)
  (begin
    (let ((tmpcc (pythagorean-triplet ii jj)))
      (if (not (equal? tmpcc #f))
	  (begin
	    (let ((tmpsum (+ ii jj tmpcc)))
	      (begin
		(if (equal? tmpsum sum-to-find)
		    (begin
		      (list aa bb cc))
		    (begin
		      #f
		      ))
		)))
	  (begin
	    #f
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop sum-to-find max-num debug-flag)
  (let ((product 1))
    (begin
      (do ((ii 1 (+ ii 1)))
	  ((>= ii max-num))
	(begin
	  (do ((jj (+ ii 1) (+ jj 1)))
	      ((> jj max-num))
	    (begin
	      (let ((tmpcc (pythagorean-triplet ii jj)))
		(if (not (equal? tmpcc #f))
		    (begin
		      (let ((tmpsum (+ ii jj tmpcc)))
			(begin
			  (if (equal? tmpsum sum-to-find)
			      (begin
				(set! product (* ii jj tmpcc))
				))
			  (if (or (equal? debug-flag #t)
				  (equal? tmpsum sum-to-find))
			      (begin
				(display (ice9-format:format #f "  ~:d^2 + ~:d^2 = ~:d + ~:d = ~:d = ~:d^2~%"
							     ii jj (* ii ii) (* jj jj)
							     (* tmpcc tmpcc) tmpcc))
				(display (ice9-format:format #f "  sum : ~:d + ~:d + ~:d = ~:d~a~%"
							     ii jj tmpcc tmpsum
							     (if (equal? tmpsum sum-to-find) "  **********" "")))
				(display (ice9-format:format #f "  product : ~:d * ~:d * ~:d = ~:d~%"
							     ii jj tmpcc (* ii jj tmpcc)))
				))
			  ))
		      )))
	      ))
	  ))
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
    (display (format #f "Project Euler 9: A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,~%"))
    (display (format #f "a^2 + b^2 = c^2~%"))
    (display (format #f "For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.~%"))
    (display (format #f "There exists exactly one Pythagorean triplet for which a + b + c = 1000.~%"))
    (display (format #f "Find the product abc.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-pythagorean-triplet-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (newline)
    (let ((num-to-find 12)
	  (max-num 12)
	  (debug-flag #t))
      (begin
	(main-loop num-to-find max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((num-to-find 1000)
	  (max-num 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop num-to-find max-num debug-flag)
	   ))
	))

    (newline)
    ))
