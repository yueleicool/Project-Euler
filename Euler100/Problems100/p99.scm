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
;;; format of base-exponents list = (list line-counter base exponent)
(define (return-largest-list base-exponents-list)
  (let ((result-list (list))
	(max-log 0.0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((this-base (list-ref this-list 1))
		 (this-exponent (list-ref this-list 2)))
	     (let ((this-log (* this-exponent (log this-base))))
	       (if (> this-log max-log)
		   (begin
		     (set! result-list this-list)
		     (set! max-log this-log)
		     ))
	       ))
	   )) base-exponents-list)

      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-return-largest-list-1)
  (let ((sub-name "test-return-largest-list-1")
	(test-list
	 (list
	  (list (list (list 1 2 3)
		      (list 2 2 4)
		      (list 3 2 2))
		(list 2 2 4))
	  (list (list (list 1 2 3)
		      (list 2 2 4)
		      (list 3 3 2))
		(list 2 2 4))
	  (list (list (list 1 2 3)
		      (list 2 2 4)
		      (list 3 3 3))
		(list 3 3 3))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((base-exponents-list (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (return-largest-list base-exponents-list)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : (~a) : error : base-exponents-list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index base-exponents-list
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
;;; returns a list of lists
(define (read-in-file fname)
  (let ((results-list (list))
	(line-counter 0))
    (begin
      (if (file-exists? fname)
	  (begin
	    (with-input-from-file fname
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited "\r\n") (ice9-rdelim:read-delimited "\r\n")))
		    ((eof-object? line))
		  (begin
		    (if (and (not (eof-object? line))
			     (> (string-length line) 0))
			(begin
			  (let ((llist (string-split line #\,)))
			    (let ((base (string->number (list-ref llist 0)))
				  (exponent (string->number (list-ref llist 1))))
			      (begin
				(set! line-counter (1+ line-counter))
				(let ((this-list (list line-counter base exponent)))
				  (begin
				    (set! results-list
					  (cons this-list results-list))
				    ))
				)))
			  ))
		    ))
		))

	    (display (ice9-format:format #f "read in ~a lines from ~a~%" line-counter fname))
	    (newline)
	    (force-output)

	    (reverse results-list))
	  (begin
	    (list)
	    ))
      )))

;;;#############################################################
;;;#############################################################
;;; format of result list = (list line-counter base exponent this-log)
(define (display-result-list result-list)
  (let ((line-counter (list-ref result-list 0))
	(base (list-ref result-list 1))
	(exponent (list-ref result-list 2)))
    (let ((logarithm (* exponent (log base))))
      (begin
	(display (ice9-format:format #f "~:d is the line number with the greatest numerical value~%" line-counter))
	(display (ice9-format:format #f "log(~:d^~:d) = ~a~%" base exponent logarithm))
	(newline)
	(force-output)
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
    (display (format #f "Project Euler 99 - Comparing two numbers written in index form like 2^11 and 3&7 is not difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.~%"))
    (newline)
    (display (format #f "However, confirming that 632382^518061 > 519432^525806 would be much more difficult, as both numbers contain over three million digits.~%"))
    (newline)
    (display (format #f "Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one thousand lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.~%"))
    (newline)
    (display (format #f "NOTE: The first two lines in the file represent the numbers in the example given above.~%"))
    (newline)
    (display (format #f "This problem is solved using the logarithmic function, which has the nice properties of being one-to-one and an increasing function.  It reduces the problem of calculating all 3 million digits, to calculating an equivalent number with 7 digits (plus decimal digits).  Since all we are looking for is the largest power, and since if x > y, then log(x) > log(y), our calculations will be speeded up tremendously.~%"))
    (newline)
    (display (format #f "for more information see http://en.wikipedia.org/wiki/Logarithm~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-return-largest-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((base-exponents-list (list (list 1 632382 518061)
				     (list 2 519432 525806))))
      (begin
	(time-code
	 (begin
	   (let ((max-list (return-largest-list base-exponents-list)))
	     (begin
	       (for-each
		(lambda (alist)
		  (begin
		    (display (format #f "~a~%" alist))
		    )) base-exponents-list)

	       (display-result-list max-list)
	       ))
	   ))
	))

    (newline)
    (force-output)

    (let ((filename "base_exp.txt"))
      (begin
	(time-code
	 (begin
	   (let ((base-exponents-list (read-in-file filename)))
	     (let ((max-list (return-largest-list base-exponents-list)))
	       (begin
		 (display-result-list max-list)
		 )))
	   ))
	))

    (newline)
    ))
