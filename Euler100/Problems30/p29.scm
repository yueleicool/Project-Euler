#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold and delete-duplicates function
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
		       (display (format #f "~a : error : number = ~a, exponent = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-num test-exp
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
(define (make-result-list min-aa max-aa min-bb max-bb)
  (let ((result-list (list)))
    (begin
      (do ((aa min-aa (+ aa 1)))
	  ((> aa max-aa))
	(begin
	  (do ((bb min-bb (+ bb 1)))
	      ((> bb max-bb))
	    (begin
	      (let ((this-elem (integer-power aa bb)))
		(set! result-list (cons this-elem result-list))
		)))))

      (let ((r2-list (srfi-1:delete-duplicates result-list)))
	(sort r2-list <)
	))))

;;;#############################################################
;;;#############################################################
(define (test-make-result-list-1)
  (let ((sub-name "test-make-result-list-1")
	(test-list
	 (list
	  (list 2 5 2 5
		(list 4 8 9 16 25 27 32 64 81 125 243 256 625 1024 3125))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((min-aa (list-ref alist 0))
		 (max-aa (list-ref alist 1))
		 (min-bb (list-ref alist 2))
		 (max-bb (list-ref alist 3))
		 (shouldbe-list (list-ref alist 4)))
	     (let ((result-list (make-result-list min-aa max-aa min-bb max-bb)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error : test min-aa=~a, max-aa=~a, min-bb=~a, max-bb=~a, shouldbe = ~a, result = ~a~%"
					sub-name min-aa max-aa min-bb max-bb
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
(define (main-loop min-aa max-aa min-bb max-bb debug-flag)
  (let ((result-list (make-result-list min-aa max-aa
				       min-bb max-bb)))
    (let ((num-results (length result-list)))
      (begin
	(display
	 (ice9-format:format #f "number of results = ~:d for ~:d < a < ~:d and ~:d < b < ~:d~%"
			     num-results min-aa max-aa min-bb max-bb))
	(if (equal? debug-flag #t)
	    (begin
	      (display (format #f "sequence = { ~a }~%"
			       (string-join
				(map
				 (lambda(num)
				   (ice9-format:format #f "~:d" num))
				 result-list) ", ")))
	      ))
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
    (display (format #f "Problem 029 - Consider all integer combinations of a^b for 2 < a < 5 and 2 < b < 5:~%"))
    (newline)
    (display (format #f "2^2=4, 2^3=8, 2^4=16, 2^5=32~%"))
    (display (format #f "3^2=9, 3^3=27, 3^4=81, 3^5=243~%"))
    (display (format #f "4^2=16, 4^3=64, 4^4=256, 4^5=1024~%"))
    (display (format #f "5^2=25, 5^3=125, 5^4=625, 5^5=3125~%"))
    (newline)
    (display (format #f "If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:~%"))
    (display (format #f "  4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125~%"))
    (newline)
    (display (format #f "How many distinct terms are in the sequence generated by a^b for 2 < a < 100 and 2 < b < 100?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-result-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((min-aa 2)
	  (max-aa 5)
	  (min-bb 2)
	  (max-bb 5)
	  (debug-flag #t))
      (begin
	(main-loop min-aa max-aa min-bb max-bb debug-flag)
	))

    (newline)
    (force-output)

    (let ((min-aa 2)
	  (max-aa 100)
	  (min-bb 2)
	  (max-bb 100)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop min-aa max-aa min-bb max-bb debug-flag)
	   ))
	))

    (newline)
    ))
