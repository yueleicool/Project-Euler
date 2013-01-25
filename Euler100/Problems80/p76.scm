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

;;;### srfi-1 for let-values (multiple value binding)
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
		   (* 1000.0
		      (- nsecs (+ (* nhours 60.0 60.0) (* nminutes 60.0))))))))
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
(define (array-to-string this-array)
  (let ((stmp "")
	(amax (car (array-dimensions this-array))))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii amax))
	(begin
	  (let ((this-num (array-ref this-array ii)))
	    (begin
	      (if (= ii 0)
		  (begin
		    (let ((this-str (ice9-format:format #f "~:d" this-num)))
		      (begin
			(set! stmp (string-append stmp this-str))
			)))
		  (begin
		    (let ((this-str (ice9-format:format #f ", ~:d" this-num)))
		      (begin
			(set! stmp (string-append stmp this-str))
			))
		    ))
	      ))
	  ))
      (string-append "[ " stmp " ]")
      )))

;;;#############################################################
;;;#############################################################
;;; 1-dimensional dynamic array
(define (compute-sum-count max-num debug-flag)
  (let ((dynamic-array (make-array 0 (1+ max-num))))
    (begin
      ;;; initialize
      (array-set! dynamic-array 1 0)

      (if (equal? debug-flag #t)
	  (begin
	    ;;; print header
	    (do ((ii 0 (1+ ii)))
		((> ii max-num))
	      (begin
		(if (= ii 0)
		    (begin
		      (display (format #f "        ~:d" ii)))
		    (begin
		      (display (format #f ", ~:d" ii))
		      ))
		))
	    (newline)
	    (force-output)
	    ))

      ;;; main loop
      (do ((ii 1 (1+ ii)))
	  ((>= ii max-num))
	(begin
	  (do ((jj ii (1+ jj)))
	      ((> jj max-num))
	    (begin
              ;;; then add previous value
	      (let ((this-sum (array-ref dynamic-array jj))
		    (sum-left (- jj ii)))
		(let ((sub-target (array-ref dynamic-array sum-left)))
		  (begin
		    (array-set! dynamic-array (+ this-sum sub-target) jj)
		    )))
	      ))

	  (if (equal? debug-flag #t)
	      (begin
		(display (ice9-format:format #f "(~:d) : ~a~%"
					     ii (array-to-string dynamic-array)))
		(force-output)
		))
	  ))

      (array-ref dynamic-array max-num)
      )))

;;;#############################################################
;;;#############################################################
(define (test-compute-sum-count-1)
  (let ((sub-name "test-compute-sum-count-1")
	(test-list
	 (list
	  (list 2 1)
	  (list 3 2)
	  (list 4 4)
	  (list 5 6)
	  ))
	(debug-flag #f)
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (compute-sum-count num debug-flag)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((rlen (compute-sum-count max-num debug-flag)))
    (begin
      (display
       (ice9-format:format
	#f "there are ~:d different ways to write ~:d as a sum of at least two positive integers~%"
	rlen max-num))
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
    (display (format #f "Problem 076 - It is possible to write five as a sum in exactly six different ways:~%"))
    (newline)
    (display (format #f "  4 + 1~%"))
    (display (format #f "  3 + 2~%"))
    (display (format #f "  3 + 1 + 1~%"))
    (display (format #f "  2 + 2 + 1~%"))
    (display (format #f "  2 + 1 + 1 + 1~%"))
    (display (format #f "  1 + 1 + 1 + 1 + 1~%"))
    (newline)
    (display (format #f "How many different ways can one hundred be written as a sum of at least two positive integers?~%"))
    (newline)
    (display (format #f "The solution is described at http://www.mathblog.dk/project-euler-76-one-hundred-sum-integers/~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-compute-sum-count-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((debug-flag #t))
      (begin
	(do ((ii 2 (+ ii 1)))
	    ((> ii 5))
	  (begin
	    (main-loop ii debug-flag)
	    ))
	))

    (newline)
    (force-output)

    (let ((max-num 100)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
