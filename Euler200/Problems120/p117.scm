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

;;;### ice-9 receive for receive function
(use-modules ((ice-9 receive)
	      :renamer (symbol-prefix-proc 'ice9-receive:)))

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
(define (count-rb-fixed-length max-array-length)
  (define (count-loop gap-len gap-htable)
    (let ((hcount (hash-ref gap-htable gap-len #f))
	  (min-color 2)
	  (max-color 4))
      (begin
	(cond
	 ((< gap-len min-color) 0)
	 ((= gap-len min-color) 1)
	 ((not (equal? hcount #f)) hcount)
	 (else
	  (begin
	    (let ((count 0))
	      (begin
		(do ((ii min-color (1+ ii)))
		    ((> ii max-color))
		  (begin
		    (let ((ii-count (1+ (- gap-len ii)))
			  (remaining-gap (- gap-len ii))
			  (sub-count 0))
		      (begin
			(do ((jj 0 (1+ jj)))
			    ((> jj remaining-gap))
			  (begin
			    (let ((next-gap (- remaining-gap jj)))
			      (let ((next-count (count-loop next-gap gap-htable)))
				(begin
				  (set! sub-count (+ sub-count next-count))
				  )))
			    ))

			(set! count (+ count ii-count sub-count))
			))
		    ))

		(hash-set! gap-htable gap-len count)

		count
		))
	    )))
	)))
  (let ((gap-htable (make-hash-table)))
    (let ((count (count-loop max-array-length gap-htable)))
      (begin
	;;; add 1 for the case with no color tiles
	(1+ count)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-rb-fixed-length-1)
  (let ((sub-name "test-count-rb-fixed-length-1")
	(test-list
	 (list
	  (list 1 1)
	  (list 2 2)
	  (list 3 4)
	  (list 4 8)
	  (list 5 15)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-len (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (count-rb-fixed-length a-len)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : array-len = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index a-len
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
(define (main-loop max-length)
  (let ((count (count-rb-fixed-length max-length)))
    (begin
      (display (ice9-format:format
		#f "there are ~:d ways of tiling a row measuring ~:d units in length.~%"
		count max-length))
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
    (display (format #f "Project Euler 117 - Using a combination of black square tiles and oblong tiles chosen from: red tiles measuring two units, green tiles measuring three units, and blue tiles measuring four units, it is possible to tile a row measuring five units in length in exactly fifteen different ways.~%"))
    (newline)
    (display (format #f "How many ways can a row measuring fifty units in length be tiled?~%"))
    (newline)
    (display (format #f "NOTE: This is related to problem 116.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-count-rb-fixed-length-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-length 5))
      (begin
	(time-code
	 (begin
	   (main-loop max-length)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-length 50))
      (begin
	(time-code
	 (begin
	   (main-loop max-length)
	   ))
	))

    (newline)
    ))
