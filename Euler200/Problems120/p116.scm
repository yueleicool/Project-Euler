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
(define (count-rb-fixed-length max-array-length red-len)
  (define (count-loop gap-len red-len gap-htable)
    (let ((hcount (hash-ref gap-htable gap-len #f)))
      (begin
	(cond
	 ((< gap-len red-len) 0)
	 ((= gap-len red-len) 1)
	 ((not (equal? hcount #f)) hcount)
	 (else
	  (begin
	    (let ((count (1+ (- gap-len red-len)))
		  (remaining-gap (- gap-len red-len)))
	      (begin
		(let ((sub-count 0))
		  (begin
		    (do ((jj 0 (1+ jj)))
			((> jj remaining-gap))
		      (begin
			(let ((next-gap (- remaining-gap jj)))
			  (let ((next-count (count-loop next-gap red-len
							gap-htable)))
			    (begin
			      (set! sub-count (+ sub-count next-count))
			      )))
			))

		    (set! count (+ count sub-count))
		    ))

		(hash-set! gap-htable gap-len count)

		count
		))
	    )))
	)))
  (let ((gap-htable (make-hash-table)))
    (let ((count (count-loop max-array-length red-len gap-htable)))
      (begin
	count
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-rb-fixed-length-1)
  (let ((sub-name "test-count-rb-fixed-length-1")
	(test-list
	 (list
	  (list 5 2 7) (list 5 3 3) (list 5 4 2)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-len (list-ref this-list 0))
		 (min-len (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (count-rb-fixed-length a-len min-len)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : array-len = ~a, min-len = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index a-len min-len
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
  (let ((a-list-list (list 2 3 4))
	(total-list (list))
	(total-sum 0))
    (begin
      (for-each
       (lambda (a-color)
	 (begin
	   (let ((count (count-rb-fixed-length max-length a-color)))
	     (begin
	       (set! total-list (cons count total-list))
	       (set! total-sum (+ total-sum count))
	       ))
	   )) a-list-list)

      (display (ice9-format:format
		#f "there are ~a = ~:d ways of replacing the black tiles in a row measuring ~:d units in length.~%"
		(string-join (map number->string (reverse total-list)) " + ")
		total-sum max-length))
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
    (display (format #f "Project Euler 116 - A row of five black square tiles is to have a number of its tiles replaced with coloured oblong tiles chosen from red (length two), green (length three), or blue (length four).~%"))
    (newline)
    (display (format #f "If red tiles are chosen there are exactly seven ways this can be done.~%"))
    (newline)
    (display (format #f "If green tiles are chosen there are three ways.~%"))
    (newline)
    (display (format #f "And if blue tiles are chosen there are two ways.~%"))
    (newline)
    (display (format #f "Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of replacing the black tiles in a row measuring five units in length.~%"))
    (newline)
    (display (format #f "How many different ways can the black tiles in a row measuring fifty units in length be replaced if colours cannot be mixed and at least one coloured tile must be used?~%"))
    (newline)
    (display (format #f "NOTE: This is related to problem 117.~%"))
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
