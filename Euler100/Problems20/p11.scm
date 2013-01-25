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
(define (along-row-prod ll-list-list row-max col-max nn)
  (let ((last-col (- col-max nn))
	(max-prod 0)
	(max-list (list)))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((>= ii row-max))
	(begin
	  (let ((this-row (list-ref ll-list-list ii)))
	    (begin
	      (do ((jj 0 (+ jj 1)))
		  ((> jj last-col))
		(begin
		  (let ((this-prod 1)
			(this-list (list)))
		    (begin
		      (do ((kk 0 (+ kk 1)))
			  ((>= kk nn))
			(begin
			  (let ((col-index (+ jj kk)))
			    (begin
			      (if (< col-index col-max)
				  (begin
				    (let ((this-value (list-ref this-row col-index)))
				      (begin
					(set! this-prod (* this-prod this-value))
					(set! this-list (cons this-value this-list))
					)))
				  (begin
				    (set! this-prod 0)
				    (set! this-list (list)))
				  )))
			  ))

		      (if (> this-prod max-prod)
			  (begin
			    (set! max-prod this-prod)
			    (set! max-list (reverse this-list))
			    ))
		      ))
		  ))
	      ))
	  ))

      (if (> max-prod 0)
	  (begin
	    (list max-prod max-list))
	  (begin
	    (list -1 (list))
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-along-row-prod-1)
  (let ((sub-name "test-along-row-prod-1")
	(test-list
	 (list
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 2 (list 72 (list 8 9)))
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 3)) 2 (list 56 (list 7 8)))
	  (list 3 3 (list (list 1 2 3) (list 9 5 6) (list 2 8 3)) 2 (list 45 (list 9 5)))
	  (list 2 4 (list (list 1 2 3 4) (list 9 0 5 6)) 2 (list 30 (list 5 6)))
	  (list 2 4 (list (list 1 2 3 4) (list 9 0 5 6)) 3 (list 24 (list 2 3 4)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-max-row (list-ref this-list 0))
		 (test-max-col (list-ref this-list 1))
		 (test-list-list (list-ref this-list 2))
		 (test-nn (list-ref this-list 3))
		 (shouldbe (list-ref this-list 4)))
	     (let ((result (along-row-prod test-list-list
					   test-max-row test-max-col test-nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list list=~a, max-row=~a, max-col=~a, nn=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list-list
					test-max-row test-max-col test-nn shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (along-col-prod ll-list-list row-max col-max nn)
  (let ((last-row (- row-max nn))
	(max-prod 0)
	(max-list (list)))
    (begin
      (do ((jj 0 (+ jj 1)))
	  ((>= jj col-max))
	(begin
	  (do ((ii 0 (+ ii 1)))
	      ((> ii last-row))
	    (begin
	      (let ((this-prod 1)
		    (this-list (list)))
		(begin
		  (do ((kk 0 (+ kk 1)))
		      ((>= kk nn))
		    (begin
		      (let ((row-index (+ ii kk))
			    (col-index jj))
			(let ((this-row (list-ref ll-list-list row-index)))
			  (begin
			    (if (and
				 (< row-index row-max)
				 (< col-index col-max))
				(begin
				  (let ((this-value (list-ref this-row jj)))
				    (begin
				      (set! this-prod (* this-prod this-value))
				      (set! this-list (cons this-value this-list))
				      )))
				(begin
				  (set! this-prod 0)
				  (set! this-list (list))
				  ))
			    )))
		      ))
		  (if (> this-prod max-prod)
		      (begin
			(set! max-prod this-prod)
			(set! max-list (reverse this-list))))
		  ))
	      ))
	  ))

      (if (> max-prod 0)
	  (begin
	    (list max-prod max-list))
	  (begin
	    (list -1 (list))
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-along-col-prod-1)
  (let ((sub-name "test-along-col-prod-1")
	(test-list
	 (list
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 2 (list 54 (list 6 9)))
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 3)) 2 (list 40 (list 5 8)))
	  (list 3 3 (list (list 1 2 3) (list 9 5 6) (list 2 8 3)) 2 (list 40 (list 5 8)))
	  (list 2 4 (list (list 1 2 3 4) (list 9 0 5 6)) 2 (list 24 (list 4 6)))
	  (list 2 4 (list (list 3 2 3 4) (list 9 0 5 6)) 2 (list 27 (list 3 9)))
	  (list 3 4 (list (list 3 2 3 4) (list 9 0 5 6) (list 4 2 2 2)) 2 (list 36 (list 9 4)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-max-row (list-ref this-list 0))
		 (test-max-col (list-ref this-list 1))
		 (test-list-list (list-ref this-list 2))
		 (test-nn (list-ref this-list 3))
		 (shouldbe (list-ref this-list 4)))
	     (let ((result (along-col-prod test-list-list
					   test-max-row test-max-col test-nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list list=~a, max-row=~a, max-col=~a, nn=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list-list
					test-max-row test-max-col test-nn shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (along-right-diag-prod ll-list-list row-max col-max nn)
  (let ((last-row (- row-max nn))
	(last-col (- col-max nn))
	(max-prod 0)
	(max-list (list)))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((> ii last-row))
	(begin
	  (do ((jj 0 (+ jj 1)))
	      ((> jj last-col))
	    (begin
	      (let ((this-prod 1)
		    (this-list (list)))
		(begin
		  (do ((kk 0 (+ kk 1)))
		      ((>= kk nn))
		    (begin
		      (let ((row-index (+ ii kk))
			    (col-index (+ jj kk)))
			(begin
			  (if (and
			       (< row-index row-max)
			       (< col-index col-max))
			      (begin
				(let ((this-row (list-ref ll-list-list row-index)))
				  (let ((this-value (list-ref this-row col-index)))
				    (begin
				      (set! this-prod (* this-prod this-value))
				      (set! this-list (cons this-value this-list))
				      ))
				  ))
			      (begin
				(set! this-prod 0)
				(set! this-list (list))
				))
			  ))
		      ))
		  (if (> this-prod max-prod)
		      (begin
			(set! max-prod this-prod)
			(set! max-list (reverse this-list))))
		  ))
	      ))
	  ))

      (if (> max-prod 0)
	  (begin
	    (list max-prod max-list))
	  (begin
	    (list -1 (list))
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-along-right-diag-prod-1)
  (let ((sub-name "test-along-right-diag-prod-1")
	(test-list
	 (list
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 2 (list 45 (list 5 9)))
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 3)) 2 (list 32 (list 4 8)))
	  (list 3 3 (list (list 1 2 3) (list 9 5 6) (list 2 8 3)) 2 (list 72 (list 9 8)))
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 3 (list 45 (list 1 5 9)))
	  (list 2 4 (list (list 1 2 3 4) (list 9 0 5 6)) 2 (list 18 (list 3 6)))
	  (list 2 4 (list (list 3 4 3 4) (list 9 0 9 6)) 2 (list 36 (list 4 9)))
	  (list 3 4 (list (list 3 2 3 4) (list 9 0 5 6) (list 4 2 2 2)) 2 (list 18 (list 3 6)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-max-row (list-ref this-list 0))
		 (test-max-col (list-ref this-list 1))
		 (test-list-list (list-ref this-list 2))
		 (test-nn (list-ref this-list 3))
		 (shouldbe (list-ref this-list 4)))
	     (let ((result (along-right-diag-prod test-list-list
						  test-max-row test-max-col test-nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list list=~a, max-row=~a, max-col=~a, nn=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list-list
					test-max-row test-max-col test-nn shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (along-left-diag-prod ll-list-list row-max col-max nn)
  (let ((last-row (- row-max nn))
	(last-col (- nn 1))
	(max-prod 0)
	(max-list (list)))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((> ii last-row))
	(begin
	  (do ((jj (- col-max 1) (- jj 1)))
	      ((< jj last-col))
	    (begin
	      (let ((this-prod 1)
		    (this-list (list)))
		(begin
		  (do ((kk 0 (+ kk 1)))
		      ((>= kk nn))
		    (begin
		      (let ((row-index (+ ii kk))
			    (col-index (- jj kk)))
			(begin
			  (if (and
			       (< row-index row-max)
			       (< col-index col-max)
			       (>= row-index 0)
			       (>= col-index 0))
			      (begin
				(let ((this-row (list-ref ll-list-list row-index)))
				  (let ((this-value (list-ref this-row col-index)))
				    (begin
				      (set! this-prod (* this-prod this-value))
				      (set! this-list (cons this-value this-list))
				      ))
				  ))
			      (begin
				(set! this-prod 0)
				(set! this-list (list))
				))
			  ))
		      ))

		  (if (> this-prod max-prod)
		      (begin
			(set! max-prod this-prod)
			(set! max-list (reverse this-list))
			))
		  ))
	      ))
	  ))

      (if (> max-prod 0)
	  (begin
	    (list max-prod max-list))
	  (begin
	    (list -1 (list))
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-along-left-diag-prod-1)
  (let ((sub-name "test-along-left-diag-prod-1")
	(test-list
	 (list
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 2 (list 48 (list 6 8)))
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 3)) 2 (list 48 (list 6 8)))
	  (list 3 3 (list (list 1 7 3) (list 9 5 6) (list 2 8 3)) 2 (list 63 (list 7 9)))
	  (list 3 3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) 3 (list 105 (list 3 5 7)))
	  (list 2 4 (list (list 1 2 3 4) (list 9 0 5 6)) 2 (list 20 (list 4 5)))
	  (list 2 4 (list (list 1 2 3 4) (list 8 9 5 6)) 2 (list 27 (list 3 9)))
	  (list 2 4 (list (list 3 5 3 4) (list 9 0 9 6)) 2 (list 45 (list 5 9)))
	  (list 3 4 (list (list 3 2 3 4) (list 9 0 5 6) (list 4 2 2 2)) 2 (list 20 (list 4 5)))
	  (list 3 4 (list (list 3 2 3 4) (list 9 0 5 6) (list 4 2 4 2)) 2 (list 24 (list 6 4)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-max-row (list-ref this-list 0))
		 (test-max-col (list-ref this-list 1))
		 (test-list-list (list-ref this-list 2))
		 (test-nn (list-ref this-list 3))
		 (shouldbe (list-ref this-list 4)))
	     (let ((result (along-left-diag-prod test-list-list
						 test-max-row test-max-col test-nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list list=~a, max-row=~a, max-col=~a, nn=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list-list
					test-max-row test-max-col test-nn shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (list-to-prod-string llist)
  (let ((stmp (string-join
	       (map
		(lambda (num)
		  (ice9-format:format #f "~:d" num))
		llist) " x ")))
    (begin
      stmp
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-prod-string-1)
  (let ((sub-name "test-list-to-prod-string-1")
	(test-list
	 (list
	  (list (list 1) "1")
	  (list (list 1 2) "1 x 2")
	  (list (list 1 2 3) "1 x 2 x 3")
	  (list (list 4 5 6 7) "4 x 5 x 6 x 7")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-prod-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (adjacent-product mat-list-list max-row max-col nn)
  (let ((row-prod-list (along-row-prod mat-list-list max-row max-col nn))
	(col-prod-list (along-col-prod mat-list-list max-row max-col nn))
	(rdiag-prod-list (along-right-diag-prod mat-list-list max-row max-col nn))
	(ldiag-prod-list (along-left-diag-prod mat-list-list max-row max-col nn)))
    (let ((rprod (list-ref row-prod-list 0))
	  (cprod (list-ref col-prod-list 0))
	  (rdprod (list-ref rdiag-prod-list 0))
	  (ldprod (list-ref ldiag-prod-list 0)))
      (begin
	(let ((max-prod (max rprod cprod rdprod ldprod))
	      (result-list (list)))
	  (begin
	    (cond
	     ((= max-prod rprod)
	      (begin
		(set! result-list row-prod-list)
		))
	     ((= max-prod cprod)
	      (begin
		(set! result-list col-prod-list)
		))
	     ((= max-prod rdprod)
	      (begin
		(set! result-list rdiag-prod-list)
		))
	     ((= max-prod ldprod)
	      (begin
		(set! result-list ldiag-prod-list)
		)))

	    result-list
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (display-matrix mat-list-list max-row max-col)
  (begin
    (do ((ii 0 (+ ii 1)))
	((>= ii max-row))
      (begin
	(let ((llist (list-ref mat-list-list ii)))
	  (display (format #f "  ~a~%" llist))
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop matrix max-row max-col nn)
  (begin
    (display-matrix matrix max-row max-col)
    (let ((results (adjacent-product matrix max-row max-col nn)))
      (let ((max-prod (list-ref results 0))
	    (max-list (list-ref results 1)))
	(begin
	  (display (ice9-format:format #f "greatest prodct of ~a adjacent numbers : ~a = ~:d~%"
				       nn (list-to-prod-string max-list) max-prod))
	  )))
    (force-output)
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
    (display (format #f "Project Euler 11: In the 20x20 grid below, four numbers along a diagonal line have been marked in red.~%"))
    (display (format #f "The product of these numbers is 26 x 63 x 78 x 14 = 1,788,696.~%"))
    (display (format #f "What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20x20 grid?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-along-row-prod-1 counter)
	   (run-test test-along-col-prod-1 counter)
	   (run-test test-along-right-diag-prod-1 counter)
	   (run-test test-along-left-diag-prod-1 counter)
	   (run-test test-list-to-prod-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
	  (max-row 3)
	  (max-col 3)
	  (nn 3))
      (begin
	(main-loop matrix max-row max-col nn)
	))

    (newline)
    (force-output)

    (let ((matrix (list
		   (list 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
		   (list 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
		   (list 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
		   (list 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
		   (list 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
		   (list 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
		   (list 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
		   (list 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
		   (list 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
		   (list 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
		   (list 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
		   (list 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
		   (list 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
		   (list 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
		   (list 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
		   (list 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
		   (list 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
		   (list 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
		   (list 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
		   (list 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)
		   ))
	  (max-row 20)
	  (max-col 20)
	  (nn 4))
      (begin
	(time-code
	 (begin
	   (display (format #f "main problem~%"))
	   (main-loop matrix max-row max-col nn)
	   ))
	))
    ))
