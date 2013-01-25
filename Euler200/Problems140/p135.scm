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
(define (current-date-time-string)
  (let ((this-datetime (srfi-19:current-date)))
    (let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	  (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
;;; u=3d-z, v=d+z
;;; d=(u+v)/4 and z=(3v-u)/4
(define (transform-uu-vv-to-xyzd uu vv)
  (let ((result-list (list))
	(dtmp1 (+ uu vv))
	(ztmp1 (- (* 3 vv) uu)))
    (let ((dtmp2 (euclidean/ dtmp1 4))
	  (ztmp2 (euclidean/ ztmp1 4)))
      (begin
	(if (and (= (* 4 dtmp2) dtmp1)
		 (= (* 4 ztmp2) ztmp1)
		 (> ztmp2 0))
	    (begin
	      (let ((xx (+ ztmp2 dtmp2 dtmp2))
		    (yy (+ ztmp2 dtmp2))
		    (zz ztmp2))
		(begin
		  (set! result-list (list xx yy zz dtmp2))
		  ))
	      ))

	result-list
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-transform-uu-vv-to-xyzd-1)
  (let ((sub-name "test-transform-uu-vv-to-xyzd-1")
	(test-list
	 (list
	  (list 1 1 (list))
	  (list 3 9 (list 12 9 6 3))
	  (list 1 27 (list 34 27 20 7))
	  (list 4 8 (list 11 8 5 3))
	  (list 1 39 (list 49 39 29 10))
	  (list 1 10 (list))
	  (list 2 5 (list))
	  ))
	(test-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((uu (list-ref this-list 0))
		 (vv (list-ref this-list 1))
		 (shouldbe-list-list (list-ref this-list 2)))
	     (let ((result-list-list (transform-uu-vv-to-xyzd uu vv)))
	       (let ((slen (length shouldbe-list-list))
		     (rlen (length result-list-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : uu=~a, vv=~a, shouldbe=~a, result=~a, length discrepancy, shouldbe=~a, result=~a~%"
					  sub-name test-index uu vv
					  shouldbe-list-list result-list-list
					  slen rlen))
			 (quit)
			 ))

		   (for-each
		    (lambda (slist)
		      (begin
			(if (equal? (member slist result-list-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : uu=~a, vv=~a, shouldbe=~a, result=~a, missing list ~a~%"
					       sub-name test-index uu vv
					       shouldbe-list-list result-list-list
					       slist))
			      (quit)
			      ))
			)) shouldbe-list-list)

		   (set! test-index (+ test-index 1))
		   ))
	       ))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (calc-solutions-list uu vv)
  (let ((results-list-list (list)))
    (begin
      (let ((dtmp (+ uu vv)))
	(let ((dd (euclidean/ dtmp 4)))
	  (begin
	    (if (= (* 4 dd) dtmp)
		(begin
		  (let ((ztmp-1 (- (* 3 vv) uu))
			(ztmp-2 (- (* 3 uu) vv)))
		    (let ((zz-1 (euclidean/ ztmp-1 4))
			  (zz-2 (euclidean/ ztmp-2 4)))
		      (begin
			(if (and (= (* 4 zz-1) ztmp-1)
				 (> zz-1 0))
			    (begin
			      (set! results-list-list
				    (cons (list uu vv) results-list-list))
			      ))

			(if (and (not (equal? uu vv))
				 (= (* 4 zz-2) ztmp-2)
				 (> zz-2 0))
			    (begin
			      (set! results-list-list
				    (cons (list vv uu) results-list-list))
			      ))
			)))
		  ))
	    )))

      results-list-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-solutions-list-1)
  (let ((sub-name "test-calc-solutions-list-1")
	(test-list
	 (list
	  (list 1 12 (list))
	  (list 2 6 (list (list 2 6)))
	  (list 3 4 (list))
	  (list 1 15 (list (list 1 15)))
	  (list 3 5 (list (list 3 5) (list 5 3)))
	  (list 1 27 (list (list 1 27)))
	  (list 3 9 (list (list 3 9)))
	  ))
	(test-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((uu (list-ref this-list 0))
		 (vv (list-ref this-list 1))
		 (shouldbe-list-list (list-ref this-list 2)))
	     (let ((result-list-list (calc-solutions-list uu vv)))
	       (let ((slen (length shouldbe-list-list))
		     (rlen (length result-list-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : uu=~a, vv=~a, nn=~a, shouldbe=~a, result=~a, length discrepancy, shouldbe=~a, result=~a~%"
					  sub-name test-index uu vv nn
					  shouldbe-list-list result-list-list
					  slen rlen))
			 (quit)
			 ))

		   (for-each
		    (lambda (slist)
		      (begin
			(if (equal? (member slist result-list-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a, missing list ~a~%"
					       sub-name test-index test-num
					       shouldbe-list-list result-list-list slist))
			      (quit)
			      ))
			)) shouldbe-list-list)

		   (set! test-index (+ test-index 1))
		   ))
	       ))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax debug-display
  (syntax-rules ()
    ((debug-display uu vv nn)
     (begin
       (let ((rlist (transform-uu-vv-to-xyzd uu vv)))
	 (let ((xx (list-ref rlist 0))
	       (yy (list-ref rlist 1))
	       (zz (list-ref rlist 2))
	       (dd (list-ref rlist 3)))
	   (begin
	     (display (ice9-format:format #f "    (~:d, ~:d)  ~:d^2 - ~:d^2 - ~:d^2 = ~:d : delta = ~:d~%"
					  uu vv xx yy zz nn dd))
	     (force-output)
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax process-uu-vv-pair
  (syntax-rules ()
    ((process-uu-vv-pair uu vv max-num continue-loop-flag
			 debug-flag results-htable)
     (begin
       (let ((nn (* uu vv)))
	 (begin
	   (if (< nn max-num)
	       (begin
		 (let ((this-solution-list (calc-solutions-list uu vv)))
		   (let ((dlen (length this-solution-list)))
		     (begin
		       (cond
			((= dlen 1)
			 (begin
			   (let ((tcount (hash-ref results-htable nn 0)))
			     (begin
			       (hash-set! results-htable nn (1+ tcount))

			       (if (equal? debug-flag #t)
				   (begin
				     (debug-display uu vv nn)
				     (newline)
				     ))
			       ))
			   ))
			((= dlen 2)
			 (begin
			   (let ((tcount (hash-ref results-htable nn 0)))
			     (begin
			       (hash-set! results-htable nn (+ tcount 2))

			       (if (equal? debug-flag #t)
				   (begin
				     (debug-display uu vv nn)
				     (debug-display vv uu nn)
				     (newline)
				     ))
			       ))
			   ))
			))
		     )))
	       (begin
		 (set! continue-loop-flag #f)
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; note: xx^2 - yy^2 - zz^2 = (zz+2dd)^2 - (zz+dd)^2 - zz^2
;;; = 3dd^2 + 2dd*zz - zz^2 = nn
;;; let u=3d-z, v=d+z, then xx^2 - yy^2 - zz^2 = u*v = n
;;; where d=(u+v)/4 and z=(3v-u)/4
(define-syntax count-solutions
  (syntax-rules ()
    ((count-solutions max-num num-solutions status-num debug-flag
		      results-htable)
     (begin
       (let ((start-jday (srfi-19:current-julian-day))
	     (max-uu (1+ (exact-integer-sqrt max-num))))
	 (begin
	   (do ((uu 1 (1+ uu)))
	       ((>= uu max-uu))
	     (begin
	       (let ((continue-loop-flag #t)
		     (max-vv (1+ (euclidean/ max-num uu))))
		 (begin
		   (do ((vv uu (1+ vv)))
		       ((or (> vv max-vv)
			    (equal? continue-loop-flag #f)))
		     (begin
		       (process-uu-vv-pair uu vv max-num continue-loop-flag
					   debug-flag results-htable)
		       ))
		   ))

	       (if (zero? (modulo uu status-num))
		   (begin
		     (display (format #f "  ~:d / ~:d : count so far = ~:d : "
				      uu max-uu results-count))
		     (let ((end-jday (srfi-19:current-julian-day)))
		       (begin
			 (display (format #f "elapsed time = ~a : ~a~%"
					  (julian-day-difference-to-string end-jday start-jday)
					  (current-date-time-string)))
			 (force-output)
			 (set! start-jday end-jday)
			 ))
		     ))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num num-solutions status-num debug-flag)
  (let ((results-htable (make-hash-table)))
    (begin
      (count-solutions max-num num-solutions status-num debug-flag
		       results-htable)


      (let ((min-nn -1)
	    (results-count 0))
	(begin
	  (hash-for-each
	   (lambda (nn ncount)
	     (begin
	       (if (= ncount num-solutions)
		   (begin
		     (if (or (< min-nn 0)
			     (< nn min-nn))
			 (begin
			   (set! min-nn nn)
			   ))
		     (set! results-count (1+ results-count))
		     ))
	       )) results-htable)

	  (display (ice9-format:format #f "Found ~:d values of n with exactly ~:d distinct solutions (less than ~:d).  The smallest is equal to ~:d.~%"
				       results-count num-solutions max-num min-nn))
	  (force-output)
	  ))
      )))

;;;#############################################################
;;;#############################################################
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
				(current-date-time-string)))
	       (force-output)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main args)
  (begin
    (display (format #f "Project Euler 135 - Given the positive integers, x, y, and z, are consecutive terms of an arithmetic progression, the least value of the positive integer, n, for which the equation, x^2 - y^2 - z^2 = n, has exactly two solutions is n = 27:~%"))
    (newline)
    (display (format #f "  34^2 - 27^2 - 20^2 = 12^2 - 9^2 - 6^2 = 27~%"))
    (newline)
    (display (format #f "It turns out that n = 1155 is the least value which has exactly ten solutions.~%"))
    (newline)
    (display (format #f "How many values of n less than one million have exactly ten distinct solutions?~%"))
    (newline)
    (display (format #f "The solution was found at http://d.hatena.ne.jp/whitypig/20120327/1332868956 and at https://github.com/EmilHernvall/projecteuler/blob/master/136.c~%"))
    (newline)
    (display (format #f "x^2 - y^2 - z^2 = n, where x, y, and z form an arithmetic progression, which means y=z+d, x=y+d=z+2d.~%"))
    (newline)
    (display (format #f "x^2 - y^2 - z^2 = (z + 2d)^2 - (z + d)^2 - z^2 = 3d^2 + 2dz - z^2 = n > 0.~%"))
    (newline)
    (display (format #f "Make a change of variables, u=3d-z, v=d+z, then 3d^2 + 2dz - z^2 = n gets transformed to uv=n.  So for each n, we only need to check the divisor pairs of n such that d=(u+v)/4 and z=(3v-u)/4, and d and z are positive integers.  Alternatively, for each integer pair u and v, we can compute the product uv=n and see if d and z are integers, this way there is no need to compute the factors of n.~%"))
    (newline)
    (display (format #f "For example, when n=27, the divisors of 27 are 1 and 27, and 3 and 9.  For (1, 27), z=20 and d=7, and for (3, 9), z=6 and d=3.  Also when n=10, the divisors of 10 are (1, 10) and (2, 5).  For (1, 10), z=29/4 and d=11/4, and for (2, 5), z=13/4 and d=7/4.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-transform-uu-vv-to-xyzd-1 counter)
	   (run-test test-calc-solutions-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 100)
	  (num-solutions 2)
	  (status-num 10000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num num-solutions status-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-num 1000000)
	  (num-solutions 10)
	  (status-num 10000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num num-solutions status-num debug-flag)
	   ))
	))

    (newline)
    ))
