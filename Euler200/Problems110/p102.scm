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
;;; assume data-list = (p-a-x p-a-y p-b-x p-b-y p-c-x p-c-y)
(define (is-origin-within-triangle? data-list)
  (let ((p-a-x (list-ref data-list 0))
	(p-a-y (list-ref data-list 1))
	(p-b-x (list-ref data-list 2))
	(p-b-y (list-ref data-list 3))
	(p-c-x (list-ref data-list 4))
	(p-c-y (list-ref data-list 5))
	(or-x 0)
	(or-y 0))
    (let ((v0-x (- p-b-x p-a-x))
	  (v0-y (- p-b-y p-a-y))
	  (v1-x (- p-c-x p-a-x))
	  (v1-y (- p-c-y p-a-y))
	  (v2-x (- or-x p-a-x))
	  (v2-y (- or-y p-a-y)))
      (let ((vv00 (+ (* v0-x v0-x) (* v0-y v0-y)))
	    (vv11 (+ (* v1-x v1-x) (* v1-y v1-y)))
	    (vv01 (+ (* v0-x v1-x) (* v0-y v1-y)))
	    (vv20 (+ (* v2-x v0-x) (* v2-y v0-y)))
	    (vv21 (+ (* v2-x v1-x) (* v2-y v1-y))))
	(let ((det (- (* vv00 vv11) (* vv01 vv01))))
	  (begin
	    (if (not (zero? det))
		(begin
		  (let ((u (/ (- (* vv20 vv11) (* vv21 vv01)) det))
			(v (/ (- (* vv21 vv00) (* vv20 vv01)) det)))
		    (begin
		      (if (and (>= u 0)
			       (>= v 0)
			       (<= (+ u v) 1))
			  (begin
			    #t)
			  (begin
			    #f
			    ))
		      )))
		(begin
		  (display (format #f "is-origin-within-triangle? ~a error : determinent = 0~%" data-list))
		  (force-output)
		  (quit)
		  ))
	    ))))
    ))

;;;#############################################################
;;;#############################################################
(define (test-is-origin-within-triangle-1)
  (let ((sub-name "test-is-origin-within-triangle-1")
	(test-list
	 (list
	  (list (list -340 495 -153 -910 835 -947) #t)
	  (list (list -175 41 -421 -714 574 -645) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((points-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (is-origin-within-triangle? points-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : points list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index points-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))
    ))

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
			    (let ((plist (map string->number llist)))
			      (begin
				(set! line-counter (1+ line-counter))
				(set! results-list
				      (cons plist results-list))
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
	    )))))

;;;#############################################################
;;;#############################################################
(define (sub-loop triangles-list debug-flag)
  (let ((triangles-counter 0)
	(origin-counter 0))
    (begin
      (if (and (list? triangles-list)
	       (> (length triangles-list) 1))
	  (begin
	    (for-each
	     (lambda (a-list)
	       (begin
		 (set! triangles-counter (1+ triangles-counter))

		 (let ((bflag (is-origin-within-triangle? a-list)))
		   (begin
		     (if (equal? bflag #t)
			 (begin
			   (set! origin-counter (1+ origin-counter))
			   ))
		     (if (equal? debug-flag #t)
			 (begin
			   (display (ice9-format:format #f "points list = ~a : contains origin = ~a~%"
							a-list (if (equal? bflag #t) "true" "false")))
			   (force-output)
			   ))
		     ))
		 )) triangles-list)

	    (display (ice9-format:format #f "there are ~:d triangles that contain the origin, out of ~:d~%"
					 origin-counter triangles-counter))
	    (force-output)
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop filename debug-flag)
  (let ((triangles-list (read-in-file filename)))
    (begin
      (sub-loop triangles-list debug-flag)
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
    (display (format #f "Project Euler 102 - Three distinct points are plotted at random on a Cartesian plane, for which -1000 <= x, y <= 1000, such that a triangle is formed.~%"))
    (newline)
    (display (format #f "Consider the following two triangles:~%"))
    (newline)
    (display (format #f "    A(-340,495), B(-153,-910), C(835,-947)~%"))
    (display (format #f "    X(-175,41), Y(-421,-714), Z(574,-645)~%"))
    (newline)
    (display (format #f "It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.~%"))
    (newline)
    (display (format #f "Using triangles.txt (right click and 'Save Link/Target As...'), a 27K text file containing the co-ordinates of one thousand 'random' triangles, find the number of triangles for which the interior contains the origin.~%"))
    (newline)
    (display (format #f "NOTE: The first two examples in the file represent the triangles in the example given above.~%"))
    (newline)
    (display (format #f "this algorithm uses barycentric coordinates to see if a point is in a triangle, http://www.blackpawn.com/texts/pointinpoly/default.html~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-is-origin-within-triangle-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (time-code
     (begin
       (let ((points-list-list
	      (list (list -340 495 -153 -910 835 -947)
		    (list -175 41 -421 -714 574 -645)))
	     (debug-flag #t))
	 (begin
	   (sub-loop points-list-list debug-flag)
	   ))
       ))

    (newline)
    (force-output)

    (time-code
     (begin
       (let ((filename "triangles.txt")
	     (debug-flag #f))
	 (begin
	   (main-loop filename debug-flag)
	   ))
       ))

    (newline)
    (force-output)

    ))
