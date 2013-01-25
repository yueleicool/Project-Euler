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

;;;### ice-9 receive for the receive function (bind multiple values)
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
(define (calc-t-count ii-xx ii-yy max-num)
  (let ((xy-gcd (gcd ii-xx ii-yy)))
    (let ((dx (euclidean/ ii-xx xy-gcd))
	  (dy (euclidean/ ii-yy xy-gcd)))
      (let ((xx-count (euclidean/ (- max-num ii-xx) dy))
	    (yy-count (euclidean/ ii-yy dx)))
	(let ((result (* 2 (min xx-count yy-count))))
	  (begin
	    result
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-calc-t-count-1)
  (let ((sub-name "test-calc-t-count-1")
	(test-list
	 (list
	  (list 1 2 10 4) (list 1 2 6 4) (list 1 2 4 2)
	  (list 2 4 11 8) (list 2 4 10 8) (list 2 4 9 6)
	  (list 2 4 8 6) (list 2 4 7 4)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((ii-xx (list-ref alist 0))
		 (ii-yy (list-ref alist 1))
		 (max-num (list-ref alist 2))
		 (shouldbe (list-ref alist 3)))
	     (let ((result (calc-t-count ii-xx ii-yy max-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : ii-xx = ~a, ii-yy = ~a, max-num = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index ii-xx ii-yy max-num
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
(define (main-loop max-num)
  (let ((triangle-count (* 3 max-num max-num)))
    (begin
      (do ((ii-xx 1 (1+ ii-xx)))
	  ((> ii-xx max-num))
	(begin
	  (do ((ii-yy 1 (1+ ii-yy)))
	      ((> ii-yy max-num))
	    (begin
	      (let ((t-count (calc-t-count ii-xx ii-yy max-num)))
		(begin
		  (set! triangle-count (+ triangle-count t-count))
		  ))
	      ))
	  ))

      (display (ice9-format:format #f "Number of triangles between 0 and ~:d is ~:d~%" max-num triangle-count))
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
    (display (format #f "Project Euler 91 - The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin, O(0,0), to form triangle(OPQ).~%"))
    (newline)
    (display (format #f "There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies between 0 and 2 inclusive; that is,
0 <= x1, y1, x2, y2 <= 2.~%"))
    (newline)
    (display (format #f "Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be formed?~%"))
    (newline)
    (display (format #f "The solution was found at http://oop123.wordpress.com/2011/08/18/project-euler-91-java/~%"))
    (newline)
    (display (format #f "It uses the idea that if you are looking at a triangle with a point on the x-axis and another point on the y-axis, then it will be a right triangle, and those are easy to count.  For the rest, consider the point (1, 2), then a right triangle will be formed if you use the origin and the point (2, 1).  To see this, note that the slope of the from the origin to (1, 2) is 2, and the slope of the line from (1, 2) to (2, 1) is -1/2, which makes the two lines meet at right angles.~%"))
    (newline)
    (display (format #f "Also, need to account for the fact that (1, 2), can have multiple right triangles, the point (3, 1) gives a right triangle, but so does (5, 0).  So the triangles can be obtained by adding a delta to the initial point, (2, 1) = (1, 2) + 1*(2, -1), and (5, 0) = (1, 2) + 2*(2, -1), this delta can be computed from the slope (in reduced form).  See the image at http://oop123.files.wordpress.com/2011/08/with_slope_31.png?w=595~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-calc-t-count-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))

    (let ((max-num 2))
      (begin
	(main-loop max-num)
	))

    (newline)
    (force-output)

    (let ((max-num 50))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))

    (newline)
    ))
