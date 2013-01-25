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
;;; define a macro to simplify code
(define-syntax handle-integer-sides-area-case
  (syntax-rules ()
    ((handle-integer-sides-area-case aa-times-3 area-times-3 max-perimeter
				     sum-of-perimeters num-triangles
				     loop-continue-flag plus-minus-num
				     debug-flag)
     (begin
       (let ((perimeter-1 (+ aa-times-3 plus-minus-num))
	     (area-1 (euclidean/ area-times-3 3)))
	 (begin
	   (if (< perimeter-1 max-perimeter)
	       (begin
		 (set! sum-of-perimeters (+ sum-of-perimeters perimeter-1))
		 (set! num-triangles (1+ num-triangles))

		 (if (equal? debug-flag #t)
		     (begin
		       (let ((aa-1 (euclidean/ aa-times-3 3)))
			 (let ((side-bb-1 (+ aa-1 plus-minus-num)))
			   (begin
			     (display (ice9-format:format
				       #f "(~:d, ~:d, ~:d), perimeter = ~:d, area = ~:d, sum-of-perimeters = ~:d~%"
				       aa-1 aa-1 side-bb-1 perimeter-1 area-1
				       sum-of-perimeters))
			     (force-output)
			     )))
		       )))
	       (begin
		 (set! loop-continue-flag #f)
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-perimeter debug-flag)
  (let ((sum-of-perimeters 0)
	(xx 2)
	(yy 1)
	(num-triangles 0)
	(loop-continue-flag #t)
	(counter 0)
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (while
       (equal? loop-continue-flag #t)
       (begin
	 (let ((aa-times-3-1 (+ (* 2 xx) 1))
	       (aa-times-3-2 (- (* 2 xx) 1))
	       (area-times-3-1 (* yy (+ xx 2)))
	       (area-times-3-2 (* yy (- xx 2))))
	   (begin
             ;;; b = a + 1
	     (if (and (> aa-times-3-1 0)
		      (zero? (modulo aa-times-3-1 3))
		      (> area-times-3-1 0)
		      (zero? (modulo area-times-3-1 3)))
		 (begin
		   (handle-integer-sides-area-case aa-times-3-1 area-times-3-1 max-perimeter
						   sum-of-perimeters num-triangles
						   loop-continue-flag 1 debug-flag)
		   ))
             ;;; b = a - 1
	     (if (and (> aa-times-3-2 0)
		      (zero? (modulo aa-times-3-2 3))
		      (> area-times-3-2 0)
		      (zero? (modulo area-times-3-2 3)))
		 (begin
		   (handle-integer-sides-area-case aa-times-3-2 area-times-3-2 max-perimeter
						   sum-of-perimeters num-triangles
						   loop-continue-flag -1 debug-flag)
		   ))
	     ))

         ;;; next xx and yy, (xx + yy*sqrt(3)) = (2+sqrt(3))^m, m>0
	 (let ((next-xx (+ (* xx 2) (* yy 3)))
	       (next-yy (+ (* yy 2) xx)))
	   (begin
	     (set! xx next-xx)
	     (set! yy next-yy)
	     ))
	 ))

      (display (ice9-format:format
		#f "found ~:d almost equilateral triangles with integeral side lengths and area with perimeter that doesn't exceed ~:d~%"
		num-triangles max-perimeter))
      (display (ice9-format:format
		#f "sum of perimeters of all almost equilateral triangles with integeral side lengths and area = ~:d~%"
		sum-of-perimeters))
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
    (display (format #f "Project Euler 94 - It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.~%"))
    (newline)
    (display (format #f "We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.~%"))
    (newline)
    (display (format #f "Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).~%"))
    (newline)
    (display (format #f "The solution using Pell's equation can be found at http://www.sosmath.com/CBB/viewtopic.php?t=16755&view=previous~%"))
    (newline)
    (display (format #f "This solution uses the standard equations for a triangle, area = b*h/2, h^2=a^2+b^2, and b=a+1 and b=a-1.~%"))
    (newline)
    (display (format #f "From the Pythagorean theorem, we have a^2=h^2+(b/2)^2, or a^2=h^2+((a +/- 1)/2)^2.~%"))
    (newline)
    (display (format #f "Squaring and combining terms, 3a^2 -/+ 2a - 1 - 4h^2 = 0. Multiply through by 3, add and subtract one to complete the square, gives (9a^2 -/+ 6a + 1) - 4 - 12h^2 = 0.~%"))
    (newline)
    (display (format #f "Dividing through by 4, we have ((3a -/+ 1)/2)^2-3h^2=1.  Let x=(3a -/+ 1)/2 and y=h, then we get Pell's equation x^2-3y^2=1.~%"))
    (newline)
    (display (format #f "To solve Pell's equation, see the lecture notes at http://www.math.ou.edu/~~kmartin/nti/chap5.pdf~%"))
    (newline)
    (display (format #f "All non-trivial solutions this verions of the Pell's equation are (for x and y), are given by the equation (x + y*sqrt(3)) = (2 + sqrt(3))^m, for m>0.~%"))
    (newline)
    (display (format #f "Plugging back in, we see that a = ((2*x) +/- 1) / 3.~%"))
    (newline)
    (display (format #f "and the height is h = y, so the area = (a +/- 1) * h / 2 = ((2x +/- 1)/3 +/- 1) * y / 2 = (x +/- 2) * y / 3.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-perimeter 200)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-perimeter debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-perimeter 1000000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-perimeter debug-flag)
	   ))
	))

    (newline)
    ))
