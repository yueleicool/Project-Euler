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
;;; initial solutions to x^2-2y^2 = -1
(define (find-initial-solutions)
  (let ((max-nn 100)
	(continue-loop-flag #t))
    (begin
      (do ((xx 1 (1+ xx)))
	  ((> xx max-nn))
	(begin
	  (let ((xx-2 (* xx xx)))
	    (begin
	      (do ((yy 1 (1+ yy)))
		  ((> yy max-nn))
		(begin
		  (let ((yy-2 (* 2 yy yy)))
		    (begin
		      (if (= (- xx-2 yy-2) -1)
			  (begin
			    (display (ice9-format:format #f "  solution found (x=~:d, y=~:d)~%" xx yy))
			    (force-output)
			    ))
		      ))
		  ))
	      ))
	  ))
      )))


;;;#############################################################
;;;#############################################################
(define-syntax display-debug-info
  (syntax-rules ()
    ((display-debug-info oo-xx-1 oo-yy-1 aa bb cc
			 perimeter max-perimeter count)
     (begin
       (let ((this-count (euclidean/ max-perimeter perimeter)))
	 (begin
	   (display (format #f " (xx=~:d, yy=~:d) : aa=~:d, bb=~:d, cc=~:d, perimeter=~:d, max-perimeter=~:d, this-count=~:d, count=~:d~%"
			    oo-xx-1 oo-yy-1 aa bb cc perimeter max-perimeter
			    this-count count))
	   (force-output)
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-perimeter debug-flag)
  (let ((count 0)
	(xx-0 3)
	(yy-0 2)
	(seen-htable (make-hash-table))
	(oo-list (list (list 7 5) (list 41 29))))
    (begin
      (for-each
       (lambda (olist)
	 (begin
	   (let ((oo-xx-0 (list-ref olist 0))
		 (oo-yy-0 (list-ref olist 1))
		 (xx-1 3)
		 (yy-1 2))
	     (let ((oo-xx-1 oo-xx-0)
		   (oo-yy-1 oo-yy-0)
		   (continue-loop-flag #t))
	       (begin
		 (while
		  (equal? continue-loop-flag #t)
		  (begin
		    (let ((alist (list oo-xx-1 oo-yy-1)))
		      (let ((hflag (hash-ref seen-htable alist #f)))
			(begin
			  (if (and (equal? hflag #f)
				   (odd? oo-xx-1))
			      (begin
				(let ((perimeter (+ oo-xx-1 oo-yy-1)))
				  (begin
				    (if (< perimeter max-perimeter)
					(begin
					  (let ((this-count (euclidean/ max-perimeter perimeter)))
					    (begin
					      (set! count (+ count this-count))
					      (hash-set! seen-htable alist #t)

					      (if (equal? debug-flag #t)
						  (begin
						    (let ((aa (/ (+ oo-xx-1 1) 2))
							  (cc oo-yy-1))
						      (let ((bb (1- aa)))
							(begin
							  (display-debug-info oo-xx-1 oo-yy-1 aa bb cc
									      perimeter max-perimeter count)
							  )))
						    ))
					      ))
					  ))
				    ))
				))

                          ;;; next solution to x^2-2y^2=1
			  (let ((next-xx-1 (+ (* xx-0 xx-1) (* 2 yy-0 yy-1)))
				(next-yy-1 (+ (* xx-0 yy-1) (* yy-0 xx-1))))
			    (begin
			      (set! xx-1 next-xx-1)
			      (set! yy-1 next-yy-1)
			      ))

                          ;;; next solution to x^2-2y^2=-1
			  (let ((next-xx-1 (+ (* oo-xx-0 xx-1) (* 2 oo-yy-0 yy-1)))
				(next-yy-1 (+ (* oo-xx-0 yy-1) (* oo-yy-0 xx-1))))
			    (begin
			      (set! oo-xx-1 next-xx-1)
			      (set! oo-yy-1 next-yy-1)
			      ))

			  (let ((perimeter (+ oo-xx-1 oo-yy-1)))
			    (begin
			      (if (> perimeter max-perimeter)
				  (begin
				    (set! continue-loop-flag #f)
				    ))
			      ))
			  )))
		    ))
		 )))
	   )) oo-list)

      (display (ice9-format:format #f "Number of Pythagorean triangles = ~:d, which can tile a square with the hole made by four of those triangles, with perimeters less than ~:d~%"
				   count max-perimeter))
      (force-output)
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
    (display (format #f "Project Euler 139 - Let (a, b, c) represent the three sides of a right angle triangle with integral length sides. It is possible to place four such triangles together to form a square with length c.~%"))
    (newline)
    (display (format #f "For example, (3, 4, 5) triangles can be placed together to form a 5 by 5 square with a 1 by 1 hole in the middle and it can be seen that the 5 by 5 square can be tiled with twenty-five 1 by 1 squares.  (See http://projecteuler.net/project/images/p_139.gif)~%"))
    (newline)
    (display (format #f "However, if (5, 12, 13) triangles were used then the hole would measure 7 by 7 and these could not be used to tile the 13 by 13 square.~%"))
    (newline)
    (display (format #f "Given that the perimeter of the right triangle is less than one-hundred million, how many Pythagorean triangles would allow such a tiling to take place?~%"))
    (newline)
    (display (format #f "A solution was found at http://d.hatena.ne.jp/Rion778/20110521/1305960442~%"))
    (newline)
    (display (format #f "Instead of generating pythagorean triples, with a little bit of algebra a much faster solution was found.~%"))
    (newline)
    (display (format #f "By the Pythagorean theorem, a^2+b^2=c^2.  Let a-b=d, then the difference d must evenly divide c^2 in order for the inner square to tile the cxc square.  This means that c=y*d for some integer y.  So, a^2+b^2=c^2 can be written as a^2+(a-d)^2=y^2*d^2 or 2a^2-2ad+d^2=y^2*d^2.  Multiply by 2 to complete the square, gives (2a-d)^2+d^2=2y^2*d^2, divide through by d^2, (2a/d-1)^2-2y^2=-1, let x=2a/d-1, then x^2-2y^2=-1 is Pell's equation.  Since (x=1, y=1) corresponds to a=0, we use another initial solution, (x=7, y=5).~%"))
    (newline)
    (display (format #f "Also, since x=2a/d-1 is an integer, we have 2a/d-1 an integer, or d always divides a evenly, we are free to choose d=1, then x=2a-1 and x is odd (look for odd solutions of the Pell's equation).~%"))
    (newline)
    (display (format #f "First find all solutions for x^2-2y^2=1, initial solution (x=3, y=2, see http://en.wikipedia.org/wiki/Pell's_equation), then compose that solution with x^2-2y^2=-1 to generate all the rest.  Second, for each solution, check to see if the perimeter, a+b+c = 2a-d+yd = (x+y)*d < 100,000,000.~%"))
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
    (find-initial-solutions)

    (let ((max-perimeter 15)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-perimeter debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-perimeter 100000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-perimeter debug-flag)
	   ))
	))

    (newline)
    ))
