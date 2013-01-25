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

;;;### srfi-11 for let-values function
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
;;; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
(define-syntax sqrt-cf-iterate
  (syntax-rules ()
    ((sqrt-cf-iterate int-dd int-sqrt-dd m0 d0 a0)
     (begin
       (let ((mn (- (* d0 a0) m0)))
	 (let ((dn (euclidean/ (- int-dd (* mn mn)) d0)))
	   (let ((an (euclidean/ (+ int-sqrt-dd mn) dn)))
	     (begin
	       (set! m0 mn)
	       (set! d0 dn)
	       (set! a0 an)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; method from
;;; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
(define (minimal-solution-pells-equation dd)
  (begin
    (srfi-11:let-values
     (((int-sqrt-num int-sqrt-remainder)
       (exact-integer-sqrt dd)))
     (begin
       (if (zero? int-sqrt-remainder)
	   (begin
	     #f)
	   (begin
	     (let ((m0 0)
		   (d0 1)
		   (a0 int-sqrt-num)
		   (hnm1 1)
		   (hnm2 0)
		   (knm1 0)
		   (knm2 1)
		   (xx 0)
		   (yy 0)
		   (loop-continue-flag #t))
	       (begin
		 (while
		  (equal? loop-continue-flag #t)
		  (begin
		    (let ((hn0 (+ (* a0 hnm1) hnm2))
			  (kn0 (+ (* a0 knm1) knm2)))
		      (let ((gcf (gcd hn0 kn0)))
			(let ((hn1 (euclidean/ hn0 gcf))
			      (kn1 (euclidean/ kn0 gcf)))
			  (begin
			    (set! hnm2 hnm1)
			    (set! knm2 knm1)
			    (set! hnm1 hn1)
			    (set! knm1 kn1)
			    (set! xx hn1)
			    (set! yy kn1)

			    (let ((pell-eqn (- (* hn1 hn1) (* dd kn1 kn1))))
			      (begin
				(if (= pell-eqn 1)
				    (begin
				      (set! loop-continue-flag #f)
				      ))
				))

			    ;;; find next m0, d0, a0 for next term in the continued fraction
			    (sqrt-cf-iterate dd int-sqrt-num m0 d0 a0)
			    ))
			))
		    ))

		 (list xx yy)
		 ))
	     ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-minimal-solution-pells-equation-1)
  (let ((sub-name "test-minimal-solution-pells-equation-1")
	(test-list
	 (list
	  (list 2 (list 3 2)) (list 3 (list 2 1))
	  (list 4 #f) (list 5 (list 9 4))
	  (list 6 (list 5 2)) (list 7 (list 8 3))
	  (list 9 #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((dd (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (minimal-solution-pells-equation dd)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : dd=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index dd
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
(define (main-loop start-num end-num debug-flag)
  (let ((largest-xx -1)
	(largest-dd -1))
    (begin
      (do ((dd start-num (1+ dd)))
	  ((> dd end-num))
	(begin
	  (let ((result-list (minimal-solution-pells-equation dd)))
	    (begin
	      (if (not (equal? result-list #f))
		  (begin
		    (let ((xx (list-ref result-list 0))
			  (yy (list-ref result-list 1)))
		      (begin
			(if (equal? debug-flag #t)
			    (begin
			      (display (ice9-format:format #f "  ~:d^2 - ~:dx~:d^2 = 1~%" xx dd yy))
			      (force-output)
			      ))

			(if (> xx largest-xx)
			    (begin
			      (set! largest-xx xx)
			      (set! largest-dd dd)
			      ))
			))
		    ))
	      ))
	  ))
      (display (ice9-format:format #f "The largest minimal solution is x = ~:d, when D = ~:d, for x^2-Dy^2=1 (between ~:d <= D <= ~:d)~%"
				   largest-xx largest-dd start-num end-num))
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
    (display (format #f "Problem 066 - Consider quadratic Diophantine equations of the form:~%"))
    (newline)
    (display (format #f "    x^2 - Dy^2 = 1~%"))
    (display (format #f "For example, when D=13, the minimal solution in x is 649^2 - 13x180^2 = 1.~%"))
    (display (format #f "It can be assumed that there are no solutions in positive integers when D is square.~%"))
    (newline)
    (display (format #f "By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:~%"))
    (display (format #f "    3^2 - 2x2^2 = 1~%"))
    (display (format #f "    2^2 - 3x12 = 1~%"))
    (display (format #f "    9^2 - 5x4^2 = 1~%"))
    (display (format #f "    5^2 - 6x2^2 = 1~%"))
    (display (format #f "    8^2 - 7x3^2 = 1~%"))
    (newline)
    (display (format #f "Hence, by considering minimal solutions in x for D <= 7, the largest x is obtained when D=5.~%"))
    (newline)
    (display (format #f "Find the value of D <= 1000 in minimal solutions of x for which the largest value of x is obtained.~%"))
    (newline)
    (display (format #f "The solution was described at http://www.mathblog.dk/project-euler-66-diophantine-equation/~%"))
    (newline)
    (display (format #f "First, find the convergents to the continued fraction for sqrt(n), hi/ki, using the continued fraction representation.  See http://en.wikipedia.org/wiki/Continued_fraction~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-minimal-solution-pells-equation-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 2)
	  (end-num 7)
	  (debug-flag #t))
      (begin
	(main-loop start-num end-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((start-num 2)
	  (end-num 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num debug-flag)
	   ))
	))

    (newline)
    ))
