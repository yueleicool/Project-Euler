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
(define (partial-count-cuboids aa bb)
  (if (<= bb aa)
      (begin
	(let ((b-half (euclidean/ bb 2)))
	  (begin
	    b-half
	    )))
      (begin
	(let ((bp1 (euclidean/ (+ bb 1) 2)))
	  (begin
	    (let ((cube-count (+ 1 (- aa bp1))))
	      (begin
		(max 0 cube-count)
		))
	    ))
	)))

;;;#############################################################
;;;#############################################################
(define (test-partial-count-cuboids-1)
  (let ((sub-name "test-partial-count-cuboids-1")
	(test-list
	 (list
	  (list 3 4 2)
	  (list 6 8 3)
	  (list 9 12 4)
	  (list 5 12 0)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((aa (list-ref a-list 0))
		 (bb (list-ref a-list 1))
		 (shouldbe (list-ref a-list 2)))
	     (let ((result (partial-count-cuboids aa bb)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : aa = ~a, bb = ~a, max-len = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index aa bb max-len
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
(define (calculate-cuboids-fixed-max aa)
  (let ((count 0)
	(max-bb (* 2 aa))
	(aa-2 (* aa aa)))
    (begin
      (do ((bb 1 (1+ bb)))
	  ((> bb max-bb))
	(begin
	  (let ((bb-2 (* bb bb)))
	    (let ((dd-sqrt (sqrt (+ aa-2 bb-2))))
	      (let ((ii-sqrt (truncate dd-sqrt)))
		(begin
		  (if (equal? dd-sqrt ii-sqrt)
		      (begin
			(let ((cube-count (partial-count-cuboids aa bb)))
			  (begin
			    (set! count (+ count cube-count))
			    ))
			))
		  ))
	      ))
	  ))
      count
      )))

;;;#############################################################
;;;#############################################################
(define (test-calculate-cuboids-fixed-max-1)
  (let ((sub-name "test-calculate-cuboids-fixed-max-1")
	(test-list
	 (list
	  (list 1 0) (list 2 0)
	  (list 3 2) (list 4 1)
	  (list 100 85)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((aa (list-ref a-list 0))
		 (shouldbe (list-ref a-list 1)))
	     (let ((result (calculate-cuboids-fixed-max aa)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : aa = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index aa shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; use euclids formula to generate pythagorean triples
(define (cummulative-count-cuboids max-len)
  (let ((cuboid-count 0))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((> ii max-len))
	(begin
	  (let ((c-count (calculate-cuboids-fixed-max ii)))
	    (begin
	      (if (> c-count 0)
		  (begin
		    (set! cuboid-count (+ cuboid-count c-count))
		    ))
	      ))
	  ))

      cuboid-count
      )))

;;;#############################################################
;;;#############################################################
(define (test-cummulative-count-cuboids-1)
  (let ((sub-name "test-cummulative-count-cuboids-1")
	(test-list
	 (list
	  (list 1 0) (list 2 0)
	  (list 3 2) (list 4 3)
	  (list 5 3) (list 6 6)
	  (list 7 6) (list 8 10)
	  (list 9 14) (list 10 14)
	  (list 11 14) (list 12 25)
	  (list 13 25) (list 14 25)
	  (list 15 35) (list 20 67)
	  (list 25 113)
	  (list 99 1975)
	  (list 100 2060)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((max-len (list-ref a-list 0))
		 (shouldbe (list-ref a-list 1)))
	     (let ((result (cummulative-count-cuboids max-len)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : max-len = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index max-len shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-length target-number-solutions status-num)
  (let ((start-jday (srfi-19:current-julian-day))
	(current-mm 0)
	(current-number-solutions 0)
	(previous-mm 0)
	(previous-number-solutions 0)
	(continue-loop-flag #t))
    (begin
      (do ((max-len 1 (1+ max-len)))
	  ((or (> max-len max-length)
	       (equal? continue-loop-flag #f)))
	(begin
	  (let ((cuboid-count (calculate-cuboids-fixed-max max-len)))
	    (begin
	      (set! previous-mm current-mm)
	      (set! previous-number-solutions current-number-solutions)
	      (set! current-mm max-len)
	      (set! current-number-solutions (+ current-number-solutions cuboid-count))
	      (if (> current-number-solutions target-number-solutions)
		  (begin
		    (set! continue-loop-flag #f)
		    ))
	      ))

	  (if (zero? (modulo max-len status-num))
	      (begin
		(let ((end-jday (srfi-19:current-julian-day)))
		  (begin
		    (display (ice9-format:format #f "completed ~:d out of ~:d : " current-mm max-length))
		    (display (ice9-format:format #f "so far cuboids = ~:d : "
						 current-number-solutions))
		    (display (ice9-format:format #f "elapsed time = ~a : ~a~%"
						 (julian-day-difference-to-string end-jday start-jday)
						 (date-time-to-string (srfi-19:current-date))))
		    (force-output)
		    (set! start-jday end-jday)
		    ))
		))
	  ))

      (if (equal? continue-loop-flag #t)
	  (begin
	    (display (ice9-format:format
		      #f "no results found for max-mm = ~:d : last mm = ~:d, last number of solutions = ~:d~%"
		      max-length previous-mm previous-number-solutions))
	    (force-output))
	  (begin
	    (display (ice9-format:format
		      #f "max m=~:d : has exactly ~:d cuboids for which the shortest distance is an integer~%"
		      current-mm current-number-solutions))
	    (display (ice9-format:format
		      #f "previous max m=~:d : has exactly ~:d cuboids for which the shortest distance is an integer~%"
		      previous-mm previous-number-solutions))
	    (force-output)
	    ))
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
    (display (format #f "Project Euler 86: - A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits in the opposite corner. By travelling on the surfaces of the room the shortest straight line distance from S to F is 10 and the path is shown on the diagram.~%"))
    (newline)
    (display (format #f "However, there are up to three shortest path candidates for any given cuboid and the shortest route is not always integer.~%"))
    (newline)
    (display (format #f "By considering all cuboid rooms with integer dimensions, up to a maximum size of M by M by M, there are exactly 2060 cuboids for which the shortest distance is integer when M=100, and this is the least value of M for which the number of solutions first exceeds two thousand; the number of solutions is 1975 when M=99.~%"))
    (newline)
    (display (format #f "Find the least value of M such that the number of solutions first exceeds one million.~%"))
    (newline)

    (display (format #f "for a good understanding of the problem see http://keyzero.wordpress.com/2011/05/24/project-euler-problem-86/~%"))
    (display (format #f "for a very efficient solution to the problem see http://www.mathblog.dk/project-euler-86-shortest-path-cuboid/~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-partial-count-cuboids-1 counter)
	   (run-test test-calculate-cuboids-fixed-max-1 counter)
	   (run-test test-cummulative-count-cuboids-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-length 1000)
	  (target-solutions 2000)
	  (status-num 1000))
      (begin
	(time-code
	 (begin
	   (main-loop max-length target-solutions status-num)
	   ))
	))
    (newline)
    (force-output)

    (let ((max-length 100000)
	  (target-solutions 1000000)
	  (status-num 1000))
      (begin
	(time-code
	 (begin
	   (main-loop max-length target-solutions status-num)
	   ))
	))

    (newline)
    ))
