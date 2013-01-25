#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice9-format:)))

;;;### srfi-1 for let-values (multiple value binding)
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
		   (* 1000.0
		      (- nsecs (+ (* nhours 60.0 60.0) (* nminutes 60.0))))))))
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
;;; define a macro to simplify code
(define-syntax process-mm-nn
  (syntax-rules ()
    ((process-mm-nn mm nn mm-2 nn-2 max-perimeter
		    perimeter-htable result-count)
     (begin
       (let ((aa (- mm-2 nn-2))
	     (bb (* 2 mm nn))
	     (cc (+ mm-2 nn-2)))
	 (let ((pp (+ aa bb cc))
	       (tmp-pp (+ aa bb cc)))
	   (begin
	     (while
	      (< tmp-pp max-perimeter)
	      (begin
		(let ((count (hash-ref perimeter-htable tmp-pp 0)))
		  (begin
		    (cond
		     ((= count 0)
		      (begin
			(set! result-count (1+ result-count))
			(hash-set! perimeter-htable tmp-pp 1)
			))
		     ((= count 1)
		      (begin
			(set! result-count (1- result-count))
			(hash-set! perimeter-htable tmp-pp 2)
			)))

		    (hash-set! perimeter-htable tmp-pp (1+ count))
		    ))
		(set! tmp-pp (+ tmp-pp pp))
		))
	     ))
	 ))
     )))

;;;#############################################################
;;;#############################################################
(define (calculate-number-of-elements max-perimeter max-kk)
  (let ((mm-max (1+ (exact-integer-sqrt (euclidean/ max-perimeter 2))))
	(perimeter-htable (make-hash-table 10000))
	(result-count 0))
    (begin
      (do ((mm 1 (1+ mm)))
	  ((> mm mm-max))
	(begin
	  (let ((mm-2 (* mm mm)))
	    (begin
	      (do ((nn 1 (1+ nn)))
		  ((>= nn mm))
		(begin
		  (if (and (equal? (gcd mm nn) 1)
			   (odd? (- mm nn)))
		      (begin
			(let ((nn-2 (* nn nn)))
			  (begin
			    (process-mm-nn mm nn mm-2 nn-2 max-perimeter
					   perimeter-htable result-count)
			    ))
			))
		  ))
	      ))
	  ))

      result-count
      )))

;;;#############################################################
;;;#############################################################
(define (test-calculate-number-of-elements-1)
  (let ((sub-name "test-calculate-number-of-elements-1"))
    (let ((test-list
	   (list
	    (list 15 4 1)
	    (list 20 4 1)
	    (list 25 4 2)
	    (list 32 4 3)
	    (list 38 4 4)
	    (list 42 4 5)
	    (list 50 4 6)
	    ))
	  (test-label-index 0))
      (for-each
       (lambda (alist)
	 (begin
	   (let ((max-perimeter (list-ref alist 0))
		 (max-kk (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (calculate-number-of-elements max-perimeter max-kk)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : max-perimeter = ~a, max-kk = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index max-perimeter
					max-kk shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-perimeter max-kk)
  (let ((result (calculate-number-of-elements max-perimeter max-kk)))
    (begin
      (display
       (ice9-format:format
	#f "there are ~:d elements with exactly 1 integer sided triangle for L <= ~:d~%"
	result max-perimeter))
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
    (display (format #f "Problem 075 - It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle in exactly one way, but there are many more examples.~%"))
    (newline)
    (display (format #f "  12 cm: (3,4,5)~%"))
    (display (format #f "  24 cm: (6,8,10)~%"))
    (display (format #f "  30 cm: (5,12,13)~%"))
    (display (format #f "  36 cm: (9,12,15)~%"))
    (display (format #f "  40 cm: (8,15,17)~%"))
    (display (format #f "  48 cm: (12,16,20)~%"))
    (newline)
    (display (format #f "In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle, and other lengths allow more than one solution to be found; for example, using 120 cm it is possible to form exactly three different integer sided right angle triangles.~%"))
    (display (format #f "120 cm: (30,40,50), (20,48,52), (24,45,51)~%"))
    (newline)
    (display (format #f "Given that L is the length of the wire, for how many values of L <= 1,500,000 can exactly one integer sided right angle triangle be formed?~%"))
    (display (format #f "Note: This problem has been changed recently, please check that you are using the right parameters.~%"))
    (newline)
    (display (format #f "The solution has been described at http://www.mathblog.dk/project-euler-75-lengths-of-wire-right-angle-triangle/~%"))
    (newline)
    (display (format #f "And the generation of pythagorean triples can be found at http://en.wikipedia.org/wiki/Pythagorean_triple~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-calculate-number-of-elements-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-perimeter 50)
	  (max-kk 4))
      (begin
	(main-loop max-perimeter max-kk)
	))

    (newline)
    (force-output)

    (let ((max-perimeter 1500000)
	  (max-kk 10))
      (begin
	(time-code
	 (begin
	   (main-loop max-perimeter max-kk)
	   ))
	))

    (newline)
    ))
