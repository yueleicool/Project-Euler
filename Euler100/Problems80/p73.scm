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
(define (brute-force-counting max-num start-numer start-denom
			      end-numer end-denom)
  (let ((count 0))
    (begin
      (do ((dd 4 (1+ dd)))
	  ((> dd max-num))
	(begin
	  (let ((start-num (+ (euclidean/ dd start-denom) 1))
		(end-num (euclidean/ dd end-denom)))
	    (begin
	      (do ((nn start-num (1+ nn)))
		  ((> nn end-num))
		(begin
		  (let ((div (gcd nn dd)))
		    (begin
		      (if (= div 1)
			  (begin
			    (set! count (1+ count))
			    ))
		      ))
		  ))
	      ))
	  ))
      count
      )))

;;;#############################################################
;;;#############################################################
(define (test-brute-force-counting-1)
  (let ((sub-name "test-brute-force-counting-1"))
    (let ((test-list
	   (list
	    (list 5 1 3 1 2 1)
	    (list 6 1 3 1 2 1)
	    (list 7 1 3 1 2 2)
	    (list 8 1 3 1 2 3)
	    ))
	  (test-label-index 0))
      (for-each
       (lambda (alist)
	 (begin
	   (let ((max-num (list-ref alist 0))
		 (start-numer (list-ref alist 1))
		 (start-denom (list-ref alist 2))
		 (end-numer (list-ref alist 3))
		 (end-denom (list-ref alist 4))
		 (shouldbe (list-ref alist 5)))
	     (let ((result (brute-force-counting max-num start-numer start-denom
						 end-numer end-denom)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : max-num = ~a, low = ~a/~a, high = ~a/~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index max-num
					start-numer start-denom
					end-numer end-denom
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
(define (find-ratio-immediate-right max-num start-numer start-denom
				    end-numer end-denom)
  (let ((left-numer start-numer)
	(left-denom start-denom)
	(right-numer end-numer)
	(right-denom end-denom)
	(mid-numer 0)
	(mid-denom 0)
	(target-numer start-numer)
	(target-denom start-denom))
    (begin
      (while
       (<= (+ left-denom right-denom) max-num)
       (begin
	 ;;; compute the mediant
	 (set! mid-numer (+ left-numer right-numer))
	 (set! mid-denom (+ left-denom right-denom))

	 ;;; if mid ratio is greater than 3/7, then mid-numer*7 > mid-denom*3
	 (if (or
	      (and (= mid-numer target-numer)
		   (= mid-denom target-denom))
	      (< (* mid-numer target-denom)
		 (* mid-denom target-numer)))
	     (begin
	       (set! left-numer mid-numer)
	       (set! left-denom mid-denom))
	     (begin
	       (set! right-numer mid-numer)
	       (set! right-denom mid-denom)
	       ))
	 ))

      (list right-numer right-denom)
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-ratio-immediate-right-1)
  (let ((sub-name "test-calculate-number-in-range-1")
	(test-list
	 (list
	  (list 5 1 3 1 2 (list 2 5))
	  (list 6 1 3 1 2 (list 2 5))
	  (list 7 1 3 1 2 (list 2 5))
	  (list 8 1 3 1 2 (list 3 8))
	  ))
	(test-label-index 0))
      (begin
	(for-each
	 (lambda (alist)
	   (begin
	     (let ((max-num (list-ref alist 0))
		   (start-numer (list-ref alist 1))
		   (start-denom (list-ref alist 2))
		   (end-numer (list-ref alist 3))
		   (end-denom (list-ref alist 4))
		   (shouldbe (list-ref alist 5)))
	       (let ((result (find-ratio-immediate-right
			      max-num start-numer start-denom
			      end-numer end-denom)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : (~a) : error : max-num = ~a, start = ~a/~a, end = ~a/~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index max-num
					  start-numer start-denom
					  end-numer end-denom shouldbe result))
			 (quit)
			 ))
		   )))
	     (set! test-label-index (1+ test-label-index))
	     ))
	 test-list)
	)))

;;;#############################################################
;;;#############################################################
(define (calculate-number-in-range max-num start-numer start-denom
				   end-numer end-denom)
  (let ((count 0)
	(next-list (find-ratio-immediate-right max-num start-numer start-denom
					       end-numer end-denom)))
    (let ((next-numer (list-ref next-list 0))
	  (next-denom (list-ref next-list 1)))
      (let ((aa start-numer)
	    (bb start-denom)
	    (cc next-numer)
	    (dd next-denom))
	(begin
	  (while
	   (not (and (= cc end-numer)
		     (= dd end-denom)))
	   (begin
	     (let ((kk (euclidean/ (+ max-num bb) dd)))
	       (let ((next-cc (- (* kk cc) aa))
		     (next-dd (- (* kk dd) bb)))
		 (begin
		   (set! count (1+ count))
		   (set! aa cc)
		   (set! bb dd)
		   (set! cc next-cc)
		   (set! dd next-dd)
		   )))
	     ))

	  count
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-calculate-number-in-range-1)
  (let ((sub-name "test-calculate-number-in-range-1")
	(test-list
	 (list
	  (list 5 1 3 1 2 1)
	  (list 6 1 3 1 2 1)
	  (list 7 1 3 1 2 2)
	  (list 8 1 3 1 2 3)
	  ))
	(test-label-index 0))
      (begin
	(for-each
	 (lambda (alist)
	   (begin
	     (let ((max-num (list-ref alist 0))
		   (start-numer (list-ref alist 1))
		   (start-denom (list-ref alist 2))
		   (end-numer (list-ref alist 3))
		   (end-denom (list-ref alist 4))
		   (shouldbe (list-ref alist 5)))
	       (let ((result (calculate-number-in-range
			      max-num start-numer start-denom
			      end-numer end-denom)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : (~a) : error : max-num = ~a, low = ~a/~a, high = ~a/~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index max-num
					  start-numer start-denom
					  end-numer end-denom shouldbe result))
			 (quit)
			 ))
		   )))
	     (set! test-label-index (1+ test-label-index))
	     ))
	 test-list)
	)))

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
(define (main-loop max-num start-numer start-denom end-numer end-denom)
  (begin
    (time-code
     (begin
       (let ((result-num (calculate-number-in-range max-num start-numer start-denom
						    end-numer end-denom)))
	 (begin
	   (display
	    (ice9-format:format
	     #f "there are ~:d elements the set of reduced proper fractions that are strictly between ~:d/~:d and ~:d/~:d, (with denominators less than ~:d, farey next-neighbors method)~%"
	     result-num start-numer start-denom end-numer end-denom max-num))
	   (force-output)
	   ))
       ))

    (newline)
    (time-code
     (begin
       (let ((result-num (brute-force-counting
			  max-num start-numer start-denom
			  end-numer end-denom)))
	 (begin
	   (display
	    (ice9-format:format
	     #f "there are ~:d elements the set of reduced proper fractions that are strictly between ~:d/~:d and ~:d/~:d, (with denominators less than ~:d, brute-force method)~%"
	     result-num start-numer start-denom end-numer end-denom max-num))
	   (force-output)
	   ))
       ))
    (newline)
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
(define (main args)
  (begin
    (display (format #f "Problem 073 - Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.~%"))
    (newline)
    (display (format #f "If we list the set of reduced proper fractions for d <= 8 in ascending order of size, we get:~%"))
    (newline)
    (display (format #f "  1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8~%"))
    (newline)
    (display (format #f "It can be seen that there are 3 fractions between 1/3 and 1/2.~%"))
    (newline)
    (display (format #f "How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d <= 12,000?~%"))
    (display (format #f "Note: The upper limit has been changed recently.~%"))
    (newline)
    (display (format #f "This program uses the nearest neighbor method for counting the number of elements in a Farey sequence.  See the wikipedia for a description of the Farey sequence and it's generation http://en.wikipedia.org/wiki/Farey_sequence~%"))
    (newline)
    (display (format #f "For a description of the solution see: http://www.mathblog.dk/project-euler-73-sorted-reduced-proper-fractions/~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-brute-force-counting-1 counter)
	   (run-test test-find-ratio-immediate-right-1 counter)
	   (run-test test-calculate-number-in-range-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 8)
	  (start-numer 1)
	  (start-denom 3)
	  (end-numer 1)
	  (end-denom 2))
      (begin
	(main-loop max-num start-numer start-denom end-numer end-denom)
	))

    (newline)
    (force-output)

    (let ((max-num 12000)
	  (start-numer 1)
	  (start-denom 3)
	  (end-numer 1)
	  (end-denom 2))
      (begin
        (main-loop max-num start-numer start-denom end-numer end-denom)
	))

    (newline)
    ))
