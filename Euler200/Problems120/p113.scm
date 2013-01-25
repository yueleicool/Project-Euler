#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;; this code is dedicated to the public domain

;;; srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;; ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice9-format:)))

;;; srfi-11 for let-values (multiple value bind)
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
(define (factorial ii-num)
  (begin
    (cond
     ((<= ii-num 1) 1)
     ((= ii-num 2) 2)
     ((= ii-num 3) 6)
     ((= ii-num 4) 24)
     ((= ii-num 5) 120)
     ((= ii-num 6) 720)
     ((= ii-num 7) 5040)
     ((= ii-num 8) 40320)
     ((= ii-num 9) 362880)
     (else
      (* ii-num (factorial (- ii-num 1)))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-factorial-1)
  (let ((sub-name "test-factorial-1")
	(test-list
	 (list
	  (list 0 1) (list 1 1) (list 2 2)
	  (list 3 6) (list 4 24) (list 5 120)
	  (list 6 720) (list 7 5040)
	  (list 8 40320) (list 9 362880)
	  (list 10 3628800)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (factorial test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (choose kk nn)
  (define (local-partial-factorial larger-num smaller-num)
    (let ((result 1))
      (begin
	(do ((ii larger-num (- ii 1)))
	    ((<= ii smaller-num))
	  (begin
	    (set! result (* result ii))
	    ))
	result
	)))
  (cond
   ((= kk 0) 0)
   ((= nn 0) 1)
   ((= kk nn) 1)
   ((>= kk nn)
    (begin
      (let ((kfact (local-partial-factorial kk nn))
	    (nfact (factorial (- kk nn))))
	(let ((result (/ kfact nfact)))
	  (begin
	    result
	    )))))
   (else
    ;;; (< kk nn)
    (begin
      (let ((kfact (factorial (- nn kk)))
	    (nfact (local-partial-factorial nn kk)))
	(let ((result (/ nfact kfact)))
	  result
	  ))))
   ))

;;;#############################################################
;;;#############################################################
(define (test-choose-1)
  (let ((sub-name "test-choose-1")
	(test-list
	 (list
	  (list 0 0 0) (list 1 0 1) (list 0 1 0)
	  (list 2 0 1) (list 2 1 2) (list 2 2 1)
	  (list 3 0 1) (list 3 1 3) (list 3 2 3) (list 3 3 1)
	  (list 4 0 1) (list 4 1 4) (list 4 2 6) (list 4 3 4)
	  (list 4 4 1)
	  (list 5 0 1) (list 5 1 5) (list 5 2 10) (list 5 3 10)
	  (list 5 4 5) (list 5 5 1)
	  (list 6 0 1) (list 6 1 6) (list 6 2 15) (list 6 3 20)
	  (list 6 4 15) (list 6 5 6) (list 6 6 1)
	  (list 0 6 0) (list 1 6 6) (list 2 6 15) (list 3 6 20)
	  (list 4 6 15) (list 5 6 6) (list 6 6 1)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-kk (list-ref alist 0))
		 (test-nn (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (let ((result-num (choose test-kk test-nn)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error : kk = ~a, nn = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-kk test-nn
					shouldbe-num result-num))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; lattice path algorithm assumes going from (0,0) to (a,b)
;;; to map to the problem, need to reduce 1 - 9 digits to 0 - 8
(define (count-increasing-numbers num-digits)
  (begin
    (cond
     ((<= num-digits 1) #f)
     (else
      (let ((nn (+ num-digits 8)))
	(let ((ncr (choose nn 8)))
	  (let ((count (- ncr 9)))
	    (begin
              ;;; removed the 9 flat numbers, 11111, 22222,..., 99999
	      count
	      ))
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-increasing-numbers-1)
  (let ((sub-name "test-count-increasing-numbers-1")
	(test-list
	 (list
	  (list 2 36) (list 3 156)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-digits (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (count-increasing-numbers num-digits)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : num-digits = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num-digits
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
;;; lattice path algorithm assumes going from (0,0) to (a,b)
(define (count-decreasing-numbers num-digits)
  (begin
    (cond
     ((<= num-digits 1) #f)
     (else
      (let ((nn (+ num-digits 9)))
	(let ((ncr (choose nn 9)))
	  (let ((count (- ncr 10)))
	    (begin
              ;;; removed the 10 flat numbers, 00000, 11111, 22222,..., 99999
	      count
	      ))
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-decreasing-numbers-1)
  (let ((sub-name "test-count-decreasing-numbers-1")
	(test-list
	 (list
	  (list 2 45) (list 3 210)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-digits (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (count-decreasing-numbers num-digits)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : num-digits = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num-digits
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
(define (reproduce-problem-statement)
  (let ((increasing-2d (count-increasing-numbers 2))
	(decreasing-2d (count-decreasing-numbers 2))
	(flat-2d 9)
	(increasing-3d (count-increasing-numbers 3))
	(decreasing-3d (count-decreasing-numbers 3))
	(flat-3d 9))
    (let ((nbouncy-2d (- 90 (+ increasing-2d decreasing-2d flat-2d)))
	  (nbouncy-3d (- 900 (+ increasing-3d decreasing-3d flat-3d))))
      (let ((total-2d (+ nbouncy-2d increasing-2d decreasing-2d flat-2d))
	    (total-3d (+ nbouncy-3d increasing-3d decreasing-3d flat-3d)))
	(begin
	  (display (ice9-format:format #f "two digit numbers: increasing=~:d, decreasing=~:d, flat=~:d, bouncy=~:d, total=~:d~%"
				       increasing-2d decreasing-2d flat-2d nbouncy-2d total-2d))
	  (display (ice9-format:format #f "three digit numbers: increasing=~:d, decreasing=~:d, flat=~:d, bouncy=~:d, total=~:d~%"
				       increasing-3d decreasing-3d flat-3d nbouncy-3d total-3d))
	  (force-output)
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-exponent debug-flag)
  (let ((increasing-count 0)
	(decreasing-count 0)
	(flat-count 9)
	(bouncy-count 0)
	(total-count 90))
    (begin
      (do ((ii 2 (1+ ii)))
	  ((> ii max-exponent))
	(begin
	  (let ((ii-increasing (count-increasing-numbers ii))
		(ii-decreasing (count-decreasing-numbers ii))
		(ii-flat 9))
	    (begin
	      (let ((ii-bouncy (- total-count
				  (+ ii-increasing ii-decreasing ii-flat))))
		(begin
		  (set! increasing-count (+ increasing-count ii-increasing))
		  (set! decreasing-count (+ decreasing-count ii-decreasing))
		  (set! flat-count (+ flat-count ii-flat))
		  (set! bouncy-count (+ bouncy-count ii-bouncy))

		  (if (equal? debug-flag #t)
		      (begin
			(display (ice9-format:format #f "~:d digit numbers: increasing=~:d, decreasing=~:d, flat=~:d, bouncy=~:d, total=~:d~%"
						     ii increasing-count decreasing-count
						     flat-count bouncy-count total-count))
			(force-output)
			))

		  (set! total-count (* total-count 10))
		  ))
	      ))
	  ))

      (let ((non-bouncy (+ increasing-count decreasing-count flat-count)))
	(let ((total (+ non-bouncy bouncy-count)))
	  (begin
	    (if (equal? debug-flag #t)
		(begin
		  (newline)
		  ))
	    (display (ice9-format:format #f "~:d digit numbers: non-bouncy=~:d, increasing=~:d, decreasing=~:d, flat=~:d, bouncy=~:d, total=~:d  (note: increasing and decreasing counts are without the flat numbers).~%"
					 max-exponent non-bouncy increasing-count
					 decreasing-count flat-count bouncy-count total))
	    (force-output)
	    )))
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
    (display (format #f "Project Euler 113 - Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.~%"))
    (newline)
    (display (format #f "Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.~%"))
    (newline)
    (display (format #f "We shall call a positive integer that is neither increasing nor decreasing a 'bouncy' number; for example, 155349.~%"))
    (newline)
    (display (format #f "As n increases, the proportion of bouncy numbers below n increases such that there are only 12951 numbers below one-million that are not bouncy and only 277032 non-bouncy numbers below 10^10.~%"))
    (newline)
    (display (format #f "How many numbers below a googol (10^100) are not bouncy?~%"))
    (newline)
    (display (format #f "An important clue was found at http://code.google.com/p/tbe-euler/source/browse/113/113.py, which uses the lattice path approach http://mathworld.wolfram.com/LatticePath.html~%"))
    (newline)
    (display (format #f "Instead of counting each possibility individually, it's possible to directly calculate the increasing and decreasing numbers using the binomial coefficient.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-factorial-1 counter)
	   (run-test test-choose-1 counter)
	   (run-test test-count-increasing-numbers-1 counter)
	   (run-test test-count-decreasing-numbers-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (newline)
    (force-output)

    (reproduce-problem-statement)

    (let ((max-exponent 6)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-exponent debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-exponent 10)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-exponent debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-exponent 100)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-exponent debug-flag)
	   ))
	))

    (newline)
    ))
