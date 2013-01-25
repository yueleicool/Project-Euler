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
(define (prime? nn)
  (define (smallest-divisor nn test-divisor max-divisor)
    (cond
     ((> test-divisor max-divisor) nn)
     ((zero? (modulo nn test-divisor)) test-divisor)
     (else
      (smallest-divisor nn (+ test-divisor 2) max-divisor)
      )))
  (begin
    (cond
     ((<= nn 1) #f)
     ((= nn 2) #t)
     ((zero? (modulo nn 2)) #f)
     (else
      (let ((max-divisor
	     (+ (exact-integer-sqrt nn) 1)))
	(= nn (smallest-divisor nn 3 max-divisor)))
      ))))

;;;#############################################################
;;;#############################################################
(define (test-prime-1)
  (let ((sub-name "test-prime-1")
	(test-list
	 (list
	  (list 0 #f) (list 1 #f) (list 2 #t) (list 3 #t)
	  (list 4 #f) (list 5 #t) (list 6 #f) (list 7 #t)
	  (list 8 #f) (list 9 #f) (list 10 #f) (list 11 #t)
	  (list 12 #f) (list 13 #t) (list 14 #f) (list 15 #f)
	  (list 16 #f) (list 17 #t) (list 18 #f) (list 19 #t)
	  (list 20 #f) (list 21 #f) (list 22 #f) (list 23 #t)
	  (list 24 #f) (list 25 #f) (list 26 #f) (list 27 #f)
	  (list 28 #f) (list 29 #t) (list 30 #f) (list 31 #t)
	  (list 32 #f) (list 33 #f) (list 34 #f) (list 35 #f)
	  (list 36 #f) (list 37 #t) (list 38 #f) (list 39 #f)
	  (list 40 #f) (list 41 #t) (list 42 #f) (list 43 #t)
	  (list 44 #f) (list 45 #f) (list 46 #f) (list 47 #t)
	  (list 48 #f) (list 49 #f) (list 50 #f) (list 51 #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (prime? test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, prime? shouldbe=~a, result=~a~%"
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
(define (to-prime-factor-list input-number)
  (define (local-divide-all-factors this-num this-factor)
    (let ((ll-num this-num)
	  (ncount 0))
      (while
       (or (zero? (modulo ll-num this-factor)) (< ll-num 1))
       (begin
	 (set! ll-num (euclidean-quotient ll-num this-factor))
	 (set! ncount (+ ncount 1))
	 ))
      (list ll-num this-factor ncount)
      ))
  (begin
    (cond
     ((<= input-number 1) (list))
     (else
      (let ((result-list (list))
	    (constant-number input-number)
	    (ll-num input-number)
	    (ll-max (+ 1 (exact-integer-sqrt input-number))))
	(begin
	  (do ((ii 2 (+ ii 1)))
	      ((or (>= ii ll-max) (<= ll-num 1)))
	    (begin
	      (if (zero? (modulo constant-number ii))
		  (begin
		    (if (prime? ii)
			(let ((partial-list (local-divide-all-factors ll-num ii)))
			  (begin
			    (set! ll-num (car partial-list))
			    (set! result-list (append result-list (list (cdr partial-list))))
			    )))

		    (let ((this-divisor (quotient constant-number ii)))
		      (if (and (> this-divisor ii)
			       (equal? (memq this-divisor result-list) #f)
			       (prime? this-divisor))
			  (let ((partial-list (local-divide-all-factors ll-num this-divisor)))
			    (begin
			      (set! ll-num (car partial-list))
			      (set! result-list (append result-list (list (cdr partial-list))))
			      ))))
		    ))
	      ))

	  (if (< (length result-list) 1)
	      (list (list input-number 1))
	      result-list
	      )))))))

;;;#############################################################
;;;#############################################################
(define (test-to-prime-factor-list-1)
  (let ((sub-name "test-to-prime-factor-list-1")
	(test-list
	 (list
	  (list 2 (list (list 2 1))) (list 3 (list (list 3 1)))
	  (list 4 (list (list 2 2))) (list 5 (list (list 5 1)))
	  (list 6 (list (list 2 1) (list 3 1))) (list 7 (list (list 7 1)))
	  (list 8 (list (list 2 3))) (list 9 (list (list 3 2)))
	  (list 10 (list (list 2 1) (list 5 1)))
	  (list 11 (list (list 11 1)))
	  (list 12 (list (list 2 2) (list 3 1)))
	  (list 13 (list (list 13 1)))
	  (list 14 (list (list 2 1) (list 7 1)))
	  (list 15 (list (list 3 1) (list 5 1)))
	  (list 16 (list (list 2 4)))
	  (list 17 (list (list 17 1)))
	  (list 18 (list (list 2 1) (list 3 2)))
	  (list 19 (list (list 19 1)))
	  (list 20 (list (list 2 2) (list 5 1)))
	  (list 21 (list (list 3 1) (list 7 1)))
	  (list 22 (list (list 2 1) (list 11 1)))
	  (list 100 (list (list 2 2) (list 5 2)))
	  (list 200 (list (list 2 3) (list 5 2)))
	  (list 400 (list (list 2 4) (list 5 2)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (to-prime-factor-list test-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : test num = ~a, shouldbe-list = ~a, result-list = ~a~%"
					sub-name test-label-index test-num
					shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (integer-power this-number this-exponent)
  (cond
   ((= this-exponent 0) 1)
   ((= this-exponent 1) this-number)
   ((< this-exponent 0) -1)
   (else
    (let ((result-num this-number)
	  (max-iter (- this-exponent 1)))
      (begin
	(do ((ii 0 (+ ii 1)))
	    ((>= ii max-iter))
	  (begin
	    (set! result-num (* result-num this-number))
	    ))
	result-num
	)))))

;;;#############################################################
;;;#############################################################
(define (test-integer-power-1)
  (let ((sub-name "test-integer-power-1")
	(test-list
	 (list
	  (list 10 0 1) (list 11 0 1) (list 12 0 1)
	  (list 10 1 10) (list 11 1 11) (list 12 1 12)
	  (list 10 2 100) (list 11 2 121) (list 12 2 144)
	  (list 2 2 4) (list 2 3 8) (list 2 4 16) (list 2 5 32)
	  (list 2 6 64) (list 2 7 128) (list 2 8 256) (list 2 9 512)
	  (list 2 10 1024)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (test-exp (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (let ((result-num (integer-power test-num test-exp)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, exponent = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num test-exp
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
(define (smallest-divisible max-num)
  (let ((pfactor-htable (make-hash-table 100)))
    (begin
      (do ((ii 2 (+ ii 1)))
	  ((> ii max-num))
	(begin
	  (let ((plist (to-prime-factor-list ii)))
	    (for-each
	     (lambda (alist)
	       (let ((pfactor (list-ref alist 0))
		     (pexp (list-ref alist 1)))
		 (let ((current-exp (hash-ref pfactor-htable pfactor 0)))
		   (begin
		     (if (> pexp current-exp)
			 (hash-set! pfactor-htable pfactor pexp))
		     ))))
	     plist))))

      (let ((result 1))
	(begin
	  (hash-for-each
	   (lambda (key value)
	     (let ((this-prod (integer-power key value)))
	       (set! result (* result this-prod))))
	   pfactor-htable)
	  result
	  )))))

;;;#############################################################
;;;#############################################################
(define (test-smallest-divisible-1)
  (let ((sub-name "test-smallest-divisible-1")
	(test-list
	 (list
	  (list 10 2520) (list 11 27720) (list 12 27720)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (smallest-divisible test-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (main-loop max-num)
  (let ((small (smallest-divisible max-num)))
    (begin
      (display (ice9-format:format #f "smallest number that's divisble evenly, up to ~:d is ~:d~%"
		       max-num small))
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
    (display (format #f "Project Euler 5: 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.~%"))
    (display (format #f "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?~%"))

    (newline)
    (display (format #f "notes: this program can be simply written by breaking down each number from 1 through 20 into it's prime constituents.~%"))
    (display (format #f "then one looks at the largest number of 2's that occur between 1 and 20 and stores that result.~%"))
    (display (format #f "repeat for the largest number of 3's, 5's, 7's, 11's, 13's, 17's, 19's.~%"))
    (display (format #f "the solution is the product of the largest number of primes needed between 1 and 20.~%"))

    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-to-prime-factor-list-1 counter)
	   (run-test test-integer-power-1 counter)
	   (run-test test-smallest-divisible-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 10))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-num 20))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))
    ))
