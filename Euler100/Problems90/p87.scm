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
	 (let ((test-num (list-ref this-list 0))
	       (test-shouldbe (list-ref this-list 1)))
	   (let ((result (prime? test-num)))
	     (begin
	       (if (not (equal? test-shouldbe result))
		   (begin
		     (display (format #f "~a : error (~a) : num=~a, prime? shouldbe=~a, result=~a~%"
				      sub-name test-label-index test-num test-shouldbe result))
		     (quit)
		     ))
	       (set! test-label-index (1+ test-label-index))
	       ))))
       test-list))))

;;;#############################################################
;;;#############################################################
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (make-prime-list max-num)
  (let ((intermediate-array (make-array 0 (1+ max-num)))
	(result-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! intermediate-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (do ((jj ii (+ jj ii)))
	      ((> jj max-num))
	    (begin
	      (let ((this-num (array-ref intermediate-array jj)))
		(begin
		  (if (= this-num ii)
		      (begin
			(set! result-list (cons ii result-list)))
		      (begin
			(array-set! intermediate-array -1 jj)
			))
		  ))
	      ))
	  ))
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-list-1)
  (let ((sub-name "test-make-prime-list-1")
	(test-list
	 (list
	  (list 2 (list 2)) (list 3 (list 2 3)) (list 4 (list 2 3))
	  (list 5 (list 2 3 5)) (list 6 (list 2 3 5))
	  (list 7 (list 2 3 5 7)) (list 8 (list 2 3 5 7))
	  (list 9 (list 2 3 5 7)) (list 10 (list 2 3 5 7))
	  (list 11 (list 2 3 5 7 11)) (list 13 (list 2 3 5 7 11 13))
	  (list 17 (list 2 3 5 7 11 13 17))
	  (list 19 (list 2 3 5 7 11 13 17 19))
	  (list 23 (list 2 3 5 7 11 13 17 19 23))
	  (list 31 (list 2 3 5 7 11 13 17 19 23 29 31))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (make-prime-list test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
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
;;; define a macro to simplify code
(define-syntax process-squares-level-2
  (syntax-rules ()
    ((process-squares-level-2 parray plen target-number
			      p3 p4 prime-3 prime-4
			      counter result-htable debug-flag)
     (begin
       (let ((continue-loop-2-flag #t))
	 (begin
	   (do ((ii-2 0 (1+ ii-2)))
	       ((or (>= ii-2 plen)
		    (equal? continue-loop-2-flag #f)))
	     (begin
	       (let ((prime-2 (array-ref parray ii-2)))
		 (let ((p2 (* prime-2 prime-2)))
		   (let ((this-num (+ p2 p3 p4)))
		     (begin
		       (if (< this-num target-number)
			   (begin
			     (let ((rflag (hash-ref result-htable this-num #f)))
			       (begin
				 (if (equal? rflag #f)
				     (begin
				       (set! counter (1+ counter))
				       (hash-set! result-htable this-num 1)
				       (if (equal? debug-flag #t)
					   (begin
					     (display (ice9-format:format #f "  ~:d = ~:d^2 + ~:d^3 + ~:d^4~%"
									  this-num prime-2 prime-3 prime-4))
					     (force-output)
					     ))
				       ))
				 )))
			   (begin
			     (set! continue-loop-2-flag #f)
			     ))
		       ))
		   ))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify code
(define-syntax process-cubes-level-3
  (syntax-rules ()
    ((process-cubes-level-3 parray plen target-number
			    p4 prime-4 counter result-htable debug-flag)
     (begin
       (let ((continue-loop-3-flag #t))
	 (begin
	   (do ((ii-3 0 (1+ ii-3)))
	       ((or (>= ii-3 plen)
		    (equal? continue-loop-3-flag #f)))
	     (begin
	       (let ((prime-3 (array-ref parray ii-3)))
		 (let ((p3 (* prime-3 prime-3 prime-3)))
		   (begin
		     (if (> (+ p3 p4) target-number)
			 (begin
			   (set! continue-loop-3-flag #f))
			 (begin
			   (process-squares-level-2 parray plen target-number
						    p3 p4 prime-3 prime-4
						    counter result-htable debug-flag)
			   ))
		     )))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify code
(define-syntax process-quartics-level-4
  (syntax-rules ()
    ((process-quartics-level-4 parray plen target-number counter result-htable debug-flag)
     (begin
       (let ((continue-loop-4-flag #t))
	 (begin
	   (do ((ii-4 0 (1+ ii-4)))
	       ((or (>= ii-4 plen)
		    (equal? continue-loop-4-flag #f)))
	     (begin
	       (let ((prime-4 (array-ref parray ii-4)))
		 (let ((p4 (* prime-4 prime-4 prime-4 prime-4)))
		   (begin
		     (if (> p4 target-number)
			 (begin
			   (set! continue-loop-4-flag #f))
			 (begin
			   (process-cubes-level-3 parray plen target-number
						  p4 prime-4 counter result-htable
						  debug-flag)
			   ))
		     )))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-prime target-number debug-flag)
  (let ((prime-list (make-prime-list max-prime))
	(result-htable (make-hash-table 10000))
	(counter 0))
    (let ((plen (length prime-list))
	  (parray (list->array 1 prime-list)))
      (begin
	(process-quartics-level-4 parray plen target-number counter result-htable debug-flag)

	(display (ice9-format:format #f "there are ~:d numbers less than ~:d that are expressible as the sum of a prime square, prime cube, and a prime fourth power.~%" counter target-number))
	(force-output)
	))
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
    (display (format #f "Project Euler 87: - The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact, there are exactly four numbers below fifty that can be expressed in such a way:~%"))
    (newline)
    (display (format #f "  28 = 2^2 + 2^3 + 2^4~%"))
    (display (format #f "  33 = 3^2 + 2^3 + 2^4~%"))
    (display (format #f "  49 = 5^2 + 2^3 + 2^4~%"))
    (display (format #f "  47 = 2^2 + 3^3 + 2^4~%"))
    (newline)
    (display (format #f "How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-make-prime-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-prime 20)
	  (target-number 50)
	  (debug-flag #t))
      (begin
	(main-loop max-prime target-number debug-flag)
	))

    (newline)
    (force-output)

    ;;; note: sqrt(50 million) is about 7,000
    ;;; so only need primes less than 10,000
    (let ((max-prime 10000)
	  (target-number 50000000)
	  (status-num 100)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-prime target-number debug-flag)
	   ))
	))

    (newline)
    ))
