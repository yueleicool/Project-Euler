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
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (make-prime-list max-num)
  (let ((result-list (list 2)))
    (begin
      (do ((ii 3 (+ ii 2)))
	  ((> ii max-num))
	(begin
	  (if (equal? (prime? ii) #t)
	      (begin
		(set! result-list (cons ii result-list))
		))))

      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-list-1)
  (let ((sub-name "test-make-prime-list-1")
	(test-list
	 (list
	  (list 5 (list 2 3 5))
	  (list 10 (list 2 3 5 7))
	  (list 20 (list 2 3 5 7 11 13 17 19))
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
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (populate-htable! sq-htable max-num)
  (do ((ii 1 (+ ii 1)))
      ((> ii max-num))
    (begin
      (let ((tnum (* 2 ii ii)))
	(hash-set! sq-htable tnum ii)
	))))

;;;#############################################################
;;;#############################################################
(define (sum-and-square-list this-num prime-list sq-htable)
  (let ((break-flag #f)
	(result-list #f)
	(dlen (length prime-list)))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((or (>= ii dlen)
	       (equal? break-flag #t)))
	(begin
	  (let ((this-prime (list-ref prime-list ii)))
	    (let ((this-diff (- this-num this-prime)))
	      (begin
		(if (< this-diff 0)
		    (begin
		      (set! break-flag #t))
		    (begin
		      (let ((d-flag (hash-ref sq-htable this-diff #f)))
			(if (not (equal? d-flag #f))
			    (begin
			      (set! break-flag #t)
			      (set! result-list (list this-num this-prime d-flag))
			      )))
		      )))))
	  ))
      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-sum-and-square-list-1)
  (let ((sub-name "test-sum-and-square-list-1")
	(test-list
	 (list
	  (list 9 100 (list 9 7 1))
	  (list 15 100 (list 15 7 2))
	  (list 21 100 (list 21 3 3))
	  (list 25 100 (list 25 7 3))
	  (list 27 100 (list 27 19 2))
	  (list 33 100 (list 33 31 1))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (test-max (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2))
		 (sq-htable (make-hash-table 100)))
	     (let ((prime-list (make-prime-list test-max)))
	       (begin
		 (populate-htable! sq-htable test-max)

		 (let ((result (sum-and-square-list test-num prime-list sq-htable)))
		   (begin
		     (if (not (equal? shouldbe result))
			 (begin
			   (display (format #f "~a : error (~a) : num=~a, max=~a, shouldbe=~a, result=~a~%"
					    sub-name test-label-index test-num test-max
					    shouldbe result))
			   (quit)
			   ))
		     ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((smallest-odd-composite max-num)
	(sq-htable (make-hash-table max-num))
	(prime-list (make-prime-list max-num))
	(break-flag #f))
    (begin
      (populate-htable! sq-htable max-num)

      (do ((nn 9 (+ nn 2)))
	  ((or (> nn max-num)
	       (equal? break-flag #t)))
	(begin
	  (if (not (prime? nn))
	      (begin
		(let ((result-list (sum-and-square-list nn prime-list sq-htable)))
		  (if (equal? result-list #f)
		      (if (< nn smallest-odd-composite)
			    (begin
			      (set! break-flag #t)
			      (set! smallest-odd-composite nn)
			      )))))
	      )))

      (if (equal? smallest-odd-composite max-num)
	  (begin
	    (display (ice9-format:format #f "no smallest composite found (for numbers less than ~:d)~%"
					 max-num)))
	  (begin
	    (display (ice9-format:format #f "smallest composite found = ~:d (for numbers less than ~:d)~%"
					 smallest-odd-composite max-num))
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
    (display (format #f "Problem 046 - It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.~%"))
    (newline)
    (display (format #f "9 = 7 + 2x1^2~%"))
    (display (format #f "15 = 7 + 2x2^2~%"))
    (display (format #f "21 = 3 + 2x3^2~%"))
    (display (format #f "25 = 7 + 2x3^2~%"))
    (display (format #f "27 = 19 + 2x2^2~%"))
    (display (format #f "33 = 31 + 2x1^2~%"))
    (newline)
    (display (format #f "It turns out that the conjecture was false.~%"))
    (newline)
    (display (format #f "What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-make-prime-list-1 counter)
	   (run-test test-sum-and-square-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 1000)
	  (debug-flag #t))
      (begin
	(main-loop max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 100000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
