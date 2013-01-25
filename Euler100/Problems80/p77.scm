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
;;; make a list of primes less than or equal to n, using a sieve method
(define (make-prime-list this-num)
  (let ((intermediate-array (make-array 0 (1+ this-num)))
	(result-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii this-num))
	(begin
	  (array-set! intermediate-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii this-num))
	(begin
	  (do ((jj ii (+ jj ii)))
	      ((> jj this-num))
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
(define (array-to-string this-array)
  (let ((stmp "")
	(amax (car (array-dimensions this-array))))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii amax))
	(begin
	  (let ((this-num (array-ref this-array ii)))
	    (begin
	      (if (= ii 0)
		  (begin
		    (let ((this-str (ice9-format:format #f "~:d" this-num)))
		      (begin
			(set! stmp (string-append stmp this-str))
			)))
		  (begin
		    (let ((this-str (ice9-format:format #f ", ~:d" this-num)))
		      (begin
			(set! stmp (string-append stmp this-str))
			))
		    ))
	      ))
	  ))
      (string-append "[ " stmp " ]")
      )))

;;;#############################################################
;;;#############################################################
;;; 1-dimensional dynamic array method
(define (compute-sum-count max-num debug-flag)
  (let ((prime-list (make-prime-list max-num))
	(count-array (make-array 0 (1+ max-num))))
    (let ((prime-array (list->array 1 prime-list))
	  (psize (length prime-list)))
      (begin
        ;;; initialize
	(array-set! count-array 1 0)

	(if (equal? debug-flag #t)
	    (begin
              ;;; print header
	      (do ((ii 0 (1+ ii)))
		  ((> ii max-num))
		(begin
		  (if (= ii 0)
		      (begin
			(display (format #f "        ~:d" ii)))
		      (begin
			(display (format #f ", ~:d" ii))
			))
		  ))
	      (newline)
	      (force-output)
	      ))

        ;;; main loop
	(do ((ii 0 (1+ ii)))
	    ((>= ii psize))
	  (begin
	    (let ((ii-prime (array-ref prime-array ii)))
	      (begin
		(do ((jj ii-prime (1+ jj)))
		    ((> jj max-num))
		  (begin
                    ;;; then find previous count that is equal
		    (let ((this-count (array-ref count-array jj)))
		      (let ((jtmp (- jj ii-prime)))
			(let ((jj-count (array-ref count-array jtmp)))
			  (begin
			    (array-set! count-array (+ this-count jj-count) jj)
			    ))
			))
		    ))
		))

	    (if (equal? debug-flag #t)
		(begin
		  (display (ice9-format:format #f "(~:d) : ~a~%"
					       ii (array-to-string count-array)))
		  (force-output)
		  ))
	    ))

	(array-ref count-array max-num)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-compute-sum-count-1)
  (let ((sub-name "test-compute-sum-count-1")
	(test-list
	 (list
	  (list 2 1)
	  (list 4 1)
	  (list 5 2)
	  (list 6 2)
	  (list 7 3)
	  (list 8 3)
	  (list 10 5)
	  ))
	(debug-flag #f)
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (compute-sum-count num debug-flag)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop min-ways debug-flag)
  (let ((loop-continue-flag #t)
	(ii-max 2)
	(ii-result 0))
    (begin
      (while
       (equal? loop-continue-flag #t)
       (begin
	 (let ((rlen (compute-sum-count ii-max debug-flag)))
	   (begin
	     (if (> rlen min-ways)
		 (begin
		   (set! loop-continue-flag #f)
		   (set! ii-result ii-max)
		   (display
		    (ice9-format:format
		     #f "~:d is the smallest number that can be written as the sum of primes in ~:d different ways (greater than ~:d)~%"
		     ii-max rlen min-ways))
		   (force-output)
		   ))
	     ))
	 (set! ii-max (1+ ii-max))
	 ))
      ii-result
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
    (display (format #f "Problem 077 - It is possible to write ten as the sum of primes in exactly five different ways:~%"))
    (newline)
    (display (format #f "  7 + 3~%"))
    (display (format #f "  5 + 5~%"))
    (display (format #f "  5 + 3 + 2~%"))
    (display (format #f "  3 + 3 + 2 + 2~%"))
    (display (format #f "  2 + 2 + 2 + 2 + 2~%"))
    (newline)
    (display (format #f "What is the first value which can be written as the sum of primes in over five thousand different ways?~%"))
    (newline)
    (display (format #f "The solution is described at http://www.mathblog.dk/project-euler-77-sum-of-primes-five-thousand-ways/~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-prime-list-1 counter)
	   (run-test test-compute-sum-count-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((debug-flag #f))
      (begin
	(do ((ii 2 (+ ii 1)))
	    ((> ii 6))
	  (begin
	    (main-loop ii debug-flag)
	    ))
	))

    (newline)
    (force-output)

    (let ((max-num 5000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
