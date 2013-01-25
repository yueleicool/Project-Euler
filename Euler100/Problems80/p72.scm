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
;;; sieve of eratosthenes, to calculate all prime divisors
;;; of all numbers less than max-num
;;; sprime-htable : key = num, value = (list prime factors)
(define (populate-single-prime-hash! sprime-htable max-num)
  (let ((num-array (make-array 0 (+ max-num 1))))
    (begin
      (hash-clear! sprime-htable)

      (do ((ii 1 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! num-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (let ((this-num (array-ref num-array ii)))
	    (begin
	      (if (= this-num ii)
		  (begin
		    (do ((jj ii (+ jj ii)))
			((> jj max-num))
		      (begin
			(let ((this-list (hash-ref sprime-htable jj (list))))
			  (begin
			    (hash-set! sprime-htable jj (cons ii this-list))
			    ))

			(let ((jnum (array-ref num-array ii)))
			  (begin
			    (array-set! num-array (euclidean/ jnum ii) jj)
			    ))
			))
		    ))
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-single-prime-hash-1)
  (let ((sub-name "test-populate-single-prime-hash-1")
	(max-num 30)
	(prime-htable (make-hash-table 30))
	(shouldbe-list-list
	 (list
	  (list 2 (list 2)) (list 3 (list 3)) (list 4 (list 2))
	  (list 5 (list 5)) (list 6 (list 2 3))
	  (list 7 (list 7)) (list 8 (list 2))
	  (list 9 (list 3)) (list 10 (list 2 5))
	  (list 11 (list 11)) (list 12 (list 2 3))
	  (list 13 (list 13)) (list 14 (list 2 7))
	  (list 15 (list 3 5)) (list 16 (list 2))
	  (list 17 (list 17)) (list 18 (list 2 3))
	  (list 19 (list 19)) (list 20 (list 2 5))
	  (list 21 (list 3 7)) (list 22 (list 2 11))
	  (list 23 (list 23)) (list 24 (list 2 3))
	  (list 25 (list 5)) (list 26 (list 2 13))
	  (list 27 (list 3)) (list 28 (list 2 7))
	  (list 29 (list 29)) (list 30 (list 2 3 5))
	  ))
	(test-label-index 0))
    (begin
      (populate-single-prime-hash! prime-htable max-num)

      (for-each
       (lambda (s-list)
	 (begin
	   (let ((num (list-ref s-list 0))
		 (shouldbe-plist (list-ref s-list 1)))
	     (let ((result-plist (sort
				  (hash-ref prime-htable num (list))
				  <)))
	       (begin
		 (if (not (equal? shouldbe-plist result-plist))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index num
					shouldbe-plist result-plist))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       shouldbe-list-list)
      )))

;;;#############################################################
;;;#############################################################
;;; count the number of relatively prime numbers to n
;;; if n even, all even numbers are not relatively prime
(define (totient-function nn sprime-htable)
  (let ((tproduct nn)
	(ll-max (1+ (exact-integer-sqrt nn)))
	(pmax nn)
	(sprime-list (hash-ref sprime-htable nn #f)))
    (begin
      (if (and (equal? sprime-list #f) (> nn 1))
	  (begin
	    (display (ice9-format:format #f "totient-function error : for number ~:d, no single prime list in sprime-htable.~%" nn))
	    (force-output)
	    (quit)
	    ))

      (cond
       ((or (<= nn 1)
	    (> ll-max pmax))
	(begin
	  (set! tproduct 1)))
       (else
	(set! pmax (srfi-1:fold
		    (lambda (this-num prev)
		      (if (> this-num prev) this-num prev))
		    0 sprime-list))

	(let ((splen (length sprime-list)))
	  (begin
	    (do ((ii 0 (1+ ii)))
		((>= ii splen))
	      (begin
		(set! tproduct
		      (* tproduct
			 (- 1 (/ 1 (list-ref sprime-list ii))
			    )))
		))
	    ))
	tproduct
	))
      )))

;;;#############################################################
;;;#############################################################
(define (test-totient-function-1)
  (let ((sub-name "test-totient-function-1")
	(test-list
	 (list
	  (list 2 1) (list 3 2) (list 4 2)
	  (list 5 4) (list 6 2) (list 7 6)
	  (list 8 4) (list 9 6) (list 10 4)
	  (list 11 10) (list 12 4) (list 13 12)
	  (list 14 6) (list 15 8) (list 16 8)
	  (list 17 16) (list 18 6) (list 19 18)
	  (list 20 8) (list 21 12) (list 22 10)
	  (list 23 22) (list 24 8) (list 25 20)
	  (list 90 24) (list 91 72) (list 92 44)
	  (list 93 60) (list 94 46) (list 95 72)
	  ))
	(max-num 100)
	(sprime-htable (make-hash-table 100))
	(test-label-index 0))
    (begin
      (populate-single-prime-hash! sprime-htable max-num)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (totient-function test-num sprime-htable)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
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
;;; note: method doesn't count 0 or 1
(define (sieve-of-eratosthenes-method max-num)
  (let ((num-array (make-array 0 (+ max-num 1)))
	(result-sum 0))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! num-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (let ((num (array-ref num-array ii)))
	    (begin
	      (if (= num ii)
		  (begin
		    (let ((factor (/ (- ii 1) ii)))
		      (begin
			(do ((jj ii (+ jj ii)))
			    ((> jj max-num))
			  (begin
			    (let ((this-num (array-ref num-array jj)))
			      (let ((next-num (* this-num factor)))
				(begin
				  (array-set! num-array next-num jj)
				  )))
			    ))
			))
		    ))
	      ))

	  (set! result-sum (+ result-sum
			      (array-ref num-array ii)))
	  ))
      result-sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-sieve-of-eratosthenes-method-1)
  (let ((sub-name "test-sieve-of-eratosthenes-method-1")
	(test-list
	 (list
	  (list 2 1) (list 3 3)
	  (list 4 5) (list 5 9)
	  (list 6 11) (list 7 17)
	  (list 8 21)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((max-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (sieve-of-eratosthenes-method max-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : max-num = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index max-num
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
;;; note: method does count 0 and 1
(define (calculate-number-of-elements max-num sprime-htable)
  (let ((elements-count 1))
    (begin
      (do ((mm 1 (1+ mm)))
	  ((> mm max-num))
	(begin
	  (let ((totient (totient-function mm sprime-htable)))
	    (begin
	      (set! elements-count (+ elements-count totient))
	      ))
	  ))
      elements-count
      )))

;;;#############################################################
;;;#############################################################
(define (test-calculate-number-of-elements-1)
  (let ((sub-name "test-calculate-number-of-elements-1")
	(test-list
	 (list
	  (list 2 10 3) (list 3 10 5)
	  (list 4 10 7) (list 5 10 11)
	  (list 6 10 13) (list 7 10 19)
	  (list 8 10 23)
	  ))
	(max-num 20)
	(sprime-htable (make-hash-table 20))
	(test-label-index 0))
    (begin
      (populate-single-prime-hash! sprime-htable max-num)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((max-num (list-ref alist 0))
		 (max-prime (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (calculate-number-of-elements max-num sprime-htable)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : max-num = ~a, max-prime = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index max-num max-prime
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
(define (main-loop max-num max-prime)
  (begin
    (time-code
     (begin
       (let ((count (sieve-of-eratosthenes-method max-num)))
	 (begin
	   (display
	    (ice9-format:format
	     #f "there are ~:d elements in the set of reduced proper fractions for d <= ~:d, (sieve of eratosthenes method)~%"
	     count max-num))
	   (force-output)
	   ))
       ))

    (time-code
     (begin
       (let ((sprime-htable (make-hash-table max-num)))
	 (begin
	   (populate-single-prime-hash! sprime-htable max-num)

	   (let ((count (calculate-number-of-elements max-num sprime-htable)))
	     (let ((without-0-1-count (- count 2)))
	       (begin
		 (display
		  (ice9-format:format
		   #f "there are ~:d elements in the set of reduced proper fractions for d <= ~:d, (eulers totient method)~%"
		   without-0-1-count max-num))
		 (force-output)
		 ))
	     ))
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
(define (main args)
  (begin
    (display (format #f "Problem 072 - Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.~%"))
    (newline)
    (display (format #f "If we list the set of reduced proper fractions for d <= 8 in ascending order of size, we get:~%"))
    (newline)
    (display (format #f "  1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8~%"))
    (newline)
    (display (format #f "It can be seen that there are 21 elements in this set.~%"))
    (newline)
    (display (format #f "How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?~%"))
    (newline)
    (force-output)
    (newline)
    (display (format #f "Several solutions can be found at http://www.mathblog.dk/project-euler-72-reduced-proper-fractions/~%"))
    (newline)
    (display (format #f "This program uses the sieve of Eratosthenes method (described by Kristan Edlund), and a straight-forward totient method for counting the number of elements in a Farey sequence.  See the wikipedia for a description of the Farey sequence and it's generation http://en.wikipedia.org/wiki/Farey_sequence~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-populate-single-prime-hash-1 counter)
	   (run-test test-totient-function-1 counter)
	   (run-test test-sieve-of-eratosthenes-method-1 counter)
	   (run-test test-calculate-number-of-elements-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 8)
	  (max-prime 100))
      (begin
	(main-loop max-num max-prime)
	))

    (newline)
    (force-output)

    (let ((max-num 1000000)
	  (max-prime 1000000))
      (begin
        (main-loop max-num max-prime)
	))

    (newline)
    ))
