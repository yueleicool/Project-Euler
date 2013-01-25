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

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

;;;### getopt-long used for command-line option arguments processing
(use-modules ((ice-9 getopt-long)
	      :renamer (symbol-prefix-proc 'ice-9-getopt:)))

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
;;; make a list of primes less than n
(define (make-prime-list this-num)
  (let ((plist (list 2)))
    (begin
      (do ((ii 3 (+ ii 2)))
	  ((> ii this-num))
	(begin
	  (if (prime? ii)
	      (begin
		(set! plist (cons ii plist))
		))
	  ))
      (reverse plist)
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
(define (single-prime-factor-divisors-list input-number prime-list)
  (let ((result-list (list)))
    (begin
      (cond
       ((or (<= input-number 1)
	    (not (list? prime-list))
	    (< (length prime-list) 1))
	(list))
       ((not (equal? (member input-number prime-list) #f))
	(list input-number))
       (else
	(let ((result-list (list))
	      (plen (length prime-list))
	      (pmax (car (last-pair prime-list)))
	      (ll-max (/ input-number 2))
	      (this-prime (car prime-list)))
	  (begin
	    (do ((ii 0 (1+ ii)))
		((or (>= ii plen)
		     (> this-prime ll-max)))
	      (begin
		(set! this-prime (list-ref prime-list ii))

		(if (zero? (modulo input-number this-prime))
		    (begin
		      (set! result-list (cons this-prime result-list))
		      ))
		))

	    (if (< pmax ll-max)
		(begin
		  (display (ice9-format:format #f "warning max generated prime = ~:d less than required sqrt(~:d) = ~:d~%" pmax input-number ll-max))
		  (display (format #f "stopping program...~%"))
		  (quit)
		  ))

	    (reverse result-list)
	    ))))
      )))

;;;#############################################################
;;;#############################################################
(define (test-single-prime-factor-divisors-list-1)
  (let ((sub-name "test-single-prime-factor-divisors-list-1")
	(test-list
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
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((plist (make-prime-list test-num)))
	       (let ((result (single-prime-factor-divisors-list test-num plist)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-num
					  shouldbe result))
			 (quit)
			 ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; count the number of relatively prime numbers to n
;;; if n even, all even numbers are not relatively prime
(define (totient-function nn prime-list)
  (let ((tproduct nn)
	(ll-max (/ nn 2))
	(pmax (car (last-pair prime-list)))
	(sprime-list
	 (single-prime-factor-divisors-list nn prime-list)))
    (let ((splen (length sprime-list)))
      (begin
	(cond
	 ((or (<= nn 1)
	      (> ll-max pmax))
	  (begin
	    (set! tproduct -1)))
	 (else
	  (do ((ii 0 (1+ ii)))
	      ((>= ii splen))
	    (begin
	      (set! tproduct
		    (* tproduct
		       (- 1 (/ 1 (list-ref sprime-list ii))
			  )))
	      ))))
	tproduct
	))
    ))

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
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((prime-list (make-prime-list test-num)))
	       (let ((result (totient-function test-num prime-list)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index test-num
					  shouldbe result))
			 (quit)
			 ))
		   ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (totient-list-function nn)
  (let ((result-list (list)))
    (begin
      (do ((ii 1 (+ ii 1)))
	  ((>= ii nn))
	(begin
	  (if (equal? (gcd nn ii) 1)
	      (begin
		(set! result-list (cons ii result-list))
		))
	  ))
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-totient-list-function-1)
  (let ((sub-name "test-totient-list-function-1")
	(test-list
	 (list
	  (list 2 (list 1)) (list 3 (list 1 2))
	  (list 4 (list 1 3)) (list 5 (list 1 2 3 4))
	  (list 6 (list 1 5)) (list 7 (list 1 2 3 4 5 6))
	  (list 8 (list 1 3 5 7)) (list 9 (list 1 2 4 5 7 8))
	  (list 10 (list 1 3 7 9))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (totient-list-function test-num)))
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
(define (range-loop start-num end-num prime-list)
  (let ((max-nn -1)
	(max-totient -1)
	(max-ratio -1))
    (begin
      (do ((ii start-num (1+ ii)))
	  ((>= ii end-num))
	(begin
	  (let ((totient (totient-function ii prime-list)))
	    (let ((totient-ratio (/ ii totient)))
	      (begin
		(if (> totient-ratio max-ratio)
		    (begin
		      (set! max-nn ii)
		      (set! max-totient totient)
		      (set! max-ratio totient-ratio)
		      ))
		)))
	  ))
      (list max-nn max-totient max-ratio)
      )))

;;;#############################################################
;;;#############################################################
(define (exhaustive-method max-prime start-num end-num)
  (let ((prime-list (make-prime-list max-prime)))
    (let ((result-list (range-loop start-num end-num prime-list)))
      (let ((max-nn (list-ref result-list 0))
	    (max-totient (list-ref result-list 1))
	    (max-ratio (list-ref result-list 2)))
	(let ((dmax-ratio (* 0.00010 (truncate
				      (* 10000.0 max-ratio)))))
	  (begin
	    (display (ice9-format:format #f "exhaustive method : n = ~:d, produces a maximum n/phi(n) = ~:d / ~:d = ~a, (for n <= ~:d)~%"
					 max-nn max-nn max-totient
					 dmax-ratio end-num))
	    (force-output)
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-prime max-num)
  (let ((max-nn 2)
	(max-ratio (/ 1 2))
	(prime-list (list 2))
	(loop-continue-flag #t))
    (begin
      (do ((ii 3 (+ ii 2)))
	  ((or (> ii max-prime)
	       (equal? loop-continue-flag #f)))
	(begin
	  (if (prime? ii)
	      (begin
		(let ((next-nn (* max-nn ii)))
		  (begin
		    (if (< next-nn max-num)
			(begin
			  (set! max-nn next-nn)
			  (set! max-ratio (* max-ratio
					     (- 1 (/ 1 ii))))
			  (set! prime-list (cons ii prime-list)))
			(begin
			  (set! loop-continue-flag #f)
			  ))
		    ))
		))
	  ))

      (let ((max-totient (* max-nn max-ratio))
	    (div-string (string-join
			 (map (lambda (num)
				(ice9-format:format #f "~:d" num))
			      (reverse prime-list))
			 ", "))
	    (dmax-ratio (* 0.00010 (truncate
				    (/ 10000.0 max-ratio)))))
	(begin
	  (display (ice9-format:format #f "n = ~:d, produces a maximum n/phi(n) = ~:d / ~:d = ~a, prime factors of n = ~a : (for n <= ~:d)~%"
				       max-nn max-nn max-totient
				       dmax-ratio div-string max-num))
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
    (display (format #f "Problem 069 - Euler's Totient function, phi(n) [sometimes called the phi function], is used to determine the number of numbers less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, phi(9)=6.~%"))
    (newline)
    (display (format #f "n	Relatively Prime	phi(n)	n/phi(n)~%"))
    (display (format #f "2	1	1	2~%"))
    (display (format #f "3	1,2	2	1.5~%"))
    (display (format #f "4	1,3	2	2~%"))
    (display (format #f "5	1,2,3,4	4	1.25~%"))
    (display (format #f "6	1,5	2	3~%"))
    (display (format #f "7	1,2,3,4,5,6	6	1.1666...~%"))
    (display (format #f "8	1,3,5,7	4	2~%"))
    (display (format #f "9	1,2,4,5,7,8	6	1.5~%"))
    (display (format #f "10	1,3,7,9	4	2.5~%"))
    (newline)
    (display (format #f "It can be seen that n=6 produces a maximum n/phi(n) for n <= 10.~%"))
    (newline)
    (display (format #f "Find the value of n <= 1,000,000 for which n/phi(n) is a maximum.~%"))
    (newline)
    (display (format #f "From the definition of Euler's totient function http://en.wikipedia.org/wiki/Totient_function~%"))
    (newline)
    (display (format #f "phi(n) = n x (1 - 1/p1) x (1 - 1/p2) x ... x (1 - 1/pk)~%"))
    (display (format #f "where the prime factors of n are p1, p2, ..., pk.~%"))
    (newline)
    (display (format #f "The solution was given at http://www.mathblog.dk/project-euler-69-find-the-value-of-n-%E2%89%A4-1000000-for-which-n%CF%86n-is-a-maximum/~%"))
    (display (format #f "and notes that to maximize the ratio n / phi(n), you need a number that is as close as possible to 1 million, but as small a phi(n) as possible.  This happens when n is made up of a product of primes that is closest to 1 million.~%"))
    (newline)
    (display (format #f "The problem was solved this way in this program, and a c++ program was used to exhaustively examine all numbers less than 1 million, (which took around 2 days to complete).  Both methods gave the same solution.~%"))
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
	   (run-test test-single-prime-factor-divisors-list-1 counter)
	   (run-test test-totient-function-1 counter)
	   (run-test test-totient-list-function-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-prime 1000)
	  (start-num 1)
	  (end-num 100))
      (begin
	(time-code
	 (begin
	   (exhaustive-method max-prime start-num end-num)
	   (newline)
	   (main-loop max-prime end-num)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-prime 10000)
	  (end-num 1000000))
      (begin
	(time-code
	 (begin
	   (main-loop max-prime end-num)
	   ))
	))

    (newline)
    ))
