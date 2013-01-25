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
	(begin
	  (= nn (smallest-divisor nn 3 max-divisor))
	  ))
      ))
    ))

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
	  (list 52 #f) (list 53 #t) (list 54 #f) (list 55 #f)
	  (list 56 #f) (list 57 #f) (list 58 #f) (list 59 #t)
	  (list 60 #f) (list 61 #t) (list 62 #f) (list 63 #f)
	  (list 64 #f) (list 65 #f) (list 66 #f) (list 67 #t)
	  (list 68 #f) (list 69 #f) (list 70 #f) (list 71 #t)
	  (list 72 #f) (list 73 #t) (list 74 #f) (list 75 #f)
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
;;; sieve of eratosthenes method
(define (populate-prime-hash! primes-htable max-num)
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
			(hash-set! primes-htable ii #t))
		      (begin
			(array-set! intermediate-array -1 jj)
			))
		  ))
	      ))

	  (let ((this-num (array-ref intermediate-array ii)))
	    (begin
	      (if (not (equal? this-num ii))
		  (begin
		    (hash-set! primes-htable ii #f)
		    ))
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-prime-hash-1)
  (let ((sub-name "test-populate-prime-hash-1")
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
	(primes-htable (make-hash-table))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (hash-clear! primes-htable)
	   (let ((max-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (begin
	       (populate-prime-hash! primes-htable max-num)

	       (do ((ii 2 (1+ ii)))
		   ((> ii max-num))
		 (begin
		   (let ((rflag (hash-ref primes-htable ii #f))
			 (sflag #t))
		     (begin
		       (if (equal? (member ii shouldbe) #f)
			   (begin
			     (set! sflag #f)
			     ))

		       (if (not (equal? sflag rflag))
			   (begin
			     (display (format #f "~a : error (~a) : max-num=~a, prime-list=~a, for ii=~a, shouldbe=~a, result=~a~%"
					      sub-name test-label-index max-num
					      shouldbe ii
					      sflag rflag))
			     (quit)
			     ))
		       ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (find-generated-primes aa bb primes-htable)
  (let ((rlist (list))
	(nn 0)
	(break-flag #f))
    (begin
      (while
       (equal? break-flag #f)
       (let ((this-value (+ (* nn nn) (* aa nn) bb)))
	 (let ((pflag (hash-ref primes-htable this-value -1)))
	   (begin
	     (cond
	      ((equal? pflag #t)
	       (begin
		 (set! rlist (cons this-value rlist))
		 (set! nn (1+ nn))
		 ))
	      ((equal? pflag #f)
	       (begin
		 (set! break-flag #t)
		 ))
	      (else
	       (let ((ppflag (prime? this-value)))
		 (begin
		   (if (equal? ppflag #t)
		       (begin
			 (set! rlist (cons this-value rlist))
			 (set! nn (1+ nn))
			 (hash-set! primes-htable this-value #t))
		       (begin
			 (set! break-flag #t)
			 ))
		   ))
	       ))
	     ))
	 ))
      (reverse rlist)
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-generated-primes-1)
  (let ((sub-name "test-find-generated-primes-1")
	(test-list
	 (list
	  (list 1 5 (list 5 7 11 17))
	  (list 1 11 (list 11 13 17 23 31 41 53 67 83 101))
	  ))
	(primes-htable (make-hash-table))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-aa (list-ref this-list 0))
		 (test-bb (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (find-generated-primes test-aa test-bb
						  primes-htable)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : a=~a, b=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-aa test-bb
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
(define-syntax sub-find-maxs
  (syntax-rules ()
    ((sub-find-maxs aa bb primes-htable
                    max-list max-aa max-bb max-len)
     (begin
       (let ((maa (* -1 aa))
             (mbb (* -1 bb)))
         (let ((rlist1
                (find-generated-primes aa bb primes-htable))
               (rlist2
                (find-generated-primes maa bb primes-htable))
               (rlist3
                (find-generated-primes aa mbb primes-htable))
               (rlist4
                (find-generated-primes maa mbb primes-htable)))
           (let ((rlen1 (length rlist1))
                 (rlen2 (length rlist2))
                 (rlen3 (length rlist3))
                 (rlen4 (length rlist4)))
             (begin
               (if (> rlen1 max-len)
                   (begin
                     (set! max-len rlen1)
                     (set! max-list rlist1)
                     (set! max-aa aa)
                     (set! max-bb bb)
                     ))
               (if (> rlen2 max-len)
                   (begin
                     (set! max-len rlen2)
                     (set! max-list rlist2)
                     (set! max-aa maa)
                     (set! max-bb bb)
                     ))
               (if (> rlen3 max-len)
                   (begin
                     (set! max-len rlen3)
                     (set! max-list rlist3)
                     (set! max-aa aa)
                     (set! max-bb mbb)
                     ))
               (if (> rlen4 max-len)
                   (begin
                     (set! max-len rlen4)
                     (set! max-list rlist4)
                     (set! max-aa maa)
                     (set! max-bb mbb)
                     ))
               ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop end-aa end-bb largest-prime)
  (let ((primes-htable (make-hash-table 100))
	(max-list (list))
	(max-aa 0)
	(max-bb 0)
	(max-len 0))
    (begin
      (populate-prime-hash! primes-htable largest-prime)

      (do ((aa 1 (+ aa 2)))
          ((> aa end-aa))
        (begin
          (do ((bb 3 (+ bb 2)))
              ((> bb end-bb))
            (begin
              (let ((pflag (hash-ref primes-htable bb)))
                (begin
                  (if (equal? pflag #t)
                      (begin
                        (sub-find-maxs
                         aa bb primes-htable
                         max-list max-aa max-bb max-len)
                        ))
                  ))
              ))
          ))

      (let ((result-len (length max-list)))
        (let ((sgn1 (if (>= max-aa 0) "+" "-"))
              (sgn2 (if (>= max-bb 0) "+" "-"))
              (ll-sgn-1 (if (>= max-aa 0) 1 -1))
              (ll-sgn-2 (if (>= max-bb 0) 1 -1)))
          (begin
            (display
             (ice9-format:format
              #f "the product of the coefficients a and b is equal to ~:d~%"
              (* max-aa max-bb)))
            (display
             (ice9-format:format
              #f "the quadratic formula n^2 ~a ~:dn ~a ~:d produces ~:d primes for consecutive values of n~%"
              sgn1 (* ll-sgn-1 max-aa)
              sgn2 (* ll-sgn-2 max-bb) result-len))
            (display
             (ice9-format:format
              #f "starting with 0, list of primes = { ~a }, (a<=~:d, b<=~:d)~%"
              (string-join
               (map
                (lambda (num)
                  (ice9-format:format #f "~:d" num))
                max-list) ", ")
              end-aa end-bb
              ))
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
    (display (format #f "Problem 027 - Euler published the remarkable quadratic formula:~%"))
    (display (format #f "  n^2 + n + 41~%"))
    (newline)
    (display (format #f "It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40x(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.~%"))
    (newline)
    (display (format #f "Using computers, the incredible formula  n^2 - 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, 79 and 1601, is 126479.~%"))
    (newline)
    (display (format #f "Considering quadratics of the form:~%"))
    (display (format #f "n^2 + an + b, where |a| < 1000 and |b| < 1000~%"))
    (display (format #f "where |n| is the modulus/absolute value of n e.g. |11| = 11 and |4| = 4~%"))
    (newline)
    (display (format #f "Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.~%"))
    (newline)
    (display (format #f "Makes use of the fact that n^2+an+b is a prime when n=0, so~%"))
    (display (format #f "b must be a prime.  Also, when n=1, 1+a+b a prime, and since~%"))
    (display (format #f "b a prime, a must be odd.  When n=2, 4+2a+b excludes b=2.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-populate-prime-hash-1 counter)
	   (run-test test-find-generated-primes-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((end-aa 10)
	  (end-bb 10)
	  (largest-prime 10000))
      (begin
	(main-loop end-aa end-bb largest-prime)
	))

    (newline)
    (force-output)

    (let ((end-aa 100)
	  (end-bb 100)
	  (largest-prime 10000))
      (begin
	(time-code
	 (begin
	   (main-loop end-aa end-bb largest-prime)
	   ))
	))

    (newline)
    (force-output)

    (let ((end-aa 1000)
	  (end-bb 1000)
	  (largest-prime 10000))
      (begin
	(time-code
	 (begin
	   (main-loop end-aa end-bb largest-prime)
	   ))
	))
    (newline)
    ))
