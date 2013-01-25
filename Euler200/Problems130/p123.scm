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
(define (julian-day-difference-to-string dend dstart)
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
(define (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (make-prime-array max-num)
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
          (let ((ii-num (array-ref intermediate-array ii)))
            (begin
              (if (= ii-num ii)
                  (begin
                    (set! result-list (cons ii result-list))

                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj max-num))
                      (begin
                        (let ((jj-num (array-ref intermediate-array jj)))
                          (begin
                            (array-set! intermediate-array -1 jj)
                            ))
                        ))
                    ))
	      ))
	  ))
      (list->array 1 (reverse result-list))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-array-1)
  (let ((sub-name "test-make-prime-array-1")
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
	  (list 40 (list 2 3 5 7 11 13 17 19 23 29 31 37))
	  (list 50 (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-array (make-prime-array test-num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (car (array-dimensions result-array))))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display
                          (format #f "~a : error (~a) : num=~a, shouldbe=~a, "
                                  sub-name test-label-index test-num
                                  shouldbe-list))
			 (display
                          (format #f "lengths not equal, shouldbe=~a, result=~a~%"
                                  slen rlen))
			 (set! ok-flag #f)
			 ))

		   (do ((ii 0 (1+ ii)))
		       ((>= ii slen))
		     (begin
		       (let ((s-elem (list-ref shouldbe-list ii))
			     (r-elem (array-ref result-array ii)))
			 (begin
			   (if (not (equal? s-elem r-elem))
			       (begin
				 (display
                                  (format #f "~a : error (~a) : num=~a, "
                                          sub-name test-label-index test-num))
				 (display
                                  (format #f "shouldbe=~a, result=~a, discrepancy at "
                                          shouldbe result))
				 (display
                                  (format #f "ii=~a, shouldbe=~a, result=~a~%"
                                          ii s-elem r-elem))
				 (set! ok-flag #f)
				 ))
			   ))
		       ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
;;; only need to consider odd nn, then (pn-1)^n+(pn+1)^n = 2npn mod pn^2
(define (main-loop target-remainder max-prime)
  (let ((previous-nn -1)
	(previous-remainder -1)
	(min-nn -1)
	(min-remainder -1)
	(continue-loop-flag #t)
	(prime-array (make-prime-array max-prime)))
    (let ((array-length (car (array-dimensions prime-array))))
      (begin
	(do ((nn 1 (+ nn 2)))
	    ((or (>= nn array-length)
		 (equal? continue-loop-flag #f)))
	  (begin
	    (set! previous-nn min-nn)
	    (set! previous-remainder min-remainder)

            (let ((pindex (1- nn)))
              (let ((pn (array-ref prime-array pindex)))
                (let ((this-remainder (* 2 nn pn)))
                  (begin
                    (set! min-nn nn)
                    (set! min-remainder this-remainder)

                    (if (> this-remainder target-remainder)
                        (begin
                          (set! continue-loop-flag #f)
                          ))
                    ))
                ))
            ))

	(display
         (ice9-format:format
          #f "~:d = the least value of n for which the remainder "
          min-nn))
	(display
         (ice9-format:format
          #f "first exceeds ~:d, with a remainder of ~:d.~%"
          target-remainder min-remainder))
	(display
         (ice9-format:format
          #f "the previous value of n is ~:d (with a remainder of ~:d).~%"
          previous-nn previous-remainder))
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
    (display (format #f "Project Euler 123 - Let pn be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the remainder when (pn-1)^n + (pn+1)^n is divided by pn^2.~%"))
    (newline)
    (display (format #f "For example, when n = 3, p3 = 5, and 43 + 63 = 280 == 5 mod 25.~%"))
    (newline)
    (display (format #f "The least value of n for which the remainder first exceeds 10^9 is 7037.~%"))
    (newline)
    (display (format #f "Find the least value of n for which the remainder first exceeds 10^10.~%"))
    (newline)
    (display (format #f "This algorithm is similar to problem 120, and uses the ideas found in http://www.lousycoder.com/index.php?/archives/151-Project-Euler-Problem-120.html~%"))
    (newline)
    (display (format #f "Using the binomial theorem, http://en.wikipedia.org/wiki/Binomial_theorem one can rewrite (a-1)^n + (a+1)^n=Sum(Choose(n,k) * a^k * ((-1)^(n-k) + 1)).~%"))
    (newline)
    (display (format #f "From the article above there are three cases to examine: n=1, n even, n odd.  When n=1, (a-1)+(a+1)=2a.  When n is even, then (n-k) is odd when k is odd, so only the even powers a^k remain (-1+1 = 0), then a^2t = 0 mod a^2.  (a-1)^n + (a+1)^n = 2 mod a^2.  When n is odd, then (n-k) is even when k is odd, so only the odd powers of a^k remain.  Every term in the binomial expansion contains a power of a^2 (each a^k is equal to a^2 times a constant), except for the k=1 term, so (a-1)^n + (a+1)^n = 2na mod a^2.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-prime-array-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((target-remainder 1000000000)
	  (max-prime 500000))
      (begin
	(time-code
	 (begin
	   (main-loop target-remainder max-prime)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-remainder 10000000000)
	  (max-prime 1000000))
      (begin
	(time-code
	 (begin
	   (main-loop target-remainder max-prime)
	   ))
	))

    (newline)
    ))
