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

;;;### srfi-11 for let-values (multiple value bind)
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
(define (modulo-power base exponent mod-num)
  (let ((cc 1))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii exponent))
	(begin
	  (set! cc (modulo (* cc base) mod-num))
	  ))
      cc
      )))

;;;#############################################################
;;;#############################################################
(define (test-modulo-power-1)
  (let ((sub-name "test-modulo-power-1")
	(test-list
	 (list
	  (list 5 3 13 8)
	  (list 4 13 497 445)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((base (list-ref alist 0))
		 (exponent (list-ref alist 1))
		 (mod-num (list-ref alist 2))
		 (shouldbe-num (list-ref alist 3)))
	     (let ((result-num (modulo-power base exponent mod-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : (~a) : error : base = ~a, exponent = ~a, mod-num = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index base exponent mod-num shouldbe-num result-num))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (extract-last-n-digits large-number n-digits)
  (let ((result-list (list))
	(ok-flag #t)
	(local-number large-number))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((or (>= ii n-digits)
	       (equal? ok-flag #f)))
	(begin
	  (if (< local-number 10)
	      (begin
		(set! result-list (cons local-number result-list))
		(set! local-number 0)
		(set! ok-flag #f))
	      (begin
		(let ((next-local (euclidean/ local-number 10)))
		  (let ((tmp-local (* 10 next-local)))
		    (let ((this-digit (- local-number tmp-local)))
		      (begin
			(if (<= next-local 0)
			    (begin
			      (set! ok-flag #f))
			    (begin
			      (set! result-list (cons this-digit result-list))
			      (set! local-number next-local)
			      ))
			))
		    ))
		))
	  ))
      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-extract-last-n-digits-1)
  (let ((sub-name "test-extract-last-n-digits-1")
	(test-list
	 (list
	  (list 5 1 (list 5)) (list 5 2 (list 5))
	  (list 12 1 (list 2)) (list 12 2 (list 1 2)) (list 12 3 (list 1 2))
	  (list 123 1 (list 3)) (list 123 2 (list 2 3)) (list 123 3 (list 1 2 3))
	  (list 1234 1 (list 4)) (list 1234 2 (list 3 4))
	  (list 1234 3 (list 2 3 4)) (list 1234 4 (list 1 2 3 4))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((large-num (list-ref alist 0))
		 (n-digits (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (extract-last-n-digits large-num n-digits)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : large-num = ~a, n-digits = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index large-num n-digits
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
(define (main-loop factor1 base exponent modulus n-digits)
  (let ((exp-term (modulo-power base exponent modulus)))
    (let ((second-term (modulo (* factor1 exp-term) modulus)))
      (let ((large-num (modulo (+ 1 second-term) modulus)))
	(let ((digit-list (extract-last-n-digits large-num n-digits)))
	  (begin
	    (display (ice9-format:format
		      #f "the last ~a digits of ~:d x ~:d^(~:d) + 1 is ~a~%"
		      n-digits factor1 base exponent digit-list))
	    (force-output)
	    ))
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
    (display (format #f "Project Euler 97 - The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne prime of the form (2^6972593)-1; it contains exactly 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^p-1, have been found which contain more digits.~%"))
    (newline)
    (display (format #f "However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433x(2^7830457)+1.~%"))
    (newline)
    (display (format #f "Find the last ten digits of this prime number.~%"))
    (newline)
    (display (format #f "for the solution method: see http://en.wikipedia.org/wiki/Modular_exponentiation~%"))
    (display (format #f "modular exponentiation means that you don't have to compute all 2 million digits.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-modulo-power-1 counter)
	   (run-test test-extract-last-n-digits-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((factor1 10)
	  (base 2)
	  (exponent 3)
	  (modulus 100000))
      (begin
	(time-code
	 (begin
	   (do ((ii 1 (1+ ii)))
	       ((> ii 3))
	     (begin
	       (main-loop factor1 base exponent modulus ii)
	       ))
	   ))
	))

    (newline)
    (force-output)

    (let ((factor1 10)
	  (base 2)
	  (exponent 4)
	  (modulus 100000))
      (begin
	(time-code
	 (begin
	   (do ((ii 1 (1+ ii)))
	       ((> ii 3))
	     (begin
	       (main-loop factor1 base exponent modulus ii)
	       ))
	   ))
	))

    (newline)
    (force-output)

    (let ((factor1 28433)
	  (base 2)
	  (exponent 7830457)
	  (mod-num 1000000000000)
	  (n-digits 10))
      (begin
	(time-code
	 (begin
	   (main-loop factor1 base exponent mod-num n-digits)
	   ))
	))

    (newline)
    ))
