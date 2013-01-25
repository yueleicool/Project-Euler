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
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (sum-prime-array max-num)
  (let ((intermediate-array (make-array 0 max-num))
        (prime-sum 0))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii max-num))
	(begin
	  (array-set! intermediate-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((>= ii max-num))
	(begin
	  (do ((jj ii (+ jj ii)))
	      ((>= jj max-num))
	    (begin
	      (let ((this-num (array-ref intermediate-array jj)))
		(begin
		  (if (= this-num ii)
		      (begin
			(set! prime-sum (+ prime-sum this-num)))
		      (begin
			(array-set! intermediate-array -1 jj)
			))
                  ))
              ))
          ))

      prime-sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-sum-prime-array-1)
  (let ((sub-name "test-sum-prime-array-1")
	(test-list
	 (list
	  (list 2 0) (list 3 2) (list 4 5)
	  (list 5 5) (list 6 10)
	  (list 7 10) (list 8 17) (list 9 17)
	  (list 10 17) (list 11 17) (list 12 28)
	  (list 13 28) (list 14 41) (list 15 41)
          (list 16 41) (list 17 41) (list 18 58)
	  (list 19 58) (list 20 77) (list 21 77)
          (list 22 77) (list 23 77) (list 24 100)
	  (list 25 100)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (sum-prime-array test-num)))
               (begin
                 (if (not (equal? shouldbe result))
                     (begin
                       (display
                        (format #f "~a : error (~a) : num=~a, "
                                sub-name test-label-index test-num))
                       (display
                        (format #f "shouldbe=~a, result=~a~%"
                                shouldbe result))
                       (set! ok-flag #f)
                       ))
                 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop ii)
  (let ((sum-primes (sum-prime-array ii)))
    (begin
      (display
       (ice9-format:format
        #f "the sum of primes less than ~:d is ~:d~%"
        ii sum-primes))
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
    (display (format #f "Project Euler 10: The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.~%"))
    (display (format #f "Find the sum of all the primes below two million.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-sum-prime-array-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (do ((ii 10 (+ ii 1)))
	((> ii 20))
      (begin
	(main-loop ii)
	))

    (newline)
    (force-output)

    (let ((nn 2000000))
      (begin
	(time-code
	 (begin
	   (main-loop nn)
	   ))
	))
    (newline)
    ))
