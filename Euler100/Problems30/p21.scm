#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### advanced format function
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
(define (proper-divisors-list this-num)
  (let ((div-list (list 1))
	(max-iter (+ (exact-integer-sqrt this-num) 1)))
    (begin
      (if (<= this-num 1)
	  (set! div-list (list)))

      (do ((ii 2 (+ ii 1)))
	  ((> ii max-iter))
	(begin
	  (if (zero? (modulo this-num ii))
	      (begin
		(let ((other-div (euclidean/ this-num ii)))
		  (begin
		    (if (< ii other-div)
			(begin
			  (set! div-list (append div-list (list ii other-div))))
			(begin
			  (if (= ii other-div)
			      (set! div-list (append div-list (list ii))))
			  ))
		    ))))
	  ))

      (sort div-list <)
      )))

;;;#############################################################
;;;#############################################################
(define (test-proper-divisors-list-1)
  (let ((sub-name "test-proper-divisors-list-1")
	(test-list
	 (list
	  (list 1 (list)) (list 3 (list 1))
	  (list 6 (list 1 2 3)) (list 10 (list 1 2 5))
	  (list 15 (list 1 3 5)) (list 21 (list 1 3 7))
	  (list 28 (list 1 2 4 7 14))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (proper-divisors-list test-num)))
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
(define (list-to-sum-string llist)
  (let ((stmp (string-join
	       (map
		(lambda (this-num)
		  (ice9-format:format #f "~:d" this-num))
		llist) " + ")))
    (begin
      stmp
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-sum-string-1)
  (let ((sub-name "test-list-to-sum-string-1")
	(test-list
	 (list
	  (list (list 1) "1")
	  (list (list 1 2) "1 + 2")
	  (list (list 1 2 3) "1 + 2 + 3")
	  (list (list 4 5 6 7) "4 + 5 + 6 + 7")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-sum-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (divisors-sum this-num)
  (let ((div-list (proper-divisors-list this-num)))
    (let ((div-sum (srfi-1:fold + 0 div-list)))
      (begin
	(list div-sum div-list)
	))))

;;;#############################################################
;;;#############################################################
(define (test-divisors-sum-1)
  (let ((sub-name "test-divisors-sum-1")
	(test-list
	 (list
	  (list 2 (list 1 (list 1))) (list 3 (list 1 (list 1)))
	  (list 4 (list 3 (list 1 2))) (list 5 (list 1 (list 1)))
	  (list 6 (list 6 (list 1 2 3))) (list 8 (list 7 (list 1 2 4)))
	  (list 10 (list 8 (list 1 2 5))) (list 12 (list 16 (list 1 2 3 4 6)))
	  (list 15 (list 9 (list 1 3 5)))
	  (list 24 (list 36 (list 1 2 3 4 6 8 12)))
	  (list 28 (list 28 (list 1 2 4 7 14)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (divisors-sum test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num)
  (let ((sum 0)
	(sum-list (list)))
    (begin
      (do ((jj 1 (+ jj 1)))
	  ((> jj max-num))
	(begin
	  (let ((result-list (divisors-sum jj)))
	    (let ((dsum (list-ref result-list 0))
		  (dlist (list-ref result-list 1)))
	      (let ((result2-list (divisors-sum dsum)))
		(let ((dsum2 (list-ref result2-list 0))
		      (dlist2 (list-ref result2-list 1)))
		  (begin
		    (if (and (equal? jj dsum2)
			     (>= dsum jj))
			(begin
			  (if (not (equal? dsum dsum2))
			      (begin
				(set! sum (+ sum dsum dsum2))
				(set! sum-list (append sum-list (list dsum dsum2)))

				(display (ice9-format:format #f "(~:d, ~:d) is an amicable pair! sum of divisors of ~:d : ~a = ~:d~%"
							     jj dsum jj
							     (list-to-sum-string dlist) dsum))
				(display (ice9-format:format #f "    sum of divisors of ~:d : ~a = ~:d~%"
							     dsum (list-to-sum-string dlist2) dsum2))
				)))
			))))
	      ))
	  ))

      (let ((s-list (sort sum-list <)))
	(let ((s-string (list-to-sum-string s-list)))
	  (begin
	    (display (ice9-format:format #f "The sum of all amicable numbers under ~:d is ~:d = ~a~%"
					 max-num sum s-string))
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
    (display (format #f "Problem 021 - Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).~%"))
    (display (format #f "If d(a) = b and d(b) = a, where a != b, then a and b are an amicable pair and each of a and b are called amicable numbers.~%"))
    (newline)
    (display (format #f "For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.~%"))
    (newline)
    (display (format #f "Evaluate the sum of all the amicable numbers under 10000.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-proper-divisors-list-1 counter)
	   (run-test test-list-to-sum-string-1 counter)
	   (run-test test-divisors-sum-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 300))
      (begin
	(main-loop max-num)
	))

    (newline)
    (force-output)

    (let ((max-num 10000))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))
    (newline)
    ))
