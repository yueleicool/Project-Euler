#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format function
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
	   (set! test-label-index (1+ test-label-index))
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
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (is-abundant? this-num)
  (let ((div-list (proper-divisors-list this-num)))
    (let ((div-sum (srfi-1:fold + 0 div-list)))
      (begin
	(if (> div-sum this-num)
	    #t
	    #f
	    )))))

;;;#############################################################
;;;#############################################################
(define (test-is-abundant-1)
  (let ((sub-name "test-is-abundant-1")
	(test-list
	 (list
	  (list 2 #f) (list 3 #f) (list 4 #f) (list 5 #f)
	  (list 6 #f) (list 8 #f) (list 9 #f) (list 10 #f)
	  (list 11 #f) (list 12 #t) (list 13 #f) (list 14 #f)
	  (list 15 #f) (list 16 #f) (list 17 #f) (list 18 #t)
	  (list 19 #f) (list 20 #t) (list 21 #f) (list 22 #f)
	  (list 23 #f) (list 24 #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (is-abundant? test-num)))
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
(define (populate-abundant-hash! ab-htable max-num)
  (do ((ii 12 (+ ii 1)))
      ((> ii max-num))
    (begin
      (if (is-abundant? ii)
	  (hash-set! ab-htable ii #t)
	  ))))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify code
(define-syntax two-abundant-check
  (syntax-rules ()
    ((two-abundant-check jj ab-length ab-list ab-htable
			 sum sum-list debug-flag)
     (begin
       (let ((two-ab-flag #f)
	     (completed-flag #f))
	 (begin
	   (do ((ii 0 (+ ii 1)))
	       ((or (>= ii ab-length)
		    (equal? completed-flag #t)))
	     (begin
	       (let ((this-ab (list-ref ab-list ii)))
		 (begin
		   (if (>= this-ab jj)
		       (begin
			 (set! completed-flag #t))
		       (begin
			 (let ((diff (- jj this-ab)))
			   (if (equal? (hash-ref ab-htable diff #f) #t)
			       (begin
				 (if (equal? debug-flag #t)
				     (display (ice9-format:format #f "debug two abundant numbers sum ~:d + ~:d = ~:d~%" this-ab diff jj)))
				 (set! two-ab-flag #t)
				 (set! completed-flag #t)))
			   )))
		   ))
	       ))

	   (if (not (equal? two-ab-flag #t))
	       (begin
		 (set! sum (+ sum jj))
		 (set! sum-list (cons jj sum-list))
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((sum 0)
	(sum-list (list))
	(ab-htable (make-hash-table 10000)))
    (begin
      (populate-abundant-hash! ab-htable max-num)

      (let ((ab-list (sort
		      (hash-map->list
		       (lambda (key value) key) ab-htable)
		      <)))
	(let ((min-ab-value (list-ref ab-list 0))
	      (max-ab-value (list-ref ab-list (- (length ab-list) 1)))
	      (ab-length (length ab-list))
	      (two-min-ab-value (* 2 (list-ref ab-list 0))))
	  (begin
	    (do ((jj 1 (+ jj 1)))
		((> jj max-num))
	      (begin
		(cond
		 ((< jj two-min-ab-value)
		  (begin
		    (set! sum (+ sum jj))
		    (set! sum-list (cons jj sum-list))
		    ))
		 (else
		  (two-abundant-check jj ab-length ab-list ab-htable
				      sum sum-list debug-flag)
		  ))
		))

	    (if (equal? debug-flag #t)
                (begin
                  (let ((tsum (srfi-1:fold + 0 sum-list)))
                    (begin
                      (if (not (equal? tsum sum))
                          (begin
                            (display
                             (ice9-format:format
                              #f "warning: sum of list ~:d is not equal to accumulated sum ~:d~%"
                              tsum sum))
                            ))
                      (display
                       (ice9-format:format
                        #f "sum of all positive integers less than ~:d that are not the sum of two abundant numbers : ~a = ~:d~%"
                        max-num (list-to-sum-string (reverse sum-list)) sum))
                      )))
                (begin
                  (display
                   (ice9-format:format
                    #f "~:d is the sum of all positive integers less than ~:d that are not the sum of two abundant numbers~%"
                    sum max-num sum))
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
    (display (format #f "Problem 023 - A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.~%"))
    (newline)
    (display (format #f "A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.~%"))
    (newline)
    (display (format #f "As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.~%"))
    (newline)
    (display (format #f "Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.~%"))
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
	   (run-test test-is-abundant-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 30)
	  (debug-flag #t))
      (begin
	(main-loop max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 28124)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))
    (newline)
    ))
