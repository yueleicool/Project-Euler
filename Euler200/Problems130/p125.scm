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
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (cond
     ((< this-num 0) acc-list)
     ((< this-num 10) (cons this-num acc-list))
     (else
      (srfi-11:let-values
       (((next-num this-digit) (euclidean/ this-num 10)))
       (begin
	 (local-loop next-num (cons this-digit acc-list))
	 )))))
  (let ((result-list (local-loop this-num (list))))
    result-list
    ))

;;;#############################################################
;;;#############################################################
(define (test-split-digits-list-1)
  (let ((sub-name "test-split-digits-list-1")
	(test-list
	 (list
	  (list 3 (list 3)) (list 4 (list 4)) (list 5 (list 5))
	  (list 13 (list 1 3)) (list 14 (list 1 4)) (list 15 (list 1 5))
	  (list 23 (list 2 3)) (list 24 (list 2 4)) (list 25 (list 2 5))
	  (list 123 (list 1 2 3)) (list 1234 (list 1 2 3 4)) (list 98765 (list 9 8 7 6 5))
	  (list 341608987 (list 3 4 1 6 0 8 9 8 7))
	  (list 116696699999166169 (list 1 1 6 6 9 6 6 9 9 9 9 9 1 6 6 1 6 9))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (split-digits-list test-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, list shouldbe = ~a, result list = ~a~%"
					sub-name test-label-index test-num
					shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (is-number-palindrome? anum)
  (let ((dlist (split-digits-list anum)))
    (let ((rlist (reverse dlist)))
      (begin
	(equal? dlist rlist)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-is-number-palindrome-1)
  (let ((sub-name "test-is-number-palindrome-1")
	(test-list
	 (list
	  (list 11 #t) (list 12 #f) (list 999 #t)
	  (list 12321 #t) (list 11223 #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((anum (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (is-number-palindrome? anum)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : anum = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index anum
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
(define (list-to-square-string a-list)
  (let ((astring (string-join
		  (map
		   (lambda (a-num)
		     (begin
		       (ice9-format:format #f "~:d^2" a-num)))
		   a-list) " + ")))
    (begin
      astring
      )))

;;;#############################################################
;;;#############################################################
(define-syntax process-palindrome
  (syntax-rules ()
    ((process-palindrome jj-sum rlist square-count square-sum
			 debug-flag seen-htable)
     (begin
       (let ((sflag (hash-ref seen-htable jj-sum #f)))
	 (begin
	   (if (equal? sflag #f)
	       (begin
		 (hash-set! seen-htable jj-sum #t)
		 (set! square-sum (+ square-sum jj-sum))
		 (set! square-count (1+ square-count))
		 (if (equal? debug-flag #t)
		     (begin
		       (let ((slist (sort rlist <)))
			 (let ((s-string (list-to-square-string slist)))
			   (begin
			     (display (format #f "  (~:d) ~:d = ~a, sum so far = ~:d~%"
					      square-count jj-sum s-string
					      square-sum))
			     (force-output)
			     )))
		       ))
		 ))
	   ))
       ))
    ))


;;;#############################################################
;;;#############################################################
(define (main-loop max-sum debug-flag)
  (let ((square-sum 0)
	(square-count 0)
	(seen-htable (make-hash-table))
	(max-ii (1+ (exact-integer-sqrt max-sum))))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((> ii max-ii))
	(begin
	  (let ((rlist (list))
		(continue-inner-loop-flag #t)
		(jj-sum 0))
	    (begin
	      (do ((jj ii (1+ jj)))
		  ((or (> jj max-ii)
		       (equal? continue-inner-loop-flag #f)))
		(begin
		  (let ((jj-sqr (* jj jj)))
		    (begin
		      (set! rlist (cons jj rlist))
		      (set! jj-sum (+ jj-sum jj-sqr))
		      (if (<= jj-sum max-sum)
			  (begin
			    (if (and (> (length rlist) 1)
				     (is-number-palindrome? jj-sum))
				(begin
				  (process-palindrome jj-sum rlist square-count square-sum
						      debug-flag seen-htable)
				  )))
			  (begin
			    (set! continue-inner-loop-flag #f)
			    ))
		      ))
		  ))
	      ))
	  ))

      (display (ice9-format:format #f "the sum of palindromes is ~:d.  found exactly ~:d palindromes that are less than ~:d.~%"
				   square-sum square-count max-sum))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
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
    (display (format #f "Project Euler 125 - The palindromic number 595 is interesting because it can be written as the sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.~%"))
    (newline)
    (display (format #f "There are exactly eleven palindromes below one-thousand that can be written as consecutive square sums, and the sum of these palindromes is 4164. Note that 1 = 0^2 + 1^2 has not been included as this problem is concerned with the squares of positive integers.~%"))
    (newline)
    (display (format #f "Find the sum of all the numbers less than 10^8 that are both palindromic and can be written as the sum of consecutive squares.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-is-number-palindrome-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-sum 1000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-sum debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-sum 100000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-sum debug-flag)
	   ))
	))

    (newline)
    ))
