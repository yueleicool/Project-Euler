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
(define (list-to-sum-string llist)
  (let ((this-string
	 (string-join
	  (map
	   (lambda (this-elem)
	     (ice9-format:format #f "~:d" this-elem))
	   llist)
	  " + ")))
    this-string
    ))

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
		       (display (format #f "~a : (~a) : error : list=~a, shouldbe=~a, result=~a~%"
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
(define (main-loop max-number max-exponent sequence-target-number debug-flag)
  (let ((sequence-counter 0)
	(continue-loop-flag #t)
	(result-list (list)))
    (begin
      (do ((ii 2 (1+ ii)))
	  ((> ii max-number))
	(begin
	  (let ((num ii))
	    (begin
	      (do ((jj 1 (1+ jj)))
		  ((> jj max-exponent))
		(begin
		  (let ((digit-list (split-digits-list num)))
		    (let ((d-sum (srfi-1:fold + 0 digit-list)))
		      (begin
			(if (and (= d-sum ii) (>= num 10))
			    (begin
			      (set! sequence-counter (1+ sequence-counter))

			      (set! result-list (cons (list num ii jj) result-list))
			      ))

			(set! num (* num ii))
			)))
		  ))
	      ))
	  ))

      (if (< sequence-counter sequence-target-number)
	  (begin
	    (display (ice9-format:format #f "no results found for numbers between 10 and ~:d~%" max-number))
	    (force-output))
	  (begin
	    (let ((sorted-list-list
		   (sort result-list
			 (lambda (a b)
			   (< (list-ref a 0) (list-ref b 0)))))
		  (rlen (length result-list)))
	      (begin
		(if (equal? debug-flag #t)
		    (begin
		      (let ((sequence-counter 1))
			(begin
			  (do ((ii 0 (1+ ii)))
			      ((> ii sequence-target-number))
			    (begin
			      (let ((a-list (list-ref sorted-list-list ii)))
				(let ((a-num (list-ref a-list 0))
				      (a-base (list-ref a-list 1))
				      (a-exp (list-ref a-list 2)))
				  (let ((d-list (split-digits-list a-num)))
				    (let ((d-string (list-to-sum-string d-list)))
				      (begin
					(display (ice9-format:format #f "  a(~:d) = ~:d : ~a = ~:d and ~:d^~:d = ~:d~%"
								     sequence-counter a-num
								     d-string a-base a-base
								     a-exp a-num))
				       (force-output)
				       (set! sequence-counter (1+ sequence-counter))
				       ))
				    )))
			      ))
			  (newline)
			  ))
		      ))

		(let ((r-list (list-ref sorted-list-list (1- sequence-target-number))))
		  (let ((a-num (list-ref r-list 0))
			(a-base (list-ref r-list 1))
			(a-exp (list-ref r-list 2)))
		    (let ((d-list (split-digits-list a-num)))
		      (let ((d-string (list-to-sum-string d-list)))
			(begin
			  (display (ice9-format:format #f "  a(~:d) = ~:d : ~a = ~:d and ~:d^~:d = ~:d~%"
						       sequence-target-number a-num
						       d-string a-base a-base
						       a-exp a-num))
			  (force-output)
			  ))
		      )))
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
    (display (format #f "Project Euler 119 - The number 512 is interesting because it is equal to the sum of its digits raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a number with this property is 614656 = 28^4.~%"))
    (newline)
    (display (format #f "We shall define an to be the nth term of this sequence and insist that a number must contain at least two digits to have a sum.~%"))
    (newline)
    (display (format #f "You are given that a2 = 512 and a10 = 614656.~%"))
    (newline)
    (display (format #f "Find a30.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-list-to-sum-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-number 100)
	  (max-exponent 20)
	  (sequence-target-number 10)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-number max-exponent sequence-target-number debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-number 100)
	  (max-exponent 20)
	  (sequence-target-number 30)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-number max-exponent sequence-target-number debug-flag)
	   ))
	))

    (newline)
    ))
