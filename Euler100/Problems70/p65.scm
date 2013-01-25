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

;;;### srfi-1 for let-values (multiple value binding)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

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
(define (factorial nn)
  (cond
   ((< nn 0) #f)
   ((= nn 0) 1)
   (else
    (* nn (factorial (- nn 1)))
    )))

;;;#############################################################
;;;#############################################################
(define (test-factorial-1)
  (let ((sub-name "test-factorial-1")
	(test-list
	 (list
	  (list 0 1) (list 1 1) (list 2 2)
	  (list 3 6) (list 4 24) (list 5 120)
	  (list 6 720)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (factorial test-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
					shouldbe-num result-num))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; need e as a rational number, use the series expansion
(define (calc-rational-e max-iterations)
  (let ((sum 1))
    (begin
      (do ((ii 1 (+ ii 1)))
	  ((> ii max-iterations))
	(begin
	  (let ((fact (factorial ii)))
	    (set! sum (+ sum (/ 1 fact)))
	    )))
      sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-rational-e-1)
  (let ((sub-name "test-calc-rational-e-1")
	(test-list
	 (list
	  (list 0 1) (list 1 2) (list 2 (/ 5 2))
	  (list 3 (/ 8 3))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (calc-rational-e test-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
					shouldbe-num result-num))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (compute-nth-expansion nn rational-e)
  (let ((sum 0)
	(local-num rational-e)
	(local-digit-list (list)))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((> ii nn))
	(begin
	  (let ((first-digit
		 (inexact->exact
		  (truncate
		   (exact->inexact local-num)))))
	    (let ((drest (- local-num first-digit)))
	      (begin
		(set! local-digit-list (cons first-digit local-digit-list))

		(set! local-num (/ 1 drest))
		)))))

      (let ((dlen (- (length local-digit-list) 1))
	    (rational-num 0))
	(begin
	  (do ((jj 0 (+ jj 1)))
	      ((>= jj dlen))
	    (begin
	      (let ((this-num (list-ref local-digit-list jj)))
		(begin
		  (set! rational-num (/ 1 (+ rational-num this-num)))
		  ))
	      ))
	  (set! rational-num (+ rational-num (car (last-pair local-digit-list))))

	  rational-num
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-compute-nth-expansion-1)
  (let ((sub-name "test-compute-nth-expansion-1")
	(test-list
	 (list
	  (list 0 10 2) (list 1 10 3)
	  (list 2 20 (/ 8 3)) (list 3 20 (/ 11 4))
	  (list 4 20 (/ 19 7)) (list 5 20 (/ 87 32))
	  (list 6 20 (/ 106 39)) (list 7 20 (/ 193 71))
	  (list 8 20 (/ 1264 465)) (list 9 20 (/ 1457 536))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((index (list-ref alist 0))
		 (max-iter (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((rat-e (calc-rational-e max-iter)))
	       (let ((result (compute-nth-expansion index rat-e)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : index=~a, max-iter=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index index max-iter
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
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (cond
     ((< this-num 0) acc-list)
     ((< this-num 10) (cons this-num acc-list))
     (else
      (let ((next-num 0)
	    (this-digit 0))
	(begin
	  (call-with-values (lambda() (euclidean/ this-num 10))
	    (lambda (a b)
	      (begin
		(set! next-num a)
		(set! this-digit b))))
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
		       (display (format #f "~a : error (~a) : number = ~a, list shouldbe = ~a, result list = ~a~%"
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
(define (main-loop max-num max-decimals debug-flag)
  (let ((rational-e (calc-rational-e max-decimals))
	(digits-sum 0))
    (begin
      (if (equal? debug-flag #t)
	  (begin
	    (display (format #f "the first ~a terms in the sequence of convergents for e are:~%" max-num))

	    (do ((ii 0 (+ ii 1)))
		((>= ii max-num))
	      (begin
		(let ((rat-term (compute-nth-expansion ii rational-e)))
		  (if (= ii 0)
		      (begin
			(display (format #f "~a" rat-term)))
		      (begin
			(display (format #f ", ~a" rat-term))
			)))
		))
	    (newline)
	    ))

      (let ((this-term (compute-nth-expansion (- max-num 1) rational-e)))
	(let ((numer (numerator this-term)))
	  (let ((dlist (split-digits-list numer)))
	    (let ((dsum (srfi-1:fold + 0 dlist)))
	      (let ((dstring (string-join
			      (map
			       (lambda (this-num)
				 (format #f "~a" this-num))
			       dlist) "+")))
		(begin
		  (display (ice9-format:format #f "the sum of digits in the numerator of the ~:dth convergent is ~a=~:d.~%"
					       max-num dstring dsum))
		  (force-output)
		  ))))))
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
    (display (format #f "Problem 065 - The square root of 2 can be written as an infinite continued fraction.~%"))
    (newline)
    (display (format #f "sqrt(2) = 1+ 1/(2+1/(2+1/(2+1/(2+...))))~%"))
    (display (format #f "The infinite continued fraction can be written, sqrt(2) = [1;(2)], (2) indicates that 2 repeats ad infinitum. In a similar way, 23 = [4;(1,3,1,8)].~%"))
    (newline)
    (display (format #f "It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations. Let us consider the convergents for sqrt(2).~%"))
    (newline)
    (display (format #f "  1+1/2 = 3/2~%"))
    (display (format #f "  1+1/(2+1/2) = 7/5~%"))
    (display (format #f "  1+1/(2+1/(2+1/2)) = 17/12~%"))
    (display (format #f "  1+1/(2+1/(2+1/(2+1/2))) = 41/29~%"))
    (display (format #f "Hence the sequence of the first ten convergents for 2 are:~%"))
    (newline)
    (display (format #f "What is most surprising is that the important mathematical constant,~%"))
    (display (format #f "e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].~%"))
    (display (format #f "The first ten terms in the sequence of convergents for e are:~%"))
    (display (format #f "2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...~%"))
    (display (format #f "The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.~%"))
    (newline)
    (display (format #f "Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-factorial-1 counter)
	   (run-test test-calc-rational-e-1 counter)
	   (run-test test-compute-nth-expansion-1 counter)
	   (run-test test-split-digits-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 10)
	  (max-decimals 20)
	  (debug-flag #t))
      (begin
	(main-loop max-num max-decimals debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 100)
	  (max-decimals 100)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num max-decimals debug-flag)
	   ))
	))

    (newline)
    ))
