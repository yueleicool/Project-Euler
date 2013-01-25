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
;;; most significant digit in position 0
(define (turn-digit-list-to-number dlist)
  (let ((this-num
	 (srfi-1:fold
	  (lambda (this-elem prev-elem)
	    (+ this-elem (* 10 prev-elem)))
	  0 dlist)))
    this-num
    ))

;;;#############################################################
;;;#############################################################
(define (test-turn-digit-list-to-number-1)
  (let ((sub-name "test-turn-digit-list-to-number-1")
	(test-list
	 (list
	  (list (list 1 2) 12)
	  (list (list 2 1) 21)
	  (list (list 1 2 3) 123)
	  (list (list 3 2 1) 321)
	  (list (list 1 2 3 4) 1234)
	  (list (list 4 3 2 1) 4321)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (turn-digit-list-to-number test-list)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : (~a) : error : list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-list
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
(define (digit-sum this-num)
  (let ((dlist (split-digits-list this-num)))
    (let ((dsum (srfi-1:fold + 0 dlist)))
      dsum
      )))

;;;#############################################################
;;;#############################################################
(define (test-digit-sum-1)
  (let ((sub-name "test-digit-sum-1")
	(test-list
	 (list
	  (list 1 1) (list 2 2) (list 3 3)
	  (list 10 1) (list 11 2) (list 12 3)
	  (list 100 1) (list 101 2) (list 102 3)
	  (list 103 4) (list 104 5) (list 123 6)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (digit-sum test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : num=~a, shouldbe=~a, result=~a~%"
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
(define (integer-power this-number this-exponent)
  (cond
   ((= this-exponent 0) 1)
   ((= this-exponent 1) this-number)
   ((< this-exponent 0) -1)
   (else
    (let ((result-num this-number)
	  (max-iter (- this-exponent 1)))
      (begin
	(do ((ii 0 (+ ii 1)))
	    ((>= ii max-iter))
	  (begin
	    (set! result-num (* result-num this-number))
	    ))
	result-num
	)))))

;;;#############################################################
;;;#############################################################
(define (test-integer-power-1)
  (let ((sub-name "test-integer-power-1")
	(test-list
	 (list
	  (list 10 0 1) (list 11 0 1) (list 12 0 1)
	  (list 10 1 10) (list 11 1 11) (list 12 1 12)
	  (list 10 2 100) (list 11 2 121) (list 12 2 144)
	  (list 2 2 4) (list 2 3 8) (list 2 4 16) (list 2 5 32)
	  (list 2 6 64) (list 2 7 128) (list 2 8 256) (list 2 9 512)
	  (list 2 10 1024)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (test-exp (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (let ((result-num (integer-power test-num test-exp)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, exponent = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num test-exp
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
(define (main-loop start-num max-num)
  (begin
    (let ((max-aa 0)
	  (max-bb 0)
	  (max-aa-bb 0)
	  (max-digit-sum 0))
      (begin
	(do ((aa start-num (+ aa 1)))
	    ((> aa max-num))
	  (begin
	    (do ((bb start-num (+ bb 1)))
		((> bb max-num))
	      (begin
		(let ((this-num (integer-power aa bb)))
		  (let ((dsum (digit-sum this-num)))
		    (begin
		      (if (> dsum max-digit-sum)
			  (begin
			    (set! max-aa aa)
			    (set! max-bb bb)
			    (set! max-aa-bb this-num)
			    (set! max-digit-sum dsum)
			    ))
		      )))
		))
	    ))

	(newline)
	(display (ice9-format:format #f "The maximum digit sum found = ~:d : a^b = ~:d^~:d = ~:d, where a and b are between ~:d and ~:d~%"
				     max-digit-sum max-aa max-bb max-aa-bb start-num max-num))
	(force-output)
	))))

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
    (display (format #f "Problem 056 - A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.~%"))
    (newline)
    (display (format #f "Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-turn-digit-list-to-number-1 counter)
	   (run-test test-digit-sum-1 counter)
	   (run-test test-integer-power-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 1)
	  (max-num 10))
      (begin
	(main-loop start-num max-num)
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (max-num 100))
      (begin
	(time-code
	 (begin
	   (main-loop start-num max-num)
	   ))
	))

    (newline)
    ))
