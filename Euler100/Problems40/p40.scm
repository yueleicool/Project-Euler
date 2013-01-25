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

;;;### ice-9 receive - receive multiple values
(use-modules ((ice-9 receive)
	      :renamer (symbol-prefix-proc 'ice9-receive:)))

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
(define (find-digit-index digit-index)
  (define (local-interp-formula min-index max-index digit-index min-num ndigits)
    (cond
     ((or (< digit-index min-index) (> digit-index max-index))
      (begin
	(display (format #f "find-digit-index error: digit index ~a must be between ~a and ~a~%"
			 digit-index min-index max-index))
	(quit)
	))
     (else
      (begin
	(let ((tmp1 0)
	      (rtmp1 0))
	  (begin
	    (ice9-receive:receive
	     (tmp1 rtmp1)
	     (euclidean/ (- digit-index min-index) ndigits)
	     (begin
	       (let ((this-num (+ tmp1 min-num)))
		 (let ((dlist (split-digits-list this-num)))
		   (if (zero? rtmp1)
		       (begin
			 (list-ref dlist 0))
		       (begin
			 (list-ref dlist rtmp1)
			 ))))
	       ))))
	))
     ))
  (let ((num-1-digits 9)
	(num-2-digits (- 99 9))                  ;;; 90, number of 2-digit numbers
	(num-3-digits (- 999 99))                ;;; 900, number of 3-digit numbers
	(num-4-digits (- 9999 999))              ;;; 9,000
	(num-5-digits (- 99999 9999))            ;;; 90,000
	(num-6-digits (- 999999 99999))          ;;; 900,000
	(num-7-digits (- 9999999 999999)))       ;;; 9,000,000
    (let* ((tr-1-index (+ num-1-digits 1))                    ;;; 10, transition from 1-digit numbers to 2-digits
	   (tr-2-index (+ tr-1-index (* 2 num-2-digits)))     ;;; 190, transition from 2-digit numbers to 3-digits
	   (tr-3-index (+ tr-2-index (* 3 num-3-digits)))     ;;; 2,890
	   (tr-4-index (+ tr-3-index (* 4 num-4-digits)))     ;;; 38,890
	   (tr-5-index (+ tr-4-index (* 5 num-5-digits)))     ;;; 488,890
	   (tr-6-index (+ tr-5-index (* 6 num-6-digits)))     ;;; 5,888,890
	   (tr-7-index (+ tr-6-index (* 7 num-7-digits))))    ;;; 68,888,890
      (let ((dresult 0))
	(begin
;;;	  (display (ice9-format:format #f "num-1=~:d, tr-1-index=~:d, num-2=~:d, tr-2=~:d, num-3=~:d, tr-3=~:d, num-4=~:d, tr-4=~:d, num-5=~:d, tr-5=~:d, num-6=~:d, tr-6=~:d, num-7=~:d, tr-7=~:d~%" num-1-digits tr-1-index num-2-digits tr-2-index num-3-digits tr-3-index num-4-digits tr-4-index num-5-digits tr-5-index num-6-digits tr-6-index num-7-digits tr-7-index))
	  (cond
	   ((<= digit-index 0) #f)
	   ((and (>= digit-index 0) (< digit-index tr-1-index)) digit-index)
	   ((and (>= digit-index tr-1-index) (< digit-index tr-2-index))
	    (begin
	      (local-interp-formula tr-1-index tr-2-index digit-index 10 2)))
	   ((and (>= digit-index tr-2-index) (< digit-index tr-3-index))
	    (begin
	      (local-interp-formula tr-2-index tr-3-index digit-index 100 3)))
	   ((and (>= digit-index tr-3-index) (< digit-index tr-4-index))
	    (begin
	      (local-interp-formula tr-3-index tr-4-index digit-index 1000 4)))
	   ((and (>= digit-index tr-4-index) (< digit-index tr-5-index))
	    (begin
	      (local-interp-formula tr-4-index tr-5-index digit-index 10000 5)))
	   ((and (>= digit-index tr-5-index) (< digit-index tr-6-index))
	    (begin
	      (local-interp-formula tr-5-index tr-6-index digit-index 100000 6)))
	   ((and (>= digit-index tr-6-index) (< digit-index tr-7-index))
	    (begin
	      (local-interp-formula tr-6-index tr-7-index digit-index 1000000 7)))
	   (else
	    (begin
	      (display (format #f "find-digit-index error: digit index ~a must be less than ~a~%"
			       digit-index tr-7-index))
	      (quit)
	      ))
	   ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-find-digit-index-1)
  (let ((sub-name "test-find-digit-index-1")
	(test-list
	 (list
	  (list 0 #f) (list 1 1) (list 2 2) (list 3 3) (list 4 4)
	  (list 5 5) (list 6 6) (list 7 7) (list 8 8) (list 9 9)
	  (list 10 1) (list 11 0) (list 12 1) (list 13 1)
	  (list 14 1) (list 15 2) (list 16 1) (list 17 3)
	  (list 18 1) (list 19 4) (list 20 1) (list 21 5)
	  (list 22 1) (list 23 6) (list 24 1) (list 25 7)
	  (list 100 5) (list 1000 3)
	  (list 10000 7)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (find-digit-index test-num)))
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
(define (split-method max-num debug-flag)
  (let ((this-dlist (list 0 1 2 3 4 5 6 7 8 9)))
    (begin
      (do ((ii 10 (+ ii 1)))
	  ((> ii max-num))
	(begin
	  (let ((dlist (split-digits-list ii)))
	    (set! this-dlist (append this-dlist dlist))
	    )))

      (let ((d1 (list-ref this-dlist 1))
	    (d10 (list-ref this-dlist 10))
	    (d100 (list-ref this-dlist 100))
	    (d1000 (list-ref this-dlist 1000))
	    (d10000 (list-ref this-dlist 10000))
	    (d100000 (list-ref this-dlist 100000))
	    (d1000000 (list-ref this-dlist 1000000)))
	(let ((product (* d1 d10 d100 d1000 d10000 d100000 d1000000)))
	  (begin
	    (display (format #f "the product d(1) x d(10) x d(100) x d(1,000) x d(10,000) x d(100,000) x d(1,000,000)~%"))
	    (display (ice9-format:format #f "~a x ~a x ~a x ~a x ~a x ~a x ~a = ~:d~%"
					 d1 d10 d100 d1000 d10000 d100000 d1000000 product))
	    (force-output)
	    )))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop)
  (begin
    (let ((d1 (find-digit-index 1))
	  (d10 (find-digit-index 10))
	  (d100 (find-digit-index 100))
	  (d1000 (find-digit-index 1000))
	  (d10000 (find-digit-index 10000))
	  (d100000 (find-digit-index 100000))
	  (d1000000 (find-digit-index 1000000)))
      (let ((product (* d1 d10 d100 d1000 d10000 d100000 d1000000)))
	(begin
	  (display (format #f "the product d(1) x d(10) x d(100) x d(1,000) x d(10,000) x d(100,000) x d(1,000,000)~%"))
	  (display (ice9-format:format #f "~a x ~a x ~a x ~a x ~a x ~a x ~a = ~:d~%"
				       d1 d10 d100 d1000 d10000 d100000 d1000000 product))
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
    (display (format #f "Problem 040 - An irrational decimal fraction is created by concatenating the positive integers:~%"))
    (newline)
    (display (format #f "0.123456789101112131415161718192021...~%"))
    (newline)
    (display (format #f "It can be seen that the 12th digit of the fractional part is 1.~%"))
    (display (format #f "If dn represents the nth digit of the fractional part, find the value of the following expression.~%"))
    (display (format #f "d1 x d10 x d100 x d1000 x d10000 x d100000 x d1000000~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-find-digit-index-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))

    (display (format #f "this program works using a simple counting method to find the nth~%"))
    (display (format #f "decimal place.  first consider that the first 9 digits go from 1 through 9.~%"))
    (display (format #f "then digit number 10 is the start of number 10, and digit 11 is the second digit of 10.~%"))
    (display (format #f "Since each concatenated number between 10 and 99 contributes 2 digits,~%"))
    (display (format #f "and each number between 100 and 999 contributes 3 digits, and each number~%"))
    (display (format #f "between 1000 and 9999 contribute 4 digits, it's possible to calculate~%"))
    (display (format #f "each digit manually. since there are 90 numbers between 99 and 10 (inclusive),~%"))
    (display (format #f "there are 90*2=180 digits that are added to the decimal (on top of the original 9).~%"))
    (newline)
    (display (format #f "d100: to find the nth digit between index 10 and 189, simply use an interpolating formula,~%"))
    (display (format #f "at index ten you will find the number 10, at index 190 you will find the number 100.~%"))
    (display (format #f "what number will you find at index 12? index 10 + 2*(x-10) = 12 where x is the number to find.~%"))
    (display (format #f "to see that f(x)=10+2*(x-10) is the right formula, examine the end points: f(x=10)=10, and f(x=100)=190~%"))
    (display (format #f "then 2*(x-10)=2, and x = 11.  For index 14, 10 + 2*(x-10) = 14, gives 2*(x-10)=4,~%"))
    (display (format #f "or x=12.  This means that at d12=1, d13=1, d14=1, d15=2.~%"))
    (display (format #f "so to find d100, 10 + 2*(x-10) = 100, 2*(x-10)=90, x=55, and d100=5.~%"))
    (newline)
    (display (format #f "d1000: this same type of formula works for finding d1000, except now the formulas change,~%"))
    (display (format #f "as we enter 3-digit territory.  position 190 is the first occurance of 100, and~%"))
    (display (format #f "2890 is the first occurance of the number 1000. the index formula becomes 190 + 3*(x-100) = 1000~%"))
    (display (format #f "note that difference between the indicies, 190 < 1000 < 2890, and the concatenated numbers~%"))
    (display (format #f "which fall in this range (100 - 999). the interpolation formula is now f(x) = 190 + 3*(x-100).~%"))
    (display (format #f "at the end points, f(x=100)=190, f(x=1000)=2890.~%"))
    (display (format #f "solving for the concatenated number x, 3*(x-100)=810, x-100=270, or x=370, and d1000=3.~%"))
    (newline)
    (display (format #f "d10000: the number 1000 is at index 2890, and the number 10000 is at index 38890.~%"))
    (display (format #f "f(x)=2890+4*(x-1000), f(x=1000)=2890, f(x=10000)=38890. since 2890 < 10,000 < 38,890,~%"))
    (display (format #f "we can use this equation to find the x which gives us the index 10,000, or 2890+4*(x-1000)=10000~%"))
    (display (format #f "4*(x-1000)=7110, or x-1000=1777.5, x=2777.5.  The extra 0.5 means that 10,000 straddles a 4-digit number.~%"))
    (display (format #f "so look at the index 2 before, 2890+4*(x-1000)=9998, 4*(x-1000)=7108, x-1000=1777, x=2777.~%"))
    (display (format #f "therefore the 2 appears at index 9998, 7 appears at 9999, and d10000=7 appears at 10,000.~%"))
    (newline)
    (display (format #f "d100000: the number 10000 is at index 38890, the number 100000 is at index 488,890,~%"))
    (display (format #f "this is a region of 5-digit numbers, f(x)=38890+5*(x-10000), f(x=10000)=38890~%"))
    (display (format #f "f(x=100000)=38890+5*90000=488,890, which implies that we can use this formula for~%"))
    (display (format #f "d100,000. find the x which is at position 100,000: 38890+5*(x-10000)=100,000~%"))
    (display (format #f "5*(x-10000)=61110, x-10000=12222, x=22222, so d100000=2~%"))
    (newline)
    (display (format #f "d1million: the number 100,000 is at index 488,890 and the number 1,000,000 is~%"))
    (display (format #f "at index 5,888,890, a region of 6-digit numbers, so f(x)=488,890+6*(x-100,000)~%"))
    (display (format #f "consistency check: f(x=100,000)=488,890, f(x=1,000,000)=488,890+6*900,000=5,888,890.~%"))
    (display (format #f "find the x which is at position 1,000,000: 488,890+6*(x-100,000)=1,000,000~%"))
    (display (format #f "6*(x-100,000)=511,110, (x-100,000)=85185, x=185185, so d1million=1~%"))
    (newline)
    (display (format #f "Solution d(1) x d(10) x d(100) x d(1000) x d(10000) x d(100000) x d(1000000)~%"))
    (display (format #f "= 1x1x5x3x7x2x1 = 210~%"))

    (newline)
    (force-output)

    (time-code
     (begin
       (main-loop)
       ))

    (newline)
    ))
