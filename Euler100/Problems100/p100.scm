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

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

;;;### getopt-long used for command-line option arguments processing
(use-modules ((ice-9 getopt-long)
	      :renamer (symbol-prefix-proc 'ice-9-getopt:)))

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
(define (prime? nn)
  (define (smallest-divisor nn test-divisor max-divisor)
    (cond
     ((> test-divisor max-divisor) nn)
     ((zero? (modulo nn test-divisor)) test-divisor)
     (else
      (smallest-divisor nn (+ test-divisor 2) max-divisor)
      )))
  (begin
    (cond
     ((<= nn 1) #f)
     ((= nn 2) #t)
     ((zero? (modulo nn 2)) #f)
     (else
      (let ((max-divisor
	     (+ (exact-integer-sqrt nn) 1)))
	(= nn (smallest-divisor nn 3 max-divisor)))
      ))))

;;;#############################################################
;;;#############################################################
(define (test-prime-1)
  (let ((sub-name "test-prime-1")
	(test-list
	 (list
	  (list 0 #f) (list 1 #f) (list 2 #t) (list 3 #t)
	  (list 4 #f) (list 5 #t) (list 6 #f) (list 7 #t)
	  (list 8 #f) (list 9 #f) (list 10 #f) (list 11 #t)
	  (list 12 #f) (list 13 #t) (list 14 #f) (list 15 #f)
	  (list 16 #f) (list 17 #t) (list 18 #f) (list 19 #t)
	  (list 20 #f) (list 21 #f) (list 22 #f) (list 23 #t)
	  (list 24 #f) (list 25 #f) (list 26 #f) (list 27 #f)
	  (list 28 #f) (list 29 #t) (list 30 #f) (list 31 #t)
	  (list 32 #f) (list 33 #f) (list 34 #f) (list 35 #f)
	  (list 36 #f) (list 37 #t) (list 38 #f) (list 39 #f)
	  (list 40 #f) (list 41 #t) (list 42 #f) (list 43 #t)
	  (list 44 #f) (list 45 #f) (list 46 #f) (list 47 #t)
	  (list 48 #f) (list 49 #f) (list 50 #f) (list 51 #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (let ((test-num (list-ref this-list 0))
	       (test-shouldbe (list-ref this-list 1)))
	   (let ((result (prime? test-num)))
	     (begin
	       (if (not (equal? test-shouldbe result))
		   (begin
		     (display (format #f "~a : error (~a) : num=~a, prime? should be=~a, result=~a~%"
				      sub-name test-label-index test-num test-shouldbe result))
		     (quit)
		     ))
	       (set! test-label-index (1+ test-label-index))
	       ))))
       test-list))))

;;;#############################################################
;;;#############################################################
(define (prime-factor-list input-number)
  (define (local-divide-all-factors this-num this-factor)
    (let ((ll-num this-num)
	  (acc-list (list)))
      (while
       (or (zero? (modulo ll-num this-factor)) (< ll-num 1))
       (begin
	 (set! ll-num (/ ll-num this-factor))
	 (set! acc-list (cons this-factor acc-list))
	 ))
      (list ll-num acc-list)))
  (begin
    (cond
     ((<= input-number 1) (list))
     (else
      (let ((result-list (list))
	    (constant-number input-number)
	    (ll-num input-number)
	    (ll-max (+ 1 (exact-integer-sqrt input-number))))
	(begin
	  (do ((ii 2 (+ ii 1)))
	      ((or (>= ii ll-max) (<= ll-num 1)))
	    (begin
	      (if (zero? (modulo constant-number ii))
		  (begin
		    (if (prime? ii)
			(let ((partial-list (local-divide-all-factors ll-num ii)))
			  (begin
			    (set! ll-num (car partial-list))
			    (set! result-list (append result-list (cadr partial-list)))
			    )))

		    (let ((this-divisor (quotient constant-number ii)))
		      (if (and (> this-divisor ii)
			       (equal? (memq this-divisor result-list) #f)
			       (prime? this-divisor))
			  (let ((partial-list (local-divide-all-factors ll-num this-divisor)))
			    (begin
			      (set! ll-num (car partial-list))
			      (set! result-list (append result-list (cadr partial-list)))
			      ))))
		    ))
	      ))

	  (if (< (length result-list) 1)
	      (list input-number)
	      (sort result-list <)
	      )))))))

;;;#############################################################
;;;#############################################################
(define (test-prime-factor-list-1)
  (let ((sub-name "test-prime-factor-list-1")
	(test-list
	 (list
	  (list 2 (list 2)) (list 3 (list 3)) (list 4 (list 2 2))
	  (list 5 (list 5)) (list 6 (list 2 3)) (list 7 (list 7))
	  (list 8 (list 2 2 2)) (list 9 (list 3 3)) (list 10 (list 2 5))
	  (list 11 (list 11)) (list 12 (list 2 2 3)) (list 13 (list 13))
	  (list 14 (list 2 7)) (list 15 (list 3 5)) (list 16 (list 2 2 2 2))
	  (list 17 (list 17)) (list 18 (list 2 3 3)) (list 19 (list 19))
	  (list 20 (list 2 2 5)) (list 21 (list 3 7)) (list 22 (list 2 11))
	  (list 100 (list 2 2 5 5))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (prime-factor-list test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error for test num = ~a, shouldbe = ~a, result = ~a~%"
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
(define (display-results bb rr)
  (if (and (> bb 0) (> rr 0))
      (begin
	(let ((total (+ bb rr))
	      (bm1 (- bb 1))
	      (tm1 (- (+ bb rr) 1)))
	  (let ((bfactors (prime-factor-list bb))
		(tfactors (prime-factor-list total))
		(bm1factors (prime-factor-list bm1))
		(tm1factors (prime-factor-list tm1))
		(term1 (* 2 bb bm1))
		(term2 (* total tm1))
		(half (/ 1 2)))
	    (begin
	      (display (ice9-format:format #f "blue = ~:d, red = ~:d, total = ~:d~%"
					   bb rr total))
	      (if (= term1 term2)
		  (begin
		    (display (ice9-format:format #f "  prob(bb) = ~a = (~:d / ~:d) * (~:d / ~:d)~%"
						 half bb total bm1 tm1)))
		  (begin
		    (display (ice9-format:format #f "  prob(bb) = ~a = (~:d / ~:d) * (~:d / ~:d)~%"
						 (/ term1 (* term2 2))
						 bb total bm1 tm1))
		    ))
	      (display (ice9-format:format #f "  ~:d -> ~a  :  ~:d -> ~a~%"
					   bb bfactors bm1 bm1factors))
	      (display (ice9-format:format #f "  ~:d -> ~a  :  ~:d -> ~a~%"
					   total tfactors tm1 tm1factors))
	      (newline)
	      )))
	)))

;;;#############################################################
;;;#############################################################
(define-syntax inner-loop-process
  (syntax-rules ()
    ((inner-loop-process xx yy bb-times-2 tt-times-2
			 maximum-total max-bb max-rr
			 loop-continue-flag debug-flag)
     (begin
       (if (and (odd? xx) (odd? yy))
	   (begin
	     (let ((bb (euclidean/ bb-times-2 2))
		   (tt (euclidean/ tt-times-2 2)))
	       (begin
		 (if (> bb max-bb)
		     (begin
		       (set! max-bb bb)
		       (set! max-rr (- tt bb))
		       ))

		 (if (equal? debug-flag #t)
		     (begin
		       (display-results max-bb max-rr)
		       (force-output)
		       ))

		 (if (> tt maximum-total)
		     (begin
		       (set! loop-continue-flag #f)
		       ))
		 ))
	     ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop maximum-total debug-flag)
  (let ((loop-continue-flag #t)
	(xx 1)
	(yy 1)
	(max-bb 0)
	(max-rr 0)
	(final-max-bb 0)
	(final-max-rr 0))
    (begin
      (while
       (equal? loop-continue-flag #t)
       (begin
	 (let ((bb-times-2 (+ yy 1))
	       (tt-times-2 (+ xx 1)))
	   (begin
	     (inner-loop-process xx yy bb-times-2 tt-times-2
				 maximum-total max-bb max-rr
				 loop-continue-flag debug-flag)
	     ))

	 (let ((next-xx (+ xx (* 2 yy)))
	       (next-yy (+ xx yy)))
	   (begin
	     (set! xx next-xx)
	     (set! yy next-yy)
	     ))
	 ))

      (display (ice9-format:format #f "the first results to exceed ~:d total discs is:~%" maximum-total))
      (display-results max-bb max-rr)
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
    (display (format #f "Project Euler 100 - If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, and two discs were taken at random, it can be seen that the probability of taking two blue discs, P(BB) = (15/21)(14/20) = 1/2.~%"))
    (newline)
    (display (format #f "The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, is a box containing eighty-five blue discs and thirty-five red discs.~%"))
    (newline)
    (display (format #f "By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, determine the number of blue discs that the box would contain.~%"))
    (newline)
    (display (format #f "Let b denote number of blue discs, r is the number of red discs, and t is the total number of discs (t = b + r), then prob(BB) = (b/t)*((b-1)/(t-1)).~%"))
    (newline)
    (display (format #f "Since we want P(BB) = 1/2 = (b/t)*((b-1)/(t-1)), we can simplify this equation to 2b^2 - 2b = t^2 - t.~%"))
    (newline)
    (display (format #f "rearranging the equation leads to (2t - 1)^2 - 2(2b - 1)^2 = -1, and if we substitute x=2t-1, y=2b-1, we get Pell's equation, x^2 - 2y^2 = -1~%"))
    (newline)
    (display (format #f "The solution for Pell's equation is at http://www.math.ou.edu/~~kmartin/nti/chap5.pdf~%"))
    (newline)
    (display (format #f "which is, for x and y, (x + y*sqrt(2)) = (1 + sqrt(2))^m, m>0.~%"))
    (newline)
    (display (format #f "then b = (y + 1)/2, and t = (x + 1)/2~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-prime-factor-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-total 1000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-total debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-total 1000000000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-total debug-flag)
	   ))
	))

    (newline)
    ))
