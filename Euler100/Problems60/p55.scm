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
(define (palindrome? nn base)
  (cond
   ((< nn 0) #f)
   (else
    (begin
      (let ((tstring (number->string nn base)))
	(let ((rstring (string-reverse tstring)))
	  (string-ci=? tstring rstring)
	  ))))
   ))

;;;#############################################################
;;;#############################################################
(define (test-palindrome-1)
  (let ((sub-name "test-palindrome-1")
	(test-list
	 (list
	  (list 3 10 #t) (list 3 2 #t)
	  (list 7 10 #t) (list 7 2 #t)
	  (list 10 10 #f) (list 11 10 #t) (list 12 10 #f) (list 13 10 #f)
	  (list 20 10 #f) (list 21 10 #f) (list 22 10 #t) (list 23 10 #f)
	  (list 313 10 #t) (list 323 10 #t) (list 333 10 #t) (list 334 10 #f)
	  (list 585 10 #t) (list 585 2 #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (test-base (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (palindrome? test-num test-base)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : num=~a, base=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-num test-base
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
(define (lychrel-number nn max-num)
  (let ((result-list (list))
	(this-num nn)
	(break-flag #f))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((or (> ii max-num)
	       (equal? break-flag #t)))
	(begin
	  (let ((this-list (split-digits-list this-num)))
	    (let ((rlist (reverse this-list)))
	      (let ((rnum (turn-digit-list-to-number rlist)))
		(let ((sum (+ this-num rnum)))
		  (begin
		    (set! result-list (cons (list this-num rnum sum) result-list))

		    (if (palindrome? sum 10)
			(begin
			  (set! break-flag #t))
			(begin
			  (set! this-num sum)))
		    )))))
	  ))
      (if (equal? break-flag #t)
	  (begin
	    (list #f (reverse result-list)))
	  (begin
	    (list #t (reverse result-list))
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-lychrel-number-1)
  (let ((sub-name "test-lychrel-number-1")
	(test-list
	 (list
	  (list 47 (list #f (list (list 47 74 121))))
	  (list 349 (list #f
			  (list (list 349 943 1292)
				(list 1292 2921 4213)
				(list 4213 3124 7337))))
	  ))
	(max-num 50)
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (lychrel-number test-num max-num)))
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
(define (main-loop start-num max-num max-iterations debug-flag)
  (begin
    (let ((counter 0))
      (begin
	(do ((ii start-num (+ ii 1)))
	    ((> ii max-num))
	  (begin
	    (let ((this-list-list (lychrel-number ii max-iterations)))
	      (let ((lychrel-flag (list-ref this-list-list 0))
		    (lychrel-list (list-ref this-list-list 1)))
		(begin
		  (if (equal? lychrel-flag #t)
		      (begin
			(set! counter (+ counter 1))
			(if (equal? debug-flag #t)
			    (begin
			      (display (format #f "(~a) lychrel number found! ~a number of iteratios = ~a~%"
					       counter ii (length lychrel-list)))
			      ))
			))
		  )))
	    ))

	(newline)
	(display (ice9-format:format #f "There are ~:d lychrel numbers between = ~:d and ~:d, max iterations = ~:d~%"
				     counter start-num max-num max-iterations))
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
    (display (format #f "Problem 055 - If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.~%"))
    (newline)
    (display (format #f "Not all numbers produce palindromes so quickly. For example,~%"))
    (display (format #f "  349 + 943 = 1292~%"))
    (display (format #f "  1292 + 2921 = 4213~%"))
    (display (format #f "  4213 + 3124 = 7337~%"))
    (newline)
    (display (format #f "That is, 349 took three iterations to arrive at a palindrome.~%"))
    (newline)
    (display (format #f "Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).~%"))
    (newline)
    (display (format #f "Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.~%"))
    (newline)
    (display (format #f "How many Lychrel numbers are there below ten-thousand?~%"))
    (display (format #f "~%"))
    (newline)
    (display (format #f "NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.~%"))


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-turn-digit-list-to-number-1 counter)
	   (run-test test-palindrome-1 counter)
	   (run-test test-lychrel-number-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 1)
	  (max-num 1000)
	  (max-iterations 50)
	  (debug-flag #t))
      (begin
	(main-loop start-num max-num max-iterations debug-flag)
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (max-num 10000)
	  (max-iterations 50)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop start-num max-num max-iterations debug-flag)
	   ))
	))

    (newline)

    ))
