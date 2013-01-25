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
;;; define a macro to simplify code
(define-syntax sqrt-cf-iteration
  (syntax-rules ()
    ((sqrt-cf-iteration int-num int-sqrt-num m0 d0 a0
			seen-htable seen-list
			result-list loop-continue-flag)
     (begin
       (let ((mn (- (* d0 a0) m0)))
	 (let ((dn (euclidean/ (- int-num (* mn mn)) d0)))
	   (let ((an (euclidean/ (+ int-sqrt-num mn) dn)))
	     (begin
	       (let ((next-triple (list mn dn an)))
		 (let ((hflag (hash-ref seen-htable next-triple #f)))
		   (begin
		     (if (equal? hflag #f)
			 (begin
			   (hash-set! seen-htable next-triple #t))
			 (begin
			   (set! loop-continue-flag #f)
			   ))
		     (set! m0 mn)
		     (set! d0 dn)
		     (set! a0 an)
		     (set! seen-list (cons next-triple seen-list))
		     (set! result-list (cons an result-list)))
		   ))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; method from
;;; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
(define (sqrt-continued-fraction-list int-num)
  (begin
    (srfi-11:let-values
     (((int-sqrt-num int-sqrt-remainder)
       (exact-integer-sqrt int-num)))
     (begin
       (if (zero? int-sqrt-remainder)
	   (begin
	     #f)
	   (begin
	     (let ((m0 0)
		   (d0 1)
		   (a0 int-sqrt-num)
		   (result-list (list int-sqrt-num))
		   (seen-htable (make-hash-table))
		   (seen-list (list (list 0 1 int-sqrt-num)))
		   (loop-continue-flag #t))
	       (begin
		 (hash-set! seen-htable (list m0 d0 a0) #t)

		 (while
		  (equal? loop-continue-flag #t)
		  (begin
		    (sqrt-cf-iteration int-num int-sqrt-num m0 d0 a0
				       seen-htable seen-list
				       result-list loop-continue-flag)
		    ))

		 (let ((last-triple (list m0 d0 a0))
		       (rseen-list (reverse seen-list)))
		   (let ((llist (member last-triple rseen-list)))
		     (begin
		       (if (list? llist)
			   (begin
			     (let ((period (- (length llist) 1)))
			       (begin
				 (list period (reverse result-list))
				 )))
			   (begin
			     #f
			     ))
		       )))
		 ))
	     ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-sqrt-continued-fraction-list-1)
  (let ((sub-name "test-sqrt-continued-fraction-list-1")
	(test-list
	 (list
	  (list 2 (list 1 (list 1 2 2)))
	  (list 3 (list 2 (list 1 1 2 1)))
	  (list 4 #f)
	  (list 5 (list 1 (list 2 4 4)))
	  (list 6 (list 2 (list 2 2 4 2)))
	  (list 7 (list 4 (list 2 1 1 1 4 1)))
	  (list 8 (list 2 (list 2 1 4 1)))
	  (list 9 #f)
	  (list 10 (list 1 (list 3 6 6)))
	  (list 11 (list 2 (list 3 3 6 3)))
	  (list 12 (list 2 (list 3 2 6 2)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (sqrt-continued-fraction-list test-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : number=~a, shouldbe=~a, result=~a~%"
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
(define (cf-list-to-string alist)
  (let ((alen (length alist)))
    (let ((blist (list-head alist (- alen 1))))
      (let ((cvalue (car blist))
	    (clist (cdr blist)))
	(begin
	  (let ((s1-tmp (string-join
		       (map
			(lambda (anum)
			  (number->string anum)) clist) ", ")))
	    (let ((s2-tmp (string-append
			   (number->string cvalue)
			   " ; (" s1-tmp ")")))
	      (begin
		(string-append "[ " s2-tmp " ]")
		)))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-cf-list-to-string-1)
  (let ((sub-name "test-cf-list-to-string-1")
	(test-list
	 (list
	  (list (list 2 1 1) "[ 2 ; (1) ]")
	  (list (list 1 1 2 1) "[ 1 ; (1, 2) ]")
	  (list (list 2 4 4) "[ 2 ; (4) ]")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (cf-list-to-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
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
(define (main-loop max-num debug-flag)
  (begin
    (let ((ntotals 0)
	  (nsquares 0)
	  (ncontinued 0)
	  (nodds 0))
      (begin
	(do ((ii 2 (+ ii 1)))
	    ((> ii max-num))
	  (begin
	    (set! ntotals (+ ntotals 1))

	    (let ((results-list (sqrt-continued-fraction-list ii)))
	      (if (not (equal? results-list #f))
		  (begin
		    (let ((period (list-ref results-list 0))
			  (cf-list (list-ref results-list 1)))
		      (begin
			(set! ncontinued (+ ncontinued 1))

			(if (equal? debug-flag #t)
			    (begin
			      (display (ice9-format:format
					#f "    sqrt(~:d) = ~a, period length = ~:d~%"
					ii (cf-list-to-string cf-list) period))
			      (force-output)
			      ))
			(if (odd? period)
			    (begin
			    (set! nodds (+ nodds 1))
			    ))
			)))
		  (begin
		    (set! nsquares (+ nsquares 1))
		    )))
	    ))

	(newline)
	(display (ice9-format:format #f "There are ~:d continued fractions with an odd period, out of ~:d continued fractions.  In addition there were ~:d squares, for a total = ~:d (for 2 <= n <= ~:d)~%"
				     nodds ncontinued nsquares ntotals max-num))
	(force-output)
	))
    ))

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
    (display (format #f "Problem 064 - All square roots are periodic when written as continued fractions and can be written in the form:~%"))
    (newline)
    (display (format #f "sqrt(N) = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))~%"))
    (display (format #f "The process can be summarised as follows:~%"))
    (display (format #f "a0=4, 1/(sqrt(23) - 4) = (sqrt(23) + 4)/7 = 1 + (sqrt(23) - 3)/7~%"))
    (display (format #f "a1=1, 7/(sqrt(23) - 3) = (sqrt(23) + 3)/2 = 3 + (sqrt(23) - 3)/2~%"))
    (display (format #f "a2=3, 2/(sqrt(23) - 3) = (sqrt(23) + 3)/7 = 1 + (sqrt(23) - 4)/7~%"))
    (display (format #f "a3=1, 7/(sqrt(23) - 4) = 7*(sqrt(23) + 4)/7 = 8 + (sqrt(23) - 4)~%"))
    (display (format #f "a4=8, 1/(sqrt(23) - 4) = (sqrt(23) + 4)/7 = 1 + (sqrt(23) - 3)/7~%"))
    (display (format #f "a5=1, 7/(sqrt(23) - 3) = (sqrt(23) + 3)/2 = 3 + (sqrt(23) - 3)/2~%"))
    (display (format #f "a6=3, 2/(sqrt(23) - 3) = 2*(sqrt(23) + 4)/14 = 1 + (sqrt(23) - 4)/7~%"))
    (display (format #f "a7=1, 7/(sqrt(23) - 4) = 7*(sqrt(23) + 4)/7 = 8 + (sqrt(23) - 4)~%"))
    (newline)
    (display (format #f "It can be seen that the sequence is repeating. For conciseness, we use the notation 23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.~%"))
    (display (format #f "The first ten continued fraction representations of (irrational) square roots are:~%"))
    (display (format #f " sqrt(2)=[1;(2)], period=1~%"))
    (display (format #f " sqrt(3)=[1;(1,2)], period=2~%"))
    (display (format #f " sqrt(5)=[2;(4)], period=1~%"))
    (display (format #f " sqrt(6)=[2;(2,4)], period=2~%"))
    (display (format #f " sqrt(7)=[2;(1,1,1,4)], period=4~%"))
    (display (format #f " sqrt(8)=[2;(1,4)], period=2~%"))
    (display (format #f " sqrt(10)=[3;(6)], period=1~%"))
    (display (format #f " sqrt(11)=[3;(3,6)], period=2~%"))
    (display (format #f " sqrt(12)= [3;(2,6)], period=2~%"))
    (display (format #f " sqrt(13)=[3;(1,1,1,1,6)], period=5~%"))
    (newline)
    (display (format #f "Exactly four continued fractions, for N <= 13, have an odd period.~%"))
    (newline)
    (display (format #f "How many continued fractions for N <= 10000 have an odd period?~%"))
    (newline)
    (display (format #f "Solved this problem using the iterative algorithm described at http://en.wikipedia.org/wiki/Methods_of_computing_square_roots~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-sqrt-continued-fraction-list-1 counter)
	   (run-test test-cf-list-to-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 13)
	  (debug-flag #t))
      (begin
	(main-loop max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 10000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)

    ))
