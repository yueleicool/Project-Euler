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
(define (digit-list-to-number llist)
  (let ((this-sum
	 (srfi-1:fold
	  (lambda (this-elem previous)
	    (+ this-elem (* 10 previous)))
	  0 llist)))
    this-sum
    ))

;;;#############################################################
;;;#############################################################
(define (test-digit-list-to-number-1)
  (let ((sub-name "test-digit-list-to-number-1")
	(test-list
	 (list
	  (list (list 1) 1)
	  (list (list 1 2) 12)
	  (list (list 1 2 3) 123)
	  (list (list 4 5 6 7) 4567)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (digit-list-to-number test-list)))
	       (begin
		 (if (not (equal? shouldbe result))
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
(define (lists-have-same-digits? ll1 ll2)
  (let ((sl1 (sort ll1 <))
	(sl2 (sort ll2 <)))
    (begin
      (equal? sl1 sl2)
      )))

;;;#############################################################
;;;#############################################################
(define (test-lists-have-same-digits-1)
  (let ((sub-name "test-lists-have-same-digits-1")
	(test-list
	 (list
	  (list (list 1 3) (list 3 1) #t)
	  (list (list 1 2 3) (list 3 1 2) #t)
	  (list (list 1 2 3 4) (list 4 1 3 2) #t)
	  (list (list 13 23 43 53 73 83) (list 83 23 43 53 73 13) #t)
	  (list (list 13 23 43 53 73 83) (list 13 23 44 53 73 83) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-l1 (list-ref this-list 0))
		 (test-l2 (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (lists-have-same-digits? test-l1 test-l2)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list 1=~a, list 2=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-l1 test-l2
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (list-to-set-string llist)
  (let ((stmp (string-join
	       (map
		(lambda (num)
		  (ice9-format:format #f "~:d" num))
		llist) ", ")))
    (begin
      (string-append "{ " stmp " }")
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-set-string-1)
  (let ((sub-name "test-list-to-set-string-1")
	(test-list
	 (list
	  (list (list 1) "{ 1 }")
	  (list (list 1 2) "{ 1, 2 }")
	  (list (list 1 2 3) "{ 1, 2, 3 }")
	  (list (list 4 5 6 7) "{ 4, 5, 6, 7 }")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-set-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num max-num status-num)
  (let ((smallest-list (list))
	(smallest-integer max-num)
	(break-flag #f)
	(counter 0))
    (begin
      (do ((ii 1 (+ ii 1)))
	  ((or (> ii max-num)
	       (equal? break-flag #t)))
	(begin
	  (let ((dlist1 (split-digits-list ii))
		(loop-continue-flag #t)
		(tmp-num ii)
		(local-list (list ii)))
	    (begin
	      (do ((jj 0 (1+ jj)))
		  ((or (>= jj 5)
		       (equal? loop-continue-flag #f)))
		(begin
		  (let ((next-tmp-num (+ tmp-num ii)))
		    (let ((dlist2 (split-digits-list next-tmp-num)))
		      (begin
			(if (lists-have-same-digits? dlist1 dlist2)
			    (begin
			      (set! local-list (cons next-tmp-num local-list)))
			    (begin
			      (set! loop-continue-flag #f)
			      (set! local-list (list))
			      ))

			(set! tmp-num next-tmp-num)
			)))
		  ))

	      (if (equal? loop-continue-flag #t)
		  (begin
		    (set! smallest-integer ii)
		    (set! smallest-list local-list)
		    (set! break-flag #t)
		    ))
	      ))

	  (set! counter (1+ counter))
	  (if (zero? (modulo counter status-num))
	      (begin
		(display (ice9-format:format #f "  status ~:d < ~:d : ~a~%"
					     ii max-num
					     (date-time-to-string (srfi-19:current-date))))
		))
	  ))

      (if (equal? break-flag #f)
	  (begin
	    (display (ice9-format:format #f "no sequences found for numbers less than ~:d~%" max-num)))
	  (begin
	    (display (ice9-format:format #f "~:d is the smallest positive integer such that 2x, 3x, 4x, 5, 6x contains the same digits~%"
					 smallest-integer))
	    (display (ice9-format:format #f "digit list = ~a~%"
					 (list-to-set-string (sort smallest-list <))))
	    (force-output)
	    ))
      break-flag
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
    (display (format #f "Problem 052 - It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.~%"))
    (newline)
    (display (format #f "Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-digit-list-to-number-1 counter)
	   (run-test test-lists-have-same-digits-1 counter)
	   (run-test test-list-to-set-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((test-list
	   (list
	    (list 1 1000000 50000)
	    (list 1000000 10000000 1000000)
	    (list 10000000 100000000 10000000)
	    (list 100000000 1000000000 100000000)
	    ))
	  (loop-continue-flag #t))
      (begin
	(for-each
	 (lambda (alist)
	   (begin
	     (let ((start-num (list-ref alist 0))
		   (end-num (list-ref alist 1))
		   (status-num (list-ref alist 2)))
	       (begin
		 (time-code
		  (begin
		    (let ((found-flag (main-loop start-num end-num status-num)))
		      (begin
			(newline)
			(force-output)

			(if (equal? found-flag #t)
			    (begin
			      (set! loop-continue-flag #f)
			      ))
			))
		    ))

		 (if (equal? loop-continue-flag #f)
		     (begin
		       (quit)
		       ))
		 ))
	     )) test-list)
	))

    (newline)
    ))
