#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

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
(define (next-num-in-seq this-num)
  (if (even? this-num)
      (begin
	(euclidean/ this-num 2))
      (begin
	(+ (* 3 this-num) 1))
      ))

;;;#############################################################
;;;#############################################################
(define (test-next-num-in-seq-1)
  (let ((sub-name "test-next-num-in-seq-1")
	(test-list
	 (list
	  (list 1 4) (list 2 1) (list 3 10) (list 4 2)
	  (list 5 16) (list 6 3) (list 7 22) (list 8 4)
	  (list 9 28) (list 10 5) (list 11 34) (list 12 6)
	  (list 13 40) (list 14 7) (list 15 46) (list 16 8)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (next-num-in-seq test-num)))
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
(define (list-to-seq-string llist)
  (let ((stmp (string-join
	       (map
		(lambda (this-num)
		  (ice9-format:format #f "~:d" this-num))
		llist) " -> ")))
    (begin
      stmp
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-seq-string-1)
  (let ((sub-name "test-list-to-seq-string-1")
	(test-list
	 (list
	  (list (list 1) "1")
	  (list (list 1 2) "1 -> 2")
	  (list (list 1 2 3) "1 -> 2 -> 3")
	  (list (list 4 5 6 7) "4 -> 5 -> 6 -> 7")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((input-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-seq-string input-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index input-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (calc-seq-list start-num previous-htable)
  (define (iter-loop this-num previous-htable acc-list)
    (cond
     ((<= this-num 1)
      (reverse (cons 1 acc-list)))
     ((list? (member this-num acc-list))
      (reverse (cons this-num acc-list)))
     (else
      (begin
	(let ((next-num (next-num-in-seq this-num)))
          (let ((prev-list (hash-ref previous-htable next-num #f)))
            (begin
              (if (not (equal? prev-list #f))
                  (begin
                    (append
                     (reverse acc-list) (list this-num) prev-list))
                  (begin
                    (iter-loop
                     next-num previous-htable (cons this-num acc-list))
                    ))
              )))
        ))
     ))
  (begin
    (let ((result-list
           (iter-loop start-num previous-htable (list))))
      (begin
        (hash-set! previous-htable start-num result-list)
        result-list
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-calc-seq-list-1)
  (let ((sub-name "test-calc-seq-list-1")
	(test-list
	 (list
	  (list 2 (list 2 1))
	  (list 3 (list 3 10 5 16 8 4 2 1))
	  (list 4 (list 4 2 1))
	  (list 5 (list 5 16 8 4 2 1))
	  (list 6 (list 6 3 10 5 16 8 4 2 1))
	  (list 7 (list 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
	  (list 8 (list 8 4 2 1))
	  (list 9 (list 9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
	  (list 10 (list 10 5 16 8 4 2 1))
          (list 11 (list 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
	  ))
        (prev-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (calc-seq-list test-num prev-htable)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : number=~a, shouldbe=~a, result=~a~%"
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
(define (longest-chain-loop max-num)
  (let ((longest-num 0)
	(longest-list (list))
	(longest-len 0)
        (prev-htable (make-hash-table 100)))
  (begin
    (do ((ii 2 (+ ii 1)))
	((> ii max-num))
      (begin
	(let ((this-list (calc-seq-list ii prev-htable)))
	  (let ((this-len (length this-list)))
	    (begin
	      (if (> this-len longest-len)
		  (begin
		    (set! longest-num ii)
		    (set! longest-list this-list)
		    (set! longest-len this-len)
		    ))

	      (if (not (equal? (car (last-pair this-list)) 1))
		  (begin
		    (display (format #f "surprise! ~a circular : ~a : length = ~a~%"
				     ii
				     (list-to-seq-string this-list)
				     this-len))
		    (force-output)
		    ))
	      )))
	))

    (display
     (ice9-format:format
      #f "Longest chain less than ~:d~%" max-num))
    (display (format #f "~a : ~a : length = ~a~%"
		     longest-num
		     (list-to-seq-string longest-list)
		     longest-len))
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
    (display (format #f "Problem 014 - The following iterative sequence is defined for the set of positive integers:~%"))
    (newline)
    (display (format #f "n -> n/2 (n is even)~%"))
    (display (format #f "n -> 3n + 1 (n is odd)~%"))
    (newline)
    (display (format #f "Using the rule above and starting with 13, we generate the following sequence:~%"))
    (display (format #f "13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1~%"))
    (display (format #f "It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.~%"))
    (newline)
    (display (format #f "Which starting number, under one million, produces the longest chain?~%"))
    (display (format #f "NOTE: Once the chain starts the terms are allowed to go above one million.~%"))
    (newline)
    (display (format #f "The key to making this program run fast is to use memoization.~%"))
    (display (format #f "See http://community.schemewiki.org/?memoization~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-next-num-in-seq-1 counter)
	   (run-test test-list-to-seq-string-1 counter)
	   (run-test test-calc-seq-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 13)
          (prev-htable (make-hash-table 100)))
      (let ((result-list (calc-seq-list start-num prev-htable)))
	(let ((rstring (list-to-seq-string result-list)))
          (begin
            (display
             (ice9-format:format
              #f "~:d : ~a : length = ~a~%"
              start-num rstring (length result-list)))
            ))
        ))
    (force-output)

    (newline)
    (let ((max-num 20))
      (begin
	(longest-chain-loop max-num)
	))
    (newline)

    (let ((max-num 100))
      (begin
	(longest-chain-loop max-num)
	))
    (newline)
    (force-output)

    (let ((max-num 1000000))
      (begin
	(time-code
	 (begin
	   (longest-chain-loop max-num)
	   ))
	))

    (newline)
    ))
