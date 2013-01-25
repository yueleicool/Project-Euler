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
(define (apply-squared-digits this-num)
  (let ((current-list (split-digits-list this-num)))
    (let ((next-num
	   (srfi-1:fold
	    + 0
	    (map
	     (lambda (this-digit)
	       (* this-digit this-digit)) current-list))))
      (begin
	next-num
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-apply-squared-digits-1)
  (let ((sub-name "test-apply-squared-digits-1")
	(test-list
	 (list
	  (list 44 32) (list 32 13) (list 13 10)
	  (list 10 1) (list 1 1) (list 85 89)
	  (list 89 145) (list 42 20) (list 20 4)
	  (list 4 16) (list 16 37) (list 37 58)
	  (list 58 89)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (apply-squared-digits num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num
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
(define (make-number-chain start-num target-1 target-2 memo-htable)
  (cond
   ((<= start-num 0) (list 0))
   (else
    (let ((local-num start-num)
	  (chain-list (list start-num))
	  (loop-continue-flag #t))
      (begin
	(while
	 (equal? loop-continue-flag #t)
	 (begin
	   (let ((next-num (hash-ref memo-htable local-num #f)))
	     (begin
	       (if (equal? next-num #f)
		   (begin
		     (let ((calc-num (apply-squared-digits local-num)))
		       (begin
			 (set! next-num calc-num)
			 (hash-set! memo-htable local-num calc-num)
			 ))
		     ))

	       (if (equal? (member next-num chain-list) #f)
		   (begin
		     (set! chain-list (cons next-num chain-list)))
		   (begin
		     (if (or (equal? next-num target-1)
			     (equal? next-num target-2))
			 (begin
			   (set! chain-list (cons next-num chain-list))
			   (set! loop-continue-flag #f)
			   ))
		     ))

	       (set! local-num next-num)
	       ))
	   ))
	(reverse chain-list)
	))
    )))

;;;#############################################################
;;;#############################################################
(define (test-make-number-chain-1)
  (let ((sub-name "test-make-number-chain-1")
	(test-list
	 (list
	  (list 44 (list 44 32 13 10 1 1))
	  (list 85 (list 85 89 145 42 20 4 16 37 58 89))
	  ))
	(target-1 1)
	(target-2 89)
	(memo-htable (make-hash-table 100))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (make-number-chain test-num target-1 target-2 memo-htable)))
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
(define (populate-2nd-opt-hash! terminating-htable max-pop target-1 target-2)
  (let ((memo-htable (make-hash-table 100)))
    (begin
      (hash-clear! terminating-htable)

      (do ((ii 0 (1+ ii)))
	  ((> ii max-pop))
	(begin
	  (let ((ii-tmp (hash-ref terminating-htable ii #f)))
	    (begin
	      (if (equal? ii-tmp #f)
		  (begin
		    (let ((tlist (make-number-chain ii target-1 target-2 memo-htable)))
		      (begin
			(let ((last-num (car (last-pair tlist))))
			  (begin
			    (hash-set! terminating-htable ii last-num)
			    ))
			))
		    ))
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (first-two-optimizations-loop start-num end-num max-pop target-1 target-2)
  (let ((target-counter 0)
	(terminating-htable (make-hash-table 600)))
    (begin
      (display (format #f "first two optimizations (store terminating results for ~a numbers in a hash table)~%" max-pop))
      (force-output)
      ;;; initialize the loop
      (populate-2nd-opt-hash! terminating-htable max-pop target-1 target-2)

      (do ((ii start-num (1+ ii)))
	  ((> ii end-num))
	(begin
	  (let ((tnum (apply-squared-digits ii)))
	    (let ((terminal-number (hash-ref terminating-htable tnum #f)))
	      (begin
		(if (equal? terminal-number #f)
		    (begin
		      (display (format #f "first-two-optimizations-loop error : ii=~a, tnum=~a, no matching terminal number!  exiting program...~%"
				       ii tnum))
		      (force-output)
		      (quit))
		    (begin
		      (if (equal? terminal-number target-2)
			  (begin
			    (set! target-counter (1+ target-counter))
			    ))
		      ))
		)))
	  ))

      (display (ice9-format:format #f "Number of chains that terminate at ~:d, between ~:d and ~:d is ~:d~%"
				   target-2 start-num end-num target-counter))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
(define (factorial ii-num)
  (begin
    (cond
     ((<= ii-num 1) 1)
     ((= ii-num 2) 2)
     ((= ii-num 3) 6)
     ((= ii-num 4) 24)
     ((= ii-num 5) 120)
     ((= ii-num 6) 720)
     ((= ii-num 7) 5040)
     ((= ii-num 8) 40320)
     ((= ii-num 9) 362880)
     (else
      (* ii-num (factorial (- ii-num 1)))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-factorial-1)
  (let ((sub-name "test-factorial-1")
	(test-list
	 (list
	  (list 0 1) (list 1 1) (list 2 2)
	  (list 3 6) (list 4 24) (list 5 120)
	  (list 6 720) (list 7 5040)
	  (list 8 40320) (list 9 362880)
	  (list 10 3628800)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (factorial test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (calc-combinations digit-list)
  (let ((d-htable (make-hash-table 10))
	(d-count 0))
    (begin
      (for-each
       (lambda (digit)
	 (begin
	   (let ((this-count (hash-ref d-htable digit 0)))
	     (begin
	       (hash-set! d-htable digit (1+ this-count))
	       (set! d-count (1+ d-count))
	       ))
	   )) digit-list)
      (let ((permutations (factorial d-count)))
	(begin
	  (hash-for-each
	   (lambda (digit d-count)
	     (begin
	       (let ((rr (factorial d-count)))
		 (begin
		   (set! permutations (euclidean/ permutations rr))
		   ))
	       )) d-htable)

	  permutations
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-combinations-1)
  (let ((sub-name "test-calc-combinations-1")
	(test-list
	 (list
	  (list (list 1 1) 1) (list (list 1 2) 2)
	  (list (list 2 2) 1) (list (list 1 2 3) 6)
	  (list (list 1 1 2) 3) (list (list 1 1 2 2) 6)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (calc-combinations test-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : list = ~a, shouldbe = ~a, result = ~a~%"
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
;;; define a macro to simplify code
(define-syntax update-terminating-hashes
  (syntax-rules ()
    ((update-terminating-hashes tlist terminating-htable perm-htable)
     (begin
       (let ((last-num (car (last-pair tlist))))
	 (begin
	   (for-each
	    (lambda (a-num)
	      (begin
		(let ((a-list (sort (split-digits-list a-num) <)))
		  (begin
		    (hash-set! terminating-htable a-list last-num)
		    (let ((perm-count (calc-combinations a-list)))
		      (begin
			(hash-set! perm-htable a-list perm-count)
			))
		    ))
		)) tlist)
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (non-decreasing-list-list start-num end-num max-depth)
  (define (local-loop depth max-depth current-max end-num
		      current-list acc-list)
    (cond
     ((>= depth max-depth)
      (begin
	(let ((s-list (sort current-list <)))
	  (begin
	    (if (equal? (member s-list acc-list))
		(begin
		  (cons s-list acc-list))
		(begin
		  acc-list
		  ))
	    ))
	))
     (else
      (do ((ii current-max (1+ ii)))
	  ((> ii end-num))
	(begin
	  (let ((this-list (cons ii current-list)))
	    (let ((next-max (srfi-1:fold max ii this-list)))
	      (let ((next-acc-list
		     (local-loop (1+ depth) max-depth next-max end-num this-list acc-list)))
		(begin
		  (set! acc-list next-acc-list)
		  ))
	      ))
	  ))
      acc-list
      )))
  (let ((rlist (local-loop 0 max-depth start-num end-num (list) (list))))
    (begin
      (reverse rlist)
      )))

;;;#############################################################
;;;#############################################################
(define (test-non-decreasing-list-list-1)
  (let ((sub-name "test-non-decreasing-list-list-1")
	(test-list
	 (list
	  (list 0 2 1 (list (list 0) (list 1) (list 2)))
	  (list 2 3 2 (list (list 2 2) (list 2 3) (list 3 3)))
	  (list 2 3 3 (list (list 2 2 2) (list 2 2 3) (list 2 3 3)
			    (list 3 3 3)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((start-num (list-ref alist 0))
		 (end-num (list-ref alist 1))
		 (max-depth (list-ref alist 2))
		 (shouldbe (list-ref alist 3)))
	     (let ((result (non-decreasing-list-list start-num end-num max-depth)))
	       (let ((slen (length shouldbe))
		     (rlen (length result)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : (~a) : error : start num = ~a, end num = ~a, max-depth = ~a, shouldbe = ~a, result = ~a : lengths differ, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index start-num end-num max-depth
					  shouldbe result slen rlen))
			 (quit)
			 ))
		   (for-each
		    (lambda (slist)
		      (begin
			(if (equal? (member slist result) #f)
			    (begin
			      (display (format #f "~a : (~a) : error : start num = ~a, end num = ~a, max-depth = ~a, shouldbe = ~a, result = ~a, missing shouldbe = ~a~%"
					  sub-name test-label-index start-num end-num max-depth
					  shouldbe result slist))
			      (quit)
			      ))
			)) shouldbe)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (populate-3rd-opt-hash! terminating-htable perm-htable
				max-depth target-1 target-2)
  (let ((memo-htable (make-hash-table 1000))
	(non-list-list (non-decreasing-list-list 0 9 max-depth)))
    (begin
      (hash-clear! terminating-htable)
      (hash-clear! perm-htable)

      (for-each
       (lambda (dlist)
	 (begin
	   (let ((ii-tmp (hash-ref terminating-htable dlist #f)))
	     (begin
	       (if (equal? ii-tmp #f)
		   (begin
		     (let ((dnum (srfi-1:fold (lambda (num prev)
						(+ num (* prev 10))) 0 dlist)))
		       (let ((tlist (make-number-chain dnum target-1 target-2 memo-htable)))
			 (let ((last-num (car (last-pair tlist))))
			   (begin
			     (hash-set! terminating-htable dlist last-num)

			     (let ((perm-count (calc-combinations dlist)))
			       (begin
				 (hash-set! perm-htable dlist perm-count)
				 ))
			     ))
			 ))
		     ))
	       ))
	   )) non-list-list)
      )))

;;;#############################################################
;;;#############################################################
;;; all permutations of digits have the same sum of squares of their digits
(define (third-optimization-loop start-num end-num max-depth target-1 target-2)
  (let ((total 0)
	(terminating-htable (make-hash-table 1000))
	(perm-htable (make-hash-table 1000)))
    (begin
      (display (format #f "digit list optimization (store sorted digit-list/terminating results in a hash table)~%"))
      (force-output)

      (populate-3rd-opt-hash! terminating-htable perm-htable max-depth target-1 target-2)

      (hash-for-each
       (lambda (dlist terminating-number)
	 (begin
	   (if (equal? terminating-number target-2)
	       (begin
		 (let ((perm-count (hash-ref perm-htable dlist 0)))
		   (begin
		     (set! total (+ total perm-count))
		     ))
		))
	    )) terminating-htable)

      (display (ice9-format:format #f "Number of chains that terminate at ~:d, between ~:d and ~:d is ~:d~%"
				   target-2 start-num end-num total))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
(define (list-to-string llist separator)
  (let ((result-string
	 (string-join
	  (map
	   (lambda (num)
	     (ice9-format:format #f "~:d" num)) llist)
	  separator)))
    (let ((final-string (string-append "{ " result-string " }")))
      (begin
	final-string
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-list-to-string-1)
  (let ((sub-name "test-list-to-string-1")
	(test-list
	 (list
	  (list (list 1 1) ", " "{ 1, 1 }")
	  (list (list 1 2) " -> " "{ 1 -> 2 }")
	  (list (list 1 2 3) " -> " "{ 1 -> 2 -> 3 }")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-list (list-ref alist 0))
		 (separator (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (list-to-string num-list separator)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : list = ~a, separator = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num-list separator
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
;;; all permutations of digits have the same sum of squares of their digits
(define (brute-force-loop start-num end-num target-1 target-2 debug-flag)
  (let ((target-counter 0)
	(memo-htable (make-hash-table 1000)))
    (begin
      (display (format #f "brute force method~%"))
      (force-output)

      (do ((ii start-num (1+ ii)))
	  ((> ii end-num))
	(begin
	  (let ((tlist (make-number-chain ii target-1 target-2 memo-htable)))
	    (begin
	      (let ((last-num (car (last-pair tlist))))
		(begin
		  (if (equal? last-num target-2)
		      (begin
			(set! target-counter (1+ target-counter))

			(if (equal? debug-flag #t)
			    (begin
			      (let ((lstring (list-to-string tlist " -> "))
				    (lcount (length tlist)))
				(begin
				  (display (ice9-format:format #f "(~:d) : ~a : sequence length = ~:d~%"
							       ii lstring lcount))
				  (force-output)
				  ))
			      ))
			))
		  ))
	      ))
	  ))

      (display (ice9-format:format #f "Number of chains that terminate at ~:d, between ~:d and ~:d is ~:d~%"
				   target-2 start-num end-num target-counter))
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
    (display (format #f "Project Euler 92 - A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.~%"))
    (newline)
    (display (format #f "For example,~%"))
    (display (format #f "  44 -> 32 -> 13 -> 10 -> 1 -> 1~%"))
    (display (format #f "  85 -> 89 -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> 89~%"))
    (newline)
    (display (format #f "Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.~%"))
    (newline)
    (display (format #f "How many starting numbers below ten million will arrive at 89?~%"))
    (newline)
    (display (format #f "The solution follows the clever ideas found at http://tafakuri.net/?p=77~%"))
    (newline)
    (display (format #f "There are three different algorithms, a brute force solution (not used), optimizations 1&2 from the above article, and a partial implementation of the third optimization method (storing sorted digits into a hash).~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-apply-squared-digits-1 counter)
	   (run-test test-make-number-chain-1 counter)
	   (run-test test-factorial-1 counter)
	   (run-test test-calc-combinations-1 counter)
	   (run-test test-non-decreasing-list-list-1 counter)
	   (run-test test-list-to-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 1)
	  (end-num 10)
	  (target-1 1)
	  (target-2 89)
	  (debug-flag #t))
      (begin
	(brute-force-loop start-num end-num target-1 target-2 debug-flag)
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (end-num 10)
	  (max-pop 81)
	  (max-depth 2)
	  (target-1 1)
	  (target-2 89)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (first-two-optimizations-loop start-num end-num max-pop target-1 target-2)
	   ))
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (end-num 10)
	  (max-depth 1)
	  (target-1 1)
	  (target-2 89)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (third-optimization-loop start-num end-num max-depth target-1 target-2)
	   ))
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (end-num 10000000)
	  (max-pop 567)
	  (target-1 1)
	  (target-2 89))
      (begin
	(time-code
	 (begin
	   (first-two-optimizations-loop start-num end-num max-pop target-1 target-2)
	   ))
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (end-num 10000000)
	  (max-depth 7)
	  (target-1 1)
	  (target-2 89))
      (begin
	(time-code
	 (begin
	   (third-optimization-loop start-num end-num max-depth target-1 target-2)
	   ))
	))

    (newline)
    ))
