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
;;; use this code to see the path over the verticies of the grid
(define (rec-down-count max-row max-col debug-flag)
  (define (inner-counter-loop this-row this-col max-row max-col curr-list acc-list)
    (begin
      (cond
       ((and (= this-row max-row) (= this-col max-col))
	(let ((this-count (+ (car acc-list) 1))
	      (next-list (list-ref acc-list 1)))
	  (begin
	    (if (equal? debug-flag #t)
		(begin
		  (display (format #f "  debug this-row/col=(~a/~a), this-count=~a, next-list=~a, curr-list=~a, acc-list=~a~%"
				   this-row this-col this-count next-list curr-list acc-list))
		  ))

	    (list this-count (cons curr-list next-list))
	    )))
       ((or (> this-row max-row) (> this-col max-col))
	(begin
	  (let ((next-acc (inner-counter-loop
			   max-row max-col max-row max-col curr-list acc-list)))
	    next-acc)))
       (else
	(begin
	  (let ((next-row this-row)
		(next-col (+ this-col 1))
		(next-curr-list curr-list)
		(second-row (+ this-row 1))
		(second-col this-col)
		(second-curr-list curr-list))
	    (begin
	      (if (equal? debug-flag #t)
		  (display (format #f "debug this-row/col=~a/~a, next-row/col=~a/~a, second-row/col=~a/~a, curr-list=~a, acc-list=~a~%"
				   this-row this-col next-row next-col second-row second-col curr-list acc-list)))

	      ;;; recursively iterate over first path
	      (cond
	       ((>= next-col max-col)
		(if (< this-row max-row)
		    (begin
		      (do ((ii this-row (+ ii 1)))
			  ((> ii max-row))
			(begin
			  (set! next-curr-list (append next-curr-list (list (list ii max-col))))
			  ))

		      (let ((acc2-list (inner-counter-loop
					max-row max-col max-row max-col
					next-curr-list acc-list)))
			(begin
			  (set! acc-list acc2-list)
			  acc-list)))
		    (begin
		      acc-list)))
	       (else
		(begin
		  (let ((acc2-list (inner-counter-loop
				    next-row next-col max-row max-col
				    (append next-curr-list (list (list next-row next-col)))
				    acc-list)))
		    (set! acc-list acc2-list))
		  acc-list
		  )))

	      ;;; second path
	      (cond
	       ((= second-row max-row)
		(begin
		  (if (not (= second-col next-col))
		      (begin
			(do ((ii second-col (+ ii 1)))
			    ((> ii max-col))
			  (begin
			    (set! second-curr-list (append second-curr-list (list (list max-row ii))))
			    ))
			(let ((acc2-list (inner-counter-loop
					  max-row max-col max-row max-col
					  second-curr-list acc-list)))
			  (set! acc-list acc2-list))
			acc-list)
		      (begin
			acc-list))))
	       ((> second-row max-row)
		acc-list)
	       (else
		(begin
		  (let ((acc2-list (inner-counter-loop
				    second-row second-col max-row max-col
				    (append second-curr-list (list (list second-row second-col)))
				    acc-list)))
		    (set! acc-list acc2-list))
		  acc-list
		  )))

	      )))))))
  (begin
    (inner-counter-loop 0 0 max-row max-col
			(list (list 0 0)) (list 0 (list)))
    ))

;;;#############################################################
;;;#############################################################
(define (dynamic-count max-rows max-cols)
  (let ((end-row-index (1+ max-rows))
	(end-col-index (1+ max-cols)))
    (let ((grid-array (make-array 0 end-row-index end-col-index)))
      (begin
	(do ((ii 0 (1+ ii)))
	    ((>= ii end-row-index))
	  (begin
	    (array-set! grid-array 1 ii 0)
	  ))

      (do ((jj 0 (1+ jj)))
	  ((>= jj end-col-index))
	(begin
	  (array-set! grid-array 1 0 jj)
	  ))

      (do ((ii 1 (1+ ii)))
	  ((>= ii end-row-index))
	(begin
	  (do ((jj 1 (1+ jj)))
	      ((>= jj end-col-index))
	    (begin
	      (let ((prev-1 (array-ref grid-array (- ii 1) jj))
		    (prev-2 (array-ref grid-array ii (- jj 1))))
		(begin
		  (array-set! grid-array (+ prev-1 prev-2) ii jj)
		  ))
	      ))
	  ))

      (let ((result (array-ref grid-array max-rows max-cols)))
	(begin
	  result
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-dynamic-count-1)
  (let ((sub-name "test-dynamic-count-1")
	(test-list
	 (list
	  (list 1 1 2)
	  (list 2 2 6)
	  (list 3 3 20)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((max-rows (list-ref this-list 0))
		 (max-cols (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (dynamic-count max-rows max-cols)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : max-rows=~a, max-cols=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index max-rows max-cols
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
(define (factorial num)
  (cond
   ((< num 0) -1)
   ((= num 0) 1)
   ((= num 1) 1)
   (else
    (let ((result 1))
      (begin
	(do ((ii 2 (1+ ii)))
	    ((> ii num))
	  (begin
	    (set! result (* result ii))
	    ))

	result
	))
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
       (lambda (this-list)
	 (begin
	   (let ((num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (factorial num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : number=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (combinatorial-count max-rows max-cols)
  (let ((total-factorial (factorial (+ max-rows max-cols)))
	(rows-factorial (factorial max-rows))
	(cols-factorial (factorial max-cols)))
    (let ((result (/ total-factorial (* rows-factorial cols-factorial))))
      (begin
	result
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-combinatorial-count-1)
  (let ((sub-name "test-combinatorial-count-1")
	(test-list
	 (list
	  (list 1 1 2)
	  (list 2 2 6)
	  (list 3 3 20)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((max-rows (list-ref this-list 0))
		 (max-cols (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (combinatorial-count max-rows max-cols)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : max-rows=~a, max-cols=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index max-rows max-cols
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
(define (main-loop max-rows max-cols debug-flag)
  (begin
    (if (equal? debug-flag #t)
	(begin
	  (let ((results (rec-down-count max-rows max-cols debug-flag)))
	    (begin
	      (display (ice9-format:format #f "brute-force method: results ~ax~a : number of paths = ~a~%"
					   max-rows max-cols results))
	      ))
	  ))

    (let ((results (dynamic-count max-rows max-cols)))
      (begin
	(display (ice9-format:format #f "dynamic method: results ~ax~a : number of paths = ~:d~%"
				     max-rows max-cols results))
	))

    (let ((results (combinatorial-count max-rows max-cols)))
      (begin
	(display (ice9-format:format #f "combinatorial method: results ~ax~a : number of paths = ~:d~%"
				     max-rows max-cols results))
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
    (display (format #f "Problem 015 - Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.~%"))
    (newline)
    (display (format #f "How many routes are there through a 20x20 grid?~%"))
    (newline)
    (display (format #f "The simplest solution is at: http://www.programmingforums.org/thread39750.html~%"))
    (display (format #f "There are three solutions used, a brute force method, using recursion, a clever method that uses dynamic programming, and a combinatorial method.~%"))
    (newline)
    (display (format #f "The brute force method traverses each and every path, counting each path when it reaches the end node.~%"))
    (newline)
    (display (format #f "The dynamic programming method is much more efficient.  It counts the number of paths to the final node as the sum of the number of ways to reach it's two nearest neighbors.  For example, in the 2x2 grid, the number of ways to reach the bottom right corner (2, 2) is 6, which is the number of ways to reach (1, 2), which is 3, plus the number of ways to reach (2, 1), which is 3.  The number of ways to reach (1, 2), which is 3, is the number of ways to reach (0, 2) which is 1, plus the number of ways to reach (1, 1) which is 2.  The number of ways to reach (2, 1) is similar to calculating the number of ways to reach (1, 2). The number of ways to reach (1, 1), which is 2, is the number of ways to reach (0, 1), which is 1, plus the number of ways to reach (1, 0), which is 1.~%"))
    (newline)
    (display (format #f "The simplest solution was described in the programming forums website as a combinatorial approach.  Every path must make 20 down moves and 20 right moves, in order to reach the bottom right corner. So the total number of moves is 40, and the number of unique permutations of 20 downs and 20 right moves is 40! / (20! x 20!)~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-dynamic-count-1 counter)
	   (run-test test-factorial-1 counter)
	   (run-test test-combinatorial-count-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((test-lists
	   (list
	    (list 1 1 #t)
	    (list 2 2 #f)
	    (list 3 3 #f)
	    (list 4 4 #f)
	    (list 10 10 #f))))
      (begin
	(for-each
	 (lambda (a-list)
	   (begin
	     (let ((max-rows (list-ref a-list 0))
		   (max-cols (list-ref a-list 1))
		   (debug-flag (list-ref a-list 2)))
	       (begin
		 (main-loop max-rows max-cols debug-flag)
		 (newline)
		 (force-output)
		 ))
	     )) test-lists)
	))

    (gc)
    (newline)
    (force-output)
    (let ((max-rows 20)
	  (max-cols 20)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-rows max-cols debug-flag)
	   ))
	))
    (newline)
    ))
