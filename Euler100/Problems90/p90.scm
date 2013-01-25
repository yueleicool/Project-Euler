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

;;;### ice-9 receive for the receive function (bind multiple values)
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
(define (are-numbers-in-separate-lists? aa bb cube-list-1 cube-list-2)
  (cond
   ((and
     (not (equal? (member aa cube-list-1) #f))
     (not (equal? (member bb cube-list-2) #f)))
    (begin
      #t
      ))
   ((and
     (not (equal? (member aa cube-list-2) #f))
     (not (equal? (member bb cube-list-1) #f)))
    (begin
      #t
      ))
   (else
    #f
    )))

;;;#############################################################
;;;#############################################################
(define (test-are-numbers-in-separate-lists-1)
  (let ((sub-name "test-are-numbers-in-separate-lists-1")
	(test-list
	 (list
	  (list 1 4 (list 1 2 3) (list 4 5 6) #t)
	  (list 4 1 (list 1 2 3) (list 4 5 6) #t)
	  (list 2 5 (list 1 2 3) (list 4 5 6) #t)
	  (list 3 6 (list 1 2 3) (list 4 5 6) #t)
	  (list 1 2 (list 1 2 3) (list 4 5 6) #f)
	  (list 3 5 (list 1 2 3) (list 4 5 6) #t)
	  (list 4 5 (list 1 2 3) (list 4 5 6) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((aa (list-ref alist 0))
		 (bb (list-ref alist 1))
		 (llist1 (list-ref alist 2))
		 (llist2 (list-ref alist 3))
		 (shouldbe (list-ref alist 4)))
	     (let ((result (are-numbers-in-separate-lists? aa bb llist1 llist2)))
	       (if (not (equal? shouldbe result))
		   (begin
		     (display (format #f "~a : (~a) : error for aa = ~a, bb = ~a, list1 = ~a, list2 = ~a, shouldbe = ~a, result = ~a~%"
				      sub-name test-label-index aa bb llist1 llist2 shouldbe result))
		     (quit)
		     ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))))

;;;#############################################################
;;;#############################################################
;;; digits on one cube { 0, 1, 2, 3, 4, 6, 8 }
;;; digits on the second { 1, 4, 9, 6, 5, 6, 9, 4, 1}
;;; 01 -> 0 and 1 must be separate sets
;;; 04 -> 0 and 4 must be in separate sets
;;; 09 -> 0 and 9 or 6 must be in separate sets
;;; 16 -> 1 and 6 must be separate
;;; 25 -> 2 and 5 must be separate
;;; 36 -> 3 and 6 must be separate
;;; 49 -> 4 and 9 must be separate
;;; 64 -> 4 and 6 must be separate
;;; 81 -> 1 and 8 must be separate
(define (is-valid-list-pair? cube-list-1 cube-list-2)
  (if (and (list? cube-list-1) (>= (length cube-list-1) 6)
	   (list? cube-list-2) (>= (length cube-list-2) 6))
      (begin
        ;;; criteria 1 - each die has a different digit
	(cond
	 ((equal? (are-numbers-in-separate-lists? 0 1 cube-list-1 cube-list-2) #f)
	  (begin
	    #f
	    ))
	 ((equal? (are-numbers-in-separate-lists? 0 4 cube-list-1 cube-list-2) #f)
	  (begin
	    #f
	    ))
	 ((and
	   (equal? (are-numbers-in-separate-lists? 0 9 cube-list-1 cube-list-2) #f)
	   (equal? (are-numbers-in-separate-lists? 0 6 cube-list-1 cube-list-2) #f))
	  (begin
	    #f
	    ))
	 ((and
	   (equal? (are-numbers-in-separate-lists? 1 6 cube-list-1 cube-list-2) #f)
	   (equal? (are-numbers-in-separate-lists? 1 9 cube-list-1 cube-list-2) #f))
	  (begin
	    #f
	    ))
	 ((equal? (are-numbers-in-separate-lists? 2 5 cube-list-1 cube-list-2) #f)
	  (begin
	    #f
	    ))
	 ((and
	   (equal? (are-numbers-in-separate-lists? 3 6 cube-list-1 cube-list-2) #f)
	   (equal? (are-numbers-in-separate-lists? 3 9 cube-list-1 cube-list-2) #f))
	  (begin
	    #f
	    ))
	 ((and
	   (equal? (are-numbers-in-separate-lists? 4 9 cube-list-1 cube-list-2) #f)
	   (equal? (are-numbers-in-separate-lists? 4 6 cube-list-1 cube-list-2) #f))
	  (begin
	    #f
	    ))
	 ((and
	   (equal? (are-numbers-in-separate-lists? 6 4 cube-list-1 cube-list-2) #f)
	   (equal? (are-numbers-in-separate-lists? 9 4 cube-list-1 cube-list-2) #f))
	  (begin
	    #f
	    ))
	 ((equal? (are-numbers-in-separate-lists? 8 1 cube-list-1 cube-list-2) #f)
	  (begin
	    #f
	    ))
	 (else
	  ;;; if we reach this point then everything is ok
	  #t
	  )))
      (begin
	#f
	)))

;;;#############################################################
;;;#############################################################
(define (test-is-valid-list-pair-1)
  (let ((sub-name "test-is-valid-list-pair-1")
	(test-list
	 (list
	  (list (list 0 5 6 7 8 9) (list 1 2 3 4 8 9) #t)
	  (list (list 0 5 6 7 8 9) (list 1 2 3 2 8 9) #f)
	  (list (list 0 5 6 7 8 9) (list 1 2 3 5 8 9) #f)
	  (list (list 0 5 6 7 8 9) (list 7 2 3 4 8 9) #f)
	  (list (list 0 5 6 7 8 4) (list 1 2 3 4 8 9) #t)
	  (list (list 0 5 3 7 8 4) (list 1 2 3 4 8 9) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((llist-1 (list-ref alist 0))
		 (llist-2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (is-valid-list-pair? llist-1 llist-2)))
	       (if (not (equal? shouldbe result))
		   (begin
		     (display (format #f "~a : (~a) : error for llist-1 = ~a, llist-2 = ~a, shouldbe = ~a, result = ~a~%"
				      sub-name test-label-index llist-1 llist-2 shouldbe result))
		     (quit)
		     ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))))

;;;#############################################################
;;;#############################################################
(define (count-label-pairs-lists debug-flag)
  (define (recursive-loop depth max-depth start-1 end-1 start-2 end-2
			  llist-1 llist-2 acc-list)
    (if (>= depth max-depth)
	(begin
	  (if (is-valid-list-pair? llist-1 llist-2)
	      (begin
		(let ((s1-list (sort llist-1 <))
		      (s2-list (sort llist-2 <)))
		  (let ((next1-list (list s1-list s2-list))
			(next2-list (list s2-list s1-list)))
		    (begin
		      (if (and
			   (equal? (member next1-list acc-list) #f)
			   (equal? (member next2-list acc-list) #f))
			  (begin
			    (let ((next-acc-list (cons next1-list acc-list)))
			      (begin
				(if (equal? debug-flag #t)
				    (begin
				      (display
				       (ice9-format:format #f "debug found pair ~a, ~a : number so far = ~:d~%"
							   s1-list s2-list (length next-acc-list)))
				      (force-output)
				      ))
				next-acc-list
				)))
			  (begin
			    acc-list
			    ))
		      ))))
	      (begin
		acc-list
		)))
	(begin
	  (do ((ii start-1 (1+ ii)))
	      ((> ii end-1))
	    (begin
	      (if (equal? (member ii llist-1) #f)
		  (begin
		    (let ((next-list-1 (cons ii llist-1)))
		      (begin
			(do ((jj start-2 (1+ jj)))
			    ((> jj end-2))
			  (begin
			    (if (equal? (member jj llist-2) #f)
				(begin
				  (let ((next-list-2 (cons jj llist-2)))
				    (begin
				      (let ((next-acc-list
					     (recursive-loop (1+ depth) max-depth
							     (1+ ii) end-1
							     (1+ jj) end-2
							     next-list-1 next-list-2
							     acc-list)))
					(begin
					  (set! acc-list next-acc-list)
					  ))
				      ))
				  ))
			    ))
			))
		    ))
	      ))
	  acc-list
	  )))
  (begin
    (let ((total-acc-lists
	   (recursive-loop 0 6 0 9 0 9
			   (list) (list) (list))))
      (begin
	(length total-acc-lists)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop debug-flag)
  (let ((count (count-label-pairs-lists debug-flag)))
    (begin
      (display (ice9-format:format #f "Number of distinct arrangements of the two cubes that allow for all the square numbers (less than 100,) to be displayed = ~:d~%" count))
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
    (display (format #f "Project Euler 90: - Each of the six faces on a cube has a different digit (0 to 9) written on it; the same is done to a second cube. By placing the two cubes side-by-side in different positions we can form a variety of 2-digit numbers.~%"))
    (newline)
    (display (format #f "For example, the square number 64 could be formed:~%"))
    (newline)
    (display (format #f "In fact, by carefully choosing the digits on both cubes it is possible to display all of the square numbers below one-hundred: 01, 04, 09, 16, 25, 36, 49, 64, and 81.~%"))
    (newline)
    (display (format #f "For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on one cube and {1, 2, 3, 4, 8, 9} on the other cube.~%"))
    (newline)
    (display (format #f "However, for this problem we shall allow the 6 or 9 to be turned upside-down so that an arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows for all nine square numbers to be displayed; otherwise it would be impossible to obtain 09.~%"))
    (newline)
    (display (format #f "In determining a distinct arrangement we are interested in the digits on each cube, not the order.~%"))
    (newline)
    (display (format #f "  {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}~%"))
    (display (format #f "  {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}~%"))
    (newline)
    (display (format #f "But because we are allowing 6 and 9 to be reversed, the two distinct sets in the last example both represent the extended set {1, 2, 3, 4, 5, 6, 9} for the purpose of forming 2-digit numbers.~%"))
    (newline)
    (display (format #f "How many distinct arrangements of the two cubes allow for all of the square numbers to be displayed?~%"))
    (newline)
    (display (format #f "The key to solving this problem is to realize that there are just Choose(10, 6) = 210 possibilities for each die, which means that the program does not have to work as hard.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-are-numbers-in-separate-lists-1 counter)
	   (run-test test-is-valid-list-pair-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (newline)
    (display (format #f "Output:~%"))
    (force-output)

    (let ((debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop debug-flag)
	   ))
	))

    (newline)
    ))
