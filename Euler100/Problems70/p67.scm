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

;;;### ice-9 rdelim - for read-delimited
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

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
;;; note: this algorithm expects a triangular list of lists
;;; each list element must be 1 greater than the list element before it
(define (display-triangle triangle-list-list margin)
  (let ((nelements (length triangle-list-list))
	(margin-string (make-string margin #\space))
	(spacer-string "  "))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((>= ii nelements))
	(begin
	  (let ((max-index ii)
		(this-level-list (list-ref triangle-list-list ii))
		(tstring ""))
	    (begin
	      (let ((extra-space (make-string (- nelements ii) #\space)))
		(begin
		  (set! tstring (string-append margin-string extra-space))
		  (for-each
		   (lambda (this-elem)
		     (begin
		       (let ((elem-string (format #f "~a" this-elem)))
			 (set! tstring (string-append tstring elem-string spacer-string))
			 )))
		   this-level-list)
		  (display (format #f "~a~%" (string-trim-right tstring)))
		  ))
	      ))))
      )))

;;;#############################################################
;;;#############################################################
;;; triangle list in the form (list (list elem1) (list elem1 elem2), ...)
(define (dynamic-method triangle-list-list)
  (let ((llen (length triangle-list-list))
	(prev-result-list (list)))
    (cond
     ((<= llen 2) -1)
     (else
      (let ((this-row (list-ref triangle-list-list (- llen 1))))
	(begin
	  (do ((ii (- llen 2) (- ii 1)))
	      ((< ii 0))
	    (begin
	      (let ((prev-row (list-ref triangle-list-list ii))
		    (this-len (length this-row))
		    (previous-list (list)))
		(begin
		  (do ((jj 0 (1+ jj)))
		      ((>= jj (- this-len 1)))
		    (begin
		      (let ((elem1 (list-ref this-row jj))
			    (elem2 (list-ref this-row (+ jj 1)))
			    (prev-elem (list-ref prev-row jj)))
			(let ((max-elem (max elem1 elem2)))
			  (let ((new-prev-elem (+ prev-elem max-elem)))
			    (begin
			      (set! previous-list (cons new-prev-elem previous-list))
			      ))
			  ))
		      ))

		  (set! prev-result-list (reverse previous-list))
		  (set! this-row prev-result-list)
		  ))
	      ))
	  (car prev-result-list)
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-dynamic-method-1)
  (let ((sub-name "test-dynamic-method-1")
	(test-list
	 (list
	  (list (list (list 1) (list 2 3) (list 4 5 6)) 10)
	  (list (list (list 1) (list 3 2) (list 6 5 4)) 10)
	  (list (list (list 3) (list 7 4) (list 2 4 6) (list 8 5 9 3)) 23)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-triangle (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (dynamic-method test-triangle)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : triangle list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-triangle
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
(define (list-to-sum-string llist)
  (let ((stmp (string-join
	       (map
		(lambda (num)
		  (ice9-format:format #f "~:d" num))
		llist) " + ")))
    (begin
      stmp
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-sum-string-1)
  (let ((sub-name "test-list-to-sum-string-1")
	(test-list
	 (list
	  (list (list 1) "1")
	  (list (list 1 2) "1 + 2")
	  (list (list 1 2 3) "1 + 2 + 3")
	  (list (list 4 5 6 7) "4 + 5 + 6 + 7")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-sum-string test-list)))
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
(define (read-in-triangle filename)
  (let ((result-list (list)))
    (begin
      (if (file-exists? filename)
	  (begin
	    (with-input-from-file filename
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited ",\n\r") (ice9-rdelim:read-delimited ",\n\r")))
		    ((eof-object? line))
		  (begin
		    (if (not (eof-object? line))
			(begin
			  (let ((tmp-line (string-copy line)))
			    (let ((this-string-list (string-split tmp-line #\space)))
			      (let ((num-list (list)))
				(begin
				  (for-each
				   (lambda(this-num)
				     (if (not (equal? (string->number this-num) #f))
					 (begin
					   (set! num-list (cons (string->number this-num) num-list)))))
				   this-string-list)

				  (if (> (length num-list) 0)
				      (set! result-list (cons (reverse num-list) result-list)))
				  ))))))
		    ))
		))))
      (reverse result-list)
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
(define (main-loop triangle-list-list debug-flag)
  (begin
    (if (equal? debug-flag #t)
	(begin
	  (let ((nsize (length triangle-list-list)))
	      (begin
		(display-triangle triangle-list-list nsize)
		(force-output)
		))
	  ))

    (let ((largest-sum (dynamic-method triangle-list-list)))
      (begin
	(display (ice9-format:format #f "the largest sum = ~:d  (dynamic program method)~%"
				     largest-sum))
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
(define (main args)
  (begin
    (display (format #f "Problem 067 - By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.~%"))
    (newline)
    (display (format #f "     3~%"))
    (display (format #f "    7 4~%"))
    (display (format #f "   2 4 6~%"))
    (display (format #f "  8 5 9 3~%"))
    (newline)
    (display (format #f "That is, 3 + 7 + 4 + 9 = 23.~%"))
    (newline)
    (display (format #f "Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.~%"))
    (newline)
    (display (format #f "NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 2^99 altogether! If you could check one trillion (10^12) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)~%"))
    (newline)
    (display (format #f "The description of the solution can be found at http://www.mathblog.dk/project-euler-67-efficient-algorithm-triangle/~%"))
    (display (format #f "Kristian Edlund describes a dynamic programming method which is superior to the brute force method, and solves the problem by working from the bottom row and working back up.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-dynamic-method-1 counter)
	   (run-test test-list-to-sum-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((list1 (list (list 3) (list 7 4) (list 2 4 6) (list 8 5 9 3)))
	  (debug-flag #t))
      (begin
        (time-code
         (begin
           (main-loop list1 debug-flag)
           ))
	))

    (newline)

    (let ((list1 (list (list 3) (list 7 4) (list 2 4 9) (list 8 5 6 3)))
	  (debug-flag #t))
      (begin
        (time-code
         (begin
           (main-loop list1 debug-flag)
           ))
	))

    (newline)
    (force-output)

    (let ((list1 (list (list 75) (list 95 64) (list 17 47 82)
		       (list 18 35 87 10) (list 20 04 82 47 65)
		       (list 19 01 23 75 03 34) (list 88 02 77 73 07 63 67)
		       (list 99 65 04 28 06 16 70 92) (list 41 41 26 56 83 40 80 70 33)
		       (list 41 48 72 33 47 32 37 16 94 29)
		       (list 53 71 44 65 25 43 91 52 97 51 14)
		       (list 70 11 33 28 77 73 17 78 39 68 17 57)
		       (list 91 71 52 38 17 14 91 43 58 50 27 29 48)
		       (list 63 66 04 68 89 53 67 30 73 16 69 87 40 31)
		       (list 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)
		       ))
	  (debug-flag #t))
      (begin
        (time-code
         (begin
           (display (format #f "problem 18 triangle~%"))
           (main-loop list1 debug-flag)
           ))
	))

    (newline)
    (force-output)

    (let ((list1 (read-in-triangle "triangle.txt"))
	  (debug-flag #f))
      (begin
	(display (format #f "problem 67 triangle (first 12 rows)~%"))
	(do ((ii 0 (+ ii 1)))
	    ((> ii 12))
	  (begin
	    (display (format #f "~a~%" (list-ref list1 ii)))
	    ))
        (time-code
         (begin
           (main-loop list1 debug-flag)
           ))
	))

    ))
