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
;;; turns a list of a list of a list into a flat list
(define (flatten-list llist)
  (define (local-recurse llist acc-list)
    (if (and (list? llist) (> (length llist) 0))
	(begin
	  (let ((this-elem (car llist))
		(tail-list (cdr llist)))
	    (begin
	      (if (list? this-elem)
		  (let ((this-list (local-recurse this-elem (list))))
		    (begin
		      (set! acc-list (append this-list acc-list))
		      ))
		  (begin
		    (set! acc-list (cons this-elem acc-list))
		    ))
	      (local-recurse tail-list acc-list)
	      )))
	(begin
	  acc-list
	  )))
  (begin
    (reverse (local-recurse llist (list)))
    ))

;;;#############################################################
;;;#############################################################
(define (test-flatten-list-1)
  (let ((sub-name "test-flatten-list-1")
	(test-list
	 (list
	  (list (list (list 1 2 3) (list 4 5 6) 7 8 9) (list 1 2 3 4 5 6 7 8 9))
	  (list (list (list 1 2 3) (list (list 4 5 6) (list 7 8 9))) (list 1 2 3 4 5 6 7 8 9))
	  (list (list (list 1 2 3) (list (list 4 5 6) 7 8 9)) (list 1 2 3 4 5 6 7 8 9))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (flatten-list test-list)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : test list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-list
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
;;; takes two lists of strings and turns them into a product list of strings
;;; note: first element assumed to be left alone, e.g. twenty, twenty-one, ...
(define (prod-string-lists prefix-list suffix-list connector-string)
  (begin
    (let ((out-list
	   (map-in-order
	    (lambda (this-prefix)
	      (srfi-1:fold
	       (lambda (this-suffix prev-2)
		 (if (string-ci=? this-suffix "zzz")
		     (cons this-prefix prev-2)
		     (cons (string-append this-prefix connector-string this-suffix) prev-2)
		     ))
	       '() (reverse (cons "zzz" suffix-list))))
	    prefix-list)))
      (begin
	(flatten-list out-list)
	))))

;;;#############################################################
;;;#############################################################
(define (test-prod-string-lists-1)
  (let ((sub-name "test-prod-string-lists-1")
	(test-list
	 (list
	  (list
	   (list "twenty" "thirty")
	   (list "one" "two")
	   "-"
	   (list "twenty" "twenty-one" "twenty-two" "thirty" "thirty-one" "thirty-two"))
	  (list
	   (list "forty" "fifty")
	   (list "three" "four")
	   "-"
	   (list "forty" "forty-three" "forty-four" "fifty" "fifty-three" "fifty-four"))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((prefix-list (list-ref alist 0))
		 (suffix-list (list-ref alist 1))
		 (connector-string (list-ref alist 2))
		 (shouldbe-list (list-ref alist 3)))
	     (let ((result-list (prod-string-lists prefix-list suffix-list connector-string)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : test prefix list = ~a, suffix list = ~a, connector = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index prefix-list suffix-list
					connector-string shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (build-numbers-list debug-flag)
  (begin
    (let ((elist
	   (list "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
      (let ((teens-list
	     (list "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")))
	(let ((tens-list (list "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety")))
	  (let ((others-list (prod-string-lists tens-list elist "-")))
	    (begin
	      (let ((sub-one-hundred-list
		     (append elist teens-list others-list)))
		(begin
		  (if (equal? debug-flag #t)
		      (display (format #f "~a~%" sub-one-hundred-list)))

		  (let ((hundreds-prefix-list
			 (list "one hundred" "two hundred" "three hundred" "four hundred"
			       "five hundred" "six hundred" "seven hundred" "eight hundred"
			       "nine hundred")))
		    (let ((upper-list (prod-string-lists hundreds-prefix-list
							 sub-one-hundred-list " and ")))
		      (let ((final-list (append sub-one-hundred-list upper-list (list "one thousand"))))
			(begin
			  final-list
			  ))))
		  ))
	      )))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (string-list-count slist)
  (begin
    (let ((t1-string (string-join slist)))
      (let ((t2-string (string-delete #\space t1-string)))
	(let ((t3-string (string-delete #\- t2-string)))
	  (let ((tcount (string-length t3-string)))
	    tcount
	    ))))))

;;;#############################################################
;;;#############################################################
(define (test-string-list-count-1)
  (let ((sub-name "test-string-list-count-1")
	(test-list
	 (list
	  (list (list "one" "two" "three") 11)
	  (list (list "one" "two" "three" "four") 15)
	  (list (list "one" "two" "three" "four" "five") 19)
	  (list (list "three hundred and forty-two") 23)
	  (list (list "one hundred and fifteen") 20)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (string-list-count test-list)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : test list = ~a, shouldbe = ~a, result = ~a~%"
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
(define (main-loop num-list range-string)
  (let ((scount (string-list-count num-list)))
    (begin
      (display (ice9-format:format #f "Number of characters from ~a = ~:d~%"
				   range-string scount))
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
    (display (format #f "Problem 017 - If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.~%"))
    (newline)
    (display (format #f "If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?~%"))
    (newline)
    (display (format #f "NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of 'and' when writing out numbers is in compliance with British usage.~%"))
    (newline)

   ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-flatten-list-1 counter)
	   (run-test test-prod-string-lists-1 counter)
	   (run-test test-string-list-count-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((num-list (list "one" "two" "three" "four" "five"))
	  (range-string "1 to 5"))
      (begin
	(main-loop num-list range-string)
	))
    (newline)
    (force-output)

    (let ((debug-flag #f))
      (let ((num-list (build-numbers-list debug-flag))
	    (range-string "1 to 1000"))
	(begin
	  (time-code
	   (begin
	     (main-loop num-list range-string)
	     ))
	  )))
    (newline)
    ))

