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
	     (* 0.0010 (truncate (* 1000.0 (- nmins (* nhours 60.0))))
		)))
	(let ((nseconds
	       (* 0.0010 (truncate
			  (* 1000.0 (- nsecs (+ (* nhours 60.0 60.0) (* nminutes 60.0)))
			     )))))
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
(define (factorial nn)
  (cond
   ((< nn 0) #f)
   ((= nn 0) 1)
   ((= nn 1) 1)
   ((= nn 2) 2)
   ((= nn 3) 6)
   ((= nn 4) 24)
   ((= nn 5) 120)
   ((= nn 6) 720)
   ((= nn 7) 5040)
   ((= nn 8) 40320)
   ((= nn 9) 362880)
   (else
    (* nn (factorial (- nn 1)))
    )))

;;;#############################################################
;;;#############################################################
(define (test-factorial-1)
  (let ((sub-name "test-factorial-1")
	(test-list
	 (list
	  (list 0 1) (list 1 1) (list 2 2)
	  (list 3 6) (list 4 24) (list 5 120)
	  (list 6 720) (list 7 5040) (list 8 40320)
	  (list 9 362880)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (factorial test-num)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
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
	  ))
      )))
  (let ((result-list (local-loop this-num (list))))
    (begin
      result-list
      )))

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
(define (digits-factorial this-num)
  (let ((dlist (split-digits-list this-num))
        (sum 0))
    (begin
      (for-each
       (lambda (this-digit)
         (let ((this-fact (factorial this-digit)))
           (begin
             (set! sum (+ sum this-fact))
             )))
       dlist)
      sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-digits-factorial-1)
  (let ((sub-name "test-digits-factorial-1")
	(test-list
	 (list
	  (list 145 145) (list 169 363601)
	  (list 363601 1454) (list 1454 169)
	  (list 871 45361) (list 45361 871)
	  (list 872 45362) (list 45362 872)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result
                    (digits-factorial test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : number = ~a : "
                         sub-name test-label-index test-num))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (calculate-sequences
         start-num next-seq-htable acc-list)
  (cond
   ((not (equal? (memq start-num acc-list) #f))
    (begin
      (reverse (cons start-num acc-list))
      ))
   (else
    (let ((next-num (hash-ref next-seq-htable start-num #f)))
      (begin
	(if (equal? next-num #f)
	    (begin
	      (let ((this-num
                     (digits-factorial start-num)))
		(begin
		  (hash-set! next-seq-htable start-num this-num)
		  (set! next-num this-num)
		  ))
	      ))

	(let ((next-list (cons start-num acc-list)))
	  (begin
	    (calculate-sequences
             next-num next-seq-htable next-list)
	    ))
	))
    )))

;;;#############################################################
;;;#############################################################
(define (test-calculate-sequences-1)
  (let ((sub-name "test-calculate-sequences-1")
	(test-list
	 (list
	  (list 145 (list 145 145))
	  (list 169 (list 169 363601 1454 169))
	  (list 363601 (list 363601 1454 169 363601))
	  (list 1454 (list 1454 169 363601 1454))
	  (list 871 (list 871 45361 871))
	  (list 872 (list 872 45362 872))
	  (list 69 (list 69 363600 1454 169 363601 1454))
	  (list 78 (list 78 45360 871 45361 871))
	  (list 540 (list 540 145 145))
	  ))
	(next-seq-htable (make-hash-table 100))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result
                    (calculate-sequences
                     test-num next-seq-htable (list))))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : number = ~a : "
                         sub-name test-label-index test-num))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (main-loop max-num target-length status-num debug-flag)
  (let ((target-num-list (list))
	(target-sequence (list))
	(target-count 0)
	(next-seq-htable (make-hash-table 1000))
	(start-jdate (srfi-19:current-julian-day)))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (let ((slist
                 (calculate-sequences
                  ii next-seq-htable (list))))
            (let ((slen (- (length slist) 1)))
              (begin
                (if (equal? slen target-length)
                    (begin
                      (set! target-num-list (cons ii target-num-list))
                      (set! target-sequence slist)
                      (set! target-count (1+ target-count))

                      (if (equal? debug-flag #t)
                          (begin
                            (display
                             (format
                              #f "start = ~a, sequence = ~a, length = ~a, so far = ~a~%"
                              ii target-sequence slen target-num-list))
                            ))
                      ))
                )))

	  (if (zero? (modulo ii status-num))
	      (begin
		(let ((end-jdate (srfi-19:current-julian-day)))
		  (let ((elapsed-time
			 (julian-day-difference-to-string
			  end-jdate start-jdate))
			(now (date-time-to-string (srfi-19:current-date))))
		    (begin
		      (display
		       (ice9-format:format
			#f "completed ~:d out of ~:d : count so far = ~:d : "
			ii max-num target-count))
		      (display
		       (ice9-format:format
			#f "elapsed time ~a : ~a~%"
			elapsed-time now))
		      (force-output)

		      (set! start-jdate end-jdate)
		      )))
		))
	  ))

      (display
       (ice9-format:format
        #f "the number of chains that contain exactly ~:d "
        target-length))
      (display
       (ice9-format:format
        #f "non-repeating terms is ~:d (for numbers less than ~:d)~%"
        target-count max-num))
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
    (display (format #f "Problem 074 - The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:~%"))
    (display (format #f "1! + 4! + 5! = 1 + 24 + 120 = 145~%"))
    (newline)
    (display (format #f "Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169; it turns out that there are only three such loops that exist:~%"))
    (display (format #f "    169 -> 363601 -> 1454 -> 169~%"))
    (display (format #f "    871 -> 45361 -> 871~%"))
    (display (format #f "    872 -> 45362 -> 872~%"))
    (newline)
    (display (format #f "It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,~%"))
    (display (format #f "    69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)~%"))
    (display (format #f "    78 -> 45360 -> 871 -> 45361 (-> 871)~%"))
    (display (format #f "    540 -> 145 (-> 145)~%"))
    (newline)
    (display (format #f "Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.~%"))
    (newline)
    (display (format #f "How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?~%"))
    (newline)
    (display (format #f "Pre-computing the factorials from 1 through 9, and storing terms in a sequence in a hash table, was the key to making this program run fast.  A more detailed description of the solution can be found at http://www.mathblog.dk/project-euler-74-determine-the-number-of-factorial-chains-that-contain-exactly-sixty-non-repeating-terms/~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-factorial-1 counter)
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-digits-factorial-1 counter)
	   (run-test test-calculate-sequences-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 100)
	  (target-length 5)
	  (status-num 1000)
	  (debug-flag #t))
      (begin
        (time-code
         (begin
           (main-loop max-num target-length status-num debug-flag)
           ))
	))

    (newline)
    (force-output)

    (let ((max-num 1000000)
	  (target-length 60)
	  (status-num 200000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num target-length status-num debug-flag)
	   ))
	))

    (newline)
    ))
