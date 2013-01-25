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
(define (triangular-number this-num)
  (let ((t1 (+ this-num 1)))
    (let ((t2 (* this-num t1)))
      (let ((t3 (euclidean/ t2 2)))
	t3
	))))

;;;#############################################################
;;;#############################################################
(define (test-triangular-number-1)
  (let ((sub-name "test-triangular-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 3) (list 3 6) (list 4 10)
	  (list 5 15)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (triangular-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (pentagonal-number this-num)
  (let ((t1 (- (* 3 this-num) 1)))
    (let ((t2 (* this-num t1)))
      (let ((t3 (euclidean/ t2 2)))
	t3
	))))

;;;#############################################################
;;;#############################################################
(define (test-pentagonal-number-1)
  (let ((sub-name "test-pentagonal-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 5) (list 3 12) (list 4 22)
	  (list 5 35) (list 6 51) (list 7 70) (list 8 92)
	  (list 9 117) (list 10 145)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (pentagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (hexagonal-number this-num)
  (let ((t1 (- (* 2 this-num) 1)))
    (let ((t2 (* this-num t1)))
      t2
      )))

;;;#############################################################
;;;#############################################################
(define (test-hexagonal-number-1)
  (let ((sub-name "test-hexagonal-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 6) (list 3 15) (list 4 28)
	  (list 5 45)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (hexagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (populate-htable! p-htable this-function max-num)
  (begin
    (do ((jj 1 (+ jj 1)))
	((> jj max-num))
      (begin
	(let ((this-num (this-function jj)))
	  (hash-set! p-htable this-num jj)
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((counter 0)
	(p-htable (make-hash-table max-num))
	(h-htable (make-hash-table max-num))
	(break-flag #f))
    (begin
      (populate-htable! p-htable pentagonal-number max-num)
      (populate-htable! h-htable hexagonal-number max-num)

      (do ((nn 2 (+ nn 1)))
	  ((> nn max-num))
	(begin
	  (let ((tr-num (triangular-number nn)))
	    (let ((p-flag (hash-ref p-htable tr-num #f))
		  (h-flag (hash-ref h-htable tr-num #f)))
	      (begin
		(if (and (not (equal? p-flag #f))
			 (not (equal? h-flag #f)))
		    (begin
		      (set! counter (+ counter 1))
		      (if (equal? debug-flag #t)
			  (begin
			    (display (ice9-format:format #f "(~:d)  T(~:d) = P(~:d) = H(~:d) = ~:d~%"
							 counter nn p-flag h-flag tr-num))
			    (force-output)
			    ))))
		)))
	  ))

      (display (ice9-format:format #f "number of triples found = ~:d, for indexes less than ~:d~%"
				   counter max-num))
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
    (display (format #f "Problem 045 - Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:~%"))
    (newline)
    (display (format #f "Triangle:  Tn=n(n+1)/2     1, 3, 6, 10, 15, ...~%"))
    (display (format #f "Pentagonal:  Pn=n(3n1)/2   1, 5, 12, 22, 35, ...~%"))
    (display (format #f "Hexagonal:  Hn=n(2n1)      1, 6, 15, 28, 45, ...~%"))
    (newline)
    (display (format #f "It can be verified that T285 = P165 = H143 = 40755.~%"))
    (newline)
    (display (format #f "Find the next triangle number that is also pentagonal and hexagonal.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-triangular-number-1 counter)
	   (run-test test-pentagonal-number-1 counter)
	   (run-test test-hexagonal-number-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 100)
	  (debug-flag #t))
      (begin
	(main-loop max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 1000000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
