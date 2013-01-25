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
(define (is-2d-cancellable-nt? numerator denominator)
  (let ((nlist (split-digits-list numerator))
	(dlist (split-digits-list denominator))
	(ratio1 (/ numerator denominator)))
    (let ((ok-flag #t))
      (let ((nlen (length nlist))
	    (dlen (length dlist)))
	(begin
	  (if (and (= nlen 2) (= dlen 2))
	      (begin
		;;; is there a common digit
		(let ((n1 (list-ref nlist 0))
		      (n2 (list-ref nlist 1)))
		  (begin
		    (cond
		     ((not (equal? (member 0 nlist) #f)) #f)
		     ((not (equal? (member 0 dlist) #f)) #f)
		     ((not (equal? (member n1 dlist) #f))
		      (begin
			(let ((reduced-list (delq1! n1 (list-copy dlist))))
			  (let ((d2 (list-ref reduced-list 0)))
			    (let ((ratio2 (/ n2 d2)))
			      (if (equal? ratio1 ratio2)
				  #t
				  #f
				  ))))))
		     ((not (equal? (member n2 dlist) #f))
		      (begin
			(let ((reduced-list (delq1! n2 (list-copy dlist))))
			  (let ((d1 (list-ref reduced-list 0)))
			    (let ((ratio2 (/ n1 d1)))
			      (if (equal? ratio1 ratio2)
				  #t
				  #f
				  ))))))
		     (else
		      #f
		      )))))
	      #f
	      ))))))

;;;#############################################################
;;;#############################################################
(define (test-is-2d-cancellable-nt-1)
  (let ((sub-name "test-is-2d-cancellable-nt-1")
	(test-list
	 (list
	  (list 10 20 #f) (list 11 22 #f) (list 12 24 #f)
	  (list 49 98 #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (test-den (list-ref alist 1))
		 (shouldbe-bool (list-ref alist 2)))
	     (let ((result-bool (is-2d-cancellable-nt? test-num test-den)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display (format #f "~a : error (~a) : numerator = ~a, denominator = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num test-den
					shouldbe-bool result-bool))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (make-2d-cancellable-list numerator denominator)
  (let ((nlist (split-digits-list numerator))
	(dlist (split-digits-list denominator))
	(ratio1 (/ numerator denominator))
	(null-list (list)))
    (let ((ok-flag #t))
      (let ((nlen (length nlist))
	    (dlen (length dlist)))
	(begin
	  (if (and (= nlen 2) (= dlen 2))
	      (begin
		;;; is there a common digit
		(let ((n1 (list-ref nlist 0))
		      (n2 (list-ref nlist 1)))
		  (begin
		    (cond
		     ((not (equal? (member 0 nlist) #f)) null-list)
		     ((not (equal? (member 0 dlist) #f)) null-list)
		     ((not (equal? (member n1 dlist) #f))
		      (begin
			(let ((reduced-list (delq1! n1 (list-copy dlist))))
			  (let ((d2 (list-ref reduced-list 0)))
			    (let ((ratio2 (/ n2 d2)))
			      (if (equal? ratio1 ratio2)
				  (list n2 d2)
				  null-list
				  ))))))
		     ((not (equal? (member n2 dlist) #f))
		      (begin
			(let ((reduced-list (delq1! n2 (list-copy dlist))))
			  (let ((d1 (list-ref reduced-list 0)))
			    (let ((ratio2 (/ n1 d1)))
			      (if (equal? ratio1 ratio2)
				  (list n1 d1)
				  null-list
				  ))))))
		     (else
		      null-list
		      )))))
	      null-list
	      ))))))

;;;#############################################################
;;;#############################################################
(define (test-make-2d-cancellable-list-1)
  (let ((sub-name "test-make-2d-cancellable-list-1")
	(test-list
	 (list
	  (list 10 20 (list)) (list 11 22 (list)) (list 12 24 (list))
	  (list 49 98 (list 4 8))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (test-den (list-ref alist 1))
		 (shouldbe-list (list-ref alist 2)))
	     (let ((result-list (make-2d-cancellable-list test-num test-den)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : numerator = ~a, denominator = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num test-den
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
(define (main-loop max-num debug-flag)
  (begin
    (let ((prod 1)
	  (counter 0))
      (begin
	(do ((ii 10 (+ ii 1)))
	    ((> ii max-num))
	  (begin
	    (do ((jj (+ ii 1) (+ jj 1)))
		((> jj max-num))
	      (begin
		(if (is-2d-cancellable-nt? ii jj)
		    (begin
		      (let ((this-ratio (/ ii jj)))
			(let ((this-num (numerator this-ratio))
			      (this-den (denominator this-ratio)))
			  (begin
			    (if (equal? debug-flag #t)
				(begin
				  (let ((rlist (make-2d-cancellable-list ii jj)))
				    (let ((rnum (list-ref rlist 0))
					  (rden (list-ref rlist 1)))
				      (begin
					(display (ice9-format:format #f "~:d/~:d = ~:d/~:d = ~:d/~:d~%"
								     ii jj rnum rden this-num this-den))
					)))))
			    (set! prod (* prod this-ratio))
			    (set! counter (1+ counter))
			    )))
		      ))))))

	(display (ice9-format:format #f "the product of the ~:d fractions, common denominator = ~a~%"
				     counter (denominator prod)))
	))))

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
    (display (format #f "Problem 033 - The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.~%"))
    (newline)
    (display (format #f "We shall consider fractions like, 30/50 = 3/5, to be trivial examples.~%"))
    (display (format #f "There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.~%"))
    (newline)
    (display (format #f "If the product of these four fractions is given in its lowest common terms, find the value of the denominator.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-is-2d-cancellable-nt-1 counter)
	   (run-test test-make-2d-cancellable-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 10)
	  (debug-flag #t))
      (begin
	(main-loop max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 100)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))
    (newline)
    ))
