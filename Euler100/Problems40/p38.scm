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
;;; most significant digit in position 0
(define (turn-digit-list-to-number dlist)
  (let ((this-num
	 (srfi-1:fold
	  (lambda (this-elem prev-elem)
	    (+ this-elem (* 10 prev-elem)))
	  0 dlist)))
    this-num
    ))

;;;#############################################################
;;;#############################################################
(define (test-turn-digit-list-to-number-1)
  (let ((sub-name "test-turn-digit-list-to-number-1")
	(test-list
	 (list
	  (list (list 1 2) 12)
	  (list (list 2 1) 21)
	  (list (list 1 2 3) 123)
	  (list (list 3 2 1) 321)
	  (list (list 1 2 3 4) 1234)
	  (list (list 4 3 2 1) 4321)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (turn-digit-list-to-number test-list)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : list = ~a, shouldbe = ~a, result = ~a~%"
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
(define (is-list-1-9-pandigital? digit-list)
  (let ((max-digit 9)
	(ok-flag #t))
    (let ((llen (length digit-list)))
      (begin
	(if (not (= llen 9))
	    (begin
	      #f)
	    (begin
	      (do ((ii 1 (+ ii 1)))
		  ((or (> ii max-digit)
		       (equal? ok-flag #f)))
		(begin
		  (if (equal? (member ii digit-list) #f)
		      (begin
			(set! ok-flag #f)
			#f)
		      )))
	      ok-flag
	      ))))))

;;;#############################################################
;;;#############################################################
(define (test-is-list-1-9-pandigital-1)
  (let ((sub-name "test-is-list-1-9-pandigital-1")
	(test-list
	 (list
	  (list (list 1 2 3 4 5 6 7 8 9) #t)
	  (list (list 9 2 3 4 5 6 7 8 1) #t)
	  (list (list 9 2 3 5 4 6 7 8 1) #t)
	  (list (list 8 2 3 4 5 6 7 8 9) #f)
	  (list (list 8 2 3 4 5 1 7 8 9) #f)
	  (list (list 1 2 3 4 5 6 7 8 9 3) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-bool (list-ref alist 1)))
	     (let ((result-bool (is-list-1-9-pandigital? test-list)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display (format #f "~a : error (~a) : list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-list
					(if (equal? shouldbe-bool #t) "true" "false")
					(if (equal? result-bool #t) "true" "false")))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; hardwire 9 since we are looking for a 1-9 pandigital
(define (concatenated-product-list this-num)
  (let ((dlist (split-digits-list this-num))
	(acc-num-list (list))
	(result-list (list))
	(acc-dlist (list))
	(break-flag #f))
    (let ((max-iter (+ 1 (euclidean/ 9 (length dlist)))))
      (begin
	(do ((jj 1 (+ jj 1)))
	    ((or (> jj max-iter)
		 (equal? break-flag #t)))
	  (begin
	    (let ((aa (* this-num jj)))
	      (let ((alist (split-digits-list aa)))
		(begin
		  (set! result-list (cons jj result-list))
		  (set! acc-num-list (cons aa acc-num-list))
		  (set! acc-dlist (append acc-dlist alist))
		  (if (>= (length acc-dlist) 9)
		      (set! break-flag #t))
		  )))
	    ))
	(if (equal? (is-list-1-9-pandigital? acc-dlist) #t)
	    (begin
	      (let ((this-pandigital (turn-digit-list-to-number acc-dlist)))
		(append (list this-pandigital) (list (reverse result-list)))
		))
	    (begin
	      (list)
	      ))
	))))

;;;#############################################################
;;;#############################################################
(define (test-concatenated-product-list-1)
  (let ((sub-name "test-concatenated-product-list-1")
	(test-list
	 (list
	  (list 9 (list 918273645 (list 1 2 3 4 5)))
	  (list 10 (list))
	  (list 192 (list 192384576 (list 1 2 3)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (concatenated-product-list test-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : num = ~a, shouldbe = ~a, result = ~a~%"
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
(define (main-loop max-num debug-flag)
  (begin
    (let ((counter 0)
	  (largest-list (list))
	  (largest-generator 0)
	  (largest-pandigital 0)
	  (null-list (list)))
      (begin
	(do ((ii 1 (+ ii 1)))
	    ((> ii max-num))
	  (begin
	    (let ((result-index-list (concatenated-product-list ii)))
	      (if (and (list? result-index-list) (> (length result-index-list) 0))
		  (begin
		    (let ((pandigital-num (list-ref result-index-list 0))
			  (index-list (list-ref result-index-list 1)))
		      (let ((index-length (length index-list)))
			(begin
			  (set! counter (+ counter 1))

			  (if (> pandigital-num largest-pandigital)
			      (begin
				(set! largest-list index-list)
				(set! largest-generator ii)
				(set! largest-pandigital pandigital-num)
				))

			  (if (equal? debug-flag #t)
			      (begin
				(display (ice9-format:format #f "(~:d)  pandigital number ~:d~%"
							     counter pandigital-num))
				(for-each
				 (lambda (this-elem)
				   (display (ice9-format:format #f "    ~:d x ~:d = ~:d~%"
								ii this-elem
								(* ii this-elem))))
				 index-list)
				(force-output)
				))))
		      )))
	      )))

	(display (ice9-format:format #f "~:d is the largest 1 to 9 pandigital number that can be formed as the concatenated product ~:d with a consecutive integer series, where n = ~:d > 1~%"
				     largest-pandigital largest-generator (length largest-list)))

	(display (ice9-format:format #f "pandigital number ~:d : (total found = ~:d)~%"
				     largest-pandigital counter))
	(for-each
	 (lambda (this-elem)
	   (display (ice9-format:format #f "    ~:d x ~:d = ~:d~%"
					largest-generator this-elem
					(* largest-generator this-elem))))
	 largest-list)
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
    (display (format #f "Problem 038 - Take the number 192 and multiply it by each of 1, 2, and 3:~%"))
    (newline)
    (display (format #f "192 x 1 = 192~%"))
    (display (format #f "192 x 2 = 384~%"))
    (display (format #f "192 x 3 = 576~%"))
    (newline)
    (display (format #f "By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)~%"))
    (newline)
    (display (format #f "The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).~%"))
    (newline)
    (display (format #f "What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n >= 1?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-turn-digit-list-to-number-1 counter)
	   (run-test test-is-list-1-9-pandigital-1 counter)
	   (run-test test-concatenated-product-list-1 counter)

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

    (let ((max-num 100000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
