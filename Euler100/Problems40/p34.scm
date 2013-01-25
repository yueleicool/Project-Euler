#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice-9-format:)))

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
(define (factorial nn)
  (cond
   ((< nn 0) #f)
   ((= nn 0) 1)
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
	  (list 6 720)
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
(define (populate-factorial-hash! factorial-htable)
  (begin
    (do ((ii 0 (1+ ii)))
        ((> ii 9))
      (begin
        (let ((ifac (factorial ii)))
          (begin
            (hash-set! factorial-htable ii ifac)
            ))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (sum-factorial-digits anum factorial-htable)
  (let ((dlist (split-digits-list anum)))
    (let ((dflist (map
                   (lambda (this-digit)
                     (let ((fn (hash-ref factorial-htable this-digit 1)))
                       fn))
                   dlist)))
      (let ((fac-sum (srfi-1:fold + 0 dflist)))
        (begin
          fac-sum
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-sum-factorial-digits-1)
  (let ((sub-name "test-sum-factorial-digits-1")
	(test-list
	 (list
	  (list 0 1) (list 1 1) (list 2 2)
          (list 9 362880) (list 10 2) (list 11 2)
          (list 19 362881) (list 29 362882)
          (list 123 9) (list 1234 33)
          (list 99 725760)
	  ))
        (factorial-htable (make-hash-table 10))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (populate-factorial-hash! factorial-htable)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num
                    (sum-factorial-digits test-num factorial-htable)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display
                        (format #f "~a : error (~a) : number = ~a, "
                                sub-name test-label-index test-num))
		       (display
                        (format #f "shouldbe = ~a, result = ~a~%"
                                shouldbe-num result-num))
		       (set! ok-flag #f)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (debug-test-print)
(let ((t-list
         (list
          2540160 2540159 2540158 1999999 1999998
          1999988 1999888 1998888 1988888 1888888)))
    (let ((st-list (sort t-list >))
          (factorial-htable (make-hash-table 10)))
      (begin
        (populate-factorial-hash! factorial-htable)

        (for-each
         (lambda (anum)
           (begin
             (let ((sum (sum-factorial-digits anum factorial-htable)))
               (begin
                 (display
                  (ice-9-format:format
                   #f "The sum of factorial of digits for ~:d is ~:d~%"
                   anum sum))
                 ))
             )) st-list)
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (begin
    (let ((sum 0)
	  (counter 0)
	  (factorial-htable (make-hash-table 20)))
      (begin
        (populate-factorial-hash! factorial-htable)

	(do ((ii 3 (+ ii 1)))
	    ((> ii max-num))
	  (begin
	    (let ((fac-sum (sum-factorial-digits ii factorial-htable)))
              (begin
                (if (= fac-sum ii)
                    (begin
                      (if (equal? debug-flag #t)
                          (begin
                            (let ((ii-list (split-digits-list ii)))
                              (let ((s1
                                     (string-join
                                      (map
                                       (lambda (this-elem)
                                         (format #f "~a!" this-elem))
                                       ii-list)
                                      " + "))
                                    (s2 (string-join
                                         (map
                                          (lambda (this-elem)
                                            (ice-9-format:format
                                             #f "~:d"
                                             (hash-ref factorial-htable this-elem 1)))
                                          ii-list)
                                         " + ")))
                                (begin
                                  (display
                                   (ice-9-format:format #f "~a = ~a = ~:d~%"
                                                        s1 s2 ii))
                                  (force-output)
                                  )))
                            ))
                      (set! sum (+ sum ii))
                      (set! counter (1+ counter))
                      ))
                ))
            ))

	(display
         (ice-9-format:format
          #f "sum of all numbers that are equal to the sum of their digits = ~:d, (~:d numbers found, less than ~:d)~%"
          sum counter max-num))
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
    (display (format #f "Problem 034 - 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.~%"))
    (newline)
    (display (format #f "Find the sum of all numbers which are equal to the sum of the factorial of their digits.~%"))
    (newline)
    (display (format #f "Note: as 1! = 1 and 2! = 2 are not sums they are not included.~%"))
    (newline)
    (display (format #f "The sum of factorial of digits means that the digits matter, not the order.~%"))
    (display (format #f "9! is 362,880, so the largest number that can be a sum of it's factorials~%"))
    (display (format #f "of it's digits is 7x9! = 2,540,160, since an 8-digit number would have at~%"))
    (display (format #f "most a 7-digit sum of digits.~%"))
    (display (format #f "The sum of factorial of digits for 2,540,160 is 869.~%"))
    (display (format #f "The sum of factorial of digits for 2,540,159 is 363,148.~%"))
    (display (format #f "The sum of factorial of digits for 2,540,158 is 40,588.~%"))
    (display (format #f "The sum of factorial of digits of 1,999,999 is 2,177,281.~%"))
    (display (format #f "The sum of factorial of digits of 1,999,998 is 1,854,721.~%"))
    (display (format #f "The sum of factorial of digits of 1,999,988 is 1,532,161.~%"))
    (display (format #f "The sum of factorial of digits of 1,999,888 is 1,209,601.~%"))
    (display (format #f "The sum of factorial of digits of 1,998,888 is 887,041.~%"))
    (display (format #f "The sum of factorial of digits of 1,988,888 is 564,481.~%"))
    (display (format #f "The sum of factorial of digits of 1,888,888 is 241,921.~%"))
    (display (format #f "So from this we can see that the maximum number that we need to~%"))
    (display (format #f "consider is around 250,000, since there is no other way to get~%"))
    (display (format #f "a larger sum of digits (less than 2,540,160).~%"))
    (display (format #f "~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-factorial-1 counter)
           (run-test test-sum-factorial-digits-1 counter)

	   (display (ice-9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 1000)
	  (debug-flag #t))
      (begin
;;;        (debug-test-print)
;;;        (newline)
	(main-loop max-num debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 250000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))
    (newline)
    ))
