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
(define (dynamic-radical-array nn)
  (let ((radical-array (make-array 1 (1+ nn)))
        (prime-array (make-array 0 (1+ nn))))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((> ii nn))
	(begin
          (array-set! prime-array ii ii)
          ))

      (do ((ii 2 (1+ ii)))
	  ((> ii nn))
	(begin
          (let ((this-num (array-ref prime-array ii)))
            (begin
              (if (= this-num ii)
                  (begin
                    (array-set! radical-array ii ii)

                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj nn))
                      (begin
                        (array-set! prime-array -1 jj)

                        (let ((jj-num (array-ref radical-array jj)))
                          (let ((next-num (* ii jj-num)))
                            (begin
                              (array-set! radical-array next-num jj)
                              )))
                        ))
                    ))
	      ))
	  ))

      radical-array
      )))

;;;#############################################################
;;;#############################################################
(define (test-dynamic-radical-array-1)
  (let ((sub-name "test-dynamic-radical-array-1")
	(test-list
	 (list
	  (list 2 2) (list 3 3) (list 4 2)
          (list 5 5) (list 6 6) (list 7 7)
	  (list 8 2) (list 9 3) (list 10 10)
	  (list 11 11) (list 12 6) (list 13 13)
	  (list 14 14) (list 15 15) (list 16 2)
	  (list 17 17) (list 18 6) (list 19 19)
	  (list 20 10) (list 21 21) (list 22 22)
	  (list 32 2) (list 64 2) (list 128 2)
	  (list 27 3) (list 81 3) (list 243 3)
	  (list 25 5) (list 125 5) (list 625 5)
	  (list 63 21) (list 441 21) (list 1323 21)
	  (list 147 21)
	  (list 100 10) (list 504 42)
	  ))
        (result-array
         (dynamic-radical-array 2000))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-index (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
             (let ((result
                    (array-ref result-array test-index)))
               (begin
                 (if (not (equal? shouldbe result))
                     (begin
                       (display
                        (format
                         #f "~a : (~a) : error for index = ~a : "
                         sub-name test-label-index test-index))
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
(define (main-loop max-index target-index debug-flag)
  (let ((radical-array (dynamic-radical-array max-index))
	(result-list-list (list)))
    (begin
      (if (< max-index target-index)
	  (begin
	    (display
             (ice9-format:format
              #f "main-loop error must have max-index = ~:d "
              max-index))
	    (display
             (ice9-format:format
              #f "greater than target-index = ~:d.  stopping program.~%"
              target-index))
	    (quit)
	    ))

      (do ((nn 1 (1+ nn)))
	  ((> nn max-index))
	(begin
	  (let ((rad (array-ref radical-array nn)))
	    (let ((a-list (list nn rad)))
	      (begin
		(set! result-list-list (cons a-list result-list-list))
		)))
	  ))

      (let ((sorted-list-list
	     (sort result-list-list
		   (lambda (a b)
		     (begin
		       (let ((a-r (list-ref a 1))
			     (b-r (list-ref b 1)))
			 (begin
			   (if (= a-r b-r)
			       (begin
				 (< (list-ref a 0) (list-ref b 0)))
			       (begin
				 (< a-r b-r)
				 ))
			   ))
		       ))
		   )))
	(begin
	  (if (equal? debug-flag #t)
	      (begin
		(display (format #f "    Sorted~%"))
		(display (format #f "  n    rad(n)    k~%"))
		(let ((slen (length sorted-list-list)))
		  (begin
		    (do ((kk 1 (1+ kk)))
			((> kk slen))
		      (begin
			(let ((s-elem (list-ref sorted-list-list (1- kk))))
			  (let ((s-n (list-ref s-elem 0))
				(s-r (list-ref s-elem 1)))
			    (begin
			      (display
                               (ice9-format:format
                                #f "  ~:d      ~:d      ~:d~%"
                                s-n s-r kk))
			      )))
			))
		    (force-output)
		    ))
		))

	  (let ((e-elem
                 (list-ref sorted-list-list (1- target-index))))
	    (let ((e-n (list-ref e-elem 0)))
	      (begin
		(display
                 (ice9-format:format
                  #f "E(~:d) = ~:d (where 1 <= n <= ~:d).~%"
                  target-index e-n max-index))
		(force-output)
		)))
	  ))
      )))

;;;#############################################################
;;;#############################################################
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
    (display (format #f "Project Euler 124 - The radical of n, rad(n), is the product of distinct prime factors of n. For example, 504 = 2^3 x 3^2 x 7, so rad(504) = 2 x 3 x 7 = 42.~%"))
    (newline)
    (display (format #f "If we calculate rad(n) for 1 <= n <= 10, then sort them on rad(n), and sorting on n if the radical values are equal, we get:~%"))
    (newline)
    (display (format #f "  Unsorted     Sorted~%"))
    (display (format #f "  n  rad(n)    n  rad(n)  k~%"))
    (display (format #f "  1  1         1  1       1~%"))
    (display (format #f "  2  2         2  2       2~%"))
    (display (format #f "  3  3         4  2       3~%"))
    (display (format #f "  4  2         8  2       4~%"))
    (display (format #f "  5  5         3  3       5~%"))
    (display (format #f "  6  6         9  3       6~%"))
    (display (format #f "  7  7         5  5       7~%"))
    (display (format #f "  8  2         6  6       8~%"))
    (display (format #f "  9  3         7  7       9~%"))
    (display (format #f "  10 10        10 10     10~%"))
    (newline)
    (display (format #f "Let E(k) be the kth element in the sorted n column; for example, E(4) = 8 and E(6) = 9.~%"))
    (newline)
    (display (format #f "If rad(n) is sorted for 1 <= n <= 100000, find E(10000).~%"))
    (newline)
    (display (format #f "This algorithm uses a sieve of Eratosthenes to compute the radical of each number.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-dynamic-radical-array-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-index 10)
	  (target-index 6)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-index target-index debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-index 10)
	  (target-index-list (list 4 6))
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (for-each
	    (lambda (tindex)
	      (begin
		(main-loop max-index tindex debug-flag)
		)) target-index-list)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-index 100000)
	  (target-index 10000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-index target-index debug-flag)
	   ))
	))
    (newline)
    ))
