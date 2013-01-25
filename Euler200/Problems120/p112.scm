#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;; this code is dedicated to the public domain

;;; srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;; ice-9 format for advanced format
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
(define (factorial ii-num)
  (begin
    (cond
     ((<= ii-num 1) 1)
     ((= ii-num 2) 2)
     ((= ii-num 3) 6)
     ((= ii-num 4) 24)
     ((= ii-num 5) 120)
     ((= ii-num 6) 720)
     ((= ii-num 7) 5040)
     ((= ii-num 8) 40320)
     ((= ii-num 9) 362880)
     (else
      (* ii-num (factorial (- ii-num 1)))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-factorial-1)
  (let ((sub-name "test-factorial-1")
	(test-list
	 (list
	  (list 0 1) (list 1 1) (list 2 2)
	  (list 3 6) (list 4 24) (list 5 120)
	  (list 6 720) (list 7 5040)
	  (list 8 40320) (list 9 362880)
	  (list 10 3628800)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (factorial test-num)))
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
(define (choose kk nn)
  (define (local-partial-factorial larger-num smaller-num)
    (let ((result 1))
      (begin
	(do ((ii larger-num (- ii 1)))
	    ((<= ii smaller-num))
	  (begin
	    (set! result (* result ii))
	    ))
	result
	)))
  (cond
   ((= kk 0) 0)
   ((= nn 0) 1)
   ((= kk nn) 1)
   ((>= kk nn)
    (begin
      (let ((kfact (local-partial-factorial kk nn))
	    (nfact (factorial (- kk nn))))
	(let ((result (/ kfact nfact)))
	  (begin
	    result
	    )))))
   (else
    ;;; (< kk nn)
    (begin
      (let ((kfact (factorial (- nn kk)))
	    (nfact (local-partial-factorial nn kk)))
	(let ((result (/ nfact kfact)))
	  result
	  ))))
   ))

;;;#############################################################
;;;#############################################################
(define (test-choose-1)
  (let ((sub-name "test-choose-1")
	(test-list
	 (list
	  (list 0 0 0) (list 1 0 1) (list 0 1 0)
	  (list 2 0 1) (list 2 1 2) (list 2 2 1)
	  (list 3 0 1) (list 3 1 3) (list 3 2 3) (list 3 3 1)
	  (list 4 0 1) (list 4 1 4) (list 4 2 6) (list 4 3 4)
	  (list 4 4 1)
	  (list 5 0 1) (list 5 1 5) (list 5 2 10) (list 5 3 10)
	  (list 5 4 5) (list 5 5 1)
	  (list 6 0 1) (list 6 1 6) (list 6 2 15) (list 6 3 20)
	  (list 6 4 15) (list 6 5 6) (list 6 6 1)
	  (list 0 6 0) (list 1 6 6) (list 2 6 15) (list 3 6 20)
	  (list 4 6 15) (list 5 6 6) (list 6 6 1)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-kk (list-ref alist 0))
		 (test-nn (list-ref alist 1))
		 (shouldbe-num (list-ref alist 2)))
	     (let ((result-num (choose test-kk test-nn)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : kk = ~a, nn = ~a : "
                         sub-name test-label-index test-kk test-nn))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
      (srfi-11:let-values
       (((next-num this-digit) (euclidean/ this-num 10)))
       (begin
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
	  (list 123 (list 1 2 3)) (list 1234 (list 1 2 3 4))
          (list 98765 (list 9 8 7 6 5))
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
		       (display
                        (format
                         #f "~a : (~a) : error : number = ~a : "
                         sub-name test-label-index test-num))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (is-bouncy? this-num)
  (cond
   ((<= this-num 99) #f)
   (else
    (begin
      (let ((dlist (split-digits-list this-num)))
	(let ((dlen (length dlist))
	      (up-flag #f)
	      (down-flag #f)
	      (last-digit (car dlist)))
	  (begin
	    (do ((ii 1 (1+ ii)))
		((>= ii dlen))
	      (begin
		(let ((this-digit (list-ref dlist ii)))
		  (begin
		    (cond
		     ((> last-digit this-digit)
		      (begin
			(set! down-flag #t)
			))
		     ((< last-digit this-digit)
		      (begin
			(set! up-flag #t)
			)))
		    (set! last-digit this-digit)
		    ))
		))
	    (if (and (equal? up-flag #t) (equal? down-flag #t))
		(begin
		  #t)
		(begin
		  #f
		  ))
	    )))
      ))
   ))

;;;#############################################################
;;;#############################################################
(define (test-is-bouncy-1)
  (let ((sub-name "test-is-bouncy-1")
	(test-list
	 (list
	  (list 3 #f) (list 99 #f) (list 101 #t) (list 102 #t)
	  (list 110 #f) (list 123 #f) (list 523 #t)
	  (list 155349 #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (is-bouncy? test-num)))
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
(define (find-least-number max-num target-pcnt
			   previous-min previous-bouncy)
  (let ((bcounter (max 0 previous-bouncy))
	(total (max 100 previous-min))
	(start-num (1+ (max 100 previous-min)))
	(min-number -1)
	(continue-loop-flag #t))
    (begin
      (do ((ii start-num (1+ ii)))
	  ((or (> ii max-num)
	       (equal? continue-loop-flag #f)))
	(begin
	  (set! total (1+ total))

	  (if (is-bouncy? ii)
	      (begin
		(set! bcounter (1+ bcounter))

		(let ((this-pcnt (/ bcounter total)))
		  (begin
		    (if (>= this-pcnt target-pcnt)
			(begin
			  (set! min-number ii)
			  (set! continue-loop-flag #f)
			  ))
		    ))
		))
	  ))

      (list min-number bcounter)
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-least-number-1)
  (let ((sub-name "test-find-least-number-1")
	(test-list
	 (list
	  (list 2000 (/ 1 2) -1 -1 (list 538 269))
	  (list 2000 (/ 1 2) 100 0 (list 538 269))
	  (list 50000 (/ 9 10) 538 269 (list 21780 19602))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((max-num (list-ref alist 0))
		 (target-pcnt (list-ref alist 1))
		 (previous-min (list-ref alist 2))
		 (previous-bouncy (list-ref alist 3))
		 (shouldbe (list-ref alist 4)))
	     (let ((result (find-least-number max-num target-pcnt
					      previous-min previous-bouncy)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : max-num = ~a, target-pcnt=~a, "
                         sub-name test-label-index max-num target-pcnt))
		       (display
                        (format
                         #f "previous-min = ~a, previous-bouncy = ~a : "
                         previous-min previous-bouncy))
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
;;; lattice path algorithm assumes going from (0,0) to (a,b)
;;; to map to the problem, need to reduce 1 - 9 digits to 0 - 8
(define (count-increasing-numbers num-digits)
  (begin
    (cond
     ((<= num-digits 1) #f)
     (else
      (let ((nn (+ num-digits 8)))
	(let ((ncr (choose nn 8)))
	  (let ((count (- ncr 9)))
	    (begin
              ;;; removed the 9 flat numbers, 11111, 22222,..., 99999
	      count
	      ))
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-increasing-numbers-1)
  (let ((sub-name "test-count-increasing-numbers-1")
	(test-list
	 (list
	  (list 2 36) (list 3 156)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-digits (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (count-increasing-numbers num-digits)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : num-digits = ~a : "
                         sub-name test-label-index num-digits))
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
;;; lattice path algorithm assumes going from (0,0) to (a,b)
(define (count-decreasing-numbers num-digits)
  (begin
    (cond
     ((<= num-digits 1) #f)
     (else
      (let ((nn (+ num-digits 9)))
	(let ((ncr (choose nn 9)))
	  (let ((count (- ncr 10)))
	    (begin
              ;;; removed the 10 flat numbers, 00000, 11111, 22222,..., 99999
	      count
	      ))
	  ))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-decreasing-numbers-1)
  (let ((sub-name "test-count-decreasing-numbers-1")
	(test-list
	 (list
	  (list 2 45) (list 3 210)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-digits (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (count-decreasing-numbers num-digits)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : num-digits = ~a : "
                         sub-name test-label-index num-digits))
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
(define (count-bouncy-ndigit-numbers ndigit-numbers)
  (let ((flat-count 9)
        (total-count
         (* 9 (inexact->exact
               (expt 10 (1- ndigit-numbers)))))
        (increasing-count
         (count-increasing-numbers ndigit-numbers))
        (decreasing-count
         (count-decreasing-numbers ndigit-numbers)))
    (let ((bouncy-count
           (- total-count
              (+ increasing-count decreasing-count flat-count))))
      (begin
        bouncy-count
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-count-bouncy-ndigit-numbers-1)
  (let ((sub-name "test-count-bouncy-ndigit-numbers-1")
	(test-list
	 (list
	  (list 2 0)
          (list 3 525)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-digits (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result
                    (count-bouncy-ndigit-numbers num-digits)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : num-digits = ~a : "
                         sub-name test-label-index num-digits))
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
;;; get a rough estimate, increment by factors of 10 (100, 1000, 10000,...)
(define (find-approximate-least-number target-pcnt max-digits)
  (let ((result-list (list))
        (bcount 0)
        (last-bcount 0)
        (last-total 100)
	(total 100)
	(start-num 2)
	(continue-loop-flag #t))
    (begin
      (do ((ii start-num (1+ ii)))
	  ((or (> ii max-digits)
	       (equal? continue-loop-flag #f)))
	(begin
          (let ((ii-bouncy-count
                 (count-bouncy-ndigit-numbers ii)))
            (begin
              (set! bcount (+ bcount ii-bouncy-count))
              (let ((exact-pcnt (/ bcount total)))
                (begin
                  (if (>= exact-pcnt target-pcnt)
                      (begin
                        (set! continue-loop-flag #f))
                      (begin
                        (set! last-bcount bcount)
                        (set! last-total total)
                        (set! total (* total 10))
                        ))
                  ))
              ))
          ))

      (list last-bcount last-total)
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-approximate-least-number-1)
  (let ((sub-name "test-find-approximate-least-number-1")
	(test-list
	 (list
	  (list 0 5 (list 0 100))
          (list (/ 1 2) 5 (list 0 100))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((target-pcnt (list-ref alist 0))
                 (max-digits (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result
                    (find-approximate-least-number
                     target-pcnt max-digits)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : target-pcnt = ~a, max-digits = ~a : "
                         sub-name test-label-index target-pcnt max-digits))
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
(define (main-loop target-pcnt max-digits max-num)
  (let ((rr-list
         (find-approximate-least-number
          target-pcnt max-digits)))
    (let ((last-bcount (list-ref rr-list 0))
          (last-total (list-ref rr-list 1)))
      (let ((rlist
             (find-least-number
              max-num target-pcnt last-total last-bcount)))
        (let ((least-num (list-ref rlist 0))
              (bouncy-count (list-ref rlist 1)))
          (begin
            (display
             (ice9-format:format
              #f "~:d = the least number for which the proportion of bouncy numbers~%"
              least-num))
            (display
             (ice9-format:format
              #f "first reaches ~a, with ~:d bouncy numbers.~%"
              target-pcnt bouncy-count))
            (force-output)
            ))
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
    (display (format #f "Project Euler 112 - Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.~%"))
    (newline)
    (display (format #f "Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.~%"))
    (newline)
    (display (format #f "We shall call a positive integer that is neither increasing nor decreasing a 'bouncy' number; for example, 155349.~%"))
    (newline)
    (display (format #f "Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525) are bouncy. In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.~%"))
    (newline)
    (display (format #f "Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.~%"))
    (newline)
    (display (format #f "Find the least number for which the proportion of bouncy numbers is exactly 99%.~%"))
    (newline)
    (display (format #f "The fast calculation of bouncy numbers uses the same method from problem 113, a lattice path calculation method for increasing and decreasing numbers.  A brute-force method is used once we get into the neighborhood.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (let ((counter 0))
      (begin
	(time-code
	 (begin
           (run-test test-factorial-1 counter)
           (run-test test-choose-1 counter)
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-is-bouncy-1 counter)
	   (run-test test-find-least-number-1 counter)
           (run-test test-count-increasing-numbers-1 counter)
           (run-test test-count-decreasing-numbers-1 counter)
           (run-test test-count-bouncy-ndigit-numbers-1 counter)
           (run-test test-find-approximate-least-number-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (newline)
    (force-output)

    (let ((target-pcnt (/ 1 2))
          (max-num 10000)
          (max-digits 3))
      (begin
	(time-code
	 (begin
	   (main-loop target-pcnt max-digits max-num)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-pcnt (/ 9 10))
          (max-digits 20)
          (max-num 100000))
      (begin
	(time-code
	 (begin
	   (main-loop target-pcnt max-digits max-num)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-pcnt (/ 99 100))
          (max-digits 100)
          (max-num 100000000))
      (begin
	(time-code
	 (begin
	   (main-loop target-pcnt max-digits max-num)
	   ))
	))

    (newline)
    ))
