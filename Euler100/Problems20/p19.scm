#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

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
(define (populate-month-hash! mhash)
  (begin
    (hash-set! mhash 1 31)   ;;; january
    (hash-set! mhash 2 28)   ;;; february
    (hash-set! mhash 3 31)   ;;; march
    (hash-set! mhash 4 30)   ;;; april
    (hash-set! mhash 5 31)   ;;; may
    (hash-set! mhash 6 30)   ;;; june
    (hash-set! mhash 7 31)   ;;; july
    (hash-set! mhash 8 31)   ;;; august
    (hash-set! mhash 9 30)   ;;; september
    (hash-set! mhash 10 31)  ;;; october
    (hash-set! mhash 11 30)  ;;; november
    (hash-set! mhash 12 31)  ;;; december
    ))

;;;#############################################################
;;;#############################################################
;;; date data structure (list day month year dow yyyymmdd)
(define (increment-date my-date-list mdate-htable)
  (define (local-make-list next-day next-month next-year next-dow last-day-of-month)
    (if (<= next-day last-day-of-month)
	(begin
	  (list next-day next-month next-year next-dow
		(+ (* 10000 next-year) (* 100 next-month) next-day)))
	(begin
	  (set! next-day 1)
	  (set! next-month (+ next-month 1))
	  (if (<= next-month 12)
	      (list next-day next-month next-year next-dow
		    (+ (* 10000 next-year) (* 100 next-month) next-day))
	      (begin
		(set! next-month 1)
		(set! next-year (+ next-year 1))
		(list next-day next-month next-year next-dow
		      (+ (* 10000 next-year) (* 100 next-month) next-day))
		))
	  )))
  (let ((this-day (list-ref my-date-list 0))
	(this-month (list-ref my-date-list 1))
	(this-year (list-ref my-date-list 2))
	(this-dow (list-ref my-date-list 3)))
    (let ((last-day-of-month (hash-ref mdate-htable this-month 1))
	  (next-day (+ this-day 1))
	  (next-month this-month)
	  (next-year this-year)
	  (next-dow (modulo (+ this-dow 1) 7)))
      (begin
	(cond
	 ((= this-month 2)
	  (begin
	    (let ((is-leap-year #f)
		  (div-by-four (zero? (modulo next-year 4))))
	      (begin
		(if (equal? div-by-four #t)
		    (let ((div-by-hundred (zero? (modulo next-year 100)))
			  (div-by-four-hundred (zero? (modulo next-year 400))))
		      (begin
			(if (not (equal? div-by-hundred #t))
			    (begin
			      (set! is-leap-year #t))
			    (if (equal? div-by-four-hundred #t)
				(set! is-leap-year #t)))
			)))
		(if (equal? is-leap-year #t)
		    (begin
		      (set! last-day-of-month 29)))

		(local-make-list next-day next-month next-year next-dow last-day-of-month)
		))))
	 (else
	  (local-make-list next-day next-month next-year next-dow last-day-of-month)
	  ))
	))))

;;;#############################################################
;;;#############################################################
(define (test-increment-date-1)
  (let ((sub-name "test-increment-date-1")
	(test-list
	 (list
	  (list (list 1 1 1900 0 19000101) (list 2 1 1900 1 19000102))
	  (list (list 2 1 1900 1 19000102) (list 3 1 1900 2 19000103))
	  (list (list 29 1 1900 1 19000129) (list 30 1 1900 2 19000130))
	  (list (list 31 1 1900 3 19000131) (list 1 2 1900 4 19000201))
	  (list (list 28 2 1900 6 19000128) (list 1 3 1900 0 19000301))
	  (list (list 28 2 1901 6 19010128) (list 1 3 1901 0 19010301))
	  (list (list 28 2 1902 6 19020128) (list 1 3 1902 0 19020301))
	  (list (list 28 2 1903 6 19030128) (list 1 3 1903 0 19030301))
	  (list (list 28 2 1904 6 19040128) (list 29 2 1904 0 19040229))
	  (list (list 29 2 1904 0 19040129) (list 1 3 1904 1 19040301))
	  (list (list 31 3 1900 5 19000331) (list 1 4 1900 6 19000401))
	  (list (list 30 4 1900 5 19000430) (list 1 5 1900 6 19000501))
	  (list (list 31 5 1900 5 19000531) (list 1 6 1900 6 19000601))
	  (list (list 30 6 1900 5 19000630) (list 1 7 1900 6 19000701))
	  (list (list 31 7 1900 5 19000731) (list 1 8 1900 6 19000801))
	  (list (list 31 8 1900 5 19000831) (list 1 9 1900 6 19000901))
	  (list (list 30 9 1900 5 19000930) (list 1 10 1900 6 19001001))
	  (list (list 31 10 1900 5 19000331) (list 1 11 1900 6 19001101))
	  (list (list 30 11 1900 5 19000330) (list 1 12 1900 6 19001201))
	  (list (list 31 12 1900 5 19000331) (list 1 1 1901 6 19010101))
	  ))
	(monthend-htable (make-hash-table 12))
	(test-label-index 0))
    (begin
      (populate-month-hash! monthend-htable)

      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-date (list-ref this-list 0))
		 (shouldbe-date (list-ref this-list 1)))
	     (let ((result-date (increment-date test-date monthend-htable)))
	       (begin
		 (if (not (equal? shouldbe-date result-date))
		     (begin
		       (display (format #f "~a : error (~a) : initial date=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-date
					shouldbe-date result-date))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; date data structure (list day month year dow yyyymmdd)
(define (date-to-string tdate)
  (let ((dow (list-ref tdate 3))
	(dstrings-list (list "monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday")))
    (let ((dow-string (format #f "dow error dow=~a should be >=0 and <=6" dow))
	  (day (list-ref tdate 0))
	  (month (list-ref tdate 1))
	  (year (list-ref tdate 2)))
      (begin
	(if (and (>= dow 0) (<= dow 6))
	    (set! dow-string (list-ref dstrings-list dow)))

	(let ((this-string (format #f "~a, ~a/~a/~a"
				   dow-string month day year)))
	  this-string
	  )))))

;;;#############################################################
;;;#############################################################
(define (test-date-to-string-1)
  (let ((sub-name "test-date-to-string-1")
	(test-list
	 (list
	  (list (list 1 1 1900 0 19000101) "monday, 1/1/1900")
	  (list (list 2 1 1900 1 19000102) "tuesday, 1/2/1900")
	  (list (list 3 1 1900 2 19000103) "wednesday, 1/3/1900")
	  (list (list 4 1 1900 3 19000104) "thursday, 1/4/1900")
	  (list (list 5 1 1900 4 19000105) "friday, 1/5/1900")
	  (list (list 6 1 1900 5 19000106) "saturday, 1/6/1900")
	  (list (list 7 1 1900 6 19000107) "sunday, 1/7/1900")
	  (list (list 8 1 1900 0 19000108) "monday, 1/8/1900")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-date (list-ref this-list 0))
		 (shouldbe-string (list-ref this-list 1)))
	     (let ((result-string (date-to-string test-date)))
	       (begin
		 (if (not (equal? shouldbe-string result-string))
		     (begin
		       (display (format #f "~a : error (~a) : initial date=~a, shouldbe=~a, result=~a~%"
					sub-name test-index test-date
					shouldbe-string result-string))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (recalc-day-of-week init-date find-date)
  (let ((this-date init-date)
	(this-yyyymmdd (list-ref init-date 4))
	(find-yyyymmdd (list-ref find-date 4))
	(mdate-htable (make-hash-table 12))
	(dow 0))
    (begin
      (populate-month-hash! mdate-htable)

      (let ((init-complete-flag #f))
	(begin
	  (while
	   (or (equal? init-complete-flag #f)
	       (> this-yyyymmdd find-yyyymmdd))
	   (begin
	     (let ((next-date (increment-date this-date mdate-htable)))
	       (let ((next-yyyymmdd (list-ref next-date 4)))
		 (if (>= next-yyyymmdd find-yyyymmdd)
		     (begin
		       (set! dow (list-ref next-date 3))
		       (set! init-complete-flag #t)
		       (break))
		     (begin
		       (set! this-date next-date)
		       ))))))))
      (list (list-ref find-date 0) (list-ref find-date 1)
	    (list-ref find-date 2) dow (list-ref find-date 4))
      )))

;;;#############################################################
;;;#############################################################
(define (test-recalc-day-of-week-1)
  (let ((sub-name "test-recalc-day-of-week-1")
	(test-list
	 (list
	  (list (list 1 1 1900 0 19000101) (list 2 1 1900 0 19000102) (list 2 1 1900 1 19000102))
	  (list (list 1 1 1900 0 19000101) (list 3 1 1900 0 19000103) (list 3 1 1900 2 19000103))
	  (list (list 1 1 1900 0 19000101) (list 4 1 1900 0 19000104) (list 4 1 1900 3 19000104))
	  (list (list 1 1 1900 0 19000101) (list 5 1 1900 0 19000105) (list 5 1 1900 4 19000105))
	  (list (list 1 1 1900 0 19000101) (list 6 1 1900 0 19000106) (list 6 1 1900 5 19000106))
	  (list (list 1 1 1900 0 19000101) (list 7 1 1900 0 19000107) (list 7 1 1900 6 19000107))
	  (list (list 1 1 1900 0 19000101) (list 8 1 1900 0 19000108) (list 8 1 1900 0 19000108))
	  (list (list 1 1 1900 0 19000101) (list 9 1 1900 0 19000109) (list 9 1 1900 1 19000109))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-init (list-ref this-list 0))
		 (test-start (list-ref this-list 1))
		 (shouldbe-date (list-ref this-list 2)))
	     (let ((result-date (recalc-day-of-week test-init test-start)))
	       (begin
		 (if (not (equal? shouldbe-date result-date))
		     (begin
		       (display (format #f "~a : error (~a) : initial date=~a, start=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-init test-start
					shouldbe-date result-date))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; date data structure (list day month year dow yyyymmdd)
;;; note: dow monday = 0, tuesday = 1, ..., sunday = 6
(define (iterate-date-loop init-date start-date end-date debug-flag)
  (let ((sunday-count 0)
	(first-month-count 0)
	(sunday-first-count 0)
	(this-date init-date)
	(restart-date start-date)
	(start-yyyymmdd (list-ref start-date 4))
	(mdate-htable (make-hash-table 12)))
    (begin
      (populate-month-hash! mdate-htable)

      ;;; initialization, from init-date to start date
      ;;; need to restart date to find the right day-of-week
      (let ((restart-date (recalc-day-of-week init-date start-date)))
	(begin
	  (if (equal? debug-flag #t)
	      (begin
		(display (format #f "debug init-date=~a, start-date=~a, restart-date=~a, end-date=~a~%"
				 init-date start-date restart-date end-date))
		(force-output)))

            ;;; now start accumulating statistics
	  (let ((loop-complete-flag #f)
		(this-date restart-date)
		(this-yyyymmdd (list-ref restart-date 4))
		(end-yyyymmdd (list-ref end-date 4)))
	    (begin
	      (while
	       (and (equal? loop-complete-flag #f)
		    (<= this-yyyymmdd end-yyyymmdd))
	       (begin
		 (let ((this-dow (list-ref this-date 3))
		       (this-day (list-ref this-date 0)))
		   (begin
		     (if (equal? this-day 1)
			 (set! first-month-count (+ first-month-count 1)))

		     (if (equal? this-dow 6)
			 (begin
			   (set! sunday-count (+ sunday-count 1))
			   (if (equal? this-day 1)
			       (begin
				 (set! sunday-first-count (+ sunday-first-count 1))
				 (if (equal? debug-flag #t)
				     (begin
				       (display (format #f "debug iterate-date-loop : first sunday ~a : ~a : ~a : ~a~%"
							this-date sunday-first-count sunday-count first-month-count))
				       (force-output)))
				 ))))
		     ))

		 (let ((next-date (increment-date this-date mdate-htable)))
		   (let ((next-yyyymmdd (list-ref next-date 4)))
		     (if (> next-yyyymmdd end-yyyymmdd)
			 (begin
			   (set! loop-complete-flag #t))
			 (begin
			   (set! this-date next-date)
			   ))))
		 ))
	      ))
	  ))
      (list sunday-first-count sunday-count first-month-count)
      )))

;;;#############################################################
;;;#############################################################
(define (test-iterate-date-loop-1)
  (let ((sub-name "test-iterate-date-loop-1")
	(test-list
	 (list
	  (list (list 1 1 1900 0 19000101) (list 2 1 1900 0 19000102)
		(list 1 2 1900 0 19000201) (list 0 4 1))
	  (list (list 1 1 1900 0 19000101) (list 2 1 1900 0 19000102)
		(list 1 1 1901 0 19010101) (list 2 52 12))
	  (list (list 1 1 1900 0 19000101) (list 1 1 1901 0 19010101)
		(list 1 1 1902 0 19020101) (list 2 52 13))
	  ))
	(debug-flag #f)
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-init (list-ref this-list 0))
		 (test-start (list-ref this-list 1))
		 (test-end (list-ref this-list 2))
		 (shouldbe-list (list-ref this-list 3)))
	     (let ((result-list (iterate-date-loop test-init test-start test-end debug-flag)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : initial date=~a, start=~a, end=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-init test-start
					test-end shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop init-date start-date end-date debug-flag)
  (let ((results (iterate-date-loop init-date start-date end-date debug-flag)))
    (begin
      (display (ice9-format:format #f "from ~a to ~a there were ~:d first of the months, ~:d sundays, and ~:d sundays that were first of the months~%"
				   (date-to-string start-date)
				   (date-to-string end-date)
				   (list-ref results 2)
				   (list-ref results 1)
				   (list-ref results 0)))
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
    (display (format #f "Problem 019 - You are given the following information, but you may prefer to do some research for yourself.~%"))
    (newline)
    (display (format #f "1 Jan 1900 was a Monday.~%"))
    (display (format #f "Thirty days has September, April, June and November.~%"))
    (display (format #f "All the rest have thirty-one, Saving February alone~%"))
    (display (format #f "Which has twenty-eight, rain or shine. And on leap years, twenty-nine.~%"))
    (display (format #f "A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.~%"))
    (newline)
    (display (format #f "How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-increment-date-1 counter)
	   (run-test test-date-to-string-1 counter)
	   (run-test test-recalc-day-of-week-1 counter)
	   (run-test test-iterate-date-loop-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((init-date (list 1 1 1900 0 19000101))
	  (start-date (list 1 1 1901 0 19010101))
	  (end-date (list 1 1 1902 0 19020101))
	  (debug-flag #f))
      (begin
	(main-loop init-date start-date end-date debug-flag)
	))

    (newline)

    (let ((init-date (list 1 1 1900 0 19000101))
	  (start-date (list 1 1 1901 0 19010101))
	  (end-date (list 31 12 2000 0 20001231))
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop init-date start-date end-date debug-flag)
	   ))
	))
    (newline)
    ))

