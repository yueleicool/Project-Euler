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

;;;### ice-9 receive - receive multiple values
(use-modules ((ice-9 receive)
	      :renamer (symbol-prefix-proc 'ice9-receive:)))

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
(define (calc-triangular-list aa bb)
  (let ((cc 0)
	(rr 0))
    (begin
      (ice9-receive:receive
       (cc rr)
       (exact-integer-sqrt (+ (* aa aa) (* bb bb)))
       (begin
	 (if (zero? rr)
	     (list cc rr (+ aa bb cc))
	     (list cc rr 0)
	     ))))))

;;;#############################################################
;;;#############################################################
(define (test-calc-triangular-list-1)
  (let ((sub-name "test-calc-triangular-list-1")
	(test-list
	 (list
	  (list 3 4 (list 5 0 12))
	  (list 3 5 (list 5 9 0))
	  (list 4 5 (list 6 5 0))
	  (list 20 48 (list 52 0 120))
	  (list 24 45 (list 51 0 120))
	  (list 30 40 (list 50 0 120))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-aa (list-ref alist 0))
		 (test-bb (list-ref alist 1))
		 (shouldbe-list (list-ref alist 2)))
	     (let ((result-list (calc-triangular-list test-aa test-bb)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : aa = ~a, bb = ~a, list shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-aa test-bb
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
(define (list-to-set-string llist)
  (let ((stmp (string-join
	       (map
		(lambda (num)
		  (ice9-format:format #f "~:d" num))
		llist) ", ")))
    (let ((stmp2 (string-append "{ " stmp " }")))
      (begin
	stmp2
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-list-to-set-string-1)
  (let ((sub-name "test-list-to-set-string-1")
	(test-list
	 (list
	  (list (list 1) "{ 1 }")
	  (list (list 1 2) "{ 1, 2 }")
	  (list (list 1 2 3) "{ 1, 2, 3 }")
	  (list (list 4 5 6 7) "{ 4, 5, 6, 7 }")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-set-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num max-perimeter debug-flag)
  (begin
    (let ((counter 0)
	  (perimeter-htable (make-hash-table 1000)))
      (begin
	(do ((aa 1 (+ aa 1)))
	    ((>= aa max-num))
	  (begin
	    (do ((bb aa (+ bb 1)))
		((> bb max-num))
	      (begin
		(let ((result-list (calc-triangular-list aa bb)))
		  (let ((cc (list-ref result-list 0))
			(rr (list-ref result-list 1))
			(perim (list-ref result-list 2)))
		    (begin
		      (if (zero? rr)
			  (begin
			    ;;; have a triangular triple, (aa, bb, cc)
			    (let ((tlist (hash-ref perimeter-htable perim #f))
				  (current-list (list aa bb cc perim)))
			      (if (equal? tlist #f)
				  (begin
				    (hash-set! perimeter-htable perim (list current-list)))
				  (begin
				    (let ((next-list (append tlist (list current-list))))
				      (hash-set! perimeter-htable perim next-list)))
				  ))

			    (if (equal? debug-flag #t)
				(begin
				  (display (ice9-format:format #f "  perimeter = ~:d : list = ~a~%"
							       perim result-list))
				  (force-output)
				  ))

			    (set! counter (+ counter 1))
			    ))

		      )))
		))
	    ))

	(let ((hkey-list
	       (sort (hash-map->list
		      (lambda (key value)
			key)
		      perimeter-htable)
		     <))
	      (largest-length 0)
	      (largest-list (list))
	      (largest-perimeter 0))
	  (begin
	    (for-each
	     (lambda (this-key)
	       (let ((this-list-list
		      (hash-ref perimeter-htable this-key #f)))
		 (if (and
		      (not (equal? this-list-list #f))
		      (<= this-key max-perimeter))
		     (begin
		       (let ((num-ways (length this-list-list)))
			 (begin
			   (if (> num-ways largest-length)
			       (begin
				 (set! largest-length num-ways)
				 (set! largest-list this-list-list)
				 (set! largest-perimeter this-key)
				 ))
			   )))
		     )))
	     hkey-list)

	    (display (ice9-format:format #f "perimeter = ~:d has the largest number of solutions (~:d), for a right triangle, with perimeter <= ~:d, and a, b < ~:d~%"
					 largest-perimeter largest-length max-perimeter max-num))

	    (let ((this-string ""))
	      (begin
		(for-each
		 (lambda (this-list)
		   (let ((set-string (list-to-set-string (list-head this-list 3))))
		     (if (string-ci=? this-string "")
			 (set! this-string (format #f "~a" set-string))
			 (set! this-string (format #f "~a, ~a" this-string set-string))
			 )))
		 largest-list)

		(display (ice9-format:format #f "  ~:d : number of solutions = ~:d : ~a~%"
					     largest-perimeter largest-length
					     this-string))
		))
	    ))))))

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
    (display (format #f "Problem 039 - If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.~%"))
    (newline)
    (display (format #f "{20,48,52}, {24,45,51}, {30,40,50}~%"))
    (newline)
    (display (format #f "For which value of p <= 1000, is the number of solutions maximised?~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-calc-triangular-list-1 counter)
	   (run-test test-list-to-set-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 10)
	  (max-perimeter 100)
	  (debug-flag #t))
      (begin
	(main-loop max-num max-perimeter debug-flag)
	))

    (newline)
    (force-output)

    (let ((max-num 1000)
	  (max-perimeter 1000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num max-perimeter debug-flag)
	   ))
	))

    (newline)
    ))
