#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format function
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice9-format:)))

;;;### ice-9 rdelim - for read-delimited
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

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
(define (populate-alpha-hash! alpha-htable)
  (let ((alist
	 (list
	  (list #\a 1) (list #\b 2) (list #\c 3) (list #\d 4)
	  (list #\e 5) (list #\f 6) (list #\g 7) (list #\h 8)
	  (list #\i 9) (list #\j 10) (list #\k 11) (list #\l 12)
	  (list #\m 13) (list #\n 14) (list #\o 15) (list #\p 16)
	  (list #\q 17) (list #\r 18) (list #\s 19) (list #\t 20)
	  (list #\u 21) (list #\v 22) (list #\w 23) (list #\x 24)
	  (list #\y 25) (list #\z 26))))
    (begin
      (for-each
       (lambda (this-list)
	 (let ((tchar (list-ref this-list 0))
	       (tnum (list-ref this-list 1)))
	   (hash-set! alpha-htable tchar tnum)))
       alist)
      )))

;;;#############################################################
;;;#############################################################
(define (populate-triangle-hash! triangle-htable max-num)
  (let ((max-tn 0))
    (begin
      (do ((ii max-num (- ii 1)))
	  ((<= ii 0))
	(begin
	  (let ((tn (euclidean/ (* ii (+ ii 1)) 2)))
	    (begin
	      (if (> tn max-tn)
		  (set! max-tn tn))
	      (hash-set! triangle-htable tn ii)
	      ))))
      max-tn
      )))

;;;#############################################################
;;;#############################################################
;;; returns a list of names, lower case
(define (read-in-file fname)
  (let ((name-list (list))
	(counter 0))
    (begin
      (if (file-exists? fname)
	  (begin
	    (with-input-from-file fname
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited ",\n") (ice9-rdelim:read-delimited ",\n")))
		    ((eof-object? line))
		  (begin
		    (cond
		     ((not (eof-object? line))
		      (let ((this-string (string-downcase (string-delete #\" line))))
			(begin
			  (set! name-list (cons this-string name-list))
			  (set! counter (1+ counter))
			  )))))
		  )))

	    (display (ice9-format:format #f "number of names read = ~:d~%" counter))
	    (force-output)
	    (set! name-list (sort name-list string-ci<?)))

	  (begin
	    (list)
	    )))))

;;;#############################################################
;;;#############################################################
;;; returns a list of names, lower case
(define (process-list name-list max-num)
  (let ((list-counter 0)
	(tn-name-count 0)
	(char-htable (make-hash-table 30))
	(triangle-htable (make-hash-table max-num)))
    (let ((max-triangle-num (populate-triangle-hash! triangle-htable max-num)))
      (begin
	(populate-alpha-hash! char-htable)

	(for-each
	 (lambda (this-name)
	   (begin
	     (set! list-counter (1+ list-counter))

	     (let ((slist (string->list this-name))
		   (this-sum 0))
	       (begin
		 (for-each
		  (lambda (this-char)
		    (let ((this-value (hash-ref char-htable this-char 0)))
		      (set! this-sum (+ this-sum this-value))
		      ))
		  slist)

		 (if (> this-sum max-triangle-num)
		     (begin
		       (display (ice9-format:format #f "process-list() error for name=~a, this-sum=~:d > max-triangle-num=~:d, re-run with larger max-num=~:d~%"
						    this-name this-sum max-triangle-num max-num))
		       (quit)
		       ))

		 (let ((tnum (hash-ref triangle-htable this-sum #f)))
		   (if (not (equal? tnum #f))
		       (begin
			 (set! tn-name-count (1+ tn-name-count))
			 )))
		 ))
	     ))
	 name-list)

	tn-name-count
	))))

;;;#############################################################
;;;#############################################################
(define (test-process-list-1)
  (let ((sub-name "test-process-list-1")
	(test-list
	 (list
	  (list (list "sky") 1000 1)
	  (list (list "a" "sky") 1000 2)
	  (list (list "a" "b" "sky") 1000 2)
	  (list (list "a" "b" "c" "sky") 1000 3)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (max-num (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (process-list test-list max-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
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
(define (main-loop filename max-num)
  (begin
    (let ((words-list (read-in-file filename)))
      (let ((triangle-word-count (process-list words-list max-num)))
	(begin
	  (display (ice9-format:format #f "number of triangle words = ~:d~%" triangle-word-count))
	  )))
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
    (display (format #f "Problem 042 - The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:~%"))
    (newline)
    (display (format #f "    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...~%"))
    (newline)
    (display (format #f "By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.~%"))
    (newline)
    (display (format #f "Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-process-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((filename "words.txt")
	  (max-num 10000))
      (begin
	(time-code
	 (begin
	   (main-loop filename max-num)
	   ))
	))

    (newline)
    ))
