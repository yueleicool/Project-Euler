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

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

;;;### getopt-long used for command-line option arguments processing
(use-modules ((ice-9 getopt-long)
	      :renamer (symbol-prefix-proc 'ice-9-getopt:)))


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
;;; define a macro to simplify code
(define-syntax process-row-col
  (syntax-rules ()
    ((process-row-col local-array ii jj
		      end-rows-index end-cols-index)
     (begin
       (if (not (and (= ii end-rows-index)
		     (= jj end-cols-index)))
	   (begin
	     (let ((this-elem (array-ref local-array ii jj))
		   (min-elem 0))
	       (begin
		 (cond
		  ((equal? jj end-cols-index)
		   (begin
		     (let ((prev-elem1 (array-ref local-array (+ ii 1) jj)))
		       (begin
			 (set! min-elem prev-elem1)
			 ))
		     ))
		  ((equal? ii end-rows-index)
		   (begin
		     (let ((prev-elem2 (array-ref local-array ii (+ jj 1))))
		       (begin
			 (set! min-elem prev-elem2)
			 ))
		     ))
		  (else
		   (begin
		     (let ((prev-elem1 (array-ref local-array (+ ii 1) jj))
			   (prev-elem2 (array-ref local-array ii (+ jj 1))))
		       (begin
			 (set! min-elem (min prev-elem1 prev-elem2))
			 ))
		     )))
		 (array-set! local-array (+ min-elem this-elem) ii jj)
		 ))
	     ))
       ))
    ))


;;;#############################################################
;;;#############################################################
(define (dynamic-method array-2d max-rows max-cols)
  (let ((local-array (make-array 0 max-rows max-cols))
	(end-rows-index (- max-rows 1))
	(end-cols-index (- max-cols 1)))
    (begin
      (array-copy! array-2d local-array)

      (cond
       ((<= max-rows 1) -1)
       (else
	(begin
	  (do ((ii end-rows-index (- ii 1)))
	      ((< ii 0))
	    (begin
	      (do ((jj end-cols-index (- jj 1)))
		  ((< jj 0))
		(begin
		  (process-row-col local-array ii jj
				   end-rows-index end-cols-index)
		  ))
	      ))

	  (array-ref local-array 0 0)
	  ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-dynamic-method-1)
  (let ((sub-name "test-dynamic-method-1")
	(test-list
	 (list
	  (list (list (list 1 2 3 4)
		      (list 3 5 9 8)
		      (list 7 11 13 19)
		      (list 44 33 17 8))
		45)
	  (list (list (list 131 673 234 103 18)
		      (list 201 96 342 965 150)
		      (list 630 803 746 422 111)
		      (list 537 699 497 121 956)
		      (list 805 732 524 37 331))
		2427)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((input-list-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((array-2d (list->array 2 input-list-list))
		   (max-rows (length input-list-list))
		   (max-cols (length input-list-list)))
	       (let ((result (dynamic-method array-2d max-rows max-cols)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : input-list-list = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index input-list-list
					  shouldbe result))
			 (quit)
			 ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (display-2d-array this-array max-row max-col)
  (cond
   ((not (array? this-array))
    (begin
      (display (format #f "display-array error: expecting array, instead received ~a~%" this-array))
      (quit)))
   (else
    (begin
      (do ((ii-row 0 (1+ ii-row)))
	  ((>= ii-row max-row))
	(begin
	  (do ((ii-col 0 (1+ ii-col)))
	      ((>= ii-col max-col))
	    (begin
	      (display
	       (ice9-format:format #f "~4:d "
				   (array-ref this-array ii-row ii-col)))
	      ))
	  (newline)
	  ))
      ))))

;;;#############################################################
;;;#############################################################
;;; returns a list of lists
(define (read-in-file fname)
  (let ((results-list-list (list))
	(counter 0))
    (begin
      (if (file-exists? fname)
	  (begin
	    (with-input-from-file fname
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited "\r\n") (ice9-rdelim:read-delimited "\r\n")))
		    ((eof-object? line))
		  (begin
		    (cond
		     ((and (not (eof-object? line))
			   (> (string-length line) 0))
		      (let ((this-string-list (string-split (string-trim-both line) #\,)))
			(begin
			  (let ((this-list
				 (map
				  (lambda (this-string)
				    (if (and (string? this-string)
					     (not (equal? (string->number this-string) #f)))
					(string->number this-string)
					-1))
				  this-string-list)))
			    (begin
			      (set! results-list-list (cons this-list results-list-list))
			      ))))))
		    ))
		))
	    (reverse results-list-list))
	  (begin
	    (list)
	    )))))

;;;#############################################################
;;;#############################################################
(define (main-loop tarray max-rows max-cols debug-flag)
  (let ((path-sum (dynamic-method tarray max-rows max-cols)))
    (begin
      (if (equal? debug-flag #t)
	  (begin
	    (display-2d-array tarray max-rows max-cols)
	    ))

      (display (ice9-format:format #f "minimal path sum = ~:d~%"
				   path-sum))
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
    (display (format #f "Problem 081 - In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only moving to the right and down, is indicated in bold red and is equal to 2427.~%"))
    (newline)
    (display (format #f "  131  673  234  103  18~%"))
    (display (format #f "  201  96   342  965  150~%"))
    (display (format #f "  630  803  746  422  111~%"))
    (display (format #f "  537  699  497  121  956~%"))
    (display (format #f "  805  732  524  37   331~%"))
    (newline)
    (display (format #f "Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.~%"))
    (newline)
    (display (format #f "The solution method can be found at http://www.mathblog.dk/project-euler-81-find-the-minimal-path-sum-from-the-top-left-to-the-bottom-right-by-moving-right-and-down/~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-dynamic-method-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((tlist
	   (list
	    (list 131 673 234 103 18)
	    (list 201 96 342 965 150)
	    (list 630 803 746 422 111)
	    (list 537 699 497 121 956)
	    (list 805 732 524 37 331)))
	  (max-rows 5)
	  (max-cols 5)
	  (debug-flag #t))
      (let ((tarray (list->array 2 tlist)))
	(begin
	  (main-loop tarray max-rows max-cols debug-flag)
	  )))

    (newline)
    (force-output)

    (time-code
     (begin
       (let ((filename "matrix.txt"))
	 (let ((tlist (read-in-file filename)))
	   (let ((max-rows (length tlist))
		 (max-cols (length (car tlist)))
		 (debug-flag #f))
	     (let ((tarray (list->array 2 tlist)))
	       (begin
		 (display (format #f "read in file ~a, rows = ~a, cols = ~a : ~a~%"
				  filename max-rows max-cols
				  (date-time-to-string (srfi-19:current-date))))
		 (force-output)
		 (main-loop tarray max-rows max-cols debug-flag)
		 ))
	     )))
       ))

    (newline)
    ))
