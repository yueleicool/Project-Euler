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

;;;### srfi-11 for let-values function
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

;;;### ice-0 rdelim for read-line functions
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
		       (display (format #f "~a : (~a) : error : number = ~a, list shouldbe = ~a, result list = ~a~%"
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
;;; returns a list of lists of digits
(define (read-in-keylogs filename)
  (let ((result-list (list)))
    (begin
      (if (file-exists? filename)
	  (begin
	    (with-input-from-file filename
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited "\n\r") (ice9-rdelim:read-delimited "\n\r")))
		    ((eof-object? line))
		  (begin
		    (if (not (eof-object? line))
			(begin
			  (let ((tmp-line (string-copy line)))
			    (let ((this-string-num (string-trim-both tmp-line)))
			      (if (not (equal? (string->number this-string-num) #f))
				  (begin
				    (let ((num (string->number this-string-num)))
				      (let ((dlist (split-digits-list num)))
					(set! result-list (cons dlist result-list))
					)))
				  )))
			  ))
		    ))
		))))
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (make-index-list pwd-list key-list)
  (begin
    (let ((index-list
	   (map
	    (lambda (this-key)
	      (srfi-1:list-index
	       (lambda (x) (= x this-key))
	       pwd-list))
	    key-list)))
      index-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-index-list-1)
  (let ((sub-name "test-make-index-list-1")
	(test-list
	 (list
	  (list (list 3 4 5) (list 3 1 2) (list 0 #f #f))
	  (list (list 1 2 3 4 5) (list 5 6 3) (list 4 #f 2))
	  (list (list 1 2 3 4 5) (list 7 2 9) (list #f 1 #f))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((pwd-list (list-ref alist 0))
		 (key-list (list-ref alist 1))
		 (shouldbe-list (list-ref alist 2)))
	     (let ((result-list (make-index-list pwd-list key-list)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : (~a) : error : pwd-list = ~a, key-list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index pwd-list key-list
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
(define (add-new-keys input-list key-list)
  (let ((pwd-list (list-copy input-list))
	(last-elem-2 #f)
	(last-elem-1 (car key-list))
	(elem-tail-list (cdr key-list)))
    (let ((last-index-1 (srfi-1:list-index
			 (lambda (x) (= x last-elem-1)) pwd-list))
	  (last-index-2 #f))
      (let ((tlen (length elem-tail-list)))
	(begin
	  (do ((jj 0 (1+ jj)))
	      ((>= jj tlen))
	    (begin
	      (let ((this-elem (list-ref elem-tail-list jj)))
		(let ((this-index
		       (srfi-1:list-index
			(lambda (x) (= x this-elem)) pwd-list)))
		  (begin
		    (cond
		     ((and (equal? last-index-1 #f)
			   (equal? this-index #f))
		      (begin
                        ;;; just append last-elem to the end
			(if (equal? jj (- tlen 1))
			    (begin
			      (let ((insert-list (list last-elem-1 this-elem)))
				(begin
				  (if (not (equal? last-elem-2 #f))
				      (begin
					(set! insert-list (cons last-elem-2 insert-list))
					(set! last-elem-2 #f)
					(set! last-index-2 #f))
				      ))
				(set! pwd-list (append pwd-list insert-list))
				))
			    (begin
			      (if (equal? last-elem-2 #f)
				  (begin
				    (set! last-elem-2 last-elem-1)
				    (set! last-index-2 last-index-1))
				  (begin
				    (set! pwd-list (append pwd-list (list last-elem-2)))
				    (set! last-elem-2 #f)
				    (set! last-index-2 #f)
				    ))
			      ;;;(set! pwd-list (append pwd-list (list last-elem)))
			      ))
			))
		     ((and (equal? last-index-1 #f)
			   (not (equal? this-index #f)))
		      (begin
                        ;;; add last-elem just before this-elem
			(let ((insert-list (list last-elem-1)))
			  (begin
			    (if (not (equal? last-elem-2 #f))
				(begin
				  (set! insert-list (cons last-elem-2 insert-list))
				  (set! last-elem-2 #f)
				  (set! last-index-2 #f)
				  ))
			    (let ((hlist (list-head pwd-list this-index))
				  (tlist (list-tail pwd-list this-index)))
			      (let ((next-list (append hlist insert-list tlist)))
				(begin
				  (set! pwd-list next-list)
				  )))))
			))
		     ((and (not (equal? last-index-1 #f))
			   (equal? this-index #f))
		      (begin
                        ;;; just append last-elem to the end
			(if (equal? jj (- tlen 1))
			    (begin
			      (set! pwd-list (append pwd-list (list this-elem)))
			      ))
			))
		     (else
		      (begin
                         ;;; both indicies were found in pwd-list
                         ;;; may need to swap places to ensure consistancy
			(if (> last-index-1 this-index)
			    (begin
			      (list-set! pwd-list this-index last-elem-1)
			      (list-set! pwd-list last-index-1 this-elem)
			      ))
			)))

		    (set! last-elem-1 this-elem)
		    (set! last-index-1 this-index)
		    )))
	      ))
	  pwd-list
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-add-new-keys-1)
  (let ((sub-name "test-add-new-keys-1")
	(test-list
	 (list
	  (list (list 3 4 5 8) (list 1 2 6) (list 3 4 5 8 1 2 6))
	  (list (list 3 4 5 8) (list 1 4 6) (list 3 1 4 5 8 6))
	  (list (list 3 4 5 8) (list 1 4 8) (list 3 1 4 5 8))
	  (list (list 3 4 5 8) (list 3 1 5) (list 3 4 1 5 8))
	  (list (list 3 4 5 8) (list 5 1 9) (list 3 4 5 8 1 9))
	  (list (list 3 4 5 8) (list 1 2 3) (list 1 2 3 4 5 8))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((pwd-list (list-ref alist 0))
		 (key-list (list-ref alist 1))
		 (shouldbe-list (list-ref alist 2)))
	     (let ((result-list (add-new-keys pwd-list key-list)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : (~a) : error : pwd-list = ~a, key-list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index pwd-list key-list
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
;;; password list is a list of digits
(define (calc-password-list kl-list-list)
  (let ((pwd-list (list-copy (car kl-list-list)))
	(first-list (car kl-list-list))
	(tail-list (cdr kl-list-list))
	(last-index 0))
    (begin
      ;;; insert into pwd-list
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((next-list (add-new-keys pwd-list a-list)))
	     (begin
	       (set! pwd-list next-list)
	       ))
	   ))
       tail-list)

      pwd-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-password-list-1)
  (let ((sub-name "test-calc-password-list-1")
	(test-list
	 (list
	  (list (list (list 3 1 9) (list 6 8 0))
		(list 3 1 9 6 8 0))
	  (list (list (list 3 1 9) (list 6 8 0) (list 1 8 0))
		(list 3 1 9 6 8 0))
	  (list (list (list 3 1 9) (list 6 8 0) (list 1 8 0) (list 6 9 0))
		(list 3 1 6 9 8 0))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((key-list (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (calc-password-list key-list)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : (~a) : error : key-list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index key-list
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
;;; password list is a list of digits
(define (check-password-consistency pwd-list kl-list-list)
  (let ((bresult #t)
	(missing-error-count 0)
	(placement-error-count 0))
    (begin
      ;;; insert into pwd-list
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((index-list (make-index-list pwd-list a-list))
		 (llen (length a-list)))
	     (begin
	       (if (and (list? index-list) (> (length index-list) 0))
		   (begin
		     (let ((last-index (car index-list)))
		       (begin
			 (do ((jj 1 (1+ jj)))
			     ((>= jj llen))
			   (begin
			     (let ((this-index (list-ref index-list jj)))
			       (begin
				 (cond
				  ((equal? this-index #f)
				   (begin
				     (set! bresult #f)
				     (set! missing-error-count (1+ missing-error-count))
				     ))
				  ((< this-index last-index)
				   (begin
				     (set! bresult #f)
				     (set! placement-error-count (1+ placement-error-count))
				     )))
				 (set! last-index this-index)
				 ))
			     ))
			 ))
		     ))
	       ))
	   ))
       kl-list-list)

      (if (or (> missing-error-count 0)
	      (> placement-error-count 0))
	  (begin
	    (display (format #f "check-password-consistency for ~a : missing error count = ~a, placement-error-count = ~a~%"
			     pwd-list missing-error-count placement-error-count))
	    (force-output)
	    ))
      bresult
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop filename)
  (let ((key-list (read-in-keylogs filename)))
    (begin
      (let ((pwd-list (calc-password-list key-list)))
	(let ((bresult (check-password-consistency pwd-list key-list)))
	  (begin
	    (display (ice9-format:format
		      #f "the hidden passcode is ~a~%"
		      pwd-list))
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
    (display (format #f "Problem 079 - A common security method used for online banking is to ask the user for three random characters from a passcode. For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.~%"))
    (newline)
    (display (format #f "The text file, keylog.txt, contains fifty successful login attempts.~%"))
    (newline)
    (display (format #f "Given that the three characters are always asked for in order, analyse the file so as to determine the shortest possible secret passcode of unknown length.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-make-index-list-1 counter)
	   (run-test test-add-new-keys-1 counter)
	   (run-test test-calc-password-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((filename "keylog.txt"))
      (begin
	(time-code
	 (begin
	   (main-loop filename)
	   ))
	))

    (newline)
    ))
