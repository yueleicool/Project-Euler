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

;;;### srfi-11 for let-values (multiple value bind)
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
;;; shuffle operator groupings
(define-syntax parens-lists
  (syntax-rules ()
    ((parens-lists num1 op1 num2 op2 num3 op3 num4)
     (begin
       (let ((result-list
	      (list
	       (list op1 num1 (list op2 num2 (list op3 num3 num4)))
	       (list op1 num1 (list op3 (list op2 num2 num3) num4))
	       (list op2 (list op1 num1 num2) (list op3 num3 num4))
	       (list op3 (list op1 num1 (list op2 num2 num3)) num4)
	       (list op3 (list op2 (list op1 num1 num2) num3) num4)
	       )))
	 (begin
	   result-list
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (make-num-lists-lists start-digit end-digit)
  (let ((acc-list (list)))
    (begin
      (do ((ii-1 start-digit (1+ ii-1)))
	  ((> ii-1 end-digit))
	(begin
	  (let ((c1-list (list ii-1)))
	    (begin
	      (do ((ii-2 start-digit (1+ ii-2)))
		  ((> ii-2 end-digit))
		(begin
		  (if (not (equal? ii-1 ii-2))
		      (begin
			(let ((c2-list (cons ii-2 c1-list)))
			  (begin
			    (do ((ii-3 start-digit (1+ ii-3)))
				((> ii-3 end-digit))
			      (begin
				(if (and (not (equal? ii-3 ii-1))
					 (not (equal? ii-3 ii-2)))
				    (begin
				      (let ((c3-list (cons ii-3 c2-list)))
					(begin
					  (do ((ii-4 start-digit (1+ ii-4)))
					      ((> ii-4 end-digit))
					    (begin
					      (if (and (not (equal? ii-4 ii-1))
						       (not (equal? ii-4 ii-2))
						       (not (equal? ii-4 ii-3)))
						  (begin
						    (let ((c4-list (cons ii-4 c3-list)))
						      (begin
							(set! acc-list (cons c4-list acc-list))
							))
						    ))
					      ))
					  ))
				      ))
				))
			    ))
			))
		  ))
	      ))
	  ))
      (reverse acc-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-num-lists-lists-1)
  (let ((sub-name "test-make-num-lists-lists-1")
	(test-list
	 (list
	  (list 0 2 (list))
	  (list 0 3 (list (list 3 2 1 0) (list 2 3 1 0) (list 3 1 2 0)
			  (list 1 3 2 0) (list 2 1 3 0) (list 1 2 3 0)
			  (list 3 2 0 1) (list 2 3 0 1) (list 3 0 2 1)
			  (list 0 3 2 1) (list 2 0 3 1) (list 0 2 3 1)
			  (list 3 1 0 2) (list 1 3 0 2) (list 3 0 1 2)
			  (list 0 3 1 2) (list 1 0 3 2) (list 0 1 3 2)
			  (list 2 1 0 3) (list 1 2 0 3) (list 2 0 1 3)
			  (list 0 2 1 3) (list 1 0 2 3) (list 0 1 2 3)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((start-digit (list-ref alist 0))
		 (end-digit (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (make-num-lists-lists start-digit end-digit)))
	       (let ((slen (length shouldbe))
		     (rlen (length result)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : (~a) : error : start-digit = ~a, end-digit = ~a, shouldbe = ~a, result = ~a : length's differ, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index start-digit end-digit
					  shouldbe result slen rlen))
			 (quit)
			 ))

		   (for-each
		    (lambda (s-list)
		      (begin
			(if (equal? (member s-list result) #f)
			    (begin
			      (display (format #f "~a : (~a) : error : start-digit = ~a, end-digit = ~a, shouldbe = ~a, result = ~a : differ at element = ~a~%"
					       sub-name test-label-index start-digit end-digit
					       shouldbe result s-list))
			      (quit)
			      ))
			)) shouldbe)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; evaluate tree
(define (evaluate-tree-list tree-list)
  (begin
    (if (and (list? tree-list) (> (length tree-list) 1))
	(begin
	  (let ((operator (list-ref tree-list 0))
		(first-expr (list-ref tree-list 1))
		(second-expr (list-ref tree-list 2)))
	    (begin
	      (if (list? first-expr)
		  (begin
		    (let ((result (evaluate-tree-list first-expr)))
		      (set! first-expr result))
		    ))
	      (if (list? second-expr)
		  (begin
		    (let ((result (evaluate-tree-list second-expr)))
		      (set! second-expr result))
		    ))
	      (if (and (equal? operator /)
		       (equal? second-expr 0))
		  (begin
		    #f)
		  (begin
		    (if (and (not (equal? first-expr #f))
			     (not (equal? second-expr #f)))
			(begin
			  (operator first-expr second-expr))
			(begin
			  #f
			  ))
		    ))
	      )))
	(begin
	  #f
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-evaluate-tree-list-1)
  (let ((sub-name "test-evaluate-tree-list-1")
	(test-list
	 (list
	  (list (list + 1 3) 4)
	  (list (list / (list + 1 3) 2) 2)
	  (list (list / (list + 3 1) 2) 2)
	  (list (list / (list * 5 (list + 1 3)) 2) 10)
	  (list (list / (list * 4 (list + 1 3)) 2) 8)
	  (list (list - (list * 4 (list + 2 3)) 1) 19)
	  (list (list * (list * 3 4) (list + 2 1)) 36)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (evaluate-tree-list test-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : list = ~a, shouldbe = ~s, result = ~s~%"
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
(define (find-min-max-from-hash results-htable)
  (let ((min -1)
	(max -1))
    (begin
      (hash-for-each
       (lambda (key value)
	 (begin
	   (if (or (< min 0)
		   (< key min))
	       (begin
		 (set! min key)
		 ))
	   (if (or (< max 0)
		   (> key max))
	       (begin
		 (set! max key)
		 ))
	   )) results-htable)
      (list min max)
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-min-max-from-hash-1)
  (let ((sub-name "test-find-min-max-from-hash-1")
	(test-list
	 (list
	  (list (list (list 1 2) (list 2 3)) (list 1 2))
	  (list (list (list 1 2) (list 2 3) (list 3 3)) (list 1 3))
	  (list (list (list 4 2) (list 2 3) (list 3 3)) (list 2 4))
	  ))
	(data-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list-list (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (begin
	       (hash-clear! data-htable)
	       (for-each
		(lambda (a-list)
		  (let ((key (list-ref a-list 0))
			(value (list-ref a-list 1)))
		    (begin
		      (hash-set! data-htable key value)
		      ))) test-list-list)

	       (let ((result-list (find-min-max-from-hash data-htable)))
		 (begin
		   (if (not (equal? shouldbe-list result-list))
		       (begin
			 (display (format #f "~a : (~a) : error : test list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-list-list
					  shouldbe-list result-list))
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
(define (hash-size results-htable)
  (let ((count 0))
    (begin
      (hash-for-each
       (lambda (key value)
	 (begin
	   (set! count (1+ count))
	   )) results-htable)
      count
      )))

;;;#############################################################
;;;#############################################################
(define (test-hash-size-1)
  (let ((sub-name "test-hash-size-1")
	(test-list
	 (list
	  (list (list (list 1 2) (list 2 3)) 2)
	  (list (list (list 1 2) (list 2 3) (list 3 3)) 3)
	  (list (list (list 4 2) (list 2 3) (list 3 3) (list 9 99)) 4)
	  ))
	(data-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (begin
	       (hash-clear! data-htable)
	       (for-each
		(lambda (a-list)
		  (let ((key (list-ref a-list 0))
			(value (list-ref a-list 1)))
		    (begin
		      (hash-set! data-htable key value)
		      ))) test-list-list)

	       (let ((result (hash-size data-htable)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : (~a) : error : test list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-list-list
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
(define (max-consecutive-keys-hash results-htable)
  (let ((ok-flag #t)
	(max-size 0)
	(hsize (hash-size results-htable)))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((or (> ii hsize)
	       (equal? ok-flag #f)))
	(begin
	  (let ((this-value (hash-ref results-htable ii #f)))
	    (begin
	      (if (equal? this-value #f)
		  (begin
		    (set! ok-flag #f))
		  (begin
		    (set! max-size ii)
		    ))
	      ))
	  ))
      max-size
      )))

;;;#############################################################
;;;#############################################################
(define (test-max-consecutive-keys-hash-1)
  (let ((sub-name "test-max-consecutive-keys-hash-1")
	(test-list
	 (list
	  (list (list (list 1 2) (list 2 3)) 2)
	  (list (list (list 1 2) (list 2 3) (list 3 3)) 3)
	  (list (list (list 4 2) (list 2 3) (list 3 3) (list 1 99)) 4)
	  (list (list (list 7 2) (list 2 3) (list 3 3) (list 1 99)) 3)
	  ))
	(data-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (begin
	       (hash-clear! data-htable)
	       (for-each
		(lambda (a-list)
		  (let ((key (list-ref a-list 0))
			(value (list-ref a-list 1)))
		    (begin
		      (hash-set! data-htable key value)
		      ))) test-list-list)

	       (let ((result (max-consecutive-keys-hash data-htable)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : (~a) : error : test list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-list-list
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
(define (list-to-string a-list-list)
  (if (and (list? a-list-list) (> (length a-list-list) 1))
      (begin
	(let ((operator (list-ref a-list-list 0))
	      (first-expr (list-ref a-list-list 1))
	      (second-expr (list-ref a-list-list 2)))
	  (begin
	    (if (list? first-expr)
		(begin
		  (let ((result (list-to-string first-expr)))
		    (begin
		      (set! first-expr result)
		      ))
		  ))
	    (if (list? second-expr)
		(begin
		  (let ((result (list-to-string second-expr)))
		    (begin
		      (set! second-expr result)
		      ))
		  ))
	    (cond
	     ((equal? operator +)
	      (begin
		(set! operator "+")
		))
	     ((equal? operator -)
	      (begin
		(set! operator "-")
		))
	     ((equal? operator *)
	      (begin
		(set! operator "*")
		))
	     ((equal? operator /)
	      (begin
		(set! operator "/")
		))
	     (else
	      (begin
		(set! operator (format #f "~a" operator))
		)))
	    (format #f "(~a ~a ~a)" operator first-expr second-expr)
	    ))
	)))

;;;#############################################################
;;;#############################################################
(define (test-list-to-string-1)
  (let ((sub-name "test-list-to-string-1")
	(test-list
	 (list
	  (list (list + 1 3) "(+ 1 3)")
	  (list (list / (list + 1 3) 2) "(/ (+ 1 3) 2)")
	  (list (list / (list + 3 1) 2) "(/ (+ 3 1) 2)")
	  (list (list / (list * 5 (list + 1 3)) 2) "(/ (* 5 (+ 1 3)) 2)")
	  (list (list / (list * 4 (list + 1 3)) 2) "(/ (* 4 (+ 1 3)) 2)")
	  (list (list - (list * 4 (list + 2 3)) 1) "(- (* 4 (+ 2 3)) 1)")
	  (list (list * (list * 3 4) (list + 2 1)) "(* (* 3 4) (+ 2 1))")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list-list (list-ref this-list 0))
		 (shouldbe-string (list-ref this-list 1)))
	     (let ((result-string (list-to-string test-list-list)))
	       (begin
		 (if (not (string-ci=? shouldbe-string result-string))
		     (begin
		       (display (format #f "~a : (~a) : error : test list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list-list
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
;;; define a macro to simplify code
(define-syntax evaluate-all-combinations
  (syntax-rules ()
    ((evaluate-all-combinations num1 num2 num3 num4 op1 op2 op3
				num-key seq-htable set-htable)
     (begin
       (let ((combo-lists (parens-lists num1 op1 num2 op2 num3 op3 num4)))
	 (begin
	   (for-each
	    (lambda (c-list)
	      (begin
		(let ((value (evaluate-tree-list c-list)))
		  (begin
		    (if (and (number? value)
			     (integer? value)
			     (> value 0))
			(begin
			  (let ((c-flag (hash-ref seq-htable value #f)))
			    (begin
			      (if (equal? c-flag #f)
				  (begin
				    (let ((c-string (list-to-string c-list)))
				      (begin
					(hash-set! seq-htable value c-string)
					))
				    ))
			      ))
			  ))
		    ))
		)) combo-lists)
	   (hash-set! set-htable num-key seq-htable)
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; set-htable key=(list num1 num2 num3 num4), value=sequence-hash-table
;;; sequence-htable key=expr numeric value, value=string representation
(define (populate-set-sequence-hash! set-htable all-num-list-list max-len)
  (let ((oper-list (list + - * /)))
    (begin
      (for-each
       (lambda (num-list)
	 (begin
	   (let ((num1 (list-ref num-list 0))
		 (num2 (list-ref num-list 1))
		 (num3 (list-ref num-list 2))
		 (num4 (list-ref num-list 3))
		 (num-key (sort num-list <)))
	     (let ((seq-htable (hash-ref set-htable num-key
					 (make-hash-table 100))))
	       (begin
		 (for-each
		  (lambda (op1)
		    (begin
		      (for-each
		       (lambda (op2)
			 (begin
			   (for-each
			    (lambda (op3)
			      (begin
				(evaluate-all-combinations num1 num2 num3 num4 op1 op2 op3
							   num-key seq-htable set-htable)
				)) oper-list)
			   )) oper-list)
		      )) oper-list)
		 )))
	   )) all-num-list-list)
      )))

;;;#############################################################
;;;#############################################################
(define (compute-max-consecutive num-list max-nn completed-htable)
  (let ((max-consecutive 0)
	(max-set (list)))
    (begin
      (populate-set-sequence-hash! completed-htable num-list max-nn)

      (hash-for-each
       (lambda (num-set r-htable)
	 (begin
	   (let ((hsize (hash-size r-htable))
		 (max-con (max-consecutive-keys-hash r-htable)))
	     (begin
	       (if (< max-consecutive max-con)
		   (begin
		     (set! max-consecutive max-con)
		     (set! max-set num-set)
		     ))
	       ))
	   )) completed-htable)

      (list max-consecutive max-set)
      )))

;;;#############################################################
;;;#############################################################
(define (find-max-key result-htable)
  (let ((max-key
	 (hash-fold
	  (lambda (key value prior)
	    (begin
	      (max key prior)
	      )) 0 result-htable)))
    (begin
      max-key
      )))

;;;#############################################################
;;;#############################################################
(define (display-results max-set max-consecutive r-htable debug-flag)
  (let ((hsize (hash-size r-htable)))
    (begin
      (display (ice9-format:format #f "  maximum set = ~a~%" max-set))
      (display (ice9-format:format #f "  maximum string = ~a~%"
				   (string-join (map number->string max-set) "")))
      (display (ice9-format:format #f "  number of results found = ~a~%" hsize))
      (display (ice9-format:format #f "  maximum consecutive results to ~a~%" max-consecutive))
      (let ((max-key (find-max-key r-htable)))
	(begin
	  (display (ice9-format:format #f "  largest target number = ~a~%" max-key))
	  ))

      (if (equal? debug-flag #t)
	  (begin
	    (hash-for-each
	     (lambda (key value)
	       (begin
		 (if (and (string? value)
			  (> (string-length value) 0))
		     (begin
		       (display (ice9-format:format #f "  ~a = ~a~%" key value))
		       ))
		 )) r-htable)
	    ))

      (force-output)
      )))

;;;#############################################################
;;;#############################################################
(define (reproduce-problem-statement)
  (let ((num-list (make-num-lists-lists 1 4))
	(max-nn 4)
	(completed-htable (make-hash-table 100)))
    (let ((max-lists (compute-max-consecutive num-list max-nn completed-htable)))
      (begin
	(let ((max-consecutive (list-ref max-lists 0))
	      (max-set (list-ref max-lists 1)))
	  (let ((r-htable (hash-ref completed-htable max-set #f)))
	    (begin
	      (if (not (equal? r-htable #f))
		  (begin
		    (display-results max-set max-consecutive r-htable #t)
		    ))
	      )))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num end-num max-nn debug-flag)
  (let ((num-list (make-num-lists-lists start-num end-num))
	(completed-htable (make-hash-table 100)))
    (let ((max-lists (compute-max-consecutive num-list max-nn completed-htable)))
      (begin
	(let ((max-consecutive (list-ref max-lists 0))
	      (max-set (list-ref max-lists 1)))
	  (let ((r-htable (hash-ref completed-htable max-set #f)))
	    (begin
	      (if (not (equal? r-htable #f))
		  (begin
		    (display-results max-set max-consecutive r-htable debug-flag)
		    ))
	      )))
	))
    ))

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
    (display (format #f "Project Euler 93 - By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic operations (+, -, *, /) and brackets/parentheses, it is possible to form different positive integer targets.~%"))
    (newline)
    (display (format #f "For example,~%"))
    (display (format #f "  8 = (4 * (1 + 3)) / 2~%"))
    (display (format #f "  14 = 4 * (3 + 1 / 2)~%"))
    (display (format #f "  19 = 4 * (2 + 3) - 1~%"))
    (display (format #f "  36 = 3 * 4 * (2 + 1)~%"))
    (newline)
    (display (format #f "Note that concatenations of the digits, like 12 + 34, are not allowed.~%"))
    (newline)
    (display (format #f "Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.~%"))
    (newline)
    (display (format #f "Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 1 to n, can be obtained, giving your answer as a string: abcd.~%"))
    (newline)
    (display (format #f "the solution is described at http://dwaynecrooks.wordpress.com/2010/07/23/project-euler-problem-93/~%"))
    (display (format #f "for more details on trees and parsing, see http://www.cs.berkeley.edu/~~bh/ssch18/trees.html~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-num-lists-lists-1 counter)
	   (run-test test-evaluate-tree-list-1 counter)
	   (run-test test-find-min-max-from-hash-1 counter)
	   (run-test test-hash-size-1 counter)
	   (run-test test-max-consecutive-keys-hash-1 counter)
	   (run-test test-list-to-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (time-code
     (begin
       (reproduce-problem-statement)
       ))

    (newline)
    (force-output)

    (let ((start-num 0)
	  (end-num 9)
	  (max-nn 4)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num max-nn debug-flag)
	   ))
	))

    (newline)
    ))
