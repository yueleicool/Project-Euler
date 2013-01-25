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

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

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
(define (update-next-edge-node weight-array-2d min-array-2d max-rows max-cols
			       current-node-list remaining-node-list)
  (let ((remain-len (length remaining-node-list))
	(current-len (length current-node-list))
	(min-weight -1)
	(min-curr-node -1)
	(min-remain-node -1))
    (begin
      (if (<= current-len 0)
	  (begin
	    (set! current-len 1)
	    (set! current-node-list (list 0))
	    ))
      (for-each
       (lambda (curr-node)
	 (begin
	   (for-each
	    (lambda (remain-node)
	      (begin
		(if (not (= remain-node curr-node))
		    (begin
		      (let ((weight (array-ref weight-array-2d curr-node remain-node)))
			(begin
			  (if (and (> weight 0)
				   (or (<= min-weight 0)
				       (< weight min-weight)))
			      (begin
				(set! min-weight weight)
				(set! min-curr-node curr-node)
				(set! min-remain-node remain-node)
				))
			  ))
		      ))
		)) remaining-node-list)
	   )) current-node-list)

      (array-set! min-array-2d min-weight min-curr-node min-remain-node)
      (array-set! min-array-2d min-weight min-remain-node min-curr-node)

      min-remain-node
      )))

;;;#############################################################
;;;#############################################################
(define (test-update-next-edge-node-1)
  (let ((sub-name "test-update-next-edge-node-1")
	(test-list
	 (list
	  (list	(list (list -1 99 88) (list 99 -1 101) (list 88 101 -1))
		3 3 (list 0) (list 1 2)
		2 (list (list -1 -1 88) (list -1 -1 -1) (list 88 -1 -1)))
	  (list	(list (list -1 16 12 21 -1 -1 -1)
		      (list 16 -1 -1 17 20 -1 -1)
		      (list 12 -1 -1 28 -1 31 -1)
		      (list 21 17 28 -1 18 19 23)
		      (list -1 20 -1 18 -1 -1 11)
		      (list -1 -1 31 19 -1 -1 27)
		      (list -1 -1 -1 23 11 27 -1))
		7 7 (list 0) (list 1 2 3 4 5 6)
		2 (list (list -1 -1 12 -1 -1 -1 -1)
			(list -1 -1 -1 -1 -1 -1 -1)
			(list 12 -1 -1 -1 -1 -1 -1)
			(list -1 -1 -1 -1 -1 -1 -1)
			(list -1 -1 -1 -1 -1 -1 -1)
			(list -1 -1 -1 -1 -1 -1 -1)
			(list -1 -1 -1 -1 -1 -1 -1)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((weight-list-list (list-ref this-list 0))
		 (max-rows (list-ref this-list 1))
		 (max-cols (list-ref this-list 2))
		 (current-nodes (list-ref this-list 3))
		 (remaining-nodes (list-ref this-list 4))
		 (shouldbe-node (list-ref this-list 5))
		 (shouldbe-list-list (list-ref this-list 6)))
	     (let ((weight-arr-2d (list->array 2 weight-list-list))
		   (result-min-arr-2d (make-array -1 max-rows max-cols))
		   (shouldbe-min-arr-2d (list->array 2 shouldbe-list-list)))
	       (let ((result-node (update-next-edge-node weight-arr-2d result-min-arr-2d
							 max-rows max-cols
							 current-nodes remaining-nodes)))
		 (begin
		   (if (not (equal? shouldbe-node result-node))
		       (begin
			 (display (format #f "~a : error (~a) : max row/col=~a/~a, llist = ~a, invalide node, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index max-rows max-cols
					  weight-list-list shouldbe-node result-node))
			 (quit)
			 ))
		   (do ((ii 0 (1+ ii)))
		       ((>= ii max-rows))
		     (begin
		       (let ((shouldbe-row (list-ref shouldbe-list-list ii)))
			 (begin
			   (do ((jj 0 (1+ jj)))
			       ((>= jj max-rows))
			     (begin
			       (let ((sweight (list-ref shouldbe-row jj))
				     (rweight (array-ref result-min-arr-2d ii jj)))
				 (begin
				   (if (not (= sweight rweight))
				       (begin
					 (display (format #f "~a : error (~a) : max row/col = ~a/~a, weight list = ~a, shouldbe = ~a, discrepancy at row/col = ~a/~a, weight shouldbe = ~a, result = ~a~%"
							  sub-name test-label-index max-rows max-cols
							  weight-list-list shouldbe-list-list
							  ii jj sweight rweight))
					 (quit)
					 ))
				   ))
			       ))
			   ))
		       ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (remove-current-nodes-from-remaining-nodes current-node-list remaining-node-list)
  (begin
    (for-each
     (lambda (c-node)
       (begin
	 (if (not (equal? (member c-node remaining-node-list) #f))
	     (begin
	       (set! remaining-node-list (delete c-node remaining-node-list))
	       ))
	 )) current-node-list)

    remaining-node-list
    ))

;;;#############################################################
;;;#############################################################
(define (test-remove-current-nodes-from-remaining-nodes-1)
  (let ((sub-name "test-remove-current-nodes-from-remaining-nodes-1")
	(test-list
	 (list
	  (list (list 0) (list 0 1 2 3) (list 1 2 3))
	  (list (list 0 1) (list 0 1 2 3) (list 2 3))
	  (list (list 0 1 2) (list 0 1 2 3) (list 3))
	  (list (list 0 1 2 3) (list 0 1 2 3) (list))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((current-list (list-ref a-list 0))
		 (remaining-list (list-ref a-list 1))
		 (shouldbe-list (list-ref a-list 2)))
	     (let ((result-list (remove-current-nodes-from-remaining-nodes
				 current-list remaining-list)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : current-list = ~a, remaining-list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index current-list
					remaining-list shouldbe-list result-list))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (find-minimal-spanning-network weight-array-2d min-array-2d
				       max-rows max-cols)
  (let ((remaining-node-list (list))
	(current-node-list (list 0))
	(continue-loop-flag #t))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((>= ii max-rows))
	(begin
	  (set! remaining-node-list (cons ii remaining-node-list))
	  ))
      (set! remaining-node-list (reverse remaining-node-list))

      (while
       (equal? continue-loop-flag #t)
       (begin
	 (let ((next-node
		(update-next-edge-node weight-array-2d min-array-2d
				       max-rows max-cols
				       current-node-list remaining-node-list)))
	   (begin
	     (if (< next-node 0)
		 (begin
		   (set! continue-loop-flag #f))
		 (begin
		   (set! current-node-list (cons next-node current-node-list))
		   (let ((next-remaining-node-list
			  (remove-current-nodes-from-remaining-nodes
			   current-node-list remaining-node-list)))
		     (begin
		       (if (or (not (list? next-remaining-node-list))
			       (<= (length next-remaining-node-list) 0))
			   (begin
			     (set! continue-loop-flag #f))
			   (begin
			     (set! remaining-node-list next-remaining-node-list)
			     ))
		       ))
		   ))
	     ))
	 ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-minimal-spanning-network-1)
  (let ((sub-name "test-find-minimal-spanning-network-1")
	(test-list
	 (list
	  (list (list (list -1 99 88) (list 99 -1 101) (list 88 101 -1))
		3 3 (list (list -1 99 88) (list 99 -1 -1) (list 88 -1 -1)))
	  (list (list (list -1 99 88) (list 99 -1 50) (list 88 50 -1))
		3 3 (list (list -1 -1 88) (list -1 -1 50) (list 88 50 -1)))
	  (list (list (list -1 -1 88) (list -1 -1 101) (list 88 101 -1))
		3 3 (list (list -1 -1 88) (list -1 -1 101) (list 88 101 -1)))
	  (list (list (list -1 16 12 21 -1 -1 -1)
		      (list 16 -1 -1 17 20 -1 -1)
		      (list 12 -1 -1 28 -1 31 -1)
		      (list 21 17 28 -1 18 19 23)
		      (list -1 20 -1 18 -1 -1 11)
		      (list -1 -1 31 19 -1 -1 27)
		      (list -1 -1 -1 23 11 27 -1))
		7 7
		(list (list -1 16 12 -1 -1 -1 -1)
		      (list 16 -1 -1 17 -1 -1 -1)
		      (list 12 -1 -1 -1 -1 -1 -1)
		      (list -1 17 -1 -1 18 19 -1)
		      (list -1 -1 -1 18 -1 -1 11)
		      (list -1 -1 -1 19 -1 -1 -1)
		      (list -1 -1 -1 -1 11 -1 -1)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((input-list-list (list-ref this-list 0))
		 (max-rows (list-ref this-list 1))
		 (max-cols (list-ref this-list 2))
		 (shouldbe-list-list (list-ref this-list 3)))
	     (let ((arr-2d (list->array 2 input-list-list))
		   (min-array-2d (make-array -1 max-rows max-cols)))
	       (begin
		 (find-minimal-spanning-network arr-2d min-array-2d max-rows max-cols)

		 (do ((ii 0 (1+ ii)))
		     ((>= ii max-rows))
		   (begin
		     (let ((shouldbe-row (list-ref shouldbe-list-list ii)))
		       (begin
			 (do ((jj 0 (1+ jj)))
			     ((>= jj max-cols))
			   (begin
			     (let ((sweight (list-ref shouldbe-row jj))
				   (rweight (array-ref min-array-2d ii jj)))
			       (begin
				 (if (not (= sweight rweight))
				     (begin
				       (display (format #f "~a : (~a) : error : input-list=~a, shouldbe=~a, discrepancy at row/col = ~a/~a, shouldbe weight ~a, result weight = ~a~%"
							sub-name test-label-index input-list-list
							shouldbe-list-list ii jj
							sweight rweight))
				       (quit)
				       ))
				 ))
			     ))
			 ))
		     ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (sum-of-the-weights array-2d max-rows max-cols)
  (let ((weight-sum 0))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii max-rows))
	(begin
	  (do ((jj ii (1+ jj)))
	      ((>= jj max-cols))
	    (begin
	      (let ((elem (array-ref array-2d ii jj)))
		(begin
		  (if (> elem 0)
		      (begin
			(set! weight-sum (+ weight-sum elem))
			))
		  ))
	      ))
	  ))

      weight-sum
      )))

;;;#############################################################
;;;#############################################################
(define (test-sum-of-the-weights-1)
  (let ((sub-name "test-sum-of-the-weights-1")
	(test-list
	 (list
	  (list 3 3 (list (list -1 99 88) (list 99 -1 101) (list 88 101 -1)) 288)
	  (list 3 3 (list (list -1 -1 88) (list -1 -1 101) (list 88 101 -1)) 189)
	  (list 7 7 (list (list -1 16 12 21 -1 -1 -1)
			  (list 16 -1 -1 17 20 -1 -1)
			  (list 12 -1 -1 28 -1 31 -1)
			  (list 21 17 28 -1 18 19 23)
			  (list -1 20 -1 18 -1 -1 11)
			  (list -1 -1 31 19 -1 -1 27)
			  (list -1 -1 -1 23 11 27 -1)) 243)
	  (list 7 7 (list (list -1 16 12 21 -1 -1 -1)
			  (list 16 -1 -1 17 20 -1 -1)
			  (list 12 -1 -1 28 -1 31 -1)
			  (list 21 17 28 -1 18 19 -1)
			  (list -1 20 -1 18 -1 -1 -1)
			  (list -1 -1 31 19 -1 -1 -1)
			  (list -1 -1 -1 -1 -1 -1 -1)) 182)
	  (list 7 7 (list (list -1 16 12 -1 -1 -1 -1)
			  (list 16 -1 -1 17 -1 -1 -1)
			  (list 12 -1 -1 -1 -1 -1 -1)
			  (list -1 17 -1 -1 18 19 -1)
			  (list -1 -1 -1 18 -1 -1 11)
			  (list -1 -1 -1 19 -1 -1 -1)
			  (list -1 -1 -1 -1 11 -1 -1)) 93)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((max-rows (list-ref this-list 0))
		 (max-cols (list-ref this-list 1))
		 (llist (list-ref this-list 2))
		 (shouldbe (list-ref this-list 3)))
	     (let ((arr-2d (list->array 2 llist)))
	       (let ((result (sum-of-the-weights arr-2d max-rows max-cols)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : llist = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index llist shouldbe result))
			 (quit)
			 ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))
    ))

;;;#############################################################
;;;#############################################################
(define (count-of-the-weights array-2d max-rows max-cols)
  (let ((weight-count 0))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii max-rows))
	(begin
	  (do ((jj ii (1+ jj)))
	      ((>= jj max-cols))
	    (begin
	      (let ((elem (array-ref array-2d ii jj)))
		(begin
		  (if (> elem 0)
		      (begin
			(set! weight-count (1+ weight-count))
			))
		  ))
	      ))
	  ))

      weight-count
      )))

;;;#############################################################
;;;#############################################################
(define (test-count-of-the-weights-1)
  (let ((sub-name "test-count-of-the-weights-1")
	(test-list
	 (list
	  (list 3 3 (list (list -1 99 88) (list 99 -1 101) (list 88 101 -1)) 3)
	  (list 3 3 (list (list -1 -1 88) (list -1 -1 101) (list 88 101 -1)) 2)
	  (list 7 7 (list (list -1 16 12 21 -1 -1 -1)
			  (list 16 -1 -1 17 20 -1 -1)
			  (list 12 -1 -1 28 -1 31 -1)
			  (list 21 17 28 -1 18 19 23)
			  (list -1 20 -1 18 -1 -1 11)
			  (list -1 -1 31 19 -1 -1 27)
			  (list -1 -1 -1 23 11 27 -1)) 12)
	  (list 7 7 (list (list -1 16 12 21 -1 -1 -1)
			  (list 16 -1 -1 17 20 -1 -1)
			  (list 12 -1 -1 28 -1 31 -1)
			  (list 21 17 28 -1 18 19 -1)
			  (list -1 20 -1 18 -1 -1 -1)
			  (list -1 -1 31 19 -1 -1 -1)
			  (list -1 -1 -1 -1 -1 -1 -1)) 9)
	  (list 7 7 (list (list -1 16 12 -1 -1 -1 -1)
			  (list 16 -1 -1 17 -1 -1 -1)
			  (list 12 -1 -1 -1 -1 -1 -1)
			  (list -1 17 -1 -1 18 19 -1)
			  (list -1 -1 -1 18 -1 -1 11)
			  (list -1 -1 -1 19 -1 -1 -1)
			  (list -1 -1 -1 -1 11 -1 -1)) 6)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((max-rows (list-ref this-list 0))
		 (max-cols (list-ref this-list 1))
		 (llist (list-ref this-list 2))
		 (shouldbe (list-ref this-list 3)))
	     (let ((arr-2d (list->array 2 llist)))
	       (let ((result (count-of-the-weights arr-2d max-rows max-cols)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : llist = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index llist shouldbe result))
			 (quit)
			 ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))
    ))

;;;#############################################################
;;;#############################################################
;;; returns a list of lists
(define (read-in-file fname)
  (let ((results-list (list))
	(line-counter 0))
    (begin
      (if (file-exists? fname)
	  (begin
	    (with-input-from-file fname
	      (lambda ()
		(do ((line (ice9-rdelim:read-delimited "\r\n") (ice9-rdelim:read-delimited "\r\n")))
		    ((eof-object? line))
		  (begin
		    (if (and (not (eof-object? line))
			     (> (string-length line) 0))
			(begin
			  (let ((llist (string-split line #\,)))
			    (let ((plist
				   (map
				    (lambda (this-string)
				      (let ((this-num (string->number this-string)))
					(begin
					  (cond
					   ((equal? this-num #f) -1)
					   (else
					    this-num
					    ))))) llist)))
			      (begin
				(set! line-counter (1+ line-counter))
				(set! results-list
				      (cons plist results-list))
				)))
			  ))
		    ))
		))

	    (display (ice9-format:format #f "read in ~a lines from ~a~%" line-counter fname))
	    (newline)
	    (force-output)

	    (reverse results-list))
	  (begin
	    (list)
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (display-results original-weights-array min-array-2d
			 max-rows max-cols)
  (let ((initial-weights-sum (sum-of-the-weights
			      original-weights-array max-rows max-cols))
	(initial-weights-count (count-of-the-weights
				original-weights-array max-rows max-cols))
	(min-weights-sum (sum-of-the-weights
			  min-array-2d max-rows max-cols))
	(min-weights-count (count-of-the-weights
			    min-array-2d max-rows max-cols)))
    (begin
      (display (ice9-format:format
		#f "initial weight sum = ~:d, number of weights = ~:d~%"
		initial-weights-sum initial-weights-count))
      (display (ice9-format:format
		#f "minimum weight sum = ~:d, number of weights = ~:d~%"
		min-weights-sum min-weights-count))
      (display (ice9-format:format
		#f "maximum weight savings = ~:d, change in number of weights = ~:d~%"
		(- initial-weights-sum min-weights-sum)
		(- initial-weights-count min-weights-count)))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
(define (reproduce-problem-statement)
  (let ((init-list (list (list -1 16 12 21 -1 -1 -1)
			 (list 16 -1 -1 17 20 -1 -1)
			 (list 12 -1 -1 28 -1 31 -1)
			 (list 21 17 28 -1 18 19 23)
			 (list -1 20 -1 18 -1 -1 11)
			 (list -1 -1 31 19 -1 -1 27)
			 (list -1 -1 -1 23 11 27 -1)))
	(max-rows 7)
	(max-cols 7))
    (let ((network-array (list->array 2 init-list))
	  (min-array-2d (make-array -1 max-rows max-cols)))
      (begin
	(display (format #f "initial matrix:~%"))
	(do ((ii 0 (1+ ii)))
	    ((>= ii max-rows))
	  (begin
	    (let ((this-row (list-ref init-list ii)))
	      (begin
		(display (format #f "    ~a~%" this-row))
		))
	    ))
	(force-output)

	(find-minimal-spanning-network network-array min-array-2d max-rows max-cols)

	(display-results network-array min-array-2d max-rows max-cols)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop filename)
  (let ((network-list-list (read-in-file filename)))
    (let ((max-rows (length network-list-list))
	  (max-cols (length (list-ref network-list-list 0)))
	  (network-array (list->array 2 network-list-list)))
      (let ((min-array-2d (make-array -1 max-rows max-cols)))
	(begin

	  (find-minimal-spanning-network network-array min-array-2d max-rows max-cols)

	  (display-results network-array min-array-2d max-rows max-cols)
	  ))
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
    (display (format #f "Project Euler 107 - The following undirected network consists of seven vertices and twelve edges with a total weight of 243.~%"))
    (newline)
    (display (format #f "The same network can be represented by the matrix below.~%"))
    (newline)
    (display (format #f "    	A	B	C	D	E	F	G~%"))
    (display (format #f "A	-	16	12	21	-	-	-~%"))
    (display (format #f "B	16	-	-	17	20	-	-~%"))
    (display (format #f "C	12	-	-	28	-	31	-~%"))
    (display (format #f "D	21	17	28	-	18	19	23~%"))
    (display (format #f "E	-	20	-	18	-	-	11~%"))
    (display (format #f "F	-	-	31	19	-	-	27~%"))
    (display (format #f "G	-	-	-	23	11	27	-~%"))
    (newline)
    (display (format #f "However, it is possible to optimise the network by removing some edges and still ensure that all points on the network remain connected. The network which achieves the maximum saving is shown below. It has a weight of 93, representing a saving of 243 - 93 = 150 from the original network.~%"))
    (newline)
    (display (format #f "Using network.txt (right click and 'Save Link/Target As...'), a 6K text file containing a network with forty vertices, and given in matrix form, find the maximum saving which can be achieved by removing redundant edges whilst ensuring that the network remains connected.~%"))
    (newline)
    (display (format #f "For an introduction to minimum spanning trees see http://en.wikipedia.org/wiki/Minimum_spanning_tree~%"))
    (newline)
    (display (format #f "The solution used was Prim's algorithm, described at http://en.wikipedia.org/wiki/Prim's_algorithm~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-update-next-edge-node-1 counter)
	   (run-test test-remove-current-nodes-from-remaining-nodes-1 counter)
	   (run-test test-find-minimal-spanning-network-1 counter)
	   (run-test test-sum-of-the-weights-1 counter)
	   (run-test test-count-of-the-weights-1 counter)

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

    (let ((filename "network.txt"))
      (begin
	(time-code
	 (begin
	   (main-loop filename)
	   ))
	))

    (newline)
    ))
