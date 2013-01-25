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
(define (are-lists-disjoint? llist1 llist2)
  (let ((ok-flag #t))
    (begin
      (for-each
       (lambda (elem1)
	 (begin
	   (if (not (equal? (member elem1 llist2) #f))
	       (begin
		 (set! ok-flag #f)
		 ))
	   )) llist1)
      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-are-lists-disjoint-1)
  (let ((sub-name "test-are-lists-disjoint-1")
	(test-list
	 (list
	  (list (list 1) (list 2) #t)
	  (list (list 1 2) (list 3 4) #t)
	  (list (list 1 2 3) (list 4 5 6) #t)
	  (list (list 1 2 3) (list 4 5 6 3) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((set-1-list (list-ref this-list 0))
		 (set-2-list (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (are-lists-disjoint? set-1-list set-2-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : set-1-list = ~a, set-2-list = ~a, shouldbe = ~a, result = ~a~%"
					       sub-name test-label-index set-1-list set-2-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax assign-value-to-list
  (syntax-rules ()
    ((assign-value-to-list ii-sum jj set-value-array sum-list-array)
     (begin
       (let ((jj-value (array-ref set-value-array jj)))
	 (begin
	   (cond
	    ((= jj-value ii-sum)
	     (begin
	       (let ((row-list (array-ref sum-list-array ii-sum)))
		 (begin
		   (array-set! sum-list-array (cons (list jj-value) row-list) ii-sum)
		   ))
	       ))
	    ((< jj-value ii-sum)
	     (begin
               ;;; try to make target value = ii-sum, using previously found diff-value
	       (let ((diff-value (- ii-sum jj-value)))
		 (begin
		   (if (and (<= diff-value ii-sum)
			    (>= diff-value 0))
		       (begin
			 (let ((dlist (array-ref sum-list-array diff-value))
			       (rlist (array-ref sum-list-array ii-sum)))
			   (begin
			     (for-each
			      (lambda (alist)
				(begin
				  (if (equal? (member jj-value alist) #f)
				      (begin
					(let ((next-alist (sort (cons jj-value alist) <)))
					  (begin
					    (if (equal? (member next-alist rlist) #f)
						(begin
						  (set! rlist (cons next-alist rlist))
						  ))
					    ))
					))
				  )) dlist)
			     (array-set! sum-list-array rlist ii-sum)
			     ))
			 ))
		   ))
	       )))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (make-array-of-sum-lists set-list)
  (let ((ok-flag #t)
	(set-value-array (list->array 1 set-list))
	(llen (length set-list))
	(max-sum (srfi-1:fold + 0 set-list)))
    (let ((sum-list-array (make-array (list) (1+ max-sum))))
      (begin
	(if (<= llen 1)
	    (begin
	      (array-set! sum-list-array (list set-list) 0)
	      sum-list-array)
	    (begin
	      (do ((ii-sum 1 (1+ ii-sum)))
		  ((or (> ii-sum max-sum)
		       (equal? ok-flag #f)))
		(begin
		  (do ((jj 0 (1+ jj)))
		      ((>= jj llen))
		    (begin
		      (assign-value-to-list ii-sum jj set-value-array sum-list-array)
		      ))
		  ))

	      sum-list-array
	      ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-make-array-of-sum-lists-1)
  (let ((sub-name "test-make-array-of-sum-lists-1")
	(test-list
	 (list
	  (list (list 1)
		(list (list) (list (list 1))))
	  (list (list 1 2)
		(list (list) (list (list 1)) (list (list 2))
		      (list (list 1 2))))
	  (list (list 1 3)
		(list (list) (list (list 1)) (list (list 3))
		      (list (list 1 3)) (list)))
	  (list (list 2 3 5)
		(list (list) (list) (list (list 2)) (list (list 3))
		      (list) (list (list 5) (list 2 3)) (list)
		      (list (list 2 5)) (list (list 3 5)) (list)
		      (list (list 2 3 5))))
	  (list (list 1 2 3 4)
		(list (list) (list (list 1)) (list (list 2))
		      (list (list 3) (list 1 2))
		      (list (list 4) (list 1 3))
		      (list (list 2 3) (list 1 4))
		      (list (list 2 4) (list 1 2 3)) (list (list 3 4) (list 1 2 4))
		      (list (list 1 3 4)) (list (list 2 3 4)) (list (list 1 2 3 4))))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list (list-ref this-list 0))
		 (shouldbe-list-list (list-ref this-list 1)))
	     (let ((result-array (make-array-of-sum-lists a-list)))
	       (let ((result-list-list (array->list result-array)))
		 (let ((slen (length shouldbe-list-list))
		       (rlen (length result-list-list)))
		   (begin
		     (if (not (equal? slen rlen))
			 (begin
			   (display (format #f "~a : error (~a) : a-list = ~a, shouldbe = ~a, result = ~a, length discrepancy~%"
					    sub-name test-label-index a-list
					    slen rlen))
			   ))
		     (for-each
		      (lambda (slist)
			(begin
			  (if (equal? (member slist result-list-list) #f)
			      (begin
				(display (format #f "~a : error (~a) : a-list = ~a, shouldbe = ~a, result = ~a, missing ~a~%"
						 sub-name test-label-index a-list
						 shouldbe-list-list result-list-list slist))
				(quit)
				))
			  )) shouldbe-list-list)
		     ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax cond-1-process-subsets
  (syntax-rules ()
    ((cond-1-process-subsets len-rr row-rr ok-flag)
     (begin
       (do ((mm 0 (1+ mm)))
	   ((or (>= mm len-rr)
		(equal? ok-flag #f)))
	 (begin
	   (let ((ll1 (list-ref row-rr mm)))
	     (begin
	       (do ((nn (1+ mm) (1+ nn)))
		   ((or (>= nn len-rr)
			(equal? ok-flag #f)))
		 (begin
		   (let ((ll2 (list-ref row-rr nn)))
		     (begin
		       (if (are-lists-disjoint? ll1 ll2)
			   (begin
                             ;;; if disjoint then condition 1 is failed
			     (set! ok-flag #f)
			     ))
		       ))
		   ))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; if there are two or more ways of summing sets to the same ii-sum
;;; then this condition is false.
(define (condition-1-ok? set-list sum-list-array)
  (let ((ok-flag #t)
	(max-sum (srfi-1:fold + 0 set-list)))
    (begin
      ;;; all lists in this row have sum = ii-sum
      (do ((rr 0 (1+ rr)))
	  ((or (> rr max-sum)
	       (equal? ok-flag #f)))
	(begin
	  (let ((row-rr (array-ref sum-list-array rr)))
	    (let ((len-rr (length row-rr)))
	      (begin
		(if (> len-rr 1)
		    (begin
		      (cond-1-process-subsets len-rr row-rr ok-flag)
		      ))
		)))
	  ))

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-condition-1-ok-1)
  (let ((sub-name "test-condition-1-ok-1")
	(test-list
	 (list
	  (list (list 1) #t)
	  (list (list 1 2) #t)
	  (list (list 1 2 3) #f)
	  (list (list 1 2 4) #t)
	  (list (list 2 3 4) #t)
	  (list (list 1 2 3 4) #f)
	  (list (list 2 3 4 5) #f)
	  (list (list 3 5 6 7) #t)
	  (list (list 6 9 11 12 13) #t)
	  (list (list 11 18 19 20 22 25) #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((sum-list-array (make-array-of-sum-lists a-list)))
	       (let ((result (condition-1-ok? a-list sum-list-array)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display
                          (format
                           #f "~a : error (~a) : a-list = ~a, shouldbe = ~a, result = ~a~%"
                           sub-name test-label-index a-list
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
(define (populate-length-sum-hash! length-sum-htable set-list sum-list-array)
  (let ((ok-flag #t)
	(set-value-array (list->array 1 set-list))
	(llen (length set-list))
	(max-sum (srfi-1:fold + 0 set-list)))
    (begin
      (if (<= llen 1)
	  (begin
	    (let ((sum (srfi-1:fold + 0 set-list)))
	      (let ((next-list (list (list sum llen set-list))))
		(begin
		  (hash-set! length-sum-htable llen next-list)
		  ))
	      ))
	  (begin
	    (do ((ii-sum 1 (1+ ii-sum)))
		((or (> ii-sum max-sum)
		     (equal? ok-flag #f)))
	      (begin
		(let ((ii-list (array-ref sum-list-array ii-sum)))
		  (let ((ii-len (length ii-list)))
		    (begin
		      (if (> ii-len 0)
			  (begin
			    (for-each
			     (lambda (alist)
			       (begin
				 (let ((asum ii-sum)
				       (alen (length alist)))
				   (let ((next-list (list asum alen alist))
					 (this-list (hash-ref length-sum-htable alen (list))))
				     (begin
				       (hash-set! length-sum-htable alen
						  (cons next-list this-list))
				       )))
				 )) ii-list)
			    ))
		      )))
		))
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-length-sum-hash-1)
  (let ((sub-name "test-populate-length-sum-hash-1")
	(test-list
	 (list
	  (list (list 1)
		(list (list 1 (list (list 1 1 (list 1))))))
	  (list (list 1 2)
		(list (list 1 (list (list 1 1 (list 1))
				    (list 2 1 (list 2))))
		      (list 2 (list (list 3 2 (list 1 2))))))
	  (list (list 1 3)
		(list (list 1 (list (list 1 1 (list 1))
				    (list 3 1 (list 3))))
		      (list 2 (list (list 4 2 (list 1 3))))))
	  (list (list 2 3 5)
		(list (list 1 (list (list 2 1 (list 2))
				    (list 3 1 (list 3))
				    (list 5 1 (list 5))))
		      (list 2 (list (list 5 2 (list 2 3))
				    (list 7 2 (list 2 5))
				    (list 8 2 (list 3 5))))
		      (list 3 (list (list 10 3 (list 2 3 5))))))
	  ))
	(length-sum-htable (make-hash-table))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list (list-ref this-list 0))
		 (shouldbe-list-list (list-ref this-list 1)))
	     (let ((sum-list-array (make-array-of-sum-lists a-list)))
	       (begin
		 (hash-clear! length-sum-htable)
		 (populate-length-sum-hash! length-sum-htable a-list sum-list-array)

		 (for-each
		  (lambda (ll-list-list)
		    (begin
		      (let ((ll-len (list-ref ll-list-list 0))
			    (ll-list (list-ref ll-list-list 1)))
			(let ((result-list-list (hash-ref length-sum-htable ll-len (list))))
			  (begin
			    (for-each
			     (lambda (b-list)
			       (begin
				 (if (equal? (member b-list result-list-list) #f)
				     (begin
				       (display (format #f "~a : error (~a) : set-list = ~a, shouldbe = ~a, result = ~a, missing key=~a, value=~a~%"
							sub-name test-label-index a-list
							shouldbe-list-list result-list-list
							ll-len b-list))
				       (quit)
				       ))
				 )) ll-list)
			    )))
		      )) shouldbe-list-list)
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax compare-ii-to-the-rest
  (syntax-rules ()
    ((compare-ii-to-the-rest ii max-len ii-sum ii-len ii-llist len-htable ok-flag)
     (begin
       (do ((jj (1+ ii) (1+ jj)))
	   ((or (> jj max-len)
		(equal? ok-flag #f)))
	 (begin
	   (let ((jj-subset-list-list (hash-ref len-htable jj (list))))
	     (let ((jj-len-len (length jj-subset-list-list)))
	       (begin
                 ;;; since jj>ii, all lengths in jj-subset-list > all lengths in ii-subset-list
		 (if (> jj-len-len 0)
		     (begin
		       (for-each
			(lambda (jj-subset-ll)
			  (begin
			    (let ((jj-sum (list-ref jj-subset-ll 0))
				  (jj-len (list-ref jj-subset-ll 1))
				  (jj-llist (list-ref jj-subset-ll 2)))
			      (begin
                                ;;; ii < jj
				(if (and (are-lists-disjoint? ii-llist jj-llist)
					 (>= ii-sum jj-sum))
				    (begin
				      (set! ok-flag #f)
				      ))
				))
			    )) jj-subset-list-list)
		       ))
		 )))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax cond-2-process-subsets
  (syntax-rules ()
    ((cond-2-process-subsets len-htable max-len ok-flag)
     (begin
       (do ((ii 0 (1+ ii)))
	   ((or (>= ii max-len)
		(equal? ok-flag #f)))
	 (begin
	   (let ((ii-subset-list-list (hash-ref len-htable ii (list))))
	     (let ((ii-len-len (length ii-subset-list-list)))
	       (begin
		 (if (> ii-len-len 0)
		     (begin
		       (for-each
			(lambda (ii-subset-ll)
			  (begin
			    (let ((ii-sum (list-ref ii-subset-ll 0))
				  (ii-len (list-ref ii-subset-ll 1))
				  (ii-llist (list-ref ii-subset-ll 2)))
			      (begin
				(compare-ii-to-the-rest ii max-len ii-sum ii-len ii-llist
							len-htable ok-flag)
				))
			    )) ii-subset-list-list)
		       ))
		 )))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; check that longer length sets have bigger sums
(define (condition-2-ok? set-list)
  (let ((ok-flag #t)
	(max-ii (euclidean/ (length set-list) 2))
        (slen (length set-list))
	(sum-small (car set-list))
        (sum-large 0))
    (begin
      ;;; a1+a2 > an, a1+a2+a3>an+an-1, ...
      (do ((ii 0 (1+ ii)))
          ((or (>= ii max-ii)
               (equal? ok-flag #f)))
        (begin
          (let ((next-small
                 (+ sum-small
                    (list-ref set-list (+ ii 1))))
                (next-large
                 (+ sum-large
                    (list-ref set-list (- slen ii 1)))))
            (begin
              (if (<= next-small next-large)
                  (begin
                    (set! ok-flag #f))
                  (begin
                    (set! sum-small next-small)
                    (set! sum-large next-large)
                    ))
              ))
          ))

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-condition-2-ok-1)
  (let ((sub-name "test-condition-2-ok-1")
	(test-list
	 (list
	  (list (list 1) #t)
	  (list (list 1 2) #t)
	  (list (list 1 2 3) #f)
	  (list (list 1 2 4) #f)
	  (list (list 2 3 4) #t)
	  (list (list 1 2 3 4) #f)
	  (list (list 2 3 4 5) #f)
	  (list (list 3 5 6 7) #t)
	  (list (list 6 9 11 12 13) #t)
	  (list (list 11 18 19 20 22 25) #t)
	  (list (list 11 18 19 20 22 26) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
             (let ((result (condition-2-ok? a-list)))
               (begin
                 (if (not (equal? shouldbe result))
                     (begin
                       (display
                        (format
                         #f "~a : error (~a) : a-list = ~a, shouldbe = ~a, result = ~a~%"
                         sub-name test-label-index a-list
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
(define-syntax handle-final-current-list
  (syntax-rules ()
    ((handle-final-current-list counter status-num max-depth start-jday
				length-htable sum-htable curr-list acc-list)
     (begin
       (set! counter (1+ counter))

       (let ((ll1 (sort curr-list <))
	     (s1 (srfi-1:fold + 0 curr-list))
	     (acc-sum -1))
	 (begin
	   (if (and (list? acc-list) (> (length acc-list) 0))
	       (begin
		 (set! acc-sum (srfi-1:fold + 0 acc-list))
		 ))

	   (if (zero? (modulo counter status-num))
	       (begin
		 (let ((end-jday (srfi-19:current-julian-day)))
		   (begin
		     (display (ice9-format:format #f "(~:d ; ~:d) : current-list = ~a, sum = ~:d, acc-list = ~a, acc-sum = ~:d : elapsed time = ~a : ~a~%"
						  max-depth counter ll1 s1 acc-list acc-sum
						  (julian-day-difference-to-string end-jday start-jday)
						  (date-time-to-string (srfi-19:current-date))))
		     (force-output)
		     (set! start-jday end-jday)
		     ))
		 ))

	   (if (or (< acc-sum 0)
		   (< s1 acc-sum))
	       (begin
		 (list counter start-jday ll1))
	       (begin
		 (list counter start-jday acc-list)
		 ))
	   ))
       ))
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
			    (let ((plist (map string->number llist)))
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
	    )))))

;;;#############################################################
;;;#############################################################
(define (main-loop filename status-num debug-flag)
  (let ((num-sss 0)
	(num-non-sss 0)
	(num-total 0)
	(sum-sa 0)
	(sa-string-list (list))
	(sa-sum-list (list))
	(set-list-list (read-in-file filename))
	(length-htable (make-hash-table 1000))
	(start-jday (srfi-19:current-julian-day)))
    (let ((slen (length set-list-list)))
      (begin
	(for-each
	 (lambda (set-list)
	   (begin
	     (set! num-total (1+ num-total))

             (let ((sorted-set-list (sort set-list <)))
               (let ((sum-array
                      (make-array-of-sum-lists sorted-set-list)))
                 (begin
                   (if (condition-1-ok? sorted-set-list sum-array)
                       (begin
                         (if (condition-2-ok? sorted-set-list)
                             (begin
                               (set! num-sss (1+ num-sss))
                               (let ((this-sa (srfi-1:fold + 0 set-list))
                                     (a-string (string-append
                                                "{ "
                                                (string-join
                                                 (map number->string sorted-set-list)
                                                 ", ") " }")))
                                 (begin
                                   (set! sum-sa (+ sum-sa this-sa))
                                   (set! sa-string-list
                                         (cons a-string sa-string-list))
                                   (set! sa-sum-list
                                         (cons this-sa sa-sum-list))
                                   (if (equal? debug-flag #t)
                                       (begin
                                         (display
                                          (ice9-format:format
                                           #f "    (~:d) set = ~a, sum = ~:d, sum total so far = ~:d~%"
                                           num-total a-string this-sa sum-sa))
                                         (force-output)
                                         ))
                                   )))
                             (begin
                               (set! num-non-sss (1+ num-non-sss))
                               )))
                       (begin
                         (set! num-non-sss (1+ num-non-sss))
                         ))
                   )))

	     (if (zero? (modulo num-total status-num))
		 (begin
		   (display (format #f "  ~a / ~a : " num-total slen))
		   (let ((end-jday (srfi-19:current-julian-day)))
		     (begin
		       (display (format #f "elapsed time = ~a : ~a~%"
					(julian-day-difference-to-string end-jday start-jday)
					(date-time-to-string (srfi-19:current-date))))
		       (force-output)
		       (set! start-jday end-jday)
		       ))
		   ))
	     )) set-list-list)

	(let ((slen (length sa-string-list))
	      (stotal 0))
	 (begin
	   (do ((ii 0 (1+ ii)))
	       ((>= ii slen))
	     (begin
	       (let ((sa-string (list-ref sa-string-list ii))
		     (sa-sum (list-ref sa-sum-list ii)))
		 (begin
		   (set! stotal (+ stotal sa-sum))
		   (display (ice9-format:format
			     #f "    (~:d) set = ~a, sum = ~:d, sum total so far = ~:d~%"
			     ii sa-string sa-sum stotal))
		   ))
	       ))

           (newline)
           (display
            (ice9-format:format
             #f "number of special sum sets = ~:d~%" num-sss))
           (display
            (ice9-format:format
             #f "number of non-special sum sets = ~:d~%" num-non-sss))
           (display
            (ice9-format:format
             #f "total number of sets = ~:d~%" num-total))
           (display
            (ice9-format:format
             #f "sum of special sets total = ~:d~%" sum-sa))

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
    (display (format #f "Project Euler 105 - Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:~%"))
    (newline)
    (display (format #f "i.  S(B) != S(C); that is, sums of subsets cannot be equal.~%"))
    (display (format #f "ii. If B contains more elements than C then S(B) > S(C).~%"))
    (newline)
    (display (format #f "For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because 65 + 87 + 88 = 75 + 81 + 84, whereas {157, 150, 164, 119, 79, 159, 161, 139, 158} satisfies both rules for all possible subset pair combinations and S(A) = 1286.~%"))
    (newline)
    (display (format #f "Using sets.txt (right click and 'Save Link/Target As...'), a 4K text file with one-hundred sets containing seven to twelve elements (the two examples given above are the first two sets in the file), identify all the special sum sets, A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).~%"))
    (newline)
    (display (format #f "NOTE: This problem is related to problems 103 and 106.~%"))
    (newline)
    (display (format #f "A speed up was found at http://www.mathblog.dk/project-euler-103-special-subset-sum/, where checking condition 2 involves just checking the sum of the smallest 2 elements against the largest element, and checking the sum of the smallest 3 elements against the sum of the largest 2 elements.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-are-lists-disjoint-1 counter)
	   (run-test test-make-array-of-sum-lists-1 counter)
	   (run-test test-condition-1-ok-1 counter)
	   (run-test test-populate-length-sum-hash-1 counter)
	   (run-test test-condition-2-ok-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((filename "sets.txt")
	  (status-num 20)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop filename status-num debug-flag)
	   ))
	))

    (newline)
    ))
