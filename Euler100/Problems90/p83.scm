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

;;;### ice-9 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

;;;### srfi-9 for record-type functions
(use-modules ((srfi srfi-9)
	      :renamer (symbol-prefix-proc 'srfi-9:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

(srfi-9:define-record-type
 node
 (make-node row col nvalue fvalue gvalue hvalue
	    parent-row parent-col)
 node?
 (row get-node-row set-node-row!)
 (col get-node-col set-node-col!)
 (nvalue get-node-nvalue set-node-nvalue!)
 (fvalue get-node-fvalue set-node-fvalue!)
 (gvalue get-node-gvalue set-node-gvalue!)
 (hvalue get-node-hvalue set-node-hvalue!)
 (parent-row get-node-parent-row set-node-parent-row!)
 (parent-col get-node-parent-col set-node-parent-col!))

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
(define (minimum-element two-d-array max-rows max-cols)
  (let ((min-element (array-ref two-d-array 0 0)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((>= ii max-rows))
	(begin
	  (do ((jj 0 (1+ jj)))
	      ((>= jj max-rows))
	    (begin
	      (let ((this-elem (array-ref two-d-array ii jj)))
		(begin
		  (if (< this-elem min-element)
		      (begin
			(set! min-element this-elem)
			))
		  ))
	      ))
	  ))
      min-element
      )))

;;;#############################################################
;;;#############################################################
;;; acc-list format lists of
;;; (list current-length current-sum end-row end-col (list current path))
(define (test-minimum-element-1)
  (let ((sub-name "test-minimum-element-1")
	(test-list
	 (list
	  (list (list (list 4 5 6) (list 7 8 9) (list 10 11 3))
		3 3 3)
	  (list (list (list 14 5 6 7) (list 7 8 9 10)
		      (list 10 11 13 7) (list 22 33 44 6))
		4 4 5)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((tlist-list (list-ref a-list 0))
		 (max-rows (list-ref a-list 1))
		 (max-cols (list-ref a-list 2))
		 (shouldbe (list-ref a-list 3)))
	     (let ((tarray (list->array 2 tlist-list)))
	       (let ((result (minimum-element tarray max-rows max-cols)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : list=~a, max-rows=~a, max-cols=~a : shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index tlist-list
					  max-rows max-cols shouldbe result))
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
;;; node definition : (row col nvalue fvalue gvalue hvalue parent-row parent-col)
(define (find-lowest-fvalue-in-hash node-htable)
  (let ((min-node #f)
	(min-key #f)
	(min-fvalue -1))
    (begin
      (hash-for-each
       (lambda (this-key this-node)
	 (let ((this-fvalue (get-node-fvalue this-node)))
	   (begin
	     (if (or (< min-fvalue 0)
		     (< this-fvalue min-fvalue))
		 (begin
		   (set! min-fvalue this-fvalue)
		   (set! min-node this-node)
		   (set! min-key this-key)
		   ))
	     ))) node-htable)
      (list min-key min-node)
      )))

;;;#############################################################
;;;#############################################################
;;; node definition : (row col nvalue fvalue gvalue hvalue parent-row parent-col)
(define (test-find-lowest-fvalue-in-hash-1)
  (let ((sub-name "test-find-lowest-fvalue-in-hash-1")
	(test-list
	 (list
	  (list (list (make-node 0 0 10 4 2 2 -1 -1)
		      (make-node 0 1 11 5 2 3 0 0)
		      (make-node 1 0 12 6 3 3 0 0)
		      (make-node 1 1 13 7 2 2 1 0))
		(list (list 0 0) (make-node 0 0 10 4 2 2 -1 -1)))
	  (list (list (make-node 0 0 10 8 2 2 -1 -1)
		      (make-node 0 1 11 5 2 3 0 0)
		      (make-node 1 0 12 6 3 3 0 0)
		      (make-node 1 1 13 4 2 2 1 0))
		(list (list 1 1) (make-node 1 1 13 4 2 2 1 0)))
	  (list (list (make-node 0 0 10 88 2 2 -1 -1)
		      (make-node 0 1 11 5 2 3 0 0)
		      (make-node 1 0 12 6 3 3 0 0)
		      (make-node 1 1 13 99 2 2 1 0))
		(list (list 0 1) (make-node 0 1 11 5 2 3 0 0)))
	  ))
	(node-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((node-list (list-ref a-list 0))
		 (shouldbe (list-ref a-list 1)))
	     (begin
	       (hash-clear! node-htable)
	       (for-each
		(lambda (this-node)
		  (let ((this-row (get-node-row this-node))
			(this-col (get-node-col this-node)))
		    (begin
		      (hash-set! node-htable (list this-row this-col) this-node)
		      ))) node-list)

	       (let ((result (find-lowest-fvalue-in-hash node-htable)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : node-list=~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index node-list
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
(define (move-from-open-to-closed lkey node open-htable closed-htable)
  (begin
    (hash-remove! open-htable lkey)
    (hash-set! closed-htable lkey node)
    ))


;;;#############################################################
;;;#############################################################
(define-syntax is-a-better-path
  (syntax-rules ()
    ((is-a-better-path p-row p-col max-rows max-cols grid-array
		       node open-htable closed-htable
		       this-grid-value this-gvalue this-hvalue)
     (begin
       (if (and (>= p-row 0)
		(< p-row max-rows)
		(>= p-col 0)
		(< p-col max-cols))
	   (begin
	     (let ((p-key (list p-row p-col))
		   (p-grid-value (array-ref grid-array p-row p-col)))
	       (let ((node-1 (hash-ref closed-htable p-key #f)))
		 (begin
		   (if (equal? node-1 #f)
		       (begin
			 (set! node-1 (hash-ref open-htable p-key))
			 ))
		   (if (not (equal? node-1 #f))
		       (begin
			 (let ((p-gvalue (get-node-gvalue node-1)))
			   (let ((next-gvalue (+ p-gvalue this-grid-value)))
			     (begin
			       (if (< next-gvalue this-gvalue)
				   (begin
				     (set! this-gvalue next-gvalue)
				     (set-node-gvalue! node next-gvalue)
				     (set-node-parent-row! node p-row)
				     (set-node-parent-col! node p-col)
				     (set-node-fvalue! node (+ next-gvalue this-hvalue))
				     ))
			       )))
			 ))
		   )))
	     ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (check-for-better-paths lkey node open-htable closed-htable
				grid-array max-rows max-cols)
  (begin
    (let ((this-row (get-node-row node))
	  (this-col (get-node-col node))
	  (this-gvalue (get-node-gvalue node))
	  (this-hvalue (get-node-hvalue node)))
      (let ((this-grid-value (array-ref grid-array this-row this-col)))
	(begin
	  (let ((parent-list
		 (list
		  (list (1- this-row) this-col)
		  (list (1+ this-row) this-col)
		  (list this-row (1- this-col))
		  (list this-row (1+ this-col)))))
	    (begin
	      (for-each
	       (lambda (lkey)
		 (let ((p-row (list-ref lkey 0))
		       (p-col (list-ref lkey 1)))
		   (begin
		     (is-a-better-path p-row p-col max-rows max-cols
				       grid-array node open-htable closed-htable
				       this-grid-value this-gvalue this-hvalue)
		     ))) parent-list)
	      ))

	  (hash-set! open-htable lkey node)
	  ))
      )))

;;;#############################################################
;;;#############################################################
;;; node definition : (row col nvalue fvalue gvalue hvalue parent-row parent-col)
(define (test-check-for-better-paths-1)
  (let ((sub-name "test-check-for-better-paths-1")
	(test-list
	 (list
	  (list 3 3
		(list (list 1 2 3)
		      (list 99 98 1)
		      (list 120 123 4))
		(list 1 1)
		(make-node 1 1 98 200 198 2 1 0)
		(list (make-node 0 0 1 5 1 4 -1 -1)
		      (make-node 0 1 2 6 3 3 0 0)
		      (make-node 1 0 99 103 100 3 0 0)
		      (make-node 1 1 13 7 2 2 1 0))
		(make-node 1 1 98 103 101 2 0 1))
	  ))
	(open-htable (make-hash-table 10))
	(closed-htable (make-hash-table 10))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (a-list)
	 (begin
	   (let ((max-rows (list-ref a-list 0))
		 (max-cols (list-ref a-list 1))
		 (grid-list-list (list-ref a-list 2))
		 (lkey (list-ref a-list 3))
		 (node (list-ref a-list 4))
		 (open-node-list (list-ref a-list 5))
		 (shouldbe-node (list-ref a-list 6)))
	     (let ((grid-array (list->array 2 grid-list-list)))
	       (begin
		 (hash-clear! open-htable)
		 (hash-clear! closed-htable)

		 (for-each
		  (lambda (this-node)
		    (let ((this-row (get-node-row this-node))
			  (this-col (get-node-col this-node)))
		      (begin
			(hash-set! open-htable (list this-row this-col) this-node)
			))) open-node-list)

		 (check-for-better-paths lkey node open-htable closed-htable
					 grid-array max-rows max-cols)

		 (let ((result-node (hash-ref open-htable lkey #f)))
		   (begin
		     (if (not (equal? shouldbe-node result-node))
			 (begin
			   (display (format #f "~a : error (~a) : grid-list=~a, node-list=~a, shouldbe = ~a, result = ~a~%"
					    sub-name test-label-index grid-list-list
					    open-node-list shouldbe-node result-node))
			   (quit)
			   ))
		     ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax process-next-node
  (syntax-rules ()
    ((process-next-node next-row next-col parent-row parent-col
			max-rows max-cols grid-array
			min-grid-value
			node open-htable closed-htable
			this-grid-value this-gvalue this-hvalue)
     (begin
       (let ((next-key (list next-row next-col)))
	 (begin
	   (if (and
		(>= next-row 0) (< next-row max-rows)
		(>= next-col 0) (< next-col max-cols)
		(equal? (hash-ref closed-htable next-key #f) #f))
	       (begin
		 (if (equal? (hash-ref open-htable next-key #f) #f)
		     (begin
		       (let ((next-value (array-ref grid-array next-row next-col))
			     (next-hvalue (* min-grid-value
					     (+ (- max-rows parent-row)
						(- max-cols parent-col))))
			     (parent-gvalue (get-node-gvalue node)))
			 (let ((next-gvalue (+ parent-gvalue next-value))
			       (next-fvalue (+ parent-gvalue next-value next-hvalue)))
			   (let ((next-node
				  (make-node next-row next-col next-value
					     next-fvalue next-gvalue next-hvalue
					     parent-row parent-col)))
			     (begin
			       (hash-set! open-htable next-key next-node)
			       ))
			   )))
		     (begin
		       (let ((next-node (hash-ref open-htable next-key #f)))
			 (begin
			   (if (not (equal? next-node #f))
			       (begin
				 (check-for-better-paths next-key next-node
							 open-htable closed-htable
							 grid-array max-rows max-cols)
				 ))
			   ))
		       ))
		 ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; node definition : (row col nvalue fvalue gvalue hvalue parent-row parent-col)
(define (astar-iteration-loop grid-array max-rows max-cols
			      init-row init-col closed-htable)
  (let ((open-htable (make-hash-table max-rows))
	(current-row init-row)
	(current-col init-col)
	(end-row (- max-rows 1))
	(end-col (- max-cols 1))
	(min-grid-value (minimum-element grid-array max-rows max-cols))
	(loop-continue-flag #t))
    (let ((gvalue (array-ref grid-array init-row init-col))
	  (hvalue (* min-grid-value (+ (- max-rows init-row)
				       (- max-cols init-col)))))
      (let ((current-node (make-node init-row init-col
				     gvalue (+ gvalue hvalue)
				     gvalue hvalue -1 -1)))
	(begin
	  (hash-clear! open-htable)
	  (hash-clear! closed-htable)

	  (hash-set! open-htable (list current-row current-col) current-node)

	  (while
	   (equal? loop-continue-flag #t)
	   (begin
	     (let ((min-list (find-lowest-fvalue-in-hash open-htable)))
	       (let ((lkey (list-ref min-list 0))
		     (min-node (list-ref min-list 1)))
		 (let ((parent-row (list-ref lkey 0))
		       (parent-col (list-ref lkey 1)))
		   (begin
		     (hash-remove! open-htable lkey)
		     (hash-set! closed-htable lkey min-node)

		     (if (and (= parent-row end-row)
			      (= parent-col end-col))
			 (begin
			   (set! loop-continue-flag #f)
			   ))

		     (let ((neighbor-list
			    (list
			     (list (- parent-row 1) parent-col)
			     (list (+ parent-row 1) parent-col)
			     (list parent-row (- parent-col 1))
			     (list parent-row (+ parent-col 1)))))
		       (begin
			 (for-each
			  (lambda (neighbor-key)
			    (begin
			      (let ((next-row (list-ref neighbor-key 0))
				    (next-col (list-ref neighbor-key 1)))
				(begin
				  (process-next-node next-row next-col parent-row parent-col
						     max-rows max-cols grid-array
						     min-grid-value min-node
						     open-htable closed-htable
						     this-grid-value this-gvalue this-hvalue)
				  ))
			      )) neighbor-list)
			 ))
		     ))
		 ))
	     ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (work-backwards-path closed-htable max-rows max-cols)
  (let ((current-row (1- max-rows))
	(current-col (1- max-cols))
	(result-list (list))
	(loop-continue-flag #t))
    (begin
      (while
       (equal? loop-continue-flag #t)
       (begin
	 (let ((lkey (list current-row current-col)))
	   (let ((node (hash-ref closed-htable lkey #f)))
	     (begin
	       (if (not (equal? node #f))
		   (begin
		     (set! result-list (cons node result-list))
		     (if (and (<= current-row 0)
			      (<= current-col 0))
			 (begin
			   (set! loop-continue-flag #f))
			 (begin
			   (set! current-row (get-node-parent-row node))
			   (set! current-col (get-node-parent-col node))
			   )))
		   (begin
		     (set! loop-continue-flag #f)
		     ))
	       )))
	 ))

      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (display-node-path node-list)
  (let ((llen (length node-list))
	(value-list (list)))
    (begin
      (for-each
       (lambda (node)
	 (begin
	   (let ((node-value (get-node-nvalue node)))
	     (begin
	       (set! value-list (cons node-value value-list))
	       ))
	   )) node-list)

      (let ((vstring
	     (string-join
	      (map (lambda (num)
		     (ice9-format:format #f "~:d" num))
		   value-list) ", "))
	    (vsum (srfi-1:fold + 0 value-list)))
	(begin
	  (display (ice9-format:format #f "path sum = ~:d, path = { ~a }~%" vsum vstring))
	  (force-output)
	  ))
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
(define (process-grid grid-array max-rows max-cols)
  (let ((closed-htable (make-hash-table max-rows)))
    (begin
      ;;; find path, store it in closed-htable
      (astar-iteration-loop grid-array max-rows max-cols 0 0 closed-htable)

      ;;; get path as a list of nodes from closed-htable
      (let ((node-path-list (work-backwards-path closed-htable max-rows max-cols)))
	(begin
	  node-path-list
	  ))
      )))

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
(define (main-loop grid-list-list max-rows max-cols debug-flag)
  (let ((grid-array (list->array 2 grid-list-list)))
    (let ((node-list (process-grid grid-array max-rows max-cols)))
      (begin
	(if (equal? debug-flag #t)
	    (begin
	      (display-2d-array grid-array max-rows max-cols)
	      ))

	(display-node-path node-list)
	(force-output)
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
    (display (format #f "Problem 083 - NOTE: This problem is a significantly more challenging version of Problem 81.~%"))
    (display (format #f "In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by moving left, right, up, and down, is indicated in bold red and is equal to 2297.~%"))
    (newline)
    (display (format #f "  131  673  234  103  18~%"))
    (display (format #f "  201  96   342  965  150~%"))
    (display (format #f "  630  803  746  422  111~%"))
    (display (format #f "  537  699  497  121  956~%"))
    (display (format #f "  805  732  524  37   331~%"))
    (newline)
    (display (format #f "Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by moving left, right, up, and down.~%"))
    (newline)
    (display (format #f "The solution follows the A* method, described in this article http://www.mathblog.dk/project-euler-83-find-the-minimal-path-sum-from-the-top-left-to-the-bottom-right-by-moving-left-right-up-and-down/~%"))
    (newline)
    (display (format #f "Implementation closely follows the description at http://www.policyalmanac.org/games/aStarTutorial.htm~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-minimum-element-1 counter)
	   (run-test test-find-lowest-fvalue-in-hash-1 counter)
	   (run-test test-check-for-better-paths-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (newline)
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
      (begin
	(time-code
	 (begin
	   (main-loop tlist max-rows max-cols debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((filename "matrix.txt")
	  (debug-flag #f))
      (let ((tlist (read-in-file filename)))
	(let ((max-rows (length tlist))
	      (max-cols (length (car tlist))))
	  (begin
	    (display (format #f "read in file ~a, rows = ~a, cols = ~a : ~a~%"
			     filename max-rows max-cols
			     (date-time-to-string (srfi-19:current-date))))
	    (force-output)

	    (time-code
	     (begin
	       (main-loop tlist max-rows max-cols debug-flag)
	       ))
	    ))
	))

    (newline)
    ))

