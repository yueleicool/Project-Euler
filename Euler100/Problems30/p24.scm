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
(define (reverse-vector-k-to-n this-vector k)
  (let ((vlen (vector-length this-vector))
	(result-vector (vector-copy this-vector)))
    (let ((index-diff (- vlen k)))
    (begin
      (cond
       ((< index-diff 0) result-vector)
       (else
	(let ((ii1 k)
	      (ii2 (- vlen 1))
	      (half-diff (euclidean/ index-diff 2)))
	  (begin
	    (do ((jj 0 (+ jj 1)))
		((or (>= jj half-diff)
		     (>= ii1 ii2)
		     (>= ii1 vlen)
		     (< ii2 0)
		     ))
	      (begin
		(let ((v1 (vector-ref result-vector ii1))
		      (v2 (vector-ref result-vector ii2)))
		  (begin
		    (vector-set! result-vector ii1 v2)
		    (vector-set! result-vector ii2 v1)
		    (set! ii1 (+ ii1 1))
		    (set! ii2 (- ii2 1))
		    ))))
	    result-vector
	    ))))))))

;;;#############################################################
;;;#############################################################
(define (test-reverse-vector-k-to-n-1)
  (let ((sub-name "test-reverse-vector-k-to-n-1")
	(test-list
	 (list
	  (list (vector 0 1 2) 1 (vector 0 2 1))
	  (list (vector 0 1 2) 0 (vector 2 1 0))
	  (list (vector 0 1 2 3) 2 (vector 0 1 3 2))
	  (list (vector 0 1 2 3) 1 (vector 0 3 2 1))
	  (list (vector 0 1 2 3) 0 (vector 3 2 1 0))
	  (list (vector 0 1 2 3 4) 3 (vector 0 1 2 4 3))
	  (list (vector 0 1 2 3 4) 2 (vector 0 1 4 3 2))
	  (list (vector 0 1 2 3 4) 1 (vector 0 4 3 2 1))
	  (list (vector 0 1 2 3 4) 0 (vector 4 3 2 1 0))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-vec (list-ref this-list 0))
		 (test-ii (list-ref this-list 1))
		 (shouldbe-vec (list-ref this-list 2)))
	     (let ((result-vec (reverse-vector-k-to-n test-vec test-ii)))
	       (begin
		 (if (not (equal? shouldbe-vec result-vec))
		     (begin
		       (display (format #f "~a : error (~a) : vector=~a, index=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-vec test-ii shouldbe-vec result-vec))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; note assumes that this-vector is sorted in ascending order
(define (next-lexicographic-permutation this-vector)
  (let ((vlen (vector-length this-vector))
	(result-vector (vector-copy this-vector))
	(kk 0)
	(aakk 0)
	(ll 0)
	(aall 0)
	(break-flag #f)
	(permutation-exists #f))
    (begin
      ;;; 1) find largest kk such that a[kk] < a[kk+1]
      (do ((ii 0 (+ ii 1)))
	  ((>= ii (- vlen 1)))
	(begin
	  (let ((v1 (vector-ref result-vector ii))
		(v2 (vector-ref result-vector (+ ii 1))))
	    (if (< v1 v2)
		(begin
		  (set! permutation-exists #t)
		  (set! kk ii)
		  (set! aakk v1)
		  )))
	  ))

      ;;; 2) find the largest ll such that a[kk] < a[ll]
      (if (equal? permutation-exists #t)
	  (begin
	    (do ((ii (+ kk 1) (+ ii 1)))
		((> ii (- vlen 1)))
	      (begin
		(let ((v1 (vector-ref result-vector ii)))
		  (if (< aakk v1)
		      (begin
			(set! ll ii)
			(set! aall v1)
			)))
		))

	    ;;; 3) swap a[kk] with a[ll]
	    (vector-set! result-vector kk aall)
	    (vector-set! result-vector ll aakk)

	    ;;; 4) reverse the sequence from (k+1) on
	    (let ((final-result (reverse-vector-k-to-n result-vector (+ kk 1))))
	      final-result
	      ))
	  (begin
	    result-vector
	    )))))

;;;#############################################################
;;;#############################################################
(define (test-next-lexicographic-permutation-1)
  (let ((sub-name "test-next-lexicographic-permutation-1")
	(test-list
	 (list
	  (list (vector 0 1 2) (vector 0 2 1))
	  (list (vector 0 2 1) (vector 1 0 2))
	  (list (vector 1 0 2) (vector 1 2 0))
	  (list (vector 1 2 0) (vector 2 0 1))
	  (list (vector 2 0 1) (vector 2 1 0))
	  (list (vector 0 1 2 3 4 5) (vector 0 1 2 3 5 4))
	  (list (vector 0 1 2 3 5 4) (vector 0 1 2 4 3 5))
	  (list (vector 0 1 2 4 3 5) (vector 0 1 2 4 5 3))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-vec (list-ref this-list 0))
		 (shouldbe-vec (list-ref this-list 1)))
	     (let ((result-vec (next-lexicographic-permutation test-vec)))
	       (begin
		 (if (not (equal? shouldbe-vec result-vec))
		     (begin
		       (display (format #f "~a : error (~a) : vector=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-vec shouldbe-vec result-vec))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; ensure that the vector is sorted
(define (main-loop this-list max-num)
  (let ((this-vector (list->vector (sort this-list <))))
    (begin
      (do ((jj 1 (+ jj 1)))
	  ((>= jj max-num))
	(begin
	  (let ((result-vector (next-lexicographic-permutation this-vector)))
	    (begin
	      (set! this-vector (vector-copy result-vector))
	      ))))

      (let ((rlist (vector->list this-vector)))
	(begin
	  (display (ice9-format:format #f "the ~:d permuation of ~a is ~a~%"
				       max-num this-list rlist))
	  (force-output)
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
    (display (format #f "Problem 024 - A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:~%"))
    (display (format #f "    012   021   102   120   201   210~%"))
    (newline)
    (display (format #f "What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?~%"))
    (newline)
    (display (format #f "see the permutation wiki page for a nice discussion of how to compute lexicographic order http://en.wikipedia.org/wiki/Permutation~%"))
    (display (format #f "~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-reverse-vector-k-to-n-1 counter)
	   (run-test test-next-lexicographic-permutation-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((this-list (list 0 1 2))
	  (max-perm 6))
      (begin
	(do ((ii 1 (+ ii 1)))
	    ((>= ii max-perm))
	  (begin
	    (main-loop this-list ii)
	    ))
	))

    (newline)
    (force-output)

    (let ((this-list (list 0 1 2 3 4 5 6 7 8 9))
	  (nth 1000000))
      (begin
	(time-code
	 (begin
	   (main-loop this-list nth)
	   ))
	))
    (newline)
    ))
