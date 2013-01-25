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
(define (list-to-string llist)
  (let ((stmp (string-join
	       (map
		number->string llist) "")))
    (begin
      stmp
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-string-1)
  (let ((sub-name "test-list-to-string-1")
	(test-list
	 (list
	  (list (list 1) "1")
	  (list (list 1 2) "12")
	  (list (list 3 1) "31")
	  (list (list 1 2 3) "123")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (list-to-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
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
;;; index labels for 3-gon
;;;  0
;;;   1
;;;  2 3 4
;;; 5
(define (three-array-to-list three-gon-array)
  (let ((result-list
	 (list (array-ref three-gon-array 0)
	       (array-ref three-gon-array 1)
	       (array-ref three-gon-array 3)
	       (array-ref three-gon-array 4)
	       (array-ref three-gon-array 3)
	       (array-ref three-gon-array 2)
	       (array-ref three-gon-array 5)
	       (array-ref three-gon-array 2)
	       (array-ref three-gon-array 1)
	       )))
    (begin
      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-three-array-to-list-1)
  (let ((sub-name "test-three-array-to-list-1")
	(test-list
	 (list
	  (list (list 4 3 1 2 6 5) (list 4 3 2 6 2 1 5 1 3))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((tarray (list->array 1 test-list)))
	       (let ((result (three-array-to-list tarray)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-list
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
;;; index labels for 3-gon
;;;  0
;;;   1
;;;  2 3 4
;;; 5
(define (three-array-to-string three-gon-array)
  (let ((result-string
	 (ice9-format:format #f "~a,~a,~a; ~a,~a,~a; ~a,~a,~a"
			     (array-ref three-gon-array 0)
			     (array-ref three-gon-array 1)
			     (array-ref three-gon-array 3)
			     (array-ref three-gon-array 4)
			     (array-ref three-gon-array 3)
			     (array-ref three-gon-array 2)
			     (array-ref three-gon-array 5)
			     (array-ref three-gon-array 2)
			     (array-ref three-gon-array 1)
			     )))
    (begin
      result-string
      )))

;;;#############################################################
;;;#############################################################
(define (test-three-array-to-string-1)
  (let ((sub-name "test-three-array-to-string-1")
	(test-list
	 (list
	  (list (list 4 3 1 2 6 5) "4,3,2; 6,2,1; 5,1,3")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((three-array (list->array 1 test-list)))
	       (let ((result (three-array-to-string three-array)))
		 (begin
		   (if (not (string-ci=? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-list
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
;;; index labels for 3-gon
;;;  0
;;;   1
;;;  2 3 4
;;; 5
(define (three-arrays-equivalent? array-1 list-2)
  (begin
    (cond
     ((and (= (array-ref array-1 0) (list-ref list-2 0))
	   (= (array-ref array-1 1) (list-ref list-2 1))
	   (= (array-ref array-1 2) (list-ref list-2 2))
	   (= (array-ref array-1 3) (list-ref list-2 3))
	   (= (array-ref array-1 4) (list-ref list-2 4))
	   (= (array-ref array-1 5) (list-ref list-2 5)))
      (begin
	#t
	))
     ((and (= (array-ref array-1 0) (list-ref list-2 4))
	   (= (array-ref array-1 1) (list-ref list-2 3))
	   (= (array-ref array-1 3) (list-ref list-2 2))
	   (= (array-ref array-1 2) (list-ref list-2 1))
	   (= (array-ref array-1 4) (list-ref list-2 5))
	   (= (array-ref array-1 5) (list-ref list-2 0)))
      (begin
	#t
	))
     ((and (= (array-ref array-1 0) (list-ref list-2 5))
	   (= (array-ref array-1 1) (list-ref list-2 2))
	   (= (array-ref array-1 2) (list-ref list-2 3))
	   (= (array-ref array-1 3) (list-ref list-2 1))
	   (= (array-ref array-1 4) (list-ref list-2 0))
	   (= (array-ref array-1 5) (list-ref list-2 4)))
      (begin
	#t
	))
     (else
      #f
      ))
    ))

;;;#############################################################
;;;#############################################################
;;; constraint propagation method
;;; index labels for 3-gon
;;;  0
;;;   1
;;;  2 3 4
;;; 5
(define (find-max-3gon-string ndigits debug-flag)
  (define (local-calc-two-sum depth three-gon-array)
    (cond
     ((= depth 4)
      (begin
	(let ((result (+ (array-ref three-gon-array 2)
			 (array-ref three-gon-array 3))))
	  (begin
	    result
	    ))
	))
     ((= depth 5)
      (begin
	(let ((result (+ (array-ref three-gon-array 1)
			 (array-ref three-gon-array 2))))
	  (begin
	    result
	    ))
	))
     (else
      -1
      )))
  (define (local-calc-three-sum depth three-gon-array)
    (cond
     ((= depth 3)
      (begin
	(let ((result (+ (array-ref three-gon-array 0)
			 (array-ref three-gon-array 1)
			 (array-ref three-gon-array 3))))
	  (begin
	    result
	    ))
	))
     ((= depth 4)
      (begin
	(let ((result (+ (array-ref three-gon-array 2)
			 (array-ref three-gon-array 3)
			 (array-ref three-gon-array 4))))
	  (begin
	    result
	    ))
	))
     ((= depth 5)
      (begin
	(let ((result (+ (array-ref three-gon-array 1)
			 (array-ref three-gon-array 2)
			 (array-ref three-gon-array 5))))
	  (begin
	    result
	    ))
	))
     (else
      -1
      )))
  (define (local-constraint-loop depth max-depth three-gon-array
				 valid-set-list three-sum acc-list debug-flag)
    (begin
      (cond
       ((>= depth max-depth)
	(begin
	  (let ((equiv-flag #f))
	    (begin
	      (for-each
	       (lambda (a-list)
		 (begin
		   (if (three-arrays-equivalent? three-gon-array a-list)
		       (begin
			 (set! equiv-flag #t)
			 ))
		   )) acc-list)
	      (if (equal? equiv-flag #f)
		  (begin
		    (set! acc-list (cons (list-copy
					  (array->list three-gon-array))
					 acc-list))
		    ))
	      ))
	  acc-list
	  ))
       ((<= depth 3)
	(begin
	  (for-each
	   (lambda (this-num)
	     (begin
	       (array-set! three-gon-array this-num depth)
	       (let ((next-depth (1+ depth))
		     (next-three-sum (local-calc-three-sum depth three-gon-array))
		     (next-set-list (delete this-num valid-set-list)))
		 (let ((next-acc-list
			(local-constraint-loop
			 next-depth max-depth three-gon-array
			 next-set-list next-three-sum acc-list debug-flag)))
		   (begin
		     (set! acc-list next-acc-list)
		     )))
	       )) valid-set-list)

	  acc-list
	  ))
       ((or (= depth 4) (= depth 5))
	(begin
	  (let ((this-sum (local-calc-two-sum depth three-gon-array)))
	    (let ((next-num (- three-sum this-sum))
		  (elem-0 (array-ref three-gon-array 0)))
	      (let ((ltmp (list-head
			   (list-copy (array->list three-gon-array))
			   depth)))
		(begin
		  (if (>= elem-0 next-num)
		      (begin
			(set! next-num -1)
			))

		  (if (and (not (equal? (member next-num valid-set-list) #f))
			   (equal? (member next-num ltmp) #f))
		      (begin
			(array-set! three-gon-array next-num depth)
			(let ((next-depth (1+ depth))
			      (next-three-sum (local-calc-three-sum depth three-gon-array)))
			  (let ((next-set-list (delete next-num valid-set-list)))
			    (begin
			      (let ((next-acc-list
				     (local-constraint-loop
				      next-depth max-depth three-gon-array
				      next-set-list next-three-sum acc-list debug-flag)))
				(begin
				  (set! acc-list next-acc-list)
				  ))
			      )))
			))
		  ))
	      ))
	  acc-list
	  ))
       )))
  (let ((three-gon-array (make-array 0 6))
	(end-index 5)
	(max-depth 6)
	(set-list (list 1 2 3 4 5 6)))
    (let ((acc-list (local-constraint-loop 0 max-depth three-gon-array
					   set-list -1 (list) debug-flag)))
      (begin
	(let ((max-string ""))
	  (begin
	    (for-each
	     (lambda (a-list)
	       (begin
		 (let ((a-array (list->array 1 a-list)))
		   (let ((cstring (three-array-to-string a-array))
			 (cat-string (list-to-string (three-array-to-list a-array)))
			 (three-sum (local-calc-three-sum 3 a-array)))
		     (begin
		       (if (equal? debug-flag #t)
			   (begin
			     (display (format #f "  ~a    ~a~%" three-sum cstring))
			     (force-output)
			     ))
		       (if (= (string-length cat-string) ndigits)
			   (begin
			     (if (string-ci>? cat-string max-string)
				 (begin
				   (set! max-string cat-string)
				   ))
			     ))
		       )))
		 )) (sort acc-list
			  (lambda (a b)
			    (begin
			      (let ((a1 (list-ref a 1))
				    (b1 (list-ref b 1)))
				(let ((a-sum (+ (list-ref a 0) (list-ref a 1) (list-ref a 3)))
				      (b-sum (+ (list-ref b 0) (list-ref b 1) (list-ref b 3))))
				  (begin
				    (cond
				     ((< a-sum b-sum) #t)
				     ((> a-sum b-sum) #f)
				     (else
				      (< a1 b1)
				      ))
				    ))
				)))
			  ))
	    max-string
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-3gon-loop ndigits debug-flag)
  (let ((rstring (find-max-3gon-string ndigits debug-flag)))
    (begin
      (display (ice9-format:format #f "Maximum magic 3-gon string = ~a, (digits = ~:d)~%"
				   rstring ndigits))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
;;; index labels for 5-gon
;;;  0
;;;    1   3
;;;  8    2
;;;9  (6 4   5)
;;;     7
(define (five-array-to-list five-gon-array)
  (let ((result-list
	 (list (array-ref five-gon-array 0)
	       (array-ref five-gon-array 1)
	       (array-ref five-gon-array 2)
	       (array-ref five-gon-array 3)
	       (array-ref five-gon-array 2)
	       (array-ref five-gon-array 4)
	       (array-ref five-gon-array 5)
	       (array-ref five-gon-array 4)
	       (array-ref five-gon-array 6)
	       (array-ref five-gon-array 7)
	       (array-ref five-gon-array 6)
	       (array-ref five-gon-array 8)
	       (array-ref five-gon-array 9)
	       (array-ref five-gon-array 8)
	       (array-ref five-gon-array 1)
	       )))
    (begin
      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-five-array-to-list-1)
  (let ((sub-name "test-five-array-to-list-1")
	(test-list
	 (list
	  (list (list 1 2 3 4 5 6 7 8 9 10) (list 1 2 3 4 3 5 6 5 7 8 7 9 10 9 2))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((tarray (list->array 1 test-list)))
	       (let ((result (five-array-to-list tarray)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-list
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
;;; index labels for 5-gon
;;;  0
;;;    1   3
;;;  8    2
;;;9  (6 4   5)
;;;     7
(define (five-array-to-string five-gon-array)
  (let ((result-string
	 (ice9-format:format #f "~a,~a,~a; ~a,~a,~a; ~a,~a,~a; ~a,~a,~a; ~a,~a,~a"
			     (array-ref five-gon-array 0)
			     (array-ref five-gon-array 1)
			     (array-ref five-gon-array 2)
			     (array-ref five-gon-array 3)
			     (array-ref five-gon-array 2)
			     (array-ref five-gon-array 4)
			     (array-ref five-gon-array 5)
			     (array-ref five-gon-array 4)
			     (array-ref five-gon-array 6)
			     (array-ref five-gon-array 7)
			     (array-ref five-gon-array 6)
			     (array-ref five-gon-array 8)
			     (array-ref five-gon-array 9)
			     (array-ref five-gon-array 8)
			     (array-ref five-gon-array 1)
			     )))
    (begin
      result-string
      )))

;;;#############################################################
;;;#############################################################
;;; index labels for 5-gon
;;;  0
;;;    1   3
;;;  8    2
;;;9  (6 4   5)
;;;     7
(define (test-five-array-to-string-1)
  (let ((sub-name "test-five-array-to-string-1")
	(test-list
	 (list
	  (list (list 1 2 3 4 5 6 7 8 9 10) "1,2,3; 4,3,5; 6,5,7; 8,7,9; 10,9,2")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((five-array (list->array 1 test-list)))
	       (let ((result (five-array-to-string five-array)))
		 (begin
		   (if (not (string-ci=? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-list
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
;;; index labels for 5-gon
;;;  0
;;;    1   3
;;;  8    2
;;;9  (6 4   5)
;;;     7
(define (five-arrays-equivalent? array-1 list-2)
  (begin
    (cond
     ((and (= (array-ref array-1 0) (list-ref list-2 0))
	   (= (array-ref array-1 1) (list-ref list-2 1))
	   (= (array-ref array-1 2) (list-ref list-2 2))
	   (= (array-ref array-1 3) (list-ref list-2 3))
	   (= (array-ref array-1 4) (list-ref list-2 4))
	   (= (array-ref array-1 5) (list-ref list-2 5))
	   (= (array-ref array-1 6) (list-ref list-2 6))
	   (= (array-ref array-1 7) (list-ref list-2 7))
	   (= (array-ref array-1 8) (list-ref list-2 8))
	   (= (array-ref array-1 9) (list-ref list-2 9)))
      (begin
	#t
	))
     ((and (= (array-ref array-1 0) (list-ref list-2 3))
	   (= (array-ref array-1 1) (list-ref list-2 2))
	   (= (array-ref array-1 2) (list-ref list-2 4))
	   (= (array-ref array-1 4) (list-ref list-2 6))
	   (= (array-ref array-1 6) (list-ref list-2 8))
	   (= (array-ref array-1 8) (list-ref list-2 1))
	   (= (array-ref array-1 3) (list-ref list-2 5))
	   (= (array-ref array-1 5) (list-ref list-2 7))
	   (= (array-ref array-1 7) (list-ref list-2 9))
	   (= (array-ref array-1 9) (list-ref list-2 0)))
      (begin
	#t
	))
     ((and (= (array-ref array-1 0) (list-ref list-2 5))
	   (= (array-ref array-1 1) (list-ref list-2 4))
	   (= (array-ref array-1 2) (list-ref list-2 6))
	   (= (array-ref array-1 4) (list-ref list-2 8))
	   (= (array-ref array-1 6) (list-ref list-2 1))
	   (= (array-ref array-1 8) (list-ref list-2 2))
	   (= (array-ref array-1 3) (list-ref list-2 7))
	   (= (array-ref array-1 5) (list-ref list-2 9))
	   (= (array-ref array-1 7) (list-ref list-2 0))
	   (= (array-ref array-1 9) (list-ref list-2 3)))
      (begin
	#t
	))
     ((and (= (array-ref array-1 0) (list-ref list-2 7))
	   (= (array-ref array-1 1) (list-ref list-2 6))
	   (= (array-ref array-1 2) (list-ref list-2 8))
	   (= (array-ref array-1 4) (list-ref list-2 1))
	   (= (array-ref array-1 6) (list-ref list-2 2))
	   (= (array-ref array-1 8) (list-ref list-2 4))
	   (= (array-ref array-1 3) (list-ref list-2 9))
	   (= (array-ref array-1 5) (list-ref list-2 0))
	   (= (array-ref array-1 7) (list-ref list-2 3))
	   (= (array-ref array-1 9) (list-ref list-2 5)))
      (begin
	#t
	))
     ((and (= (array-ref array-1 0) (list-ref list-2 9))
	   (= (array-ref array-1 1) (list-ref list-2 8))
	   (= (array-ref array-1 2) (list-ref list-2 1))
	   (= (array-ref array-1 4) (list-ref list-2 2))
	   (= (array-ref array-1 6) (list-ref list-2 4))
	   (= (array-ref array-1 8) (list-ref list-2 6))
	   (= (array-ref array-1 3) (list-ref list-2 0))
	   (= (array-ref array-1 5) (list-ref list-2 3))
	   (= (array-ref array-1 7) (list-ref list-2 5))
	   (= (array-ref array-1 9) (list-ref list-2 7)))
      (begin
	#t
	))
     (else
      #f
      ))
    ))

;;;#############################################################
;;;#############################################################
;;; constraint propagation method - reduces number of possibilities to examine
;;; index labels for 5-gon
;;;  0
;;;    1   3
;;;  8    2
;;;9  (6 4   5)
;;;     7
(define (find-max-5gon-string ndigits debug-flag)
  (define (local-five-calc-two-sum depth five-gon-array)
    (cond
     ((= depth 4)
      (begin
	(let ((result (+ (array-ref five-gon-array 2)
			 (array-ref five-gon-array 3))))
	  (begin
	    result
	    ))
	))
     ((= depth 6)
      (begin
	(let ((result (+ (array-ref five-gon-array 4)
			 (array-ref five-gon-array 5))))
	  (begin
	    result
	    ))
	))
     ((= depth 8)
      (begin
	(let ((result (+ (array-ref five-gon-array 6)
			 (array-ref five-gon-array 7))))
	  (begin
	    result
	    ))
	))
     ((= depth 9)
      (begin
	(let ((result (+ (array-ref five-gon-array 1)
			 (array-ref five-gon-array 8))))
	  (begin
	    result
	    ))
	))
     (else
      -1
      )))
  (define (local-five-calc-three-sum depth five-gon-array)
    (cond
     ((= depth 2)
      (begin
	(let ((result (+ (array-ref five-gon-array 0)
			 (array-ref five-gon-array 1)
			 (array-ref five-gon-array 2))))
	  (begin
	    result
	    ))
	))
     ((= depth 4)
      (begin
	(let ((result (+ (array-ref five-gon-array 2)
			 (array-ref five-gon-array 3)
			 (array-ref five-gon-array 4))))
	  (begin
	    result
	    ))
	))
     ((= depth 6)
      (begin
	(let ((result (+ (array-ref five-gon-array 4)
			 (array-ref five-gon-array 5)
			 (array-ref five-gon-array 6))))
	  (begin
	    result
	    ))
	))
     ((= depth 8)
      (begin
	(let ((result (+ (array-ref five-gon-array 6)
			 (array-ref five-gon-array 7)
			 (array-ref five-gon-array 8))))
	  (begin
	    result
	    ))
	))
     ((= depth 9)
      (begin
	(let ((result (+ (array-ref five-gon-array 1)
			 (array-ref five-gon-array 8)
			 (array-ref five-gon-array 9))))
	  (begin
	    result
	    ))
	))
     ((>= depth 2)
      (begin
	(let ((result (+ (array-ref five-gon-array 0)
			 (array-ref five-gon-array 1)
			 (array-ref five-gon-array 2))))
	  (begin
	    result
	    ))
	))
     (else
      -1
      )))
  (define (local-five-constraint-loop depth max-depth five-gon-array
				 valid-set-list three-sum acc-list debug-flag)
    (begin
      (cond
       ((>= depth max-depth)
	(begin
	  (let ((equiv-flag #f))
	    (begin
	      (for-each
	       (lambda (a-list)
		 (begin
		   (if (five-arrays-equivalent? five-gon-array a-list)
		       (begin
			 (set! equiv-flag #t)
			 ))
		   )) acc-list)

	      (if (equal? equiv-flag #f)
		  (begin
		    (set! acc-list (cons (list-copy
					  (array->list five-gon-array))
					 acc-list))
		    ))
	      ))
	  acc-list
	  ))
       ((or (<= depth 3) (= depth 5) (= depth 7))
	(begin
	  (for-each
	   (lambda (this-num)
	     (begin
	       (let ((elem-0 (array-ref five-gon-array 0)))
		 (begin
		   (if (or
			(<= depth 2)
			(and (or (= depth 3) (= depth 5) (= depth 7))
			     (< elem-0 this-num)))
		       (begin
			 (array-set! five-gon-array this-num depth)
			 (let ((next-depth (1+ depth))
			       (next-three-sum (local-five-calc-three-sum depth five-gon-array))
			       (next-set-list (delete this-num valid-set-list)))
			   (let ((next-acc-list
				  (local-five-constraint-loop
				   next-depth max-depth five-gon-array
				   next-set-list next-three-sum acc-list debug-flag)))
			     (begin
			       (set! acc-list next-acc-list)
			       )))
			 ))
		   ))
	       )) valid-set-list)

	  acc-list
	  ))
       ((or (= depth 4) (= depth 6) (= depth 8) (= depth 9))
	(begin
	  (let ((this-sum (local-five-calc-two-sum depth five-gon-array)))
	    (let ((next-num (- three-sum this-sum))
		  (elem-0 (array-ref five-gon-array 0)))
	      (let ((ltmp (list-head
			   (list-copy (array->list five-gon-array))
			   depth)))
		(begin
		  (if (and (= depth 9) (>= elem-0 next-num))
		      (begin
			(set! next-num -1)
			))

		  (if (and (not (equal? (member next-num valid-set-list) #f))
			   (equal? (member next-num ltmp) #f))
		      (begin
			(array-set! five-gon-array next-num depth)
			(let ((next-depth (1+ depth))
			      (next-three-sum (local-five-calc-three-sum depth five-gon-array)))
			  (let ((next-set-list (delete next-num valid-set-list)))
			    (begin
			      (let ((next-acc-list
				     (local-five-constraint-loop
				      next-depth max-depth five-gon-array
				      next-set-list next-three-sum acc-list debug-flag)))
				(begin
				  (set! acc-list next-acc-list)
				  ))
			      )))
			))
		  ))
	      ))
	  acc-list
	  ))
       )))
  (let ((five-gon-array (make-array 0 10))
	(max-depth 10)
	(set-list (list 1 2 3 4 5 6 7 8 9 10)))
    (let ((acc-list (local-five-constraint-loop 0 max-depth five-gon-array
					   set-list -1 (list) debug-flag)))
      (begin
	(let ((max-string ""))
	  (begin
	    (for-each
	     (lambda (a-list)
	       (begin
		 (let ((a-array (list->array 1 a-list)))
		   (let ((cstring (five-array-to-string a-array))
			 (cat-string (list-to-string (five-array-to-list a-array)))
			 (three-sum (local-five-calc-three-sum 3 a-array)))
		     (begin
		       (if (equal? debug-flag #t)
			   (begin
			     (display (format #f "  ~a    ~a~%" three-sum cstring))
			     (force-output)
			     ))
		       (if (= (string-length cat-string) ndigits)
			   (begin
			     (if (string-ci>? cat-string max-string)
				 (begin
				   (set! max-string cat-string)
				   ))
			     ))
		       )))
		 )) (sort acc-list
			  (lambda (a b)
			    (begin
			      (let ((a1 (+ (* 10 (list-ref a 0)) (list-ref a 1)))
				    (b1 (+ (* 10 (list-ref b 0)) (list-ref b 1))))
				(let ((a-sum (+ (list-ref a 0) (list-ref a 1) (list-ref a 2)))
				      (b-sum (+ (list-ref b 0) (list-ref b 1) (list-ref b 2))))
				  (begin
				    (cond
				     ((< a-sum b-sum) #t)
				     ((> a-sum b-sum) #f)
				     (else
				      (< a1 b1)
				      ))
				    ))
				)))
			  ))
	    max-string
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-5gon-loop ndigits debug-flag)
  (let ((rstring (find-max-5gon-string ndigits debug-flag)))
    (begin
      (display (ice9-format:format #f "Maximum magic 5-gon string = ~a, (digits = ~:d)~%"
				   rstring ndigits))
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
    (display (format #f "Problem 068 - Consider the following 'magic' 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.~%"))
    (newline)
    (display (format #f "Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.~%"))
    (newline)
    (display (format #f "It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.~%"))
    (display (format #f "Total    Solution Set~%"))
    (display (format #f "  9      4,2,3; 5,3,1; 6,1,2~%"))
    (display (format #f "  9      4,3,2; 6,2,1; 5,1,3~%"))
    (display (format #f " 10      2,3,5; 4,5,1; 6,1,3~%"))
    (display (format #f " 10      2,5,3; 6,3,1; 4,1,5~%"))
    (display (format #f " 11      1,4,6; 3,6,2; 5,2,4~%"))
    (display (format #f " 11      1,6,4; 5,4,2; 3,2,6~%"))
    (display (format #f " 12      1,5,6; 2,6,4; 3,4,5~%"))
    (display (format #f " 12      1,6,5; 3,5,4; 2,4,6~%"))
    (newline)
    (display (format #f "By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.~%"))
    (newline)
    (display (format #f "Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum 16-digit string for a 'magic' 5-gon ring?~%"))
    (newline)
    (display (format #f "The different constraints that apply to each node is propagated to the other nodes.  The numbering of the nodes used is the same as in the diagram at http://www.mathblog.dk/wp-content/uploads/2011/10/datastructure.png~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-list-to-string-1 counter)
	   (run-test test-three-array-to-list-1 counter)
	   (run-test test-three-array-to-string-1 counter)

	   (run-test test-five-array-to-list-1 counter)
	   (run-test test-five-array-to-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((ndigits 9)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-3gon-loop ndigits debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((ndigits 16)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-5gon-loop ndigits debug-flag)
	   ))
	))

    (newline)
    ))
