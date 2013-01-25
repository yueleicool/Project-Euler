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
(define (triangular-number this-num)
  (let ((t1 (+ this-num 1)))
    (let ((t2 (* this-num t1)))
      (let ((t3 (euclidean/ t2 2)))
	t3
	))))

;;;#############################################################
;;;#############################################################
(define (test-triangular-number-1)
  (let ((sub-name "test-triangular-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 3) (list 3 6) (list 4 10)
	  (list 5 15)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (triangular-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (square-number this-num)
  (* this-num this-num))

;;;#############################################################
;;;#############################################################
(define (test-square-number-1)
  (let ((sub-name "test-square-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 4) (list 3 9) (list 4 16)
	  (list 5 25)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (square-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (pentagonal-number this-num)
  (let ((t1 (- (* 3 this-num) 1)))
    (let ((t2 (* this-num t1)))
      (let ((t3 (euclidean/ t2 2)))
	t3
	))))

;;;#############################################################
;;;#############################################################
(define (test-pentagonal-number-1)
  (let ((sub-name "test-pentagonal-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 5) (list 3 12) (list 4 22)
	  (list 5 35) (list 6 51) (list 7 70) (list 8 92)
	  (list 9 117) (list 10 145)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (pentagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (hexagonal-number this-num)
  (let ((t1 (- (* 2 this-num) 1)))
    (let ((t2 (* this-num t1)))
      t2
      )))

;;;#############################################################
;;;#############################################################
(define (test-hexagonal-number-1)
  (let ((sub-name "test-hexagonal-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 6) (list 3 15) (list 4 28)
	  (list 5 45)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (hexagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (heptagonal-number this-num)
  (let ((t1 (- (* 5 this-num) 3)))
    (let ((t2 (* this-num t1)))
      (let ((t3 (euclidean/ t2 2)))
	t3
	))))

;;;#############################################################
;;;#############################################################
(define (test-heptagonal-number-1)
  (let ((sub-name "test-heptagonal-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 7) (list 3 18) (list 4 34)
	  (list 5 55)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (heptagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (octagonal-number this-num)
  (let ((t1 (- (* 3 this-num) 2)))
    (let ((t2 (* this-num t1)))
      t2
      )))

;;;#############################################################
;;;#############################################################
(define (test-octagonal-number-1)
  (let ((sub-name "test-octagonal-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 8) (list 3 21) (list 4 40)
	  (list 5 65)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (octagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num
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
(define (populate-htable! p-htable this-function max-num)
  (let ((break-flag #f))
    (begin
      (do ((jj 1 (+ jj 1)))
	  ((or (> jj max-num)
	       (equal? break-flag #t)))
	(begin
	  (let ((this-num (this-function jj)))
	    (begin
	      (if (> this-num max-num)
		  (begin
		    (set! break-flag #t)))
	      (hash-set! p-htable this-num jj)
	      ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (cond
     ((< this-num 0) acc-list)
     ((< this-num 10) (cons this-num acc-list))
     (else
      (let ((next-num 0)
	    (this-digit 0))
	(begin
	  (call-with-values (lambda() (euclidean/ this-num 10))
	    (lambda (a b)
	      (begin
		(set! next-num a)
		(set! this-digit b))))
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
		       (display (format #f "~a : error (~a) : number = ~a, list shouldbe = ~a, result list = ~a~%"
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
;;; most significant digit in position 0
(define (turn-digit-list-to-number dlist)
  (let ((this-num
	 (srfi-1:fold
	  (lambda (this-elem prev-elem)
	    (+ this-elem (* 10 prev-elem)))
	  0 dlist)))
    this-num
    ))

;;;#############################################################
;;;#############################################################
(define (test-turn-digit-list-to-number-1)
  (let ((sub-name "test-turn-digit-list-to-number-1")
	(test-list
	 (list
	  (list (list 1 2) 12)
	  (list (list 2 1) 21)
	  (list (list 1 2 3) 123)
	  (list (list 3 2 1) 321)
	  (list (list 1 2 3 4) 1234)
	  (list (list 4 3 2 1) 4321)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num (turn-digit-list-to-number test-list)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display (format #f "~a : error (~a) : list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-list
					shouldbe-num result-num))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; format of list-list is (order, index, value)
(define (is-order-in-list-list? this-order alist-lists)
  (let ((result #f)
	(alen (length alist-lists))
	(break-flag #f))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((or (>= ii alen)
	       (equal? break-flag #t)))
	(begin
	  (let ((this-elem (list-ref alist-lists ii)))
	    (let ((elem-order (list-ref this-elem 0)))
	      (begin
		(if (equal? this-order elem-order)
		    (begin
		      (set! result #t)
		      (set! break-flag #t)
		      ))
		)))
	  ))
      result
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-order-in-list-list-1)
  (let ((sub-name "test-is-order-in-list-list-1")
	(test-list
	 (list
	  (list 3 (list
		   (list 1 2 3) (list 2 4 55)
		   (list 3 4 4)) #t)
	  (list 2 (list
		   (list 1 2 3) (list 2 4 55)
		   (list 3 4 4)) #t)
	  (list 4 (list
		   (list 1 2 3) (list 2 4 55)
		   (list 3 4 4)) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-order (list-ref alist 0))
		 (test-alist (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result (is-order-in-list-list? test-order test-alist)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : order = ~a, list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index order test-alist
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
(define (get-order-index
	 this-num
	 p3-htable p4-htable p5-htable
	 p6-htable p7-htable p8-htable)
  (begin
    (let ((result-list (list)))
      (begin
	(if (not (equal? (hash-ref p3-htable this-num #f) #f))
	    (begin
	      (set! result-list
		    (cons
		     (list 3 (hash-ref p3-htable this-num #f) this-num)
		     result-list))
	      ))
	(if (not (equal? (hash-ref p4-htable this-num #f) #f))
	    (begin
	      (set! result-list
		    (cons
		     (list 4 (hash-ref p4-htable this-num #f) this-num)
		     result-list))
	      ))
	(if (not (equal? (hash-ref p5-htable this-num #f) #f))
	    (begin
	      (set! result-list
		    (cons
		     (list 5 (hash-ref p5-htable this-num #f) this-num)
		     result-list))
	      ))
	(if (not (equal? (hash-ref p6-htable this-num #f) #f))
	    (begin
	      (set! result-list
		    (cons
		     (list 6 (hash-ref p6-htable this-num #f) this-num)
		     result-list))
	      ))
	(if (not (equal? (hash-ref p7-htable this-num #f) #f))
	    (begin
	      (set! result-list
		    (cons
		     (list 7 (hash-ref p7-htable this-num #f) this-num)
		     result-list))
	      ))
	(if (not (equal? (hash-ref p8-htable this-num #f) #f))
	    (begin
	      (set! result-list
		    (cons
		     (list 8 (hash-ref p8-htable this-num #f) this-num)
		     result-list))
	      ))
	(reverse result-list)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-get-order-index-1)
  (let ((sub-name "test-get-order-index-1")
	(max-num 10000)
	(test-list
	 (list
	  (list 1281 (list
		      (list 8 21 1281)))
	  (list 2512 (list
		      (list 7 32 2512)))
	  (list 8128 (list
		      (list 3 127 8128)
		      (list 6 64 8128)))
	  (list 2882 (list
		      (list 5 44 2882)))
	  (list 5625 (list
		      (list 4 75 5625)))
	  (list 8256 (list
		      (list 3 128 8256)))
	  ))
	(test-label-index 0))
    (let ((p3-htable (make-hash-table max-num))
	  (p4-htable (make-hash-table max-num))
	  (p5-htable (make-hash-table max-num))
	  (p6-htable (make-hash-table max-num))
	  (p7-htable (make-hash-table max-num))
	  (p8-htable (make-hash-table max-num)))
      (begin
	(populate-htable! p3-htable triangular-number max-num)
	(populate-htable! p4-htable square-number max-num)
	(populate-htable! p5-htable pentagonal-number max-num)
	(populate-htable! p6-htable hexagonal-number max-num)
	(populate-htable! p7-htable heptagonal-number max-num)
	(populate-htable! p8-htable octagonal-number max-num)

	(for-each
	 (lambda (alist)
	   (begin
	     (let ((test-num (list-ref alist 0))
		   (shouldbe (list-ref alist 1)))
	       (let ((result (get-order-index
			      test-num
			      p3-htable p4-htable p5-htable
			      p6-htable p7-htable p8-htable)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index test-num
					  shouldbe result))
			 (quit)
			 ))
		   )))
	     (set! test-label-index (1+ test-label-index))
	     ))
	 test-list)
	))
    ))

;;;#############################################################
;;;#############################################################
;;; acc-list contains list of 4-digit numbers, full-acc-list contains
;;; lists of lists of the form (order index number)
(define (main-loop start-num end-num max-depth)
  (define (local-calc-3-8 this-depth max-depth
			  this-num dlist
			  acc-list full-acc-list
			  p3-htable p4-htable p5-htable
			  p6-htable p7-htable p8-htable)
    (let ((local-possible-orders (get-order-index
				  this-num
				  p3-htable p4-htable p5-htable
				  p6-htable p7-htable p8-htable))
	  (local-results #f)
	  (d1 (list-tail dlist 2)))
      (begin
	(cond
	 ((and (>= this-depth max-depth)
	       (equal? max-depth (length acc-list))
	       (equal? max-depth (length full-acc-list)))
	  (begin
	    ;;; require that the first and last numbers are cyclic
	    (let ((first-d1 (split-digits-list (car (last-pair acc-list))))
		  (last-d1 (split-digits-list (car acc-list))))
	      (let ((f1 (list-head first-d1 2))
		    (d1 (list-tail last-d1 2)))
		(begin
		  (if (equal? f1 d1)
		      (begin
			(let ((require-flag #t))
			  (begin
                            ;;; require that the first max-depth orders are represented
			    (do ((ii 3 (+ ii 1)))
				((> ii (+ max-depth 2)))
			      (begin
				(if (not (is-order-in-list-list? ii full-acc-list))
				    (begin
				      (set! require-flag #f)))
				))

			    (if (equal? require-flag #t)
				(begin
				  (set! local-results
					(list (reverse acc-list)
					      (reverse full-acc-list)))
				  local-results)
				(begin
				  #f
				  )))))
		      (begin
			#f
			))
		  )))
	    ))
	 (else
	  (begin
	    (if (and
		 (not (equal? local-possible-orders #f))
		 (list? local-possible-orders)
		 (> (length local-possible-orders) 0))
		(begin
		  (for-each
		   (lambda (this-list)
		     (begin
		       (let ((local-order (list-ref this-list 0))
			     (local-nn (list-ref this-list 1))
			     (local-num (list-ref this-list 2))
			     (break-flag #f))
			 (begin
			   (if (equal? (is-order-in-list-list?
					local-order full-acc-list)
				       #f)
			       (begin
				 (do ((ii 10 (+ ii 1)))
				     ((or (> ii 99)
					  (equal? break-flag #t)))
				   (begin
				     (let ((d2 (split-digits-list ii)))
				       (let ((d3 (append d1 d2)))
					 (let ((next-num (turn-digit-list-to-number d3)))
					   (if (and (>= next-num 1000)
						    (<= next-num 9999))
					       (begin
						 (let ((results-list
							(local-calc-3-8
							 (+ this-depth 1) max-depth
							 next-num d3
							 (cons this-num acc-list)
							 (cons (list local-order local-nn this-num)
							       full-acc-list)
							 p3-htable p4-htable p5-htable
							 p6-htable p7-htable p8-htable)))
						   (begin
						     (if (not (equal? results-list #f))
							 (begin
							   (set! local-results results-list)
							   (set! break-flag #t)
							   ))
						     ))
						 ))
					   )))
				     ))
				 ))
			   ))))
		   local-possible-orders)
		  ))
	    local-results
	    )))
	)))
  (let ((counter 0)
	(p3-htable (make-hash-table end-num))
	(p4-htable (make-hash-table end-num))
	(p5-htable (make-hash-table end-num))
	(p6-htable (make-hash-table end-num))
	(p7-htable (make-hash-table end-num))
	(p8-htable (make-hash-table end-num))
	(break-flag #f))
    (begin
      (populate-htable! p3-htable triangular-number end-num)
      (populate-htable! p4-htable square-number end-num)
      (populate-htable! p5-htable pentagonal-number end-num)
      (populate-htable! p6-htable hexagonal-number end-num)
      (populate-htable! p7-htable heptagonal-number end-num)
      (populate-htable! p8-htable octagonal-number end-num)

      (do ((nn start-num (+ nn 1)))
	  ((> nn end-num))
	(begin
	  (let ((dlist (split-digits-list nn)))
	    (let ((results (local-calc-3-8
			    1 max-depth nn dlist
			    (list) (list)
			    p3-htable p4-htable p5-htable
			    p6-htable p7-htable p8-htable)))
	      (begin
		(if (and
		     (not (equal? results #f))
		     (list? results)
		     (> (length results) 0))
		    (begin
		      (let ((num-list (list-ref results 0))
			    (full-num-list (list-ref results 1)))
			(begin
			  (set! counter (+ counter 1))
			  (display (ice9-format:format #f "(~:d)  sum = ~:d : list of numbers = ~a~%"
						       counter
						       (srfi-1:fold + 0 num-list)
						       num-list))
			  (force-output)
			  (for-each
			   (lambda(this-list)
			     (let ((order (list-ref this-list 0))
				   (nn (list-ref this-list 1))
				   (num (list-ref this-list 2)))
			       (begin
				 (display (ice9-format:format
					   #f "  P(~:d, ~:d) = ~:d~%" order nn num))
				 )))
			   full-num-list)
			  (force-output)
			  ))))
		)))
	  ))

      (newline)
      (display (ice9-format:format #f "number of ~:d cyclic 4-digit numbers found = ~:d~%"
				   max-depth counter))
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
    (display (format #f "Problem 061 - Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers and are generated by the following formulae:~%"))
    (newline)
    (display (format #f "Triangle:    P(3,n)=n(n+1)/2     1, 3, 6, 10, 15, ...~%"))
    (display (format #f "Square:      P(4,n)=n^2          1, 4, 9, 16, 25, ...~%"))
    (display (format #f "Pentagonal:  P(5,n)=n(3n-1)/2    1, 5, 12, 22, 35, ...~%"))
    (display (format #f "Hexagonal:   P(6,n)=n(2n-1)      1, 6, 15, 28, 45, ...~%"))
    (display (format #f "Heptagonal:  P(7,n)=n(5n-3)/2    1, 7, 18, 34, 55, ...~%"))
    (display (format #f "Octagonal:   P(8,n)=n(3n-2)      1, 8, 21, 40, 65, ...~%"))
    (newline)
    (display (format #f "The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.~%"))
    (display (format #f "  1. The set is cyclic, in that the last two digits of each number is the first two digits of the next number (including the last number with the first).~%"))
    (display (format #f "  2. Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882), is represented by a different number in the set.~%"))
    (display (format #f "  3. This is the only set of 4-digit numbers with this property.~%"))
    (newline)
    (display (format #f "Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-triangular-number-1 counter)
	   (run-test test-square-number-1 counter)
	   (run-test test-pentagonal-number-1 counter)
	   (run-test test-hexagonal-number-1 counter)
	   (run-test test-heptagonal-number-1 counter)
	   (run-test test-octagonal-number-1 counter)
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-turn-digit-list-to-number-1 counter)
	   (run-test test-is-order-in-list-list-1 counter)
	   (run-test test-get-order-index-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (newline)
    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 1000)
	  (end-num 9999)
	  (max-depth 3))
      (begin
	(main-loop start-num end-num max-depth)
	))

    (newline)
    (force-output)
    (let ((start-num 1000)
	  (end-num 9999)
	  (max-depth 6))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num max-depth)
	   ))
	))

    (newline)
    ))
