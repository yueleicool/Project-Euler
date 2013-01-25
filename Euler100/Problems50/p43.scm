#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice-9-format:)))

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
		    ))
                ))
	    result-vector
	    ))
        ))
      ))
    ))

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
					sub-name test-label-index test-vec test-ii
					shouldbe-vec result-vec))
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
  (let ((vlen (1- (vector-length this-vector)))
	(result-vector (vector-copy this-vector))
	(kk 0)
	(aakk 0)
	(ll 0)
	(aall 0)
	(break-flag #f)
	(permutation-exists #f))
    (begin
      ;;; 1) find largest kk such that a[kk] < a[kk+1]
      (do ((ii 0 (1+ ii)))
	  ((>= ii vlen))
	(begin
	  (let ((v1 (vector-ref result-vector ii))
		(v2 (vector-ref result-vector (1+ ii))))
            (begin
              (if (< v1 v2)
                  (begin
                    (set! permutation-exists #t)
                    (set! kk ii)
                    (set! aakk v1)
                    ))
              ))
	  ))

      ;;; 2) find the largest ll such that a[kk] < a[ll]
      (if (equal? permutation-exists #t)
	  (begin
	    (do ((ii (+ kk 1) (1+ ii)))
		((> ii vlen))
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
	    (let ((final-result
                   (reverse-vector-k-to-n result-vector (+ kk 1))))
	      final-result
	      ))
	  (begin
	    #f
	    ))
      )))

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
					sub-name test-label-index test-vec
					shouldbe-vec result-vec))
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
;;; return empty list if not a circular prime
(define (check-pandigital-property digit-list prime-list)
  (let ((dlen (length digit-list))
        (result-list (list))
        (break-flag #f)
        (d4 (list-ref digit-list 3))
        (d6 (list-ref digit-list 5)))
    (begin
      (if (and
           (even? d4)
           (or
            (= d6 0)
            (= d6 5)))
          (begin
            (let ((first-num (list-ref digit-list 1))
                  (second-num (list-ref digit-list 2)))
              (begin
                (do ((ii 3 (1+ ii)))
                    ((or (>= ii dlen)
                         (equal? break-flag #t)))
                  (begin
                    (let ((third-num (list-ref digit-list ii)))
                      (let ((this-num
                             (+ (* 100 first-num)
                                (* 10 second-num)
                                third-num)))
                        (let ((this-prime (list-ref prime-list (- ii 2))))
                          (begin
                            (if (zero? (modulo this-num this-prime))
                                (begin
                                  (set! result-list
                                        (cons (list this-num this-prime) result-list)))
                                (begin
                                  (set! result-list #f)
                                  (set! break-flag #t)
                                  ))

                            (set! first-num second-num)
                            (set! second-num third-num)
                            ))
                        ))
                    ))
                ))

            (if (equal? result-list #f)
                (begin
                  #f)
                (begin
                  (reverse result-list)
                  )))
          (begin
            #f
            ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-check-pandigital-property-1)
  (let ((sub-name "test-check-pandigital-property-1")
	(test-list
	 (list
	  (list (list 1 4 0 6 3 5 7 2 8 9)
		(list (list 406 2) (list 063 3) (list 635 5)
		      (list 357 7) (list 572 11) (list 728 13)
		      (list 289 17)))
	  ))
        (prime-list (list 1 2 3 5 7 11 13 17))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list
                    (check-pandigital-property test-list prime-list)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : test list = ~a, shouldbe = ~a, result = ~a~%"
                         sub-name test-label-index test-list
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
(define (exchange-aa-for-bb-list pan-sub-list aa bb)
  (let ((llen (length pan-sub-list))
        (result-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii llen))
        (begin
          (let ((ii-elem (list-ref pan-sub-list ii)))
            (begin
              (cond
               ((equal? ii-elem aa)
                (begin
                  (set! result-list (cons bb result-list))
                  ))
               ((equal? ii-elem bb)
                (begin
                  (set! result-list (cons aa result-list))
                  ))
               (else
                (begin
                  (set! result-list (cons ii-elem result-list))
                  )))
              ))
          ))
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-exchange-aa-for-bb-list-1)
  (let ((sub-name "test-exchange-aa-for-bb-list-1")
	(test-list
	 (list
	  (list (list 1 4 0 6 3 5 7 2 8 9)
                1 9
                (list 9 4 0 6 3 5 7 2 8 1))
	  (list (list 1 4 0 6 3 5 7 2 8 9)
                4 8
                (list 1 8 0 6 3 5 7 2 4 9))
	  (list (list 1 4 0 6 3 5 7 2 8 9)
                0 2
                (list 1 4 2 6 3 5 7 0 8 9))
	  (list (list 1 4 0 6 3 5 7 2 8 9)
                6 7
                (list 1 4 0 7 3 5 6 2 8 9))
	  (list (list 1 4 0 6 3 5 7 2 8 9)
                3 5
                (list 1 4 0 6 5 3 7 2 8 9))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
                 (aa (list-ref alist 1))
                 (bb (list-ref alist 2))
		 (shouldbe-list (list-ref alist 3)))
	     (let ((result-list
                    (exchange-aa-for-bb-list test-list aa bb)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : test list = ~a, aa = ~a, bb = ~a, "
                         sub-name test-label-index test-list aa bb))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (make-fixed-list pan-sub-list dd-4 dd-6)
  (let ((sub-1-list (list-head pan-sub-list 3))
        (tail-2-list (list-tail pan-sub-list 3)))
    (let ((sub-2-list (list-head tail-2-list 1))
          (sub-3-list (list-tail tail-2-list 1)))
      (let ((result-list
             (append
              sub-1-list (list dd-4)
              sub-2-list (list dd-6)
              sub-3-list)))
        (begin
          result-list
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-fixed-list-1)
  (let ((sub-name "test-make-fixed-list-1")
	(test-list
	 (list
	  (list (list 0 1 2 4 6 7 8 9)
                3 5
                (list 0 1 2 3 4 5 6 7 8 9))
	  (list (list 0 1 3 4 6 7 8 5)
                2 9
                (list 0 1 3 2 4 9 6 7 8 5))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
                 (aa (list-ref alist 1))
                 (bb (list-ref alist 2))
		 (shouldbe-list (list-ref alist 3)))
	     (let ((result-list
                    (make-fixed-list test-list aa bb)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : test list = ~a, aa = ~a, bb = ~a, "
                         sub-name test-label-index test-list aa bb))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define-syntax sub-process-list
  (syntax-rules ()
    ((sub-process-list rr-list prime-list
                       pan-count sum debug-flag)
     (begin
       (let ((result-list
              (check-pandigital-property
               rr-list prime-list)))
         (begin
           (if (and
                (list? result-list)
                (> (length result-list) 0))
               (begin
                 (let ((next-num
                        (turn-digit-list-to-number
                         rr-list)))
                   (begin
                     (set! pan-count (1+ pan-count))
                     (set! sum (+ sum next-num))
                     (if (equal? debug-flag #t)
                         (begin
                           (let ((this-index 2))
                             (begin
                               (display
                                (ice-9-format:format
                                 #f "(~:d)  ~a pandigital~%" pan-count next-num))
                               (for-each
                                (lambda (this-list)
                                  (begin
                                    (let ((num (list-ref this-list 0))
                                          (prime (list-ref this-list 1)))
                                      (begin
                                        (display
                                         (format #f "    (~a) ~a is divisible by ~a~%"
                                                 this-index num prime))
                                        (force-output)
                                        (set! this-index (1+ this-index))
                                        ))
                                    )) result-list)
                               ))
                           ))
                     ))
                 ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax process-valid-vector
  (syntax-rules ()
    ((process-valid-vector next-vector prime-list
                           pan-count sum debug-flag)
     (begin
       (let ((sub-list (vector->list next-vector)))
         (let ((full-list (make-fixed-list sub-list 2 5)))
           (let ((full-0-list (exchange-aa-for-bb-list full-list 5 0)))
             (let ((ll-list-list
                    (list
                     (list-copy full-list)
                     (exchange-aa-for-bb-list full-list 0 2)
                     (exchange-aa-for-bb-list full-list 4 2)
                     (exchange-aa-for-bb-list full-list 6 2)
                     (exchange-aa-for-bb-list full-list 8 2)
                     (list-copy full-0-list)
                     (exchange-aa-for-bb-list full-0-list 4 2)
                     (exchange-aa-for-bb-list full-0-list 6 2)
                     (exchange-aa-for-bb-list full-0-list 8 2))))
               (begin
                 (for-each
                  (lambda (rr-list)
                    (begin
                      (sub-process-list
                       rr-list prime-list pan-count sum debug-flag)
                      )) ll-list-list)
                 ))
             )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop debug-flag)
  (let ((counter 0)
	(sum 0)
	(pan-count 0)
        (prime-list (list 1 2 3 5 7 11 13 17))
	(init-vector (vector 0 1 3 4 6 7 8 9))
	(this-vector #f)
	(break-flag #f))
    (begin
      (set! this-vector init-vector)

      (while (equal? break-flag #f)
        (begin
          (let ((next-vector
                 (next-lexicographic-permutation this-vector)))
            (begin
              (if (equal? next-vector #f)
                  (begin
                    (set! break-flag #t))
                  (begin
                    (set! this-vector next-vector)
                    (set! counter (1+ counter))

                    (process-valid-vector
                     next-vector prime-list pan-count sum debug-flag)
                    ))
              ))
          ))

      (display
       (ice-9-format:format
        #f "Sum of all 0 to 9 pandigital numbers = ~:d~%" sum))
      (display
       (ice-9-format:format
        #f "There were ~:d pandigital numbers out of ~:d configurations that satisfied the property.~%"
        pan-count counter))
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
    (display (format #f "Problem 043 - The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.~%"))
    (newline)
    (display (format #f "Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:~%"))
    (display (format #f "d2d3d4=406 is divisible by 2~%"))
    (display (format #f "d3d4d5=063 is divisible by 3~%"))
    (display (format #f "d4d5d6=635 is divisible by 5~%"))
    (display (format #f "d5d6d7=357 is divisible by 7~%"))
    (display (format #f "d6d7d8=572 is divisible by 11~%"))
    (display (format #f "d7d8d9=728 is divisible by 13~%"))
    (display (format #f "d8d9d10=289 is divisible by 17~%"))
    (newline)
    (display (format #f "Find the sum of all 0 to 9 pandigital numbers with this property.~%"))
    (newline)
    (display (format #f "The key to making this algorithm go fast is to reduce the number of~%"))
    (display (format #f "permutations to check.  Since there are 10 digits, there are 10!=3.6 million~%"))
    (display (format #f "permutations.  By fixing 1 digit, we can see that there will be just~%"))
    (display (format #f "9!=362,880 permutations, and by fixing 2 digits, there are 8!=40,320~%"))
    (display (format #f "permutations.~%"))
    (display (format #f "From the above, we can see that d4 must be even, "))
    (display (format #f "and d6 must be 0 or 5.~%"))
    (display (format #f "So the algorithm fixes 2 and 5 (keeping them at position 4 and position 6,)~%"))
    (display (format #f "and permutes the rest of the digits.  In order to examine all possibilites~%"))
    (display (format #f "an exchange routine switches 2 with 0, 4, 6, 8, and switches 5 with 0.~%"))
    (display (format #f "This will ensure that wherever 0 was,  2 will now occupy the 0-space,~%"))
    (display (format #f "and since 0 will be permutated through all positions, so will 2.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-reverse-vector-k-to-n-1 counter)
	   (run-test test-next-lexicographic-permutation-1 counter)
	   (run-test test-turn-digit-list-to-number-1 counter)
	   (run-test test-check-pandigital-property-1 counter)
           (run-test test-exchange-aa-for-bb-list-1 counter)
           (run-test test-make-fixed-list-1 counter)

	   (display (ice-9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop debug-flag)
	   ))
	))

    (newline)
    ))
