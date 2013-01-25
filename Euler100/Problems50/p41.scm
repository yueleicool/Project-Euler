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
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (make-prime-array max-num)
  (let ((intermediate-array (make-array 0 (1+ max-num)))
	(result-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! intermediate-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (do ((jj ii (+ jj ii)))
	      ((> jj max-num))
	    (begin
	      (let ((this-num (array-ref intermediate-array jj)))
		(begin
		  (if (= this-num ii)
		      (begin
			(set! result-list (cons ii result-list)))
		      (begin
			(array-set! intermediate-array -1 jj)
			))
		  ))
	      ))
	  ))
      (list->array 1 (reverse result-list))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-array-1)
  (let ((sub-name "test-make-prime-array-1")
	(test-list
	 (list
	  (list 2 (list 2)) (list 3 (list 2 3)) (list 4 (list 2 3))
	  (list 5 (list 2 3 5)) (list 6 (list 2 3 5))
	  (list 7 (list 2 3 5 7)) (list 8 (list 2 3 5 7))
	  (list 9 (list 2 3 5 7)) (list 10 (list 2 3 5 7))
	  (list 11 (list 2 3 5 7 11)) (list 13 (list 2 3 5 7 11 13))
	  (list 17 (list 2 3 5 7 11 13 17))
	  (list 19 (list 2 3 5 7 11 13 17 19))
	  (list 23 (list 2 3 5 7 11 13 17 19 23))
	  (list 31 (list 2 3 5 7 11 13 17 19 23 29 31))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-array (make-prime-array test-num)))
               (let ((slen (length shouldbe-list))
                     (rlen (car (array-dimensions result-array))))
                 (begin
                   (if (not (equal? slen rlen))
                       (begin
                         (display
                          (format #f "~a : error (~a) : num=~a, "
                                  sub-name test-label-index test-num))
                         (display
                          (format #f "shouldbe=~a, result=~a : "
                                  shouldbe-list result-array))
                         (display
                          (format #f "length discrepancy : shouldbe=~a, result=~a~%"
                                  slen rlen))
                         (quit)
                         ))
                   (do ((ii 0 (1+ ii)))
                       ((>= ii slen))
                     (begin
                       (let ((s-elem (list-ref shouldbe-list ii))
                             (r-elem (array-ref result-array ii)))
                         (begin
                           (if (not (equal? s-elem r-elem))
                               (begin
                                 (display
                                  (format
                                   #f "~a : error (~a) : num=~a, "
                                   sub-name test-label-index test-num))
                                 (display
                                  (format
                                   #f "shouldbe=~a, result=~a : "
                                   shouldbe-list result-array))
                                 (display
                                  (format
                                   #f "discrepancy at element shouldbe=~a, result=~a~%"
                                   s-elem r-elem))
                                 (quit)
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
(define (binary-search prime-array arr-size num)
  (let ((lower 0)
        (mid (euclidean/ arr-size 2))
        (upper (1- arr-size))
        (result #f))
    (let ((a-lower (array-ref prime-array lower))
          (a-mid (array-ref prime-array mid))
          (a-upper (array-ref prime-array upper))
          (delta (- upper lower))
          (continue-loop-flag #t))
      (begin
        (if (and (>= num a-lower)
                 (<= num a-upper))
            (begin
              (while (equal? continue-loop-flag #t)
                (begin
                  (cond
                   ((= num a-lower)
                    (begin
                      (set! continue-loop-flag #f)
                      (set! result a-lower)
                      (break)
                      ))
                   ((= num a-upper)
                    (begin
                      (set! continue-loop-flag #f)
                      (set! result a-upper)
                      (break)
                      ))
                   ((= num a-mid)
                    (begin
                      (set! continue-loop-flag #f)
                      (set! result a-mid)
                      (break)
                      ))
                   ((and (> num a-lower) (< num a-mid)
                         (> delta 1))
                    (begin
                      (set! upper mid)
                      (set! a-upper a-mid)
                      (set! mid
                            (+ lower (euclidean/ (- upper lower) 2)))
                      (set! a-mid (array-ref prime-array mid))
                      (set! delta (- upper lower))
                      ))
                   ((and (> num a-mid) (< num a-upper)
                         (> delta 1))
                    (begin
                      (set! lower mid)
                      (set! a-lower a-mid)
                      (set! mid
                            (+ lower (euclidean/ (- upper lower) 2)))
                      (set! a-mid (array-ref prime-array mid))
                      (set! delta (- upper lower))
                      ))
                   ((<= delta 1)
                    (begin
                      (set! continue-loop-flag #f)
                      (cond
                       ((equal? num a-lower)
                        (begin
                          (set! result a-lower)
                          (break)
                          ))
                       ((equal? num a-upper)
                        (begin
                          (set! result a-upper)
                          (break)
                          ))
                       (else
                        (begin
                          (set! result #f)
                          (break)
                          )))
                      ))
                   (else
                    (begin
                      (set! result #f)
                      (break)
                      )))
                  ))
              ))
        result
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-binary-search-1)
  (let ((sub-name "test-binary-search-1")
        (test-list
	 (list
	  (list (list 2 3 5 7 11) 5 -1 #f)
	  (list (list 2 3 5 7 11) 5 1 #f)
	  (list (list 2 3 5 7 11) 5 2 2)
	  (list (list 2 3 5 7 11) 5 3 3)
	  (list (list 2 3 5 7 11) 5 4 #f)
	  (list (list 2 3 5 7 11) 5 5 5)
	  (list (list 2 3 5 7 11) 5 6 #f)
	  (list (list 2 3 5 7 11) 5 7 7)
	  (list (list 2 3 5 7 11) 5 8 #f)
	  (list (list 2 3 5 7 11) 5 9 #f)
	  (list (list 2 3 5 7 11) 5 10 #f)
	  (list (list 2 3 5 7 11) 5 11 11)
	  (list (list 2 3 5 7 11) 5 12 #f)
	  (list (list 2 3 5 7 11) 5 13 #f)
	  (list (list 2 3 5 7 11 13) 6 1 #f)
	  (list (list 2 3 5 7 11 13) 6 5 5)
	  (list (list 2 3 5 7 11 13) 6 6 #f)
	  (list (list 2 3 5 7 11 13) 6 7 7)
	  (list (list 2 3 5 7 11 13) 6 13 13)
	  (list (list 2 3 5 7 11 13) 6 14 #f)
	  (list (list 2 3 5 7 11 13) 6 20 #f)
          (list (list 2 3 5 7 11 13 17 19) 8 7 7)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-array
                  (list->array 1 (list-ref this-list 0)))
                 (arr-size (list-ref this-list 1))
                 (num (list-ref this-list 2))
		 (shouldbe (list-ref this-list 3)))
	     (let ((result
                    (binary-search test-array arr-size num)))
               (let ((err-1
                      (format #f "~a : error (~a) : num=~a, array=~a : "
                              sub-name test-label-index num test-array))
                     (err-2
                      (format #f "shouldbe=~a, result=~a~%"
                              shouldbe result)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a~a~%"
                                          err-1 err-2))
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
(define (is-array-prime? nn prime-array)
  (begin
    (cond
     ((<= nn 1) #f)
     ((= nn 2) #t)
     ((even? nn) #f)
     (else
      (let ((max-divisor
	     (+ (exact-integer-sqrt nn) 1))
	    (asize (car (array-dimensions prime-array))))
        (let ((max-arr-prime
               (array-ref prime-array (1- asize))))
          (begin
            (cond
             ((<= nn max-arr-prime)
              (begin
                (let ((b-result (binary-search prime-array asize nn)))
                  (begin
                    (if (equal? b-result #f)
                        (begin
                          #f)
                        (begin
                          #t
                          ))
                    ))
                ))
             ((<= max-divisor max-arr-prime)
              (begin
                (let ((continue-loop-flag #t)
                      (aprime 2)
                      (is-prime-flag #t))
                  (begin
                    (do ((ii 0 (1+ ii)))
                        ((or (>= ii asize)
                             (> aprime max-divisor)
                             (equal? continue-loop-flag #f)))
                      (begin
                        (set! aprime (array-ref prime-array ii))

                        (if (zero? (modulo nn aprime))
                            (begin
                              (set! is-prime-flag #f)
                              (set! continue-loop-flag #f)
                              ))
                        ))

                    is-prime-flag
                    ))
                ))
             (else
              (begin
                (let ((continue-loop-flag #t)
                      (aprime 2)
                      (is-prime-flag #t))
                  (begin
                    (do ((ii 0 (1+ ii)))
                        ((or (>= ii asize)
                             (equal? continue-loop-flag #f)))
                      (begin
                        (set! aprime (array-ref prime-array ii))

                        (if (zero? (modulo nn aprime))
                            (begin
                              (set! is-prime-flag #f)
                              (set! continue-loop-flag #f)
                              ))
                        ))

                    (if (equal? is-prime-flag #t)
                        (begin
                          (let ((continue-loop-flag #t))
                            (begin
                              (do ((ii (+ max-arr-prime 2) (+ ii 2)))
                                  ((or (> ii max-divisor)
                                       (equal? continue-loop-flag #f)))
                                (begin
                                  (if (zero? (modulo nn ii))
                                      (begin
                                        (set! is-prime-flag #f)
                                        (set! continue-loop-flag #f)
                                        ))
                                  ))
                              ))
                          ))

                    is-prime-flag
                    ))
                )))
            )))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-is-array-prime-1)
  (let ((sub-name "test-is-array-prime-1")
        (test-list
	 (list
	  (list 0 #f) (list 1 #f) (list 2 #t) (list 3 #t)
	  (list 4 #f) (list 5 #t) (list 6 #f) (list 7 #t)
	  (list 8 #f) (list 9 #f) (list 10 #f) (list 11 #t)
	  (list 12 #f) (list 13 #t) (list 14 #f) (list 15 #f)
	  (list 16 #f) (list 17 #t) (list 18 #f) (list 19 #t)
	  (list 20 #f) (list 21 #f) (list 22 #f) (list 23 #t)
	  (list 24 #f) (list 25 #f) (list 26 #f) (list 27 #f)
	  (list 28 #f) (list 29 #t) (list 30 #f) (list 31 #t)
	  (list 32 #f) (list 33 #f) (list 34 #f) (list 35 #f)
	  (list 36 #f) (list 37 #t) (list 38 #f) (list 39 #f)
	  (list 40 #f) (list 41 #t) (list 42 #f) (list 43 #t)
	  (list 44 #f) (list 45 #f) (list 46 #f) (list 47 #t)
	  (list 48 #f) (list 49 #f) (list 50 #f) (list 51 #f)
	  (list 52 #f) (list 53 #t) (list 54 #f) (list 55 #f)
	  (list 56 #f) (list 57 #f) (list 58 #f) (list 59 #t)
	  (list 60 #f) (list 61 #t) (list 62 #f) (list 63 #f)
	  (list 64 #f) (list 65 #f) (list 66 #f) (list 67 #t)
	  (list 68 #f) (list 69 #f) (list 70 #f) (list 71 #t)
	  (list 72 #f) (list 73 #t) (list 74 #f) (list 75 #f)
	  (list 76 #f) (list 77 #f) (list 78 #f) (list 79 #t)
	  (list 80 #f) (list 81 #f) (list 82 #f) (list 83 #t)
	  (list 84 #f) (list 85 #f) (list 86 #f) (list 87 #f)
	  (list 88 #f) (list 89 #t) (list 90 #f) (list 91 #f)
	  (list 92 #f) (list 93 #f) (list 94 #f) (list 95 #f)
	  (list 96 #f) (list 97 #t) (list 98 #f) (list 99 #f)
	  ))
	(prime-array (make-prime-array 5))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result
                    (is-array-prime? test-num prime-array)))
               (let ((err-1
                      (format #f "~a : error (~a) : num=~a, "
                              sub-name test-label-index test-num))
                     (err-2
                      (format #f "shouldbe=~a, result=~a~%"
                              shouldbe result)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a~a~%" err-1 err-2))
                         (force-output)
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
	  ))
      )))
  (let ((result-list (local-loop this-num (list))))
    (begin
      result-list
      )))

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
					sub-name test-label-index test-vec test-ii
					shouldbe-vec result-vec))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
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
	    #f
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
	  (list (vector 2 1 0) #f)
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
(define (turn-digit-vector-to-number dvector max-len)
  (let ((result-num 0))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii max-len))
        (begin
          (let ((this-elem (vector-ref dvector ii)))
            (begin
              (set! result-num
                    (+ (* 10 result-num) this-elem))
              ))
          ))
      result-num
      )))

;;;#############################################################
;;;#############################################################
(define (test-turn-digit-vector-to-number-1)
  (let ((sub-name "test-turn-digit-vector-to-number-1")
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
             (let ((test-vector (list->vector test-list))
                   (tlen (length test-list)))
               (let ((result-num
                      (turn-digit-vector-to-number test-vector tlen)))
                 (begin
                   (if (not (equal? shouldbe-num result-num))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : list = ~a, shouldbe = ~a, result = ~a~%"
                           sub-name test-label-index test-list
                           shouldbe-num result-num))
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
;;; most significant digit in position 0
(define (largest-n-digit-prime nn prime-array)
  (let ((result-num 0)
	(dlist (list))
        (max-index (- nn 1))
        (iter-vector #f))
    (begin
      (do ((ii 1 (1+ ii)))
	  ((> ii nn))
	(begin
	  (set! dlist (cons ii dlist))
	  ))
      (set! dlist (reverse dlist))
      (set! iter-vector (list->vector dlist))

      (while (not (equal? iter-vector #f))
        (begin
          (let ((this-num
                 (turn-digit-vector-to-number iter-vector nn)))
            (begin
              (if (and
                   (odd? this-num)
                   (> this-num result-num)
                   (is-array-prime? this-num prime-array))
                  (begin
                    (set! result-num this-num)
                    ))
              (let ((next-vector
                     (next-lexicographic-permutation iter-vector)))
                (begin
                  (set! iter-vector next-vector)
                  ))
              ))
          ))
      result-num
      )))

;;;#############################################################
;;;#############################################################
(define (test-largest-n-digit-prime-1)
  (let ((sub-name "test-largest-n-digit-prime-1")
	(test-list
	 (list
	  (list 2 0)
	  (list 3 0)
	  (list 4 4231)
	  ))
        (prime-array (make-prime-array 100))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-nn (list-ref alist 0))
		 (shouldbe-num (list-ref alist 1)))
	     (let ((result-num
                    (largest-n-digit-prime test-nn prime-array)))
	       (begin
		 (if (not (equal? shouldbe-num result-num))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : nn = ~a, shouldbe = ~a, result = ~a~%"
                         sub-name test-label-index test-nn
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
(define (main-loop)
  (let ((counter 0)
	(largest-nn 0)
	(largest-prime-pandigital 0)
        (prime-array
         (make-prime-array 500000))
	(break-flag #f))
    (begin
      (do ((ii 9 (- ii 1)))
	  ((or (< ii 2)
	       (equal? break-flag #t)))
	(begin
	  (let ((rnum (largest-n-digit-prime ii prime-array)))
	    (if (> rnum largest-prime-pandigital)
		(begin
		  (set! largest-prime-pandigital rnum)
		  (set! largest-nn ii)
		  (set! break-flag #f)
		  )))
	  ))

      (display
       (ice9-format:format
        #f "largest n-digit pandigital prime = ~:d (~:d digits)~%"
        largest-prime-pandigital largest-nn))
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
    (display (format #f "Problem 041 - We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.~%"))
    (newline)
    (display (format #f "What is the largest n-digit pandigital prime that exists?~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-prime-array-1 counter)
           (run-test test-binary-search-1 counter)
           (run-test test-is-array-prime-1 counter)
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-reverse-vector-k-to-n-1 counter)
	   (run-test test-next-lexicographic-permutation-1 counter)
	   (run-test test-turn-digit-vector-to-number-1 counter)
	   (run-test test-largest-n-digit-prime-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (time-code
     (begin
       (main-loop)
       ))

    (newline)
    ))
