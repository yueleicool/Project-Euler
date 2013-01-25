#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;; this code is dedicated to the public domain

;;; srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;; ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice9-format:)))

;;; srfi-11 for let-values (multiple value bind)
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
(define (digit-list-to-number llist)
  (let ((this-sum
	 (srfi-1:fold
	  (lambda (this-elem previous)
	    (+ this-elem (* 10 previous)))
	  0 llist)))
    this-sum
    ))

;;;#############################################################
;;;#############################################################
(define (test-digit-list-to-number-1)
  (let ((sub-name "test-digit-list-to-number-1")
	(test-list
	 (list
	  (list (list 1) 1)
	  (list (list 1 2) 12)
	  (list (list 1 2 3) 123)
	  (list (list 4 5 6 7) 4567)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (digit-list-to-number test-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error : list=~a, shouldbe=~a, result=~a~%"
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
          (let ((ii-num (array-ref intermediate-array ii)))
            (begin
              (if (= ii-num ii)
                  (begin
                    (set! result-list (cons ii result-list))

                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj max-num))
                      (begin
                        (let ((jj-num (array-ref intermediate-array jj)))
                          (begin
                            (array-set! intermediate-array -1 jj)
                            ))
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
	  (list 40 (list 2 3 5 7 11 13 17 19 23 29 31 37))
	  (list 50 (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
	  ))
	(test-label-index 0)
        (ok-flag #t))
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
                          (format #f "~a : error (~a) : num=~a, shouldbe=~a, "
                                  sub-name test-label-index test-num
                                  shouldbe-list))
			 (display
                          (format #f "lengths not equal, shouldbe=~a, result=~a~%"
                                  slen rlen))
			 (set! ok-flag #f)
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
                                  (format #f "~a : error (~a) : num=~a, "
                                          sub-name test-label-index test-num))
				 (display
                                  (format #f "shouldbe=~a, result=~a, discrepancy at "
                                          shouldbe result))
				 (display
                                  (format #f "ii=~a, shouldbe=~a, result=~a~%"
                                          ii s-elem r-elem))
				 (set! ok-flag #f)
				 ))
			   ))
		       ))
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
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
	(test-label-index 0)
        (ok-flag #t))
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
                         (set! ok-flag #f)
                         ))
                   ))
               ))

	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
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
	(prime-array (make-prime-array 20))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (is-array-prime? test-num prime-array)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format #f "~a : error (~a) : num=~a, "
                                sub-name test-label-index test-num))
		       (display
                        (format #f "shouldbe=~a, result=~a~%"
                                shouldbe result))
		       (set! ok-flag #f)
		       ))
		 )))

	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define-syntax count-digits
  (syntax-rules ()
    ((count-digits dlist a-digit dcount)
     (begin
       (let ((ncount (srfi-1:fold
		      (lambda (ldigit prev)
			(begin
			  (if (= ldigit a-digit)
			      (begin
				(+ prev 1))
			      (begin
				prev
				))
			  )) 0 dlist)))
	 (begin
	   (set! dcount ncount)
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (make-repeat-digits-primes a-digit num-repeats num-digits prime-array)
  (define (inner-loop depth max-depth a-digit num-repeats
		      prime-array current-list acc-list)
    (cond
     ((>= depth max-depth)
      (begin
	(let ((ncount 0))
	  (begin
	    (count-digits current-list a-digit ncount)

	    (if (and (= (length current-list) max-depth)
		     (= ncount num-repeats))
		(begin
		  (let ((c-number (digit-list-to-number (reverse current-list))))
		    (begin
		      (if (is-array-prime? c-number prime-array)
			  (begin
			    (set! acc-list (cons c-number acc-list))
			    ))
		      ))
		  ))
	    acc-list
	    ))
	))
     (else
      (begin
	(let ((start-num 0))
	  (begin
	    (if (<= depth 0)
		(begin
		  (set! start-num 1)
		  ))

	    (do ((ii start-num (1+ ii)))
		((> ii 9))
	      (begin
		(let ((next-list (cons ii current-list)))
		  (let ((ndigits 0)
			(depth-to-go (- max-depth depth)))
		    (begin
		      (count-digits next-list a-digit ndigits)

		      (if (>= depth-to-go (- num-repeats ndigits))
			  (begin
			    (let ((next-acc-list
				   (inner-loop
                                    (1+ depth) max-depth a-digit num-repeats
                                    prime-array next-list acc-list)))
			      (begin
				(set! acc-list next-acc-list)
				))
			    ))
		      )))
		))
	    ))
	acc-list
	))
     ))
  (let ((acc-list
         (inner-loop
          0 num-digits a-digit num-repeats
          prime-array (list) (list))))
    (begin
      acc-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-repeat-digits-primes-1)
  (let ((sub-name "test-make-repeat-digits-primes-1")
	(test-list
	 (list
	  (list 1 2 3 100
                (list 113 191 181 151 131 911 811 311 211 101))
	  (list 1 3 4 1000
                (list 1117 1151 1171 1181 1511 1811 2111 4111 8111))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((a-digit (list-ref alist 0))
		 (num-repeats (list-ref alist 1))
		 (num-digits (list-ref alist 2))
		 (max-prime (list-ref alist 3))
		 (shouldbe-list (list-ref alist 4)))
	     (let ((prime-array (make-prime-array max-prime)))
	       (let ((result-list (make-repeat-digits-primes a-digit num-repeats num-digits
							     prime-array)))
		 (let ((slen (length shouldbe-list))
		       (rlen (length result-list)))
		   (begin
		     (if (not (= slen rlen))
			 (begin
			   (display (format #f "~a : (~a) : error : a-digit = ~a, num-repeats = ~a, num-digits = ~a, shouldbe = ~a, result = ~a, length mismatch, shouldbe = ~a, result = ~a~%"
					    sub-name test-label-index a-digit num-repeats num-digits
					    shouldbe-list result-list
					    slen rlen))
			   (quit)
			   ))
		     (for-each
		      (lambda (s-elem)
			(begin
			  (if (equal? (member s-elem result-list) #f)
			      (begin
				(display (format #f "~a : (~a) : error : a-digit = ~a, num-repeats = ~a, num-digits = ~a, shouldbe = ~a, result = ~a, discrepancy at ~a~%"
						 sub-name test-label-index a-digit num-repeats num-digits
						 shouldbe-list result-list
						 s-elem))
				(quit)
				))
			  )) shouldbe-list)
		     ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

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
;;; tally-htable key=(list digit num-repeats) value=(list of primes with that key property)
(define (process-tally-statistics num-digits tally-htable debug-flag)
  (let ((sum 0))
    (begin
      (if (equal? debug-flag #t)
	  (begin
	    (display (ice9-format:format #f "    digit d,    M(~a, d),    N(~a, d),    S(~a, d)~%" num-digits num-digits num-digits))
	    (force-output)
	    ))

      (do ((jj 0 (1+ jj)))
	  ((> jj 9))
	(begin
	  (let ((max-count 0))
	    (begin
	      (do ((kk 1 (1+ kk)))
		  ((> kk num-digits))
		(begin
		  (let ((key-list (list jj kk)))
		    (let ((plist (hash-ref tally-htable key-list #f)))
		      (begin
			(if (and (list? plist) (> (length plist) 0)
				 (> kk max-count))
			    (begin
			      (set! max-count kk)
			      ))
			)))
		  ))

	      (let ((key-list (list jj max-count)))
		(let ((plist (hash-ref tally-htable key-list #f)))
		  (begin
		    (if (list? plist)
			(begin
			  (let ((mm max-count)
				(nn (length plist))
				(this-dsum (srfi-1:fold + 0 plist)))
			    (begin
			      (set! sum (+ sum this-dsum))
			      (if (equal? debug-flag #t)
				  (begin
				    (display
                                     (ice9-format:format
                                      #f "    ~:d    ~:d    ~:d    ~:d~%"
                                      jj mm nn this-dsum))
				    (force-output)
				    ))
			      ))
			  ))
		    )))
	      ))
	  ))

      (display
       (ice9-format:format
        #f "For d = 0 to 9, the sum of all S(~a, d) is ~:d~%"
        num-digits sum))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
(define (reproduce-problem-statement)
  (let ((num-digits 4)
	(largest-num 9999)
        (max-prime 10000)
	(debug-flag #t))
    (let ((tally-htable (make-hash-table))
          (prime-array (make-prime-array max-prime)))
      (begin
        ;;; group primes into tally-htable
        ;;; key=(list digit num-repeats) value=(list of primes with that key property)
        (do ((ii-digit 0 (1+ ii-digit)))
            ((> ii-digit 9))
          (begin
            (do ((ii-repeats 1 (1+ ii-repeats)))
                ((>= ii-repeats num-digits))
              (begin
                (let ((plist
                       (make-repeat-digits-primes
                        ii-digit ii-repeats num-digits prime-array)))
                  (begin
                    (let ((key-list (list ii-digit ii-repeats)))
                      (begin
                        (hash-set! tally-htable key-list plist)
                        ))
                    ))
                ))
            ))

        (process-tally-statistics num-digits tally-htable debug-flag)
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop num-digits largest-num max-prime debug-flag)
  (let ((tally-htable (make-hash-table))
        (prime-array (make-prime-array max-prime))
        (last-prime-list (list))
        (last-repeats -1))
    (begin
      ;;; group primes into tally-htable
      ;;; key=(list digit num-repeats) value=(list of primes with that key property)
      (do ((ii-digit 0 (1+ ii-digit)))
          ((> ii-digit 9))
        (begin
          (let ((continue-loop-flag #t))
            (begin
              (time-code
               (begin
                 (do ((ii-repeats (1- num-digits) (1- ii-repeats)))
                     ((or (<= ii-repeats 1)
                          (equal? continue-loop-flag #f)))
                   (begin
                     (let ((plist
                            (make-repeat-digits-primes
                             ii-digit ii-repeats num-digits prime-array)))
                       (begin
                         (if (and (list? plist) (> (length plist) 0))
                             (begin
                               (let ((key-list (list ii-digit ii-repeats)))
                                 (begin
                                   (hash-set! tally-htable key-list plist)
                                   ))

                               (set! last-prime-list plist)
                               (set! last-repeats ii-repeats)
                               (set! continue-loop-flag #f)
                               ))
                         ))
                     ))

                 (display
                  (ice9-format:format
                   #f "completed tally for digit = ~:d, ii-repeats = ~:d, prime-list = ~a : "
                   ii-digit last-repeats last-prime-list))
                 (force-output)
                 ))
              ))
          ))

      (process-tally-statistics num-digits tally-htable debug-flag)
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
(define (main args)
  (begin
    (display (format #f "Project Euler 111 - Considering 4-digit primes containing repeated digits it is clear that they cannot all be the same: 1111 is divisible by 11, 2222 is divisible by 22, and so on. But there are nine 4-digit primes containing three ones:~%"))
    (newline)
    (display (format #f "    1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111~%"))
    (newline)
    (display (format #f "We shall say that M(n, d) represents the maximum number of repeated digits for an n-digit prime where d is the repeated digit, N(n, d) represents the number of such primes, and S(n, d) represents the sum of these primes.~%"))
    (newline)
    (display (format #f "So M(4, 1) = 3 is the maximum number of repeated digits for a 4-digit prime where one is the repeated digit, there are N(4, 1) = 9 such primes, and the sum of these primes is S(4, 1) = 22275. It turns out that for d = 0, it is only possible to have M(4, 0) = 2 repeated digits, but there are N(4, 0) = 13 such cases.~%"))
    (newline)
    (display (format #f "In the same way we obtain the following results for 4-digit primes.~%"))
    (newline)
    (display (format #f "Digit, d	M(4, d)	N(4, d)	S(4, d)~%"))
    (display (format #f "0	2	13	67061~%"))
    (display (format #f "1	3	9	22275~%"))
    (display (format #f "2	3	1	2221~%"))
    (display (format #f "3	3	12	46214~%"))
    (display (format #f "4	3	2	8888~%"))
    (display (format #f "5	3	1	5557~%"))
    (display (format #f "6	3	1	6661~%"))
    (display (format #f "7	3	9	57863~%"))
    (display (format #f "8	3	1	8887~%"))
    (display (format #f "9	3	7	48073~%"))
    (newline)
    (display (format #f "For d = 0 to 9, the sum of all S(4, d) is 273700.~%"))
    (newline)
    (display (format #f "Find the sum of all S(10, d).~%"))
    (newline)
    (force-output)

    ;;; run tests
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-digit-list-to-number-1 counter)
	   (run-test test-make-prime-array-1 counter)
           (run-test test-binary-search-1 counter)
	   (run-test test-is-array-prime-1 counter)
	   (run-test test-make-repeat-digits-primes-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (time-code
     (begin
       (reproduce-problem-statement)
       ))

    (newline)
    (force-output)

    (let ((ndigits 10)
	  (largest-num 9999999999)
          (max-prime 1000000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop ndigits largest-num max-prime debug-flag)
	   ))
	))

    (newline)
    ))
