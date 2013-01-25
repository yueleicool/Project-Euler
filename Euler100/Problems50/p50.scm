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
(define (make-prime-list max-num)
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
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-list-1)
  (let ((sub-name "test-make-prime-list-1")
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
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-list (make-prime-list test-num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
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
			     (r-elem (list-ref result-list ii)))
			 (begin
			   (if (not (equal? s-elem r-elem))
			       (begin
				 (display
                                  (format #f "~a : error (~a) : num=~a, "
                                          sub-name test-label-index test-num))
				 (display
                                  (format #f "shouldbe=~a, result=~a, discrepancy at "
                                          shouldbe-list result-list))
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
	(prime-list (make-prime-list 5))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1))
                 (prime-array (list->array 1 prime-list)))
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
(define (list-to-sum-string llist)
  (let ((this-string
	 (string-join
	  (map
	   (lambda (this-elem)
	     (ice9-format:format #f "~:d" this-elem))
	   llist)
	  " + ")))
    this-string
    ))

;;;#############################################################
;;;#############################################################
(define (test-list-to-sum-string-1)
  (let ((sub-name "test-list-to-sum-string-1")
	(test-list
	 (list
	  (list (list 1) "1")
	  (list (list 1 2) "1 + 2")
	  (list (list 1 2 3) "1 + 2 + 3")
	  (list (list 4 5 6 7) "4 + 5 + 6 + 7")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-sum-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
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
(define (main-loop max-num max-sum)
  (let ((prime-list (make-prime-list max-num))
	(sum 0)
	(sum-list (list))
	(sum-count 0))
    (let ((prime-vector (list->vector prime-list))
          (prime-array (list->array 1 prime-list)))
      (let ((vec-len (vector-length prime-vector)))
	(begin
	  (do ((ii-start 0 (+ ii-start 1)))
	      ((>= ii-start vec-len))
	    (begin
	      (let ((inner-break-flag #f)
		    (current-sum 0)
		    (current-list (list))
		    (current-length 0))
		(begin
		  (do ((jj ii-start (+ jj 1)))
		      ((or (>= jj vec-len)
			   (equal? inner-break-flag #t)))
		    (begin
		      (let ((this-prime (vector-ref prime-vector jj)))
			(begin
			  (set! current-sum (+ current-sum this-prime))
			  (set! current-list (cons this-prime current-list))
			  (set! current-length (+ current-length 1))

			  (if (> current-sum max-sum)
			      (begin
				(set! inner-break-flag #t))
			      (begin
				(if (and
				     (> current-length sum-count)
				     (is-array-prime? current-sum prime-array))
				    (begin
				      (set! sum current-sum)
				      (set! sum-list (reverse current-list))
				      (set! sum-count current-length)
				      ))
				))
			  ))))
		  ))))

	  (if (<= sum 0)
	      (begin
		(display (ice9-format:format #f "no sum found (for numbers less than ~:d)~%"
					     max-num)))
	      (begin
		(display (ice9-format:format #f "the sum of the most consecutive primes (~:d) less than ~:d is:~%" sum-count max-sum))
		(display (ice9-format:format #f "    ~:d = ~a~%"
					     sum (list-to-sum-string sum-list)))
		))
	  )))))

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
    (display (format #f "Problem 050 - The prime 41, can be written as the sum of six consecutive primes:~%"))
    (newline)
    (display (format #f "41 = 2 + 3 + 5 + 7 + 11 + 13~%"))
    (newline)
    (display (format #f "This is the longest sum of consecutive primes that adds to a prime below one-hundred.~%"))
    (display (format #f "The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.~%"))
    (newline)
    (display (format #f "Which prime, below one-million, can be written as the sum of the most consecutive primes?~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-prime-list-1 counter)
           (run-test test-binary-search-1 counter)
           (run-test test-is-array-prime-1 counter)
	   (run-test test-list-to-sum-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 100))
      (begin
        (time-code
         (begin
           (main-loop max-num max-num)
           ))
	))

    (newline)
    (force-output)

    (let ((max-num 1000000))
      (begin
        (time-code
         (begin
           (main-loop max-num max-num)
           ))
	))

    (newline)
    ))
