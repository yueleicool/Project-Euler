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
(define (divide-all-factors this-num this-factor)
  (let ((ll-num this-num)
        (acc-list (list)))
    (begin
      (while (and (zero? (modulo ll-num this-factor))
                  (> ll-num 1))
        (begin
          (set! ll-num (euclidean/ ll-num this-factor))
          (set! acc-list (cons this-factor acc-list))
          ))

      (list ll-num acc-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-divide-all-factors-1)
  (let ((sub-name "test-divide-all-factors-1")
	(test-list
	 (list
	  (list 0 2 (list 0 (list)))
          (list 2 2 (list 1 (list 2)))
          (list 4 2 (list 1 (list 2 2)))
          (list 8 2 (list 1 (list 2 2 2)))
          (list 12 2 (list 3 (list 2 2)))
          (list 20 2 (list 5 (list 2 2)))
          (list 33 3 (list 11 (list 3)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((num (list-ref this-list 0))
                 (factor (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (divide-all-factors num factor)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format #f "~a : error (~a) : num=~a, factor=~a, "
                                sub-name test-label-index num factor))
		       (display
                        (format #f "shouldbe=~a, result=~a~%"
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
;;; populate a hash with numbers and a list of prime factors
;;; sieve of eratosthenes method
(define (populate-prime-div-list-hash! factor-htable max-num)
  (let ((prime-array (make-array 0 (1+ max-num)))
        (factor-list-array (make-array (list) (1+ max-num))))
    (begin
      (hash-clear! factor-htable)

      (do ((ii 0 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! prime-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
          (let ((ii-flag (array-ref prime-array ii)))
            (begin
              (do ((jj ii (+ jj ii)))
                  ((> jj max-num))
                (begin
                  (let ((this-num (array-ref prime-array jj)))
                    (begin
                      (if (not (= this-num ii))
                          (begin
                            (array-set! prime-array -1 jj)

                            (if (= ii-flag ii)
                                (begin
                                  (let ((rlist (divide-all-factors jj ii)))
                                    (let ((fact-list (cadr rlist))
                                          (this-list (array-ref factor-list-array jj)))
                                      (begin
                                        (array-set!
                                         factor-list-array
                                         (append this-list fact-list) jj)
                                        )))
                                  ))
                            ))
                      ))
		  ))
	      ))

          (let ((this-list (array-ref factor-list-array ii))
                (tflag (array-ref prime-array ii)))
            (begin
              (if (and (equal? this-list (list))
                       (equal? tflag ii))
                  (begin
                    (hash-set! factor-htable ii (list ii)))
                  (begin
                    (hash-set! factor-htable ii this-list)
                    ))
              ))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-prime-div-list-hash-1)
  (let ((sub-name "test-populate-prime-div-list-hash-1")
	(test-list
	 (list
	  (list 2 (list 2)) (list 3 (list 3)) (list 4 (list 2 2))
	  (list 5 (list 5)) (list 6 (list 2 3))
	  (list 7 (list 7)) (list 8 (list 2 2 2))
	  (list 9 (list 3 3)) (list 10 (list 2 5))
	  (list 11 (list 11)) (list 12 (list 2 2 3))
          (list 13 (list 13)) (list 14 (list 2 7))
          (list 15 (list 3 5)) (list 16 (list 2 2 2 2))
	  (list 17 (list 17)) (list 18 (list 2 3 3))
	  (list 19 (list 19)) (list 20 (list 2 2 5))
	  ))
        (factors-htable (make-hash-table 20))
        (max-num 20)
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (populate-prime-div-list-hash! factors-htable max-num)

      (for-each
       (lambda (this-list)
	 (begin
	   (let ((num (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-list (hash-ref factors-htable num #f)))
               (begin
                 (if (not (equal? shouldbe-list result-list))
                     (begin
                       (display
                        (format
                         #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
                         sub-name test-label-index num
                         shouldbe-list result-list))
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
(define (prime? nn)
  (define (smallest-divisor nn test-divisor max-divisor)
    (cond
     ((> test-divisor max-divisor) nn)
     ((zero? (modulo nn test-divisor)) test-divisor)
     (else
      (smallest-divisor nn (+ test-divisor 2) max-divisor)
      )))
  (begin
    (cond
     ((<= nn 1) #f)
     ((= nn 2) #t)
     ((zero? (modulo nn 2)) #f)
     (else
      (let ((max-divisor
	     (+ (exact-integer-sqrt nn) 1)))
	(= nn (smallest-divisor nn 3 max-divisor)))
      ))))

;;;#############################################################
;;;#############################################################
(define (test-prime-1)
  (let ((sub-name "test-prime-1")
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
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (prime? test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num=~a, prime? shouldbe=~a, result=~a~%"
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax divide-macro-process
  (syntax-rules ()
    ((divide-macro-process
      ll-num div-factor constant-number result-list)
     (begin
       (if (prime? div-factor)
           (begin
             (let ((partial-list (divide-all-factors ll-num div-factor)))
               (begin
                 (set! ll-num (car partial-list))
                 (set! result-list
                       (append result-list (cadr partial-list)))
                 ))
             ))

       (let ((this-divisor
              (euclidean/ constant-number div-factor)))
         (begin
           (if (and (> this-divisor div-factor)
                    (equal? (memq this-divisor result-list) #f)
                    (prime? this-divisor))
               (begin
                 (let ((partial-list
                        (divide-all-factors ll-num this-divisor)))
                   (begin
                     (set! ll-num (car partial-list))
                     (set! result-list
                           (append result-list (cadr partial-list)))
                     ))
                 ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (prime-factor-list input-number)
  (begin
    (if (<= input-number 1)
      (begin
        (list))
      (begin
        (let ((result-list (list))
              (constant-number input-number)
              (ll-num input-number)
              (ll-max (+ 1 (exact-integer-sqrt input-number))))
          (begin
            (if (even? constant-number)
                (begin
                  (divide-macro-process
                   ll-num 2 constant-number result-list)
                  ))

            (do ((ii 3 (+ ii 2)))
                ((or (> ii ll-max) (<= ll-num 1)))
              (begin
                (if (zero? (modulo constant-number ii))
                    (begin
                      (divide-macro-process
                       ll-num ii constant-number result-list)
                      ))
                ))

            (if (< (length result-list) 1)
                (begin
                  (list input-number))
                (begin
                  (sort result-list <)
                  ))
            ))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-prime-factor-list-1)
  (let ((sub-name "test-prime-factor-list-1")
	(test-list
	 (list
	  (list 2 (list 2)) (list 3 (list 3)) (list 4 (list 2 2))
	  (list 5 (list 5)) (list 6 (list 2 3)) (list 7 (list 7))
	  (list 8 (list 2 2 2)) (list 9 (list 3 3)) (list 10 (list 2 5))
	  (list 11 (list 11)) (list 12 (list 2 2 3)) (list 13 (list 13))
	  (list 14 (list 2 7)) (list 15 (list 3 5)) (list 16 (list 2 2 2 2))
	  (list 17 (list 17)) (list 18 (list 2 3 3)) (list 19 (list 19))
	  (list 20 (list 2 2 5)) (list 21 (list 3 7)) (list 22 (list 2 11))
	  (list 100 (list 2 2 5 5))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (prime-factor-list test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : (~a) : error for test num = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (contains-nn-distinct-primes p1-list nn)
  (let ((p1-distinct (srfi-1:delete-duplicates p1-list)))
    (let ((p1-len (length p1-distinct)))
      (begin
	(= nn p1-len)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-contains-nn-distinct-primes-1)
  (let ((sub-name "test-contains-nn-distinct-primes-1")
	(test-list
	 (list
	  (list (list 2 7) 2 #t)
	  (list (list 3 5) 2 #t)
	  (list (list 2 3 5) 3 #t)
	  (list (list 7 11 13) 3 #t)
	  (list (list 2 2 7 23) 3 #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (test-seq (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (contains-nn-distinct-primes test-list test-seq)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : list=~a, seq=~a, shouldbe=~a, result=~a~%"
                         sub-name test-label-index test-list test-seq
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
(define (main-loop max-num seq-num debug-flag)
  (let ((break-flag #f)
        (factors-htable (make-hash-table max-num))
	(seq-counter 0)
	(seq-list (list)))
    (begin
      (populate-prime-div-list-hash! factors-htable max-num)

      (do ((nn 1 (1+ nn)))
	  ((or (> nn max-num)
	       (equal? break-flag #t)))
	(begin
	  (let ((this-list (hash-ref factors-htable nn (list))))
	    (begin
	      (if (contains-nn-distinct-primes this-list seq-num)
		  (begin
		    (set! seq-counter (+ seq-counter 1))
		    (set! seq-list (cons (list nn this-list) seq-list))
		    (if (>= seq-counter seq-num)
			(begin
			  (set! break-flag #t)
			  (for-each
			   (lambda (this-list)
                             (begin
                               (let ((this-num (list-ref this-list 0))
                                     (plist (list-ref this-list 1)))
                                 (let ((pstring
                                        (string-join
                                         (map
                                          (lambda (num)
                                            (begin
                                              (ice-9-format:format #f "~:d" num)
                                              ))
                                          plist) " x ")))
                                   (begin
                                     (display
                                      (ice-9-format:format
                                       #f "~:d = ~a~%"
                                       this-num pstring))
                                     )))
                               )) (reverse seq-list))
			  )))
		  (begin
		    (set! seq-counter 0)
		    (set! seq-list (list))
		    ))
	      ))
          ))

      (if (< seq-counter seq-num)
	  (begin
	    (display
             (ice-9-format:format
              #f "no ~:d sequence found (for numbers less than ~:d)~%"
              seq-num max-num)))
          (begin
            (display
             (ice-9-format:format
              #f "min ~:d sequence found~%" seq-num))
            ))
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
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
    (display (format #f "Problem 047 - The first two consecutive numbers to have two distinct prime factors are:~%"))
    (newline)
    (display (format #f "14 = 2 x 7~%"))
    (display (format #f "15 = 3 x 5~%"))
    (newline)
    (display (format #f "The first three consecutive numbers to have three distinct prime factors are:~%"))
    (display (format #f "644 = 2^2 x 7 x 23~%"))
    (display (format #f "645 = 3 x 5 x 43~%"))
    (display (format #f "646 = 2 x 17 x 19~%"))
    (newline)
    (display (format #f "Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
           (run-test test-divide-all-factors-1 counter)
           (run-test test-populate-prime-div-list-hash-1 counter)
	   (run-test test-prime-1 counter)
	   (run-test test-contains-nn-distinct-primes-1 counter)

	   (display (ice-9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 1000)
	  (nn 3)
	  (debug-flag #t))
      (begin
        (time-code
         (begin
           (main-loop max-num nn debug-flag)
           ))
	))

    (newline)
    (force-output)

    (let ((max-num 200000)
	  (nn 4)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num nn debug-flag)
	   ))
	))

    (newline)
    ))
