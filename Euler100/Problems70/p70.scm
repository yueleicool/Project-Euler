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

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

;;;### getopt-long used for command-line option arguments processing
(use-modules ((ice-9 getopt-long)
	      :renamer (symbol-prefix-proc 'ice-9-getopt:)))

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
;;; make a list of primes less than n
(define (make-prime-list max-num)
  (let ((int-array (make-array 0 (1+ max-num)))
        (result-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! int-array ii ii)
          ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
          (let ((ii-num (array-ref int-array ii)))
            (begin
              (if (= ii-num ii)
                  (begin
                    (set! result-list (cons ii-num result-list))

                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj max-num))
                      (begin
                        (array-set! int-array -1 jj)
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
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (make-prime-list test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
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
(define (divide-out-number this-num pnum)
  (let ((result this-num))
    (begin
      (while
       (zero? (modulo result pnum))
       (begin
	 (set! result (euclidean/ result pnum))
	 ))
      result
      )))

;;;#############################################################
;;;#############################################################
(define (test-divide-out-number-1)
  (let ((sub-name "test-divide-out-number-1")
	(test-list
	 (list
	  (list 4 2 1) (list 6 3 2) (list 9 3 1)
	  (list 10 2 5) (list 10 5 2) (list 11 7 11)
	  (list 28 2 7) (list 28 7 4)
	  (list 36 2 9) (list 36 3 4)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((input-num (list-ref this-list 0))
		 (pnum (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (divide-out-number input-num pnum)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : input-num=~a, pnum=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index input-num pnum
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
(define (single-prime-factor-divisors-list input-number prime-list)
  (let ((result-list (list)))
    (begin
      (cond
       ((or (<= input-number 1)
	    (not (list? prime-list))
	    (< (length prime-list) 1))
	(list))
       ((not (equal? (member input-number prime-list) #f))
	(list input-number))
       (else
	(let ((result-list (list))
	      (plen (length prime-list))
	      (pmax (car (last-pair prime-list)))
	      (ll-max (+ (exact-integer-sqrt input-number) 1))
	      (this-prime (car prime-list))
	      (local-num input-number))
	  (begin
	    (do ((ii 0 (1+ ii)))
		((or (>= ii plen)
		     (> this-prime ll-max)))
	      (begin
		(set! this-prime (list-ref prime-list ii))

		(if (zero? (modulo local-num this-prime))
		    (begin
		      (if (equal? (member this-prime result-list) #f)
			  (begin
			    (set! result-list (cons this-prime result-list))
			    ))

		      (let ((div (euclidean/ local-num this-prime)))
			(begin
			  (if (prime? div)
			      (begin
				(if (and (> div this-prime)
					 (equal? (member div result-list) #f))
				    (begin
				      (set! result-list (cons div result-list))
				      ))

				(set! local-num (divide-out-number local-num this-prime))
				))
			  ))

		      (set! local-num (divide-out-number local-num this-prime))
		      ))
		))

	    (if (< pmax ll-max)
		(begin
		  (display (ice9-format:format #f "warning max generated prime = ~:d less than required sqrt(~:d) = ~a, ll-max=~:d~%" pmax input-number (sqrt input-number) ll-max))
		  (display (format #f "stopping program...~%"))
		  (quit)
		  ))

	    (sort result-list <)
	    ))))
      )))

;;;#############################################################
;;;#############################################################
(define (test-single-prime-factor-divisors-list-1)
  (let ((sub-name "test-single-prime-factor-divisors-list-1")
	(test-list
	 (list
	  (list 2 (list 2)) (list 3 (list 3)) (list 4 (list 2))
	  (list 5 (list 5)) (list 6 (list 2 3))
	  (list 7 (list 7)) (list 8 (list 2))
	  (list 9 (list 3)) (list 10 (list 2 5))
	  (list 11 (list 11)) (list 12 (list 2 3))
	  (list 13 (list 13)) (list 14 (list 2 7))
	  (list 15 (list 3 5)) (list 16 (list 2))
	  (list 17 (list 17)) (list 18 (list 2 3))
	  (list 19 (list 19)) (list 20 (list 2 5))
	  (list 21 (list 3 7)) (list 22 (list 2 11))
	  (list 23 (list 23)) (list 24 (list 2 3))
	  (list 25 (list 5)) (list 26 (list 2 13))
	  (list 27 (list 3)) (list 28 (list 2 7))
	  (list 29 (list 29)) (list 30 (list 2 3 5))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((plist (make-prime-list test-num)))
	       (let ((result (single-prime-factor-divisors-list test-num plist)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-num
					  shouldbe result))
			 (quit)
			 ))
		   ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; count the number of relatively prime numbers to n
;;; if n even, all even numbers are not relatively prime
(define (totient-function nn prime-list)
  (let ((tproduct nn)
	(ll-max (1+ (exact-integer-sqrt nn)))
	(pmax (car (last-pair prime-list)))
	(sprime-list (single-prime-factor-divisors-list nn prime-list)))
    (let ((splen (length sprime-list)))
      (begin
	(cond
	 ((or (<= nn 1)
	      (> ll-max pmax))
	  (begin
	    (set! tproduct -1)))
	 (else
	  (do ((ii 0 (1+ ii)))
	      ((>= ii splen))
	    (begin
	      (set! tproduct
		    (* tproduct
		       (- 1 (/ 1 (list-ref sprime-list ii))
			  )))
	      ))))
	tproduct
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-totient-function-1)
  (let ((sub-name "test-totient-function-1")
	(test-list
	 (list
	  (list 2 1) (list 3 2) (list 4 2)
	  (list 5 4) (list 6 2) (list 7 6)
	  (list 8 4) (list 9 6) (list 10 4)
	  (list 11 10) (list 12 4) (list 13 12)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((prime-list (make-prime-list test-num)))
	       (let ((result (totient-function test-num prime-list)))
		 (begin
		   (if (not (equal? shouldbe result))
		       (begin
			 (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
					  sub-name test-label-index test-num
					  shouldbe result))
			 (quit)
			 ))
		   ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (totient-list-function nn)
  (let ((result-list (list)))
    (begin
      (cond
       ((<= nn 1) #f)
       ((even? nn)
	(do ((ii 1 (+ ii 2)))
	    ((>= ii nn))
	  (begin
	    (if (equal? (gcd nn ii) 1)
		(begin
		  (set! result-list (cons ii result-list))
		  ))
	    )))
       (else
	(do ((ii 1 (+ ii 1)))
	    ((>= ii nn))
	  (begin
	    (if (equal? (gcd nn ii) 1)
		(begin
		  (set! result-list (cons ii result-list))
		  ))
	    ))
	))
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-totient-list-function-1)
  (let ((sub-name "test-totient-list-function-1")
	(test-list
	 (list
	  (list 2 (list 1)) (list 3 (list 1 2))
	  (list 4 (list 1 3)) (list 5 (list 1 2 3 4))
	  (list 6 (list 1 5)) (list 7 (list 1 2 3 4 5 6))
	  (list 8 (list 1 3 5 7)) (list 9 (list 1 2 4 5 7 8))
	  (list 10 (list 1 3 7 9))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (totient-list-function test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (cond
     ((< this-num 0) acc-list)
     ((< this-num 10) (cons this-num acc-list))
     (else
      (srfi-11:let-values
       (((next-num this-digit) (euclidean/ this-num 10)))
       (begin
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
(define (populate-digits-hash! digits-htable prime-list)
  (begin
    (hash-clear! digits-htable)

    (for-each
     (lambda (aprime)
       (begin
         (let ((dlist (split-digits-list aprime)))
           (begin
             (hash-set! digits-htable aprime dlist)
             ))
         )) prime-list)
    ))

;;;#############################################################
;;;#############################################################
(define (is-num1-permutation-num2? num1 num2 digits-htable)
  (let ((num1-dlist (hash-ref digits-htable num1 #f))
        (num2-dlist (hash-ref digits-htable num2 #f)))
    (begin
      (if (equal? num1-dlist #f)
          (begin
            (let ((dlist (split-digits-list num1)))
              (begin
                (hash-set! digits-htable num1 dlist)
                (set! num1-dlist dlist)
                ))
            ))
      (if (equal? num2-dlist #f)
          (begin
            (let ((dlist (split-digits-list num2)))
              (begin
                (hash-set! digits-htable num2 dlist)
                (set! num2-dlist dlist)
                ))
            ))

      (let ((a1-dlist (sort num1-dlist <))
            (a2-dlist (sort num2-dlist <)))
        (begin
          (equal? a1-dlist a2-dlist)
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-num1-permutation-num2-1)
  (let ((sub-name "test-is-num1-permutation-num2-1")
	(test-list
	 (list
	  (list 2 2 #t) (list 9 9 #t)
	  (list 11 11 #t) (list 12 21 #t)
	  (list 13 31 #t) (list 99 99 #t)
	  (list 123 321 #t) (list 123 124 #f)
	  ))
        (split-list (list 2 9 11 12 13))
        (digits-htable (make-hash-table 20))
	(test-label-index 0))
    (begin
      (populate-digits-hash! digits-htable split-list)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((num1 (list-ref alist 0))
		 (num2 (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result
                    (is-num1-permutation-num2?
                     num1 num2 digits-htable)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : num1 = ~a, num = ~a : "
                         sub-name test-label-index num1 num2))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (reproduce-problem-statement)
  (let ((num 87109)
	(max-prime 1000)
        (digits-htable (make-hash-table 10)))
    (let ((prime-list (make-prime-list max-prime)))
      (let ((totient (totient-function num prime-list)))
	(let ((totient-ratio (exact->inexact (/ num totient)))
	      (perm-flag
               (is-num1-permutation-num2? num totient digits-htable)))
	  (begin
	    (display
             (ice9-format:format
              #f "n=~:d, totient = ~:d, ratio = ~1,6f, is-permutation=~a~%"
              num totient totient-ratio
              (if (equal? perm-flag #t) "true" "false")))
	    (force-output)
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (find-min-totient-ratio
         depth max-depth index end-index
         max-num prime-list digits-htable
         current-num current-list
         acc-min-ratio acc-list)
  (begin
    (if (>= depth max-depth)
        (begin
          (let ((totient
                 (srfi-1:fold
                  (lambda (anum prev)
                    (* (1- anum) prev)) 1 current-list)))
            (begin
              (let ((ratio (exact->inexact (/ current-num totient))))
                (begin
                  (if (or (< acc-min-ratio 0)
                          (< ratio acc-min-ratio))
                      (begin
                        (if (and
                             (<= current-num max-num)
                             (is-num1-permutation-num2?
                              current-num totient digits-htable))
                            (begin
                              (set! acc-min-ratio ratio)
                              (set! acc-list
                                    (list current-num totient ratio current-list))
                              ))
                        ))
                  ))
              (list acc-min-ratio acc-list)
              )))
        (begin
          (let ((next-depth (1+ depth))
                (break-flag #f))
            (begin
              (do ((ii index (1+ ii)))
                  ((or (>= ii end-index)
                       (equal? break-flag #t)))
                (begin
                  (let ((aprime (list-ref prime-list ii)))
                    (let ((next-index (1+ ii))
                          (next-current-list (cons aprime current-list))
                          (nn (* current-num aprime)))
                      (begin
                        (if (<= nn max-num)
                            (begin
                              (let ((result-list
                                     (find-min-totient-ratio
                                      next-depth max-depth next-index end-index
                                      max-num prime-list digits-htable
                                      nn next-current-list
                                      acc-min-ratio acc-list)))
                                (begin
                                  (if (and (list? result-list)
                                           (> (length result-list) 0))
                                      (begin
                                        (let ((next-ratio
                                               (list-ref result-list 0))
                                              (next-acc-list
                                               (list-ref result-list 1)))
                                          (begin
                                            (set! acc-min-ratio next-ratio)
                                            (set! acc-list next-acc-list)
                                            ))
                                        ))
                                  )))
                            (begin
                              (set! break-flag #t)
                              ))
                        )))
                  ))
              (list acc-min-ratio acc-list)
              ))
          ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num end-num max-prime max-num)
  (let ((prime-list (make-prime-list max-prime))
        (digits-htable (make-hash-table 1000))
	(max-nn -1)
	(max-totient -1)
	(dmax-ratio -1)
	(factors-list (list))
	(start-jday (srfi-19:current-julian-day)))
    (let ((end-index (length prime-list)))
      (begin
        (populate-digits-hash! digits-htable prime-list)

        (do ((ii start-num (1+ ii)))
            ((> ii end-num))
          (begin
            (let ((rlist
                   (find-min-totient-ratio
                    0 ii 0 end-index max-num
                    prime-list digits-htable
                    1 (list) dmax-ratio (list))))
              (begin
                (if (and (list? rlist)
                         (> (length rlist) 0))
                    (begin
                      (let ((ii-min-ratio (list-ref rlist 0))
                            (acc-list (list-ref rlist 1)))
                        (begin
                          (if (and (list? acc-list)
                                   (> (length acc-list) 0))
                              (begin
                                (let ((nn (list-ref acc-list 0))
                                      (totient (list-ref acc-list 1))
                                      (rr (list-ref acc-list 2))
                                      (flist (list-ref acc-list 3)))
                                  (begin
                                    (if (or (<= dmax-ratio 0)
                                            (< ii-min-ratio dmax-ratio))
                                        (begin
                                          (set! dmax-ratio ii-min-ratio)
                                          (set! max-nn nn)
                                          (set! max-totient totient)
                                          (set! factors-list flist)
                                          ))
                                    ))
                                ))
                          ))
                      ))
                ))

            (let ((end-jday (srfi-19:current-julian-day)))
              (begin
                (display
                 (ice9-format:format
                  #f "completed ~:d : ratio so far=~1,6f, nn=~:d, totient=~:d, "
                  ii dmax-ratio max-nn max-totient))
                (display
                 (ice9-format:format
                  #f "factors list = ~a : elapsed time = ~a : ~a~%"
                  factors-list
                  (julian-day-difference-to-string end-jday start-jday)
                  (date-time-to-string (srfi-19:current-date))))
                (force-output)
                (set! start-jday end-jday)
                ))
            ))

        (if (> dmax-ratio 0)
            (begin
              (display
               (ice9-format:format
                #f "for numbers that are products of primes ranging from ~:d to ~:d factors~%"
                start-num end-num))
              (let ((perm-flag
                     (is-num1-permutation-num2?
                      max-nn max-totient digits-htable)))
                (begin
                  (display
                   (ice9-format:format
                    #f "min ratio = ~1,6f, num=~:d, totient=~:d, "
                    dmax-ratio max-nn max-totient))
                  (display
                   (ice9-format:format
                    #f "factors-list=~a, is-permutation=~a~%"
                    factors-list (if (equal? perm-flag #t) "true" "false")))
                  (force-output)
                  )))
            (begin
              (display
               (ice9-format:format
                #f "no numbers found between ~:d to ~:d factors~%"
                start-num end-num))
              ))
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
    (display (format #f "Problem 070 - Euler's Totient function, phi(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, phi(9)=6.~%"))
    (newline)
    (display (format #f "The number 1 is considered to be relatively prime to every positive number, so phi(1)=1.~%"))
    (newline)
    (display (format #f "Interestingly, phi(87109)=79180, and it can be seen that 87109 is a permutation of 79180.~%"))
    (newline)
    (display (format #f "Find the value of n, 1 < n < 10^7, for which phi(n) is a permutation of n and the ratio n/phi(n) produces a minimum.~%"))
    (newline)
    (display (format #f "The solution can be found at http://www.mathblog.dk/project-euler-70-investigate-values-of-n-for-which-%CF%86n-is-a-permutation-of-n/~%"))
    (display (format #f "and the description of the totient function at http://en.wikipedia.org/wiki/Totient_function~%"))
    (display (format #f "phi(n) = n x (1 - 1/p1) x (1 - 1/p2) x ... x (1 - 1/pk)~%"))
    (display (format #f "where the prime factors of n are p1, p2, ..., pk.~%"))
    (newline)
    (display (format #f "Since we are looking for a minimum ratio n/phi(n), this means that we should look for the smallest n, with the largest phi(n).~%"))
    (display (format #f "Kristian Edlund argues that since phi(n) = n-1 when n is a prime, that it cannot be a permutation of n, so n must be a product of two or more primes.~%"))
    (display (format #f "The largest value of phi occurs when n contains a few large primes.~%"))
    (display (format #f "This program looks for the minimum ratio of n/phi(n) that can be found for 2, 3, 4, ... prime factors.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-make-prime-list-1 counter)
	   (run-test test-divide-out-number-1 counter)
	   (run-test test-single-prime-factor-divisors-list-1 counter)
	   (run-test test-totient-function-1 counter)
	   (run-test test-totient-list-function-1 counter)
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-is-num1-permutation-num2-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    ;;; show that the program is working with a simple test case
    (time-code
     (begin
       (reproduce-problem-statement)
       ))

    (newline)
    (force-output)

    (let ((start-num 2)
	  (end-num 4)
	  (max-prime 10000)
	  (max-num 10000000))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num max-prime max-num)
	   ))
	))

    (newline)
    ))
