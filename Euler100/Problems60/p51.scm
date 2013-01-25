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
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (replace-vector-with-same-digit-list local-vector)
  (let ((vlen (vector-length local-vector))
	(replace-index-list (list))
	(results-list (list)))
    (begin
      (do ((ii 0 (+ ii 1)))
	  ((>= ii vlen))
	(begin
	  (let ((this-digit (vector-ref local-vector ii)))
	    (if (< this-digit 0)
		(set! replace-index-list (cons ii replace-index-list))
		))))

      (do ((jj 0 (+ jj 1)))
	  ((> jj 9))
	(begin
	  (let ((vcopy (vector-copy local-vector)))
	    (begin
	      (for-each
	       (lambda (this-index)
		 (vector-set! vcopy this-index jj))
	       replace-index-list)

	      (let ((dlist (vector->list vcopy)))
		(let ((dnum (digit-list-to-number dlist)))
		  (if (prime? dnum)
		      (if (not (zero? (list-ref dlist 0)))
			  (set! results-list (cons dnum results-list))
			  ))))
	      ))))
      (sort results-list <)
      )))

;;;#############################################################
;;;#############################################################
(define (test-replace-vector-with-same-digit-list-1)
  (let ((sub-name "test-replace-vector-with-same-digit-list-1")
	(test-list
	 (list
	  (list (list -1 3) (list 13 23 43 53 73 83))
	  (list (list 5 6 -1 -1 3) (list 56003 56113 56333 56443 56663 56773 56993))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (replace-vector-with-same-digit-list
			    (list->vector test-list))))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (substitute-digits-list digit-list config-list num-primes)
  (let ((result-list (list))
	(dlen (length digit-list))
	(rlist (reverse digit-list)))
    (let ((tlen (- dlen 1)))
      (begin
	(for-each
	 (lambda (cfg-list)
	   (begin
	     (let ((c-len (- (length cfg-list) 1))
		   (c-num (srfi-1:fold + 0 cfg-list))
		   (this-list (list))
		   (jj tlen))
	       (begin
		 (if (= dlen c-num)
		     (begin
		       (do ((ii c-len (1- ii)))
			   ((< ii 0))
			 (begin
			   (let ((c-flag (list-ref cfg-list ii)))
			     (begin
			       (if (and (= c-flag 1) (>= jj 0))
				   (begin
				     (set! this-list
					   (cons (list-ref digit-list jj) this-list))
				     (set! jj (1- jj)))
				   (begin
				     (set! this-list
					   (cons -1 this-list))
				     ))
			       ))
			   ))
		       (let ((plist (replace-vector-with-same-digit-list
				     (list->vector this-list))))
			 (let ((num-results (length result-list))
			       (num-plist (length plist)))
			   (begin
			     (if (and (>= num-plist num-primes)
				      (> num-plist num-results))
				 (begin
				   (set! result-list plist)
				   ))
			     )))
		       ))
		 ))
	     )) config-list)
	result-list
	))
    ))

;;;#############################################################
;;;#############################################################
(define (possible-3-digits-list digit-list num-primes)
  (let ((dlen (length digit-list))
	(config-list
	 (list (list 1 0 1)
	       (list 0 1 1)
	       (list 0 0 1))))
    (begin
      (cond
       ((<= dlen 1) #f)
       (else
	(let ((result-list
	       (substitute-digits-list digit-list config-list num-primes)))
	  (begin
	    result-list
	    ))
	))
      )))

;;;#############################################################
;;;#############################################################
(define (test-possible-3-digits-list-1)
  (let ((sub-name "test-possible-3-digits-list-1")
	(test-list
	 (list
	  (list (list 2 1) 5 (list 211 241 251 271 281))
	  (list (list 3 1) 4 (list 131 331 431 631))
	  (list (list 1 3) 2 (list 103 113 163 173 193))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (num-primes (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (possible-3-digits-list test-list num-primes)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (possible-4-digits-list digit-list num-primes)
  (let ((dlen (length digit-list))
	(config-list
	 (list (list 1 0 0 1)
	       (list 0 1 0 1)
	       (list 0 0 1 1)
	       (list 1 1 0 1)
	       (list 1 0 1 1)
	       (list 0 1 1 1))))
    (begin
      (cond
       ((<= dlen 1) #f)
       (else
	(let ((result-list
	       (substitute-digits-list digit-list config-list num-primes)))
	  (begin
	    result-list
	    ))
	))
      )))

;;;#############################################################
;;;#############################################################
(define (test-possible-4-digits-list-1)
  (let ((sub-name "test-possible-4-digits-list-1")
	(test-list
	 (list
	  (list (list 7 2 1) 3 (list 7121 7321 7621))
	  (list (list 5 3 1) 3 (list 5231 5431 5531))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (num-primes (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (possible-4-digits-list test-list num-primes)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (possible-5-digits-list digit-list num-primes)
  (let ((dlen (length digit-list))
	(config-list
	 (list (list 1 0 0 0 1)
	       (list 0 1 0 0 1)
	       (list 0 0 1 0 1)
	       (list 0 0 0 1 1)
	       (list 1 1 0 0 1)
	       (list 1 0 1 0 1)
	       (list 1 0 0 1 1)
	       (list 0 1 1 0 1)
	       (list 0 1 0 1 1)
	       (list 0 0 1 1 1)
	       (list 1 1 1 0 1)
	       (list 1 1 0 1 1)
	       (list 1 0 1 1 1)
	       (list 0 1 1 1 1)
	       )))
    (begin
      (cond
       ((<= dlen 1) #f)
       (else
	(let ((result-list
	       (substitute-digits-list digit-list config-list num-primes)))
	  (begin
	    result-list
	    ))
	))
      )))

;;;#############################################################
;;;#############################################################
(define (test-possible-5-digits-list-1)
  (let ((sub-name "test-possible-5-digits-list-1")
	(test-list
	 (list
	  (list (list 3 7 2 1) 3 (list 31721 33721 34721 36721))
	  (list (list 8 5 3 1) 3 (list 85331 85531 85831 85931))
	  (list (list 5 6 3) 3 (list 56003 56113 56333 56443 56663 56773 56993))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (num-primes (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (possible-5-digits-list test-list num-primes)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (possible-6-digits-list digit-list num-primes)
  (let ((dlen (length digit-list))
	(config-list
	 (list (list 1 0 0 0 0 1)
	       (list 0 1 0 0 0 1)
	       (list 0 0 1 0 0 1)
	       (list 0 0 0 1 0 1)
	       (list 0 0 0 0 1 1)
	       (list 1 1 0 0 0 1)
	       (list 1 0 1 0 0 1)
	       (list 1 0 0 1 0 1)
	       (list 1 0 0 0 1 1)
	       (list 0 1 1 0 0 1)
	       (list 0 1 0 1 0 1)
	       (list 0 1 0 0 1 1)
	       (list 0 0 1 1 0 1)
	       (list 0 0 1 0 1 1)
	       (list 0 0 0 1 1 1)
	       (list 1 1 1 0 0 1)
	       (list 1 1 0 1 0 1)
	       (list 1 1 0 0 1 1)
	       (list 1 0 1 1 0 1)
	       (list 1 0 1 0 1 1)
	       (list 1 0 0 1 1 1)
	       (list 0 1 1 1 0 1)
	       (list 0 1 1 0 1 1)
	       (list 0 1 0 1 1 1)
	       (list 0 0 1 1 1 1)
	       (list 1 1 1 1 0 1)
	       (list 1 1 1 0 1 1)
	       (list 1 1 0 1 1 1)
	       (list 1 0 1 1 1 1)
	       (list 0 1 1 1 1 1)
	       )))
    (begin
      (cond
       ((<= dlen 1) #f)
       (else
	(let ((result-list
	       (substitute-digits-list digit-list config-list num-primes)))
	  (begin
	    result-list
	    ))
	))
      )))

;;;#############################################################
;;;#############################################################
(define (test-possible-6-digits-list-1)
  (let ((sub-name "test-possible-6-digits-list-1")
	(test-list
	 (list
	  (list (list 3 3 7 2 1) 3 (list 330721 333721 334721 337721))
	  (list (list 3 6 5 3 1) 3 (list 361531 365531 367531 368531))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (num-primes (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (possible-6-digits-list test-list num-primes)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; replace nn digits at a time in input vector
;;; return a list of primes
(define (possible-n-digits-list digit-list min-num)
  (let ((dlen (length digit-list))
	(min-list (list))
	(min-length 0)
	(min-elem 0)
	(function-list (list possible-3-digits-list
			     possible-4-digits-list
			     possible-5-digits-list
			     possible-6-digits-list)))
    (begin
      (for-each
       (lambda (possible-func)
	 (begin
	   (let ((rlist
		  (possible-func digit-list min-num)))
	     (begin
	       (if (and (list? rlist) (> (length rlist) 0))
		   (begin
		     (let ((rlen (length rlist)))
		       (begin
			 (if (= rlen min-num)
			     (begin
			       (let ((this-elem (car rlist)))
				 (begin
				   (if (or (<= min-elem 0)
					   (< this-elem min-elem))
				       (begin
					 (set! min-list rlist)
					 (set! min-length rlen)
					 (set! min-elem this-elem)
					 ))
				   ))
			       ))
			 ))
		     ))
	       ))
	   )) function-list)

      min-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-possible-n-digits-list-1)
  (let ((sub-name "test-possible-n-digits-list-1")
	(test-list
	 (list
	  (list (list 2 3) 5 (list 223 233 263 283 293))
	  (list (list 1 2 3) 6 (list 1123 1223 1423 1523 1723 1823))
	  (list (list 5 6 1 3) 3 (list 56113 56713 56813))
	  (list (list 5 6 3) 7 (list 56003 56113 56333 56443 56663 56773 56993))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (test-nn (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (possible-n-digits-list test-list test-nn)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, nn=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list test-nn
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (list-to-set-string llist)
  (let ((stmp (string-join
	       (map
		(lambda (num)
		  (ice9-format:format #f "~:d" num)) llist)
	       ", ")))
    (begin
      (string-append "{ " stmp " }")
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-set-string-1)
  (let ((sub-name "test-list-to-set-string-1")
	(test-list
	 (list
	  (list (list 1) "{ 1 }")
	  (list (list 1 2) "{ 1, 2 }")
	  (list (list 1 2 3) "{ 1, 2, 3 }")
	  (list (list 4 5 6 7) "{ 4, 5, 6, 7 }")
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (list-to-set-string test-list)))
	       (begin
		 (if (not (string-ci=? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : list=~a, shouldbe=~a, result=~a~%"
					sub-name test-label-index test-list
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (+ test-label-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num end-num min-nn)
  (let ((smallest-list (list))
	(smallest-count 0)
	(smallest-elem 0)
	(begin-num start-num)
	(counter 0)
	(break-flag #f))
    (begin
      (if (zero? (modulo start-num 2))
	  (begin
	    (set! begin-num (1+ begin-num))
	    ))

      (do ((ii begin-num (+ ii 2)))
	  ((or (> ii end-num)
	       (equal? break-flag #t)))
	(begin
	  (if (not (zero? (modulo ii 5)))
	      (begin
		(let ((dlist (split-digits-list ii)))
		  (let ((dlen (length dlist)))
		    (begin
		      (let ((this-list (possible-n-digits-list dlist min-nn)))
			(begin
			  (if (and (list? this-list)
				   (= (length this-list) min-nn))
			      (begin
				(let ((first-elem (car this-list)))
				  (begin
				    (if (or (<= smallest-elem 0)
					    (<= first-elem smallest-elem))
					(begin
					  (set! smallest-list this-list)
					  (set! smallest-count (length this-list))
					  (set! smallest-elem first-elem)
					  ))
				    ))
				))
			  ))
		      )))
		))
	  ))

      (if (not (= smallest-count min-nn))
	  (begin
	    (display (ice9-format:format #f "no sequence of ~:d found (for numbers between ~:d and ~:d)~%"
					 min-nn start-num end-num))
	    #f)
	  (begin
	    (display (ice9-format:format #f "~:d is the smallest prime which, by replacing part of the number with the same digit, is part of an ~:d-prime value family, (for generating numbers between ~:d and ~:d)~%" (list-ref smallest-list 0) min-nn start-num end-num))
	    (display (ice9-format:format #f "    count = ~:d : ~a~%"
					 smallest-count (list-to-set-string smallest-list)))
	    (force-output)
	    #t
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
    (display (format #f "Problem 051 - By replacing the 1st digit of *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.~%"))
    (newline)
    (display (format #f "By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.~%"))
    (newline)
    (display (format #f "Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.~%"))
    (newline)
    (display (format #f "The key idea for solving this problem was gotten from http://www.mathblog.dk/project-euler-51-eight-prime-family/~%"))
    (display (format #f "Hard-coding the possible permutations enabled the program to complete much faster than by standard recursion over all possible rearrangements of digits.~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-digit-list-to-number-1 counter)
	   (run-test test-replace-vector-with-same-digit-list-1 counter)
	   (run-test test-possible-3-digits-list-1 counter)
	   (run-test test-possible-4-digits-list-1 counter)
	   (run-test test-possible-5-digits-list-1 counter)
	   (run-test test-possible-6-digits-list-1 counter)
	   (run-test test-possible-n-digits-list-1 counter)
	   (run-test test-list-to-set-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((test-list-list
	   (list
	    (list 10 1000 7)
	    (list 10 1000 8)
	    ))
	  (continue-loop-flag #t))
      (let ((max-tests (length test-list-list)))
	(begin
	  (do ((ii 0 (1+ ii)))
	      ((>= ii max-tests))
	    (begin
	      (let ((a-list (list-ref test-list-list ii)))
		(let ((start-num (list-ref a-list 0))
		      (end-num (list-ref a-list 1))
		      (min-nn (list-ref a-list 2)))
		  (begin
		    (time-code
		     (begin
		       (main-loop start-num end-num min-nn)
		       ))

		    (newline)
		    (force-output)
		    )))
	      ))
	  )))

    (newline)
    ))
