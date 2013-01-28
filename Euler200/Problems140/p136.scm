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
(define (julian-day-difference-to-string dend dstart)
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
(define (current-date-time-string)
  (let ((this-datetime (srfi-19:current-date)))
    (let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	  (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (make-odd-prime-array max-num)
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
          (let ((this-num (array-ref intermediate-array ii)))
            (begin
              (if (= this-num ii)
                  (begin
                    (set! result-list (cons ii result-list))

                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj max-num))
		      (begin
			(array-set! intermediate-array -1 jj)
			))
                    ))
	      ))
	  ))

      (let ((rlist (reverse result-list)))
	(let ((olist (cdr rlist)))
	  (begin
	    (list->array 1 olist)
	    )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-odd-prime-array-1)
  (let ((sub-name "test-make-odd-prime-array-1")
	(test-list
	 (list
	  (list 2 (list)) (list 3 (list 3)) (list 4 (list 3))
	  (list 5 (list 3 5)) (list 6 (list 3 5))
	  (list 7 (list 3 5 7)) (list 8 (list 3 5 7))
	  (list 9 (list 3 5 7)) (list 10 (list 3 5 7))
	  (list 11 (list 3 5 7 11))
	  (list 13 (list 3 5 7 11 13))
	  (list 17 (list 3 5 7 11 13 17))
	  (list 19 (list 3 5 7 11 13 17 19))
	  (list 23 (list 3 5 7 11 13 17 19 23))
	  (list 31 (list 3 5 7 11 13 17 19 23 29 31))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-array (make-odd-prime-array test-num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (car (array-dimensions result-array))))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display
                          (format
                           #f "~a : error (~a) : num=~a, shouldbe=~a : "
                           sub-name test-label-index test-num shouldbe-list))
			 (display
                          (format
                           #f "lengths not equal, shouldbe=~a, result=~a~%"
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
                                   #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a : "
                                   sub-name test-label-index test-num
                                   shouldbe-list result))
				 (display
                                  (format
                                   #f "discrepancy at ii=~a, shouldbe=~a, result=~a~%"
                                   ii s-elem r-elem))
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
;;; u=3d-z, v=d+z
;;; d=(u+v)/4 and z=(3v-u)/4
(define (transform-uu-vv-to-xyzd uu vv)
  (let ((result-list (list))
	(dtmp1 (+ uu vv))
	(ztmp1 (- (* 3 vv) uu)))
    (let ((dtmp2 (euclidean/ dtmp1 4))
	  (ztmp2 (euclidean/ ztmp1 4)))
      (begin
	(if (and (= (* 4 dtmp2) dtmp1)
		 (= (* 4 ztmp2) ztmp1)
		 (> ztmp2 0))
	    (begin
	      (let ((xx (+ ztmp2 dtmp2 dtmp2))
		    (yy (+ ztmp2 dtmp2))
		    (zz ztmp2))
		(begin
		  (set! result-list (list xx yy zz dtmp2))
		  ))
	      ))

	result-list
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-transform-uu-vv-to-xyzd-1)
  (let ((sub-name "test-transform-uu-vv-to-xyzd-1")
	(test-list
	 (list
	  (list 1 1 (list))
	  (list 3 9 (list 12 9 6 3))
	  (list 1 27 (list 34 27 20 7))
	  (list 4 8 (list 11 8 5 3))
	  (list 1 39 (list 49 39 29 10))
	  (list 1 10 (list))
	  (list 2 5 (list))
	  ))
	(test-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((uu (list-ref this-list 0))
		 (vv (list-ref this-list 1))
		 (shouldbe-list-list (list-ref this-list 2)))
	     (let ((result-list-list (transform-uu-vv-to-xyzd uu vv)))
	       (let ((slen (length shouldbe-list-list))
		     (rlen (length result-list-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : uu=~a, vv=~a, shouldbe=~a, result=~a, length discrepancy, shouldbe=~a, result=~a~%"
					  sub-name test-index uu vv
					  shouldbe-list-list result-list-list
					  slen rlen))
			 (quit)
			 ))

		   (for-each
		    (lambda (slist)
		      (begin
			(if (equal? (member slist result-list-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : uu=~a, vv=~a, shouldbe=~a, result=~a, missing list ~a~%"
					       sub-name test-index uu vv
					       shouldbe-list-list result-list-list
					       slist))
			      (quit)
			      ))
			)) shouldbe-list-list)

		   (set! test-index (+ test-index 1))
		   ))
	       ))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax debug-display
  (syntax-rules ()
    ((debug-display uu vv nn)
     (begin
       (let ((rlist (transform-uu-vv-to-xyzd uu vv)))
	 (let ((xx (list-ref rlist 0))
	       (yy (list-ref rlist 1))
	       (zz (list-ref rlist 2))
	       (dd (list-ref rlist 3)))
	   (begin
	     (display (ice9-format:format #f "    (~:d, ~:d)  ~:d^2 - ~:d^2 - ~:d^2 = ~:d : delta = ~:d~%"
					  uu vv xx yy zz nn dd))
	     (force-output)
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; count primes that are p = 3 mod 4, that are less than or equal to n
;;; sieve of eratosthenes method
(define (count-case-1-primes max-num)
  (let ((intermediate-array (make-array 0 (1+ max-num)))
	(results-count 0))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((> ii max-num))
	(begin
	  (array-set! intermediate-array ii ii)
	  ))

      (do ((ii 2 (1+ ii)))
	  ((> ii max-num))
	(begin
          (let ((this-num (array-ref intermediate-array ii)))
            (begin
              (if (= this-num ii)
                  (begin
                    (if (= (modulo this-num 4) 3)
                        (begin
                          (set! results-count (1+ results-count))
                          ))
                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj max-num))
                      (begin
			(array-set! intermediate-array -1 jj)
			))
                    ))
	      ))
	  ))

      results-count
      )))

;;;#############################################################
;;;#############################################################
(define (test-count-case-1-primes-1)
  (let ((sub-name "test-count-case-1-primes-1")
	(test-list
	 (list
	  (list 5 1) (list 7 2) (list 11 3)
	  (list 12 3) (list 19 4) (list 20 4)
	  (list 23 5) (list 31 6) (list 43 7)
	  ))
	(test-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((max-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (count-case-1-primes max-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : max-num=~a, shouldbe=~a, result=~a~%"
					sub-name test-index max-num
					shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-index (+ test-index 1))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax count-case-4-solutions
  (syntax-rules ()
    ((count-case-4-solutions max-num results-count debug-flag)
     (begin
       (let ((max-4-mm (euclidean/ max-num 4)))
	 (let ((odd-prime-array (make-odd-prime-array max-4-mm)))
	   (begin
             ;;; add in special case n=4
	     (set! results-count (1+ results-count))

             ;;; now take care of everything else
	     (let ((count (car (array-dimensions odd-prime-array))))
	       (begin
		 (set! results-count (+ results-count count))
		 ))
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax count-case-6-solutions
  (syntax-rules ()
    ((count-case-6-solutions max-num results-count debug-flag)
     (begin
       (let ((max-16-mm (euclidean/ max-num 16)))
	 (let ((odd-prime-array (make-odd-prime-array max-16-mm)))
	   (begin
             ;;; add in special case n=16
	     (set! results-count (1+ results-count))

             ;;; now take care of everything else
	     (let ((count (car (array-dimensions odd-prime-array))))
	       (begin
		 (set! results-count (+ results-count count))
		 ))
	     )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((results-count 0)
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (let ((c1-count (count-case-1-primes max-num)))
	(begin
	  (set! results-count (+ results-count c1-count))
	  ))

      (gc)
      (let ((end-jday (srfi-19:current-julian-day)))
	(begin
	  (display
           (ice9-format:format
            #f "completed case 1 : counts = ~:d : elapsed time = ~a : ~a~%"
            results-count
            (julian-day-difference-to-string end-jday start-jday)
            (current-date-time-string)))
	  (force-output)
	  (set! start-jday end-jday)
	  ))

      (count-case-4-solutions max-num results-count debug-flag)
      (gc)
      (let ((end-jday (srfi-19:current-julian-day)))
	(begin
	  (display
           (ice9-format:format
            #f "completed case 4 : counts = ~:d : elapsed time = ~a : ~a~%"
            results-count
            (julian-day-difference-to-string end-jday start-jday)
            (current-date-time-string)))
	  (force-output)
	  (set! start-jday end-jday)
	  ))

      (count-case-6-solutions max-num results-count debug-flag)
      (gc)
      (let ((end-jday (srfi-19:current-julian-day)))
	(begin
	  (display
           (ice9-format:format
            #f "completed case 6 : counts = ~:d : elapsed time = ~a : ~a~%"
            results-count
            (julian-day-difference-to-string end-jday start-jday)
            (current-date-time-string)))
	  (force-output)
	  (set! start-jday end-jday)
	  ))

      (display
       (ice9-format:format
        #f "Found ~:d values of n with exactly 1 distinct solutions (less than ~:d).~%"
        results-count max-num))
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
				(current-date-time-string)))
	       (force-output)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main args)
  (begin
    (display (format #f "Project Euler 136 - The positive integers, x, y, and z, are consecutive terms of an arithmetic progression. Given that n is a positive integer, the equation, x^2 - y^2 - z^2 = n, has exactly one solution when n = 20:~%"))
    (newline)
    (display (format #f "  13^2 - 10^2 - 7^2 = 20~%"))
    (newline)
    (display (format #f "In fact there are twenty-five values of n below one hundred for which the equation has a unique solution.~%"))
    (newline)
    (display (format #f "How many values of n less than fifty million have exactly one solution?~%"))
    (newline)
    (display (format #f "The solution was found at http://d.hatena.ne.jp/inamori/20100726/p1~%"))
    (newline)
    (display (format #f "x^2 - y^2 - z^2 = n, where x, y, and z form an arithmetic progression, which means y=z+d, x=y+d=z+2d.~%"))
    (newline)
    (display (format #f "x^2 - y^2 - z^2 = (z + 2d)^2 - (z + d)^2 - z^2 = 3d^2 + 2dz - z^2 = n > 0.~%"))
    (newline)
    (display (format #f "Make a change of variables, u=3d-z, v=d+z, then 3d^2 + 2dz - z^2 = n gets transformed to u*v=n.  Reversing the change of variables yields d=(u+v)/4 and z=(3v-u)/4, and d and z are positive integers.~%"))
    (newline)
    (display (format #f "n = u*v, where d=(u+v)/4, z=(3v-u)/4~%"))
    (newline)
    (display (format #f "For example, when n=27, the divisors of 27 are 1 and 27, and 3 and 9.  For (1, 27), z=20 and d=7, and for (3, 9), z=6 and d=3.  Also when n=10, the divisors of 10 are (1, 10) and (2, 5).  For (1, 10), z=29/4 and d=11/4, and for (2, 5), z=13/4 and d=7/4. When n=u*v=5*7=35, and d=(u+v)/4=(5+7)/4=12/4=3, and z=(3v-u)/4=(21-5)/4=16/4=4.~%"))
    (newline)
    (display (format #f "Consider d=(u+v)/4 and z=(3v-u)/4, both d>0, z>0.  This means that (u+v) and (3v-u) a multiple of 4.  Following http://d.hatena.ne.jp/inamori/20100726/p1, we see that single solution values of n breaks down into simple cases.  Consider n=(2^exp)*M, where M contains no factors of 2.~%"))
    (newline)
    (display (format #f "case (1) exp=0, M is an odd prime, n=p<max-num. If M is an odd prime p, then there is only one possible solution u=1 v=p, where (p+1)/4 an integer and (3p-1)/4 an integer when p=3 mod 4, or p=4k+3 for some integer k. (u+v)/4 = (4k+3+1)/4 = (4k+4)/4 an integer.  Second condition: (3v-u)/4 = (3*(4k+3)-1)/4 = (12k+9-1)/4 = (12k+4)/4 an integer.  Note that v=p and u=1 means that z<=0, so there is only one valid solution when (u=1, v=p), p a prime such that p=3 mod 4.  For example (u=1, v=3) : 4^2 - 3^2 - 2^2 = 3~%"))
    (newline)
    (display (format #f "case (2) exp=0, M odd integer, M=qr, q and r are odd numbers. (u=1, v=qr) and (u=q, v=r) have either zero, two, or three solutions that satisfy (u+v)/4 and (3v-u)/4 an integer. Let q=2j+1, r=2k+1, with q<r and j<k, and consider the case (u=1, v=qr) : (u+v)/4 = (1+ (2j+1)*(2k+1))/4 = (4jk+2(j+k)+2)/4 = (2jk+j+k+1)/2.  Since q<r, then j<k and let k=j+a, so (2jk+j+k+1)/2 = (2jk+2j+a+1)/2 is an integer when a is odd, so no solutions when a is even. For the second condition of (u=1, v=qr) : (3v-u)/4 = (3(2j+1)*(2k+1)-1)/4 = (3(4jk+2(j+k)+1)-1)/4 = (12jk+6(j+k)+2)/4 = (12jk+6(2j+a)+2)/4 = (12jk+12j+2(3a+1))/4 is a solution when a is odd (set a=2b+1, then the term (3v-u)/4 is an integer). For example, (u=1, v=35=5*7) : then q=5=2j+1=2*2+1, r=7=2k+1=2*3+1, and k=3=j+a=2+1, where a=1 is odd, and the solution is 44^2 - 35^2 - 26^2 = 35.  There are no solutions when (u=1, v=45=5*9) where q=5=2j+1=2*2+1, r=9=2k+1=2*4+1 and a=2 is even. The second case also has a solution (u=q, v=r) : (u+v)/4 = (2j+1+2k+1)/4 = (2(j+k)+2)/4 if k=j+a then (u+v)/4 = (2(2j+a)+2)/4 = (2j+a+1)/2 a solution is possible when a is odd. Second condition of (u=q, v=r) : (3v-u)/4 = (3(2k+1)-(2j+1))/4 = (6k-2j+2)/4 = (6*(j+a)-2j+2)/4 = (4j+2(3a+1)/4, and we have a solution when a is odd, no solution when a is even.  The third case (u=r, v=q) is similar, only the second conditions is different, (u=r, v=q) : (3v-u)/4 = (3(2j+1)-(2k+1))/4 = (3(2j+1)-(2*(j+a)+1))/4 = (4j-2a+2)/4, .  For example when (u=5, v=7) : a=1 odd, 10^2 - 7^2 - 4^2 = 35, and for (u=7, v=5) : a=1 odd, 8^2 - 5^2 - 2^2 = 35. There are no solutions when (u=5, v=9) or (u=9, v=5), since a is even.  In summary, in the cases where M=qr, q<r, q=2j+1, r=2k+1, k=j+a, and a odd, we have solutions when (u=1, v=qr), (u=q, v=r), and (u=r, v=q).  When a is even, no solutions.  So we either have no solutions or too many solutions.~%"))
    (newline)
    (display (format #f "case (3) exp=1, n=2*M, M odd, this will never produce a solution since (2+M)/4 will never be an integer.~%"))
    (newline)
    (display (format #f "case (4) exp=2, n=4*M, M an odd number, 1<=M<max-num/4, there is no possible solution when (u=1, v=4M) : since the condition (u+v)/4 = (1+4M)/4 is never an integer.  When (u=2, v=2M), then (u+v)/4 = (2+2M)/4 and M=2k+1, so (u+v)/4 = (2+4k+2)/4 = (k+1) a possible solution.  For the second condition (u=2, v=2M), (3v-u)/4 = (6M-2)/4 = (6(2k+1)-2)/4 = (12k+4)/4 a solution.  When (u=2M, v=2), the first condition is statisfied, and the second is (3v-u)/4 = (6 - 2M)/4 a positive integer only for M=1, or n=4.  However, when M=1, then u=2, v=2, the same as in the previous case of (u=2, v=2M), so n=4 has just one solution, and the rest n=4*M has one solution when (u=2, v=2M), 1<=M<max-num/4, M an odd integer.  When (u=4, v=M), then (u+v)/4 = (4+M)/4 can never be a solution.  By the same reasoning as in case (2), we see that M must be an odd prime or 1.~%"))
    (newline)
    (display (format #f "case (5) exp=3, n=8*M, M odd prime, M<max-num/8, there are no solutions when (u=1, v=8M), (u=8M, v=1), (u=2, v=4M), (u=4M, v=2), (u=4, v=2M), (u=2M, v=4), (u=8, v=M), (u=M, v=8), since M is an odd integer and those cases do not satisfy condition 1 (u+v)/4 a positive integer~%"))
    (newline)
    (display (format #f "case (6) exp=4, n=16*M, M odd prime (similar reasoning to case 2), 1<=M<max-num/16, only solutions where (u=4, v=4M) : condition 1 is (u+v)/4 = (4+4M)/4 an integer, and (3v-u)/4 = (12M-4)/4 an integer.  The other possibilities fail condition 1 (u+v)/4 an integer.~%"))
    (newline)
    (display (format #f "case (7) exp=e > 4, n=(2^e)*M, M odd prime, then an arbitrary possbility is (u=2^g, v=(2^(e-g))*M) : condition 1 is (u+v)/4 = (2^g - 2^(e-g)*M)/4 is a solution if 1<g<e-1, no solution if g=0, g=1, g=e-1, or g=e. condition 2 is (3v-u)/4 = (3*2^(e-g)*M - 2^g)/4 also a solution when 1<g<e-1. However, (u=2^(e-g)*M, v=2^g) is also a solution by the same reasoning.  So for all case 7 solutions there are zero, 2 or more solutions, no single solutions.~%"))
    (newline)
    (display (format #f "This program takes about 30 minutes to run, so it was re-written in c++ and completed in 2 seconds using the same algorithm.  This method is probably ok.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-odd-prime-array-1 counter)
	   (run-test test-transform-uu-vv-to-xyzd-1 counter)
	   (run-test test-count-case-1-primes-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 100)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-num 50000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
