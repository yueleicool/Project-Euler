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
  (begin
    (let ((result-list (local-loop this-num (list))))
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
(define (list-to-number digit-list)
  (let ((result-num (srfi-1:fold
		     (lambda (this-digit prev)
		       (+ this-digit (* 10 prev)))
		     0 digit-list)))
    (begin
      result-num
      )))

;;;#############################################################
;;;#############################################################
(define (test-list-to-number-1)
  (let ((sub-name "test-list-to-number-1")
	(test-list
	 (list
	  (list (list 3) 3) (list (list 4) 4) (list (list 5) 5)
	  (list (list 1 3) 13) (list (list 1 4) 14) (list (list 1 5) 15)
	  (list (list 2 3) 23) (list (list 2 4) 24) (list (list 2 5) 25)
	  (list (list 2 0) 20) (list (list 3 0) 30) (list (list 9 9) 99)
	  (list (list 1 2 3) 123) (list (list 1 2 3 4) 1234)
	  (list (list 9 8 7 6 5) 98765)
	  (list (list 3 4 1 6 0 8 9 8 7) 341608987)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((digit-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (list-to-number digit-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : digit list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index digit-list
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
(define (make-prime-permutation-list input-number)
  (define (local-loop digit-list dlength current-list clength acc-list)
    (if (>= clength dlength)
	(begin
	  (let ((sc-list (sort current-list <)))
	    (begin
	      (if (and
		   (not (zero? (car current-list)))
		   (equal? sc-list digit-list))
		  (begin
		    (let ((this-num (list-to-number current-list)))
		      (begin
			(if (prime? this-num)
			    (begin
			      (cons this-num acc-list))
			    (begin
			      acc-list
			      ))
		    )))
		  (begin
		    acc-list
		    ))
	      )))
	(begin
	  (for-each
	   (lambda (tmp-digit)
	     (begin
	       (let ((next-current-list (cons tmp-digit current-list)))
		 (let ((next-clength (length next-current-list)))
		   (begin
		     (let ((next-acc-list
			    (local-loop digit-list dlength
					next-current-list next-clength
					acc-list)))
		       (begin
			 (set! acc-list next-acc-list)
			 ))
		     )))
	       )) digit-list)

	  acc-list
	  )))
  (let ((dlist (sort (split-digits-list input-number) <)))
    (let ((dlen (length dlist)))
      (begin
	(let ((pperm-list (local-loop dlist dlen (list) 0 (list))))
	  (begin
	    pperm-list
	    )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-permutation-list-1)
  (let ((sub-name "test-make-prime-permutation-list-1")
	(test-list
	 (list
	  (list 3 (list 3)) (list 4 (list)) (list 5 (list 5))
	  (list 13 (list 13 31)) (list 14 (list 41)) (list 15 (list))
	  (list 23 (list 23)) (list 24 (list)) (list 25 (list))
	  (list 173 (list 173 137 317))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (make-prime-permutation-list num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : num = ~a, shouldbe = ~a, result = ~a, length shouldbe=~a, result=~a~%"
					  sub-name test-label-index num
					  shouldbe-list result-list slen rlen))
			 (quit)
			 ))

		   (for-each
		    (lambda (s-list)
		      (begin
			(if (equal? (member s-list result-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : num = ~a, shouldbe = ~a, result = ~a, differs at element ~a~%"
					       sub-name test-label-index num
					       shouldbe result s-list))
			      (quit)
			      ))
			)) shouldbe-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; example list (list (list 1487 4817) (list 4817 8147))
;;; so 1487 and 8147 are the outside nodes, and 4817 is the internal node
(define (is-list-list-connected? input-list-list)
  (let ((llen (length input-list-list))
	(result-flag #f))
    (let ((num-count-htable (make-hash-table llen)))
      (begin
	(for-each
	 (lambda (a-list)
	   (begin
	     (let ((a1 (list-ref a-list 0))
		   (a2 (list-ref a-list 1)))
	       (begin
		 (let ((count1 (hash-ref num-count-htable a1 0))
		       (count2 (hash-ref num-count-htable a2 0)))
		   (begin
		     (hash-set! num-count-htable a1 (+ count1 1))
		     (hash-set! num-count-htable a2 (+ count2 1))
		     ))
		 ))
	     )) input-list-list)

	(let ((num-1 0)
	      (num-2 0)
	      (num-other 0)
	      (total 0)
	      (ok-flag #f))
	  (begin
	    (hash-for-each
	     (lambda (num count)
	       (begin
		 (set! total (1+ total))
		 (cond
		  ((= count 1)
		   (set! num-1 (1+ num-1)))
		  ((= count 2)
		   (set! num-2 (1+ num-2)))
		  (else
		   (set! num-other (1+ num-other))
		   ))
		 )) num-count-htable)

	    (if (and (= num-1 2)
		     (= total (+ num-1 num-2)))
		(begin
		  (set! ok-flag #t)
		  ))
	    ok-flag
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-is-list-list-connected-1)
  (let ((sub-name "test-is-list-list-connected-1")
	(test-list
	 (list
	  (list (list (list 1 3) (list 3 4)) #t)
	  (list (list (list 1 2) (list 2 3) (list 3 4)) #t)
	  (list (list (list 1 2) (list 4 9) (list 3 4)) #f)
	  (list (list (list 1 2) (list 3 4) (list 5 6)
		      (list 7 8) (list 9 10)) #f)
	  (list (list (list 1 2) (list 3 4) (list 5 6)
		      (list 7 8) (list 9 10) (list 2 3)) #f)
	  (list (list (list 1 2) (list 3 4) (list 5 6)
		      (list 7 8) (list 9 10) (list 2 3)
		      (list 4 5)) #f)
	  (list (list (list 1 2) (list 3 4) (list 5 6)
		      (list 7 8) (list 9 10) (list 2 3)
		      (list 4 5) (list 6 7) (list 8 9)) #t)
	  (list (list (list 1309 3091) (list 9031 3091)) #t)
	  (list (list (list 3109 9103) (list 3019 9013)) #f)
	  (list (list (list 9013 9103) (list 3019 3109)) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((llist (list-ref alist 0))
		 (shouldbe-bool (list-ref alist 1)))
	     (let ((result-bool (is-list-list-connected? llist)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display (format #f "~a : error (~a) : list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index llist
					(if (equal? shouldbe-bool #t) "true" "false")
					(if (equal? result-bool #t) "true" "false")))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (make-constant-increasing-list perm-list)
  (let ((sp-list (sort perm-list <))
	(sp-len (length perm-list)))
    (let ((sp-len-m1 (- sp-len 1))
	  (diff-htable (make-hash-table sp-len))
	  (result-diff -1)
	  (result-len -1)
	  (result-list (list (list))))
      (begin
	(do ((ii 0 (1+ ii)))
	    ((>= ii sp-len-m1))
	  (begin
            ;;; find the difference
	    (let ((first-num (list-ref sp-list ii)))
	      (begin
		(do ((jj (+ ii 1) (1+ jj)))
		    ((>= jj sp-len))
		  (begin
		    (let ((second-num (list-ref sp-list jj)))
		      (let ((this-diff (- second-num first-num)))
			(let ((saved-list (hash-ref diff-htable this-diff (list)))
			      (next-list (list first-num second-num)))
			  (begin
			    (if (and (> this-diff 0)
				     (equal? (member next-list saved-list) #f))
				(begin
				  (set! saved-list (cons (list first-num second-num) saved-list))
				  (hash-set! diff-htable this-diff saved-list)
				  ))
			    ))
			))
		    ))
		))
	    ))

	(hash-for-each
	 (lambda (diff llist)
	   (begin
	     (let ((llen (length llist)))
	       (begin
		 (if (and
		      (> llen 1)
		      (is-list-list-connected? llist))
		     (begin
		       (if (<= result-len 0)
			   (begin
			     (set! result-diff diff)
			     (set! result-len llen)
			     (set! result-list (list llist)))
			   (begin
			     (if (>= llen result-len)
				 (begin
				   (set! result-diff diff)
				   (set! result-len llen)
				   (set! result-list (cons llist result-list))
				   ))
			     ))
		       ))
		 ))
	     )) diff-htable)

	(list result-diff result-list)
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-make-constant-increasing-list-1)
  (let ((sub-name "test-make-constant-increasing-list-1")
	(test-list
	 (list
	  (list 173 (list 0 (list (list))))
	  (list 1487 (list 3330
			   (list (list (list 1487 4817)
				       (list 4817 8147)))))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((perm-list (make-prime-permutation-list num)))
	       (let ((result-list (make-constant-increasing-list perm-list)))
		 (let ((s-diff (list-ref shouldbe-list 0))
		       (s-list (car (list-ref shouldbe-list 1)))
		       (r-diff (list-ref result-list 0))
		       (r-list (car (list-ref result-list 1))))
		   (let ((slen (length s-list))
			 (rlen (length r-list)))
		     (begin
		       (if (not (equal? slen rlen))
			   (begin
			     (display (format #f "~a : error (~a) : num = ~a, shouldbe = ~a, result = ~a, length shouldbe=~a, result=~a~%"
					      sub-name test-label-index num
					      shouldbe-list result-list slen rlen))
			     (quit)
			     ))

		       (for-each
			(lambda (s-elem)
			  (begin
			    (if (equal? (member s-elem r-list) #f)
				(begin
				  (display (format #f "~a : error (~a) : num = ~a, shouldbe = ~a, result = ~a, differs at element ~a~%"
						   sub-name test-label-index num
						   shouldbe-list result-list s-elem))
				  (quit)
				  ))
			    )) s-list)
		       ))
		   ))))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (connected-list-list-to-sequence c-list-list)
  (let ((result-list (list)))
    (begin
      (for-each
       (lambda (c-list)
	 (let ((c1 (list-ref c-list 0))
	       (c2 (list-ref c-list 1)))
	   (begin
	     (if (equal? (member c1 result-list) #f)
		 (begin
		   (set! result-list (cons c1 result-list))
		   ))
	     (if (equal? (member c2 result-list) #f)
		 (begin
		   (set! result-list (cons c2 result-list))
		   ))
	     ))) c-list-list)

      (sort result-list <)
      )))

;;;#############################################################
;;;#############################################################
(define (test-connected-list-list-to-sequence-1)
  (let ((sub-name "test-connected-list-list-to-sequence-1")
	(test-list
	 (list
	  (list (list (list 1 2) (list 2 3) (list 3 4))
		(list 1 2 3 4))
	  (list (list (list 1487 4817)
		      (list 4817 8147))
		(list 1487 4817 8147))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((llist (list-ref alist 0))
		 (shouldbe-list (list-ref alist 1)))
	     (let ((result-list (connected-list-list-to-sequence llist)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : llist = ~a, shouldbe = ~a, result = ~a, length shouldbe=~a, result=~a~%"
					  sub-name test-label-index llist
					  shouldbe-list result-list slen rlen))
			 (quit)
			 ))

		   (for-each
		    (lambda (s-elem)
		      (begin
			(if (equal? (member s-elem result-list) #f)
			    (begin
			      (display (format #f "~a : error (~a) : llist = ~a, shouldbe = ~a, result = ~a, differs at element ~a~%"
					       sub-name test-label-index llist
					       shouldbe-list result-list s-elem))
			      (quit)
			      ))
			)) shouldbe-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (concatenate-numbers num-list)
  (let ((all-list (list)))
    (begin
      (for-each
       (lambda (elem)
	 (let ((dlist (split-digits-list elem)))
	   (begin
	     (set! all-list (append all-list dlist))
	     ))) num-list)

      (let ((conc-num
	     (srfi-1:fold
	      (lambda (num prev)
		(+ num (* 10 prev))) 0 all-list)))
	(begin
	  conc-num
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-concatenate-numbers-1)
  (let ((sub-name "test-concatenate-numbers-1")
	(test-list
	 (list
	  (list (list 33 44 55) 334455)
	  (list (list 12 34 56) 123456)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((num-list (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (concatenate-numbers num-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : num-list = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index num-list
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
(define (list-to-sequence-string llist)
  (let ((this-string
	 (string-join
	  (map
	   (lambda (this-elem)
	     (ice9-format:format #f "~:d" this-elem))
	   llist)
	  ", ")))
    (string-append "{ " this-string " }")
    ))

;;;#############################################################
;;;#############################################################
(define (test-list-to-sequence-string-1)
  (let ((sub-name "test-list-to-sequence-string-1")
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
	     (let ((result (list-to-sequence-string test-list)))
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
(define (display-results ii sequence cdiff)
  (let ((con-num (concatenate-numbers sequence))
	(seq-string (list-to-sequence-string sequence)))
    (begin
      (display (ice9-format:format
		#f "(~:d)  arithmetic sequence ~a : constant difference = ~:d : 12-digit number = ~:d~%"
		ii seq-string cdiff con-num))

      (for-each
       (lambda (elem)
	 (begin
	   (let ((pflag (prime? elem)))
	     (begin
	       (display (ice9-format:format #f "  prime?(~:d) = ~a~%"
					    elem (if (equal? pflag #t) "true" "false")))
	       )))) sequence)
      (newline)
      (force-output)
      )))

;;;#############################################################
;;;#############################################################
(define (main-secondary-loop ii seq-seen-htable prime-seen-htable)
  (let ((perm-list (make-prime-permutation-list ii))
	(num-results 0))
    (let ((cseq-list (make-constant-increasing-list perm-list)))
      (let ((c-diff (list-ref cseq-list 0))
	    (c-list (list-ref cseq-list 1)))
	(let ((c-len (length c-list)))
	  (begin
	    (for-each
	     (lambda (this-prime)
	       (begin
		 (hash-set! prime-seen-htable this-prime #t)
		 )) perm-list)

	    (if (and (list? c-list) (> c-len 0)
		     (> c-diff 0))
		(begin
		  (for-each
		   (lambda (c2-list)
		     (begin
		       (let ((sequence
			       (connected-list-list-to-sequence c2-list)))
			 (begin
			   (if (and (list? sequence) (> (length sequence) 0))
			       (begin
				 (let ((seen-flag (hash-ref seq-seen-htable sequence #f)))
				   (begin
				     (if (equal? seen-flag #f)
					 (begin
					   (set! num-results (1+ num-results))
					   (display-results ii sequence c-diff)
					   (hash-set! seq-seen-htable sequence #t)
					   ))
				     ))
				 ))
			   ))
		       )) c-list)
		  ))
	    num-results
	    ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num end-num)
  (let ((num-results 0)
	(seq-seen-htable (make-hash-table 100))
	(prime-seen-htable (make-hash-table 100)))
    (begin
      (do ((ii start-num (1+ ii)))
	  ((> ii end-num))
	(begin
	  (let ((pflag (hash-ref prime-seen-htable ii)))
	    (begin
	      (if (equal? pflag #f)
		  (begin
		    (let ((this-num (main-secondary-loop
				     ii seq-seen-htable prime-seen-htable)))
		      (begin
			(set! num-results (+ num-results this-num))
			))
		    ))
	      ))
	  ))

      (display (ice9-format:format #f "total number of results found = ~:d, (between ~:d and ~:d)~%"
				   num-results start-num end-num))
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
    (display (format #f "Problem 049 - The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.~%"))
    (newline)
    (display (format #f "There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.~%"))
    (newline)
    (display (format #f "What 12-digit number do you form by concatenating the three terms in this sequence?~%"))
    (newline)


    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-prime-1 counter)
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-list-to-number-1 counter)
	   (run-test test-make-prime-permutation-list-1 counter)
	   (run-test test-is-list-list-connected-1 counter)
	   (run-test test-make-constant-increasing-list-1 counter)
	   (run-test test-connected-list-list-to-sequence-1 counter)
	   (run-test test-concatenate-numbers-1 counter)
	   (run-test test-list-to-sequence-string-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 1000)
	  (end-num 10000))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num)
	   ))
	))

    (newline)
    ))
