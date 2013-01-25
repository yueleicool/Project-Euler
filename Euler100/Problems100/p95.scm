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

;;;### srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

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
(define (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
	(let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	      (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
	  (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
;;; form a list of the sum of proper divisors of nn
;;; 4 -> (list (list 2 1) (list 3 1) (list 4 3))
;;; 6 -> (list (list 2 1) (list 3 1) (list 4 3) (list 5 1) (list 6 6))
(define (populate-sum-of-divisors-hash! div-htable end-num)
  (let ((local-array (make-array 1 (1+ end-num))))
    (begin
      (hash-clear! div-htable)
      (array-set! local-array 0 1)
      (hash-set! div-htable 1 0)

      (do ((ii 2 (1+ ii)))
	  ((> ii end-num))
	(begin
	  (do ((jj (+ ii ii) (+ jj ii)))
	      ((> jj end-num))
	    (begin
	      (let ((jj-sum (array-ref local-array jj)))
		(begin
                  (let ((next-sum (+ ii jj-sum)))
                    (begin
                      (array-set! local-array next-sum jj)
                      ))
		  ))
	      ))

	  (let ((elem (array-ref local-array ii)))
	    (begin
	      (hash-set! div-htable ii elem)
              ))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-sum-of-divisors-hash-1)
  (let ((sub-name "test-populate-sum-of-divisors-hash-1")
	(test-list
	 (list
	  (list 1 (list (list 1 0)))
	  (list 2 (list (list 1 0) (list 2 1)))
	  (list 3 (list (list 1 0) (list 2 1) (list 3 1)))
	  (list 4 (list (list 1 0) (list 2 1) (list 3 1) (list 4 3)))
	  (list 5 (list (list 1 0) (list 2 1) (list 3 1) (list 4 3) (list 5 1)))
	  (list 20 (list (list 1 0) (list 2 1) (list 3 1) (list 4 3) (list 5 1)
			 (list 6 6) (list 7 1) (list 8 7) (list 9 4) (list 10 8)
			 (list 11 1) (list 12 16) (list 13 1) (list 14 10)
			 (list 15 9) (list 16 15) (list 17 1) (list 18 21)
			 (list 19 1) (list 20 22)))
	  (list 30 (list (list 1 0) (list 2 1) (list 3 1) (list 4 3) (list 5 1)
			 (list 6 6) (list 7 1) (list 8 7) (list 9 4) (list 10 8)
			 (list 11 1) (list 12 16) (list 13 1) (list 14 10)
			 (list 15 9) (list 16 15) (list 17 1) (list 18 21)
			 (list 19 1) (list 20 22) (list 21 11) (list 22 14)
			 (list 23 1) (list 24 36) (list 25 6) (list 26 16)
			 (list 27 13) (list 28 28) (list 29 1) (list 30 42)))
	  ))
	(result-htable (make-hash-table 100))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((max-num (list-ref alist 0))
		 (shouldbe-list-list (list-ref alist 1)))
	     (begin
	       (hash-clear! result-htable)
	       (populate-sum-of-divisors-hash! result-htable max-num)

	       (for-each
		(lambda (s-list)
		  (begin
		    (let ((skey (list-ref s-list 0))
			  (svalue (list-ref s-list 1)))
		      (let ((rvalue (hash-ref result-htable skey 0)))
			(begin
			  (if (not (equal? svalue rvalue))
			      (begin
				(display
                                 (format
                                  #f "~a : error (~a) : max num = ~a, key = ~a : "
                                  sub-name test-label-index max-num skey))
				(display
                                 (format
                                  #f "shouldbe = ~a, result = ~a~%"
                                  svalue rvalue))
				(quit)
				))
			  )))
		    )) shouldbe-list-list)
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (amicable-chain start-num max-iterations max-element
                        div-sum-htable seen-htable)
  (let ((chain-list (list start-num))
	(chain-length 1)
	(current-num start-num)
	(current-iteration 0)
	(continue-loop-flag #t))
    (begin
      (if (> start-num 1)
	  (begin
	    (while
                (and (equal? continue-loop-flag #t)
                     (<= current-iteration max-iterations))
              (begin
                (let ((next-num
                       (hash-ref div-sum-htable current-num 0)))
                  (begin
                    (cond
                     ((or (<= next-num 1)
                          (> next-num max-element)
                          (> chain-length max-iterations))
                      (begin
                        ;;; chain terminates
                        (set! continue-loop-flag #f)
                        (set! chain-list #f)
                        ))
                     ((= next-num start-num)
                      (begin
                        ;;; amicable chain found!
                        (set! continue-loop-flag #f)
                        (set! chain-list (cons next-num chain-list))
                        (set! chain-length (1+ chain-length))
                        ))
                     ((not (equal? (member next-num chain-list) #f))
                      (begin
                        ;;; chain forms inner cycle
                        (set! continue-loop-flag #f)
                        (set! chain-list #f)
                        ))
                     (else
                      (set! current-num next-num)
                      (set! chain-list (cons current-num chain-list))
                      (set! chain-length (1+ chain-length))
                      ))

                    (set! current-iteration (1+ current-iteration)))
                    )))
            ))

      (if (list? chain-list)
          (begin
            (for-each
             (lambda (anum)
               (begin
                 (hash-set! seen-htable anum #t)
                 )) chain-list)
            (reverse chain-list))
          (begin
            #f
            ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-amicable-chain-1)
  (let ((sub-name "test-amicable-chain-1")
	(test-list
	 (list
	  (list 2 #f) (list 3 #f) (list 4 #f) (list 5 #f)
	  (list 6 (list 6 6)) (list 7 #f) (list 8 #f) (list 9 #f)
	  (list 10 #f) (list 11 #f) (list 12 #f) (list 13 #f)
	  (list 14 #f) (list 15 #f) (list 16 #f) (list 17 #f)
	  (list 18 #f) (list 19 #f) (list 20 #f) (list 21 #f)
	  (list 22 #f) (list 23 #f) (list 24 #f) (list 25 #f)
	  (list 26 #f) (list 27 #f) (list 28 (list 28 28)) (list 29 #f)
	  (list 220 (list 220 284 220)) (list 284 (list 284 220 284))
	  (list 12496 (list 12496 14288 15472 14536 14264 12496))
	  ))
	(max-iterations 1000)
	(div-sum-htable (make-hash-table 100))
	(seen-htable (make-hash-table 100))
	(max-div-table-value 20000)
	(max-element 1000000)
	(test-label-index 0))
    (begin
      (populate-sum-of-divisors-hash! div-sum-htable max-div-table-value)

      (for-each
       (lambda (alist)
	 (begin
	   (let ((start-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result
                    (amicable-chain
                     start-num max-iterations max-element
                     div-sum-htable seen-htable)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : (~a) : error : start-num = ~a, shouldbe = ~a, result = ~a~%"
                         sub-name test-label-index start-num shouldbe result))
		       (quit)
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
	       (display
                (format
                 #f "elapsed time = ~a : ~a~%"
                 (julian-day-difference-to-string end-jday start-jday)
                 (date-time-to-string (srfi-19:current-date))))
	       (force-output)
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop start-num end-num max-iterations max-element status-num debug-flag)
  (let ((longest-chain-length 0)
	(longest-chain-list (list))
	(longest-chain-start 0)
	(continue-loop-flag #t)
	(counter 0)
	(div-sum-htable (make-hash-table max-element))
	(seen-htable (make-hash-table))
	(start-jday (srfi-19:current-julian-day)))
    (begin
      (time-code
       (begin
         (populate-sum-of-divisors-hash! div-sum-htable max-element)
         (gc)

         (display
          (ice9-format:format
           #f "completed populate-sum-of-divisors-hash! ~:d : "
           max-element))
         (force-output)
         ))

      (do ((ii start-num (1+ ii)))
	  ((or (> ii end-num)
	       (equal? continue-loop-flag #f)))
	(begin
          (if (equal? (hash-ref seen-htable ii #f) #f)
              (begin
                (let ((amicable-list
                       (amicable-chain
                        ii max-iterations max-element
                        div-sum-htable seen-htable)))
                  (begin
                    (if (list? amicable-list)
                        (begin
                          (let ((alen (length amicable-list)))
                            (if (> alen longest-chain-length)
                                (begin
                                  (if (equal? debug-flag #t)
                                      (begin
                                        (display
                                         (ice9-format:format
                                          #f "  ~:d : chain = ~a : length = ~:d : "
                                          ii amicable-list alen))
                                        (display
                                         (ice9-format:format
                                          #f "longest chain = ~a : length = ~:d~%"
                                          longest-chain-list longest-chain-length))
                                        (force-output)
                                        ))

                                  (set! longest-chain-length alen)
                                  (set! longest-chain-list amicable-list)
                                  (set! longest-chain-start ii)
                                  )))
                          ))
                    ))
                ))

	  (set! counter (1+ counter))
	  (if (zero? (modulo counter status-num))
	      (begin
		(let ((end-jday (srfi-19:current-julian-day)))
		  (begin
		    (display
                     (ice9-format:format
                      #f "(~:d / ~:d) : longest so far : ~:d : chain = ~a : "
                      counter end-num longest-chain-start longest-chain-list))
		    (display
                     (ice9-format:format
                      #f "length = ~:d : elapsed time ~a : ~a~%"
                      longest-chain-length
                      (julian-day-difference-to-string end-jday start-jday)
                      (date-time-to-string (srfi-19:current-date))))
		    (force-output)
		    (set! start-jday end-jday)
		    ))
		))
	  ))

      (newline)
      (display
       (ice9-format:format
        #f "for numbers between ~:d and ~:d, maximum iterations = ~:d, "
        start-num end-num max-iterations))
      (display
       (ice9-format:format
        #f "with max-element < ~:d~%" max-element))
      (display
       (ice9-format:format
        #f "longest amicable chain starts at ~:d : chain list = ~a : length = ~:d~%"
        longest-chain-start longest-chain-list longest-chain-length))

      (display
       (ice9-format:format
        #f "smallest member of the longest chain = ~:d (where no element exceeds ~:d)~%"
        (srfi-1:fold min (car longest-chain-list) longest-chain-list)
        max-element))
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
(define (main args)
  (begin
    (display (format #f "Project Euler 95 - The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we call it a perfect number.~%"))
    (newline)
    (display (format #f "Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.~%"))
    (newline)
    (display (format #f "Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:~%"))
    (newline)
    (display (format #f "  12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)~%"))
    (newline)
    (display (format #f "Since this chain returns to its starting point, it is called an amicable chain.~%"))
    (newline)
    (display (format #f "Find the smallest member of the longest amicable chain with no element exceeding one million.~%"))
    (newline)
    (display (format #f "This solution uses the sieve of Eratosthenes to quickly find the sum of the proper divisors for each number up to a million, http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-populate-sum-of-divisors-hash-1 counter)
	   (run-test test-amicable-chain-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((start-num 1)
	  (end-num 1000)
	  (max-iterations 1000)
	  (max-element 100000)
	  (status-num 10000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num max-iterations max-element
		      status-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((start-num 1)
	  (end-num 1000000)
	  (max-iterations 1000)
	  (max-element 1000000)
	  (status-num 5000000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop start-num end-num max-iterations max-element
		      status-num debug-flag)
	   ))
	))

    (newline)
    ))
