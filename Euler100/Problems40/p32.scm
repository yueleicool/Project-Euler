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
(define (split-digits-list this-num)
  (define (local-loop this-num acc-list)
    (begin
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
        ))
      ))
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
					sub-name test-num test-label-index
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
(define (is-triple-pandigital? multiplicand multiplier product)
  (let ((m1-list (split-digits-list multiplicand))
	(m2-list (split-digits-list multiplier))
	(p-list (split-digits-list product)))
    (let ((all-list (append m1-list m2-list p-list))
	  (max-digit 9)
	  (digit-hash (make-hash-table 10))
	  (ok-flag #t))
      (let ((llen (length all-list)))
	(begin
	  (if (not (= llen max-digit))
	      (begin
		#f)
	      (begin
		(hash-clear! digit-hash)

		(do ((ii 0 (1+ ii)))
		    ((>= ii max-digit))
		  (begin
		    (let ((this-digit (list-ref all-list ii)))
		      (let ((this-count (hash-ref digit-hash this-digit 0)))
			(begin
                          (if (>= this-count 1)
                              (begin
                                (set! ok-flag #f)
                                ))

                          (hash-set! digit-hash this-digit (1+ this-count))
			  )))
		    ))

                (if (equal? ok-flag #t)
                    (begin
                      (do ((ii 1 (1+ ii)))
                          ((or (> ii max-digit)
                               (equal? ok-flag #f)))
                        (begin
                          (let ((this-digit (hash-ref digit-hash ii #f)))
                            (begin
                              (if (or (equal? this-digit #f)
                                      (> this-digit 1)
                                      (< this-digit 1))
                                  (begin
                                    (set! ok-flag #f))
                                  )))
                          ))
                      ))

		ok-flag
		))
	  ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-triple-pandigital-1)
  (let ((sub-name "test-is-triple-pandigital-1")
	(test-list
	 (list
	  (list 3 5 15 #f)
          (list 12 483 5796 #t)
	  (list 39 186 7254 #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num1 (list-ref alist 0))
		 (test-num2 (list-ref alist 1))
		 (test-prod (list-ref alist 2))
		 (shouldbe-bool (list-ref alist 3)))
	     (let ((result-bool (is-triple-pandigital? test-num1 test-num2 test-prod)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display (format #f "~a : error (~a) : number1 = ~a, number2 = ~a, product = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-num1 test-num2 test-prod
					shouldbe-bool result-bool))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (is-trip-list-pandigital? m1-list m2-list p-list)
  (let ((all-list (append m1-list m2-list p-list))
        (max-digit 9)
        (ok-flag #t))
    (let ((llen (length all-list)))
      (begin
        (if (not (= llen max-digit))
            (begin
              #f)
            (begin
              (do ((ii 1 (1+ ii)))
                  ((or (> ii max-digit)
                       (equal? ok-flag #f)))
                (begin
                  (let ((bflag (member ii all-list)))
                    (begin
                      (if (equal? bflag #f)
                          (begin
                            (set! ok-flag #f)
                            ))
                      ))
                  ))

              ok-flag
              ))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-is-trip-list-pandigital-1)
  (let ((sub-name "test-is-trip-list-pandigital-1")
	(test-list
	 (list
	  (list (list 3) (list 5) (list 1 5) #f)
          (list (list 1 2) (list 4 8 3) (list 5 7 9 6) #t)
	  (list (list 3 9) (list 1 8 6) (list 7 2 5 4) #t)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((mlist1 (list-ref alist 0))
		 (mlist2 (list-ref alist 1))
		 (plist (list-ref alist 2))
		 (shouldbe-bool (list-ref alist 3)))
	     (let ((result-bool
                    (is-trip-list-pandigital? mlist1 mlist2 plist)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : mlist1 = ~a, mlist2 = ~a, "
                         sub-name test-label-index mlist1 mlist2))
		       (display
                        (format
                         #f "product = ~a, shouldbe = ~a, result = ~a~%"
                         plist shouldbe-bool result-bool))
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
(define (estimate-start ii)
  (begin
    (cond
     ((< ii 10)
      (begin
        1000
        ))
     ((< ii 100)
      (begin
        100
        ))
     (else
      (begin
        (1+ ii)
        )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-estimate-start-1)
  (let ((sub-name "test-estimate-start-1")
	(test-list
	 (list
	  (list 3 1000)
          (list 5 1000)
          (list 99 100)
          (list 101 102)
          (list 1000 1001)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((ii (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (estimate-start ii)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : ii = ~a, "
                         sub-name test-label-index ii))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (estimate-max-jj ii max-num)
  (begin
    (cond
     ((< ii 10)
      (begin
        (min 9999 max-num)
        ))
     (else
      (begin
        (min 999 max-num)
        )))
    ))

;;;#############################################################
;;;#############################################################
;;; define a macro to simplify code
(define-syntax process-inner-loop
  (syntax-rules ()
    ((process-inner-loop ii ii-list jj jj-list
                         sum result-htable)
     (begin
       (let ((jj-list (split-digits-list jj))
             (product (* ii jj)))
         (let ((pp-list (split-digits-list product))
               (ok-ii-jj-flag #t)
               (ok-ii-pp-flag #t)
               (ok-jj-pp-flag #t))
           (begin
             (for-each
              (lambda (ii-elem)
                (begin
                  (if (not (equal? (member ii-elem jj-list) #f))
                      (begin
                        (set! ok-ii-jj-flag #f)
                        ))
                  (if (not (equal? (member ii-elem pp-list) #f))
                      (begin
                        (set! ok-ii-pp-flag #f)
                        ))
                  )) ii-list)
             (if (and ok-ii-jj-flag ok-ii-pp-flag)
                 (begin
                   (for-each
                    (lambda (jj-elem)
                      (begin
                        (if (not (equal? (member jj-elem pp-list) #f))
                            (begin
                              (set! ok-jj-pp-flag #f)
                              ))
                        (if (= jj-elem 0)
                            (begin
                              (set! ok-jj-pp-flag #f)
                              ))
                        )) jj-list)
                   (if ok-jj-pp-flag
                       (begin
                         (for-each
                          (lambda (pp-elem)
                            (begin
                              (if (= pp-elem 0)
                                  (begin
                                    (set! ok-jj-pp-flag #f)
                                    ))
                              )) pp-list)

                         (if (and
                              (equal? ok-jj-pp-flag #t)
                              (is-trip-list-pandigital?
                               ii-list jj-list pp-list))
                             (begin
                               (let ((this-list
                                      (hash-ref result-htable product #f)))
                                 (begin
                                   (if (equal? this-list #f)
                                       (begin
                                         (set! sum (+ sum product))
                                         (hash-set!
                                          result-htable
                                          product (list ii jj product))
                                         ))
                                   ))
                               ))
                         ))
                   ))
             )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (begin
    (let ((sum 0)
          (max-ii (min 999 max-num))
          (digit-htable (make-hash-table 10))
	  (result-htable (make-hash-table 100)))
      (begin
	(do ((ii 1 (1+ ii)))
	    ((> ii max-ii))
	  (begin
            (let ((ii-list (split-digits-list ii))
                  (jstart (estimate-start ii)))
              (let ((ii2-list (srfi-1:delete-duplicates ii-list))
                    (ii-flag (member 0 ii-list))
                    (max-jj (estimate-max-jj ii max-num)))
                (begin
                  (if (and (equal? ii-list ii2-list)
                           (equal? ii-flag #f))
                      (begin
                        (do ((jj jstart (1+ jj)))
                            ((> jj max-jj))
                          (begin
                            (process-inner-loop
                             ii ii-list jj jj-list
                             sum result-htable)
                            ))
                        ))
                  )))
            ))

	(if (equal? debug-flag #t)
	    (begin
	      (let ((key-list
                     (sort
                      (hash-map->list
                       (lambda (key value)
                         (begin
                           key
                           )) result-htable) <))
                    (local-sum 0)
		    (local-sum-list (list)))
		(begin
		  (for-each
		   (lambda (key)
		     (begin
                       (let ((vlist (hash-ref result-htable key (list))))
                         (begin
                           (set! local-sum (+ local-sum key))
                           (set! local-sum-list
                                 (append local-sum-list (list vlist)))

                           (let ((mult1 (list-ref vlist 0))
                                 (mult2 (list-ref vlist 1))
                                 (prod (list-ref vlist 2)))
                             (begin
                               (display
                                (ice9-format:format
                                 #f "    ~:d x ~:d = ~:d~%"
                                 mult1 mult2 prod))
                               ))
                           ))
                       )) key-list)
		  ))
              ))

	(display
         (ice9-format:format
          #f "sum of all products = ~:d, where the mulitpliers are less than ~:d~%"
          sum max-num))
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
    (display (format #f "Problem 032 - We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.~%"))
    (newline)
    (display (format #f "The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.~%"))
    (display (format #f "Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.~%"))
    (newline)
    (display (format #f "HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.~%"))
    (newline)
    (display (format #f "Uses the fact that the number of digits of the first~%"))
    (display (format #f "number multiplied by the second, plus the product digits~%"))
    (display (format #f "should be equal to 9. If the first multiplier < second~%"))
    (display (format #f "then the first multiplier can at most be 3-digits long,~%"))
    (display (format #f "and the product must be 3-digits long as well.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-is-triple-pandigital-1 counter)
           (run-test test-is-trip-list-pandigital-1 counter)
           (run-test test-estimate-start-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 1000)
	  (debug-flag #t))
      (begin
        (time-code
         (begin
           (main-loop max-num debug-flag)
           ))
	))

    (newline)
    (force-output)

    (let ((max-num 10000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))
    (newline)
    ))
