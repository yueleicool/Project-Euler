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
;;; radical is the product of the distinct prime factors of a number
(define (calc-radical-array max-num)
  (let ((radical-array (make-array 1 (1+ max-num))))
    (begin
      (do ((cc 2 (1+ cc)))
	  ((> cc max-num))
	(begin
	  (let ((cc-elem (array-ref radical-array cc)))
	    (begin
	      (if (= cc-elem 1)
		  (begin
		    (array-set! radical-array cc cc)

		    (do ((jj (+ cc cc) (+ jj cc)))
			((> jj max-num))
		      (begin
			(let ((rad-elem (array-ref radical-array jj)))
			  (begin
			    (array-set! radical-array (* rad-elem cc) jj)
			    ))
			))
                    ))
              ))
          ))
      radical-array
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-radical-array-1)
  (let ((sub-name "test-calc-radical-array-1")
	(test-list
	 (list
	  (list 2 2) (list 3 3) (list 4 2)
          (list 5 5) (list 6 6) (list 7 7)
          (list 8 2) (list 9 3) (list 10 10)
          (list 11 11) (list 12 6) (list 13 13)
          (list 14 14) (list 15 15) (list 16 2)
          (list 17 17) (list 18 6) (list 19 19)
          (list 20 10)
	  ))
	(max-num 20)
	(test-label-index 0))
    (let ((result-array (calc-radical-array max-num)))
      (begin
        (for-each
         (lambda (a-list)
           (begin
             (let ((aindex (list-ref a-list 0))
                   (svalue (list-ref a-list 1)))
               (let ((rvalue (array-ref result-array aindex)))
                 (begin
                   (if (not (equal? svalue rvalue))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : index = ~a : "
                           sub-name test-label-index aindex))
                         (display
                          (format
                           #f "shouldbe=~a, result=~a~%"
                           svalue rvalue))
                         (quit)
                         ))
                   )))
             (set! test-label-index (1+ test-label-index))
             )) test-list)
        ))
    ))

;;;#############################################################
;;;#############################################################
;;; radical is the product of the distinct prime factors of a number
(define (calc-radical-product-list radical-array max-num)
  (let ((result-list-list (list)))
    (begin
      (do ((cc 2 (1+ cc)))
	  ((> cc max-num))
	(begin
          (let ((half-cc (euclidean/ cc 2))
                (rad-cc (array-ref radical-array cc))
                (delta 1))
            (let ((cc-less-rad (euclidean/ cc rad-cc)))
              (begin
                (if (< rad-cc half-cc)
                    (begin
                      (if (even? cc)
                          (begin
                            (set! delta 2)
                            ))

                      (do ((aa 1 (+ aa delta)))
                          ((> aa half-cc))
                        (begin
                          (let ((bb (- cc aa)))
                            (begin
                              (if (> bb aa)
                                  (begin
                                    (let ((rad-aa (array-ref radical-array aa))
                                          (rad-bb (array-ref radical-array bb)))
                                      (let ((rad-ab (* rad-aa rad-bb)))
                                        (begin
                                          (if (< rad-ab cc-less-rad)
                                              (begin
                                                ;;; if gcd(a, b)=1 then gcd(a, c) = gcd(b, c) = 1
                                                (if (= (gcd rad-aa rad-bb) 1)
                                                    (begin
                                                      (set! result-list-list
                                                            (cons
                                                             (list aa bb cc
                                                                   rad-aa rad-bb rad-cc
                                                                   (* rad-ab rad-cc))
                                                             result-list-list))
                                                      ))
                                                ))
                                          )))
                                    ))
                              ))
                          ))
                      ))
                )))
          ))

      (reverse result-list-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-calc-radical-product-list-1)
  (let ((sub-name "test-calc-radical-product-list-1")
	(test-list
	 (list
	  (list 1 8 9 1 2 3 6)
	  (list 1 48 49 1 6 7 42)
	  (list 1 63 64 1 21 2 42)
	  (list 1 80 81 1 10 3 30)
          (list 5 27 32 5 3 2 30)
          (list 32 49 81 2 7 3 42)
	  ))
	(max-num 100)
	(test-label-index 0))
    (let ((radical-array (calc-radical-array max-num)))
      (let ((result-list-list
             (calc-radical-product-list radical-array max-num)))
        (begin
          (for-each
           (lambda (shouldbe-list)
             (begin
               (if (equal?
                    (member shouldbe-list result-list-list)
                    #f)
                   (begin
                     (display
                      (format
                       #f "~a : error (~a) : "
                       sub-name test-label-index))
                     (display
                      (format
                       #f "shouldbe=~a, result=~a~%"
                       shouldbe-list result-list-list))
                     (quit)
                     ))
               (set! test-label-index (1+ test-label-index))
               )) test-list)
          )))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax display-debug-info
  (syntax-rules ()
    ((display-debug-info
      aa bb cc rad-aa rad-bb rad-cc
      sum-cc abc-count)
     (begin
       (display
        (ice9-format:format
         #f "abc-hit = (~:d; ~:d; ~:d) : rad(~:d) = ~:d < ~:d : "
         aa bb cc (* aa bb cc)
         (* rad-aa rad-bb rad-cc) cc))
       (display
        (ice9-format:format
         #f "sum-cc = ~:d, abc-count = ~:d~%"
         sum-cc abc-count))
       (display
        (ice9-format:format
         #f "    aa = ~:d : rad-aa = ~:d~%"
         aa rad-aa))
       (display
        (ice9-format:format
         #f "    bb = ~:d : rad-bb = ~:d~%"
         bb rad-bb))
       (display
        (ice9-format:format
         #f "    cc = ~:d : rad-cc = ~:d~%"
         cc rad-cc))
       (force-output)
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
(define (main-loop max-cc debug-flag)
  (let ((sum-cc 0)
	(abc-count 0)
        (radical-array (calc-radical-array max-cc)))
    (let ((result-list-list
           (calc-radical-product-list radical-array max-cc)))
      (begin
        ;;; summarize results
        (for-each
         (lambda (a-list)
           (begin
             (let ((aa (list-ref a-list 0))
                   (bb (list-ref a-list 1))
                   (cc (list-ref a-list 2))
                   (rad-aa (list-ref a-list 3))
                   (rad-bb (list-ref a-list 4))
                   (rad-cc (list-ref a-list 5))
                   (rad-abc (list-ref a-list 6)))
               (begin
                 (set! sum-cc (+ sum-cc cc))
                 (set! abc-count (1+ abc-count))

                 (if (equal? debug-flag #t)
                     (begin
                       (display-debug-info
                        aa bb cc rad-aa rad-bb rad-cc
                        sum-cc abc-count)
                       ))
                 ))
             )) result-list-list)

        (display
         (ice9-format:format
          #f "Sum(c) = ~:d, and there are ~:d abc hits (where c <= ~:d).~%"
          sum-cc abc-count max-cc))
        (force-output)
        ))
    ))

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
(define (main args)
  (begin
    (display (format #f "Project Euler 127 - The radical of n, rad(n), is the product of distinct prime factors of n. For example, 504 = 2^3 x 3^2 x 7, so rad(504) = 2 x 3 x 7 = 42.~%"))
    (newline)
    (display (format #f "We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:~%"))
    (display (format #f "1. GCD(a, b) = GCD(a, c) = GCD(b, c) = 1~%"))
    (display (format #f "2. a < b~%"))
    (display (format #f "3. a + b = c~%"))
    (display (format #f "4. rad(abc) < c~%"))
    (newline)
    (display (format #f "For example, (5, 27, 32) is an abc-hit, because:~%"))
    (display (format #f "1. GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1~%"))
    (display (format #f "2. 5 < 27~%"))
    (display (format #f "3. 5 + 27 = 32~%"))
    (display (format #f "4. rad(4320) = 30 < 32~%"))
    (newline)
    (display (format #f "It turns out that abc-hits are quite rare and there are only thirty-one abc-hits for c < 1000, with Sum(c) = 12523.~%"))
    (newline)
    (display (format #f "Find Sum(c) for c < 120000.~%"))
    (newline)
    (display (format #f "Note: This problem has been changed recently, please check that you are using the right parameters.~%"))
    (newline)
    (display (format #f "The first step is to use a method similar to the Sieve of Eratosthenes (see http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes for more details).  The idea is that every 2nd array element starting with index 2 is divisible by 2, every third element starting with index 3 is divisible by 3, and so the radical for each number is just the product of the primes that are found with this sieve.~%"))
    (display (format #f "Also, if gcd(a, b) = 1, then gcd(a, a+b) = 1, see http://www.mathblog.dk/project-euler-127-abc-hits/.~%"))
    (display (format #f "Another idea was found at https://bitbucket.org/shlomif/project-euler/src/a66c05c62987/project-euler/127/euler-127.pl.  Since a+b=c, then rad(abc)=rad(a)*rad(b)*rad(c)>=2*rad(c).  b=c-a>= rad(abc) - a >=2*rad(c) - a. b+a>2*rad(c), or 2b>a+b>2*rad(c).  This suggests looping over c, then checking to see if rad(c)<c/2, to reduce the amount of work done.~%"))
    (display (format #f "This program still took around 2 hours to run, so it was re-written in c++, and completed in about 1 second.  The algorithm is probably ok.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
           (run-test test-calc-radical-array-1 counter)
	   (run-test test-calc-radical-product-list-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-cc 1000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-cc debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-cc 120000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-cc debug-flag)
	   ))
	))

    (newline)
    ))
