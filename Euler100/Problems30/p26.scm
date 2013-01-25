#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;; srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice-9-format:)))

;;;### for let-values
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
(define (decimal-divisors-digits-list numerator denominator max-decimals)
  (let ((result-list (list))
	(counter 0)
	(num numerator)
	(break-flag #f))
    (begin
      (while
       (and (equal? break-flag #f)
	    (< counter max-decimals))
       (begin
         (srfi-11:let-values
          (((q1 r1) (euclidean/ num denominator)))
          (begin
            (set! result-list (cons q1 result-list))
            (set! num (* r1 10))
            (set! counter (1+ counter))
            (if (zero? r1)
                (begin
                  (set! break-flag #t)
                  ))
            ))
         ))

      (set! result-list (reverse result-list))

      result-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-decimal-divisors-digits-list-1)
  (let ((sub-name "test-decimal-divisors-digits-list-1")
	(test-list
	 (list
	  (list 1 1 10 (list 1)) (list 1 2 10 (list 0 5))
	  (list 1 3 10 (list 0 3 3 3 3 3 3 3 3 3))
	  (list 1 4 10 (list 0 2 5)) (list 1 5 10 (list 0 2))
	  (list 1 6 10 (list 0 1 6 6 6 6 6 6 6 6))
	  (list 1 7 10 (list 0 1 4 2 8 5 7 1 4 2))
	  (list 1 8 10 (list 0 1 2 5))
	  (list 1 9 10 (list 0 1 1 1 1 1 1 1 1 1))
	  (list 1 10 10 (list 0 1))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-numer (list-ref alist 0))
		 (test-denom (list-ref alist 1))
		 (test-max (list-ref alist 2))
		 (shouldbe-list (list-ref alist 3)))
	     (let ((result-list (decimal-divisors-digits-list test-numer test-denom test-max)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : numerator = ~a, denominator = ~a, max-digits = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index test-numer test-denom test-max
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
;;; return a list of lists that are each nwidth long starting at
;;; position start-pos
(define (split-list init-list start-pos nwidth)
  (let ((llen (length init-list))
        (result-list-list (list))
        (tmp-count 0)
        (tmp-list (list)))
    (begin
      (do ((ii start-pos (1+ ii)))
          ((>= ii llen))
        (begin
          (let ((elem (list-ref init-list ii)))
            (begin
              (set! tmp-list (cons elem tmp-list))
              (set! tmp-count (1+ tmp-count))
              (if (>= tmp-count nwidth)
                  (begin
                    (set! result-list-list
                          (cons
                           (reverse tmp-list)
                           result-list-list))
                    (set! tmp-list (list))
                    (set! tmp-count 0)
                    ))
              ))
          ))
      (if (> tmp-count 0)
          (begin
            (set! result-list-list
                  (cons
                   (reverse tmp-list)
                   result-list-list))
            ))

      result-list-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-split-list-1)
  (let ((sub-name "test-split-list-1")
	(test-list
	 (list
	  (list (list 1 2 3 4 5 6 7 8 9 10 11 12)
                0 2
                (list (list 1 2) (list 3 4) (list 5 6)
                      (list 7 8) (list 9 10) (list 11 12)))
	  (list (list 1 2 3 4 5 6 7 8 9 10 11 12)
                0 3
                (list (list 1 2 3) (list 4 5 6) (list 7 8 9)
                      (list 10 11 12)))
	  (list (list 1 2 3 4 5 6 7 8 9 10 11 12)
                0 4
                (list (list 1 2 3 4) (list 5 6 7 8)
                      (list 9 10 11 12)))
	  (list (list 1 2 3 4 5 6 7 8 9 10 11 12)
                0 5
                (list (list 1 2 3 4 5) (list 6 7 8 9 10)
                      (list 11 12)))
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list (list-ref alist 0))
		 (start-pos (list-ref alist 1))
		 (nwidth (list-ref alist 2))
		 (shouldbe-list-list (list-ref alist 3)))
	     (let ((result-list-list
                    (split-list test-list start-pos nwidth)))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length result-list-list)))
                 (let ((err-1
                        (format #f "~a : error (~a) : list=~a, start=~a, width=~a : "
                                sub-name test-label-index test-list
                                start-pos nwidth))
                       (err-2
                        (format #f "shouldbe=~a, result=~a : "
                                shouldbe-list-list result-list-list))
                       (err-3
                        (format #f "length discrepency shouldbe=~a, result=~a"
                                slen rlen)))
                   (begin
                     (if (not (equal? slen rlen))
                         (begin
                           (display (format #f "~a~a~a~%" err-1 err-2 err-3))
                           (set! ok-flag #f)
                           ))

                     (for-each
                      (lambda (slist)
                        (begin
                          (if (equal? (member slist result-list-list) #f)
                              (begin
                                (let ((err-4
                                       (format #f "missing element ~a" slist)))
                                  (begin
                                    (display
                                     (format #f "~a~a~a~%" err-1 err-2 err-4))
                                    (set! ok-flag #f)
                                    ))
                                ))
                          )) shouldbe-list-list)
                     )))
               ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
;;; how many times does sub-list appear in init-list
(define (count-list-repeats init-list sub-list)
  (let ((sub-count 0)
        (llen (length init-list)))
    (begin
      (for-each
       (lambda (ilist)
         (begin
           (if (equal? ilist sub-list)
               (begin
                 (set! sub-count (1+ sub-count))
                 ))
           )) init-list)

      sub-count
      )))

;;;#############################################################
;;;#############################################################
(define (test-count-list-repeats-1)
  (let ((sub-name "test-count-list-repeats-1")
	(test-list
	 (list
	  (list (list (list 1 2) (list 3 4) (list 1 2)
                      (list 7 8) (list 9 10) (list 11 12))
                (list 1 2)
                2)
	  (list (list (list 1 2) (list 3 4) (list 1 2)
                      (list 7 8) (list 9 10) (list 11 12))
                (list 11 12)
                1)
	  (list (list (list 1 2 3) (list 3 4 5) (list 1 2 3)
                      (list 7 8 9) (list 7 8 9) (list 7 8 9))
                (list 1 2 3)
                2)
	  (list (list (list 1 2 3) (list 3 4 5) (list 1 2 3)
                      (list 7 8 9) (list 7 8 9) (list 7 8 9))
                (list 3 4 5)
                1)
	  (list (list (list 1 2 3) (list 3 4 5) (list 1 2 3)
                      (list 7 8 9) (list 7 8 9) (list 7 8 9))
                (list 7 8 9)
                3)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-list-list (list-ref alist 0))
		 (sub-list (list-ref alist 1))
		 (shouldbe (list-ref alist 2)))
	     (let ((result
                    (count-list-repeats test-list-list sub-list)))
               (let ((err-1
                      (format #f "~a : error (~a) : list=~a : "
                              sub-name test-label-index test-list-list))
                     (err-2
                      (format #f "shouldbe=~a, result=~a"
                              shouldbe result)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a~a~%" err-1 err-2))
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
;;; look for repeating sub-lists through to the end of dlist
(define (does-list-repeat? dlist ii-start-pos ii-window)
  (let ((local-dlist (list-tail dlist ii-start-pos)))
    (let ((local-dlen (length local-dlist))
          (local-list-list (split-list local-dlist 0 ii-window))
          (sub-list (list-head local-dlist ii-window)))
      (let ((scount (count-list-repeats local-list-list sub-list))
            (expected-match-count (1- (euclidean/ local-dlen ii-window))))
        (begin
          (if (<= ii-window 2)
              (begin
                (set! expected-match-count (- expected-match-count 2))
                ))
          (if (>= scount expected-match-count)
              (begin
                #t)
              (begin
                #f
                ))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-does-list-repeat-1)
  (let ((sub-name "test-does-list-repeat-1")
	(test-list
	 (list
	  (list (list 3 3 3 3 3 3 3 3 3) 0 3 #t)
	  (list (list 0 3 3 3 3 3 3 3 3) 1 3 #t)
	  (list (list 4 3 3 3 3 3 3 3 3) 0 3 #f)
          (list (list 6 6 6 6 6 6 6 6 1 0) 0 1 #t)
	  (list (list 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4) 0 6 #t)
	  (list (list 0 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4) 1 6 #t)
	  (list (list 0 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4) 1 5 #f)
	  (list (list 0 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4) 1 3 #f)
	  (list (list 0 5 8 8 2 3 5 2 9 4 1 1 7 6 4 7 0 5 8 8 2 3 5 2 9 4 1 1 7 6 4 7)
                0 16 #t)
	  (list (list 0 5 8 8 2 3 5 2 9 4 1 1 7 6 4 7 0 5 8 8 2 3 5 2 9 4 1 1 7 6 4 7 5 8 8)
                0 4 #f)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-dlist (list-ref alist 0))
		 (start-pos (list-ref alist 1))
		 (ii-window (list-ref alist 2))
		 (shouldbe-bool (list-ref alist 3)))
	     (let ((result-bool
                    (does-list-repeat? test-dlist start-pos ii-window)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display (format #f "~a : error (~a) : test list=~a, "
					sub-name test-label-index test-dlist))
                       (display (format #f "start pos=~a, ii-window=~a : "
					start-pos ii-window))
		       (display (format #f "shouldbe = ~a, result = ~a~%"
                                        (if shouldbe-bool "true" "false")
                                        (if result-bool "true" "false")))
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
;;; makes use of the fact that repeating decimals for 1/p,
;;; where p a prime, repeat at most by the number itself
;;; see http://en.wikipedia.org/wiki/Repeating_decimal
(define (find-repeat-decimal numerator denominator)
  (let ((max-num (* 4 denominator))
	(max-window denominator))
    (let ((dlist (decimal-divisors-digits-list
		  numerator denominator max-num))
          (max-pos 5)
	  (max-digits denominator)
	  (min-repeat-list (list))
	  (min-repeat-length -1)
	  (break-flag #f))
      (let ((dlen (length dlist))
            (rdlist (reverse dlist)))
	(begin
	  (cond
	   ((< dlen max-num)
            (begin
              ;;; return empty list for terminating decimals
              (list)
              ))
	   (else
	    (begin
	      ;;; we have a non-terminating decimal expansion,
	      ;;; so look at various window sizes, reversed dlist
              (do ((ii-start 0 (1+ ii-start)))
                  ((or (>= ii-start max-pos)
                       (equal? break-flag #t)))
                (begin
                  (do ((ii-window 1 (1+ ii-window)))
                      ((or (>= ii-window max-window)
                           (equal? break-flag #t)))
                    (begin
                      (let ((rbool
                             (does-list-repeat? dlist ii-start ii-window)))
                        (begin
                          (if (equal? rbool #t)
                              (begin
                                (set! min-repeat-list
                                      (list-head
                                       (list-tail dlist ii-start)
                                       ii-window))
                                (set! min-repeat-length ii-window)
                                (set! break-flag #t)
                                ))
                          ))
                      ))
                  ))

              min-repeat-list
              )))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-repeat-decimal-1)
  (let ((sub-name "find-repeat-decimal-1")
	(test-list
	 (list
	  (list 1 2 (list))
	  (list 1 3 (list 3))
	  (list 1 5 (list))
	  (list 1 6 (list 6))
	  (list 1 7 (list 1 4 2 8 5 7))
	  (list 1 11 (list 0 9))
	  (list 1 13 (list 0 7 6 9 2 3))
	  (list 1 17 (list 0 5 8 8 2 3 5 2 9 4 1 1 7 6 4 7))
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((numerator (list-ref alist 0))
		 (denominator (list-ref alist 1))
		 (shouldbe-list (list-ref alist 2)))
	     (let ((result-list
                    (find-repeat-decimal numerator denominator)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : numerator = ~a, denominator = ~a, "
                         sub-name test-label-index numerator denominator))
		       (display
                        (format #f "shouldbe = ~a, result = ~a~%"
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
	(begin
	  (= nn (smallest-divisor nn 3 max-divisor))
	  ))
      ))
    ))

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
	  (list 52 #f) (list 53 #t) (list 54 #f) (list 55 #f)
	  (list 56 #f) (list 57 #f) (list 58 #f) (list 59 #t)
	  (list 60 #f) (list 61 #t) (list 62 #f) (list 63 #f)
	  (list 64 #f) (list 65 #f) (list 66 #f) (list 67 #t)
	  (list 68 #f) (list 69 #f) (list 70 #f) (list 71 #t)
	  (list 72 #f) (list 73 #t) (list 74 #f) (list 75 #f)
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
;;; since primes give the maximal number of repeating decimals
;;; only look at odd numbers to reduce the amount of work
;;; by fermat's little theorem, 10^(p-1) = 1 mod p, the
;;; period is equal to the p-1 if 10 a primative root mod p
;;; larger the prime the larger the repeating cycle.
(define (main-loop max-num)
  (let ((result-list (list))
	(result-len 0)
	(result-denominator 0)
        (start-num max-num)
        (end-loop-flag #f)
        (start-jday (srfi-19:current-julian-day)))
    (begin
      (if (even? start-num)
          (begin
            (set! start-num (1- start-num))
            ))

      (do ((ii-denom start-num (- ii-denom 2)))
	  ((or (<= ii-denom 1)
               (equal? end-loop-flag #t)))
	(begin
          (if (prime? ii-denom)
              (begin
                (let ((this-list (find-repeat-decimal 1 ii-denom)))
                  (begin
                    (if (and (list? this-list)
                             (> (length this-list) 0))
                        (begin
                          (let ((this-len (length this-list)))
                            (begin
                              (if (> this-len result-len)
                                  (begin
                                    (set! result-list this-list)
                                    (set! result-len this-len)
                                    (set! result-denominator ii-denom)
                                    ))

                              (if (>= this-len (- ii-denom 1))
                                  (begin
                                    (set! end-loop-flag #t)
                                    ))
                              ))
                          ))
                    ))
                ))
	  ))

      (if (> result-denominator 0)
	  (begin
	    (let ((rdiv (/ 1.0 result-denominator)))
	      (begin
		(display
                 (ice-9-format:format
                  #f "1/~:d = ~3,8f ..., has a ~:d digit recurring cycle = ~a~%"
                  result-denominator rdiv result-len
                  result-list))
		(display
                 (ice-9-format:format
                  #f "(for denominators less than ~:d)~%"
                  max-num))
		)))
	  (begin
	    (display
             (ice-9-format:format
              #f "no results found for denominators less than ~:d~%"
              max-num))
	    ))
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
    (display (format #f "Problem 026 - A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:~%"))
    (display (format #f "1/2 = 0.5, 1/3 = 0.(3), 1/4 = 0.25, 1/5 = 0.2~%"))
    (display (format #f "1/6 = 0.1(6), 1/7 = 0.(142857), 1/8 = 0.125, 1/9 = 0.(1), 1/10 = 0.1~%"))
    (newline)
    (display (format #f "Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.~%"))
    (newline)
    (display (format #f "Find the value of d <= 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.~%"))
    (newline)
    (display (format #f "The key to speeding this program up is to note that the repeating decimal~%"))
    (display (format #f "period is equal to p-1 if p a prime, by fermat's little theorem.~%"))
    (display (format #f "This means that one should work backwards, in order to find the prime~%"))
    (display (format #f "with the largest period.~%"))
    (display (format #f "See http://en.wikipedia.org/wiki/Repeating_decimal~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-decimal-divisors-digits-list-1 counter)
           (run-test test-split-list-1 counter)
           (run-test test-count-list-repeats-1 counter)
	   (run-test test-does-list-repeat-1 counter)
           (run-test test-find-repeat-decimal-1 counter)
           (run-test test-prime-1 counter)

	   (display (ice-9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (newline)
    (force-output)

    (let ((max-num 100))
      (begin
        (time-code
         (begin
	   (main-loop max-num)
           ))
	))

    (newline)
    (force-output)

    (let ((max-num 1000))
      (begin
	(time-code
	 (begin
	   (main-loop max-num)
	   ))
	))
    (newline)
    ))
