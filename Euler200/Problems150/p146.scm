#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### This is free and unencumbered software released into the public domain.
;;;###
;;;### Anyone is free to copy, modify, publish, use, compile, sell, or
;;;### distribute this software, either in source code form or as a compiled
;;;### binary, for any purpose, commercial or non-commercial, and by any
;;;### means.
;;;###
;;;### In jurisdictions that recognize copyright laws, the author or authors
;;;### of this software dedicate any and all copyright interest in the
;;;### software to the public domain. We make this dedication for the benefit
;;;### of the public at large and to the detriment of our heirs and
;;;### successors. We intend this dedication to be an overt act of
;;;### relinquishment in perpetuity of all present and future rights to this
;;;### software under copyright law.
;;;###
;;;### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;### IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;### OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;### ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;### OTHER DEALINGS IN THE SOFTWARE.
;;;###
;;;### For more information, please refer to <http://unlicense.org/>
;;;###

;;;######################################################
;;;######################################################
;;;###                                                ###
;;;###  project euler 146                             ###
;;;###                                                ###
;;;###  last updated December 25, 2012                ###
;;;###                                                ###
;;;###  written by Robert Haramoto                    ###
;;;###                                                ###
;;;######################################################
;;;######################################################

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice-9-format:)))

;;;### srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
	      :renamer (symbol-prefix-proc 'srfi-11:)))

;;;### srfi-19 for date functions
(use-modules ((srfi srfi-19)
	      :renamer (symbol-prefix-proc 'srfi-19:)))

;;;### ls - find all test subroutines to run
(use-modules ((ice-9 ls)
	      :renamer (symbol-prefix-proc 'ice-9-ls:)))

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
	     (* 0.0010
                (truncate (* 1000.0 (- nmins (* nhours 60.0)))))))
	(let ((nseconds
	       (* 0.0010
		  (truncate
		   (* 1000.0
                      (- nsecs
                         (+ (* nhours 60.0 60.0)
                            (* nminutes 60.0))))))))
	  (begin
	    (if (<= nhours 0.0)
                (begin
                  (if (<= nminutes 0.0)
                      (begin
                        (format #f "~a seconds" nsecs))
                      (begin
                        (format #f "~a minutes, ~a seconds" nminutes nseconds)
                        )))
                (begin
                  (if (<= nminutes 0.0)
                      (begin
                        (format #f "~a hours, ~a seconds" nhours nseconds))
                      (begin
                        (format #f "~a hours, ~a minutes, ~a seconds"
                                nhours nminutes nseconds)
                        ))
                  ))
            ))
        )))
  (if (and (number? dend) (number? dstart))
      (begin
	(let ((jd-diff (exact->inexact (- dend dstart))))
          (begin
            (if (< jd-diff 1.0)
                (begin
                  (let ((tstring (local-process-sub-day jd-diff)))
                    (begin
                      tstring
                      )))
                (begin
                  (let ((ndays (truncate jd-diff)))
                    (let ((dfract-diff (- jd-diff ndays)))
                      (let ((tstring (local-process-sub-day dfract-diff)))
                        (let ((ttstring (format #f "~a days, ~a" ndays tstring)))
                          (begin
                            ttstring
                            )))
                      ))
                  ))
            )))
      (begin
        #f
        )))

;;;#############################################################
;;;#############################################################
(define (current-date-time-string)
  (let ((this-datetime (srfi-19:current-date)))
    (let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
	  (s2 (string-downcase
               (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
      (begin
	(format #f "~a, ~a" s1 s2)
	))
    ))

;;;#############################################################
;;;#############################################################
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (make-prime-array max-num)
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
      (list->array 1 (reverse result-list))
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-array-1)
  (let ((sub-name "test-make-prime-array-1")
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
	     (let ((result-array (make-prime-array test-num)))
	       (let ((slen (length shouldbe-list))
		     (rlen (car (array-dimensions result-array))))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, lengths not equal, shouldbe=~a, result=~a~%"
					  sub-name test-label-index test-num
					  shouldbe slen rlen))
			 (set! ok-flag #f)
			 ))
		   (do ((ii 0 (1+ ii)))
		       ((>= ii slen))
		     (begin
		       (let ((s-elem (list-ref shouldbe-list ii))
			     (r-elem (array-ref result-array ii)))
			 (begin
			   (if (not (equal? s-elem r-elem))
			       (begin
				 (display (format #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a, discrepancy at ii=~a, shouldbe=~a, result=~a~%"
						  sub-name test-label-index test-num
						  shouldbe result ii s-elem r-elem))
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
	(test-label-index 0)
        (ok-flag #t))
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
	(prime-array (make-prime-array 20))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result (is-array-prime? test-num prime-array)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format #f "~a : error (~a) : num=~a, "
                                sub-name test-label-index test-num))
		       (display
                        (format #f "shouldbe=~a, result=~a~%"
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
(define-syntax check-prime-list
  (syntax-rules ()
    ((check-prime-list aprime prime-list is-prime-flag)
     (begin
       (for-each
	(lambda (pr-num)
	  (begin
	    (if (and (equal? is-prime-flag #t)
		     (zero? (modulo pr-num aprime)))
		(begin
		  (set! is-prime-flag #f)
		  ))
	    )) prime-list)
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax check-composite-list
  (syntax-rules ()
    ((check-composite-list aprime composite-list composite-htable)
     (begin
       (for-each
	(lambda (cp-num)
	  (begin
	    (if (and (equal? (hash-ref composite-htable cp-num -1) -1)
		     (zero? (modulo cp-num aprime)))
		(begin
		  (hash-set! composite-htable cp-num #t)
		  ))
	    )) composite-list)
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (is-sequence-prime? nn plist clist prime-array)
  (let ((nn-2 (* nn nn))
	(max-delta (srfi-1:fold (lambda (anum prev) (max anum prev))
				(car plist) plist)))
    (let ((plen (length plist))
	  (clen (length clist))
	  (aprime 2)
	  (asize (car (array-dimensions prime-array)))
	  (is-prime-flag #t)
	  (max-prime -1)
	  (max-divisor (+ nn max-delta))
	  (prime-list (map (lambda (pnum) (+ nn-2 pnum)) plist))
	  (composite-list (map (lambda (cnum) (+ nn-2 cnum)) clist))
	  (composite-htable (make-hash-table)))
      (begin
	(do ((ii 0 (1+ ii)))
	    ((or (>= ii asize)
		 (>= aprime max-divisor)
		 (equal? is-prime-flag #f)))
	  (begin
	    (set! aprime (array-ref prime-array ii))

	    (check-prime-list aprime prime-list is-prime-flag)

	    (if (equal? is-prime-flag #t)
		(begin
		  (check-composite-list aprime composite-list composite-htable)
		  ))
	    ))

	(if (and (equal? is-prime-flag #t)
		 (> max-divisor aprime))
	    (begin
	      (do ((ii (+ aprime 2) (+ ii 2)))
		  ((or (>= ii max-divisor)
		       (equal? is-prime-flag #f)))
		(begin
		  (check-prime-list ii prime-list is-prime-flag)

		  (if (equal? is-prime-flag #t)
		      (begin
			(check-composite-list aprime composite-list composite-htable)
			))
		  ))
	      ))

	(if (equal? is-prime-flag #t)
	    (begin
	      (for-each
	       (lambda (c-num)
		 (begin
		   (let ((hflag (hash-ref composite-htable c-num #f)))
		     (begin
		       (if (equal? hflag #f)
			   (begin
			     (set! is-prime-flag #f)
			     ))
		       ))
		   )) composite-list)
	      ))

	is-prime-flag
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-is-sequence-prime-1)
  (let ((sub-name "test-is-sequence-prime-1")
	(test-list
	 (list
	  (list 0 #f) (list 2 #f)
	  (list 10 #t) (list 20 #f) (list 30 #f)
	  (list 315410 #t) (list 315420 #f)
	  ))
	(plist (list 1 3 7 9 13 27))
	(clist (list 5 11 15 17 19 21 23 25))
	(prime-array (make-prime-array 20))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result
                    (is-sequence-prime? test-num plist clist
                                        prime-array)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : num=~a, shouldbe=~a, result=~a~%"
                         sub-name test-label-index test-num
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
(define-syntax display-debug-info
  (syntax-rules ()
    ((display-debug-info nn count sum)
     (begin
       (display
        (ice-9-format:format
         #f "nn = ~:d, nn^2 = ~:d : count = ~:d, sum = ~:d~%"
         nn (* nn nn) count sum))
       (force-output)
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax display-status-info
  (syntax-rules ()
    ((display-status-info this-num max-num sum count start-jday)
     (begin
       (let ((end-jday (srfi-19:current-julian-day)))
	 (begin
	   (display
            (ice-9-format:format
             #f "~:d / ~:d : sum = ~:d, count = ~:d : "
             this-num max-num sum count))
	   (display
            (format #f "elapsed time = ~a : ~a~%"
                    (julian-day-difference-to-string end-jday start-jday)
                    (current-date-time-string)))
	   (force-output)
	   (set! start-jday end-jday)
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num max-prime status-num debug-flag)
  (let ((start-jday (srfi-19:current-julian-day)))
    (let ((count 0)
	  (sum 0)
	  (plist (list 1 3 7 9 13 27))
	  (clist (list 11 17 19 21 23))
	  (prime-array (make-prime-array max-prime)))
      (begin
	(let ((end-jday (srfi-19:current-julian-day)))
	  (begin
	    (display
             (ice-9-format:format
              #f "completed prime array calculation to ~:d : elapsed time = ~a : ~a~%"
              max-prime
              (julian-day-difference-to-string end-jday start-jday)
              (current-date-time-string)))
	    (force-output)
	    (set! start-jday end-jday)
	    ))

	(do ((nn 10 (+ nn 10)))
	    ((>= nn max-num))
	  (begin
	    (if (and (not (zero? (modulo nn 3)))
		     (not (zero? (modulo nn 7)))
		     (not (zero? (modulo nn 13))))
		(begin
		  (if (is-sequence-prime? nn plist clist prime-array)
		      (begin
			(set! count (1+ count))
			(set! sum (+ sum nn))

			(if (equal? debug-flag #t)
			    (begin
			      (display-debug-info nn count sum)
			      ))
			))
		  ))

	    (if (zero? (modulo nn status-num))
		(begin
		  (display-status-info nn max-num sum count start-jday)
		  ))
	    ))

	(display
         (ice-9-format:format
          #f "The sum of integers = ~:d : count = ~:d : (which make consecutive primes using ~a, below ~:d)~%"
          sum count plist max-num))
	(force-output)
	))
    ))

;;;###################################################
;;;###################################################
;;;###
;;;###  find-all-tests - returns a list of "test-*"
;;;###  methods
;;;###
(define-public (find-all-tests)
  (define (local-make-test-list input-list result-list)
    (if (or
	 (null? input-list)
	 (not (list? input-list))
	 (< (length input-list) 1))
	(begin
	  result-list)
	(begin
	  (let ((this-func (car input-list))
		(tail-list (cdr input-list)))
            (begin
              (if (symbol? this-func)
                  (begin
                    (let ((tstring (symbol->string this-func)))
                      (if (string-prefix-ci? "test-" tstring)
                          (begin
                            (local-make-test-list
                             tail-list (append (list this-func) result-list)))
                          (begin
                            (local-make-test-list tail-list result-list)
                            ))
                      ))
		(begin
		  (local-make-test-list tail-list result-list)
		  ))
              ))
          )))
  (let ((lfunc-list (ice-9-ls:lls)))
    (begin
      (let ((test-func-list (local-make-test-list lfunc-list (list))))
	test-func-list
	))
    ))

;;;#############################################################
;;;#############################################################
(define (display-all-test-names st-list test-htable)
  (begin
    (let ((count (length st-list)))
      (begin
        (for-each
         (lambda (atest)
           (begin
             (let ((sname (symbol->string atest)))
               (let ((pr-name (hash-ref test-htable sname sname)))
                 (begin
                   (display (format #f "  ~a~%" pr-name))
                   )))
             )) st-list)

        (display (format #f "total number of tests = ~a~%" count))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (run-all-tests debug-flag)
  (begin
    (let ((nsuccess 0)
          (nfailures 0)
          (ntotals 0)
          (test-htable (make-hash-table 50))
          (t-list (find-all-tests)))
      (let ((st-list
             (sort
              t-list
              (lambda (a b)
                (begin
                  (string-ci<? (symbol->string a)
                               (symbol->string b))
                  ))))
            (count (length t-list)))
        (begin
          (hash-clear! test-htable)

          (for-each
           (lambda (atest)
             (begin
               (let ((a-func (primitive-eval atest)))
                 (let ((bresult (a-func))
                       (s-name (symbol->string atest)))
                   (begin
                     (if (equal? bresult #t)
                         (begin
                           (set! nsuccess (1+ nsuccess))
                           (hash-set! test-htable s-name
                                      (format #f "ok! ~a" s-name)))
                         (begin
                           (set! nfailures (1+ nfailures))
                           (hash-set! test-htable
                                      s-name
                                      (format #f "error! *** ~a ***" s-name))
                           ))
                     (set! ntotals (1+ ntotals))
                     )))
               )) st-list)

          (if (equal? debug-flag #t)
              (begin
                (display-all-test-names st-list test-htable)
                ))

          (if (> ntotals 0)
              (begin
                (display
                 (format #f "=====================================================~%"))
                (display (format #f "Test Summary~%"))
                (display (ice-9-format:format
                          #f "successful tests = ~:d (~,1f%)~%"
                          nsuccess (* 100.0 (/ nsuccess ntotals))))
                (display (ice-9-format:format
                          #f "failed tests = ~:d (~,1f%)~%"
                          nfailures (* 100.0 (/ nfailures ntotals))))
                (display (ice-9-format:format
                          #f "total tests = ~:d (~,1f%)~%"
                          (+ nsuccess nfailures)
                          (* 100.0 (/ (+ nsuccess nfailures) ntotals))))
                (display
                 (format #f "=====================================================~%"))
                ))
          ))
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
    (display (format #f "Project Euler 146 - The smallest positive integer n for which the numbers n^2+1, n^2+3, n^2+7, n^2+9, n^2+13, and n^2+27 are consecutive primes is 10. The sum of all such integers n below one-million is 1242490.~%"))
    (newline)
    (display (format #f "What is the sum of all such integers n below 150 million?~%"))
    (newline)
    (display (format #f "A key to speeding up the program is to look at the entire sequence when determining if they are prime.  This idea was found at http://www.purebasic.fr/english/viewtopic.php?f=7&t=26985&start=120.~%"))
    (newline)
    (display (format #f "Another key is to look only at n=ak, where a is even since all the sums numbers must be odd, and a is a multiple of 5 and a multiple of 2, since all numbers can't be divisible by 5.  This was found at http://mukeshiiitm.wordpress.com/2011/06/22/project-euler-146/~%"))
    (newline)
    (display (format #f "Note: this program takes over 8 hours to complete.  It was re-written in c++ and completed around 1 minute.  It's probably due to c++ using 64-bit integers (int64_t), while guile is likely using big nums to do the calculations.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((debug-flag #t))
      (begin
	(time-code
	 (begin
	   (run-all-tests debug-flag)
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 1000000)
	  (max-prime 100000)
	  (status-num 10000000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num max-prime status-num debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((max-num 150000000)
	  (max-prime 5000000)
	  (status-num 25000000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop max-num max-prime status-num debug-flag)
	   ))
	))

    (newline)
    ))
