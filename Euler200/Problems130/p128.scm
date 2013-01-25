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
          (let ((ii-num (array-ref intermediate-array ii)))
            (begin
              (if (= ii-num ii)
                  (begin
                    (set! result-list (cons ii result-list))

                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj max-num))
                      (begin
                        (let ((jj-num (array-ref intermediate-array jj)))
                          (begin
                            (array-set! intermediate-array -1 jj)
                            ))
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
	  (list 40 (list 2 3 5 7 11 13 17 19 23 29 31 37))
	  (list 50 (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))
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
			 (display
                          (format #f "~a : error (~a) : num=~a, shouldbe=~a, "
                                  sub-name test-label-index test-num
                                  shouldbe-list))
			 (display
                          (format #f "lengths not equal, shouldbe=~a, result=~a~%"
                                  slen rlen))
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
				 (display
                                  (format #f "~a : error (~a) : num=~a, "
                                          sub-name test-label-index test-num))
				 (display
                                  (format #f "shouldbe=~a, result=~a, discrepancy at "
                                          shouldbe result))
				 (display
                                  (format #f "ii=~a, shouldbe=~a, result=~a~%"
                                          ii s-elem r-elem))
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
(define-syntax check-element-0
  (syntax-rules ()
    ((check-element-0
      nn this-start-num this-last-elem
      outer-start-num outer-last-elem
      inner-start-num inner-last-elem
      last-tile pd-count target-count
      prime-array
      continue-loop-flag debug-flag)
     (begin
       (let ((diff-1-0 (- (1+ outer-start-num) this-start-num))
             (diff-2-0 (- outer-last-elem this-start-num))
             (diff-3-0 (- this-last-elem this-start-num)))
         (begin
           (if (and (is-array-prime? diff-1-0 prime-array)
                    (is-array-prime? diff-2-0 prime-array)
                    (is-array-prime? diff-3-0 prime-array))
               (begin
                 (set! pd-count (1+ pd-count))
                 (set! last-tile this-start-num)
                 (if (equal? debug-flag #t)
                     (begin
                       (display
                        (format
                         #f "debug num=~a, layer=~a, neighbors=~a, ~a, ~a, ~a, ~a : count = ~a~%"
                         this-start-num nn this-last-elem outer-start-num
                         outer-last-elem inner-start-num inner-last-elem pd-count))
                       (force-output)
                       ))
                 (if (>= pd-count target-count)
                     (begin
                       (set! continue-loop-flag #f)
                       ))
                 ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax check-element-last
  (syntax-rules ()
    ((check-element-last
      nn this-start-num this-last-elem
      outer-start-num outer-last-elem
      inner-start-num inner-last-elem
      last-tile pd-count target-count
      prime-array
      continue-loop-flag debug-flag)
     (begin
       (let ((diff-1-1 (- this-last-elem inner-start-num))
             (diff-2-1 (- this-last-elem this-start-num))
             (diff-3-1
              (- (1- outer-last-elem) this-last-elem)))
         (begin
           (if (and (is-array-prime? diff-1-1 prime-array)
                    (is-array-prime? diff-2-1 prime-array)
                    (is-array-prime? diff-3-1 prime-array))
               (begin
                 (set! pd-count (1+ pd-count))
                 (set! last-tile this-last-elem)
                 (if (equal? debug-flag #t)
                     (begin
                       (display
                        (format
                         #f "debug num=~a, layer=~a, neighbors=~a, ~a, ~a, ~a, ~a : count = ~a~%"
                         this-start-num nn this-last-elem outer-start-num
                         outer-last-elem inner-start-num inner-last-elem pd-count))
                       (force-output)
                       ))
                 (if (>= pd-count target-count)
                     (begin
                       (set! continue-loop-flag #f)
                       ))
                 ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; just two hexagons to be checked for each layer
;;; offset=0 and offset=num-in-layer-1
(define (main-loop target-count max-layers max-prime debug-flag)
  (let ((pd-count 2)
        (last-tile 2)
	(prime-array (make-prime-array max-prime))
	(continue-loop-flag #t)
        (inner-start-num 2)
        (inner-last-elem 7)
        (num-in-inner-layer 6))
    (begin
      ;;; start from the second layer
      (do ((nn 2 (1+ nn)))
          ((or (> nn max-layers)
               (equal? continue-loop-flag #f)))
        (begin
          (let ((num-in-this-layer (+ 6 num-in-inner-layer))
                (num-in-outer-layer (+ 12 num-in-inner-layer)))
            (let ((this-start-num (1+ inner-last-elem))
                  (this-last-elem
                   (+ inner-last-elem num-in-this-layer)))
              (let ((outer-start-num
                     (+ this-start-num num-in-this-layer)))
                (let ((outer-last-elem
                       (+ this-last-elem num-in-outer-layer)))
                  (begin
                    (check-element-0
                     nn this-start-num this-last-elem
                     outer-start-num outer-last-elem
                     inner-start-num inner-last-elem
                     last-tile pd-count target-count
                     prime-array
                     continue-loop-flag debug-flag)

                    (if (equal? continue-loop-flag #t)
                        (begin
                          (check-element-last
                           nn this-start-num this-last-elem
                           outer-start-num outer-last-elem
                           inner-start-num inner-last-elem
                           last-tile pd-count target-count
                           prime-array
                           continue-loop-flag debug-flag)
                          ))

                    (set! inner-start-num this-start-num)
                    (set! inner-last-elem this-last-elem)
                    (set! num-in-inner-layer num-in-this-layer)
                    ))
                )))
          ))

      (display
       (ice9-format:format
        #f "~:d = the ~:d tile in the sequence~%"
        last-tile target-count))
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
    (display (format #f "Project Euler 128 - A hexagonal tile with number 1 is surrounded by a ring of six hexagonal tiles, starting at '12 o'clock' and numbering the tiles 2 to 7 in an anti-clockwise direction.~%"))
    (newline)
    (display (format #f "New rings are added in the same fashion, with the next rings being numbered 8 to 19, 20 to 37, 38 to 61, and so on. The diagram below shows the first three rings.~%"))
    (newline)
    (display (format #f "By finding the difference between tile n and each its six neighbours we shall define PD(n) to be the number of those differences which are prime.~%"))
    (newline)
    (display (format #f "For example, working clockwise around tile 8 the differences are 12, 29, 11, 6, 1, and 13. So PD(8) = 3.~%"))
    (newline)
    (display (format #f "In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and 10, hence PD(17) = 2.~%"))
    (newline)
    (display (format #f "It can be shown that the maximum value of PD(n) is 3.~%"))
    (newline)
    (display (format #f "If all of the tiles for which PD(n) = 3 are listed in ascending order to form a sequence, the 10th tile would be 271.~%"))
    (newline)
    (display (format #f "Find the 2000th tile in this sequence.~%"))
    (newline)
    (display (format #f "This problem is easily solved by noting that each layer additional layer is 6 greater than the layer before it.  The first layer has 6 hexagons, the second has 12, the third has 18,...~%"))
    (newline)
    (display (format #f "Every hexagon can be numbered by label l = Sum_k=1^(n-1)(6k) + offset where the sum is from 1 through n (the layer number), and the offset goes from 0 through 6n-1. There are three types of hexagons, middle elements, corner elements, and the special case offset=6m-1.~%"))
    (display (format #f "Middle elements have 2 neighbors on the interior layer, 2 neighbors on the same layer, and 2 neighbors on the outer layer.  Each neighbor within the same layer has a difference of 1, not a prime.  So of the remaining four hexagon neighbors to be considered, 3 must have prime differences, which is impossible, since both the inner layer and outer layer are consecutive, so if one of them has a prime difference, the other won't.  This means we don't have to consider middle elements at all.~%"))
    (display (format #f "Corner elements have 1 neighbor on the inner layer, 2 neighbors in the same layer, and 3 neighbors on the outer layer, and there are 6 of them per layer.  There are two types to consider.  The first type is has offset greater than zero, and the second has an offset equal to zero.  When the offset is greater than zero, the hexagon label can be written as l = 1 + Sum_k=1^(n-1)(6k) + n*m, where n is the layer number, and m is a counter from 1 through 5 (m=0 gives the offset=0 case).  The middle outside layer element is given by the formula l-out = 1 + Sum_k=1^(n)(6k) + (n+1)*m (with the same offset m). Subtracting the two gives, diff = 6n + m, where n is the layer number, and m goes from 1 through 5.  If this difference is prime, then the other two outer neighbors cannot have a prime difference, so at most this possibility has PD = 2.  If 6n+m is even, then 6n+m-1 and 6n+m+1 must be prime if PD=3.  However, since twin primes must have the form 6x-1/6x+1, we see that it's impossible for the two outer neighbors to be twin primes.  The exception is the case where m=0.~%"))
    (display (format #f "To see that that twin primes can only have the form 6x-1/6x+1 see http://numbers.computation.free.fr/Constants/Primes/twin.html.~%"))
    (display (format #f "Finally, the special case of the middle element where the offset = 6m-1 must be checked, since the same layer may have a prime difference, in addition to a prime difference in the interior and outer layers.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-make-prime-array-1 counter)
           (run-test test-binary-search-1 counter)
	   (run-test test-is-array-prime-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((target-count 10)
	  (max-prime 1000)
          (max-layers 1000)
	  (debug-flag #t))
      (begin
	(time-code
	 (begin
	   (main-loop target-count max-layers
                      max-prime debug-flag)
	   ))
	))

    (newline)
    (force-output)

    (let ((target-count 2000)
	  (max-prime 10000)
          (max-layers 100000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop target-count max-layers
                      max-prime debug-flag)
	   ))
	))

    (newline)
    ))
