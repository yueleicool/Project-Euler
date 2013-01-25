#! /usr/local/bin/guile \
--no-auto-compile -e main -s
!#

;;;### this code is dedicated to the public domain

;;;### srfi-1 for fold function
(use-modules ((srfi srfi-1)
	      :renamer (symbol-prefix-proc 'srfi-1:)))

;;;### ice-9 format for advanced format
(use-modules ((ice-9 format)
	      :renamer (symbol-prefix-proc 'ice-9-format:)))

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
(define (pentagonal-number this-num)
  (let ((t1 (- (* 3 this-num) 1)))
    (let ((t2 (* this-num t1)))
      (let ((t3 (euclidean/ t2 2)))
	t3
	))))

;;;#############################################################
;;;#############################################################
(define (test-pentagonal-number-1)
  (let ((sub-name "test-pentagonal-number-1")
	(test-list
	 (list
	  (list 1 1) (list 2 5) (list 3 12) (list 4 22)
	  (list 5 35) (list 6 51) (list 7 70) (list 8 92)
	  (list 9 117) (list 10 145)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result (pentagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (inverse-pentagonal-number xx)
  (let ((result #f))
    (let ((tmp1 (sqrt (1+ (* 24 xx)))))
      (let ((tmp2 (/ (1+ tmp1) 6)))
        (begin
          (if (integer? tmp2)
              (begin
                (set! result (inexact->exact tmp2))
                ))
          result
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-inverse-pentagonal-number-1)
  (let ((sub-name "test-inverse-pentagonal-number-1")
	(test-list
	 (list
          (list 1 1) (list 5 2) (list 12 3) (list 22 4)
          (list 35 5) (list 51 6) (list 70 7) (list 92 8)
          (list 117 9) (list 145 10)
	  (list 1560090 1020)
	  (list 23 #f) (list 24 #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
	 (begin
	   (let ((test-num (list-ref alist 0))
		 (shouldbe (list-ref alist 1)))
	     (let ((result
                    (inverse-pentagonal-number test-num)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : test number = ~a, shouldbe = ~a, result = ~a~%"
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
(define (make-pentagonal-list max-num)
  (let ((result-list (list)))
    (begin
      (do ((jj 1 (+ jj 1)))
	  ((> jj max-num))
	(begin
	  (let ((this-pentagonal-num (pentagonal-number jj)))
	    (set! result-list (cons this-pentagonal-num result-list))
	    )))

      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-pentagonal-list-1)
  (let ((sub-name "test-make-pentagonal-list-1")
	(test-list
	 (list
	  (list 1 (list 1))
	  (list 2 (list 1 5))
	  (list 3 (list 1 5 12))
	  (list 4 (list 1 5 12 22))
	  (list 5 (list 1 5 12 22 35))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((test-num (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-list (make-pentagonal-list test-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display (format #f "~a : error (~a) : max num=~a, shouldbe=~a, result=~a~%"
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
(define (populate-pentagonal-htable! p-htable max-num)
  (begin
    (do ((jj 1 (+ jj 1)))
	((> jj max-num))
      (begin
	(let ((this-pentagonal-num (pentagonal-number jj)))
	  (hash-set! p-htable this-pentagonal-num jj)
	  )))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax debug-macro-display
  (syntax-rules ()
    ((debug-macro-display
      ii jj pi-elem pj-elem pdiff
      min-pj min-jj min-pk min-kk
      min-sum min-sum-index min-diff min-diff-index)
     (begin
       (display
        (ice-9-format:format
         #f "debug p(~:d) = ~:d, p(~:d) = ~:d, diff = ~:d~%"
         ii pi-elem jj pj-elem pdiff))
       (display
        (ice-9-format:format
         #f "p(~:d) + p(~:d) = ~:d + ~:d = ~:d = p(~:d)~%"
         min-kk min-jj min-pk min-pj min-sum min-sum-index))
       (display
        (ice-9-format:format
         #f "p(~:d) - p(~:d) = ~:d - ~:d = ~:d = p(~:d)~%"
         min-kk min-jj min-pk min-pj min-diff min-diff-index))
       (force-output)
       ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax inner-macro-process
  (syntax-rules ()
    ((inner-macro-process
      ii jj pi-elem pj-elem pent-htable
      min-pj min-jj min-pk min-kk
      min-sum min-sum-index min-diff min-diff-index
      inner-break-flag debug-flag)
     (begin
       (let ((psum (+ pj-elem pi-elem))
             (pdiff (- pj-elem pi-elem)))
         (let ((psum-flag (hash-ref pent-htable psum #f))
               (pdiff-flag (hash-ref pent-htable pdiff #f)))
           (begin
             (if (equal? psum #f)
                 (begin
                   (let ((inv-psum (inverse-pentagonal-number psum)))
                     (begin
                       (if (not (equal? inv-psum #f))
                           (begin
                             (hash-set! pent-htable psum inv-psum)
                             ))
                       ))
                   ))
             (if (equal? pdiff #f)
                 (begin
                   (let ((inv-pdiff (inverse-pentagonal-number pdiff)))
                     (begin
                       (if (not (equal? inv-pdiff #f))
                           (begin
                             (hash-set! pent-htable pdiff inv-psum)
                             ))
                       ))
                   ))

             (if (and (not (equal? psum-flag #f))
                      (not (equal? pdiff-flag #f)))
                 (begin
                   (if (< pdiff min-diff)
                       (begin
                         (set! min-pj pi-elem)
                         (set! min-jj ii)
                         (set! min-pk pj-elem)
                         (set! min-kk jj)
                         (set! min-sum psum)
                         (set! min-sum-index psum-flag)
                         (set! min-diff pdiff)
                         (set! min-diff-index pdiff-flag)
                         (set! inner-break-flag #t)
                         ))

                   (if (equal? debug-flag #t)
                       (begin
                         (debug-macro-display
                          ii jj pi-elem pj-elem pdiff
                          min-pj min-jj min-pk min-kk
                          min-sum min-sum-index min-diff min-diff-index)
                         ))
                   ))

             (if (> pdiff min-diff)
                 (begin
                   (set! inner-break-flag #t)
                   ))
             )))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-loop max-num debug-flag)
  (let ((counter 0)
        (pent-array
         (list->array 1 (make-pentagonal-list max-num)))
        (pent-htable (make-hash-table max-num))
	(max-pent (pentagonal-number max-num))
	(min-diff (pentagonal-number max-num))
	(min-pj 0)
	(min-jj 0)
	(min-pk 0)
	(min-kk 0)
	(min-sum 0)
	(min-sum-index 0)
	(min-diff-index 0)
        (break-flag #f))
    (let ((plen (car (array-dimensions pent-array))))
      (begin
        (populate-pentagonal-htable! pent-htable max-num)

        (do ((ii 0 (1+ ii)))
            ((>= ii plen))
          (begin
            (let ((pi-elem (array-ref pent-array ii))
                  (inner-break-flag #f))
              (begin
                (do ((jj (1+ ii) (1+ jj)))
                    ((or (>= jj plen)
                         (equal? inner-break-flag #t)))
                  (begin
                    (let ((pj-elem (array-ref pent-array jj)))
                      (begin
                        (inner-macro-process
                         ii jj pi-elem pj-elem pent-htable
                         min-pj min-jj min-pk min-kk
                         min-sum min-sum-index min-diff min-diff-index
                         inner-break-flag debug-flag)
                        ))
                    ))
                ))
            ))

        (if (< min-diff max-pent)
            (begin
              (display
               (ice-9-format:format
                #f "the pair of pentagonal numbers, pj and pk, for which their sum and difference is pentagonal and D = |pk - pj| is minimised, (for index < ~:d, p < ~:d)~%"
                max-num max-pent))
              (display
               (ice-9-format:format
                #f "p(~:d) + p(~:d) = ~:d + ~:d = ~:d = p(~:d)~%"
                min-kk min-jj min-pk min-pj min-sum min-sum-index))
              (display
               (ice-9-format:format
                #f "p(~:d) - p(~:d) = ~:d - ~:d = ~:d = p(~:d)~%"
                min-kk min-jj min-pk min-pj min-diff min-diff-index))
              (display
               (ice-9-format:format
                #f "value of difference = ~:d~%" min-diff)))
            (begin
              (display
               (ice-9-format:format
                #f "no pentagonal pairs found for which their sum and differences are also pentagonal (for index < ~:d, p < ~:d)~%"
                max-num max-pent))
              ))
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
    (display (format #f "Problem 044 - Pentagonal numbers are generated by the formula, Pn=n(3n-1)/2. The first ten pentagonal numbers are:~%"))
    (newline)
    (display (format #f "    1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...~%"))
    (newline)
    (display (format #f "It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70 - 22 = 48, is not pentagonal.~%"))
    (newline)
    (display (format #f "Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference is pentagonal and D = |Pk - Pj| is minimised; what is the value of D?~%"))
    (newline)
    (display (format #f "The inverse of the pentagonal function can be found for any number x,~%"))
    (display (format #f "by considering x = n*(3n-1)/2, and solving the quadratic equation.~%"))
    (display (format #f "n = (1 + sqrt(1 + 24*x))/6.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-pentagonal-number-1 counter)
	   (run-test test-make-pentagonal-list-1 counter)
           (run-test test-inverse-pentagonal-number-1 counter)

	   (display (ice-9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 100)
	  (debug-flag #t))
      (begin
	(main-loop max-num debug-flag)
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
