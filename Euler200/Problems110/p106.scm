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

;;;### ice-0 rdelim for read-line functions
(use-modules ((ice-9 rdelim)
	      :renamer (symbol-prefix-proc 'ice9-rdelim:)))

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
(define (are-lists-disjoint? llist1 llist2)
  (let ((ok-flag #t))
    (begin
      (for-each
       (lambda (elem1)
	 (if (not (equal? (member elem1 llist2) #f))
	     (begin
	       (set! ok-flag #f)
	       ))) llist1)
      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-are-lists-disjoint-1)
  (let ((sub-name "test-are-lists-disjoint-1")
	(test-list
	 (list
	  (list (list 1) (list 2) #t)
	  (list (list 1 2) (list 3 4) #t)
	  (list (list 1 2 3) (list 4 5 6) #t)
	  (list (list 1 2 3) (list 4 5 6 3) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((set-1-list (list-ref this-list 0))
		 (set-2-list (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (are-lists-disjoint? set-1-list set-2-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : set-1-list = ~a, set-2-list = ~a, shouldbe = ~a, result = ~a~%"
					       sub-name test-label-index set-1-list set-2-list shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (are-subset-pairs-dominant? subset-1-list subset-2-list)
  (define (local-l1-greater-l2 llist1 llist2 llen1)
    (let ((ok-flag #t)
	  (loop-continue-flag #t))
      (begin
	(do ((ii 0 (1+ ii)))
	    ((or (>= ii llen1)
		 (equal? loop-continue-flag #f)))
	  (begin
	    (let ((elem1 (list-ref llist1 ii))
		  (elem2 (list-ref llist2 ii)))
	      (begin
		(if (<= elem1 elem2)
		    (begin
		      (set! ok-flag #f)
		      (set! loop-continue-flag #f)
		      ))
		))
	    ))
	ok-flag
	)))
  (begin
    (let ((llen1 (length subset-1-list))
	  (llen2 (length subset-2-list)))
      (begin
	(if (equal? llen1 llen2)
	    (begin
	      (let ((elem1 (car subset-1-list))
		    (elem2 (car subset-2-list)))
		(begin
		  (if (>= elem1 elem2)
		      (begin
			(local-l1-greater-l2
                         subset-1-list subset-2-list llen1))
		      (begin
			(local-l1-greater-l2
                         subset-2-list subset-1-list llen1)
			))
		  )))
	    (begin
	      #f
	      ))
	))
    ))

;;;#############################################################
;;;#############################################################
(define (test-are-subset-pairs-dominant-1)
  (let ((sub-name "test-are-subset-pairs-dominant-1")
	(test-list
	 (list
	  (list (list 1 2) (list 3 4) #t)
	  (list (list 3 4) (list 1 2) #t)
	  (list (list 1 3) (list 2 4) #t)
	  (list (list 2 4) (list 1 3) #t)
	  (list (list 2 3) (list 1 4) #f)
	  (list (list 1 4) (list 2 3) #f)
	  (list (list 1 2 3) (list 4 5 6) #t)
	  (list (list 4 5 6) (list 1 2 3) #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((subset1 (list-ref this-list 0))
		 (subset2 (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (are-subset-pairs-dominant? subset1 subset2)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display (format #f "~a : error (~a) : subset1 = ~a, subset2 = ~a, shouldbe = ~a, result = ~a~%"
					sub-name test-label-index subset1 subset2 shouldbe result))
		       (quit)
		       ))
		 )))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list))
    ))

;;;#############################################################
;;;#############################################################
(define (make-power-set start-list)
  (define (local-loop
           depth max-depth start-index end-index start-list
           current-list acc-list)
    (begin
      (cond
       ((>= depth max-depth)
        (begin
          (let ((sorted-list (sort current-list <)))
            (begin
              (cons sorted-list acc-list)
              ))
          ))
       (else
        (begin
          (do ((ii start-index (1+ ii)))
              ((>= ii end-index))
            (begin
              (let ((anum (list-ref start-list ii)))
                (let ((next-current-list (cons anum current-list)))
                  (let ((next-acc-list
                         (local-loop
                          (1+ depth) max-depth (1+ ii) end-index
                          start-list next-current-list acc-list)))
                    (begin
                      (set! acc-list next-acc-list)
                      ))
                  ))
              ))

          acc-list
          )))
      ))
  (begin
    (let ((result-list-list (list))
          (slen (length start-list)))
      (begin
        (do ((ii 1 (1+ ii)))
            ((> ii slen))
          (begin
            (let ((rr-list-list
                   (local-loop
                    0 ii 0 slen start-list
                    (list) (list))))
              (begin
                (set! result-list-list
                      (append result-list-list rr-list-list))
                ))
            ))

        result-list-list
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-make-power-set-1)
  (let ((sub-name "test-make-power-set-1")
	(test-list
	 (list
	  (list (list 1 2)
                (list (list 1) (list 2) (list 1 2)))
	  (list (list 1 2 3)
                (list (list 1) (list 2) (list 3)
                      (list 1 2) (list 1 3) (list 2 3)
                      (list 1 2 3)))
          (list (list 1 2 3 4)
                (list (list 1) (list 2) (list 3) (list 4)
                      (list 1 2) (list 1 3) (list 1 4)
                      (list 2 3) (list 2 4) (list 3 4)
                      (list 1 2 3) (list 1 2 4) (list 1 3 4)
                      (list 2 3 4) (list 1 2 3 4)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((start-set (list-ref this-list 0))
		 (shouldbe-list-list (list-ref this-list 1)))
	     (let ((result-list-list
                    (make-power-set start-set)))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length result-list-list)))
                 (begin
                   (if (not (equal? slen rlen))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : start = ~a : "
                           sub-name test-label-index start-set))
                         (display
                          (format
                           #f "shouldbe = ~a, result = ~a : "
                           shouldbe-list-list result-list-list))
                         (display
                          (format
                           #f "length discrepency, shouldbe = ~a, result = ~a~%"
                           slen rlen))
                         (quit)
                         ))
                   (for-each
                    (lambda (s-list)
                      (begin
                        (if (equal? (member s-list result-list-list) #f)
                            (begin
                              (display
                               (format
                                #f "~a : error (~a) : start = ~a : "
                                sub-name test-label-index start-set))
                              (display
                               (format
                                #f "shouldbe = ~a, result = ~a : "
                                shouldbe-list-list result-list-list))
                              (display
                               (format
                                #f "missing shouldbe element = ~a~%"
                                s-list))
                              (quit)
                              ))
                        )) shouldbe-list-list)
                   ))
               ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (extract-subsets-fixed-len powerset-list asize)
  (let ((result-list-list (list)))
    (begin
      (for-each
       (lambda (a-list)
         (begin
           (let ((a-len (length a-list)))
             (begin
               (if (equal? a-len asize)
                   (begin
                     (set! result-list-list
                           (cons a-list result-list-list))
                     ))
               ))
           )) powerset-list)
      result-list-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-extract-subsets-fixed-len-1)
  (let ((sub-name "test-extract-subsets-fixed-len-1")
	(test-list
	 (list
	  (list (list (list 1) (list 2) (list 1 2))
                1 (list (list 1) (list 2)))
	  (list (list (list 1) (list 2) (list 1 2))
                2 (list (list 1 2)))
	  (list (list (list 1) (list 2) (list 3)
                      (list 1 2) (list 1 3) (list 2 3)
                      (list 1 2 3))
                2 (list (list 1 2) (list 1 3) (list 2 3)))
	  (list (list (list 1) (list 2) (list 3)
                      (list 1 2) (list 1 3) (list 2 3)
                      (list 1 2 3))
                3 (list (list 1 2 3)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((powerset-list (list-ref this-list 0))
                 (asize (list-ref this-list 1))
		 (shouldbe-list-list (list-ref this-list 2)))
	     (let ((result-list-list
                    (extract-subsets-fixed-len powerset-list asize)))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length result-list-list)))
                 (begin
                   (if (not (equal? slen rlen))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : start = ~a, size = ~a : "
                           sub-name test-label-index powerset-list asize))
                         (display
                          (format
                           #f "shouldbe = ~a, result = ~a : "
                           shouldbe-list-list result-list-list))
                         (display
                          (format
                           #f "length discrepency, shouldbe = ~a, result = ~a~%"
                           slen rlen))
                         (quit)
                         ))
                   (for-each
                    (lambda (s-list)
                      (begin
                        (if (equal? (member s-list result-list-list) #f)
                            (begin
                              (display
                               (format
                                #f "~a : error (~a) : start = ~a, size = ~a : "
                                sub-name test-label-index powerset-list asize))
                              (display
                               (format
                                #f "shouldbe = ~a, result = ~a : "
                                shouldbe-list-list result-list-list))
                              (display
                               (format
                                #f "missing shouldbe element = ~a~%"
                                s-list))
                              (quit)
                              ))
                        )) shouldbe-list-list)
                   ))
               ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; all subsets are the same size
(define (count-non-dominated-fixed-size subset-list)
  (let ((count 0)
        (slen (length subset-list)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii slen))
        (begin
          (let ((alist (list-ref subset-list ii)))
            (begin
              (do ((jj (1+ ii) (1+ jj)))
                  ((>= jj slen))
                (begin
                  (let ((blist (list-ref subset-list jj)))
                    (begin
                      (if (are-lists-disjoint?
                           alist blist)
                          (begin
                            (if (not (are-subset-pairs-dominant?
                                      alist blist))
                                (begin
                                  (set! count (1+ count))
                                  ))
                            ))
                      ))
                  ))
              ))
          ))

      count
      )))

;;;#############################################################
;;;#############################################################
(define (test-count-non-dominated-fixed-size-1)
  (let ((sub-name "test-count-non-dominated-fixed-size-1")
	(test-list
	 (list
	  (list
           (list (list 1 2) (list 1 3) (list 1 4)
                 (list 2 3) (list 2 4) (list 3 4))
           1)
          (list
           (list (list 1 2) (list 1 3) (list 1 4) (list 1 5)
                 (list 2 3) (list 2 4) (list 2 5) (list 3 4)
                 (list 3 5) (list 4 5))
           5)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((powerset-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result
                    (count-non-dominated-fixed-size powerset-list)))
               (begin
                 (if (not (equal? shouldbe result))
                     (begin
                       (display
                        (format
                         #f "~a : error (~a) : powerset = ~a : "
                         sub-name test-label-index powerset-list))
                       (display
                        (format
                         #f "shouldbe = ~a, result = ~a : "
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
;;; all subsets are the same size
(define (count-all-disjointed-subset-pairs powerset-list)
  (let ((count 0)
        (plen (length powerset-list)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii plen))
        (begin
          (let ((ii-list (list-ref powerset-list ii)))
            (begin
              (do ((jj (1+ ii) (1+ jj)))
                  ((>= jj plen))
                (begin
                  (let ((jj-list (list-ref powerset-list jj)))
                    (begin
                      (if (are-lists-disjoint? ii-list jj-list)
                          (begin
                            (set! count (1+ count))
                            ))
                      ))
                  ))
              ))
          ))
      count
      )))

;;;#############################################################
;;;#############################################################
(define (test-count-all-disjointed-subset-pairs-1)
  (let ((sub-name "test-count-all-disjointed-subset-pairs-1")
	(test-list
	 (list
          (list (list 1 2) 1)
          (list (list 1 2 3) 6)
          (list (list 1 2 3 4) 25)
          (list (list 1 2 3 4 5 6 7) 966)
;;;          (list (list 1 2 3 4 5 6 7 8 9 10 11 12) 261625)
          ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((set-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
             (let ((powerset-list (make-power-set set-list)))
               (let ((result
                      (count-all-disjointed-subset-pairs powerset-list)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : set = ~a, powerset = ~a : "
                           sub-name test-label-index set-list powerset-list))
                         (display
                          (format
                           #f "shouldbe = ~a, result = ~a : "
                           shouldbe result))
                         (quit)
                         ))
                   ))
               ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop set-list)
  (let ((powerset-list (make-power-set set-list))
	(counter 0)
        (slen (length set-list)))
    (let ((plen (length powerset-list))
          (max-pair-size (euclidean/ slen 2)))
      (begin
        (do ((ii 2 (1+ ii)))
            ((> ii max-pair-size))
          (begin
            (let ((ii-pairs-list
                   (extract-subsets-fixed-len powerset-list ii)))
              (let ((subcount
                     (count-non-dominated-fixed-size ii-pairs-list)))
                (begin
                  (set! counter (+ counter subcount))
                  )))
            ))

        (display
         (ice9-format:format
          #f "~:d is the number of sets that need to be tested for equality : "
          counter))
        (display
         (ice9-format:format
          #f "n = ~:d, set = ~a~%"
          (length set-list) set-list))
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
    (display (format #f "Project Euler 106 - Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:~%"))
    (newline)
    (display (format #f "i.  S(B) != S(C); that is, sums of subsets cannot be equal.~%"))
    (display (format #f "ii. If B contains more elements than C then S(B) > S(C).~%"))
    (newline)
    (display (format #f "For this problem we shall assume that a given set contains n strictly increasing elements and it already satisfies the second rule.~%"))
    (newline)
    (display (format #f "Surprisingly, out of the 25 possible subset pairs that can be obtained from a set for which n = 4, only 1 of these pairs need to be tested for equality (first rule). Similarly, when n = 7, only 70 out of the 966 subset pairs need to be tested.~%"))
    (newline)
    (display (format #f "For n = 12, how many of the 261625 subset pairs that can be obtained need to be tested for equality?~%"))
    (newline)
    (display (format #f "NOTE: This problem is related to problems 103 and 105.~%"))
    (newline)
    (display (format #f "The solution was described at http://jsomers.net/blog/pe-oeis.  The only sets that need to be checked are those that have the same length, and are not dominated.  This algorithm is a straight-forward version, which calculates the possible subsets and does not use the more advanced Catalan sequence.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-are-lists-disjoint-1 counter)
	   (run-test test-are-subset-pairs-dominant-1 counter)
           (run-test test-make-power-set-1 counter)
           (run-test test-extract-subsets-fixed-len-1 counter)
           (run-test test-count-non-dominated-fixed-size-1 counter)
           (run-test test-count-all-disjointed-subset-pairs-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((sets-list
	   (list
	    (list 1 2 3 4)
	    (list 1 2 3 4 5 6 7)
	    (list 1 2 3 4 5 6 7 8 9 10 11 12)
	    )))
      (begin
	(for-each
	 (lambda (set-list)
	   (begin
	     (time-code
	      (begin
		(main-loop set-list)
		))

	     (newline)
	     (force-output)
	     )) sets-list)
	))

    (newline)
    ))
