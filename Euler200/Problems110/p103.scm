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
  (let ((intersect-list (srfi-1:lset-intersection = llist1 llist2)))
    (begin
      (if (equal? intersect-list (list))
	  (begin
	    #t)
	  (begin
	    #f
	    ))
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
					sub-name test-label-index set-1-list set-2-list
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
(define (make-incremental-powerset subset-list next-element)
  (let ((next-subset-list
         (cons (list next-element) subset-list)))
    (begin
      (for-each
       (lambda (sub-list)
         (begin
           (let ((ne-sub-list
                  (sort (cons next-element sub-list) <)))
             (begin
               (set! next-subset-list
                     (cons ne-sub-list next-subset-list))
               ))
           )) subset-list)

      next-subset-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-incremental-powerset-1)
  (let ((sub-name "test-make-incremental-powerset-1")
	(test-list
	 (list
	  (list (list (list 1)) 2
		(list (list 1) (list 2) (list 1 2)))
	  (list (list (list 1) (list 2) (list 1 2)) 3
		(list (list 1) (list 2) (list 1 2)
                      (list 3) (list 1 3) (list 2 3)
                      (list 1 2 3)))
	  (list (list
                 (list 1) (list 2) (list 3)
                 (list 1 2) (list 1 3) (list 2 3)
                 (list 1 2 3)) 4
                (list
                 (list 1) (list 2) (list 3)
                 (list 1 2) (list 1 3) (list 2 3)
                 (list 1 2 3)
                 (list 4) (list 1 4) (list 2 4)
                 (list 3 4) (list 1 2 4) (list 1 3 4)
                 (list 2 3 4) (list 1 2 3 4)))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list-list (list-ref this-list 0))
		 (a-elem (list-ref this-list 1))
		 (shouldbe-list-list (list-ref this-list 2)))
             (let ((rr-list-list
                    (make-incremental-powerset a-list-list a-elem)))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length rr-list-list))
                     (result-list-list (list)))
                 (begin
                   (for-each
                    (lambda (alist)
                      (begin
                        (let ((sr-list (sort alist <)))
                          (begin
                            (set! result-list-list
                                  (cons sr-list result-list-list))
                            ))
                        )) rr-list-list)

                   (if (not (equal? slen rlen))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : a-list = ~a, a-elem = ~a : "
                           sub-name test-label-index a-list-list a-elem))
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
                                  #f "~a : error (~a) : a-list = ~a, a-elem = ~a : "
                                  sub-name test-label-index a-list-list a-elem))
                                (display
                                 (format
                                  #f "shouldbe = ~a, result = ~a : "
                                  shouldbe-list-list result-list-list))
                                (display
                                 (format
                                  #f "missing element shouldbe = ~a~%"
                                  slist))
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
(define (condition-1-same-size-subsets? subset-list)
  (let ((ok-flag #t)
        (slen (length subset-list)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((or (>= ii slen)
               (equal? ok-flag #f)))
        (begin
          (let ((alist (list-ref subset-list ii)))
            (begin
              (do ((jj (1+ ii) (1+ jj)))
                  ((or (>= jj slen)
                       (equal? ok-flag #f)))
                (begin
                  (let ((blist (list-ref subset-list jj)))
                    (begin
                      (if (are-lists-disjoint?
                           alist blist)
                          (begin
                            (if (not (are-subset-pairs-dominant?
                                      alist blist))
                                (begin
                                  (let ((asum (srfi-1:fold + 0 alist))
                                        (bsum (srfi-1:fold + 0 blist)))
                                    (begin
                                      (if (= asum bsum)
                                          (begin
                                            (set! ok-flag #f)
                                            ))
                                      ))
                                  ))
                            ))
                      ))
                  ))
              ))
          ))

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-condition-1-same-size-subsets-1)
  (let ((sub-name "test-condition-1-same-size-subsets-1")
	(test-list
	 (list
	  (list
           (list (list 2 3) (list 2 4) (list 3 4))
           #t)
          (list
           (list (list 1 2) (list 1 3) (list 1 4) (list 1 5)
                 (list 2 3) (list 2 4) (list 2 5) (list 3 4)
                 (list 3 5) (list 4 5))
           #f)
	  (list
           (list (list 3 5) (list 3 6) (list 3 7)
                 (list 5 6) (list 5 7) (list 6 7))
           #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((powerset-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
	     (let ((result
                    (condition-1-same-size-subsets? powerset-list)))
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
;;; condition-1-ok? assumes that condition-2-ok? first!
(define (condition-1-ok? set-list powerset-list)
  (let ((ok-flag #t)
        (slen (length set-list))
        (plen (length powerset-list)))
    (let ((max-pair-size (euclidean/ slen 2)))
      (begin
        (do ((ii 2 (1+ ii)))
            ((or (> ii max-pair-size)
                 (equal? ok-flag #f)))
          (begin
            (let ((ii-pairs-list
                   (extract-subsets-fixed-len powerset-list ii)))
              (let ((aflag
                     (condition-1-same-size-subsets? ii-pairs-list)))
                (begin
                  (if (equal? aflag #f)
                      (begin
                        (set! ok-flag #f)
                        ))
                  )))
            ))
        ok-flag
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-condition-1-ok-1)
  (let ((sub-name "test-condition-1-ok-1")
	(test-list
	 (list
	  (list (list 1) #t)
	  (list (list 1 2) #t)
	  (list (list 2 3 4) #t)
	  (list (list 1 2 3 4) #f)
	  (list (list 2 3 4 5) #f)
	  (list (list 3 5 6 7) #t)
	  (list (list 6 9 11 12 13) #t)
	  (list (list 11 18 19 20 22 25) #t)
	  (list (list 11 18 19 20 22 26) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((set-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
             (let ((power-set (make-power-set set-list)))
               (let ((result (condition-1-ok? set-list power-set)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : set-list = ~a, shouldbe = ~a, result = ~a~%"
                           sub-name test-label-index set-list
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
(define (condition-2-ok? set-list)
  (let ((ok-flag #t)
        (sum-small (car set-list))
        (sum-large 0)
        (slen (length set-list)))
    (let ((max-ii (euclidean/ slen 2)))
      (begin
        (do ((ii 0 (1+ ii)))
            ((or (>= ii max-ii)
                 (equal? ok-flag #f)))
          (begin
            (let ((next-small
                   (+ sum-small
                      (list-ref set-list (+ ii 1))))
                  (next-large
                   (+ sum-large
                      (list-ref set-list (- slen ii 1)))))
              (begin
                (if (<= next-small next-large)
                    (begin
                      (set! ok-flag #f))
                    (begin
                      (set! sum-small next-small)
                      (set! sum-large next-large)
                      ))
                ))
            ))
        ok-flag
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-condition-2-ok-1)
  (let ((sub-name "test-condition-2-ok-1")
	(test-list
	 (list
	  (list (list 1) #t)
	  (list (list 1 2) #t)
	  (list (list 1 2 3) #f)
	  (list (list 1 2 4) #f)
	  (list (list 2 3 4) #t)
	  (list (list 1 2 3 4) #f)
	  (list (list 2 3 4 5) #f)
	  (list (list 3 5 6 7) #t)
	  (list (list 6 9 11 12 13) #t)
	  (list (list 11 18 19 20 22 25) #t)
	  (list (list 11 18 19 20 22 26) #f)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list (list-ref this-list 0))
		 (shouldbe (list-ref this-list 1)))
             (let ((result (condition-2-ok? a-list)))
               (begin
                 (if (not (equal? shouldbe result))
                     (begin
                       (display
                        (format
                         #f "~a : error (~a) : a-list = ~a, shouldbe = ~a, result = ~a~%"
                         sub-name test-label-index a-list
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
(define-syntax check-subsets
  (syntax-rules ()
    ((check-subsets
      full-set-list powerset-list ss-set-flag)
     (begin
       (let ((cond-2-flag
              (condition-2-ok? full-set-list)))
         (begin
           (if (equal? cond-2-flag #t)
               (begin
                 (let ((cond-1-flag
                        (condition-1-ok?
                         full-set-list powerset-list)))
                   (begin
                     (if (equal? cond-1-flag #t)
                         (begin
                           (set! ss-set-flag #t))
                         (begin
                           (set! ss-set-flag #f)
                           ))
                       )))
               (begin
                 (set! ss-set-flag #f)
                 ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; assumes sets of length n-1 is consistent,
;;; add 1 element, then check if its ok
(define (find-minimum-special-sum-set nn start-num end-num)
  (define (generate-sets-of-length-nn
           depth max-depth start-num end-num
           current-sum current-list current-powerset-list
           acc-sum acc-list)
    (begin
      (cond
       ((>= depth max-depth)
	(begin
	  (if (or (<= acc-sum 0)
		  (< current-sum acc-sum))
	      (begin
		(list current-sum (sort current-list <)))
	      (begin
		(list acc-sum acc-list)
		))
	  ))
       (else
	(begin
	  (let ((continue-loop-flag #t)
                (s-current-list (sort current-list <)))
            (let ((stop-num end-num))
              (begin
                (if (>= (length current-list) 2)
                    (begin
                      (let ((sum-first-two
                             (+ (car current-list) (cadr current-list))))
                        (begin
                          (set! stop-num (min sum-first-two end-num))
                          ))
                      ))

                (do ((kk start-num (1+ kk)))
                    ((or (> kk stop-num)
                         (equal? continue-loop-flag #f)))
                  (begin
                    (let ((next-sum (+ current-sum kk)))
                      (begin
                        (if (or (<= acc-sum 0)
                                (<= next-sum acc-sum))
                            (begin
                              (let ((next-powerset-list
                                     (make-incremental-powerset
                                      current-powerset-list kk)))
                                (let ((next-current-list
                                       (append current-list (list kk)))
                                      (ss-set-flag #t))
                                  (begin
                                    (check-subsets
                                     next-current-list next-powerset-list
                                     ss-set-flag)

;;;                                  (display (format #f "debug (~a/~a) current=~a, k=~a, half=~a, ss-set=~a, acc-list=~a~%"
;;;                                                   depth max-depth current-powerset-list
;;;                                                   kk half-powerset-list ss-set-flag acc-list))
;;;                                  (force-output)
                                    (if (equal? ss-set-flag #t)
                                        (begin
                                          (let ((next-list-list
                                                 (generate-sets-of-length-nn
                                                  (1+ depth) max-depth (1+ kk) end-num
                                                  next-sum next-current-list
                                                  next-powerset-list
                                                  acc-sum acc-list)))
                                            (let ((next-acc-sum
                                                   (list-ref next-list-list 0))
                                                  (next-acc-list
                                                   (list-ref next-list-list 1)))
                                              (begin
                                                (set! acc-sum next-acc-sum)
                                                (set! acc-list next-acc-list)
                                                )))
                                          ))
                                    ))
                                ))
                            (begin
                              (set! continue-loop-flag #f)
                              ))
                        ))
                    ))
                (list acc-sum acc-list)
                )))
          ))
       )))
  (begin
    (let ((results-list
           (generate-sets-of-length-nn
            0 nn start-num end-num
            0 (list) (list)
            -1 (list))))
      (begin
        (cadr results-list)
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-find-mininum-special-sum-set-1)
  (let ((sub-name "test-find-minimum-special-sum-set-1")
	(test-list
	 (list
	  (list 1 1 10 (list 1))
	  (list 2 1 10 (list 1 2))
	  (list 3 1 10 (list 2 3 4))
	  (list 4 1 10 (list 3 5 6 7))
	  (list 5 1 20 (list 6 9 11 12 13))
	  (list 6 1 30 (list 11 18 19 20 22 25))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((nn (list-ref this-list 0))
		 (start-num (list-ref this-list 1))
		 (end-num (list-ref this-list 2))
		 (shouldbe-list (list-ref this-list 3)))
	     (let ((result-list
                    (find-minimum-special-sum-set
                     nn start-num end-num)))
	       (begin
		 (if (not (equal? shouldbe-list result-list))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : nn = ~a, start-num = ~a, end-num = ~a : "
                         sub-name test-label-index nn start-num end-num))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (main-loop nn-size start-num end-num)
  (begin
    (let ((min-list
           (find-minimum-special-sum-set
            nn-size start-num end-num)))
      (let ((min-sum-string
             (string-join
              (map number->string min-list) ", "))
            (set-string (string-join
                         (map number->string min-list) "")))
        (begin
          (display
           (format #f "  n = ~a : { ~a } : set-string = ~a~%"
                   nn-size min-sum-string set-string))
          (force-output)
          )))
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
	       (display
                (format #f "elapsed time = ~a : ~a~%"
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
    (display (format #f "Project Euler 103 - Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:~%"))
    (newline)
    (display (format #f "i.  S(B) != S(C); that is, sums of subsets cannot be equal.~%"))
    (display (format #f "ii. If B contains more elements than C then S(B) > S(C).~%"))
    (newline)
    (display (format #f "If S(A) is minimised for a given n, we shall call it an optimum special sum set. The first five optimum special sum sets are given below.~%"))
    (newline)
    (display (format #f "  n = 1: {1}~%"))
    (display (format #f "  n = 2: {1, 2}~%"))
    (display (format #f "  n = 3: {2, 3, 4}~%"))
    (display (format #f "  n = 4: {3, 5, 6, 7}~%"))
    (display (format #f "  n = 5: {6, 9, 11, 12, 13}~%"))
    (newline)
    (display (format #f "It seems that for a given optimum set, A = {a1, a2, ... , an}, the next optimum set is of the form B = {b, a1+b, a2+b, ... ,an+b}, where b is the 'middle' element on the previous row.~%"))
    (newline)
    (display (format #f "By applying this 'rule' we would expect the optimum set for n = 6 to be A = {11, 17, 20, 22, 23, 24}, with S(A) = 117. However, this is not the optimum set, as we have merely applied an algorithm to provide a near optimum set. The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and corresponding set string: 111819202225.~%"))
    (newline)
    (display (format #f "Given that A is an optimum special sum set for n = 7, find its set string.~%"))
    (newline)
    (display (format #f "NOTE: This problem is related to problems 105 and 106.~%"))
    (newline)
    (display (format #f "The forward reference to problems 105 and 106 is an important clue.  Problem 105 tells you how to speed up condition 2, and problem 106 shows how to speed up condition 1 (given that condition 2 is satisfied).~%"))
    (display (format #f "A fast way to check condition 2 was found at http://www.mathblog.dk/project-euler-103-special-subset-sum/. It involves just checking the sum of the smallest 2 elements against the largest element, and checking the sum of the smallest 3 elements against the sum of the largest 2 elements.  The relation a1+a2>an was also used to reduce the largest number in the do loops.~%"))
    (display (format #f "A fast way to check condition 1 was found at http://jsomers.net/blog/pe-oeis, where only non-dominated sets of equal length need to be compared.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-are-lists-disjoint-1 counter)
           (run-test test-make-power-set-1 counter)
           (run-test test-make-incremental-powerset-1 counter)
           (run-test test-are-subset-pairs-dominant-1 counter)
           (run-test test-extract-subsets-fixed-len-1 counter)
           (run-test test-condition-1-same-size-subsets-1 counter)
           (run-test test-condition-1-ok-1 counter)
           (run-test test-condition-2-ok-1 counter)
	   (run-test test-find-mininum-special-sum-set-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((num-list-list
           (list (list 1 1 10) (list 2 1 10)
                 (list 3 1 10) (list 4 1 10))))
      (begin
	(time-code
	 (begin
	   (for-each
	    (lambda (a-list)
	      (begin
		(let ((nn-size (list-ref a-list 0))
		      (start-num (list-ref a-list 1))
		      (end-num (list-ref a-list 2)))
		  (begin
		    (main-loop nn-size start-num end-num)
		    ))
		)) num-list-list)
	   ))
	))

    (newline)
    (force-output)

    (let ((num-list-list
	   (list (list 5 1 15) (list 6 6 30) (list 7 11 50))))
      (begin
	(for-each
	 (lambda (a-list)
	   (begin
	     (time-code
	      (begin
		(let ((nn-size (list-ref a-list 0))
		      (start-num (list-ref a-list 1))
		      (end-num (list-ref a-list 2)))
		  (begin
		    (main-loop nn-size start-num end-num)
		    ))
		))
	     (newline)
	     (force-output)
	     )) num-list-list)
	))

    (newline)
    ))
