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

;;;### ice-9 receive for receive function
(use-modules ((ice-9 receive)
	      :renamer (symbol-prefix-proc 'ice9-receive:)))

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
;;; assume each list has max length of 3
(define (are-two-lists-equivalent? a-list-list b-list-list)
  (let ((a-last-elem (car (last-pair a-list-list)))
	(b-last-elem (car (last-pair b-list-list)))
	(a-len (length a-list-list))
	(b-len (length b-list-list)))
    (begin
      (if (or (not (equal? a-last-elem b-last-elem))
	      (not (equal? a-len b-len)))
	  (begin
	    #f)
	  (begin
	    (if (and (= a-len b-len)
		     (equal? a-last-elem b-last-elem))
		(begin
		  (cond
		   ((= a-len 1)
		    (begin
		      #t
		      ))
		   ((= a-len 2)
		    (begin
		      (let ((a0-elem (list-ref a-list-list 0))
			    (b0-elem (list-ref b-list-list 0)))
			(begin
			  (if (equal? a0-elem b0-elem)
			      (begin
				#t)
			      (begin
				#f
				))
			  ))
		      ))
		   ((= a-len 3)
		    (begin
		      (let ((l1 (sort a-list-list string-ci<?))
			    (l2 (sort b-list-list string-ci<?)))
			(begin
			  (equal? l1 l2)
			  ))
		      ))
		   (else
		    #f
		    )))
		(begin
		  #f
		  ))
	    ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-are-two-lists-equivalent-1)
  (let ((sub-name "test-are-two-lists-equivalent-1")
	(test-list
	 (list
	  (list (list "d2" "d1")
		(list "d1" "d2")
                #f)
	  (list (list "s1" "s2" "d3")
		(list "s2" "s1" "d3")
                #t)
	  (list (list "t1" "s2" "d3")
		(list "s2" "t1" "d3")
                #t)
	  (list (list "s2" "t1" "d2")
		(list "s2" "t2" "d3")
                #f)
	  (list (list "s4" "t1" "d3")
		(list "s2" "t1" "d3")
                #f)
	  (list (list "d1" "d1" "d1")
		(list "s2" "d1" "d1")
                #f)
	  (list (list "d1" "s2" "d1")
		(list "s2" "d1" "d1")
                #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list (list-ref this-list 0))
		 (b-list (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result
                    (are-two-lists-equivalent? a-list b-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : a-list = ~a, b-list = ~a : "
                         sub-name test-label-index a-list b-list))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
(define (is-result-in-list? result-list acc-list)
  (let ((in-list-flag #f)
	(alen (length acc-list)))
    (begin
      (do ((ii 0 (1+ ii)))
	  ((or (>= ii alen)
	       (equal? in-list-flag #t)))
	(begin
	  (let ((a-list (list-ref acc-list ii)))
	    (begin
	      (if (are-two-lists-equivalent? result-list a-list)
		  (begin
		    (set! in-list-flag #t)
		    ))
	      ))
	  ))

      in-list-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-result-in-list-1)
  (let ((sub-name "test-is-result-in-list-1")
	(test-list
	 (list
	  (list (list "s1" "s2" "d3")
		(list (list "s2" "s1" "d3"))
                #t)
	  (list (list "t1" "s1" "d3")
		(list (list "t1" "s1" "d3"))
                #t)
	  (list (list "s1" "s2" "d2")
		(list (list "t1" "s1" "d3"))
                #f)
	  (list (list "s4" "t1" "d3")
		(list (list "s2" "s1" "d3"))
                #f)
	  (list (list "d1" "d1" "d1")
		(list (list "s2" "d1" "d1"))
                #f)
	  (list (list "d1" "d1" "d1")
		(list (list "s2" "d1" "d1")
		      (list "s3" "d1" "d1"))
                #f)
	  (list (list "d1" "d1" "d1")
		(list (list "s2" "d1" "d1")
		      (list "s3" "d1" "d1")
		      (list "d1" "d1" "d1"))
                #t)
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list (list-ref this-list 0))
		 (b-list (list-ref this-list 1))
		 (shouldbe (list-ref this-list 2)))
	     (let ((result (is-result-in-list? a-list b-list)))
	       (begin
		 (if (not (equal? shouldbe result))
		     (begin
		       (display
                        (format
                         #f "~a : error (~a) : a-list = ~a, b-list = ~a : "
                         sub-name test-label-index a-list b-list))
		       (display
                        (format
                         #f "shouldbe = ~a, result = ~a~%"
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
;;; at most 3 elements for a darts check-out
(define (combine-lists a-list-list a-len b-list-list b-len)
  (define (local-list-not-in-list? ltmp acc-list-list)
    (if (equal? (member ltmp acc-list-list) #f)
        (begin
          #t)
        (begin
          #f
          )))
  (let ((acc-list-list (list)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii a-len))
        (begin
          (let ((sub-a-list (list-ref a-list-list ii)))
            (let ((sub-a-len (length sub-a-list)))
              (begin
                (if (and (< sub-a-len 3) (> sub-a-len 0))
                    (begin
                      (do ((jj 0 (1+ jj)))
                          ((>= jj b-len))
                        (begin
                          (let ((sub-b-list (list-ref b-list-list jj)))
                            (let ((sub-b-len (length sub-b-list)))
                              (begin
                                (if (<= (+ sub-a-len sub-b-len) 3)
                                    (begin
                                      (let ((ltmp
                                             (sort
                                              (append
                                               sub-b-list sub-a-list)
                                              string-ci<?)))
                                        (begin
                                          (if (local-list-not-in-list?
                                               ltmp acc-list-list)
                                              (begin
                                                (set! acc-list-list
                                                      (cons
                                                       ltmp acc-list-list))
                                                ))
                                          ))
                                      ))
                                ))
                            ))
                        ))
                    ))
              ))
          ))

      acc-list-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-combine-lists-1)
  (let ((sub-name "test-combine-lists-1")
	(test-list
	 (list
	  (list (list (list "s1")) (list (list "d1"))
		(list (list "s1" "d1")))
	  (list (list (list "d1")) (list (list "s1"))
		(list (list "s1" "d1")))
	  (list (list (list "s1" "d1")) (list (list "s1"))
		(list (list "s1" "s1" "d1")))
	  (list (list (list "d2") (list "s4") (list "s2" "d1")
                      (list "d1" "d1"))
                (list (list "s1" "d1") (list "s1" "s2"))
		(list (list "d2" "s1" "d1") (list "d2" "s1" "s2")
                      (list "s4" "s1" "d1") (list "s4" "s1" "s2")))
	  ))
	(test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((a-list-list (list-ref this-list 0))
		 (b-list-list (list-ref this-list 1))
		 (shouldbe-list-list (list-ref this-list 2)))
             (let ((alen (length a-list-list))
                   (blen (length b-list-list)))
               (let ((result-list-list
                      (combine-lists a-list-list alen b-list-list blen)))
                 (let ((slen (length shouldbe-list-list))
                       (rlen (length result-list-list))
                       (sres-list-list
                        (map (lambda (alist) (sort alist string-ci<?))
                             result-list-list))
                       (sshd-list-list
                        (map (lambda (alist) (sort alist string-ci<?))
                             shouldbe-list-list)))
                   (begin
                     (if (not (equal? slen rlen))
                         (begin
                           (display
                            (format
                             #f "~a : error (~a) : a-list = ~a, b-list = ~a : "
                             sub-name test-label-index a-list-list b-list-list))
                           (display
                            (format
                             #f "shouldbe = ~a, result = ~a : "
                             sshd-list-list sres-list-list))
                           (display
                            (format
                             #f "lengths not equal, shouldbe = ~a, result = ~a : "
                             slen rlen))
                           (quit)
                           ))
                     (for-each
                      (lambda (slist)
                        (begin
                          (if (equal? (member slist sres-list-list) #f)
                              (begin
                                (display
                                 (format
                                  #f "~a : error (~a) : a-list = ~a, b-list = ~a : "
                                  sub-name test-label-index a-list-list b-list-list))
                                (display
                                 (format
                                  #f "shouldbe = ~a, result = ~a : "
                                  sshd-list-list sres-list-list))
                                (display
                                 (format
                                  #f "missing shouldbe element ~a~%"
                                  slist))
                                (quit)
                                ))
                          )) sshd-list-list)
                     )))
               ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; makes lists independent of order, so (d1 d2) is the same as (d2 d1)
;;; later, at tally time, each d in the list will count as 1 configuration
;;; so (d1 d2) will count as 2, and (d1 d2 d3) will count as 3.
(define (dynamic-checkout-to-array
         min-score max-score dart-scores-htable)
  (let ((score-list-array (make-array (list) (1+ max-score))))
    (begin
      ;;; initialize array
      (hash-for-each
       (lambda (str-elem svalue)
         (begin
           (if (<= svalue max-score)
               (begin
                 (let ((this-list
                        (array-ref score-list-array svalue)))
                   (begin
                     (array-set!
                      score-list-array
                      (cons (list str-elem) this-list) svalue)
                     ))
                 ))
           )) dart-scores-htable)

      (do ((ii 2 (1+ ii)))
          ((> ii max-score))
        (begin
          (let ((ii-list (array-ref score-list-array ii))
                (max-jj (euclidean/ ii 2)))
            (let ((ii-len (length ii-list))
                  (ii-acc-list (list)))
              (begin
                (do ((jj 1 (1+ jj)))
                    ((> jj max-jj))
                  (begin
                    (let ((dvalue (- ii jj)))
                      (let ((jj-list (array-ref score-list-array jj))
                            (dd-list (array-ref score-list-array dvalue)))
                        (let ((jj-len (length jj-list))
                              (dd-len (length dd-list)))
                          (let ((combined-list
                                 (combine-lists
                                  jj-list jj-len dd-list dd-len)))
                            (begin
                              (set!
                               ii-acc-list
                               (append combined-list ii-acc-list))
                              ))
                          )))
                    ))

                (let ((next-acc-list (list-copy ii-list)))
                  (begin
                    (for-each
                     (lambda (a-list)
                       (begin
                         (if (equal? (member a-list next-acc-list) #f)
                             (begin
                               (set!
                                next-acc-list
                                (cons a-list next-acc-list))
                               ))
                         )) ii-acc-list)

                    (array-set! score-list-array next-acc-list ii)
                    ))
                )))
          ))

      score-list-array
      )))

;;;#############################################################
;;;#############################################################
(define (test-dynamic-checkout-to-array-1)
  (let ((sub-name "test-dynamic-checkout-to-array-1")
	(test-list
	 (list
	  (list 2 6 1 (list (list "s1")))
	  (list 2 6 2 (list (list "s1" "s1")
                            (list "d1") (list "s2")))
	  (list 2 6 3 (list (list "s1" "s1" "s1")
                            (list "s1" "d1") (list "s1" "s2")
                            (list "s3") (list "t1")))
	  (list 2 6 4 (list (list "s1" "s1" "d1")
                            (list "s1" "s1" "s2")
                            (list "s2" "s2")
                            (list "s2" "d1")
                            (list "d1" "d1")
                            (list "s1" "t1")
                            (list "s1" "s3")
                            (list "d2") (list "s4")))
	  ))
        (dart-scores-htable (make-hash-table))
	(test-label-index 0))
    (begin
      (populate-dart-score-hash-table! dart-scores-htable)

      (for-each
       (lambda (this-list)
	 (begin
	   (let ((min-score (list-ref this-list 0))
                 (max-score (list-ref this-list 1))
                 (ii (list-ref this-list 2))
		 (shouldbe-list-list (list-ref this-list 3)))
             (let ((result-list-list
                    (list-ref
                     (array->list
                      (dynamic-checkout-to-array
                       min-score max-score dart-scores-htable))
                     ii)))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length result-list-list))
                     (sres-list-list
                      (map
                       (lambda (alist)
                         (sort alist string-ci<?))
                       result-list-list))
                     (sshd-list-list
                      (map (lambda (alist)
                             (sort alist string-ci<?))
                           shouldbe-list-list)))
                 (begin
                   (if (not (equal? slen rlen))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : min = ~a, max = ~a, ii = ~a : "
                           sub-name test-label-index min-score max-score ii))
                         (display
                          (format
                           #f "shouldbe = ~a, result = ~a : "
                           sshd-list-list sres-list-list))
                         (display
                          (format
                           #f "lengths not equal, shouldbe = ~a, result = ~a~%"
                           slen rlen))
                         (quit)
                         ))
                   (for-each
                    (lambda (slist)
                      (begin
                        (if (equal? (member slist sres-list-list) #f)
                            (begin
                              (display
                               (format
                                #f "~a : error (~a) : min = ~a, max = ~a, ii = ~a : "
                                sub-name test-label-index min-score max-score ii))
                              (display
                               (format
                                #f "shouldbe = ~a, result = ~a : "
                                sshd-list-list sres-list-list))
                              (display
                               (format
                                #f "missing shouldbe element ~a~%"
                                slist))
                              (quit)
                              ))
                        )) sshd-list-list)
                   ))
               ))
           (set! test-label-index (1+ test-label-index))
           ))
      test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax down-one-more-level
  (syntax-rules ()
    ((down-one-more-level
      fixed-sum current-sum current-list
      sorted-input-list input-list-length dart-scores-htable
      acc-list-list inner-loop)
     (begin
       (let ((clen (length current-list))
	     (continue-loop-flag #t))
	 (begin
	   (if (> clen 3)
	       (begin
		 (set! continue-loop-flag #f)
		 ))

	   (do ((ii 0 (1+ ii)))
	       ((or (>= ii input-list-length)
                    (equal? continue-loop-flag #f)))
	     (begin
	       (let ((ii-label
                      (list-ref sorted-input-list ii)))
		 (let ((ii-value
                        (hash-ref dart-scores-htable ii-label)))
                   (let ((next-current-sum
                          (+ ii-value current-sum)))
                     (begin
                       (if (<= next-current-sum fixed-sum)
                           (begin
                             (let ((next-current-list
                                    (cons ii-label current-list)))
                               (let ((next-acc-list-list
                                      (inner-loop
                                       next-current-sum fixed-sum
                                       sorted-input-list input-list-length
                                       dart-scores-htable
                                       next-current-list acc-list-list)))
                                 (begin
                                   (set! acc-list-list next-acc-list-list)
                                   ))
                               ))
                           (begin
                             (set! continue-loop-flag #f)
                             ))
                       ))
                   ))
	       ))
	   ))
       ))
    ))

;;;#############################################################
;;;#############################################################
;;; sorted-input-list sorted by dart-scores (see main-loop)
(define (make-checkout-list-fixed-sum
         sorted-input-list dart-scores-htable fixed-sum)
  (define (inner-loop
           current-sum fixed-sum
           sorted-input-list input-list-length
           dart-scores-htable
           current-list acc-list-list)
    (cond
     ((= current-sum fixed-sum)
      (begin
	(let ((last-elem (car (last-pair current-list))))
	  (begin
	    (if (and (<= (length current-list) 3)
		     (equal?
                      (is-result-in-list? current-list acc-list-list)
                      #f))
		(begin
		  (cons current-list acc-list-list))
		(begin
		  acc-list-list
		  ))
	    ))
	))
     ((> current-sum fixed-sum)
      (begin
	acc-list-list
	))
     (else
      (begin
	(down-one-more-level
         fixed-sum current-sum current-list
         sorted-input-list input-list-length dart-scores-htable
         acc-list-list inner-loop)

	acc-list-list
	))
     ))
  (let ((input-list-length (length sorted-input-list))
	(acc-list (list)))
    (begin
      (for-each
       (lambda (str-elem)
	 (begin
	   (if (string-ci= str-elem "d" 0 1)
	       (begin
		 ;;; guarantee that the last throw checks out with a double
		 (let ((current-sum
                        (hash-ref dart-scores-htable str-elem 0)))
		   (let ((next-acc-list
                          (inner-loop
                           current-sum fixed-sum
                           sorted-input-list input-list-length
                           dart-scores-htable
                           (list str-elem) acc-list)))
		     (begin
		       (set! acc-list next-acc-list)
		       ))
		   )))
	   )) sorted-input-list)

      acc-list
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-checkout-list-fixed-sum-1)
  (let ((sub-name "test-make-checkout-list-fixed-sum-1")
	(test-list
	 (list
	  (list (list "d3") 6 (list (list "d3")))
	  (list (list "d1" "d2" "d3")
		6 (list (list "d3")
			(list "d2" "d1")
			(list "d1" "d2")
			(list "d1" "d1" "d1")))
	  (list (list "s1" "d2" "d3")
		6 (list (list "d3")
			(list "s1" "s1" "d2")))
	  ))
        (dart-scores-htable (make-hash-table))
	(test-label-index 0))
    (begin
      (populate-dart-score-hash-table! dart-scores-htable)

      (for-each
       (lambda (this-list)
	 (begin
	   (let ((input-list (list-ref this-list 0))
		 (fixed-sum (list-ref this-list 1))
		 (shouldbe-list-list (list-ref this-list 2)))
	     (let ((result-list-list
                    (make-checkout-list-fixed-sum
                     input-list dart-scores-htable fixed-sum)))
	       (let ((slen (length shouldbe-list-list))
		     (rlen (length result-list-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display
                          (format
                           #f "~a : error (~a) : input-list = ~a, fixed-sum = ~a : "
                           sub-name test-label-index input-list fixed-sum))
			 (display
                          (format
                           #f "shouldbe-list-list = ~a, result-list-list = ~a : "
                           shouldbe-list-list result-list-list))
			 (display
                          (format
                           #f "lengths differ, shouldbe = ~a, result = ~a~%"
                           slen rlen))
			 (quit)
			 ))
		   (for-each
		    (lambda (slist)
		      (begin
			(if (equal? (member slist result-list-list) #f)
			    (begin
                              (display
                               (format
                                #f "~a : error (~a) : input-list = ~a, fixed-sum = ~a : "
                                sub-name test-label-index input-list fixed-sum))
                              (display
                               (format
                                #f "shouldbe-list-list = ~a, result-list-list = ~a : "
                                shouldbe-list-list result-list-list))
                              (display
                               (format
                                #f "missing shouldbe element = ~a~%"
                                slist))
			      (quit)
			      ))
			)) shouldbe-list-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   )) test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (populate-dart-score-hash-table! dart-scores-htable)
  (let ((scores-list-list
	 (list
	  (list "s1" 1) (list "s2" 2) (list "s3" 3)
	  (list "s4" 4) (list "s5" 5) (list "s6" 6)
	  (list "s7" 7) (list "s8" 8) (list "s9" 9)
	  (list "s10" 10) (list "s11" 11) (list "s12" 12)
	  (list "s13" 13) (list "s14" 14) (list "s15" 15)
	  (list "s16" 16) (list "s17" 17) (list "s18" 18)
	  (list "s19" 19) (list "s20" 20)
	  (list "d1" 2) (list "d2" 4) (list "d3" 6)
	  (list "d4" 8) (list "d5" 10) (list "d6" 12)
	  (list "d7" 14) (list "d8" 16) (list "d9" 18)
	  (list "d10" 20) (list "d11" 22) (list "d12" 24)
	  (list "d13" 26) (list "d14" 28) (list "d15" 30)
	  (list "d16" 32) (list "d17" 34) (list "d18" 36)
	  (list "d19" 38) (list "d20" 40)
	  (list "t1" 3) (list "t2" 6) (list "t3" 9)
	  (list "t4" 12) (list "t5" 15) (list "t6" 18)
	  (list "t7" 21) (list "t8" 24) (list "t9" 27)
	  (list "t10" 30) (list "t11" 33) (list "t12" 36)
	  (list "t13" 39) (list "t14" 42) (list "t15" 45)
	  (list "t16" 48) (list "t17" 51) (list "t18" 54)
	  (list "t19" 57) (list "t20" 60)
	  (list "bull" 25) (list "dbull" 50)
	  )))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((astring (list-ref alist 0))
                 (avalue (list-ref alist 1)))
             (begin
               (hash-set! dart-scores-htable astring avalue)
               ))
           )) scores-list-list)
      )))

;;;#############################################################
;;;#############################################################
(define (sort-out-list-list out-list-list)
  (let ((sorted-list-list
	 (sort
          out-list-list
          (lambda (a b)
            (begin
              (let ((alen (length a))
                    (acar (car (last-pair a)))
                    (blen (length b))
                    (bcar (car (last-pair b))))
                (begin
                  (cond
                   ((= alen blen)
                    (begin
                      (string-ci>? acar bcar)
                      ))
                   (else
                    (< alen blen)
                    ))
                  ))
              ))
          )))
    (begin
      sorted-list-list
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop min-score max-score debug-flag)
  (let ((dart-scores-htable (make-hash-table))
        (dart-labels-list (list)))
    (begin
      (populate-dart-score-hash-table! dart-scores-htable)

      (hash-for-each
       (lambda (dstring dvalue)
         (begin
           (set! dart-labels-list
                 (cons dstring dart-labels-list))
           )) dart-scores-htable)

      (let ((score-sum 0)
            (acc-list-list (list))
            (sorted-dart-labels-list
             (sort
              dart-labels-list
              (lambda (astring bstring)
                (begin
                  (let ((anum (hash-ref dart-scores-htable astring))
                        (bnum (hash-ref dart-scores-htable bstring)))
                    (begin
                      (< anum bnum)
                      ))
                  ))
              )))
        (begin
          (do ((ii min-score (1+ ii)))
              ((> ii max-score))
            (begin
              (let ((out-list-list
                     (make-checkout-list-fixed-sum
                      sorted-dart-labels-list dart-scores-htable ii)))
                (let ((out-len (length out-list-list)))
                  (begin
                    (set! acc-list-list (append out-list-list acc-list-list))
                    (set! score-sum (+ score-sum out-len))

                    (if (equal? debug-flag #t)
                        (begin
                          (display
                           (ice9-format:format
                            #f "score ~:d : ~:d ways : sum so far = ~:d~%"
                            out-len ii score-sum))
                          (force-output)
                          ))
                    )))
              ))

          (if (equal? debug-flag #t)
              (begin
                (let ((sorted-list-list
                       (sort-out-list-list acc-list-list)))
                  (begin
                    (for-each
                     (lambda (a-list)
                       (begin
                         (display (format #f "    ~a~%" a-list))
                         )) sorted-list-list)
                    (force-output)
                    ))
                ))
          (display
           (ice9-format:format
            #f "~:d = number of ways to checkout with a score less than ~:d~%"
            score-sum max-score))
          (force-output)
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (dynamic-main-loop min-score max-score debug-flag)
  (define (local-count-ds a-sub-list count-htable)
    (let ((count 0))
      (begin
        (hash-clear! count-htable)
        ;;; increment for every d found
        (for-each
         (lambda (a-str)
           (begin
             (if (string-ci= a-str "d" 0 1)
                 (begin
                   (set! count (1+ count))
                   (let ((hc (hash-ref count-htable a-str 0)))
                     (begin
                       (hash-set! count-htable a-str (1+ hc))
                       ))
                   ))
             )) a-sub-list)

        ;;; if there are multiples of the same d, then subtract
        (hash-for-each
         (lambda (a-str hc)
           (begin
             (cond
              ((= hc 2)
               (begin
                 (set! count (1- count))
                 ))
              ((= hc 3)
               (begin
                 (set! count (- count 2))
                 )))
             )) count-htable)

        count
        )))
  (let ((dart-scores-htable (make-hash-table))
        (count-htable (make-hash-table))
        (score-sum 0)
        (acc-list-list (list)))
    (begin
      (populate-dart-score-hash-table! dart-scores-htable)

      (let ((results-array
             (dynamic-checkout-to-array
              min-score max-score dart-scores-htable)))
        (begin
          (do ((ii min-score (1+ ii)))
              ((> ii max-score))
            (begin
              (let ((out-list-list
                     (array-ref results-array ii))
                    (ii-count 0))
                (begin
                  (for-each
                   (lambda (a-list)
                     (begin
                       (let ((ds-count
                              (local-count-ds a-list count-htable)))
                         (begin
                           (set! score-sum (+ score-sum ds-count))
                           ))
                       )) out-list-list)

                  (if (equal? debug-flag #t)
                      (begin
                        (display
                         (ice9-format:format
                          #f "score ~:d : ~:d ways : sum so far = ~:d~%"
                          ii ii-count score-sum))
                        (force-output)
                        ))
                  ))
              ))

          (display
           (ice9-format:format
            #f "~:d = number of ways to checkout with a score between ~:d and ~:d~%"
            score-sum min-score max-score))
          (force-output)
          ))
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
    (display (format #f "Project Euler 109 - In the game of darts a player throws three darts at a target board which is split into twenty equal sized sections numbered one to twenty.~%"))
    (newline)
    (display (format #f "The score of a dart is determined by the number of the region that the dart lands in. A dart landing outside the red/green outer ring scores zero. The black and cream regions inside this ring represent single scores. However, the red/green outer ring and middle ring score double and treble scores respectively.~%"))
    (newline)
    (display (format #f "At the centre of the board are two concentric circles called the bull region, or bulls-eye. The outer bull is worth 25 points and the inner bull is a double, worth 50 points.~%"))
    (newline)
    (display (format #f "There are many variations of rules but in the most popular game the players will begin with a score 301 or 501 and the first player to reduce their running total to zero is a winner. However, it is normal to play a 'doubles out' system, which means that the player must land a double (including the double bulls-eye at the centre of the board) on their final dart to win; any other dart that would reduce their running total to one or lower means the score for that set of three darts is 'bust'.~%"))
    (newline)
    (display (format #f "When a player is able to finish on their current score it is called a 'checkout' and the highest checkout is 170: T20 T20 D25 (two treble 20s and double bull).~%"))
    (newline)
    (display (format #f "There are exactly eleven distinct ways to checkout on a score of 6:~%"))
    (display (format #f "  D3~%"))
    (display (format #f "  D1	D2~%"))
    (display (format #f "  S2	D2~%"))
    (display (format #f "  D2	D1~%"))
    (display (format #f "  S4	D1~%"))
    (display (format #f "  S1	S1	D2~%"))
    (display (format #f "  S1	T1	D1~%"))
    (display (format #f "  S1	S3	D1~%"))
    (display (format #f "  D1	D1	D1~%"))
    (display (format #f "  D1	S2	D1~%"))
    (display (format #f "  S2	S2	D1~%"))
    (newline)
    (display (format #f "Note that D1 D2 is considered different to D2 D1 as they finish on different doubles. However, the combination S1 T1 D1 is considered the same as T1 S1 D1.~%"))
    (newline)
    (display (format #f "In addition we shall not include misses in considering combinations; for example, D3 is the same as 0 D3 and 0 0 D3.~%"))
    (newline)
    (display (format #f "Incredibly there are 42336 distinct ways of checking out in total.~%"))
    (newline)
    (display (format #f "How many distinct ways can a player checkout with a score less than 100?~%"))
    (newline)
    (display (format #f "Dynamic programming was used to compute the number of ways of checking out in darts.  An array stores valid configurations of dart throws which add up to a specific number (which equals the array index).  Order of the throws was not preserved, only at the end during the tally phase, were the special cases like (D1 D2) =/= (D2 D1) or (D1 D1) = (D1 D1), were taken into account.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-are-two-lists-equivalent-1 counter)
	   (run-test test-is-result-in-list-1 counter)
           (run-test test-combine-lists-1 counter)
           (run-test test-dynamic-checkout-to-array-1 counter)
	   (run-test test-make-checkout-list-fixed-sum-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((min-score 6)
          (max-score 6)
          (debug-flag #t))
      (begin
        (time-code
         (begin
           ;;; recursive technique
           (main-loop min-score max-score debug-flag)
           ))
        (time-code
         (begin
           ;;; dynamic programming technique
           (dynamic-main-loop min-score max-score debug-flag)
           ))
        ))

    (newline)
    (force-output)

    (let ((min-score 2)
          (max-score 99)
	  (debug-flag #f))
      (begin
        (time-code
         (begin
           ;;; dynamic programming technique
           (dynamic-main-loop min-score max-score debug-flag)
           ))
	))

    (newline)
    ))
