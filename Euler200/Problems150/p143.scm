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
;;;###  project euler 143                             ###
;;;###                                                ###
;;;###  last updated December 24, 2012                ###
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
;;;### uses the algorithm found at http://stackoverflow.com/questions/295579/fastest-way-to-determine-if-an-integers-square-root-is-an-integer
;;;### (logand 1^2 15) = 1, (logand 2^2 15) = 4, (logand 3^2 15) 9,
;;;### (logand 4^2 15) = 0, (logand 5^2 15) = 9, (logand 6^2 15) = 4,
;;;### (logand 7^2 15) = 1, (logand 8^2 15) = 0, (logand 9^2 15) = 1,
;;;### (logand 10^2 15) = 4, (logand 11^2 15) = 9, (logand 12^2 15) = 0,...
(define (is-perfect-square? anum)
  (let ((lan (logand anum 15)))
    (begin
      (cond
       ((< anum 0)
        (begin
          #f
          ))
       ((or (equal? lan 0) (equal? lan 1)
            (equal? lan 4) (equal? lan 9))
        (begin
          (let ((ltmp (exact-integer-sqrt anum)))
            (begin
              (equal? (* ltmp ltmp) anum)
              ))
          ))
       (else
        (begin
          #f
          )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-is-perfect-square-1)
  (let ((sub-name "test-is-perfect-square-1")
	(test-list
	 (list
          (list -10 #f) (list 0 #t) (list 1 #t)
	  (list 2 #f) (list 3 #f) (list 4 #t) (list 5 #f)
          (list 6 #f) (list 7 #f) (list 8 #f) (list 9 #t)
          (list 10 #f) (list 11 #f) (list 12 #f) (list 13 #f)
          (list 14 #f) (list 15 #f) (list 16 #t) (list 17 #f)
          (list 18 #f) (list 19 #f) (list 20 #f) (list 21 #f)
          (list 22 #f) (list 23 #f) (list 24 #f) (list 25 #t)
          (list 26 #f) (list 27 #f) (list 28 #f) (list 29 #f)
          (list 30 #f) (list 31 #f) (list 32 #f) (list 33 #f)
          (list 36 #t) (list 49 #t) (list 64 #t) (list 81 #t)
          (list 100 #t) (list 121 #t) (list 144 #t) (list 169 #t)
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((nn (list-ref this-list 0))
		 (shouldbe-bool (list-ref this-list 1)))
	     (let ((result-bool (is-perfect-square? nn)))
	       (begin
		 (if (not (equal? shouldbe-bool result-bool))
		     (begin
		       (display
                        (format #f "~a : (~a) : error : nn=~a, "
                                sub-name test-label-index nn))
		       (display
                        (format #f "shouldbe=~a, result=~a~%"
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
(define (generate-pairs-list max-num)
  (let ((max-nn (1+ (exact-integer-sqrt max-num)))
	(result-list (list)))
    (begin
      (do ((nn 1 (1+ nn)))
	  ((> nn max-nn))
	(begin
	  (let ((nn-2 (* nn nn)))
	    (begin
	      (do ((mm (1+ nn) (1+ mm)))
		  ((>= mm max-nn))
		(begin
                  (if (and
                       (= (gcd mm nn) 1)
                       (not (zero? (modulo (- mm nn) 3))))
                      (begin
                        (let ((mn (* mm nn))
                              (mm-2 (* mm mm)))
                          (let ((aa (+ (* 2 mn) nn-2))
                                (bb (- mm-2 nn-2))
                                (cc (+ mm-2 mn nn-2)))
                            (let ((aa-2 (* aa aa))
                                  (bb-2 (* bb bb))
                                  (aabb (* aa bb)))
                              (begin
                                (if (is-perfect-square?
                                     (+ aa-2 bb-2 aabb))
                                    (begin
                                      (let ((tmp-ab (+ aa bb))
                                            (klen (+ aa bb))
                                            (tmp-aabb
                                             (+ aa-2 bb-2 aabb)))
                                        (begin
                                          (if (< aa bb)
                                              (begin
                                                (let ((tmp bb))
                                                  (begin
                                                    (set! bb aa)
                                                    (set! aa tmp)
                                                    ))
                                                ))
                                          (do ((kk 1 (1+ kk)))
                                              ((> klen max-num))
                                            (begin
                                              (let ((kaa (* kk aa))
                                                    (kbb (* kk bb))
                                                    (tmp-klen (* kk tmp-ab))
                                                    (ksqr (* kk kk tmp-aabb)))
                                                (let ((t1 (list kbb kaa)))
                                                  (begin
                                                    (if (and (< kaa max-num)
                                                             (< kbb max-num))
                                                        (begin
                                                          (set! result-list
                                                                (cons t1 result-list))
                                                          ))

                                                    (set! klen tmp-klen)
                                                    )))
                                              ))
                                          ))
                                      ))
                                ))
                            ))
                        ))
		  ))
	      ))
	  ))

      (sort
       result-list
       (lambda (a b)
         (begin
           (let ((a-1 (car a))
                 (b-1 (car b)))
             (begin
               (cond
                ((= a-1 b-1)
                 (begin
                   (< (cadr a) (cadr b))
                   ))
                (else
                 (begin
                   (< a-1 b-1)
                   )))
               ))
           )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-generate-pairs-list-1)
  (let ((sub-name "test-generate-pairs-list-1")
	(test-list
	 (list
	  (list 10 (list (list 3 5)))
          (list 20 (list (list 3 5) (list 6 10) (list 7 8)
                         (list 9 15) (list 14 16)))
          ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((max-num (list-ref this-list 0))
		 (shouldbe-list-list (list-ref this-list 1)))
	     (let ((result-list-list
                    (generate-pairs-list max-num)))
	       (let ((slen (length shouldbe-list-list))
		     (rlen (length result-list-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display
                          (format #f "~a : (~a) : error : max-num=~a, "
                                  sub-name test-label-index max-num))
			 (display
                          (format #f "shouldbe=~a, result=~a, length discrepancy, "
                                  shouldbe-list-list result-list-list))
			 (display
                          (format #f "shouldbe=~a, result=~a~%"
                                  slen rlen))
			 (set! ok-flag #f)
			 ))
		   (for-each
		    (lambda (slist)
		      (begin
			(if (equal? (member slist result-list-list) #f)
			    (begin
			      (display
                               (format #f "~a : (~a) : error : max-num=~a, "
                                       sub-name test-label-index max-num))
			      (display
                               (format #f "shouldbe=~a, result=~a, "
                                       shouldbe-list-list result-list-list))
			      (display
                               (format #f "shouldbe element missing=~a~%"
                                       slist))
			      (set! ok-flag #f)
			      ))
			)) shouldbe-list-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define-syntax display-status-info
  (syntax-rules ()
    ((display-status-info ii max-ii count start-jday)
     (begin
       (let ((end-jday (srfi-19:current-julian-day)))
	 (begin
	   (display
            (ice-9-format:format #f "~:d / ~:d : count = ~:d : "
                                 ii max-ii count))
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
(define (extract-all-first-items pairs-list)
  (let ((result-list (list))
        (plen (length pairs-list)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii plen))
        (begin
          (let ((this-pair (list-ref pairs-list ii)))
            (let ((this-elem (car this-pair)))
              (begin
                (set! result-list (cons this-elem result-list))
                )))
          ))

      (srfi-1:delete-duplicates result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-extract-all-first-items-1)
  (let ((sub-name "test-extract-all-first-items-1")
	(test-list
	 (list
	  (list (list (list 3 5) (list 1 2) (list 2 3) (list 4 3))
                (list 1 2 3 4))
	  (list (list (list 3 5) (list 1 2) (list 2 3) (list 1 7))
                (list 1 2 3))
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((pairs-list-list (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-list
                    (extract-all-first-items pairs-list-list)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display
                          (format #f "~a : (~a) : error : pairs-list=~a, "
                                  sub-name test-label-index pairs-list))
			 (display
                          (format #f "shouldbe=~a, result=~a, length discrepancy, "
                                  shouldbe-list result-list))
			 (display
                          (format #f "shouldbe=~a, result=~a~%"
                                  slen rlen))
			 (set! ok-flag #f)
			 ))
		   (for-each
		    (lambda (s-elem)
		      (begin
			(if (equal? (member s-elem result-list) #f)
			    (begin
			      (display
                               (format #f "~a : (~a) : error : pairs-list=~a, "
                                       sub-name test-label-index pairs-list))
			      (display
                               (format #f "shouldbe=~a, result=~a, "
                                       shouldbe-list result-list))
			      (display
                               (format #f "shouldbe element missing=~a~%"
                                       s-elem))
			      (set! ok-flag #f)
			      ))
			)) shouldbe-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (extract-all-second-items pairs-list)
  (let ((result-list (list))
        (plen (length pairs-list)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii plen))
        (begin
          (let ((this-pair (list-ref pairs-list ii)))
            (let ((this-elem (cadr this-pair)))
              (begin
                (set! result-list (cons this-elem result-list))
                )))
          ))

      (srfi-1:delete-duplicates result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-extract-all-second-items-1)
  (let ((sub-name "test-extract-all-second-items-1")
	(test-list
	 (list
	  (list (list (list 3 5) (list 1 2) (list 2 3) (list 4 3))
                (list 2 3 5))
	  (list (list (list 3 5) (list 1 2) (list 2 3) (list 4 7))
                (list 2 3 5 7))
	  ))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (this-list)
	 (begin
	   (let ((pairs-list-list (list-ref this-list 0))
		 (shouldbe-list (list-ref this-list 1)))
	     (let ((result-list
                    (extract-all-second-items pairs-list-list)))
	       (let ((slen (length shouldbe-list))
		     (rlen (length result-list)))
		 (begin
		   (if (not (equal? slen rlen))
		       (begin
			 (display
                          (format #f "~a : (~a) : error : pairs-list=~a, "
                                  sub-name test-label-index pairs-list))
			 (display
                          (format #f "shouldbe=~a, result=~a, length discrepancy, "
                                  shouldbe-list result-list))
			 (display
                          (format #f "shouldbe=~a, result=~a~%"
                                  slen rlen))
			 (set! ok-flag #f)
			 ))
		   (for-each
		    (lambda (s-elem)
		      (begin
			(if (equal? (member s-elem result-list) #f)
			    (begin
			      (display
                               (format #f "~a : (~a) : error : pairs-list=~a, "
                                       sub-name test-label-index pairs-list))
			      (display
                               (format #f "shouldbe=~a, result=~a, "
                                       shouldbe-list result-list))
			      (display
                               (format #f "shouldbe element missing=~a~%"
                                       s-elem))
			      (set! ok-flag #f)
			      ))
			)) shouldbe-list)
		   ))
	       ))
	   (set! test-label-index (1+ test-label-index))
	   ))
       test-list)

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (populate-index-hash! index-htable pairs-list)
  (let ((plen (length pairs-list)))
    (begin
      (hash-clear! index-htable)

      (do ((ii 0 (1+ ii)))
          ((>= ii plen))
        (begin
          (let ((alist (list-ref pairs-list ii)))
            (let ((pp (car alist)))
              (let ((hindex (hash-ref index-htable pp -1)))
                (begin
                  (if (< hindex 0)
                      (begin
                        (hash-set! index-htable pp ii)
                        ))
                  ))
              ))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-index-hash-1)
  (let ((sub-name "test-populate-index-hash-1")
        (pairs-list
         (list
          (list 3 4) (list 3 5) (list 3 6) (list 5 5)
          (list 5 7) (list 5 9) (list 5 10) (list 7 11)
          (list 7 12) (list 7 13) (list 7 14) (list 8 88)))
	(test-list
	 (list
	  (list 3 0) (list 5 3) (list 7 7)
          (list 8 11)
	  ))
        (index-htable (make-hash-table 10))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (populate-index-hash! index-htable pairs-list)

      (for-each
       (lambda (a-list)
	 (begin
	   (let ((a-num (list-ref a-list 0))
		 (shouldbe-index (list-ref a-list 1)))
	     (let ((result-index
                    (hash-ref index-htable a-num -1)))
               (begin
                 (if (not (equal? shouldbe-index result-index))
                     (begin
                       (display
                        (format #f "~a : (~a) : error : pairs-list=~a : "
                                sub-name test-label-index pairs-list))
                       (display
                        (format #f "a-num=~a, shouldbe=~a, result=~a"
                                a-num shouldbe-index result-index))
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
(define (extract-all-pairs-first-equals pp pairs-array psize
                                        index-htable)
  (let ((result-list (list))
        (start-index (hash-ref index-htable pp 0))
        (break-flag #f)
        (last-elem pp))
    (begin
      (do ((ii start-index (1+ ii)))
          ((or (>= ii psize)
               (equal? break-flag #t)))
        (begin
          (let ((this-pair (array-ref pairs-array ii)))
            (let ((this-elem (car this-pair)))
              (begin
                (if (= this-elem last-elem)
                    (begin
                      (set! result-list (cons this-pair result-list)))
                    (begin
                      (set! break-flag #t)
                      ))

                (set! this-elem last-elem)
                )))
          ))
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-extract-all-pairs-first-equals-1)
  (let ((sub-name "test-extract-all-pairs-first-equals-1")
        (pairs-list
         (list
          (list 3 5) (list 3 7) (list 3 9) (list 3 10)
          (list 5 5) (list 7 7) (list 8 8) (list 8 9)))
	(test-list
	 (list
	  (list 3 (list (list 3 5) (list 3 7)
                        (list 3 9) (list 3 10)))
	  (list 5 (list (list 5 5)))
	  (list 7 (list (list 7 7)))
	  (list 8 (list (list 8 8) (list 8 9)))
	  ))
        (index-htable (make-hash-table 10))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (populate-index-hash! index-htable pairs-list)

      (let ((parray (list->array 1 pairs-list))
            (plen (length pairs-list)))
        (begin
          (for-each
           (lambda (this-list)
             (begin
               (let ((pp (list-ref this-list 0))
                     (shouldbe-list-list (list-ref this-list 1)))
                 (let ((result-list-list
                        (extract-all-pairs-first-equals
                         pp parray plen index-htable)))
                   (let ((slen (length shouldbe-list-list))
                         (rlen (length result-list-list)))
                     (begin
                       (if (not (equal? slen rlen))
                           (begin
                             (display
                              (format #f "~a : (~a) : error : pp=~a, "
                                      sub-name test-label-index pp))
                             (display
                              (format #f "shouldbe=~a, result=~a, length discrepancy, "
                                      shouldbe-list-list result-list-list))
                             (display
                              (format #f "shouldbe=~a, result=~a~%"
                                      slen rlen))
                             (set! ok-flag #f)
                             ))
                       (for-each
                        (lambda (slist)
                          (begin
                            (if (equal? (member slist result-list-list) #f)
                                (begin
                                  (display
                                   (format #f "~a : (~a) : error : pp=~a, "
                                           sub-name test-label-index pp))
                                  (display
                                   (format #f "shouldbe=~a, result=~a, "
                                           shouldbe-list-list result-list-list))
                                  (display
                                   (format #f "shouldbe element missing=~a~%"
                                           slist))
                                  (set! ok-flag #f)
                                  ))
                            )) shouldbe-list-list)
                       ))
                   ))
               (set! test-label-index (1+ test-label-index))
               ))
           test-list)

          ok-flag
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (pairs-to-triples pp pairs-array psize
                          index-htable)
  (let ((result-list (list)))
    (let ((first-list
           (extract-all-pairs-first-equals
            pp pairs-array psize index-htable)))
      (let ((first-elements-list
             (extract-all-second-items first-list)))
        (begin
          (for-each
           (lambda (qq)
             (begin
               (if (not (= pp qq))
                   (begin
                     (let ((second-list
                            (extract-all-pairs-first-equals
                             qq pairs-array psize index-htable)))
                       (let ((second-elements-list
                              (extract-all-second-items
                               second-list)))
                         (begin
                           (for-each
                            (lambda (rr)
                              (begin
                                (if (not
                                     (equal?
                                      (member
                                       rr first-elements-list)
                                      #f))
                                    (begin
                                      (if (and
                                           (not (= pp rr))
                                           (not (= qq rr)))
                                          (begin
                                            (set! result-list
                                                  (cons (list pp qq rr)
                                                        result-list))
                                            ))
                                      ))
                                )) second-elements-list)
                           )))
                     ))
               )) first-elements-list)

          (reverse result-list)
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-pairs-to-triples-1)
  (let ((sub-name "test-pairs-to-triples-1")
        (pairs-list
         (list
          (list 3 5) (list 3 7) (list 3 9) (list 3 10)
          (list 5 6) (list 5 7) (list 7 13)
          (list 8 9) (list 8 10) (list 8 11) (list 9 10)
          (list 10 11)))
	(test-list
	 (list
	  (list 3 (list (list 3 5 7) (list 3 9 10)))
	  (list 5 (list))
	  (list 7 (list))
          (list 8 (list (list 8 9 10) (list 8 10 11)))
	  ))
        (index-htable (make-hash-table 10))
	(test-label-index 0)
        (ok-flag #t))
    (begin
      (populate-index-hash! index-htable pairs-list)

      (let ((parray (list->array 1 pairs-list))
            (plen (length pairs-list)))
        (begin
          (for-each
           (lambda (this-list)
             (begin
               (let ((pp (list-ref this-list 0))
                     (shouldbe-list-list (list-ref this-list 1)))
                 (let ((result-list-list
                        (pairs-to-triples pp parray plen index-htable)))
                   (let ((slen (length shouldbe-list-list))
                         (rlen (length result-list-list)))
                     (begin
                       (if (not (equal? slen rlen))
                           (begin
                             (display
                              (format #f "~a : (~a) : error : pp=~a, "
                                      sub-name test-label-index pp))
                             (display
                              (format #f "shouldbe=~a, result=~a, "
                                      shouldbe-list-list result-list-list))
                             (display
                              (format #f "length discrepancy : shouldbe=~a, result=~a~%"
                                      slen rlen))
                             (set! ok-flag #f)
                             ))

                       (for-each
                        (lambda (s-list)
                          (begin
                            (if (equal? (member s-list result-list-list) #f)
                                (begin
                                  (display
                                   (format #f "~a : (~a) : error : pp=~a, "
                                           sub-name test-label-index pp))
                                  (display
                                   (format #f "shouldbe=~a, result=~a, "
                                           shouldbe-list-list result-list-list))
                                  (display
                                   (format #f "shouldbe element missing=~a~%"
                                           s-list))
                                  (set! ok-flag #f)
                                  ))
                            )) shouldbe-list-list)
                       ))
                   ))
               (set! test-label-index (1+ test-label-index))
               ))
           test-list)

          ok-flag
          ))
      )))

;;;#############################################################
;;;#############################################################
(define-syntax display-debug-info
  (syntax-rules ()
    ((display-debug-info pp qq rr this-sum sum count max-num)
     (begin
       (display
        (ice-9-format:format
         #f "  (pp, qq, rr) = (~:d, ~:d, ~:d), " pp qq rr))
       (let ((aa-2 (+ (* pp pp) (* qq qq) (* pp qq)))
             (bb-2 (+ (* pp pp) (* rr rr) (* pp rr)))
             (cc-2 (+ (* qq qq) (* rr rr) (* qq rr))))
         (let ((aa (exact-integer-sqrt aa-2))
               (bb (exact-integer-sqrt bb-2))
               (cc (exact-integer-sqrt cc-2)))
           (begin
             (display
              (ice-9-format:format
               #f "(aa, bb, cc) = (~:d~a, ~:d~a, ~:d~a) : "
               aa (if (= (* aa aa) aa-2) "" "***error***")
               bb (if (= (* bb bb) bb-2) "" "***error***")
               cc (if (= (* cc cc) cc-2) "" "***error***")))
             )))

       (let ((ptmp (+ pp qq rr)))
         (begin
           (display
            (ice-9-format:format
             #f "this-sum = ~:d~a, sum = ~:d, count=~:d~%"
             this-sum (if (> ptmp max-num) "***error***" "")
             sum count))
           ))

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
(define (main-loop max-num debug-flag)
  (let ((sum-abc 0)
	(count 0)
	(start-jday (srfi-19:current-julian-day))
        (index-htable (make-hash-table 100))
        (dup-htable (make-hash-table 100))
	(pairs-list #f))
    (begin
      (time-code
       (begin
         (let ((plist (generate-pairs-list max-num)))
           (begin
             ;;; generate-pairs-list sorts resulting list
             (set! pairs-list plist)
             ))
         (display
          (ice-9-format:format
           #f "completed generating pairs-list (~:d) : "
           (length pairs-list)))
         (force-output)
         ))

      (let ((first-list
             (extract-all-first-items pairs-list))
            (plen (length pairs-list))
            (pairs-array (list->array 1 pairs-list))
            (result-list (list)))
        (begin
          (populate-index-hash! index-htable pairs-list)

          (for-each
           (lambda (pp)
             (begin
               (let ((triples-list
                      (pairs-to-triples
                       pp pairs-array plen index-htable)))
                 (begin
                   (for-each
                    (lambda (alist)
                      (begin
                        (let ((pp (list-ref alist 0))
                              (qq (list-ref alist 1))
                              (rr (list-ref alist 2)))
                          (let ((sum (+ pp qq rr)))
                            (begin
                              (if (<= sum max-num)
                                  (begin
                                    (let ((hcount (hash-ref dup-htable sum 0)))
                                      (begin
                                        (if (= hcount 0)
                                            (begin
                                              (set! sum-abc (+ sum-abc sum))
                                              (set! count (1+ count))

                                              (if (equal? debug-flag #t)
                                                  (begin
                                                    (display-debug-info
                                                     pp qq rr sum sum-abc
                                                     count max-num)
                                                    ))
                                              ))
                                        (hash-set! dup-htable sum (1+ hcount))
                                        ))
                                    ))
                              )))
                        )) triples-list)
                   ))
               )) first-list)

          (display
           (ice-9-format:format
            #f "Sum = ~:d, number of Torricelli triangles = ~:d, (p+q+r<=~:d)~%"
            sum-abc count max-num))
          (force-output)
          ))
      )))

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

          (display (format #f "=====================================================~%"))
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
          (display (format #f "=====================================================~%"))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (main args)
  (begin
    (display (format #f "Project Euler 143 - Let ABC be a triangle with all interior angles being less than 120 degrees. Let X be any point inside the triangle and let XA = p, XB = q, and XC = r.~%"))
    (newline)
    (display (format #f "Fermat challenged Torricelli to find the position of X such that p + q + r was minimised.~%"))
    (newline)
    (display (format #f "Torricelli was able to prove that if equilateral triangles AOB, BNC and AMC are constructed on each side of triangle ABC, the circumscribed circles of AOB, BNC, and AMC will intersect at a single point, T, inside the triangle. Moreover he proved that T, called the Torricelli/Fermat point, minimises p + q + r. Even more remarkable, it can be shown that when the sum is minimised, AN = BM = CO = p + q + r and that AN, BM and CO also intersect at T.~%"))
    (newline)
    (display (format #f "If the sum is minimised and a, b, c, p, q and r are all positive integers we shall call triangle ABC a Torricelli triangle. For example, a = 399, b = 455, c = 511 is an example of a Torricelli triangle, with p + q + r = 784.~%"))
    (newline)
    (display (format #f "Find the sum of all distinct values of p + q + r <= 120000 for Torricelli triangles.~%"))
    (newline)
    (display (format #f "Note: This problem has been changed recently, please check that you are using the right parameters.~%"))
    (newline)
    (display (format #f "The solution, which includes a terrific explanation, was found at http://luckytoilet.wordpress.com/2010/08/25/fermat-points-and-parameterizing-the-120-degree-integer-triangle-project-euler-143/~%"))
    (newline)
    (display (format #f "To solve this problem one makes use of the fact that all three angles of the Torricelli point are 120 degrees, and so a^2 = q^2+r^2-2rq*cos(120)=q^2+r^2+rq, b^2=p^2+r^2+pr, c^2=p^2+q^2+pq~%"))
    (newline)
    (display (format #f "All pairs (a, b) that satisfy a^2+b^2+ab = square are stored in a list.  The algorithm then looks for all pairs where (p, q), (p, r), and (q, r) exist, then we have found the desired solution.~%"))
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

    (let ((max-num 120000)
	  (debug-flag #f))
      (begin
	(time-code
	 (begin
	   (main-loop max-num debug-flag)
	   ))
	))

    (newline)
    ))
