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
	  )))))
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
(define-syntax turn-digit-list-to-number
  (syntax-rules ()
    ((turn-digit-list-to-number dlist)
     (begin
       (let ((this-num
              (srfi-1:fold
               (lambda (this-elem prev-elem)
                 (+ this-elem (* 10 prev-elem)))
               0 dlist)))
         (begin
           this-num
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-turn-digit-list-to-number-1)
  (let ((sub-name "test-turn-digit-list-to-number-1")
        (test-list
         (list
          (list (list 1 2) 12)
          (list (list 2 1) 21)
          (list (list 1 2 3) 123)
          (list (list 3 2 1) 321)
          (list (list 1 2 3 4) 1234)
          (list (list 4 3 2 1) 4321)
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((test-list (list-ref alist 0))
                 (shouldbe-num (list-ref alist 1)))
             (let ((result-num (turn-digit-list-to-number test-list)))
               (begin
                 (if (not (equal? shouldbe-num result-num))
                     (begin
                       (display (format #f "~a : (~a) : error : list = ~a, shouldbe = ~a, result = ~a~%"
                                        sub-name test-label-index test-list
                                        shouldbe-num result-num))
                       (quit)
                       ))
                 )))
           (set! test-label-index (1+ test-label-index))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; result list of the form (list (list num num-cubed digit-list) ...
(define (populate-cube-hashes! count-htable num-lists-htable max-num)
  (begin
    (hash-clear! count-htable)
    (hash-clear! num-lists-htable)

    (do ((ii 1 (1+ ii)))
        ((> ii max-num))
      (begin
        (let ((cubed-num (* ii ii ii)))
          (let ((dlist (split-digits-list cubed-num)))
            (let ((slist (sort dlist >)))
              (let ((snum (turn-digit-list-to-number slist)))
                (let ((c-count (hash-ref count-htable snum 0))
                      (this-data (list ii cubed-num))
                      (data-list
                       (hash-ref num-lists-htable snum (list))))
                  (begin
                    (hash-set! count-htable snum (1+ c-count))
                    (hash-set!
                     num-lists-htable
                     snum (cons this-data data-list))
                    ))
                ))
            ))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-populate-cube-hashes-1)
  (let ((sub-name "test-populate-cube-hashes-1")
        (test-list
         (list
          (list 1 1 (list (list 1 1)))
          (list 8 1 (list (list 2 8)))
          (list 72 1 (list (list 3 27)))
          (list 64 1 (list (list 4 64)))
          ))
        (count-htable (make-hash-table 10))
        (num-lists-htable (make-hash-table 10))
        (max-num 4)
        (test-label-index 0))
    (begin
      (populate-cube-hashes! count-htable num-lists-htable max-num)

      (for-each
       (lambda (alist)
         (begin
           (let ((sorted-num (list-ref alist 0))
                 (shouldbe-count (list-ref alist 1))
                 (shouldbe-list-list (list-ref alist 2)))
             (let ((result-count (hash-ref count-htable sorted-num 0))
                   (result-list-list
                    (hash-ref num-lists-htable sorted-num (list))))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length result-list-list)))
                 (begin
                   (if (not (equal? slen rlen))
                       (begin
                         (display
                          (format #f "~a : (~a) : error : sorted num = ~a : "
                                  sub-name test-label-index sorted-num))
                         (display
                          (format #f "shouldbe = ~a, result = ~a : "
                                  shouldbe-list-list result-list-list))
                         (display
                          (format
                           #f "length discrepancy, shouldbe = ~a, result = ~a~%"
                           slen rlen))
                         (quit)
                         ))
                   (for-each
                    (lambda (slist)
                      (begin
                        (if (equal? (member slist result-list-list) #f)
                            (begin
                              (display
                               (format #f "~a : (~a) : error : sorted num = ~a : "
                                       sub-name test-label-index sorted-num))
                              (display
                               (format #f "shouldbe = ~a, result = ~a : "
                                       shouldbe-list-list result-list-list))
                              (display
                               (format #f "missing shouldbe element ~a~%"
                                       slist))
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
(define (main-loop max-num num-perm)
  (let ((count-htable (make-hash-table 1000))
        (num-lists-htable (make-hash-table 1000)))
    (begin
      (populate-cube-hashes! count-htable num-lists-htable max-num)

      (let ((key-list (list)))
        (begin
          (hash-for-each
           (lambda (key count)
             (begin
               (if (= count num-perm)
                   (begin
                     (set! key-list (cons key key-list))
                     ))
               )) count-htable)

          (let ((smallest-cube -1)
                (smallest-num -1)
                (smallest-cubes-list (list)))
            (begin
              (for-each
               (lambda (snum)
                 (begin
                   (let ((scube-list-list
                          (hash-ref num-lists-htable snum (list)))
                         (min-cube -1)
                         (min-num -1))
                     (begin
                       (for-each
                        (lambda (alist)
                          (begin
                            (let ((anum (list-ref alist 0))
                                  (acube (list-ref alist 1)))
                              (begin
                                (if (or (< min-cube 0)
                                        (< acube min-cube))
                                    (begin
                                      (set! min-cube acube)
                                      (set! min-num anum)
                                      ))
                                ))
                            )) scube-list-list)
                       (if (or (< smallest-cube 0)
                               (< min-cube smallest-cube))
                           (begin
                             (set! smallest-cube min-cube)
                             (set! smallest-num min-num)
                             (set! smallest-cubes-list scube-list-list)
                             ))
                       ))
                   )) key-list)

              (if (> smallest-cube 0)
                  (begin
                    (display
                     (ice9-format:format
                      #f "the smallest cube is ~:d^3 = ~:d with exactly ~:d permutations~%"
                      smallest-num smallest-cube num-perm))

                    (for-each
                     (lambda (this-list)
                       (begin
                         (let ((anum (list-ref this-list 0))
                               (acube (list-ref this-list 1)))
                           (begin
                             (display
                              (ice9-format:format
                               #f "  ~:d^3 = ~:d~%" anum acube))
                             ))
                         )) (sort
                             smallest-cubes-list
                             (lambda (a b)
                               (< (car a) (car b)))))

                    (newline)
                    (force-output))
                  (begin
                    (display
                     (ice9-format:format
                      #f "no ~:d permutations found (less than ~:d)~%"
                      num-perm max-num))
                    (force-output)
                    ))
              ))
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
    (display (format #f "Problem 062 - The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.~%"))
    (newline)
    (display (format #f "Find the smallest cube for which exactly five permutations of its digits are cube.~%"))
    (newline)
    (display (format #f "This program uses a hash table to collect all cubes which have the same digits, then sorts through which elements have the required number of permutations.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
	(time-code
	 (begin
	   (run-test test-split-digits-list-1 counter)
	   (run-test test-turn-digit-list-to-number-1 counter)
	   (run-test test-populate-cube-hashes-1 counter)

	   (display (ice9-format:format #f "~:d tests completed~%" counter))
	   ))
	))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-num 1000)
	  (num-perm 3))
      (begin
        (time-code
         (begin
           (main-loop max-num num-perm)
           ))
	))

    (newline)
    (force-output)

    (let ((max-num 10000)
	  (num-perm 5))
      (begin
	(time-code
	 (begin
	   (main-loop max-num num-perm)
	   ))
	))

    (newline)
    ))
