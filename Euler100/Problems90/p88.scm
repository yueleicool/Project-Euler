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

;;;### getopt-long used for command-line option arguments processing
(use-modules ((ice-9 getopt-long)
              :renamer (symbol-prefix-proc 'ice-9-getopt:)))

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
             (* 0.0010 (truncate (* 1000.0
                                    (- nmins (* nhours 60.0)))))))
        (let ((nseconds
               (* 0.0010
                  (truncate
                   (* 1000.0 (- nsecs (+ (* nhours 60.0 60.0)
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
                        (format #f "~a hours, ~a minutes, ~a seconds" nhours nminutes nseconds)
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
                  tstring
                  ))
              (begin
                (let ((ndays (truncate jd-diff)))
                  (let ((dfract-diff (- jd-diff ndays)))
                    (let ((tstring (local-process-sub-day dfract-diff)))
                      (let ((ttstring (format #f "~a days, ~a" ndays tstring)))
                        (begin
                          ttstring
                          ))
                      )))
                ))
            )))
      (begin
        #f
        )))

;;;#############################################################
;;;#############################################################
(define-public (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
        (let ((s1
               (string-downcase
                (srfi-19:date->string
                 this-datetime "~A, ~B ~d, ~Y")))
              (s2
               (string-downcase
                (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
          (begin
            (format #f "~a, ~a" s1 s2)
            )))
      (begin
        #f
        )))

;;;#############################################################
;;;#############################################################
;;; from a list of divisors of nn, make a list of lists that
;;; 6 -> (list (list 4 (list 2 2)) (list 6 (list 2 3)))
;;; 8 -> (list (list 4 (list 2 2)) (list 6 (list 2 3)) (list 8 (list 2 2 2)) (list 8 (list 2 4)))
(define (construct-factors-list-list end-num)
  (begin
    (let ((local-array (make-array (list) (1+ end-num))))
      (begin
        (do ((ii 2 (1+ ii)))
            ((> ii end-num))
          (begin
            (let ((ii-list (array-ref local-array ii)))
              (begin
                (if (equal? ii-list (list))
                    (begin
                      (array-set! local-array (list ii) ii))
                    (begin
                      (array-set! local-array
                                  (cons (list ii) ii-list) ii)
                      ))

                (do ((jj (+ ii ii) (+ jj ii)))
                    ((> jj end-num))
                  (begin
                    (let ((this-list (array-ref local-array jj)))
                      (let ((div (euclidean/ jj ii)))
                        (let ((prev-list (array-ref local-array div)))
                          (begin
                            (for-each
                             (lambda (a-list)
                               (begin
                                 (if (list? a-list)
                                     (begin
                                       (set! this-list (cons (cons ii a-list) this-list)))
                                     (begin
                                       (set! this-list (cons (list ii a-list) this-list))
                                       ))
                                 )) prev-list)

                            (array-set! local-array this-list jj)
                            ))
                        ))
                    ))
                ))
            ))

        (let ((result-list-list (list)))
          (begin
            (do ((ii 0 (1+ ii)))
                ((> ii end-num))
              (begin
                (let ((elem (array-ref local-array ii)))
                  (begin
                    (if (and (list? elem) (> (length elem) 0))
                        (begin
                          (for-each
                           (lambda (a-list)
                             (begin
                               (if (and (list? a-list) (> (length a-list) 1))
                                   (begin
                                     (let ((stmp (sort a-list <)))
                                       (begin
                                         (set! result-list-list
                                               (cons (list ii stmp)
                                                     result-list-list))
                                         ))
                                     ))
                               )) elem)
                          ))
                    ))
                ))

            (reverse result-list-list)
            ))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-construct-factors-list-list-1)
  (let ((sub-name "test-construct-factors-list-list-1")
        (test-list
         (list
          (list 2 (list))
          (list 3 (list))
          (list 4 (list (list 4 (list 2 2))))
          (list 5 (list (list 4 (list 2 2))))
          (list 6 (list (list 4 (list 2 2)) (list 6 (list 2 3))))
          (list 10 (list (list 4 (list 2 2)) (list 6 (list 2 3))
                         (list 8 (list 2 2 2)) (list 8 (list 2 4))
                         (list 9 (list 3 3)) (list 10 (list 2 5))))
          (list 12 (list (list 4 (list 2 2)) (list 6 (list 2 3))
                         (list 8 (list 2 2 2)) (list 8 (list 2 4))
                         (list 9 (list 3 3)) (list 10 (list 2 5))
                         (list 12 (list 2 2 3)) (list 12 (list 2 6))
                         (list 12 (list 3 4))))
          (list 20 (list (list 4 (list 2 2)) (list 6 (list 2 3))
                         (list 8 (list 2 4)) (list 8 (list 2 2 2))
                         (list 9 (list 3 3)) (list 10 (list 2 5))
                         (list 12 (list 2 2 3)) (list 12 (list 2 6))
                         (list 12 (list 3 4)) (list 14 (list 2 7))
                         (list 15 (list 3 5)) (list 16 (list 2 2 2 2))
                         (list 16 (list 2 8)) (list 16 (list 2 2 4))
                         (list 16 (list 4 4)) (list 18 (list 2 9))
                         (list 18 (list 3 6)) (list 18 (list 2 3 3))
                         (list 20 (list 2 10)) (list 20 (list 2 2 5))
                         (list 20 (list 4 5))))
          ))
        (test-label-index 0)
        (ok-flag #t))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((max-num (list-ref alist 0))
                 (shouldbe-list-list (list-ref alist 1)))
             (let ((result-list-list (construct-factors-list-list max-num)))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length result-list-list)))
                 (begin
                   (if (not (equal? slen rlen))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : max num = ~a : "
                           sub-name test-label-index max-num))
                         (display
                          (format
                           #f "shouldbe = ~a, result = ~a : "
                           shouldbe-list-list result-list-list))
                         (display
                          (format
                           #f "lengths not equal, shouldbe = ~a, result = ~a~%"
                           slen rlen))
                         (set! ok-flag #f)
                         ))

                   (for-each
                    (lambda (s-list)
                      (begin
                        (if (equal? (member s-list result-list-list) #f)
                            (begin
                              (display
                               (format
                                #f "~a : error (~a) : max num = ~a : "
                                sub-name test-label-index max-num))
                              (display
                               (format
                                #f "shouldbe = ~a, result = ~a : "
                                shouldbe-list-list result-list-list))
                              (display
                               (format
                                #f "discrepency at item ~a~%"
                                s-list))
                              (quit)
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
;;; tack on one's til the sum is equal
;;; { 8, (list 2 4) } -> (list 1 1 2 4)
(define (tack-on-ones nn factor-list)
  (let ((this-sum (srfi-1:fold + 0 factor-list)))
    (begin
      (cond
       ((> this-sum nn) #f)
       ((= this-sum nn) factor-list)
       (else
        (let ((one-list (make-list (- nn this-sum) 1)))
          (append one-list factor-list)
          )))
      )))

;;;#############################################################
;;;#############################################################
(define (test-tack-on-ones-1)
  (let ((sub-name "test-tack-on-ones-1")
        (test-list
         (list
          (list 4 (list 2 2) (list 2 2))
          (list 5 (list 1 5) #f)
          (list 6 (list 2 3) (list 1 2 3))
          (list 8 (list 2 4) (list 1 1 2 4))
          (list 8 (list 2 2 2) (list 1 1 2 2 2))
          (list 12 (list 2 6) (list 1 1 1 1 2 6))
          (list 12 (list 3 4) (list 1 1 1 1 1 3 4))
          (list 12 (list 2 2 3) (list 1 1 1 1 1 2 2 3))
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((nn (list-ref alist 0))
                 (div-list (list-ref alist 1))
                 (shouldbe (list-ref alist 2)))
             (let ((result (tack-on-ones nn div-list)))
               (begin
                 (if (not (equal? shouldbe result))
                     (begin
                       (display (format #f "~a : (~a) : error for nn = ~a, div-list = ~a, shouldbe = ~a, result = ~a~%"
                                        sub-name test-label-index nn div-list shouldbe result))
                       (quit)
                       ))
                 )))
           (set! test-label-index (1+ test-label-index))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
;;; div-list-list format (list (list 6 (list 2 3)) (list 8 (list 2 4)) ...
;;; primary-list-list -> (list (list num kk product sum nfactors factor-list)
;;; (list num kk product sum nfactors factor-list) ...)
(define (make-primary-list-list div-list-list)
  (let ((result-list (list)))
    (begin
      (for-each
       (lambda (d-list)
         (begin
           (let ((ddnum (list-ref d-list 0))
                 (ddlist (list-ref d-list 1)))
             (begin
               (if (and (list? ddlist) (> (length ddlist) 1))
                   (begin
                     (let ((product (srfi-1:fold * 1 ddlist))
                           (sum (srfi-1:fold + 0 ddlist))
                           (nfactors (length ddlist)))
                       (let ((kk (+ nfactors (- product sum))))
                         (let ((tlist (list ddnum kk product sum nfactors ddlist)))
                           (begin
                             (set! result-list (cons tlist result-list))
                             ))
                         ))
                     ))
               ))
           )) div-list-list)

      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
;;; primary-list-list -> (list (list num kk product sum nfactors factor-list)
(define (test-make-primary-list-list-1)
  (let ((sub-name "test-make-primary-list-list-1")
        (test-list
         (list
          (list (list (list 4 (list 2 2)) (list 6 (list 2 3)))
                (list (list 4 2 4 4 2 (list 2 2)) (list 6 3 6 5 2 (list 2 3))))
          (list (list (list 8 (list 2 4)) (list 8 (list 2 2 2)))
                (list (list 8 5 8 6 3 (list 2 2 2)) (list 8 4 8 6 2 (list 2 4))))
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((div-list (list-ref alist 0))
                 (shouldbe-list-list (list-ref alist 1)))
             (let ((result-list-list (make-primary-list-list div-list)))
               (let ((slen (length shouldbe-list-list))
                     (rlen (length result-list-list))
                     (max-len (length shouldbe-list-list)))
                 (begin
                   (if (not (equal? rlen slen))
                       (begin
                         (display (format #f "~a : error (~a) : div-list = ~a, length shouldbe = ~a, result = ~a~%"
                                          sub-name test-label-index div-list slen rlen))
                         (set! max-len (min slen rlen))
                         ))
                   (do ((ii 0 (1+ ii)))
                       ((>= ii max-len))
                     (begin
                       (let ((shouldbe-elem (list-ref shouldbe-list-list ii)))
                         (begin
                           (if (equal? (member shouldbe-elem result-list-list) #f)
                               (begin
                                 (display (format #f "~a : error (~a) : div-list = ~a, missing element ~a, shouldbe = ~a, result = ~a~%"
                                                  sub-name test-label-index
                                                  div-list shouldbe-elem
                                                  shouldbe-list-list result-list-list))
                                 (quit)
                                 ))
                           ))
                       ))
                   ))
               ))
           (set! test-label-index (1+ test-label-index))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (list-to-string llist inner-string)
  (let ((this-string
         (string-join
          (map
           (lambda (this-elem)
             (ice9-format:format #f "~:d" this-elem))
           llist)
          inner-string)))
    this-string
    ))

;;;#############################################################
;;;#############################################################
(define (test-list-to-string-1)
  (let ((sub-name "test-list-to-string-1")
        (test-list
         (list
          (list (list 1) " + " "1")
          (list (list 1 2) " + " "1 + 2")
          (list (list 1 2 3) " * " "1 * 2 * 3")
          (list (list 4 5 6 7) " x " "4 x 5 x 6 x 7")
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (let ((llist (list-ref this-list 0))
               (inner-string (list-ref this-list 1))
               (shouldbe (list-ref this-list 2)))
           (let ((result (list-to-string llist inner-string)))
             (begin
               (if (not (string-ci=? shouldbe result))
                   (begin
                     (display (format #f "~a : (~a) : error : list = ~a, inner-string = ~a, shouldbe=~a, result=~a~%"
                                      sub-name test-label-index llist inner-string shouldbe result))
                     (quit)
                     ))
               (set! test-label-index (1+ test-label-index))
               ))))
       test-list))))

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
;;; div-list-list format (list (list 6 (list 2 3)) (list 8 (list 2 4)) ...
;;; primary-list-list -> (list (list num kk product sum nfactors factor-list)
(define (main-loop max-kk max-num status-num debug-flag)
  (let ((min-sum 0)
        (min-prod-sum-set (list))
        (div-list-list (list))
        (kk-dnum-htable (make-hash-table max-kk))
        (kk-plist-htable (make-hash-table max-kk))
        (continue-loop-flag #t)
        (primary-list-list (list))
        (plen -1))
    (begin
      ;;; fact-list-list -> (list (list 4 (list 2 2)) (list 6 (list 2 3)) (list 8 (list 2 2 2)) (list 8 (list 2 4)),...)
      (let ((fac-list-list (construct-factors-list-list max-num)))
        (begin
          (set! div-list-list fac-list-list)
          ))

      ;;; primary-list-list -> (list (list num kk product sum nfactors factor-list),...)
      (let ((prim-list-list (make-primary-list-list div-list-list)))
        (let ((pp-len (length prim-list-list)))
          (begin
            (set! primary-list-list prim-list-list)
            (set! plen pp-len)
            )))

      (let ((sorted-primary-list
             (sort primary-list-list
                   (lambda (a b)
                     (cond
                      ((< (list-ref a 1) (list-ref b 1)) #t)
                      ((> (list-ref a 1) (list-ref b 1)) #f)
                      (else
                       (< (list-ref a 0) (list-ref b 0))
                       )))
                   )))
        (begin
          (do ((ii 0 (1+ ii)))
              ((>= ii plen))
            (begin
              (let ((plist (list-ref sorted-primary-list ii)))
                (let ((dkk (list-ref plist 1)))
                  (let ((min-num (hash-ref kk-dnum-htable dkk -1)))
                    (begin
                      (if (< min-num 0)
                          (begin
                            (hash-set! kk-dnum-htable dkk (list-ref plist 0))
                            (hash-set! kk-plist-htable dkk plist)
                            ))
                      ))
                  ))
              ))

          (do ((kk 2 (1+ kk)))
              ((> kk max-kk))
            (begin
              (let ((dnum (hash-ref kk-dnum-htable kk -1)))
                (begin
                  (if (> dnum 0)
                      (begin
                        (set! min-prod-sum-set (cons dnum min-prod-sum-set))

                        (if (equal? debug-flag #t)
                            (begin
                              (let ((plist (hash-ref kk-plist-htable kk)))
                                (let ((dkk (list-ref plist 1))
                                      (dproduct (list-ref plist 2))
                                      (dsum (list-ref plist 3))
                                      (dfactors (list-ref plist 4))
                                      (dlist (list-ref plist 5)))
                                  (let ((factors-list (tack-on-ones dnum dlist)))
                                    (begin
                                      (display
                                       (ice9-format:format #f "  k=~:d : ~:d = ~a = ~a~%"
                                                           kk dnum
                                                           (list-to-string factors-list " x ")
                                                           (list-to-string factors-list " + ")))
                                      (force-output)
                                      ))
                                  ))
                              )))
                      (begin
                        (display
                         (ice9-format:format #f "  warning: no results for k=~:d and max-num = ~a~%"
                                             kk max-num))
                        (force-output)
                        ))
                  ))
              ))

          (set! min-prod-sum-set
                (srfi-1:delete-duplicates (reverse min-prod-sum-set)))

          (if (equal? debug-flag #t)
              (begin
                (display
                 (ice9-format:format
                  #f "the complete set of minimal product-sum numbers for 2 <= k <= ~:d "
                  max-kk))
                (display
                 (ice9-format:format
                  #f "is  { ~a }, the sum is ~:d.~%"
                  (list-to-string min-prod-sum-set ", ")
                  (srfi-1:fold + 0 min-prod-sum-set))))
              (begin
                (display
                 (ice9-format:format
                  #f "the complete sum of minimal product-sum numbers is ~:d, "
                  (srfi-1:fold + 0 min-prod-sum-set)))
                (display
                 (ice9-format:format
                  #f "for 2 <= k <= ~:d.~%"
                  max-kk))
                ))
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
(define (main args)
  (begin
    (display (format #f "Project Euler 88: - A natural number, N, that can be written as the sum and product of a given set of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak = a1 x a2 x ... x ak.~%"))
    (newline)
    (display (format #f "For example, 6 = 1 + 2 + 3 = 1 x 2 x 3.~%"))
    (display (format #f "For a given set of size, k, we shall call the smallest N with this property a minimal product-sum number. The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.~%"))
    (newline)
    (display (format #f "  k=2: 4 = 2 x 2 = 2 + 2~%"))
    (display (format #f "  k=3: 6 = 1 x 2 x 3 = 1 + 2 + 3~%"))
    (display (format #f "  k=4: 8 = 1 x 1 x 2 x 4 = 1 + 1 + 2 + 4~%"))
    (display (format #f "  k=5: 8 = 1 x 1 x 2 x 2 x 2 = 1 + 1 + 2 + 2 + 2~%"))
    (display (format #f "  k=6: 12 = 1 x 1 x 1 x 1 x 2 x 6 = 1 + 1 + 1 + 1 + 2 + 6~%"))
    (newline)
    (display (format #f "Hence for 2<=k<=6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8 is only counted once in the sum.~%"))
    (newline)
    (display (format #f "In fact, as the complete set of minimal product-sum numbers for 2<=k<=12 is {4, 6, 8, 12, 15, 16}, the sum is 61.~%"))
    (newline)
    (display (format #f "What is the sum of all the minimal product-sum numbers for 2<=k<=12000?~%"))
    (newline)
    (display (format #f "This solution uses dynamic programming, similar to problem 76.  A single array is used to store a list of lists of combinations of divisors of n, between 2 and max-num.  Then the sum and product are computed from the list of lists, sorted, and the minimal k is extracted.~%"))
    (newline)
    (display (format #f "Another, faster solution was described at http://www.mathblog.dk/project-euler-88-minimal-product-sum-numbers/~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
        (time-code
         (begin
           (run-test test-construct-factors-list-list-1 counter)
           (run-test test-tack-on-ones-1 counter)
           (run-test test-make-primary-list-list-1 counter)
           (run-test test-list-to-string-1 counter)

           (display (ice9-format:format #f "~:d tests completed~%" counter))
           ))
        ))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((max-kk 12)
          (max-num 100)
          (status-num 1000)
          (debug-flag #t))
      (begin
        (main-loop max-kk max-num status-num debug-flag)
        ))

    (newline)
    (force-output)

    (let ((max-kk 12000)
          (max-num 15000)
          (status-num 1000)
          (debug-flag #f))
      (begin
        (time-code
         (begin
           (main-loop max-kk max-num status-num debug-flag)
           ))
        ))

    (newline)
    ))
