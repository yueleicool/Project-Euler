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
;;; make a list of primes less than or equal to n
;;; sieve of eratosthenes method
(define (make-prime-list max-num)
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
              (if (equal? ii ii-num)
                  (begin
                    (set! result-list (cons ii result-list))

                    (do ((jj (+ ii ii) (+ jj ii)))
                        ((> jj max-num))
                      (begin
                        (let ((this-num (array-ref intermediate-array jj)))
                          (begin
                            (array-set! intermediate-array -1 jj)
                            ))
                        ))
                    ))
              ))
          ))
      (reverse result-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-prime-list-1)
  (let ((sub-name "test-make-prime-list-1")
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
             (let ((result-list (make-prime-list test-num)))
               (let ((slen (length shouldbe-list))
                     (rlen (length result-list)))
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
                             (r-elem (list-ref result-list ii)))
                         (begin
                           (if (not (equal? s-elem r-elem))
                               (begin
                                 (display
                                  (format #f "~a : error (~a) : num=~a, "
                                          sub-name test-label-index test-num))
                                 (display
                                  (format #f "shouldbe=~a, result=~a, discrepancy at "
                                          shouldbe-list result-list))
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
        (test-label-index 0))
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
        (prime-list (make-prime-list 5))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((test-num (list-ref this-list 0))
                 (shouldbe (list-ref this-list 1))
                 (prime-array (list->array 1 prime-list)))
             (let ((result
                    (is-array-prime? test-num prime-array)))
               (let ((err-1
                      (format #f "~a : error (~a) : num=~a, "
                              sub-name test-label-index test-num))
                     (err-2
                      (format #f "shouldbe=~a, result=~a~%"
                              shouldbe result)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a~a~%" err-1 err-2))
                         (force-output)
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
(define (concat-two-prime-lists plist1 plist2 prime-array)
  (let ((dlist1 (append plist1 plist2))
        (dlist2 (append plist2 plist1)))
    (let ((num1 (turn-digit-list-to-number dlist1))
          (num2 (turn-digit-list-to-number dlist2)))
      (let ((pflag1 (is-array-prime? num1 prime-array))
            (pflag2 (is-array-prime? num2 prime-array)))
        (begin
          (if (and (equal? pflag1 #t)
                   (equal? pflag2 #t))
              (begin
                (sort (list num1 num2) <))
              (begin
                #f
                ))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-concat-two-prime-lists-1)
  (let ((sub-name "test-concat-two-prime-lists-1")
        (test-list
         (list
          (list (list 3) (list 7) (list 37 73))
          (list (list 3) (list 1 0 9) (list 1093 3109))
          (list (list 7) (list 1 0 9) (list 1097 7109))
          (list (list 2) (list 1 0 9) #f)
          (list (list 5) (list 7) #f)
          ))
        (prime-array
         (list->array 1 (make-prime-list 10)))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((plist1 (list-ref alist 0))
                 (plist2 (list-ref alist 1))
                 (shouldbe-list (list-ref alist 2)))
             (let ((result-list
                    (concat-two-prime-lists plist1 plist2 prime-array)))
               (begin
                 (if (not (equal? shouldbe-list result-list))
                     (begin
                       (display (format #f "~a : (~a) : error : plist1 = ~a, plist2 = ~a, shouldbe = ~a, result = ~a~%"
                                        sub-name test-label-index plist1 plist2
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
(define (populate-prime-digits-hash!
         prime-digits-htable prime-array prime-length)
  (begin
    (do ((ii 0 (1+ ii)))
        ((>= ii prime-length))
      (begin
        (let ((aprime (array-ref prime-array ii)))
          (let ((alist (split-digits-list aprime)))
            (begin
              (hash-set! prime-digits-htable aprime alist)
              )))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (populate-valid-prime-pairs-hash!
         valid-prime-pairs-htable prime-digits-htable prime-array)
  (let ((plen (car (array-dimensions prime-array))))
    (begin
      (hash-clear! valid-prime-pairs-htable)

      (do ((ii 0 (1+ ii)))
          ((>= ii plen))
        (begin
          (let ((ii-prime (array-ref prime-array ii)))
            (let ((ii-dlist
                   (hash-ref prime-digits-htable ii-prime (list))))
              (begin
                (do ((jj (1+ ii) (1+ jj)))
                    ((>= jj plen))
                  (begin
                    (let ((jj-prime (array-ref prime-array jj)))
                      (let ((jj-dlist
                             (hash-ref prime-digits-htable jj-prime (list))))
                        (let ((pp-flag
                               (concat-two-prime-lists
                                ii-dlist jj-dlist prime-array)))
                          (begin
                            (if (not (equal? pp-flag #f))
                                (begin
                                  (hash-set!
                                   valid-prime-pairs-htable
                                   (list ii-prime jj-prime) pp-flag)
                                  ))
                            ))
                        ))
                    ))
                )))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-populate-valid-prime-pairs-hash-1)
  (let ((sub-name "test-populate-valid-prime-pairs-hash-1")
        (test-list
         (list
          (list (list 3 7) (list 37 73))
          (list (list 3 11) (list 113 311))
          (list (list 3 17) (list 173 317))
          (list (list 13 19) (list 1319 1913))
          (list (list 2 3) #f)
          (list (list 2 5) #f)
          (list (list 3 5) #f)
          (list (list 5 7) #f)
          ))
        (valid-prime-pairs-htable (make-hash-table 10))
        (prime-digits-htable (make-hash-table 10))
        (prime-array
         (list->array 1 (make-prime-list 20)))
        (test-label-index 0))
    (begin
      (let ((prime-length (car (array-dimensions prime-array))))
        (begin
          (populate-prime-digits-hash!
           prime-digits-htable prime-array prime-length)

          (populate-valid-prime-pairs-hash!
           valid-prime-pairs-htable prime-digits-htable prime-array)
          ))

      (for-each
       (lambda (alist)
         (begin
           (let ((plist1 (list-ref alist 0))
                 (shouldbe-list (list-ref alist 1)))
             (let ((result-list
                    (hash-ref valid-prime-pairs-htable plist1 #f)))
               (begin
                 (if (not (equal? shouldbe-list result-list))
                     (begin
                       (display
                        (format
                         #f "~a : (~a) : error : plist1 = ~a, shouldbe = ~a, result = ~a~%"
                         sub-name test-label-index plist1
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
(define (prime-concats-to-other-primes
         prime-acc-list this-prime this-pdlist
         prime-array prime-digits-htable
         valid-prime-pairs-htable)
  (let ((inner-break-flag #f)
        (ok-flag #t)
        (plen (length prime-acc-list))
        (concatenated-list (list)))
    (begin
      (do ((jj 0 (1+ jj)))
          ((or (>= jj plen)
               (equal? inner-break-flag #t)))
        (begin
          (let ((tmp-prime (list-ref prime-acc-list jj)))
            (let ((prime-key-list (sort (list this-prime tmp-prime) <)))
              (let ((prime-pair-list
                     (hash-ref valid-prime-pairs-htable prime-key-list #f)))
                (begin
                  (cond
                   ((equal? prime-pair-list #f)
                    (begin
                      (set! ok-flag #f)
                      (set! inner-break-flag #t)
                      ))
                   (else
                    (begin
                      (set! concatenated-list
                            (cons (append prime-key-list prime-pair-list)
                                  concatenated-list))
                      )))
                  ))
              ))
          ))
      (if (equal? ok-flag #t)
          (begin
            concatenated-list)
          (begin
            #f))
      )))

;;;#############################################################
;;;#############################################################
(define (test-prime-concats-to-other-primes-1)
  (let ((sub-name "test-prime-concats-to-other-primes-1")
        (test-list
         (list
          (list (list 2) 3 (list 3) #f)
          (list (list 7) 3 (list 3) (list (list 3 7 37 73)))
          (list (list 3) 7 (list 7) (list (list 3 7 37 73)))
          (list (list 109) 3 (list 3) (list (list 3 109 1093 3109)))
          ))
        (valid-prime-pairs-htable (make-hash-table))
        (prime-digits-htable (make-hash-table))
        (prime-list (make-prime-list 200))
        (test-label-index 0))
    (let ((prime-array (list->array 1 prime-list))
          (prime-length (length prime-list)))
      (begin
        (populate-prime-digits-hash!
         prime-digits-htable prime-array prime-length)

        (populate-valid-prime-pairs-hash!
         valid-prime-pairs-htable prime-digits-htable prime-array)

        (for-each
         (lambda (this-list)
           (begin
             (let ((prime-acc-list (list-ref this-list 0))
                   (this-prime (list-ref this-list 1))
                   (this-pdlist (list-ref this-list 2))
                   (shouldbe (list-ref this-list 3)))
               (let ((result
                      (prime-concats-to-other-primes
                       prime-acc-list this-prime this-pdlist
                       prime-array prime-digits-htable
                       valid-prime-pairs-htable)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display
                          (format
                           #f "~a : error (~a) : prime-acc-list=~a, this-prime=~a, this-pdlist=~a, shouldbe=~a, result=~a~%"
                           sub-name test-label-index prime-acc-list this-prime
                           this-pdlist shouldbe result))
                         (quit)
                         ))
                   )))
             (set! test-label-index (1+ test-label-index))
             ))
         test-list)
        ))
    ))

;;;#############################################################
;;;#############################################################
(define-syntax process-ii-index
  (syntax-rules ()
    ((process-ii-index depth max-depth ii max-index
                       prime-acc-list this-prime this-pdlist
                       prime-array prime-digits-htable
                       valid-prime-pairs-htable
                       min-sum-so-far master-acc-list quints-list-lists)
     (begin
       (let ((this-pdlist (hash-ref prime-digits-htable this-prime)))
         (begin
           ;;; see if this-prime concats with previous primes
           (let ((tmp1
                  (prime-concats-to-other-primes
                   prime-acc-list this-prime this-pdlist
                   prime-array prime-digits-htable
                   valid-prime-pairs-htable)))
             (begin
               (if (not (equal? tmp1 #f))
                   (begin
                     (let ((next-result-list
                            (rec-inner-loop
                             (+ depth 1) max-depth
                             (+ ii 1) max-index
                             prime-array prime-digits-htable
                             valid-prime-pairs-htable
                             (cons this-prime prime-acc-list)
                             (append quints-list-lists tmp1)
                             min-sum-so-far
                             master-acc-list)))
                       (begin
                         (if (and (list? next-result-list)
                                  (> (length next-result-list) 0))
                             (begin
                               (let ((next-sum (list-ref next-result-list 0)))
                                 (begin
                                   (if (or (< min-sum-so-far 0)
                                           (< next-sum min-sum-so-far))
                                       (begin
                                         (set! min-sum-so-far next-sum)
                                         (set! master-acc-list next-result-list)
                                         ))
                                   ))
                               ))
                         ))
                     ))
               ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (rec-inner-loop
         depth max-depth this-index max-index prime-array
         prime-digits-htable valid-prime-pairs-htable
         prime-acc-list quints-list-lists
         min-sum-so-far master-acc-list)
  (begin
    (if (>= depth max-depth)
        (begin
          (if (>= (length prime-acc-list) max-depth)
              (begin
                (let ((current-sum
                       (srfi-1:fold + 0 prime-acc-list)))
                  (begin
                    (if (or (<= min-sum-so-far 0)
                            (< current-sum min-sum-so-far))
                        (begin
                          (set! master-acc-list
                                (list current-sum
                                      (sort prime-acc-list <)
                                      quints-list-lists))
                          ))
                    ))
                ))
          master-acc-list)
        (begin
          (let ((continue-loop-flag #t))
            (begin
              (do ((ii this-index (+ ii 1)))
                  ((or (>= ii max-index)
                       (equal? continue-loop-flag #f)))
                (begin
                  (let ((this-prime (array-ref prime-array ii)))
                    (let ((psum (+ (srfi-1:fold + 0 prime-acc-list)
                                   (* (- max-depth depth) this-prime))))
                      (begin
                        (if (and (> min-sum-so-far 0)
                                 (> psum min-sum-so-far))
                            (begin
                              (set! continue-loop-flag #f)
                              ))

                        (process-ii-index
                         depth max-depth ii max-index
                         prime-acc-list this-prime this-pdlist
                         prime-array prime-digits-htable
                         valid-prime-pairs-htable
                         min-sum-so-far master-acc-list quints-list-lists)
                        )))
                  ))

              master-acc-list
              ))
          ))
    ))

;;;#############################################################
;;;#############################################################
(define (main-recursive-loop nseq max-num debug-flag)
  (begin
    (let ((prime-list (sort (make-prime-list max-num) <)))
      (let ((prime-array (list->array 1 prime-list))
            (array-len (length prime-list))
            (prime-digits-htable (make-hash-table))
            (valid-prime-pairs-htable (make-hash-table))
            (prime-acc-list (list)))
        (begin
          (populate-prime-digits-hash!
           prime-digits-htable prime-array array-len)

          (populate-valid-prime-pairs-hash!
           valid-prime-pairs-htable prime-digits-htable prime-array)

          ;;; all variables initialized, so call recursion
          ;;; start from index=1 since we don't need to consider 2
          (let ((master-acc-list-list
                 (rec-inner-loop 0 nseq 1 array-len
                                 prime-array prime-digits-htable
                                 valid-prime-pairs-htable
                                 (list) (list) -1 (list))))
            (let ((min-sum (list-ref master-acc-list-list 0))
                  (acc-list (list-ref master-acc-list-list 1))
                  (quints-list-lists (list-ref master-acc-list-list 2)))
              (begin
                (display
                 (ice-9-format:format
                  #f "~:d = the lowest sum for a set of ~:d primes, where any two "
                  min-sum nseq))
                (display
                 (ice-9-format:format
                  #f "primes concatenate to form a prime, (for primes less than ~:d)~%"
                  max-num))
                (display
                 (ice-9-format:format #f "the primes are: ~a~%"
                                      acc-list))

                (if (equal? debug-flag #t)
                    (begin
                      (let ((squints-list-lists
                             (sort
                              quints-list-lists
                              (lambda (a b)
                                (begin
                                  (let ((a1-elem (car a))
                                        (b1-elem (car b)))
                                    (begin
                                      (if (= a1-elem b1-elem)
                                          (begin
                                            (let ((a2-elem (cadr a))
                                                  (b2-elem (cadr b)))
                                              (begin
                                                (< a2-elem b2-elem)
                                                )))
                                          (begin
                                            (< a1-elem b1-elem)
                                            ))
                                      )))))))
                        (begin
                          (for-each
                           (lambda (this-list)
                             (begin
                               (display
                                (ice-9-format:format
                                 #f "    primes ~:d and ~:d concatenate to ~:d and ~:d which are both prime.~%"
                                 (list-ref this-list 0) (list-ref this-list 1)
                                 (list-ref this-list 2) (list-ref this-list 3)))
                               )) squints-list-lists)
                          (newline)
                          (force-output)
                          ))
                      ))
                )))
          )))
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
    (display (format #f "Problem 060 - The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.~%"))
    (newline)
    (display (format #f "Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.~%"))
    (newline)
    (display (format #f "The key idea of reducing the possible solutions is to store valid prime pairs that concatenate to primes.  This was described at http://www.mathblog.dk/project-euler-60-primes-concatenate/~%"))
    (display (format #f "This program takes around 32 minutes to run.  It was re-written in c++, and it completed in 20 seconds (for primes less than 10,000).  This means I probably have an ok algorithm.~%"))
    (newline)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
        (time-code
         (begin
           (run-test test-split-digits-list-1 counter)
           (run-test test-turn-digit-list-to-number-1 counter)
           (run-test test-make-prime-list-1 counter)
           (run-test test-binary-search-1 counter)
           (run-test test-is-array-prime-1 counter)
           (run-test test-concat-two-prime-lists-1 counter)
           (run-test test-populate-valid-prime-pairs-hash-1 counter)
           (run-test test-prime-concats-to-other-primes-1 counter)

           (display (ice-9-format:format #f "~:d tests completed~%" counter))
           ))
        ))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((set-max 4)
          (max-num 1000)
          (debug-flag #t))
      (begin
        (time-code
         (begin
           (main-recursive-loop set-max max-num debug-flag)
           ))
        ))

    (newline)
    (force-output)

    (let ((set-max 5)
          (max-num 10000)
          (debug-flag #f))
      (begin
        (time-code
         (begin
           (main-recursive-loop set-max max-num debug-flag)
           ))
        ))

    (newline)
    ))
