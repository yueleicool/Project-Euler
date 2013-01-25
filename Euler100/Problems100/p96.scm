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

;;;### srfi-11 for let-values (multiple value bind)
(use-modules ((srfi srfi-11)
              :renamer (symbol-prefix-proc 'srfi-11:)))

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
(define (find-min-possibilities
         sudoku-array max-rows max-cols this-row this-col)
  (let ((min-row -1)
        (min-col -1)
        (min-len -1)
        (min-list (list)))
    (begin
      (do ((ii-row 0 (1+ ii-row)))
          ((>= ii-row max-rows))
        (begin
          (do ((ii-col 0 (1+ ii-col)))
              ((>= ii-col max-cols))
            (begin
              (let ((s-list
                     (array-ref sudoku-array ii-row ii-col)))
                (begin
                  (if (list? s-list)
                      (begin
                        (let ((s-len (length s-list)))
                          (begin
                            (if (and
                                 (not (= ii-row this-row))
                                 (not (= ii-col this-col)))
                                (begin
                                  (if (or (< min-len 0)
                                          (<= s-len min-len))
                                      (begin
                                        (set! min-row ii-row)
                                        (set! min-col ii-col)
                                        (set! min-len s-len)
                                        (set! min-list s-list)
                                        ))
                                  ))
                            ))
                        ))
                  ))
              ))
          ))

      (list min-row min-col min-list)
      )))

;;;#############################################################
;;;#############################################################
(define (test-find-min-possibilities-1)
  (let ((sub-name "test-find-min-possibilities-1")
        (test-list
         (list
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3 (list 1 2 3 4 5 6)
                  2 (list 1 2 3 4 5 6) 6 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3 (list 1 2 3 4 5 6)
                  5 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  1 8 (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  8 1 (list 1 2 3 4 5 6 7 8 9) 2 9
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9)
                  3 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           0 0
           (list 4 1 (list 1 2 3)))
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3 (list 1 2 3 4 5 6)
                  2 (list 1 2 3 4 5 6) 6 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6)
                  3 (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  1 8 (list 1 2 3 4 5 6 7 8 9) 6 4
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  8 1 (list 1 2 3 4 5 6 7 8 9) 2 9
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  6 7 (list 1 2 3 4 5 6 7 8 9) 8 2
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  2 6 (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  2 (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  5 (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9)
                  3 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           0 0
           (list 6 7 (list 1 2)))
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list-list (list-ref this-list 0))
                 (this-row (list-ref this-list 1))
                 (this-col (list-ref this-list 2))
                 (shouldbe (list-ref this-list 3)))
             (let ((aa-array (list->array 2 aa-list-list)))
               (let ((max-rows (car (array-dimensions aa-array)))
                     (max-cols (cadr (array-dimensions aa-array))))
                 (let ((result
                        (find-min-possibilities
                         aa-array max-rows max-cols this-row this-col)))
                   (begin
                     (if (not (equal? shouldbe result))
                         (begin
                           (display
                            (format
                             #f "~a : error (~a) : first-row=~a : "
                             sub-name test-label-index (car aa-list-list)))
                           (display
                            (format
                             #f "shouldbe=~a, result=~a~%"
                             shouldbe result))
                           (quit)
                           ))
                     ))
                 )))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax eliminate-macro-dd
  (syntax-rules ()
    ((eliminate-macro-dd sudoku-array ii jj dd)
     (begin
       (let ((possible-list (array-ref sudoku-array ii jj)))
         (begin
           (if (list? possible-list)
               (begin
                 (let ((next-possible-list
                        (delete dd possible-list)))
                   (begin
                     (if (> (length next-possible-list) 0)
                         (begin
                           (array-set!
                            sudoku-array next-possible-list ii jj)
                           ))
                     ))
                 ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (eliminate-other-cells-possibilities
         sudoku-array max-rows max-cols sub-block-size
         dd-row dd-col dd)
  (begin
    ;;; row first
    (do ((ii 0 (1+ ii)))
        ((>= ii max-rows))
      (begin
        (if (not (= ii dd-row))
            (begin
              (eliminate-macro-dd sudoku-array ii dd-col dd)
              ))
        ))
    ;;; column
    (do ((jj 0 (1+ jj)))
        ((>= jj max-cols))
      (begin
        (if (not (= jj dd-col))
            (begin
              (eliminate-macro-dd sudoku-array dd-row jj dd)
              ))
        ))
    ;;; sub-blocks
    (let ((sub-block-srow (euclidean/ dd-row sub-block-size))
          (sub-block-scol (euclidean/ dd-col sub-block-size)))
      (let ((sr-start (* sub-block-size sub-block-srow))
            (sc-start (* sub-block-size sub-block-scol)))
        (begin
          (do ((ii-rr 0 (1+ ii-rr)))
              ((>= ii-rr sub-block-size))
            (begin
              (let ((ii-row (+ ii-rr sr-start)))
                (begin
                  (if (not (= ii-row dd-row))
                      (begin
                        (do ((jj-cc 0 (1+ jj-cc)))
                            ((>= jj-cc sub-block-size))
                          (begin
                            (let ((jj-col (+ jj-cc sc-start)))
                              (begin
                                (if (not (= jj-col dd-col))
                                    (begin
                                      (eliminate-macro-dd
                                       sudoku-array ii-row jj-col dd)
                                      ))
                                ))
                            ))
                        ))
                  ))
              ))
          )))
    ))

;;;#############################################################
;;;#############################################################
(define (test-eliminate-other-cells-possibilities-1)
  (let ((sub-name "test-eliminate-other-cells-possibilities-1")
        (test-list
         (list
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 2 (list 1 2 3 4 5 6) 6
                  (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           1 1 3
           (list
            (list (list 1 2 4 5 6) (list 1 2 4 5 6) 3 (list 1 2 3 4 5 6)
                  2 (list 1 2 3 4 5 6) 6 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 4 5 6) 3 (list 1 2 4 5 6)
                  5 (list 1 2 4 5 6)  (list 1 2 4 5 6 7) 1)
            (list (list 1 2 4 5 6 7 8 9) (list 1 2 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9)
                  2 9 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))))
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3 (list 1 2 3 4 5 6)
                  2 (list 1 2 3 4 5 6) 6 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3 (list 1 2 3 4 5 6)
                  5 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8 (list 1 2 3 4 5 6 7 8 9)
                  6 4 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1 (list 1 2 3 4 5 6 7 8 9)
                  2 9 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7 (list 1 2 3 4 5 6 7 8 9)
                  8 2 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6 (list 1 2 3 4 5 6 7 8 9)
                  9 5 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 (list 1 2 3 4 5 6 7 8 9)
                  3 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5 (list 1 2 3 4 5 6 7 8 9)
                  1 (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           7 8 9
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3 (list 1 2 3 4 5 6)
                  2 (list 1 2 3 4 5 6) 6 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7 8))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3 (list 1 2 3 4 5 6)
                  5 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8)
                  (list 1 2 3 4 5 6 7 8))
            (list 8 (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8) 2
                  (list 1 2 3 4 5 6 7 8) 3 (list 1 2 3 4 5 6 7 8)
                  (list 1 2 3 4 5 6 7 8) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8))))
          ))
        (sub-block-size 3)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list-list (list-ref this-list 0))
                 (dd-row (list-ref this-list 1))
                 (dd-col (list-ref this-list 2))
                 (dd (list-ref this-list 3))
                 (shouldbe-list-list (list-ref this-list 4)))
             (let ((aa-array (list->array 2 aa-list-list)))
               (let ((max-rows (car (array-dimensions aa-array)))
                     (max-cols (cadr (array-dimensions aa-array))))
                 (begin
                   (eliminate-other-cells-possibilities
                    aa-array max-rows max-cols sub-block-size
                    dd-row dd-col dd)

                   (do ((ii 0 (1+ ii)))
                       ((>= ii max-rows))
                     (begin
                       (do ((jj 0 (1+ jj)))
                           ((>= jj max-cols))
                         (begin
                           (let ((s-num
                                  (list-ref
                                   (list-ref shouldbe-list-list ii) jj))
                                 (r-num (array-ref aa-array ii jj)))
                             (begin
                               (if (not (equal? s-num r-num))
                                   (begin
                                     (display
                                      (format
                                       #f "~a : (~a) : error : row/col=~a/~a : "
                                       sub-name test-label-index ii jj))
                                     (display
                                      (format
                                       #f "shouldbe=~a, result=~a~%"
                                       s-num r-num))
                                     (quit)
                                     ))
                               ))
                           ))
                       ))
                   ))
               ))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (check-ok-to-add-here?
         sudoku-array max-rows max-cols sub-block-size
         dd-row dd-col dd)
  (let ((ok-flag #t))
    (begin
      ;;; row first
      (do ((ii 0 (1+ ii)))
          ((or (>= ii max-rows)
               (equal? ok-flag #f)))
        (begin
          (let ((row-elem (array-ref sudoku-array ii dd-col)))
            (begin
              (if (and
                   (not (= ii dd-row))
                   (number? row-elem)
                   (= row-elem dd))
                  (begin
                    (set! ok-flag #f)
                    ))
              ))
          ))

      ;;; column
      (do ((jj 0 (1+ jj)))
          ((or (>= jj max-cols)
               (equal? ok-flag #f)))
        (begin
          (let ((col-elem (array-ref sudoku-array dd-row jj)))
            (begin
              (if (and
                   (not (= jj dd-col))
                   (number? col-elem)
                   (= col-elem dd))
                  (begin
                    (set! ok-flag #f)
                    ))
              ))
          ))

      ;;; sub-blocks
      (let ((sub-block-srow (euclidean/ dd-row sub-block-size))
            (sub-block-scol (euclidean/ dd-col sub-block-size)))
        (let ((sr-start (* sub-block-size sub-block-srow))
              (sc-start (* sub-block-size sub-block-scol)))
          (begin
            (do ((ii-rr 0 (1+ ii-rr)))
                ((or (>= ii-rr sub-block-size)
                     (equal? ok-flag #f)))
              (begin
                (let ((ii-row (+ ii-rr sr-start)))
                  (begin
                    (if (not (= ii-row dd-row))
                        (begin
                          (do ((jj-cc 0 (1+ jj-cc)))
                              ((or (>= jj-cc sub-block-size)
                                   (equal? ok-flag #f)))
                            (begin
                              (let ((jj-col (+ jj-cc sc-start)))
                                (begin
                                  (if (not (= jj-col dd-col))
                                      (begin
                                        (let ((elem
                                               (array-ref
                                                sudoku-array ii-row jj-col)))
                                          (begin
                                            (if (and
                                                 (number? elem)
                                                 (= elem dd))
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
            )))
      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-check-ok-to-add-here-1)
  (let ((sub-name "test-check-ok-to-add-here-1")
        (test-list
         (list
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 2 (list 1 2 3 4 5 6) 6
                  (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 7 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           0 0 1 #t)
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 2 (list 1 2 3 4 5 6) 6
                  (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           0 0 3 #f)
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 2 (list 1 2 3 4 5 6) 6
                  (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           0 0 6 #f)
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 2 (list 1 2 3 4 5 6) 6
                  (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           8 8 5 #f)
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 2 (list 1 2 3 4 5 6) 6
                  (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           8 8 1 #f)
          (list
           (list
            (list (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 2 (list 1 2 3 4 5 6)
                  6 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6 7 8 9))
            (list 9 (list 1 2 3 4 5 6) (list 1 2 3 4 5 6) 3
                  (list 1 2 3 4 5 6) 5 (list 1 2 3 4 5 6)
                  (list 1 2 3 4 5 6 7) 1)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 1 8
                  (list 1 2 3 4 5 6 7 8 9) 6 4 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 8 1
                  (list 1 2 3 4 5 6 7 8 9) 2 9 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 7 (list 1 2 3) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 8)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 6 7
                  (list 1 2 3 4 5 6 7 8 9) 8 2 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2 6
                  (list 1 2 3 4 5 6 7 8 9) 9 5 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list 8 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 2
                  (list 1 2 3 4 5 6 7 8 9) 3 (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) 9)
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 5
                  (list 1 2 3 4 5 6 7 8 9) 1 (list 1 2 3 4 5 6 7 8 9) 3
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)))
           5 7 8 #f)
          ))
        (sub-block-size 3)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list-list (list-ref this-list 0))
                 (dd-row (list-ref this-list 1))
                 (dd-col (list-ref this-list 2))
                 (dd (list-ref this-list 3))
                 (shouldbe (list-ref this-list 4)))
             (let ((aa-array (list->array 2 aa-list-list)))
               (let ((max-rows (car (array-dimensions aa-array)))
                     (max-cols (cadr (array-dimensions aa-array))))
                 (let ((result
                        (check-ok-to-add-here?
                         aa-array max-rows max-cols sub-block-size
                         dd-row dd-col dd)))
                   (begin
                     (if (not (equal? shouldbe result))
                         (begin
                           (display
                            (format
                             #f "~a : (~a) : error : row/col=~a/~a, dd=~a : "
                             sub-name test-label-index dd-row dd-col dd))
                           (display
                            (format
                             #f "shouldbe=~a, result=~a~%"
                             (if shouldbe "true" "false")
                             (if result "true" "false")))
                           (quit)
                           ))
                     ))
                 )))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (first-pass-reduction
         sudoku-array max-rows max-cols sub-block-size)
  (begin
    (do ((ii-row 0 (1+ ii-row)))
        ((>= ii-row max-rows))
      (begin
        (do ((jj-col 0 (1+ jj-col)))
            ((>= jj-col max-cols))
          (begin
            (let ((this-elem
                   (array-ref sudoku-array ii-row jj-col)))
              (begin
                (if (not (list? this-elem))
                    (begin
                      (eliminate-other-cells-possibilities
                       sudoku-array max-rows max-cols sub-block-size
                       ii-row jj-col this-elem)
                      ))
                ))
            ))
        ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-first-pass-reduction-1)
  (let ((sub-name "test-first-pass-reduction-1")
        (test-list
         (list
          (list
           (list
            (list 1 (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9))
            (list (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9) 9))
           (list
            (list 1 (list 2 3 4 5 6 7 8 9) (list 2 3 4 5 6 7 8 9)
                  (list 2 3 4 5 6 7 8 9) (list 2 3 4 5 6 7 8 9)
                  (list 2 3 4 5 6 7 8 9) (list 2 3 4 5 6 7 8 9)
                  (list 2 3 4 5 6 7 8 9) (list 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8 9) (list 2 3 4 5 6 7 8 9)
                  (list 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8 9) (list 2 3 4 5 6 7 8 9)
                  (list 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8 9) (list 1 2 3 4 5 6 7 8 9)
                  (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8)
                  (list 1 2 3 4 5 6 7 8))
            (list (list 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8)
                  (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8)
                  (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8)
                  (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 8) 9)))
          ))
        (sub-block-size 3)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list-list (list-ref this-list 0))
                 (shouldbe-list-list (list-ref this-list 1)))
             (let ((aa-array (list->array 2 aa-list-list)))
               (let ((max-rows (car (array-dimensions aa-array)))
                     (max-cols (cadr (array-dimensions aa-array))))
                 (begin
                   (first-pass-reduction
                    aa-array max-rows max-cols sub-block-size)

                   (do ((ii 0 (1+ ii)))
                       ((>= ii max-rows))
                     (begin
                       (do ((jj 0 (1+ jj)))
                           ((>= jj max-cols))
                         (begin
                           (let ((s-num
                                  (list-ref
                                   (list-ref shouldbe-list-list ii) jj))
                                 (r-num (array-ref aa-array ii jj)))
                             (begin
                               (if (not (equal? s-num r-num))
                                   (begin
                                     (display
                                      (format
                                       #f "~a : (~a) : error : row/col=~a/~a : "
                                       sub-name test-label-index ii jj))
                                     (display
                                      (format
                                       #f "shouldbe=~a, result=~a~%"
                                       s-num r-num))
                                     (quit)
                                     ))
                               ))
                           ))
                       ))
                   ))
               ))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define-syntax inner-multi-element-macro
  (syntax-rules ()
    ((inner-multi-element-macro
      sudoku-array original-array max-rows max-cols
      sub-square-size this-row this-col
      possible-list start-row start-col
      depth
      continue-loop-flag valid-flag)
     (begin
       (let ((plen (length possible-list))
             (pok-flag #f))
         (begin
           ;;; try all possibilities, stop if found one that works
           (do ((ii-loop 0 (1+ ii-loop)))
               ((or (>= ii-loop plen)
                    (equal? pok-flag #t)))
             (begin
               (let ((dd (list-ref possible-list ii-loop)))
                 (begin
                   (if (check-ok-to-add-here?
                        sudoku-array max-rows max-cols sub-square-size
                        this-row this-col dd)
                       (begin
                         (array-copy! sudoku-array original-array)
                         (array-set! sudoku-array dd this-row this-col)
                         (eliminate-other-cells-possibilities
                          sudoku-array max-rows max-cols sub-square-size
                          this-row this-col dd)

                         (let ((vflag
                                (solve-sudoku!
                                 sudoku-array max-rows max-cols
                                 sub-square-size (1+ depth))))
                           (begin
                             (if (equal? vflag #t)
                                 (begin
                                   (set! continue-loop-flag #f)
                                   (set! pok-flag #t)
                                   (set! valid-flag #t))
                                 (begin
                                   (array-copy! original-array sudoku-array)
                                   (set! start-row this-row)
                                   (set! start-col this-col)
                                   ))
                             ))
                         ))
                   ))
               ))
           (if (equal? pok-flag #f)
               (begin
                 (set! continue-loop-flag #f)
                 ))
           ))
       ))
    ))

;;;#############################################################
;;;#############################################################
(define (solve-sudoku! sudoku-array max-rows max-cols
                       sub-square-size depth)
  (let ((start-row -1)
        (start-col -1)
        (continue-loop-flag #t)
        (valid-flag #f)
        (max-seen 3)
        (seen-htable (make-hash-table))
        (original-array
         (make-array 0 max-rows max-cols)))
    (begin
      (while (equal? continue-loop-flag #t)
        (begin
          (let ((rlist
                 (find-min-possibilities
                  sudoku-array max-rows max-cols
                  start-row start-col)))
            (let ((possible-len (length (list-ref rlist 2)))
                  (hcount (hash-ref seen-htable rlist 0)))
              (begin
                (if (<= possible-len 2)
                    (begin
                      (set! hcount (1+ hcount))
                      (hash-set! seen-htable rlist hcount)

                      (if (>= hcount max-seen)
                          (begin
                            (set! continue-loop-flag #f)
                            (set! valid-flag #f)
                            ))
                      ))

                (let ((this-row (list-ref rlist 0))
                      (this-col (list-ref rlist 1))
                      (possible-list (list-ref rlist 2)))
                  (begin
                    (if (and (>= this-row 0)
                             (>= this-col 0))
                        (begin
                          (inner-multi-element-macro
                           sudoku-array original-array max-rows max-cols
                           sub-square-size this-row this-col
                           possible-list start-row start-col
                           depth
                           continue-loop-flag valid-flag))
                        (begin
                          (set! continue-loop-flag #f)
                          ))

;;;                    (display
;;;                     (format
;;;                      #f "debug (b, depth=~a) start row/col=~a/~a, rlist=~a, is-valid=~a, continue-loop-flag=~a~%"
;;;                      depth start-row start-col rlist valid-flag continue-loop-flag))
;;;                    (display-sudoku-array sudoku-array sub-square-size max-rows max-cols)
                    ))
                )))

          (if (not (equal? valid-flag #t))
              (begin
                (let ((v-flag
                       (is-valid-sudoku-array?
                        sudoku-array max-rows max-cols sub-square-size)))
                  (begin
                    (if (equal? v-flag #t)
                        (begin
                          (set! continue-loop-flag #f)
                          (set! valid-flag #t)
                          ))
                    ))
                ))
          ))

      valid-flag
      )))

;;;#############################################################
;;;#############################################################
(define (replace-zeros-with-all! aa-array max-rows max-cols)
  (let ((all-possibilities (list 1 2 3 4 5 6 7 8 9)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((>= ii max-rows))
        (begin
          (do ((jj 0 (1+ jj)))
              ((>= jj max-cols))
            (begin
              (let ((elem (array-ref aa-array ii jj)))
                (begin
                  (if (<= elem 0)
                      (begin
                        (array-set! aa-array all-possibilities ii jj)
                        ))
                  ))
              ))
          ))
      )))

;;;#############################################################
;;;#############################################################
(define (test-solve-sudoku-1)
  (let ((sub-name "test-solve-sudoku-1")
        (test-list
         (list
          (list (list (list 0 0 3 0 2 0 6 0 0)
                      (list 9 0 0 3 0 5 0 0 1)
                      (list 0 0 1 8 0 6 4 0 0)
                      (list 0 0 8 1 0 2 9 0 0)
                      (list 7 0 0 0 0 0 0 0 8)
                      (list 0 0 6 7 0 8 2 0 0)
                      (list 0 0 2 6 0 9 5 0 0)
                      (list 8 0 0 2 0 3 0 0 9)
                      (list 0 0 5 0 1 0 3 0 0))
                (list (list 4 8 3 9 2 1 6 5 7)
                      (list 9 6 7 3 4 5 8 2 1)
                      (list 2 5 1 8 7 6 4 9 3)
                      (list 5 4 8 1 3 2 9 7 6)
                      (list 7 2 9 5 6 4 1 3 8)
                      (list 1 3 6 7 9 8 2 4 5)
                      (list 3 7 2 6 8 9 5 1 4)
                      (list 8 1 4 2 5 3 7 6 9)
                      (list 6 9 5 4 1 7 3 8 2)))
          (list (list (list 2 0 0 0 8 0 3 0 0)
                      (list 0 6 0 0 7 0 0 8 4)
                      (list 0 3 0 5 0 0 2 0 9)
                      (list 0 0 0 1 0 5 4 0 8)
                      (list 0 0 0 0 0 0 0 0 0)
                      (list 4 0 2 7 0 6 0 0 0)
                      (list 3 0 1 0 0 7 0 4 0)
                      (list 7 2 0 0 4 0 0 6 0)
                      (list 0 0 4 0 1 0 0 0 3))
                (list (list 2 4 5 9 8 1 3 7 6)
                      (list 1 6 9 2 7 3 5 8 4)
                      (list 8 3 7 5 6 4 2 1 9)
                      (list 9 7 6 1 2 5 4 3 8)
                      (list 5 1 3 4 9 8 6 2 7)
                      (list 4 8 2 7 3 6 9 5 1)
                      (list 3 9 1 6 5 7 8 4 2)
                      (list 7 2 8 3 4 9 1 6 5)
                      (list 6 5 4 8 1 2 7 9 3)))
          (list (list (list 0 0 0 0 0 0 9 0 7)
                      (list 0 0 0 4 2 0 1 8 0)
                      (list 0 0 0 7 0 5 0 2 6)
                      (list 1 0 0 9 0 4 0 0 0)
                      (list 0 5 0 0 0 0 0 4 0)
                      (list 0 0 0 5 0 7 0 0 9)
                      (list 9 2 0 1 0 8 0 0 0)
                      (list 0 3 4 0 5 9 0 0 0)
                      (list 5 0 7 0 0 0 0 0 0))
                (list (list 4 6 2 8 3 1 9 5 7)
                      (list 7 9 5 4 2 6 1 8 3)
                      (list 3 8 1 7 9 5 4 2 6)
                      (list 1 7 3 9 8 4 2 6 5)
                      (list 6 5 9 3 1 2 7 4 8)
                      (list 2 4 8 5 6 7 3 1 9)
                      (list 9 2 6 1 7 8 5 3 4)
                      (list 8 3 4 2 5 9 6 7 1)
                      (list 5 1 7 6 4 3 8 9 2)))
          (list (list (list 0 0 0 0 0 0 0 8 0)
                      (list 8 0 0 7 0 1 0 4 0)
                      (list 0 4 0 0 2 0 0 3 0)
                      (list 3 7 4 0 0 0 9 0 0)
                      (list 0 0 0 0 3 0 0 0 0)
                      (list 0 0 5 0 0 0 3 2 1)
                      (list 0 1 0 0 6 0 0 5 0)
                      (list 0 5 0 8 0 2 0 0 6)
                      (list 0 8 0 0 0 0 0 0 0))
                (list (list 7 6 1 5 4 3 2 8 9)
                      (list 8 3 2 7 9 1 6 4 5)
                      (list 5 4 9 6 2 8 1 3 7)
                      (list 3 7 4 2 1 5 9 6 8)
                      (list 1 2 8 9 3 6 5 7 4)
                      (list 6 9 5 4 8 7 3 2 1)
                      (list 4 1 7 3 6 9 8 5 2)
                      (list 9 5 3 8 7 2 4 1 6)
                      (list 2 8 6 1 5 4 7 9 3)))
          ))
        (sub-square-size 3)
        (max-rows 9)
        (max-cols 9)
        (depth 0)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list (list-ref this-list 0))
                 (shouldbe (list-ref this-list 1)))
             (let ((aa-array (list->array 2 aa-list)))
               (let ((max-rows (car (array-dimensions aa-array)))
                     (max-cols (cadr (array-dimensions aa-array))))
                 (begin
                   (replace-zeros-with-all! aa-array max-rows max-cols)

                   (first-pass-reduction aa-array max-rows max-cols
                                         sub-square-size)

                   (solve-sudoku! aa-array max-rows max-cols
                                  sub-square-size depth)

                   (let ((result-list (array->list aa-array)))
                     (begin
                       (if (not (equal? shouldbe result-list))
                           (begin
                             (display (format #f "~a : error (~a) : shouldbe=~a, result=~a~%"
                                              sub-name test-label-index shouldbe result-list))
                             (display-sudoku-array aa-array sub-square-size max-rows max-cols)
                             (force-output)
                             (quit)
                             ))
                       ))
                   ))
               ))

           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (are-rows-valid? sudoku-array max-rows max-cols)
  (let ((numbers-list (list))
        (ok-flag #t))
    (begin
      (do ((ii 0 (1+ ii)))
          ((or (>= ii max-rows)
               (equal? ok-flag #f)))
        (begin
          (do ((jj 0 (1+ jj)))
              ((or (>= jj max-cols)
                   (equal? ok-flag #f)))
            (begin
              (let ((this-elem (array-ref sudoku-array ii jj)))
                (begin
                  (if (and
                       (not (list? this-elem))
                       (> this-elem 0) (< this-elem 10)
                       (equal? (member this-elem numbers-list) #f))
                      (begin
                        (set! numbers-list (cons this-elem numbers-list)))
                      (begin
                        (set! ok-flag #f)
                        ))
                  ))
              ))

          (if (equal? ok-flag #t)
              (begin
                (set! numbers-list (list))
                ))
          ))

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-are-rows-valid-1)
  (let ((sub-name "test-are-rows-valid-1")
        (test-list
         (list
          (list (list (list 0 0 3 0 2 0 6 0 0)
                      (list 9 0 0 3 0 5 0 0 1)
                      (list 0 0 1 8 0 6 4 0 0)
                      (list 0 0 8 1 0 2 9 0 0)
                      (list 7 0 0 0 0 0 0 0 8)
                      (list 0 0 6 7 0 8 2 0 0)
                      (list 0 0 2 6 0 9 5 0 0)
                      (list 8 0 0 2 0 3 0 0 9)
                      (list 0 0 5 0 1 0 3 0 0))
                #f)
          (list (list (list 4 8 3 9 2 1 6 5 7)
                      (list 9 6 7 3 4 5 8 2 1)
                      (list 2 5 1 8 7 6 4 9 3)
                      (list 5 4 8 1 3 2 9 7 6)
                      (list 7 2 9 5 6 4 1 3 8)
                      (list 1 3 6 7 9 8 2 4 5)
                      (list 3 7 2 6 8 9 5 1 4)
                      (list 8 1 4 2 5 3 7 6 9)
                      (list 6 9 5 4 1 7 3 8 2))
                #t)
          ))
        (max-rows 9)
        (max-cols 9)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list (list-ref this-list 0))
                 (shouldbe (list-ref this-list 1)))
             (let ((aa-array (list->array 2 aa-list)))
               (let ((result
                      (are-rows-valid? aa-array max-rows max-cols)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a : error (~a) : shouldbe=~a, result=~a~%"
                                          sub-name test-label-index shouldbe result))
                         (quit)
                         ))
                   ))
               ))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (are-cols-valid? sudoku-array max-rows max-cols)
  (let ((numbers-list (list))
        (ok-flag #t))
    (begin
      (do ((jj 0 (1+ jj)))
          ((or (>= jj max-cols)
               (equal? ok-flag #f)))
        (begin
          (do ((ii 0 (1+ ii)))
              ((or (>= ii max-rows)
                   (equal? ok-flag #f)))
            (begin
              (let ((this-elem (array-ref sudoku-array ii jj)))
                (begin
                  (if (and
                       (not (list? this-elem))
                       (> this-elem 0) (< this-elem 10)
                       (equal? (member this-elem numbers-list) #f))
                      (begin
                        (set! numbers-list (cons this-elem numbers-list)))
                      (begin
                        (set! ok-flag #f)
                        ))
                  ))
              ))

          (if (equal? ok-flag #t)
              (begin
                (set! numbers-list (list))
                ))
          ))

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-are-cols-valid-1)
  (let ((sub-name "test-are-cols-valid-1")
        (test-list
         (list
          (list (list (list 0 0 3 0 2 0 6 0 0)
                      (list 9 0 0 3 0 5 0 0 1)
                      (list 0 0 1 8 0 6 4 0 0)
                      (list 0 0 8 1 0 2 9 0 0)
                      (list 7 0 0 0 0 0 0 0 8)
                      (list 0 0 6 7 0 8 2 0 0)
                      (list 0 0 2 6 0 9 5 0 0)
                      (list 8 0 0 2 0 3 0 0 9)
                      (list 0 0 5 0 1 0 3 0 0))
                #f)
          (list (list (list 4 8 3 9 2 1 6 5 7)
                      (list 9 6 7 3 4 5 8 2 1)
                      (list 2 5 1 8 7 6 4 9 3)
                      (list 5 4 8 1 3 2 9 7 6)
                      (list 7 2 9 5 6 4 1 3 8)
                      (list 1 3 6 7 9 8 2 4 5)
                      (list 3 7 2 6 8 9 5 1 4)
                      (list 8 1 4 2 5 3 7 6 9)
                      (list 6 9 5 4 1 7 3 8 2))
                #t)
          ))
        (max-rows 9)
        (max-cols 9)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list (list-ref this-list 0))
                 (shouldbe (list-ref this-list 1)))
             (let ((aa-array (list->array 2 aa-list)))
               (let ((result (are-cols-valid? aa-array max-rows max-cols)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a : error (~a) : shouldbe=~a, result=~a~%"
                                          sub-name test-label-index shouldbe result))
                         (quit)
                         ))
                   ))
               ))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (are-sub-squares-valid? sudoku-array sub-square-size max-rows max-cols)
  (let ((numbers-list (list))
        (ok-flag #t)
        (max-block-rows (euclidean-quotient max-rows sub-square-size))
        (max-block-cols (euclidean-quotient max-cols sub-square-size)))
    (begin
      ;;; loop over sub-blocks
      (do ((ii-block 0 (1+ ii-block)))
          ((or (>= ii-block max-block-rows)
               (equal? ok-flag #f)))
        (begin
          (let ((init-row (* ii-block sub-square-size))
                (end-row (* (+ ii-block 1) sub-square-size)))
            (begin
              (do ((jj-block 0 (1+ jj-block)))
                  ((or (>= jj-block max-block-cols)
                       (equal? ok-flag #f)))
                (begin
                  (let ((init-col (* jj-block sub-square-size))
                        (end-col (* (+ jj-block 1) sub-square-size)))
                    (begin
                      ;;; loop over elements within a sub-block
                      (do ((ii init-row (1+ ii)))
                          ((or (>= ii end-row)
                               (equal? ok-flag #f)))
                        (begin
                          (do ((jj init-col (1+ jj)))
                              ((or (>= jj end-col)
                                   (equal? ok-flag #f)))
                            (begin
                              (let ((this-elem (array-ref sudoku-array ii jj)))
                                (begin
                                  (if (and
                                       (not (list? this-elem))
                                       (> this-elem 0) (< this-elem 10)
                                       (equal? (member this-elem numbers-list) #f))
                                      (begin
                                        (set! numbers-list (cons this-elem numbers-list)))
                                      (begin
                                        (set! ok-flag #f)
                                        ))
                                  ))
                              ))
                          ))
                      (if (equal? ok-flag #t)
                          (begin
                            (set! numbers-list (list))
                            ))
                      ))
                  ))
              ))
          ))

      ok-flag
      )))

;;;#############################################################
;;;#############################################################
(define (test-are-sub-squares-valid-1)
  (let ((sub-name "test-are-sub-squares-valid-1")
        (test-list
         (list
          (list (list (list 0 0 3 0 2 0 6 0 0)
                      (list 9 0 0 3 0 5 0 0 1)
                      (list 0 0 1 8 0 6 4 0 0)
                      (list 0 0 8 1 0 2 9 0 0)
                      (list 7 0 0 0 0 0 0 0 8)
                      (list 0 0 6 7 0 8 2 0 0)
                      (list 0 0 2 6 0 9 5 0 0)
                      (list 8 0 0 2 0 3 0 0 9)
                      (list 0 0 5 0 1 0 3 0 0))
                #f)
          (list (list (list 4 8 3 9 2 1 6 5 7)
                      (list 9 6 7 3 4 5 8 2 1)
                      (list 2 5 1 8 7 6 4 9 3)
                      (list 5 4 8 1 3 2 9 7 6)
                      (list 7 2 9 5 6 4 1 3 8)
                      (list 1 3 6 7 9 8 2 4 5)
                      (list 3 7 2 6 8 9 5 1 4)
                      (list 8 1 4 2 5 3 7 6 9)
                      (list 6 9 5 4 1 7 3 8 2))
                #t)
          ))
        (sub-square-size 3)
        (max-rows 9)
        (max-cols 9)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list (list-ref this-list 0))
                 (shouldbe (list-ref this-list 1)))
             (let ((aa-array (list->array 2 aa-list)))
               (let ((result (are-sub-squares-valid?
                              aa-array sub-square-size max-rows max-cols)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a : error (~a) : shouldbe=~a, result=~a~%"
                                          sub-name test-label-index shouldbe result))
                         (quit)
                         ))
                   ))
               ))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (is-valid-sudoku-array?
         sudoku-array max-rows max-cols sub-square-size)
  (cond
   ((equal? (are-rows-valid? sudoku-array max-rows max-cols) #f) #f)
   ((equal? (are-cols-valid? sudoku-array max-rows max-cols) #f) #f)
   ((equal? (are-sub-squares-valid?
             sudoku-array sub-square-size max-rows max-cols) #f) #f)
   (else
    #t
    )))

;;;#############################################################
;;;#############################################################
(define (test-is-valid-sudoku-array-1)
  (let ((sub-name "test-is-valid-sudoku-array-1")
        (test-list
         (list
          (list (list (list 0 0 3 0 2 0 6 0 0)
                      (list 9 0 0 3 0 5 0 0 1)
                      (list 0 0 1 8 0 6 4 0 0)
                      (list 0 0 8 1 0 2 9 0 0)
                      (list 7 0 0 0 0 0 0 0 8)
                      (list 0 0 6 7 0 8 2 0 0)
                      (list 0 0 2 6 0 9 5 0 0)
                      (list 8 0 0 2 0 3 0 0 9)
                      (list 0 0 5 0 1 0 3 0 0))
                #f)
          (list (list (list 4 8 3 9 2 1 6 5 7)
                      (list 9 6 7 3 4 5 8 2 1)
                      (list 2 5 1 8 7 6 4 9 3)
                      (list 5 4 8 1 3 2 9 7 6)
                      (list 7 2 9 5 6 4 1 3 8)
                      (list 1 3 6 7 9 8 2 4 5)
                      (list 3 7 2 6 8 9 5 1 4)
                      (list 8 1 4 2 5 3 7 6 9)
                      (list 6 9 5 4 1 7 3 8 2))
                #t)
          ))
        (sub-square-size 3)
        (max-rows 9)
        (max-cols 9)
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list (list-ref this-list 0))
                 (shouldbe (list-ref this-list 1)))
             (let ((aa-array (list->array 2 aa-list)))
               (let ((result (is-valid-sudoku-array?
                              aa-array max-rows max-cols sub-square-size)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a : error (~a) : shouldbe=~a, result=~a~%"
                                          sub-name test-label-index shouldbe result))
                         (quit)
                         ))
                   ))
               ))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (three-digit-number sudoku-array)
  (let ((result-num 0))
    (begin
      (do ((jj 0 (1+ jj)))
          ((>= jj 3))
        (begin
          (let ((this-elem (array-ref sudoku-array 0 jj)))
            (begin
              (set! result-num (+ (* result-num 10) this-elem))
              ))
          ))

      result-num
      )))

;;;#############################################################
;;;#############################################################
(define (test-three-digit-number-1)
  (let ((sub-name "test-three-digit-number-1")
        (test-list
         (list
          (list (list (list 4 8 3 9 2 1 6 5 7)
                      (list 9 6 7 3 4 5 8 2 1)
                      (list 2 5 1 8 7 6 4 9 3)
                      (list 5 4 8 1 3 2 9 7 6)
                      (list 7 2 9 5 6 4 1 3 8)
                      (list 1 3 6 7 9 8 2 4 5)
                      (list 3 7 2 6 8 9 5 1 4)
                      (list 8 1 4 2 5 3 7 6 9)
                      (list 6 9 5 4 1 7 3 8 2))
                483)
          (list (list (list 5 8 3 9 2 1 6 5 7)
                      (list 9 6 7 3 4 5 8 2 1)
                      (list 2 5 1 8 7 6 4 9 3)
                      (list 5 4 8 1 3 2 9 7 6)
                      (list 7 2 9 5 6 4 1 3 8)
                      (list 1 3 6 7 9 8 2 4 5)
                      (list 3 7 2 6 8 9 5 1 4)
                      (list 8 1 4 2 5 3 7 6 9)
                      (list 6 9 5 4 1 7 3 8 2))
                583)
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (this-list)
         (begin
           (let ((aa-list (list-ref this-list 0))
                 (shouldbe (list-ref this-list 1)))
             (let ((aa-array (list->array 2 aa-list)))
               (let ((result (three-digit-number aa-array)))
                 (begin
                   (if (not (equal? shouldbe result))
                       (begin
                         (display (format #f "~a : error (~a) : shouldbe=~a, result=~a~%"
                                          sub-name test-label-index shouldbe result))
                         (quit)
                         ))
                   ))
               ))
           (set! test-label-index (+ test-label-index 1))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (display-sudoku-array sudoku-array sub-square-size
                              max-rows max-cols)
  (let ((hlen (+ (* 2 (+ max-cols sub-square-size)) 1)))
    (let ((hline (make-string hlen #\-)))
      (begin
        (do ((ii 0 (1+ ii)))
            ((>= ii max-rows))
          (begin
            (let ((pstring ""))
              (begin
                (do ((jj 0 (1+ jj)))
                    ((>= jj max-cols))
                  (begin
                    (let ((elem-string
                           (format #f "~a" (array-ref sudoku-array ii jj))))
                      (begin
                        (if (zero? (modulo jj sub-square-size))
                            (begin
                              (set! pstring (string-append pstring " | " elem-string)))
                            (begin
                              (set! pstring (string-append pstring " " elem-string))
                              ))
                        ))
                    ))

                (if (zero? (modulo ii sub-square-size))
                    (begin
                      (display (format #f " ~a~%" hline))
                      ))

                (display (format #f "~a |~%" pstring))
                ))
            ))
        (display (format #f " ~a~%" hline))
        ))
    ))

;;;#############################################################
;;;#############################################################
;;; returns a list of lists
(define (read-in-file fname)
  (let ((results-list-list (list))
        (current-list-list (list))
        (counter 0))
    (begin
      (if (file-exists? fname)
          (begin
            (with-input-from-file fname
              (lambda ()
                (do ((line (ice9-rdelim:read-delimited "\r\n") (ice9-rdelim:read-delimited "\r\n")))
                    ((eof-object? line))
                  (begin
                    (if (and (not (eof-object? line))
                             (> (string-length line) 0))
                        (begin
                          (cond
                           ((string-prefix-ci? "grid" line)
                            (begin
                              (set! counter (1+ counter))
                              (if (and (list? current-list-list)
                                       (> (length current-list-list) 0))
                                  (begin
                                    (set! results-list-list
                                          (cons (reverse current-list-list) results-list-list))
                                    (set! current-list-list (list))
                                    ))
                              ))
                           (else
                            (let ((this-number-list
                                   (map string->number
                                        (map string (string->list (string-trim-both line)))
                                        )))
                              (begin
                                (set! current-list-list (cons this-number-list current-list-list))
                                ))))
                          ))
                    ))
                ))

            (if (and (list? current-list-list)
                     (> (length current-list-list) 0))
                (begin
                  (set! results-list-list
                        (cons (reverse current-list-list) results-list-list))
                  (set! current-list-list (list))
                  ))

            (display (format #f "read in ~a grids from ~a~%" counter fname))
            (newline)
            (force-output)

            (reverse results-list-list))
          (begin
            (list)
            ))
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop filename sub-square-size debug-flag)
  (let ((continue-loop-flag #t)
        (scounter 0)
        (three-digit-sum 0)
        (depth 0)
        (sudoku-list-list (read-in-file filename)))
    (begin
      (for-each
       (lambda (aa-list-list)
         (begin
           (set! scounter (1+ scounter))
           (let ((aa-array (list->array 2 aa-list-list)))
             (let ((array-dims (array-dimensions aa-array)))
               (let ((max-rows (list-ref array-dims 0))
                     (max-cols (list-ref array-dims 1)))
                 (begin
                   (replace-zeros-with-all! aa-array max-rows max-cols)

                   ;;; get rid of possibilities using the given elements
                   (first-pass-reduction aa-array max-rows max-cols
                                         sub-square-size)

                   (solve-sudoku! aa-array max-rows max-cols
                                  sub-square-size depth)

                   (if (is-valid-sudoku-array?
                        aa-array max-rows max-cols sub-square-size)
                       (begin
                         (let ((tcode (three-digit-number aa-array)))
                           (begin
                             (set! three-digit-sum (+ three-digit-sum tcode))

                             (display
                              (ice9-format:format
                               #f " (~:d) three digit code = ~:d : sum so far = ~:d~%"
                               scounter tcode three-digit-sum))
                             (force-output)

                             (if (equal? debug-flag #t)
                                 (begin
                                   (display-sudoku-array aa-array sub-square-size max-rows max-cols)
                                   (force-output)
                                   ))
                             )))
                       (begin
                         (display
                          (ice9-format:format
                           #f " (~:d) invalid sudoku array found!~%" scounter))
                         (display-sudoku-array
                          aa-array sub-square-size max-rows max-cols)

                         (let ((aa-array (list->array 2 aa-list-list)))
                           (let ((array-dims (array-dimensions aa-array)))
                             (let ((max-rows (list-ref array-dims 0))
                                   (max-cols (list-ref array-dims 1)))
                               (begin
                                 (display (format #f "original matrix~%"))
                                 (display-sudoku-array
                                  aa-array sub-square-size max-rows max-cols)

                                 (display (format #f "stopping program...~%"))
                                 (force-output)
                                 (quit)
                                 ))
                             ))
                         ))
                   )))
             ))) sudoku-list-list)

      (newline)
      (let ((end-jday (srfi-19:current-julian-day)))
        (begin
          (display (ice9-format:format
                    #f "The sum of all 3-digit numbers of all ~:d puzzles is ~:d~%"
                    scounter three-digit-sum))
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
    (display (format #f "Project Euler 96 - Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept. Its origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar, and much more difficult, puzzle idea called Latin Squares. The objective of Su Doku puzzles, however, is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column, and 3 by 3 box contains each of the digits 1 to 9. Below is an example of a typical starting puzzle grid and its solution grid.~%"))
    (newline)
    (display (format #f "A well constructed Su Doku puzzle has a unique solution and can be solved by logic, although it may be necessary to employ 'guess and test' methods in order to eliminate options (there is much contested opinion over this). The complexity of the search determines the difficulty of the puzzle; the example above is considered easy because it can be solved by straight forward direct deduction.~%"))
    (newline)
    (display (format #f "The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'), contains fifty different Su Doku puzzles ranging in difficulty, but all with unique solutions (the first puzzle in the file is the example above).~%"))
    (display (format #f "http://projecteuler.net/project/sudoku.txt~%"))
    (newline)
    (display (format #f "By solving all fifty puzzles find the sum of the 3-digit numbers found in the top left corner of each solution grid; for example, 483 is the 3-digit number found in the top left corner of the solution grid above.~%"))
    (newline)
    (display (format #f "The algorithm uses constraint programming (see http://en.wikipedia.org/wiki/Constraint_programming), as described by Peter Norvig at http://norvig.com/sudoku.html, a depth-first recursive search.~%"))
    (display (format #f "The minimum-remaining values, recursive search involves looking at a location with the fewest possible values, and then tries them one at a time.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
        (time-code
         (begin
           (run-test test-find-min-possibilities-1 counter)
           (run-test test-eliminate-other-cells-possibilities-1 counter)
           (run-test test-check-ok-to-add-here-1 counter)
           (run-test test-first-pass-reduction-1 counter)
           (run-test test-solve-sudoku-1 counter)
           (run-test test-three-digit-number-1 counter)
           (run-test test-are-rows-valid-1 counter)
           (run-test test-are-cols-valid-1 counter)
           (run-test test-are-sub-squares-valid-1 counter)
           (run-test test-is-valid-sudoku-array-1 counter)

           (display (ice9-format:format #f "~:d tests completed~%" counter))
           ))
        ))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((input-list
           (list (list 0 0 3 0 2 0 6 0 0)
                 (list 9 0 0 3 0 5 0 0 1)
                 (list 0 0 1 8 0 6 4 0 0)
                 (list 0 0 8 1 0 2 9 0 0)
                 (list 7 0 0 0 0 0 0 0 8)
                 (list 0 0 6 7 0 8 2 0 0)
                 (list 0 0 2 6 0 9 5 0 0)
                 (list 8 0 0 2 0 3 0 0 9)
                 (list 0 0 5 0 1 0 3 0 0)))
          (sub-square-size 3)
          (max-rows 9)
          (max-cols 9)
          (depth 0))
      (let ((input-array (list->array 2 input-list)))
        (begin
          (time-code
           (begin
             (display (format #f "input array~%"))
             (display-sudoku-array input-array sub-square-size
                                   max-rows max-cols)
             (replace-zeros-with-all! input-array max-rows max-cols)
             (first-pass-reduction input-array max-rows max-cols
                                   sub-square-size)
             (solve-sudoku! input-array max-rows max-cols
                            sub-square-size depth)

             (display (format #f "solution array~%"))
             (display-sudoku-array input-array sub-square-size
                                   max-rows max-cols)
             (let ((tcode (three-digit-number input-array)))
               (begin
                 (display (format #f "three digit code = ~a~%" tcode))
                 ))

             (newline)
             ))
          )))

    (newline)
    (force-output)

    (let ((filename "sudoku.txt")
          (sub-square-size 3)
          (debug-flag #f))
      (begin
        (time-code
         (begin
           (main-loop filename sub-square-size debug-flag)
           ))
        ))
    ))
