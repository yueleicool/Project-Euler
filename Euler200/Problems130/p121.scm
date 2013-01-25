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
(define (date-time-to-string this-datetime)
  (if (srfi-19:date? this-datetime)
      (begin
        (let ((s1 (srfi-19:date->string this-datetime "~A, ~B ~d, ~Y"))
              (s2 (string-downcase (srfi-19:date->string this-datetime "~I:~M:~S ~p"))))
          (format #f "~a, ~a" s1 s2)))
      #f))

;;;#############################################################
;;;#############################################################
(define (factorial ii-num)
  (begin
    (cond
     ((<= ii-num 1) 1)
     ((= ii-num 2) 2)
     ((= ii-num 3) 6)
     ((= ii-num 4) 24)
     ((= ii-num 5) 120)
     ((= ii-num 6) 720)
     ((= ii-num 7) 5040)
     ((= ii-num 8) 40320)
     ((= ii-num 9) 362880)
     (else
      (* ii-num (factorial (- ii-num 1)))
      ))
    ))

;;;#############################################################
;;;#############################################################
(define (test-factorial-1)
  (let ((sub-name "test-factorial-1")
        (test-list
         (list
          (list 0 1) (list 1 1) (list 2 2)
          (list 3 6) (list 4 24) (list 5 120)
          (list 6 720) (list 7 5040)
          (list 8 40320) (list 9 362880)
          (list 10 3628800)
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((test-num (list-ref alist 0))
                 (shouldbe (list-ref alist 1)))
             (let ((result (factorial test-num)))
               (begin
                 (if (not (equal? shouldbe result))
                     (begin
                       (display (format #f "~a : (~a) : error : number = ~a, shouldbe = ~a, result = ~a~%"
                                        sub-name test-label-index test-num
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
;;; if you pull a blue chip or red chip, replace the chip and add a red
(define (make-count-array num-turns)
  (let ((count-array (make-array 0 (1+ num-turns))))
    (begin
      ;;; just 1 blue chip
      (array-set! count-array 1 0)

      (do ((ii 1 (1+ ii)))
          ((> ii num-turns))
        (begin
          ;;; number of red chips in the bag at turn ii is ii
          (do ((jj ii (1- jj)))
              ((<= jj 0))
            (begin
              (let ((jj-count (array-ref count-array jj))
                    (jj-m1-count (array-ref count-array (1- jj))))
                (let ((next-count (+ (* jj-m1-count ii) jj-count)))
                  (begin
                    (array-set! count-array next-count jj)
                    )))
              ))
          ))

      count-array
      )))

;;;#############################################################
;;;#############################################################
(define (test-make-count-array-1)
  (let ((sub-name "test-make-count-array-1")
        (test-list
         (list
          (list 1
                (list (list 0 1) (list 1 1)))
          (list 2
                (list (list 0 1) (list 1 3) (list 2 2)))
          (list 3
                (list
                 (list 0 1) (list 1 6) (list 2 11)
                 (list 3 6)))
          (list 4
                (list
                 (list 0 1) (list 1 10) (list 2 35)
                 (list 3 50) (list 4 24)))
          ))
        (test-label-index 0))
    (begin
      (for-each
       (lambda (alist)
         (begin
           (let ((num-turns (list-ref alist 0))
                 (shouldbe-list-list (list-ref alist 1)))
             (let ((result-array (make-count-array num-turns)))
               (begin
                 (for-each
                  (lambda (slist)
                    (begin
                      (let ((sindex (list-ref slist 0))
                            (svalue (list-ref slist 1)))
                        (let ((rvalue (array-ref result-array sindex)))
                          (begin
                            (if (not (equal? svalue rvalue))
                                (begin
                                  (display
                                   (format
                                    #f "~a : (~a) : error : num-turns = ~a : "
                                    sub-name test-label-index num-turns))
                                  (display
                                   (format
                                    #f "for index=~a, shouldbe = ~a, result = ~a~%"
                                    sindex svalue rvalue))
                                  (quit)
                                  ))
                            )))
                      )) shouldbe-list-list)
                 )))
           (set! test-label-index (1+ test-label-index))
           ))
       test-list)
      )))

;;;#############################################################
;;;#############################################################
(define (main-loop num-turns)
  (let ((sum 0)
        (total 0)
        (count-array (make-count-array num-turns))
        (min-blue (euclidean/ num-turns 2)))
    (begin
      (do ((ii 0 (1+ ii)))
          ((> ii min-blue))
        (begin
          (let ((count (array-ref count-array ii)))
            (begin
              (set! sum (+ sum count))
              ))
          ))
      (do ((ii 0 (1+ ii)))
          ((> ii num-turns))
        (begin
          (let ((count (array-ref count-array ii)))
            (begin
              (set! total (+ total count))
              ))
          ))

      (newline)
      (display
       (ice9-format:format
        #f "the maximum prize fund = ~:d : number games won = ~:d~%"
        (euclidean/ total sum) sum))
      (display
       (ice9-format:format
        #f "total = ~:d (single game with ~:d turns).~%"
        total num-turns))
      (force-output)
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
    (display (format #f "Project Euler 121 - A bag contains one red disc and one blue disc. In a game of chance a player takes a disc at random and its colour is noted. After each turn the disc is returned to the bag, an extra red disc is added, and another disc is taken at random.~%"))
    (newline)
    (display (format #f "The player pays 1 to play and wins if they have taken more blue discs than red discs at the end of the game.~%"))
    (newline)
    (display (format #f "If the game is played for four turns, the probability of a player winning is exactly 11/120, and so the maximum prize fund the banker should allocate for winning in this game would be 10 before they would expect to incur a loss. Note that any payout will be a whole number of pounds and also includes the original 1 paid to play the game, so in the example given the player actually wins 9.~%"))
    (newline)
    (display (format #f "Find the maximum prize fund that should be allocated to a single game in which fifteen turns are played.~%"))
    (newline)
    (display (format #f "The solution was found at http://www.mathblog.dk/project-euler-121-coloured-discs/.~%"))
    (display (format #f "It is a path-counting algorithm, which enumerates all the possible outcomes using an array.~%"))
    (newline)
    (force-output)

    ;;; run tests
    (display (format #f "running tests...~%"))
    (let ((counter 0))
      (begin
        (time-code
         (begin
           (run-test test-factorial-1 counter)
           (run-test test-make-count-array-1 counter)

           (display (ice9-format:format #f "~:d tests completed~%" counter))
           ))
        ))

    (display (format #f "Output:~%"))
    (force-output)

    (let ((num-turns 4))
      (begin
        (time-code
         (begin
           (main-loop num-turns)
           ))
        ))

    (newline)
    (force-output)

    (let ((num-turns 15))
      (begin
        (time-code
         (begin
           (main-loop num-turns)
           ))
        ))

    (newline)
    ))
